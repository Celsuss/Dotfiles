pragma Singleton
import QtQuick
import Quickshell
import Quickshell.Io
import qs

// MPD over its TCP port via bash's /dev/tcp (Quickshell sockets are
// unix-only and mpd.conf only binds 127.0.0.1:6600). Each poll sends a
// command list and parses the `list_OK`-separated responses.
Singleton {
    id: root

    readonly property string host: "127.0.0.1"
    readonly property int port: 6600
    readonly property string radioPlaylist: "Radios"

    property bool available: false
    property string state: "stop"        // play | pause | stop
    readonly property bool playing: state === "play"
    property string title: ""
    property string artist: ""
    property string name: ""             // stream name from #EXTINF
    property string file: ""
    property int songPos: -1
    property int queueLength: 0
    property var stations: []            // [{ name, file }]
    property var queueFiles: []
    property bool busy: false

    readonly property string headline: name !== "" ? name : (title !== "" ? title : file)
    readonly property string subline: {
        if (name !== "" && title !== "") return title;
        return artist;
    }
    readonly property int currentStation: stations.findIndex(s => s.file === file)

    // The shell script that runs a command list and dumps the reply.
    readonly property string script: "
exec 3<>/dev/tcp/" + host + "/" + port + " || exit 1
{ printf 'command_list_ok_begin\\n'; for c in \"$@\"; do printf '%s\\n' \"$c\"; done; printf 'command_list_end\\nclose\\n'; } >&3
timeout 3 cat <&3"

    function refresh() {
        if (pollProc.running) return;
        pollProc.command = ["bash", "-c", script, "mpd",
            "status", "currentsong", "listplaylistinfo " + radioPlaylist, "playlistinfo"];
        pollProc.running = true;
    }

    function send(cmds) {
        if (actionProc.running) return;
        busy = true;
        actionProc.command = ["bash", "-c", script, "mpd"].concat(cmds);
        actionProc.running = true;
    }

    function toggle()   { send([state === "stop" ? "play" : "pause"]); }
    function stop()     { send(["stop"]); }
    function next()     { send(["next"]); }
    function previous() { send(["previous"]); }

    // Play a station. If the queue already is the radio playlist just jump
    // to it; otherwise replace the queue with the radio playlist first.
    function playStation(index) {
        const radioFiles = stations.map(s => s.file);
        const queueIsRadio = queueFiles.length === radioFiles.length
            && queueFiles.every((f, i) => f === radioFiles[i]);
        if (queueIsRadio) send(["play " + index]);
        else send(["clear", "load " + quote(radioPlaylist), "play " + index]);
    }

    function quote(s) { return "\"" + s.replace(/["\\]/g, "\\$&") + "\""; }

    // Parse one "key: value" block into an object (first value wins).
    function parseBlock(block) {
        const o = {};
        for (const line of block.split("\n")) {
            const i = line.indexOf(": ");
            if (i > 0) {
                const k = line.substring(0, i);
                if (!(k in o)) o[k] = line.substring(i + 2);
            }
        }
        return o;
    }

    // Parse a block of repeated "file:" entries into a list of objects.
    function parseList(block) {
        const out = [];
        let cur = null;
        for (const line of block.split("\n")) {
            const i = line.indexOf(": ");
            if (i <= 0) continue;
            const k = line.substring(0, i), v = line.substring(i + 2);
            if (k === "file") { cur = { file: v }; out.push(cur); }
            else if (cur) cur[k] = v;
        }
        return out;
    }

    Process {
        id: pollProc
        stdout: StdioCollector {
            onStreamFinished: {
                if (text.indexOf("OK MPD") !== 0) { root.available = false; return; }
                const parts = text.split("list_OK\n");
                if (parts.length < 4) { root.available = false; return; }
                root.available = true;
                const st = root.parseBlock(parts[0]);
                const song = root.parseBlock(parts[1]);
                root.state = st["state"] || "stop";
                root.songPos = parseInt(st["song"] || "-1");
                root.queueLength = parseInt(st["playlistlength"] || "0");
                root.title = song["Title"] || "";
                root.artist = song["Artist"] || "";
                root.name = song["Name"] || "";
                root.file = song["file"] || "";
                root.stations = root.parseList(parts[2]).map(e => ({ name: e["Name"] || e["Title"] || e.file, file: e.file }));
                root.queueFiles = root.parseList(parts[3]).map(e => e.file);
            }
        }
        onExited: (code) => { if (code !== 0) root.available = false }
    }

    Process {
        id: actionProc
        stdout: StdioCollector {}
        onExited: { root.busy = false; root.refresh(); }
    }

    Timer {
        interval: ShellState.panelOpen ? 2000 : 15000
        running: true
        repeat: true
        triggeredOnStart: true
        onTriggered: root.refresh()
    }

    Connections {
        target: ShellState
        function onPanelOpenChanged() { if (ShellState.panelOpen) root.refresh(); }
    }
}
