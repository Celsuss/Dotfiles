pragma Singleton
import QtQuick
import Quickshell
import Quickshell.Io
import qs

// NordVPN state via the `nordvpn` CLI. Status is polled; actions run
// one-shot processes and re-poll when they finish.
Singleton {
    id: root

    property bool connected: false
    property bool connecting: false
    property string server: ""     // e.g. "Sweden #123"
    property string hostname: ""
    property string country: ""
    property string city: ""
    property string ip: ""
    property string technology: ""
    property string transfer: ""
    property string uptime: ""

    property bool killSwitch: false
    property var countries: []
    property bool busy: false      // an action process is running
    property string error: ""
    property bool available: true  // false if the CLI is missing / daemon down

    readonly property string summary: {
        if (!available) return "nordvpn unavailable";
        if (connecting) return "Connecting…";
        if (!connected) return "Disconnected";
        const place = [city, country].filter(s => s !== "").join(", ");
        return place !== "" ? place : server;
    }

    function refresh() {
        if (!statusProc.running) statusProc.running = true;
        if (!settingsProc.running) settingsProc.running = true;
    }

    function loadCountries() {
        if (countries.length === 0 && !countriesProc.running) countriesProc.running = true;
    }

    function connect(target) {
        runAction(target ? ["nordvpn", "connect", target] : ["nordvpn", "connect"]);
    }

    function disconnect() { runAction(["nordvpn", "disconnect"]); }

    function toggle() { connected ? disconnect() : connect(); }

    function setKillSwitch(on) {
        runAction(["nordvpn", "set", "killswitch", on ? "on" : "off"]);
    }

    function runAction(cmd) {
        if (actionProc.running) return;
        error = "";
        busy = true;
        actionProc.command = cmd;
        actionProc.running = true;
    }

    // "Key: Value" lines -> object. The CLI prints spinner glyphs and \r
    // while working; strip anything before the last \r on each line.
    function parseKv(text) {
        const out = {};
        for (let line of text.split("\n")) {
            line = line.substring(line.lastIndexOf("\r") + 1).trim();
            const i = line.indexOf(":");
            if (i > 0) out[line.substring(0, i).trim()] = line.substring(i + 1).trim();
        }
        return out;
    }

    Process {
        id: statusProc
        command: ["nordvpn", "status"]
        stdout: StdioCollector {
            onStreamFinished: {
                const kv = root.parseKv(text);
                const status = (kv["Status"] || "").toLowerCase();
                root.available = status !== "";
                root.connected = status === "connected";
                root.connecting = status === "connecting";
                root.server = kv["Server"] || "";
                root.hostname = kv["Hostname"] || "";
                root.country = kv["Country"] || "";
                root.city = kv["City"] || "";
                root.ip = kv["IP"] || kv["Server IP"] || "";
                root.technology = kv["Current technology"] || "";
                root.transfer = kv["Transfer"] || "";
                root.uptime = kv["Uptime"] || "";
            }
        }
        onExited: (code) => { if (code !== 0) root.available = false }
    }

    Process {
        id: settingsProc
        command: ["nordvpn", "settings"]
        stdout: StdioCollector {
            onStreamFinished: {
                const kv = root.parseKv(text);
                root.killSwitch = (kv["Kill Switch"] || "").toLowerCase() === "enabled";
            }
        }
    }

    Process {
        id: countriesProc
        command: ["nordvpn", "countries"]
        stdout: StdioCollector {
            onStreamFinished: {
                root.countries = text.split(/\s+/)
                    .map(s => s.substring(s.lastIndexOf("\r") + 1).trim())
                    .filter(s => s !== "" && /^[A-Za-z_]+$/.test(s));
            }
        }
    }

    Process {
        id: actionProc
        stdout: StdioCollector { id: actionOut }
        stderr: StdioCollector { id: actionErr }
        onExited: (code) => {
            root.busy = false;
            if (code !== 0) {
                const msg = (actionErr.text + "\n" + actionOut.text).trim();
                root.error = msg.substring(msg.lastIndexOf("\r") + 1).trim() || ("nordvpn exited with " + code);
            }
            root.refresh();
        }
    }

    // Fast polling while the panel is open, slow otherwise.
    Timer {
        interval: ShellState.panelOpen ? 3000 : 30000
        running: true
        repeat: true
        triggeredOnStart: true
        onTriggered: root.refresh()
    }

    Connections {
        target: ShellState
        function onPanelOpenChanged() {
            if (ShellState.panelOpen) { root.refresh(); root.loadCountries(); }
        }
    }
}
