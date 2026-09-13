pragma Singleton
import QtQuick
import Quickshell
import Quickshell.Io
import qs

// Tailscale state via `tailscale status --json`.
Singleton {
    id: root

    property string backendState: "Stopped" // Running | Stopped | NeedsLogin | Starting | NoState
    readonly property bool running: backendState === "Running"
    readonly property bool needsLogin: backendState === "NeedsLogin"
    property string ip: ""
    property string dnsName: ""
    property string tailnet: ""
    property int peersOnline: 0
    property int peersTotal: 0
    property var exitNodes: []       // [{ name, id, online }]
    property string exitNode: ""     // DNS short name of the active exit node
    property string exitNodeId: ""
    property var health: []

    property bool busy: false
    property string error: ""
    property bool available: true
    // Set when the CLI refuses because the user is not the tailscale operator.
    property bool accessDenied: false

    readonly property string summary: {
        if (!available) return "tailscale unavailable";
        if (needsLogin) return "Needs login";
        if (!running) return backendState === "Starting" ? "Starting…" : "Down";
        return exitNode !== "" ? "Up · exit " + exitNode : "Up · " + peersOnline + " peers";
    }

    function refresh() { if (!statusProc.running) statusProc.running = true; }
    function up()      { runAction(["tailscale", "up"]); }
    function down()    { runAction(["tailscale", "down"]); }
    function toggle()  { running ? down() : up(); }
    function setExitNode(name) { runAction(["tailscale", "set", "--exit-node=" + name]); }

    function runAction(cmd) {
        if (actionProc.running) return;
        error = "";
        busy = true;
        actionProc.command = cmd;
        actionProc.running = true;
    }

    function shortName(dns) {
        return dns.replace(/\.$/, "").split(".")[0];
    }

    Process {
        id: statusProc
        command: ["tailscale", "status", "--json"]
        stdout: StdioCollector {
            onStreamFinished: {
                let st;
                try { st = JSON.parse(text); } catch (e) { root.available = false; return; }
                root.available = true;
                root.backendState = st.BackendState || "NoState";
                root.ip = (st.TailscaleIPs && st.TailscaleIPs.length) ? st.TailscaleIPs[0] : "";
                root.dnsName = st.Self ? root.shortName(st.Self.DNSName || "") : "";
                root.tailnet = (st.CurrentTailnet && st.CurrentTailnet.Name) || "";
                root.health = st.Health || [];

                const peers = st.Peer ? Object.values(st.Peer) : [];
                root.peersTotal = peers.length;
                root.peersOnline = peers.filter(p => p.Online).length;
                root.exitNodes = peers
                    .filter(p => p.ExitNodeOption)
                    .map(p => ({ name: root.shortName(p.DNSName), id: p.ID, online: p.Online }))
                    .sort((a, b) => a.name.localeCompare(b.name));
                const active = peers.find(p => p.ExitNode);
                root.exitNode = active ? root.shortName(active.DNSName) : "";
                root.exitNodeId = active ? active.ID : "";
            }
        }
        onExited: (code) => { if (code !== 0) root.available = false }
    }

    Process {
        id: actionProc
        stdout: StdioCollector { id: actionOut }
        stderr: StdioCollector { id: actionErr }
        onExited: (code) => {
            root.busy = false;
            const msg = (actionErr.text + "\n" + actionOut.text).trim();
            root.accessDenied = msg.indexOf("Access denied") !== -1;
            if (code !== 0 || root.accessDenied) root.error = msg.split("\n")[0] || ("tailscale exited with " + code);
            // `tailscale up` returns before the backend settles.
            settleTimer.restart();
            root.refresh();
        }
    }

    Timer { id: settleTimer; interval: 1500; onTriggered: root.refresh() }

    Timer {
        interval: ShellState.panelOpen ? 3000 : 30000
        running: true
        repeat: true
        triggeredOnStart: true
        onTriggered: root.refresh()
    }
}
