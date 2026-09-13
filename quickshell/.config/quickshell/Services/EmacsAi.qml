pragma Singleton
import QtQuick
import Quickshell
import Quickshell.Io
import qs

// gptel / ellama / agent-shell state from the running Emacs daemon, polled
// with `emacsclient --eval` of Services/ai-dash-status.el (nothing needs to
// be added to the Emacs config; packages that aren't loaded report []).
Singleton {
    id: root

    property bool available: false
    property var gptel: []          // [{ buffer, backend, model, busy }]
    property var gptelDefault: null // { backend, model }
    property int gptelBusy: 0
    property var ellama: []         // [{ buffer, model, busy }]
    property string ellamaDefault: ""
    property var agentShell: []     // [{ buffer, agent, model, title, cwd, busy, pid }]

    readonly property int busyCount: gptelBusy
        + ellama.filter(e => e.busy).length
        + agentShell.filter(e => e.busy).length

    readonly property string script:
        "emacsclient --eval \"$(cat '" + Quickshell.shellDir + "/Services/ai-dash-status.el')\" 2>/dev/null"

    function refresh() { if (!proc.running) proc.running = true; }

    // The pid the hook records is the `claude` child of the ACP adapter, so
    // an agent-shell entry matches a session by pid or parent pid.
    function shellFor(session) {
        return agentShell.find(a => a.pid && (a.pid === session.pid || a.pid === session.ppid)) || null;
    }

    Process {
        id: proc
        command: ["sh", "-c", root.script]
        stdout: StdioCollector {
            onStreamFinished: {
                const raw = text.trim();
                if (raw === "") { root.available = false; return; }
                let data;
                try {
                    // emacsclient prints the result as an elisp string literal.
                    data = JSON.parse(JSON.parse(raw));
                } catch (e) {
                    root.available = false;
                    return;
                }
                root.available = true;
                root.gptel = data.gptel || [];
                root.gptelDefault = data.gptel_default || null;
                root.gptelBusy = data.gptel_busy || 0;
                root.ellama = data.ellama || [];
                root.ellamaDefault = data.ellama_default || "";
                root.agentShell = data.agent_shell || [];
            }
        }
    }

    Timer {
        interval: 5000
        running: ShellState.aiDashOpen
        repeat: true
        triggeredOnStart: true
        onTriggered: root.refresh()
    }
}
