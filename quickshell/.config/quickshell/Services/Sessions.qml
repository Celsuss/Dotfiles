pragma Singleton
import QtQuick
import Quickshell
import Quickshell.Io
import qs

// Live AI agent sessions. Reads the registry written by `ai-dash-hook`
// ($XDG_RUNTIME_DIR/ai-dash/sessions/*.json, one file per session, see the
// script for the schema), prunes files whose pid is gone, and adds OpenCode
// instances found by process scan (no hook integration yet).
Singleton {
    id: root

    property var sessions: []       // sorted: waiting, working, idle, unknown
    property int waiting: 0
    property int working: 0
    property bool loading: false

    readonly property string dir: Quickshell.env("XDG_RUNTIME_DIR") + "/ai-dash/sessions"

    readonly property string script: "
d=\"" + dir + "\"
for f in \"$d\"/*.json; do
  [ -f \"$f\" ] || continue
  pid=$(jq -r '.pid // empty' \"$f\")
  if [ -n \"$pid\" ] && [ ! -d \"/proc/$pid\" ]; then rm -f \"$f\"; continue; fi
  t=$(jq -r '.transcript // empty' \"$f\")
  m=0; [ -n \"$t\" ] && [ -f \"$t\" ] && m=$(stat -c %Y \"$t\")
  pp=$(awk '{print $4}' /proc/$pid/stat 2>/dev/null); [ -n \"$pp\" ] || pp=0
  jq -c --argjson m \"$m\" --argjson pp \"$pp\" '. + {transcript_mtime: $m, ppid: $pp}' \"$f\"
done
for p in $(pgrep -x opencode); do
  cwd=$(readlink /proc/$p/cwd 2>/dev/null) || continue
  printf '{\"tool\":\"opencode\",\"id\":\"pid-%s\",\"pid\":%s,\"cwd\":\"%s\",\"frontend\":\"terminal\",\"state\":\"unknown\",\"started\":%s}\\n' \"$p\" \"$p\" \"$cwd\" \"$(stat -c %Y /proc/$p)\"
done"

    readonly property var stateOrder: ({ waiting: 0, working: 1, idle: 2, unknown: 3 })

    function refresh() {
        if (proc.running) return;
        loading = true;
        proc.running = true;
    }

    function project(s) {
        const p = (s.cwd || "").replace(/\/+$/, "");
        return p.substring(p.lastIndexOf("/") + 1) || p || "?";
    }

    function elapsed(s) {
        const secs = Math.max(0, Math.floor(Date.now() / 1000) - (s.started || 0));
        if (secs < 60) return secs + "s";
        if (secs < 3600) return Math.floor(secs / 60) + "m";
        return Math.floor(secs / 3600) + "h " + (Math.floor(secs / 60) % 60) + "m";
    }

    function kill(s) { Quickshell.execDetached(["kill", "-TERM", String(s.pid)]); }

    Process {
        id: proc
        command: ["sh", "-c", root.script]
        stdout: StdioCollector {
            onStreamFinished: {
                const list = [];
                const seenOpencode = {};   // cwd -> keep the oldest pid per project
                for (const line of text.split("\n")) {
                    if (line.trim() === "") continue;
                    let s;
                    try { s = JSON.parse(line); } catch (e) { continue; }
                    if (!s.state) s.state = "unknown";
                    if (s.tool === "opencode") {
                        const prev = seenOpencode[s.cwd];
                        if (prev && prev.pid < s.pid) continue;
                        if (prev) list.splice(list.indexOf(prev), 1);
                        seenOpencode[s.cwd] = s;
                    }
                    list.push(s);
                }
                list.sort((a, b) => {
                    const d = (root.stateOrder[a.state] ?? 9) - (root.stateOrder[b.state] ?? 9);
                    return d !== 0 ? d : (a.started || 0) - (b.started || 0);
                });
                root.sessions = list;
                root.waiting = list.filter(s => s.state === "waiting").length;
                root.working = list.filter(s => s.state === "working").length;
            }
        }
        onExited: root.loading = false
    }

    Timer {
        interval: 2000
        running: ShellState.aiDashOpen
        repeat: true
        triggeredOnStart: true
        onTriggered: root.refresh()
    }
}
