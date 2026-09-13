pragma Singleton
import QtQuick
import Quickshell
import Quickshell.Io
import qs

// Window focusing and launching for the AI dashboard's actions. Hyprland
// knows the window's pid (the terminal or Emacs), so a session's window is
// found by walking the agent's ancestors until one matches a client.
Singleton {
    id: root

    property var recentProjects: []   // absolute paths, most recently used first

    readonly property string terminal: "kitty"

    readonly property string focusScript: "
pid=$1
clients=$(hyprctl clients -j)
while [ \"$pid\" -gt 1 ] 2>/dev/null; do
  addr=$(printf '%s' \"$clients\" | jq -r --argjson p \"$pid\" '.[] | select(.pid == $p) | .address' | head -1)
  if [ -n \"$addr\" ]; then hyprctl dispatch focuswindow \"address:$addr\" >/dev/null; exit 0; fi
  pid=$(awk '{print $4}' /proc/$pid/stat 2>/dev/null) || exit 1
done
exit 1"

    readonly property string emacsScript: "
addr=$(hyprctl clients -j | jq -r '.[] | select(.class | test(\"^emacs\"; \"i\")) | .address' | head -1)
if [ -n \"$addr\" ]; then hyprctl dispatch focuswindow \"address:$addr\" >/dev/null; else emacsclient -c -n; fi"

    // Projects Claude Code was last used in, taken from each project dir's
    // newest transcript (the dir name is a lossy encoding of the path).
    readonly property string projectsScript: "
for d in \"$HOME/.claude/projects\"/*/; do
  f=$(ls -t \"$d\"*.jsonl 2>/dev/null | head -1)
  [ -n \"$f\" ] || continue
  cwd=$(head -c 8000 \"$f\" | jq -r 'select(.cwd) | .cwd' 2>/dev/null | head -1)
  [ -n \"$cwd\" ] && [ -d \"$cwd\" ] && printf '%s\\t%s\\n' \"$(stat -c %Y \"$f\")\" \"$cwd\"
done | sort -rn | cut -f2 | awk '!seen[$0]++' | head -15"

    function focusSession(s) {
        focusProc.command = ["sh", "-c", root.focusScript, "sh", String(s.pid)];
        focusProc.running = true;
        ShellState.closeAiDash();
    }

    function focusEmacs() {
        Quickshell.execDetached(["sh", "-c", root.emacsScript]);
        ShellState.closeAiDash();
    }

    function newClaude(cwd) {
        Quickshell.execDetached([root.terminal, "--directory", cwd, "claude"]);
        ShellState.closeAiDash();
    }

    function refreshProjects() { if (!projectsProc.running) projectsProc.running = true; }

    Process { id: focusProc }

    Process {
        id: projectsProc
        command: ["sh", "-c", root.projectsScript]
        stdout: StdioCollector {
            onStreamFinished: root.recentProjects = text.split("\n").filter(l => l !== "")
        }
    }

    Connections {
        target: ShellState
        function onAiDashOpenChanged() { if (ShellState.aiDashOpen) root.refreshProjects() }
    }
}
