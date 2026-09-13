import Quickshell
import Quickshell.Io
import qs
import qs.ControlCenter
import qs.AiDash
import qs.Notifications

ShellRoot {
    Panel {}
    Dashboard {}
    Popups {}

    // `quickshell ipc call controlCenter toggle` (also open/close).
    IpcHandler {
        target: "controlCenter"
        function toggle(): void { ShellState.toggle() }
        function open(): void   { ShellState.open() }
        function close(): void  { ShellState.close() }
    }

    // `quickshell ipc call aiDash toggle` (also open/close).
    IpcHandler {
        target: "aiDash"
        function toggle(): void { ShellState.toggleAiDash() }
        function open(): void   { ShellState.openAiDash() }
        function close(): void  { ShellState.closeAiDash() }
    }
}
