import Quickshell
import Quickshell.Io
import qs
import qs.ControlCenter
import qs.Notifications

ShellRoot {
    Panel {}
    Popups {}

    // `quickshell ipc call controlCenter toggle` (also open/close).
    IpcHandler {
        target: "controlCenter"
        function toggle(): void { ShellState.toggle() }
        function open(): void   { ShellState.open() }
        function close(): void  { ShellState.close() }
    }
}
