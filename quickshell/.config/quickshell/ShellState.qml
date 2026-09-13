pragma Singleton
import QtQuick
import Quickshell

// Global UI state shared between the IPC handler, panel and widgets.
Singleton {
    id: root

    property bool panelOpen: false

    // Do Not Disturb. Wired to the notification server in phase 5;
    // until then it is just a flag shown in the header.
    property bool dnd: false

    function open()   { panelOpen = true }
    function close()  { panelOpen = false }
    function toggle() { panelOpen = !panelOpen }
}
