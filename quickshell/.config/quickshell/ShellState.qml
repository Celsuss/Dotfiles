pragma Singleton
import QtQuick
import Quickshell

// Global UI state shared between the IPC handler, panels and widgets.
Singleton {
    id: root

    property bool panelOpen: false
    property bool aiDashOpen: false

    // Do Not Disturb. Wired to the notification server in phase 5;
    // until then it is just a flag shown in the header.
    property bool dnd: false

    // Only one overlay at a time: opening one closes the other so the
    // focus grabs don't fight.
    function open()   { aiDashOpen = false; panelOpen = true }
    function close()  { panelOpen = false }
    function toggle() { panelOpen ? close() : open() }

    function openAiDash()   { panelOpen = false; aiDashOpen = true }
    function closeAiDash()  { aiDashOpen = false }
    function toggleAiDash() { aiDashOpen ? closeAiDash() : openAiDash() }
}
