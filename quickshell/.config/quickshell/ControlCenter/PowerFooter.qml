import QtQuick
import QtQuick.Layouts
import Quickshell
import qs
import qs.Widgets

// Session/power actions. Destructive ones require a second click within 3s.
RowLayout {
    id: root
    Layout.fillWidth: true
    spacing: Theme.spacing

    property string pending: ""

    Timer {
        id: pendingReset
        interval: 3000
        onTriggered: root.pending = ""
    }

    function run(name, cmd, confirm) {
        if (confirm && root.pending !== name) {
            root.pending = name;
            pendingReset.restart();
            return;
        }
        root.pending = "";
        pendingReset.stop();
        ShellState.close();
        Quickshell.execDetached(cmd);
    }

    component PowerButton: IconButton {
        required property string name
        required property var cmd
        property bool confirm: false
        size: 40
        active: root.pending === name
        hoverColor: confirm ? Qt.alpha(Theme.red, 0.3) : Theme.bg3
        onClicked: root.run(name, cmd, confirm)
    }

    Label {
        text: root.pending !== "" ? "Click again to " + root.pending : ""
        color: Theme.yellow
        Layout.fillWidth: true
    }

    PowerButton { name: "lock";     icon: "󰌾"; cmd: ["hyprlock"] }
    PowerButton { name: "suspend";  icon: "󰒲"; cmd: ["systemctl", "suspend"] }
    PowerButton { name: "log out";  icon: "󰗽"; cmd: ["hyprctl", "dispatch", "exit"]; confirm: true }
    PowerButton { name: "reboot";   icon: "󰜉"; cmd: ["systemctl", "reboot"]; confirm: true }
    PowerButton { name: "shutdown"; icon: "󰐥"; cmd: ["systemctl", "poweroff"]; confirm: true }
}
