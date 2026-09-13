import QtQuick
import QtQuick.Layouts
import Quickshell
import qs
import qs.Widgets

// Clock/date on the left, DND + close on the right.
RowLayout {
    Layout.fillWidth: true
    spacing: Theme.spacing

    SystemClock {
        id: clock
        precision: SystemClock.Minutes
    }

    ColumnLayout {
        spacing: 0
        Label {
            text: Qt.formatTime(clock.date, "HH:mm")
            font.pixelSize: 28
            font.bold: true
        }
        Label {
            text: Qt.formatDate(clock.date, "dddd, d MMMM")
            dim: true
        }
    }

    Item { Layout.fillWidth: true }

    IconButton {
        icon: ShellState.dnd ? "󰂛" : "󰂚"
        active: ShellState.dnd
        onClicked: ShellState.dnd = !ShellState.dnd
    }

    IconButton {
        icon: "󰅖"
        onClicked: ShellState.close()
    }
}
