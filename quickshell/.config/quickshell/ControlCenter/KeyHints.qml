import QtQuick
import QtQuick.Layouts
import qs
import qs.Widgets

// Keybinding cheatsheet, toggled with `?`.
Rectangle {
    id: root

    readonly property var bindings: [
        { keys: "j / k",        what: "next / previous control" },
        { keys: "gg / G",       what: "first / last control" },
        { keys: "C-d / C-u",    what: "jump five controls" },
        { keys: "Enter / Space", what: "activate" },
        { keys: "h / l",        what: "slider, switch, dropdown, group" },
        { keys: "m",            what: "mute (on a volume slider)" },
        { keys: "d",            what: "dismiss notification / group" },
        { keys: "D",            what: "clear all notifications" },
        { keys: "C-n / C-p",    what: "move inside an open dropdown" },
        { keys: "q / Esc",      what: "close the panel" },
        { keys: "?",            what: "close this list" }
    ]

    anchors.fill: parent
    color: Qt.alpha(Theme.bg, 0.92)

    // Swallow clicks so nothing behind the sheet reacts.
    MouseArea {
        anchors.fill: parent
        onClicked: root.visible = false
    }

    ColumnLayout {
        anchors {
            fill: parent
            margins: Theme.padding * 2
        }
        spacing: Theme.spacing

        Item { Layout.fillHeight: true }

        Label {
            text: "Keyboard"
            font.pixelSize: Theme.fontLarge
            font.bold: true
            Layout.bottomMargin: Theme.spacing
        }

        Repeater {
            model: root.bindings

            RowLayout {
                id: bindingRow
                required property var modelData
                Layout.fillWidth: true
                spacing: Theme.spacing

                Rectangle {
                    Layout.preferredWidth: 110
                    implicitHeight: 24
                    radius: Theme.radius - 4
                    color: Theme.bg2

                    Label {
                        anchors.centerIn: parent
                        text: bindingRow.modelData.keys
                        color: Theme.accent
                        font.pixelSize: Theme.fontSmall
                        font.bold: true
                    }
                }

                Label {
                    text: bindingRow.modelData.what
                    dim: true
                    font.pixelSize: Theme.fontSmall
                    Layout.fillWidth: true
                }
            }
        }

        Item { Layout.fillHeight: true }
    }
}
