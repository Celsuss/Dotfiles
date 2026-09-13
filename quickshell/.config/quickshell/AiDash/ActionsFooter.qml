import QtQuick
import QtQuick.Layouts
import qs
import qs.Widgets
import qs.Services

// Launchers: new Claude Code session in a recent project, Emacs, Ollama restart.
ColumnLayout {
    id: root
    Layout.fillWidth: true
    spacing: Theme.spacing

    property bool picking: false

    RowLayout {
        Layout.fillWidth: true
        spacing: Theme.spacing

        Button {
            text: root.picking ? "󰅖  cancel" : "󰐕  new claude session"
            accent: !root.picking
            onClicked: root.picking = !root.picking
        }
        Button {
            text: "󰘧  emacs"
            onClicked: Launcher.focusEmacs()
        }
        Item { Layout.fillWidth: true }
        Button {
            text: "󰑐  restart ollama"
            onClicked: Ollama.restart()
        }
    }

    // Project picker: recent Claude Code working directories.
    Flow {
        visible: root.picking
        Layout.fillWidth: true
        spacing: 4

        Repeater {
            model: Launcher.recentProjects
            delegate: Button {
                required property string modelData
                text: modelData.replace(/^\/home\/[^/]+/, "~")
                onClicked: { root.picking = false; Launcher.newClaude(modelData); }
            }
        }

        Label {
            visible: Launcher.recentProjects.length === 0
            dim: true
            text: "No recent projects found in ~/.claude/projects"
        }
    }
}
