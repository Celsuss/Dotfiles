import QtQuick
import QtQuick.Layouts
import qs
import qs.Widgets
import qs.Services

// Scrollable list of live agent sessions.
Card {
    title: "Sessions"
    Layout.fillHeight: true

    Label {
        visible: Sessions.sessions.length === 0
        Layout.fillWidth: true
        dim: true
        wrapMode: Text.WordWrap
        text: "No agent sessions.\n\nClaude Code sessions appear once ai-dash-hook is registered in ~/.claude/settings.json; OpenCode is found by process scan."
    }

    Flickable {
        Layout.fillWidth: true
        Layout.fillHeight: true
        contentHeight: rows.implicitHeight
        clip: true
        boundsBehavior: Flickable.StopAtBounds

        ColumnLayout {
            id: rows
            width: parent.width
            spacing: Theme.spacing

            Repeater {
                model: Sessions.sessions
                delegate: SessionRow {
                    required property var modelData
                    session: modelData
                }
            }
        }
    }
}
