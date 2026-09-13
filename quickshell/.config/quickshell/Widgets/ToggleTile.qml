import QtQuick
import QtQuick.Layouts
import qs

// Quick-toggle tile: icon + label + subtitle. Accent-filled when active.
Rectangle {
    id: root

    property string icon: ""
    property string label: ""
    property string subtitle: ""
    property bool active: false
    property bool busy: false
    property bool enabled: true

    signal clicked()

    Layout.fillWidth: true
    implicitHeight: 56
    radius: Theme.radius
    color: active ? Theme.accent : (mouse.containsMouse && enabled ? Theme.bg3 : Theme.bg2)
    opacity: enabled ? 1 : 0.5

    Behavior on color { ColorAnimation { duration: 100 } }

    RowLayout {
        anchors {
            fill: parent
            leftMargin: Theme.padding
            rightMargin: Theme.padding
        }
        spacing: Theme.spacing

        Text {
            text: root.icon
            color: root.active ? Theme.bg : Theme.fg
            font.family: Theme.iconFont
            font.pixelSize: 20
        }

        ColumnLayout {
            Layout.fillWidth: true
            spacing: 0
            Label {
                text: root.label
                color: root.active ? Theme.bg : Theme.fg
                font.bold: true
                Layout.fillWidth: true
            }
            Label {
                text: root.busy ? "Working…" : root.subtitle
                color: root.active ? Qt.alpha(Theme.bg, 0.75) : Theme.gray
                font.pixelSize: Theme.fontSmall
                visible: text !== ""
                Layout.fillWidth: true
            }
        }
    }

    MouseArea {
        id: mouse
        anchors.fill: parent
        hoverEnabled: true
        enabled: root.enabled && !root.busy
        cursorShape: Qt.PointingHandCursor
        onClicked: root.clicked()
    }
}
