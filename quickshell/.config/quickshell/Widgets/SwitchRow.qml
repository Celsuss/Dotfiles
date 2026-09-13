import QtQuick
import QtQuick.Layouts
import qs

// Label on the left, pill switch on the right.
RowLayout {
    id: root

    property string text: ""
    property bool checked: false
    property bool enabled: true

    signal toggled(bool checked)

    Layout.fillWidth: true
    spacing: Theme.spacing
    opacity: enabled ? 1 : 0.5

    Label {
        text: root.text
        Layout.fillWidth: true
    }

    Rectangle {
        implicitWidth: 40
        implicitHeight: 22
        radius: height / 2
        color: root.checked ? Theme.accent : Theme.bg3

        Behavior on color { ColorAnimation { duration: 100 } }

        Rectangle {
            width: 16
            height: 16
            radius: 8
            y: 3
            x: root.checked ? parent.width - width - 3 : 3
            color: root.checked ? Theme.bg : Theme.fg
            Behavior on x { NumberAnimation { duration: 100 } }
        }

        MouseArea {
            anchors.fill: parent
            enabled: root.enabled
            cursorShape: Qt.PointingHandCursor
            onClicked: root.toggled(!root.checked)
        }
    }
}
