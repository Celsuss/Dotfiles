import QtQuick
import qs

// Text button. `accent` fills it with the accent color.
Rectangle {
    id: root

    property string text: ""
    property bool accent: false
    property bool enabled: true
    property color textColor: accent ? Theme.bg : Theme.fg

    signal clicked()

    implicitWidth: label.implicitWidth + Theme.padding * 2
    implicitHeight: 32
    radius: Theme.radius
    color: accent ? (mouse.containsMouse ? Qt.lighter(Theme.accent, 1.1) : Theme.accent)
                  : (mouse.containsMouse ? Theme.bg3 : Theme.bg2)
    opacity: enabled ? 1 : 0.5

    Behavior on color { ColorAnimation { duration: 100 } }

    Label {
        id: label
        anchors.centerIn: parent
        text: root.text
        color: root.textColor
        font.bold: root.accent
    }

    MouseArea {
        id: mouse
        anchors.fill: parent
        hoverEnabled: true
        enabled: root.enabled
        cursorShape: Qt.PointingHandCursor
        onClicked: root.clicked()
    }
}
