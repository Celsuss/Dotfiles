import QtQuick
import qs

// Square icon button using Nerd Font glyphs.
Rectangle {
    id: root

    property string icon: ""
    property string tooltip: ""
    property color iconColor: Theme.fg
    property color hoverColor: Theme.bg3
    property bool active: false
    property int size: 36

    signal clicked()

    implicitWidth: size
    implicitHeight: size
    radius: Theme.radius
    color: active ? Theme.accent : (mouse.containsMouse ? hoverColor : "transparent")

    Behavior on color { ColorAnimation { duration: 100 } }

    Text {
        anchors.centerIn: parent
        text: root.icon
        color: root.active ? Theme.bg : root.iconColor
        font.family: Theme.iconFont
        font.pixelSize: root.size * 0.5
    }

    MouseArea {
        id: mouse
        anchors.fill: parent
        hoverEnabled: true
        cursorShape: Qt.PointingHandCursor
        onClicked: root.clicked()
    }
}
