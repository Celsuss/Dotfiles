import QtQuick
import qs

// Small colored status pill ("waiting", "working", ...). `pulse` fades the
// pill in and out to draw the eye.
Rectangle {
    id: root

    property string text: ""
    property color tint: Theme.gray
    property bool pulse: false

    implicitWidth: label.implicitWidth + 14
    implicitHeight: 20
    radius: 10
    color: Qt.alpha(tint, 0.2)
    border.color: Qt.alpha(tint, 0.6)
    border.width: 1

    Label {
        id: label
        anchors.centerIn: parent
        text: root.text
        color: root.tint
        font.pixelSize: Theme.fontSmall
        font.bold: true
    }

    SequentialAnimation on opacity {
        running: root.pulse
        loops: Animation.Infinite
        onRunningChanged: if (!running) root.opacity = 1
        NumberAnimation { to: 0.35; duration: 700; easing.type: Easing.InOutSine }
        NumberAnimation { to: 1;    duration: 700; easing.type: Easing.InOutSine }
    }
}
