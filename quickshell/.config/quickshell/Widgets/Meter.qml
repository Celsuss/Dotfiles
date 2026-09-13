import QtQuick
import QtQuick.Layouts
import qs

// Stat tile: label, big value, small detail and a thin fill bar.
Rectangle {
    id: root

    property string label: ""
    property string value: ""
    property string detail: ""
    property real fill: 0          // 0..1
    property color barColor: fill > 0.9 ? Theme.red : fill > 0.7 ? Theme.yellow : Theme.aqua

    Layout.fillWidth: true
    implicitHeight: col.implicitHeight + Theme.padding * 1.5
    radius: Theme.radius
    color: Theme.bg2

    ColumnLayout {
        id: col
        anchors {
            fill: parent
            margins: Theme.padding * 0.75
        }
        spacing: 2

        Label { text: root.label; dim: true; font.pixelSize: Theme.fontSmall; font.bold: true }
        Label { text: root.value; font.pixelSize: Theme.fontLarge; font.bold: true }
        Label { text: root.detail; dim: true; font.pixelSize: Theme.fontSmall; visible: text !== "" }

        Rectangle {
            Layout.fillWidth: true
            Layout.topMargin: 4
            implicitHeight: 4
            radius: 2
            color: Theme.bg3
            Rectangle {
                width: parent.width * Math.max(0, Math.min(1, root.fill))
                height: parent.height
                radius: 2
                color: root.barColor
                Behavior on width { NumberAnimation { duration: 300 } }
            }
        }
    }
}
