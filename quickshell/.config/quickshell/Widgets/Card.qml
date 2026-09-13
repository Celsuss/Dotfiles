import QtQuick
import QtQuick.Layouts
import qs

// Rounded section container with optional title. Children go into `content`.
Rectangle {
    id: root

    property string title: ""
    default property alias content: body.data

    Layout.fillWidth: true
    implicitHeight: column.implicitHeight + Theme.padding * 2
    color: Theme.bg1
    radius: Theme.radius
    border.color: Theme.border
    border.width: 1

    ColumnLayout {
        id: column
        anchors {
            fill: parent
            margins: Theme.padding
        }
        spacing: Theme.spacing

        Text {
            visible: root.title !== ""
            text: root.title
            color: Theme.gray
            font.family: Theme.font
            font.pixelSize: Theme.fontSmall
            font.bold: true
            font.capitalization: Font.AllUppercase
        }

        ColumnLayout {
            id: body
            Layout.fillWidth: true
            spacing: Theme.spacing
        }
    }
}
