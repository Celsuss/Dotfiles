import QtQuick
import QtQuick.Layouts
import qs

// Inline dropdown: a button showing the current value; clicking expands a
// filterable list below it (no popup windows on layer-shell).
ColumnLayout {
    id: root

    property string placeholder: "Select…"
    property string current: ""
    property var items: []          // list of strings
    property bool expanded: false
    property int maxListHeight: 180
    property bool filterable: items.length > 8

    signal selected(string value)

    Layout.fillWidth: true
    spacing: 4

    Rectangle {
        Layout.fillWidth: true
        implicitHeight: 32
        radius: Theme.radius
        color: headMouse.containsMouse ? Theme.bg3 : Theme.bg2

        RowLayout {
            anchors {
                fill: parent
                leftMargin: Theme.padding
                rightMargin: Theme.padding
            }
            Label {
                text: root.current !== "" ? root.current : root.placeholder
                dim: root.current === ""
                Layout.fillWidth: true
            }
            Text {
                text: root.expanded ? "󰅃" : "󰅀"
                color: Theme.gray
                font.family: Theme.iconFont
                font.pixelSize: 14
            }
        }

        MouseArea {
            id: headMouse
            anchors.fill: parent
            hoverEnabled: true
            cursorShape: Qt.PointingHandCursor
            onClicked: {
                root.expanded = !root.expanded;
                if (root.expanded && root.filterable) filter.forceActiveFocus();
            }
        }
    }

    Rectangle {
        visible: root.expanded
        Layout.fillWidth: true
        implicitHeight: listCol.implicitHeight + 8
        radius: Theme.radius
        color: Theme.bg2
        border.color: Theme.border
        border.width: 1

        ColumnLayout {
            id: listCol
            anchors {
                fill: parent
                margins: 4
            }
            spacing: 4

            Rectangle {
                visible: root.filterable
                Layout.fillWidth: true
                implicitHeight: 28
                radius: Theme.radius - 4
                color: Theme.bg1

                TextInput {
                    id: filter
                    anchors {
                        fill: parent
                        leftMargin: 8
                        rightMargin: 8
                    }
                    verticalAlignment: TextInput.AlignVCenter
                    color: Theme.fg
                    font.family: Theme.font
                    font.pixelSize: Theme.fontSize
                    clip: true
                    Keys.onEscapePressed: root.expanded = false
                    Keys.onReturnPressed: if (list.count > 0) root.pick(list.itemAtIndex(0).value)

                    Label {
                        anchors.fill: parent
                        verticalAlignment: Text.AlignVCenter
                        text: "Filter…"
                        dim: true
                        visible: filter.text === ""
                    }
                }
            }

            ListView {
                id: list
                Layout.fillWidth: true
                implicitHeight: Math.min(contentHeight, root.maxListHeight)
                clip: true
                boundsBehavior: Flickable.StopAtBounds
                model: {
                    const q = filter.text.toLowerCase();
                    return root.items.filter(s => q === "" || s.toLowerCase().indexOf(q) !== -1);
                }
                delegate: Rectangle {
                    required property string modelData
                    readonly property string value: modelData
                    width: list.width
                    height: 28
                    radius: Theme.radius - 4
                    color: modelData === root.current ? Qt.alpha(Theme.accent, 0.25)
                         : (itemMouse.containsMouse ? Theme.bg3 : "transparent")

                    Label {
                        anchors {
                            fill: parent
                            leftMargin: 8
                        }
                        verticalAlignment: Text.AlignVCenter
                        text: modelData
                    }

                    MouseArea {
                        id: itemMouse
                        anchors.fill: parent
                        hoverEnabled: true
                        cursorShape: Qt.PointingHandCursor
                        onClicked: root.pick(modelData)
                    }
                }
            }
        }
    }

    function pick(value) {
        root.expanded = false;
        filter.text = "";
        root.selected(value);
    }
}
