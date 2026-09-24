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

    function set(on) { if (root.enabled) root.toggled(on) }

    Layout.fillWidth: true
    spacing: Theme.spacing
    opacity: enabled ? 1 : 0.5

    Label {
        text: root.text
        Layout.fillWidth: true
    }

    // The pill is the focus stop -- a RowLayout can't hold a FocusRing.
    Rectangle {
        implicitWidth: 40
        implicitHeight: 22
        radius: height / 2
        color: root.checked ? Theme.accent : Theme.bg3

        activeFocusOnTab: root.enabled

        Keys.onReturnPressed: root.set(!root.checked)
        Keys.onEnterPressed:  root.set(!root.checked)
        Keys.onSpacePressed:  root.set(!root.checked)
        Keys.onPressed: event => {
            if (event.key === Qt.Key_H || event.key === Qt.Key_Left) {
                root.set(false);
                event.accepted = true;
            } else if (event.key === Qt.Key_L || event.key === Qt.Key_Right) {
                root.set(true);
                event.accepted = true;
            }
        }

        Behavior on color { ColorAnimation { duration: 100 } }

        FocusRing {}

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
