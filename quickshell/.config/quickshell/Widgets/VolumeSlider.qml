import QtQuick
import QtQuick.Layouts
import qs

// Icon button (mute) + slider + percentage. Bound to a PwNodeAudio.
RowLayout {
    id: root

    property var audio: null     // PwNodeAudio
    property string icon: "󰕾"
    property string mutedIcon: "󰖁"

    readonly property bool muted: audio ? audio.muted : false
    readonly property real volume: audio ? audio.volume : 0

    Layout.fillWidth: true
    spacing: Theme.spacing
    enabled: audio !== null
    opacity: enabled ? 1 : 0.5

    IconButton {
        icon: root.muted ? root.mutedIcon : root.icon
        iconColor: root.muted ? Theme.red : Theme.fg
        size: 30
        onClicked: if (root.audio) root.audio.muted = !root.audio.muted
    }

    Item {
        Layout.fillWidth: true
        implicitHeight: 20

        Rectangle {
            anchors.verticalCenter: parent.verticalCenter
            width: parent.width
            height: 6
            radius: 3
            color: Theme.bg3

            Rectangle {
                width: parent.width * Math.min(root.volume, 1)
                height: parent.height
                radius: 3
                color: root.muted ? Theme.gray : Theme.accent
            }
        }

        Rectangle {
            x: parent.width * Math.min(root.volume, 1) - width / 2
            anchors.verticalCenter: parent.verticalCenter
            width: 14
            height: 14
            radius: 7
            color: Theme.fg
            visible: mouse.containsMouse || mouse.pressed
        }

        MouseArea {
            id: mouse
            anchors.fill: parent
            hoverEnabled: true
            cursorShape: Qt.PointingHandCursor
            function setFrom(mx) {
                if (root.audio) root.audio.volume = Math.max(0, Math.min(1, mx / width));
            }
            onPressed: mouse => setFrom(mouse.x)
            onPositionChanged: mouse => { if (pressed) setFrom(mouse.x); }
            onWheel: wheel => {
                if (!root.audio) return;
                const step = wheel.angleDelta.y > 0 ? 0.05 : -0.05;
                root.audio.volume = Math.max(0, Math.min(1, root.audio.volume + step));
            }
        }
    }

    Label {
        text: Math.round(root.volume * 100) + "%"
        dim: true
        font.pixelSize: Theme.fontSmall
        horizontalAlignment: Text.AlignRight
        Layout.preferredWidth: 36
    }
}
