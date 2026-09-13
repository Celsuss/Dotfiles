import QtQuick
import QtQuick.Layouts
import Quickshell
import Quickshell.Wayland
import qs
import qs.Widgets
import qs.Services

// Toast stack in the top-right corner. The window only exists while there
// are toasts and is masked to them so clicks elsewhere fall through.
PanelWindow {
    id: win

    readonly property int toastWidth: 380

    visible: Notifs.popups.length > 0
    anchors {
        top: true
        right: true
    }
    margins {
        top: Theme.padding
        right: Theme.padding
    }
    implicitWidth: toastWidth
    implicitHeight: Math.max(1, stack.implicitHeight)
    exclusiveZone: 0
    color: "transparent"

    WlrLayershell.layer: WlrLayer.Overlay
    WlrLayershell.namespace: "quickshell:toasts"
    WlrLayershell.keyboardFocus: WlrKeyboardFocus.None

    mask: Region { item: stack }

    ColumnLayout {
        id: stack
        width: parent.width
        spacing: Theme.spacing

        Repeater {
            model: Notifs.popups

            Rectangle {
                id: toast
                required property var modelData
                Layout.fillWidth: true
                implicitHeight: item.implicitHeight + 2
                radius: Theme.radius
                color: Theme.bg
                border.color: Theme.border
                border.width: 1

                NotificationItem {
                    id: item
                    anchors {
                        fill: parent
                        margins: 1
                    }
                    entry: toast.modelData
                    onDismissed: Notifs.dismiss(toast.modelData)
                }

                // Hover pauses nothing, but clicking the toast body with no
                // default action opens the panel.
                MouseArea {
                    anchors.fill: parent
                    z: -1
                    onClicked: { Notifs.hideToast(toast.modelData); ShellState.open(); }
                }

                // Slide in from the right.
                x: 0
                Component.onCompleted: { x = width; slideIn.start(); }
                NumberAnimation { id: slideIn; target: toast; property: "x"; to: 0; duration: Theme.animMs; easing.type: Easing.OutCubic }
            }
        }
    }
}
