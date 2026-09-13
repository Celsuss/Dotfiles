import QtQuick
import QtQuick.Layouts
import Quickshell
import Quickshell.Wayland
import Quickshell.Hyprland
import qs
import qs.Widgets

// Right-edge slide-in panel. The window is transparent and stays mapped
// while the content slides out, then unmaps once the animation finishes.
PanelWindow {
    id: panel

    property bool shown: ShellState.panelOpen

    anchors {
        top: true
        right: true
        bottom: true
    }
    implicitWidth: Theme.panelWidth
    exclusiveZone: 0
    color: "transparent"
    visible: false

    WlrLayershell.layer: WlrLayer.Overlay
    WlrLayershell.namespace: "quickshell:controlcenter"
    WlrLayershell.keyboardFocus: shown ? WlrKeyboardFocus.OnDemand : WlrKeyboardFocus.None

    onShownChanged: {
        if (shown) {
            panel.visible = true;
            content.forceActiveFocus();
        }
    }

    // Click outside (any other surface) closes the panel.
    HyprlandFocusGrab {
        windows: [panel]
        active: panel.shown
        onCleared: ShellState.close()
    }

    Rectangle {
        id: content
        width: parent.width
        height: parent.height
        x: panel.shown ? 0 : width
        color: Theme.bg
        border.color: Theme.border
        border.width: 1
        focus: true

        Keys.onEscapePressed: ShellState.close()

        Behavior on x {
            NumberAnimation {
                id: slide
                duration: Theme.animMs
                easing.type: Easing.OutCubic
                onRunningChanged: if (!running && !panel.shown) panel.visible = false
            }
        }

        ColumnLayout {
            anchors {
                fill: parent
                margins: Theme.padding
            }
            spacing: Theme.spacing

            Header {}

            Rectangle {
                Layout.fillWidth: true
                implicitHeight: 1
                color: Theme.border
            }

            // Scrollable middle: cards get added here in later phases.
            Flickable {
                Layout.fillWidth: true
                Layout.fillHeight: true
                contentHeight: cards.implicitHeight
                clip: true
                boundsBehavior: Flickable.StopAtBounds

                ColumnLayout {
                    id: cards
                    width: parent.width
                    spacing: Theme.spacing

                    QuickToggles {}
                    NotificationList {}
                    VpnCard {}
                    AudioCard {}
                    BluetoothCard {}
                    MediaCard {}
                    StatsCard {}
                    HomelabCard {}
                }
            }

            Rectangle {
                Layout.fillWidth: true
                implicitHeight: 1
                color: Theme.border
            }

            PowerFooter {}
        }
    }
}
