import QtQuick
import QtQuick.Layouts
import Quickshell
import Quickshell.Wayland
import Quickshell.Hyprland
import qs
import qs.Widgets

// Centered AI agent dashboard overlay: sessions on the left, Ollama / usage /
// Emacs on the right, actions along the bottom. Same show/hide mechanics as
// the control center panel (transparent window stays mapped during the fade).
PanelWindow {
    id: win

    property bool shown: ShellState.aiDashOpen

    // No anchors: layer-shell centers the surface on both axes.
    implicitWidth: 1040
    implicitHeight: 680
    exclusiveZone: 0
    color: "transparent"
    visible: false

    WlrLayershell.layer: WlrLayer.Overlay
    WlrLayershell.namespace: "quickshell:aidash"
    WlrLayershell.keyboardFocus: shown ? WlrKeyboardFocus.OnDemand : WlrKeyboardFocus.None

    onShownChanged: {
        if (shown) {
            win.visible = true;
            content.forceActiveFocus();
        }
    }

    HyprlandFocusGrab {
        windows: [win]
        active: win.shown
        onCleared: ShellState.closeAiDash()
    }

    Rectangle {
        id: content
        anchors.fill: parent
        color: Theme.bg
        border.color: Theme.border
        border.width: 1
        radius: Theme.radius
        focus: true
        opacity: win.shown ? 1 : 0
        scale: win.shown ? 1 : 0.97

        Keys.onEscapePressed: ShellState.closeAiDash()

        Behavior on opacity {
            NumberAnimation {
                duration: Theme.animMs
                easing.type: Easing.OutCubic
                onRunningChanged: if (!running && !win.shown) win.visible = false
            }
        }
        Behavior on scale { NumberAnimation { duration: Theme.animMs; easing.type: Easing.OutCubic } }

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

            RowLayout {
                Layout.fillWidth: true
                Layout.fillHeight: true
                spacing: Theme.spacing

                SessionList {
                    Layout.fillHeight: true
                    Layout.fillWidth: false
                    Layout.preferredWidth: Math.round((content.width - Theme.padding * 2 - Theme.spacing) * 0.55)
                }

                Flickable {
                    Layout.fillWidth: true
                    Layout.fillHeight: true
                    Layout.preferredWidth: 100
                    contentHeight: rightCol.implicitHeight
                    clip: true
                    boundsBehavior: Flickable.StopAtBounds

                    ColumnLayout {
                        id: rightCol
                        width: parent.width
                        spacing: Theme.spacing

                        OllamaCard {}
                        UsageCard {}
                        EmacsCard {}
                    }
                }
            }

            Rectangle {
                Layout.fillWidth: true
                implicitHeight: 1
                color: Theme.border
            }

            ActionsFooter {}
        }
    }
}
