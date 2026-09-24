import QtQuick
import QtQuick.Layouts
import Quickshell
import Quickshell.Wayland
import Quickshell.Hyprland
import qs
import qs.Widgets
import qs.Services

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
            // Focus the panel itself, not a control: no ring until the first
            // j/k, and Escape keeps working the moment the panel opens.
            content.forceActiveFocus();
        } else {
            content.hintsShown = false;
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

        property bool hintsShown: false
        property bool awaitingG: false

        KeyNav {
            id: nav
            scope: content
            scroller: flick
        }

        // `gg`: a lone `g` waits briefly for its partner.
        Timer {
            id: pendingG
            interval: 600
            onTriggered: content.awaitingG = false
        }

        // `d` on whatever holds focus, then put the ring back on the
        // notification that took its place (the delegates are rebuilt).
        function dismissFocused() {
            const cur = nav.current;
            if (!cur || !cur.navDismiss) return;
            const nextKey = cur.navNextKey === undefined ? -1 : cur.navNextKey;
            cur.navDismiss();
            Qt.callLater(() => content.refocusNotification(nextKey));
        }

        function refocusNotification(key) {
            let item = key >= 0 ? nav.find(i => i.entry && i.entry.key === key) : null;
            if (!item) item = nav.find(i => !!i.entry);
            if (item) nav.focusItem(item);
            else nav.clear();
        }

        Keys.onPressed: event => {
            const ctrl = (event.modifiers & Qt.ControlModifier) !== 0;
            const shift = (event.modifiers & Qt.ShiftModifier) !== 0;
            event.accepted = true;

            if (ctrl) {
                if (event.key === Qt.Key_D) nav.jump(true);
                else if (event.key === Qt.Key_U) nav.jump(false);
                else event.accepted = false;
                return;
            }

            if (event.key === Qt.Key_G && !shift) {
                if (content.awaitingG) {
                    content.awaitingG = false;
                    pendingG.stop();
                    nav.first();
                } else {
                    content.awaitingG = true;
                    pendingG.restart();
                }
                return;
            }

            // Any other key cancels a pending `g`.
            content.awaitingG = false;
            pendingG.stop();

            switch (event.key) {
            case Qt.Key_J:
            case Qt.Key_Down:     nav.move(true); break;
            case Qt.Key_K:
            case Qt.Key_Up:       nav.move(false); break;
            case Qt.Key_G:        nav.last(); break;
            case Qt.Key_D:        shift ? Notifs.dismissAll() : content.dismissFocused(); break;
            case Qt.Key_Question: content.hintsShown = !content.hintsShown; break;
            case Qt.Key_Q:
            case Qt.Key_Escape:   ShellState.close(); break;
            default:              event.accepted = false;
            }
        }

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
                id: flick
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

        KeyHints {
            visible: content.hintsShown
            z: 1
        }
    }
}
