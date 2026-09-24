import QtQuick
import QtQuick.Layouts
import Quickshell
import qs
import qs.Widgets
import qs.Services

// Notifications grouped by app; groups ordered by their newest entry.
ColumnLayout {
    id: root
    Layout.fillWidth: true
    spacing: Theme.spacing

    readonly property int collapsedLimit: 3
    property var expandedApps: ({})

    function setExpanded(app, on) {
        const m = Object.assign({}, expandedApps);
        m[app] = on;
        expandedApps = m;
    }

    readonly property var groups: {
        const byApp = {};
        const order = [];
        for (const e of Notifs.list) {
            if (!(e.appName in byApp)) { byApp[e.appName] = []; order.push(e.appName); }
            byApp[e.appName].push(e);
        }
        return order.map(app => ({ app: app, items: byApp[app] }));
    }

    RowLayout {
        Layout.fillWidth: true
        Label {
            text: "Notifications" + (Notifs.count > 0 ? " (" + Notifs.count + ")" : "")
            font.bold: true
            Layout.fillWidth: true
        }
        Button {
            visible: Notifs.count > 0
            text: "Clear all"
            implicitHeight: 26
            onClicked: Notifs.dismissAll()
        }
    }

    Label {
        visible: Notifs.count === 0
        text: ShellState.dnd ? "Do not disturb is on" : "No notifications"
        dim: true
        font.pixelSize: Theme.fontSmall
        Layout.alignment: Qt.AlignHCenter
        Layout.topMargin: Theme.padding
    }

    Repeater {
        model: root.groups

        ColumnLayout {
            id: group
            required property var modelData
            readonly property bool expanded: root.expandedApps[modelData.app] === true
            readonly property var shown: expanded ? modelData.items : modelData.items.slice(0, root.collapsedLimit)
            Layout.fillWidth: true
            spacing: 4

            RowLayout {
                Layout.fillWidth: true
                Label {
                    text: group.modelData.app
                    color: Theme.blue
                    font.pixelSize: Theme.fontSmall
                    font.bold: true
                    Layout.fillWidth: true
                }
                Label {
                    visible: group.modelData.items.length > root.collapsedLimit
                    text: group.expanded ? "Show less" : "+" + (group.modelData.items.length - root.collapsedLimit) + " more"
                    color: Theme.gray
                    font.pixelSize: Theme.fontSmall

                    activeFocusOnTab: true

                    Keys.onReturnPressed: root.setExpanded(group.modelData.app, !group.expanded)
                    Keys.onEnterPressed:  root.setExpanded(group.modelData.app, !group.expanded)
                    Keys.onSpacePressed:  root.setExpanded(group.modelData.app, !group.expanded)
                    Keys.onPressed: event => {
                        if (event.key === Qt.Key_L || event.key === Qt.Key_Right) {
                            root.setExpanded(group.modelData.app, true);
                            event.accepted = true;
                        } else if (event.key === Qt.Key_H || event.key === Qt.Key_Left) {
                            root.setExpanded(group.modelData.app, false);
                            event.accepted = true;
                        }
                    }

                    FocusRing {}

                    MouseArea {
                        anchors.fill: parent
                        cursorShape: Qt.PointingHandCursor
                        onClicked: root.setExpanded(group.modelData.app, !group.expanded)
                    }
                }
                IconButton {
                    icon: "󰎟"
                    size: 20
                    iconColor: Theme.gray
                    // `d` on this header dismisses the whole group.
                    function navDismiss() { for (const e of group.modelData.items) Notifs.dismiss(e); }
                    readonly property int navNextKey: -1
                    onClicked: navDismiss()
                }
            }

            Repeater {
                model: group.shown
                NotificationItem {
                    required property var modelData
                    entry: modelData
                    showApp: false
                    onDismissed: Notifs.dismiss(modelData)
                }
            }
        }
    }
}
