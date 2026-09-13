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
                    MouseArea {
                        anchors.fill: parent
                        cursorShape: Qt.PointingHandCursor
                        onClicked: {
                            const m = Object.assign({}, root.expandedApps);
                            m[group.modelData.app] = !group.expanded;
                            root.expandedApps = m;
                        }
                    }
                }
                IconButton {
                    icon: "󰎟"
                    size: 20
                    iconColor: Theme.gray
                    onClicked: { for (const e of group.modelData.items) Notifs.dismiss(e); }
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
