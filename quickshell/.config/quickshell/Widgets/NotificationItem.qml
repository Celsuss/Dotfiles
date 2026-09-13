import QtQuick
import QtQuick.Layouts
import Quickshell
import Quickshell.Services.Notifications
import qs
import qs.Services

// One notification: icon/image, app + time, summary, body, action buttons.
Rectangle {
    id: root

    required property var entry
    property bool showApp: true

    signal dismissed()

    readonly property bool critical: entry.urgency === NotificationUrgency.Critical
    // Quickshell folds the app icon into `image` as an unchecked
    // image://icon/<name> url; re-resolve those so a missing icon falls back
    // to the glyph instead of Qt's checkerboard.
    readonly property string iconSource: {
        const src = entry.image || entry.appIcon || "";
        if (src === "") return "";
        if (src.indexOf("image://icon/") === 0) return Quickshell.iconPath(src.substring(13), true);
        if (src.indexOf("/") === 0 || src.indexOf("file://") === 0 || src.indexOf("image://") === 0) return src;
        return Quickshell.iconPath(src, true);
    }
    readonly property bool hasPicture: iconSource !== "" && iconSource.indexOf("image://icon/") !== 0

    Layout.fillWidth: true
    implicitHeight: row.implicitHeight + Theme.padding * 1.5
    radius: Theme.radius - 2
    color: hover.containsMouse ? Theme.bg3 : Theme.bg2
    border.color: critical ? Theme.red : "transparent"
    border.width: 1

    // Minute clock so relative times re-evaluate.
    SystemClock { id: clock; precision: SystemClock.Minutes }

    function relTime(t) {
        const s = Math.max(0, Math.floor((clock.date.getTime() - t) / 1000));
        if (s < 60) return "now";
        if (s < 3600) return Math.floor(s / 60) + "m";
        if (s < 86400) return Math.floor(s / 3600) + "h";
        return Math.floor(s / 86400) + "d";
    }

    MouseArea {
        id: hover
        anchors.fill: parent
        hoverEnabled: true
        acceptedButtons: Qt.LeftButton | Qt.MiddleButton
        cursorShape: Notifs.hasDefaultAction(root.entry) ? Qt.PointingHandCursor : Qt.ArrowCursor
        onClicked: mouse => {
            if (mouse.button === Qt.MiddleButton) root.dismissed();
            else if (Notifs.hasDefaultAction(root.entry)) { Notifs.invoke(root.entry, "default"); root.dismissed(); }
        }
    }

    RowLayout {
        id: row
        anchors {
            fill: parent
            margins: Theme.padding * 0.75
        }
        spacing: Theme.spacing

        Rectangle {
            Layout.alignment: Qt.AlignTop
            implicitWidth: 40
            implicitHeight: 40
            radius: Theme.radius - 4
            color: Theme.bg1
            clip: true

            Image {
                anchors.fill: parent
                anchors.margins: root.hasPicture ? 0 : 6
                source: root.iconSource
                fillMode: Image.PreserveAspectFit
                asynchronous: true
                visible: status === Image.Ready
            }
            Text {
                anchors.centerIn: parent
                text: root.critical ? "󰀦" : "󰂚"
                color: root.critical ? Theme.red : Theme.gray
                font.family: Theme.iconFont
                font.pixelSize: 20
                visible: root.iconSource === ""
            }
        }

        ColumnLayout {
            Layout.fillWidth: true
            spacing: 2

            RowLayout {
                Layout.fillWidth: true
                spacing: 6
                Label {
                    visible: root.showApp
                    text: root.entry.appName
                    color: Theme.blue
                    font.pixelSize: Theme.fontSmall
                    font.bold: true
                }
                Label {
                    text: root.relTime(root.entry.time)
                    dim: true
                    font.pixelSize: Theme.fontSmall
                    Layout.fillWidth: true
                }
                IconButton {
                    icon: "󰅖"
                    size: 20
                    iconColor: Theme.gray
                    onClicked: root.dismissed()
                }
            }

            Label {
                Layout.fillWidth: true
                text: root.entry.summary
                font.bold: true
                wrapMode: Text.WordWrap
                maximumLineCount: 2
                visible: text !== ""
            }

            Label {
                Layout.fillWidth: true
                text: root.entry.body
                dim: true
                font.pixelSize: Theme.fontSmall
                textFormat: Text.StyledText
                linkColor: Theme.aqua
                wrapMode: Text.WordWrap
                maximumLineCount: 6
                visible: text !== ""
                onLinkActivated: link => Quickshell.execDetached(["xdg-open", link])
            }

            Flow {
                Layout.fillWidth: true
                Layout.topMargin: 4
                spacing: 6
                visible: root.entry.live !== null && actionRepeater.count > 0

                Repeater {
                    id: actionRepeater
                    model: root.entry.actions.filter(a => a.id !== "default")
                    Button {
                        required property var modelData
                        text: modelData.text
                        implicitHeight: 26
                        onClicked: { Notifs.invoke(root.entry, modelData.id); root.dismissed(); }
                    }
                }
            }
        }
    }
}
