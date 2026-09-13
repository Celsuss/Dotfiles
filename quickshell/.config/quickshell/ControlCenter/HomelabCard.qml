import QtQuick
import QtQuick.Layouts
import qs
import qs.Widgets
import qs.Services

Card {
    id: card
    title: "Homelab"

    RowLayout {
        Layout.fillWidth: true
        spacing: Theme.spacing

        Rectangle {
            implicitWidth: 10
            implicitHeight: 10
            radius: 5
            color: !Homelab.available ? Theme.gray : (Homelab.allReady ? Theme.green : Theme.yellow)
        }

        Label {
            Layout.fillWidth: true
            text: {
                if (Homelab.error !== "") return "kubectl: " + Homelab.error;
                if (!Homelab.available) return Homelab.loading ? "Loading…" : "Cluster unavailable";
                const nodes = Homelab.nodes.map(n => n.name + (n.ready ? "" : " (NotReady)")).join(", ");
                return nodes + "  ·  " + Homelab.podsHealthy + "/" + Homelab.podsTotal + " pods";
            }
            font.pixelSize: Theme.fontSmall
            color: Homelab.error !== "" ? Theme.red : Theme.fg
        }

        IconButton {
            icon: "󰑐"
            size: 26
            onClicked: Homelab.refresh()
        }
    }

    Repeater {
        model: Homelab.unhealthyPods
        Label {
            required property var modelData
            Layout.fillWidth: true
            text: "󰀦  " + modelData.ns + "/" + modelData.name + "  —  " + modelData.status
            color: Theme.yellow
            font.pixelSize: Theme.fontSmall
        }
    }

    GridLayout {
        visible: Homelab.services.length > 0
        Layout.fillWidth: true
        columns: 3
        rowSpacing: 6
        columnSpacing: 6

        Repeater {
            model: Homelab.services

            Rectangle {
                required property var modelData
                Layout.fillWidth: true
                implicitHeight: 30
                radius: Theme.radius - 4
                color: svcMouse.containsMouse ? Theme.bg3 : Theme.bg2

                Label {
                    anchors {
                        fill: parent
                        leftMargin: 8
                        rightMargin: 8
                    }
                    verticalAlignment: Text.AlignVCenter
                    text: modelData.name
                    font.pixelSize: Theme.fontSmall
                }

                MouseArea {
                    id: svcMouse
                    anchors.fill: parent
                    hoverEnabled: true
                    cursorShape: Qt.PointingHandCursor
                    onClicked: { Homelab.open(modelData); ShellState.close(); }
                }
            }
        }
    }
}
