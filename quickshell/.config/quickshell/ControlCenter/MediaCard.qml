import QtQuick
import QtQuick.Layouts
import Quickshell.Services.Mpris
import qs
import qs.Widgets
import qs.Services

Card {
    id: card
    title: "Media"

    // MPRIS players that currently have a track loaded, playing ones first.
    readonly property var players: Mpris.players.values
        .filter(p => p.trackTitle !== "" || p.playbackState === MprisPlaybackState.Playing)
        .sort((a, b) => (b.playbackState === MprisPlaybackState.Playing) - (a.playbackState === MprisPlaybackState.Playing))

    component TransportButton: IconButton {
        size: 32
        iconColor: enabled ? Theme.fg : Theme.gray
        property bool enabled: true
        opacity: enabled ? 1 : 0.4
    }

    Label {
        visible: card.players.length === 0 && !Mpd.available
        text: "Nothing playing"
        dim: true
        font.pixelSize: Theme.fontSmall
    }

    // ---- MPRIS players -------------------------------------------------
    Repeater {
        model: card.players

        RowLayout {
            id: row
            required property var modelData
            readonly property bool playing: modelData.playbackState === MprisPlaybackState.Playing
            Layout.fillWidth: true
            spacing: Theme.spacing

            Rectangle {
                implicitWidth: 56
                implicitHeight: 56
                radius: Theme.radius - 4
                color: Theme.bg2
                clip: true

                Image {
                    anchors.fill: parent
                    source: row.modelData.trackArtUrl
                    fillMode: Image.PreserveAspectCrop
                    asynchronous: true
                    visible: status === Image.Ready
                }
                Text {
                    anchors.centerIn: parent
                    text: "󰝚"
                    color: Theme.gray
                    font.family: Theme.iconFont
                    font.pixelSize: 24
                    visible: row.modelData.trackArtUrl === ""
                }
            }

            ColumnLayout {
                Layout.fillWidth: true
                spacing: 0
                Label { text: row.modelData.trackTitle || "Unknown"; font.bold: true; Layout.fillWidth: true }
                Label { text: row.modelData.trackArtist; dim: true; font.pixelSize: Theme.fontSmall; Layout.fillWidth: true; visible: text !== "" }
                Label { text: row.modelData.identity; color: Theme.blue; font.pixelSize: Theme.fontSmall; Layout.fillWidth: true }
            }

            TransportButton { icon: "󰒮"; enabled: row.modelData.canGoPrevious; onClicked: row.modelData.previous() }
            TransportButton {
                icon: row.playing ? "󰏤" : "󰐊"
                enabled: row.modelData.canTogglePlaying
                onClicked: row.modelData.togglePlaying()
            }
            TransportButton { icon: "󰒭"; enabled: row.modelData.canGoNext; onClicked: row.modelData.next() }
        }
    }

    Rectangle {
        visible: card.players.length > 0 && Mpd.available
        Layout.fillWidth: true
        implicitHeight: 1
        color: Theme.border
    }

    // ---- MPD / radio ---------------------------------------------------
    RowLayout {
        visible: Mpd.available
        Layout.fillWidth: true
        spacing: Theme.spacing

        Rectangle {
            implicitWidth: 56
            implicitHeight: 56
            radius: Theme.radius - 4
            color: Mpd.playing ? Qt.alpha(Theme.accent, 0.2) : Theme.bg2
            Text {
                anchors.centerIn: parent
                text: "󰐹"
                color: Mpd.playing ? Theme.accent : Theme.gray
                font.family: Theme.iconFont
                font.pixelSize: 26
            }
        }

        ColumnLayout {
            Layout.fillWidth: true
            spacing: 0
            Label {
                text: Mpd.state === "stop" ? "Stopped" : Mpd.headline
                font.bold: true
                Layout.fillWidth: true
            }
            Label {
                text: Mpd.state === "stop" ? "" : Mpd.subline
                dim: true
                font.pixelSize: Theme.fontSmall
                Layout.fillWidth: true
                visible: text !== ""
            }
            Label { text: "MPD"; color: Theme.blue; font.pixelSize: Theme.fontSmall }
        }

        TransportButton { icon: "󰒮"; enabled: Mpd.queueLength > 1 && !Mpd.busy; onClicked: Mpd.previous() }
        TransportButton { icon: Mpd.playing ? "󰏤" : "󰐊"; enabled: Mpd.queueLength > 0 && !Mpd.busy; onClicked: Mpd.toggle() }
        TransportButton { icon: "󰓛"; enabled: Mpd.state !== "stop" && !Mpd.busy; onClicked: Mpd.stop() }
        TransportButton { icon: "󰒭"; enabled: Mpd.queueLength > 1 && !Mpd.busy; onClicked: Mpd.next() }
    }

    Dropdown {
        visible: Mpd.available && Mpd.stations.length > 0
        placeholder: "Radio station…"
        items: Mpd.stations.map(s => s.name)
        current: Mpd.currentStation >= 0 ? Mpd.stations[Mpd.currentStation].name : ""
        onSelected: value => {
            const i = Mpd.stations.findIndex(s => s.name === value);
            if (i >= 0) Mpd.playStation(i);
        }
    }
}
