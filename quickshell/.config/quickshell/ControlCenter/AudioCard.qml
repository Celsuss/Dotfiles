import QtQuick
import QtQuick.Layouts
import qs
import qs.Widgets
import qs.Services

Card {
    title: "Audio"

    // Output
    VolumeSlider {
        audio: Audio.sink ? Audio.sink.audio : null
        icon: "󰕾"
        mutedIcon: "󰖁"
    }

    Dropdown {
        visible: Audio.sinks.length > 1
        placeholder: "Output device…"
        items: Audio.sinks.map(n => Audio.nodeName(n))
        current: Audio.nodeName(Audio.sink)
        onSelected: value => {
            const node = Audio.sinks.find(n => Audio.nodeName(n) === value);
            if (node) Audio.setDefaultSink(node);
        }
    }

    // Input (hidden when there is no capture device)
    VolumeSlider {
        visible: Audio.source !== null
        audio: Audio.source ? Audio.source.audio : null
        icon: "󰍬"
        mutedIcon: "󰍭"
    }

    Rectangle {
        visible: Audio.streams.length > 0
        Layout.fillWidth: true
        implicitHeight: 1
        color: Theme.border
    }

    // Per-application streams
    Repeater {
        model: Audio.streams

        ColumnLayout {
            required property var modelData
            Layout.fillWidth: true
            spacing: 2

            RowLayout {
                Layout.fillWidth: true
                Label {
                    text: Audio.nodeName(modelData)
                    font.pixelSize: Theme.fontSmall
                    font.bold: true
                }
                Label {
                    text: Audio.streamTitle(modelData)
                    dim: true
                    font.pixelSize: Theme.fontSmall
                    Layout.fillWidth: true
                }
            }

            VolumeSlider {
                audio: modelData.audio
                icon: "󰝚"
                mutedIcon: "󰝛"
            }
        }
    }
}
