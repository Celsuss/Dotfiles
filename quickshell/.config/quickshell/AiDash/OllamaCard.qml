import QtQuick
import QtQuick.Layouts
import qs
import qs.Widgets
import qs.Services

// Ollama: host switcher, GPU meters, loaded models with keep-alive actions,
// and an installed-model picker to load one.
Card {
    id: card
    title: "Ollama"

    readonly property var st: Ollama.currentStatus
    property string pick: ""

    RowLayout {
        Layout.fillWidth: true
        spacing: 4

        Repeater {
            model: Ollama.hosts
            delegate: Button {
                required property var modelData
                readonly property var hs: Ollama.status[modelData.name]
                text: modelData.name + (hs && hs.online ? " · " + hs.version : " · offline")
                accent: Ollama.current === modelData.name
                onClicked: Ollama.current = modelData.name
            }
        }

        Item { Layout.fillWidth: true }

        Label {
            visible: Ollama.lastError !== ""
            text: Ollama.lastError
            color: Theme.red
            font.pixelSize: Theme.fontSmall
            Layout.maximumWidth: 180
        }
    }

    GridLayout {
        visible: Stats.hasGpu && Ollama.current === "local"
        Layout.fillWidth: true
        columns: 2
        rowSpacing: Theme.spacing
        columnSpacing: Theme.spacing

        Meter {
            label: "GPU"
            value: Stats.gpuUsage + "%"
            detail: Stats.gpuTemp + "°C"
            fill: Stats.gpuUsage / 100
        }
        Meter {
            label: "VRAM"
            value: Stats.vramUsed.toFixed(1) + " GiB"
            detail: "of " + Stats.vramTotal.toFixed(0) + " GiB"
            fill: Stats.vramTotal > 0 ? Stats.vramUsed / Stats.vramTotal : 0
        }
    }

    Label {
        visible: card.st.online && card.st.loaded.length === 0
        dim: true
        text: "No model loaded"
    }

    Repeater {
        model: card.st.loaded
        delegate: Rectangle {
            required property var modelData
            Layout.fillWidth: true
            implicitHeight: lrow.implicitHeight + Theme.padding
            radius: Theme.radius
            color: Theme.bg2

            RowLayout {
                id: lrow
                anchors {
                    fill: parent
                    margins: Theme.padding * 0.5
                    leftMargin: Theme.padding * 0.75
                }
                spacing: Theme.spacing

                ColumnLayout {
                    spacing: 0
                    Layout.fillWidth: true
                    Label { text: modelData.name; font.bold: true; Layout.fillWidth: true }
                    Label {
                        dim: true
                        font.pixelSize: Theme.fontSmall
                        text: {
                            const vramShare = modelData.size > 0 ? Math.round(modelData.vram / modelData.size * 100) : 0;
                            const exp = Ollama.expiresIn(modelData);
                            return Ollama.gib(modelData.vram) + " VRAM (" + vramShare + "%)"
                                 + (modelData.ctx ? " · ctx " + ClaudeUsage.fmt(modelData.ctx) : "")
                                 + (exp ? " · " + (exp === "∞" ? "kept warm" : "unloads in " + exp) : "");
                        }
                    }
                }

                Button {
                    text: "keep"
                    enabled: Ollama.expiresIn(modelData) !== "∞"
                    onClicked: Ollama.keepWarm(modelData.name)
                }
                Button {
                    text: "unload"
                    onClicked: Ollama.unload(modelData.name)
                }
            }
        }
    }

    RowLayout {
        visible: card.st.online
        Layout.fillWidth: true
        spacing: Theme.spacing

        Dropdown {
            Layout.fillWidth: true
            placeholder: "Load a model…"
            current: card.pick
            items: card.st.installed.map(m => m.name)
            onSelected: value => card.pick = value
        }
        Button {
            text: "load"
            accent: true
            enabled: card.pick !== ""
            Layout.alignment: Qt.AlignTop
            onClicked: { Ollama.load(card.pick); card.pick = ""; }
        }
    }

    Label {
        visible: card.st.online && card.pick !== ""
        dim: true
        font.pixelSize: Theme.fontSmall
        text: {
            const m = card.st.installed.find(x => x.name === card.pick);
            return m ? [m.params, m.quant, Ollama.gib(m.size), m.family].filter(s => s).join(" · ") : "";
        }
    }
}
