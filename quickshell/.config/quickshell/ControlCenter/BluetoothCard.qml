import QtQuick
import QtQuick.Layouts
import Quickshell.Bluetooth
import qs
import qs.Widgets

Card {
    id: card
    title: "Bluetooth"

    readonly property var adapter: Bluetooth.defaultAdapter
    readonly property bool on: adapter ? adapter.enabled : false

    // Paired devices, connected ones first.
    readonly property var devices: !adapter ? [] : adapter.devices.values
        .filter(d => d.paired || d.bonded)
        .sort((a, b) => (b.connected - a.connected) || a.name.localeCompare(b.name))

    function deviceIcon(d) {
        const i = d.icon || "";
        if (i.indexOf("headset") !== -1 || i.indexOf("headphone") !== -1) return "󰋋";
        if (i.indexOf("audio") !== -1) return "󰓃";
        if (i.indexOf("input-gaming") !== -1) return "󰊴";
        if (i.indexOf("input-keyboard") !== -1) return "󰌌";
        if (i.indexOf("input-mouse") !== -1) return "󰍽";
        if (i.indexOf("phone") !== -1) return "󰏲";
        return "󰂯";
    }

    SwitchRow {
        text: !adapter ? "No adapter" : (on ? "Enabled" : "Disabled")
        checked: card.on
        enabled: adapter !== null
        onToggled: v => { if (adapter) adapter.enabled = v; }
    }

    Label {
        visible: card.on && card.devices.length === 0
        text: "No paired devices"
        dim: true
        font.pixelSize: Theme.fontSmall
    }

    Repeater {
        model: card.on ? card.devices : []

        RowLayout {
            required property var modelData
            readonly property bool busy: modelData.state === BluetoothDeviceState.Connecting
                                      || modelData.state === BluetoothDeviceState.Disconnecting
            Layout.fillWidth: true
            spacing: Theme.spacing

            Text {
                text: card.deviceIcon(modelData)
                color: modelData.connected ? Theme.blue : Theme.gray
                font.family: Theme.iconFont
                font.pixelSize: 18
            }

            ColumnLayout {
                Layout.fillWidth: true
                spacing: 0
                Label { text: modelData.name; Layout.fillWidth: true }
                Label {
                    text: busy ? "Working…"
                        : modelData.connected
                            ? "Connected" + (modelData.batteryAvailable ? " · " + Math.round(modelData.battery * 100) + "%" : "")
                            : "Not connected"
                    dim: true
                    font.pixelSize: Theme.fontSmall
                    Layout.fillWidth: true
                }
            }

            Button {
                text: busy ? "…" : (modelData.connected ? "Disconnect" : "Connect")
                accent: !modelData.connected
                enabled: !busy
                onClicked: modelData.connected ? modelData.disconnect() : modelData.connect()
            }
        }
    }
}
