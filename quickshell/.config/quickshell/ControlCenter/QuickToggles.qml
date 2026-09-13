import QtQuick
import QtQuick.Layouts
import Quickshell.Bluetooth
import qs
import qs.Widgets
import qs.Services

// 2x2 grid of the most-used toggles.
GridLayout {
    Layout.fillWidth: true
    columns: 2
    rowSpacing: Theme.spacing
    columnSpacing: Theme.spacing

    readonly property var adapter: Bluetooth.defaultAdapter

    ToggleTile {
        icon: ShellState.dnd ? "󰂛" : "󰂚"
        label: "Do not disturb"
        subtitle: ShellState.dnd ? "On" : "Off"
        active: ShellState.dnd
        onClicked: ShellState.dnd = !ShellState.dnd
    }

    ToggleTile {
        icon: "󰦝"
        label: "NordVPN"
        subtitle: Nord.summary
        active: Nord.connected
        busy: Nord.busy || Nord.connecting
        enabled: Nord.available
        onClicked: Nord.toggle()
    }

    ToggleTile {
        icon: "󰖂"
        label: "Tailscale"
        subtitle: Tailscale.summary
        active: Tailscale.running
        busy: Tailscale.busy
        enabled: Tailscale.available
        onClicked: Tailscale.toggle()
    }

    ToggleTile {
        icon: adapter && adapter.enabled ? "󰂯" : "󰂲"
        label: "Bluetooth"
        subtitle: !adapter ? "No adapter"
                : !adapter.enabled ? "Off"
                : (adapter.devices.values.filter(d => d.connected).length + " connected")
        active: adapter ? adapter.enabled : false
        enabled: adapter !== null
        onClicked: if (adapter) adapter.enabled = !adapter.enabled
    }
}
