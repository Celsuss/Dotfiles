import QtQuick
import QtQuick.Layouts
import qs
import qs.Widgets
import qs.Services

Card {
    title: "VPN"

    // Small status dot + provider name + summary, with an action button.
    component ProviderRow: RowLayout {
        property string name: ""
        property string summary: ""
        property bool on: false
        property bool busy: false
        property bool enabled: true
        property string buttonText: on ? "Disconnect" : "Connect"
        signal action()

        Layout.fillWidth: true
        spacing: Theme.spacing

        Rectangle {
            implicitWidth: 10
            implicitHeight: 10
            radius: 5
            color: busy ? Theme.yellow : (on ? Theme.green : Theme.gray)
        }
        ColumnLayout {
            Layout.fillWidth: true
            spacing: 0
            Label { text: name; font.bold: true }
            Label { text: summary; dim: true; font.pixelSize: Theme.fontSmall; Layout.fillWidth: true }
        }
        Button {
            text: busy ? "…" : buttonText
            accent: !on
            enabled: parent.enabled && !busy
            onClicked: parent.action()
        }
    }

    component ErrorLine: Label {
        Layout.fillWidth: true
        color: Theme.red
        font.pixelSize: Theme.fontSmall
        wrapMode: Text.WordWrap
        visible: text !== ""
    }

    component Detail: Label {
        Layout.fillWidth: true
        dim: true
        font.pixelSize: Theme.fontSmall
        visible: text !== ""
    }

    // ---- NordVPN ------------------------------------------------------
    ProviderRow {
        name: "NordVPN"
        summary: Nord.summary
        on: Nord.connected
        busy: Nord.busy || Nord.connecting
        enabled: Nord.available
        onAction: Nord.toggle()
    }

    Detail {
        text: Nord.connected
            ? [Nord.hostname, Nord.ip, Nord.technology, Nord.uptime].filter(s => s !== "").join("  ·  ")
            : ""
    }

    RowLayout {
        Layout.fillWidth: true
        spacing: Theme.spacing
        visible: Nord.available

        Dropdown {
            id: countryPick
            placeholder: "Connect to country…"
            items: Nord.countries.map(c => c.replace(/_/g, " "))
            current: Nord.connected ? Nord.country : ""
            onSelected: value => Nord.connect(value.replace(/ /g, "_"))
        }
    }

    SwitchRow {
        text: "Kill switch"
        checked: Nord.killSwitch
        enabled: Nord.available && !Nord.busy
        onToggled: on => Nord.setKillSwitch(on)
    }

    ErrorLine { text: Nord.error }

    Rectangle { Layout.fillWidth: true; implicitHeight: 1; color: Theme.border }

    // ---- Tailscale ----------------------------------------------------
    ProviderRow {
        name: "Tailscale"
        summary: Tailscale.summary
        on: Tailscale.running
        busy: Tailscale.busy
        enabled: Tailscale.available
        buttonText: Tailscale.running ? "Down" : "Up"
        onAction: Tailscale.toggle()
    }

    Detail {
        text: Tailscale.running
            ? [Tailscale.ip, Tailscale.dnsName, Tailscale.tailnet,
               Tailscale.peersOnline + "/" + Tailscale.peersTotal + " peers online"]
              .filter(s => s !== "").join("  ·  ")
            : ""
    }

    Dropdown {
        visible: Tailscale.running
        placeholder: "Exit node: none"
        items: ["None"].concat(Tailscale.exitNodes.map(n => n.name + (n.online ? "" : " (offline)")))
        current: Tailscale.exitNode !== "" ? "Exit node: " + Tailscale.exitNode : ""
        onSelected: value => Tailscale.setExitNode(value === "None" ? "" : value.replace(/ \(offline\)$/, ""))
    }

    ErrorLine { text: Tailscale.error }

    Detail {
        visible: Tailscale.accessDenied
        color: Theme.yellow
        wrapMode: Text.WordWrap
        text: "Run once to allow control without sudo:\n  sudo tailscale set --operator=$USER"
    }
}
