import QtQuick
import QtQuick.Layouts
import qs
import qs.Widgets
import qs.Services

Card {
    title: "System"

    GridLayout {
        Layout.fillWidth: true
        columns: 2
        rowSpacing: Theme.spacing
        columnSpacing: Theme.spacing

        Meter {
            label: "CPU"
            value: Math.round(Stats.cpuUsage * 100) + "%"
            detail: Stats.cpuTemp > 0 ? Stats.cpuTemp + "°C" : ""
            fill: Stats.cpuUsage
        }
        Meter {
            label: "Memory"
            value: Stats.memUsed.toFixed(1) + " GiB"
            detail: "of " + Stats.memTotal.toFixed(0) + " GiB"
            fill: Stats.memTotal > 0 ? Stats.memUsed / Stats.memTotal : 0
        }
        Meter {
            visible: Stats.hasGpu
            label: "GPU"
            value: Stats.gpuUsage + "%"
            detail: Stats.gpuTemp + "°C"
            fill: Stats.gpuUsage / 100
        }
        Meter {
            visible: Stats.hasGpu
            label: "VRAM"
            value: Stats.vramUsed.toFixed(1) + " GiB"
            detail: "of " + Stats.vramTotal.toFixed(0) + " GiB"
            fill: Stats.vramTotal > 0 ? Stats.vramUsed / Stats.vramTotal : 0
        }
    }
}
