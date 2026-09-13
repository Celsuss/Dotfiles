pragma Singleton
import QtQuick
import Quickshell

// Gruvbox dark (hard) palette, matching rofi/waybar/rmpc themes.
Singleton {
    id: root

    readonly property color bg:       "#1d2021"
    readonly property color bg1:      "#282828"
    readonly property color bg2:      "#3c3836"
    readonly property color bg3:      "#504945"
    readonly property color fg:       "#ebdbb2"
    readonly property color fg2:      "#d5c4a1"
    readonly property color gray:     "#928374"

    readonly property color red:      "#fb4934"
    readonly property color green:    "#b8bb26"
    readonly property color yellow:   "#fabd2f"
    readonly property color blue:     "#83a598"
    readonly property color purple:   "#d3869b"
    readonly property color aqua:     "#8ec07c"
    readonly property color orange:   "#fe8019"

    readonly property color accent:   orange
    readonly property color border:   bg3

    readonly property string font:     "JetBrainsMono Nerd Font"
    readonly property string iconFont: "Symbols Nerd Font"
    readonly property int fontSize:    13
    readonly property int fontSmall:   11
    readonly property int fontLarge:   16

    readonly property int radius:      10
    readonly property int padding:     12
    readonly property int spacing:     8
    readonly property int panelWidth:  450
    readonly property int animMs:      180
}
