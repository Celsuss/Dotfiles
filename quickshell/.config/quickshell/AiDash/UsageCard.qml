import QtQuick
import QtQuick.Layouts
import qs
import qs.Widgets
import qs.Services

// Claude Code token usage: today's breakdown and a 7-day bar strip. Clicking
// a bar shows that day instead of today.
Card {
    id: card
    title: "Claude Code usage" + (ClaudeUsage.loading ? " …" : "")

    property string pickedDate: ""
    readonly property var shown: ClaudeUsage.daily.find(e => e.date === card.pickedDate) || ClaudeUsage.today
    readonly property real maxTotal: Math.max(1, ...ClaudeUsage.daily.map(e => ClaudeUsage.total(e)))

    Label {
        visible: ClaudeUsage.daily.length === 0
        dim: true
        text: ClaudeUsage.loading ? "Aggregating transcripts…" : "No usage in the last 7 days"
    }

    RowLayout {
        visible: card.shown !== null
        Layout.fillWidth: true
        spacing: Theme.spacing

        Label {
            text: card.shown ? (card.shown.date === Qt.formatDate(new Date(), "yyyy-MM-dd") ? "Today" : card.shown.date) : ""
            font.bold: true
        }
        Item { Layout.fillWidth: true }
        Label {
            dim: true
            font.pixelSize: Theme.fontSmall
            text: card.shown ? card.shown.turns + " turns · " + ClaudeUsage.fmt(ClaudeUsage.total(card.shown)) + " total" : ""
        }
    }

    GridLayout {
        visible: card.shown !== null
        Layout.fillWidth: true
        columns: 4
        rowSpacing: 0
        columnSpacing: Theme.spacing

        Repeater {
            model: card.shown ? [
                { k: "output",      v: card.shown.out, c: Theme.accent },
                { k: "input",       v: card.shown.in,  c: Theme.fg },
                { k: "cache read",  v: card.shown.cr,  c: Theme.aqua },
                { k: "cache write", v: card.shown.cc,  c: Theme.purple }
            ] : []
            delegate: ColumnLayout {
                required property var modelData
                spacing: 0
                Label { text: ClaudeUsage.fmt(modelData.v); font.bold: true; font.pixelSize: Theme.fontLarge; color: modelData.c }
                Label { text: modelData.k; dim: true; font.pixelSize: Theme.fontSmall }
            }
        }
    }

    Label {
        visible: card.shown !== null && card.shown.models
        Layout.fillWidth: true
        dim: true
        font.pixelSize: Theme.fontSmall
        text: {
            if (!card.shown || !card.shown.models) return "";
            return Object.keys(card.shown.models)
                .sort((a, b) => card.shown.models[b] - card.shown.models[a])
                .map(m => ClaudeUsage.shortModel(m) + " " + ClaudeUsage.fmt(card.shown.models[m]))
                .join(" · ") + " out";
        }
    }

    // 7-day strip: one bar per day (total tokens), newest on the right.
    RowLayout {
        visible: ClaudeUsage.daily.length > 0
        Layout.fillWidth: true
        Layout.topMargin: 4
        spacing: 4

        Repeater {
            model: ClaudeUsage.daily
            delegate: ColumnLayout {
                id: day
                required property var modelData
                readonly property bool active: card.shown && card.shown.date === modelData.date
                Layout.fillWidth: true
                spacing: 2

                Rectangle {
                    Layout.fillWidth: true
                    implicitHeight: 44
                    radius: 4
                    color: Theme.bg2
                    Rectangle {
                        anchors.bottom: parent.bottom
                        width: parent.width
                        height: Math.max(3, parent.height * ClaudeUsage.total(day.modelData) / card.maxTotal)
                        radius: 4
                        color: day.active ? Theme.accent : Theme.bg3
                    }
                    MouseArea {
                        anchors.fill: parent
                        cursorShape: Qt.PointingHandCursor
                        onClicked: card.pickedDate = day.active ? "" : day.modelData.date
                    }
                }
                Label {
                    Layout.alignment: Qt.AlignHCenter
                    dim: !day.active
                    font.pixelSize: Theme.fontSmall
                    text: Qt.formatDate(new Date(day.modelData.date + "T12:00:00"), "ddd")
                }
            }
        }
    }
}
