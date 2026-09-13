import QtQuick
import QtQuick.Layouts
import qs
import qs.Widgets
import qs.Services

// Title, at-a-glance counters and close.
RowLayout {
    Layout.fillWidth: true
    spacing: Theme.spacing

    Text {
        text: "󱚝"
        color: Theme.accent
        font.family: Theme.iconFont
        font.pixelSize: 26
    }

    ColumnLayout {
        spacing: 0
        Label {
            text: "AI Agents"
            font.pixelSize: Theme.fontLarge
            font.bold: true
        }
        Label {
            dim: true
            font.pixelSize: Theme.fontSmall
            text: {
                const n = Sessions.sessions.length;
                let parts = [n + (n === 1 ? " session" : " sessions")];
                if (Sessions.working > 0) parts.push(Sessions.working + " working");
                if (Ollama.loadedTotal > 0) parts.push(Ollama.loadedTotal + " model" + (Ollama.loadedTotal === 1 ? "" : "s") + " loaded");
                if (ClaudeUsage.today && ClaudeUsage.total(ClaudeUsage.today) > 0) parts.push("today " + ClaudeUsage.fmt(ClaudeUsage.total(ClaudeUsage.today)) + " tok");
                return parts.join(" · ");
            }
        }
    }

    Item { Layout.fillWidth: true }

    StatePill {
        visible: Sessions.waiting > 0
        text: Sessions.waiting + " waiting for you"
        tint: Theme.orange
        pulse: true
    }

    IconButton {
        icon: "󰑐"
        tooltip: "Refresh"
        onClicked: {
            Sessions.refresh();
            Ollama.refresh();
            ClaudeUsage.refreshDaily();
            EmacsAi.refresh();
        }
    }

    IconButton {
        icon: "󰅖"
        onClicked: ShellState.closeAiDash()
    }
}
