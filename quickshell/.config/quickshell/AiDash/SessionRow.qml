import QtQuick
import QtQuick.Layouts
import qs
import qs.Widgets
import qs.Services

// One live agent session: project, tool/frontend badges, state, model,
// what it is doing, context fill and focus/kill actions.
Rectangle {
    id: root

    property var session: ({})

    readonly property var usage: ClaudeUsage.bySession[session.id] || null
    readonly property var shell: session.frontend === "emacs" ? EmacsAi.shellFor(session) : null
    readonly property string model: usage && usage.model ? usage.model : (session.model || (shell ? shell.model : "") || "")
    readonly property real ctxFill: usage && usage.ctx > 0 ? usage.ctx / ClaudeUsage.contextWindow(model) : 0

    readonly property color tint: session.state === "waiting" ? Theme.orange
                                : session.state === "working" ? Theme.green
                                : session.state === "idle"    ? Theme.gray
                                : Theme.blue

    // Re-evaluated every poll so the elapsed time ticks.
    readonly property string elapsed: Sessions.sessions ? Sessions.elapsed(session) : ""

    readonly property string activity: {
        if (session.state === "working" && session.last_tool)
            return session.last_tool + (session.last_tool_detail ? ": " + session.last_tool_detail : "");
        if (session.state === "waiting")
            return "needs permission" + (session.last_tool ? " for " + session.last_tool : "");
        if (shell && shell.title) return shell.title;
        return session.last_prompt ? "› " + session.last_prompt : "";
    }

    Layout.fillWidth: true
    implicitHeight: col.implicitHeight + Theme.padding * 1.5
    radius: Theme.radius
    color: session.state === "waiting" ? Qt.alpha(Theme.orange, 0.08) : Theme.bg2
    border.color: session.state === "waiting" ? Qt.alpha(Theme.orange, 0.5) : "transparent"
    border.width: 1

    ColumnLayout {
        id: col
        anchors {
            fill: parent
            margins: Theme.padding * 0.75
        }
        spacing: 4

        RowLayout {
            Layout.fillWidth: true
            spacing: Theme.spacing

            Rectangle {
                implicitWidth: 10
                implicitHeight: 10
                radius: 5
                color: root.tint
            }

            Label {
                text: Sessions.project(root.session)
                font.bold: true
                Layout.maximumWidth: 200
            }

            Label {
                dim: true
                font.pixelSize: Theme.fontSmall
                text: root.session.tool + " · " + (root.session.frontend || "terminal")
            }

            Item { Layout.fillWidth: true }

            StatePill {
                text: root.session.state
                tint: root.tint
                pulse: root.session.state === "waiting"
            }

            IconButton {
                size: 26
                icon: "󰈈"
                tooltip: "Focus window"
                onClicked: Launcher.focusSession(root.session)
            }
            IconButton {
                size: 26
                icon: "󰅙"
                iconColor: Theme.red
                tooltip: "Terminate"
                onClicked: Sessions.kill(root.session)
            }
        }

        RowLayout {
            Layout.fillWidth: true
            spacing: Theme.spacing

            Label {
                dim: true
                font.pixelSize: Theme.fontSmall
                text: [ClaudeUsage.shortModel(root.model) || "?", root.elapsed,
                       root.usage ? root.usage.turns + " turns · " + ClaudeUsage.fmt(root.usage.out) + " out" : ""]
                      .filter(s => s !== "").join(" · ")
            }

            Item { Layout.fillWidth: true }

            Label {
                visible: root.usage !== null
                dim: true
                font.pixelSize: Theme.fontSmall
                text: "ctx " + ClaudeUsage.fmt(root.usage ? root.usage.ctx : 0) + " (" + Math.round(root.ctxFill * 100) + "%)"
            }

            Rectangle {
                visible: root.usage !== null
                implicitWidth: 80
                implicitHeight: 4
                radius: 2
                color: Theme.bg3
                Rectangle {
                    width: parent.width * Math.min(1, root.ctxFill)
                    height: parent.height
                    radius: 2
                    color: root.ctxFill > 0.85 ? Theme.red : root.ctxFill > 0.6 ? Theme.yellow : Theme.aqua
                    Behavior on width { NumberAnimation { duration: 300 } }
                }
            }
        }

        Label {
            visible: root.activity !== ""
            Layout.fillWidth: true
            text: root.activity
            color: root.session.state === "waiting" ? Theme.orange : Theme.fg2
            font.pixelSize: Theme.fontSmall
        }
    }
}
