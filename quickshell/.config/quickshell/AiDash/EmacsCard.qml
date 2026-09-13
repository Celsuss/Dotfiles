import QtQuick
import QtQuick.Layouts
import qs
import qs.Widgets
import qs.Services

// gptel / ellama / agent-shell buffers in the running Emacs.
Card {
    id: card
    title: "Emacs"

    readonly property var rows: {
        const out = [];
        for (const g of EmacsAi.gptel)
            out.push({ kind: "gptel", name: g.buffer, detail: [g.backend, g.model].filter(s => s && s !== "nil").join(" / "), busy: !!g.busy });
        for (const e of EmacsAi.ellama)
            out.push({ kind: "ellama", name: e.buffer, detail: e.model || "", busy: !!e.busy });
        for (const a of EmacsAi.agentShell)
            out.push({ kind: "agent-shell", name: a.buffer, detail: [a.agent, ClaudeUsage.shortModel(a.model)].filter(s => s).join(" · "), busy: !!a.busy });
        return out;
    }

    Label {
        visible: !EmacsAi.available
        dim: true
        text: "Emacs daemon not reachable"
    }

    Label {
        visible: EmacsAi.available && card.rows.length === 0
        dim: true
        wrapMode: Text.WordWrap
        Layout.fillWidth: true
        text: {
            const d = [];
            if (EmacsAi.gptelDefault && EmacsAi.gptelDefault.backend)
                d.push("gptel: " + EmacsAi.gptelDefault.backend + " / " + EmacsAi.gptelDefault.model);
            if (EmacsAi.ellamaDefault) d.push("ellama: " + EmacsAi.ellamaDefault);
            return "No active AI buffers" + (d.length ? "\n" + d.join("\n") : "");
        }
    }

    Repeater {
        model: card.rows
        delegate: RowLayout {
            required property var modelData
            Layout.fillWidth: true
            spacing: Theme.spacing

            Rectangle {
                implicitWidth: 8
                implicitHeight: 8
                radius: 4
                color: modelData.busy ? Theme.green : Theme.gray
            }
            Label {
                text: modelData.kind
                dim: true
                font.pixelSize: Theme.fontSmall
                Layout.preferredWidth: 80
            }
            Label {
                text: modelData.name
                Layout.fillWidth: true
            }
            Label {
                text: modelData.detail
                dim: true
                font.pixelSize: Theme.fontSmall
                Layout.maximumWidth: 200
            }
        }
    }
}
