pragma Singleton
import QtQuick
import Quickshell
import Quickshell.Services.Pipewire

// PipeWire audio: default sink/source, selectable sinks and app streams.
// Nodes only expose volume/mute while bound, so every node we show is
// tracked here.
Singleton {
    id: root

    readonly property PwNode sink: Pipewire.defaultAudioSink
    readonly property PwNode source: Pipewire.defaultAudioSource

    readonly property var sinks: Pipewire.nodes.values.filter(n =>
        n.type === PwNodeType.AudioSink)

    // Application playback streams (type Audio|Stream|Sink).
    readonly property var streams: Pipewire.nodes.values.filter(n =>
        n.type === PwNodeType.AudioOutStream)

    PwObjectTracker {
        objects: [root.sink, root.source].concat(root.sinks, root.streams).filter(n => n !== null)
    }

    function setDefaultSink(node) { Pipewire.preferredDefaultAudioSink = node; }

    // Best human-readable name for a node.
    function nodeName(node) {
        if (!node) return "";
        if (node.isStream) {
            const p = node.properties;
            return p["application.name"] || p["node.name"] || node.name;
        }
        return node.nickname || node.description || node.name;
    }

    function streamTitle(node) {
        const p = node.properties;
        const media = p["media.name"] || "";
        const clean = media.replace(/^\s*-\s*/, "");
        return clean !== "" && clean !== nodeName(node) ? clean : "";
    }
}
