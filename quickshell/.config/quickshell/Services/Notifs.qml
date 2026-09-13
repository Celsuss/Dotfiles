pragma Singleton
import QtQuick
import Quickshell
import Quickshell.Io
import Quickshell.Services.Notifications
import qs

// Notification daemon (org.freedesktop.Notifications). Notifications stay
// open D-Bus-wise while they sit in the center so their actions keep
// working; a toast timing out only hides the toast. History is mirrored
// to a JSON file so the list survives restarts (restored entries have no
// live object, hence no actions).
Singleton {
    id: root

    // Newest first. Entries: { key, id, appName, appIcon, summary, body, image,
    //   urgency, time, transient, actions: [{id, text}], live: Notification|null }
    property var list: []
    property var popups: []          // subset of `list` currently shown as toasts
    readonly property int count: list.length

    readonly property int toastTimeout: 5000
    readonly property int criticalTimeout: 0   // 0 = stays until dismissed
    readonly property int historyLimit: 100

    property int _nextKey: 1

    NotificationServer {
        id: server
        keepOnReload: true
        actionsSupported: true
        bodySupported: true
        bodyMarkupSupported: true
        imageSupported: true
        persistenceSupported: true
        actionIconsSupported: false

        onNotification: n => {
            n.tracked = true;
            root.add(n);
        }
    }

    function add(n) {
        const entry = {
            key: _nextKey++,
            id: n.id,
            appName: n.appName || (n.desktopEntry || "Notification"),
            appIcon: n.appIcon,
            summary: n.summary,
            body: n.body,
            image: n.image,
            urgency: n.urgency,
            time: Date.now(),
            transient: n.transient,
            actions: n.actions.map(a => ({ id: a.identifier, text: a.text })),
            live: n
        };

        // Replacing an existing id (e.g. media players updating a track).
        const prev = list.findIndex(e => e.live && e.id === n.id && e.live !== n);
        let next = list.slice();
        if (prev >= 0) next.splice(prev, 1);
        next.unshift(entry);
        list = next.slice(0, historyLimit);

        n.closed.connect(reason => root.onClosed(entry, reason));

        if (!ShellState.dnd && !ShellState.panelOpen) showToast(entry);
        save();
    }

    function showToast(entry) {
        popups = [entry].concat(popups);
        const ms = entry.urgency === NotificationUrgency.Critical ? criticalTimeout : toastTimeout;
        if (ms > 0) toastTimer.createObject(root, { entry: entry, interval: ms });
    }

    function hideToast(entry) {
        popups = popups.filter(e => e.key !== entry.key);
        if (entry.transient) dismiss(entry);
    }

    function hideAllToasts() { popups = []; }

    // Called when the client closes it or after we dismissed/expired it.
    function onClosed(entry, reason) {
        popups = popups.filter(e => e.key !== entry.key);
        // Keep a dead copy in the center unless the app itself retracted it.
        if (reason === NotificationCloseReason.CloseRequested || entry.transient) {
            list = list.filter(e => e.key !== entry.key);
        } else {
            entry.live = null;
            list = list.slice();
        }
        save();
    }

    function dismiss(entry) {
        popups = popups.filter(e => e.key !== entry.key);
        list = list.filter(e => e.key !== entry.key);
        if (entry.live) {
            const n = entry.live;
            entry.live = null;
            n.dismiss();
        }
        save();
    }

    function dismissAll() {
        const all = list;
        list = [];
        popups = [];
        for (const e of all) if (e.live) e.live.dismiss();
        save();
    }

    function invoke(entry, actionId) {
        if (!entry.live) return;
        const a = entry.live.actions.find(a => a.identifier === actionId);
        if (a) a.invoke();
    }

    function hasDefaultAction(entry) { return !!entry.live && entry.actions.some(a => a.id === "default"); }

    component ToastTimer: Timer {
        property var entry
        running: true
        onTriggered: { root.hideToast(entry); destroy(); }
    }
    Component { id: toastTimer; ToastTimer {} }

    // ---- persistence -------------------------------------------------
    readonly property string historyPath: Quickshell.statePath("notifications.json")
    property bool _loaded: false

    function save() {
        if (!_loaded) return;
        const data = list.filter(e => !e.transient).map(e => ({
            appName: e.appName, appIcon: e.appIcon, summary: e.summary, body: e.body,
            image: e.image, urgency: e.urgency, time: e.time
        }));
        history.setText(JSON.stringify(data));
    }

    FileView {
        id: history
        path: root.historyPath
        printErrors: false
        onLoaded: {
            // Our own saves re-trigger loaded; only restore once.
            if (root._loaded) return;
            try {
                const data = JSON.parse(text());
                const restored = data.map(d => Object.assign({ key: root._nextKey++, id: -1, transient: false, actions: [], live: null }, d));
                root.list = root.list.concat(restored).slice(0, root.historyLimit);
            } catch (e) {}
            root._loaded = true;
        }
        onLoadFailed: root._loaded = true
    }

    // Suppress toasts while the panel is open; nothing to do on close.
    Connections {
        target: ShellState
        function onPanelOpenChanged() { if (ShellState.panelOpen) root.hideAllToasts(); }
        function onDndChanged() { if (ShellState.dnd) root.hideAllToasts(); }
    }
}
