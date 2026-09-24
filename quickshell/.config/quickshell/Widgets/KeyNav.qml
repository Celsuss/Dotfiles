import QtQuick
import QtQuick.Window
import qs

// Keyboard navigation over Qt's focus chain.
//
// Widgets opt in with `activeFocusOnTab: true`; the chain then walks them in
// declaration order and skips hidden, disabled and non-opted-in items for us.
// `scope` is the focus root (nothing focused = the chain starts at its edges,
// so the first "next" lands on the first control and the first "previous" on
// the last), `scroller` the Flickable the controls live in.
QtObject {
    id: root

    property Item scope: null
    property Flickable scroller: null
    readonly property int jumpSize: 5

    readonly property Item current: scope ? scope.Window.activeFocusItem : null

    function contains(item, ancestor) {
        for (let p = item; p; p = p.parent)
            if (p === ancestor) return true;
        return false;
    }

    // Scroll `item` into the viewport. No-op for controls outside the
    // Flickable (the header and the power footer).
    function ensureVisible(item) {
        if (!scroller || !item || !contains(item, scroller.contentItem)) return;

        const pos = item.mapToItem(scroller.contentItem, 0, 0);
        const top = pos.y - Theme.spacing;
        const bottom = pos.y + item.height + Theme.spacing;
        const maxY = Math.max(0, scroller.contentHeight - scroller.height);

        if (top < scroller.contentY) scroller.contentY = Math.max(0, top);
        else if (bottom > scroller.contentY + scroller.height)
            scroller.contentY = Math.min(maxY, bottom - scroller.height);
    }

    function focusItem(item) {
        if (!item) return null;
        item.forceActiveFocus(Qt.TabFocusReason);
        ensureVisible(item);
        return item;
    }

    function stepFrom(item, forward) {
        return item ? focusItem(item.nextItemInFocusChain(forward)) : null;
    }

    // One step from wherever focus currently is.
    function move(forward) {
        const from = (current && contains(current, scope)) ? current : scope;
        return stepFrom(from, forward);
    }

    function jump(forward) {
        let last = null;
        for (let i = 0; i < jumpSize; i++) {
            const next = move(forward);
            if (!next || next === last) break;
            last = next;
        }
        return last;
    }

    function first() { return stepFrom(scope, true) }
    function last()  { return stepFrom(scope, false) }

    // Walk the whole chain once, returning the first item matching `pred`.
    function find(pred) {
        if (!scope) return null;
        const start = scope.nextItemInFocusChain(true);
        for (let it = start; it; it = it.nextItemInFocusChain(true)) {
            if (pred(it)) return it;
            if (it.nextItemInFocusChain(true) === start) break;
        }
        return null;
    }

    // Drop the focus ring without leaving the panel unfocused.
    function clear() { if (scope) scope.forceActiveFocus() }
}
