import QtQuick
import qs

// Keyboard focus indicator. Drop it inside any focusable widget; it outlines
// the parent whenever that parent holds active focus.
//
// It must never be a *direct* child of a RowLayout/ColumnLayout — the layout
// would manage it as a cell. Focus an inner Rectangle/Item in those widgets.
Rectangle {
    property Item target: parent

    anchors.fill: parent
    anchors.margins: -Theme.focusRingWidth
    radius: (parent && parent.radius !== undefined ? parent.radius : Theme.radius) + Theme.focusRingWidth
    color: "transparent"
    border.color: Theme.focusRing
    border.width: Theme.focusRingWidth
    visible: target ? target.activeFocus : false
}
