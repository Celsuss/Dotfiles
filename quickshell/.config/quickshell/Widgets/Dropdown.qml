import QtQuick
import QtQuick.Layouts
import qs

// Inline dropdown: a button showing the current value; clicking expands a
// filterable list below it (no popup windows on layer-shell).
ColumnLayout {
    id: root

    property string placeholder: "Select…"
    property string current: ""
    property var items: []          // list of strings
    property bool expanded: false
    property int maxListHeight: 180
    property bool filterable: items.length > 8

    signal selected(string value)

    Layout.fillWidth: true
    spacing: 4

    Rectangle {
        id: head
        Layout.fillWidth: true
        implicitHeight: 32
        radius: Theme.radius
        color: headMouse.containsMouse ? Theme.bg3 : Theme.bg2

        activeFocusOnTab: true

        Keys.onReturnPressed: root.expanded ? root.collapse() : root.open()
        Keys.onEnterPressed:  root.expanded ? root.collapse() : root.open()
        Keys.onSpacePressed:  root.expanded ? root.collapse() : root.open()
        Keys.onEscapePressed: event => {
            // Only swallow Escape while open; otherwise let the panel close.
            if (root.expanded) { root.collapse(); event.accepted = true; }
            else event.accepted = false;
        }
        Keys.onPressed: event => {
            if (event.key === Qt.Key_L || event.key === Qt.Key_Right) {
                root.open();
                event.accepted = true;
            } else if ((event.key === Qt.Key_H || event.key === Qt.Key_Left) && root.expanded) {
                root.collapse();
                event.accepted = true;
            }
        }

        FocusRing {}

        RowLayout {
            anchors {
                fill: parent
                leftMargin: Theme.padding
                rightMargin: Theme.padding
            }
            Label {
                text: root.current !== "" ? root.current : root.placeholder
                dim: root.current === ""
                Layout.fillWidth: true
            }
            Text {
                text: root.expanded ? "󰅃" : "󰅀"
                color: Theme.gray
                font.family: Theme.iconFont
                font.pixelSize: 14
            }
        }

        MouseArea {
            id: headMouse
            anchors.fill: parent
            hoverEnabled: true
            cursorShape: Qt.PointingHandCursor
            onClicked: root.expanded ? root.collapse() : root.open()
        }
    }

    Rectangle {
        visible: root.expanded
        Layout.fillWidth: true
        implicitHeight: listCol.implicitHeight + 8
        radius: Theme.radius
        color: Theme.bg2
        border.color: Theme.border
        border.width: 1

        ColumnLayout {
            id: listCol
            anchors {
                fill: parent
                margins: 4
            }
            spacing: 4

            Rectangle {
                visible: root.filterable
                Layout.fillWidth: true
                implicitHeight: 28
                radius: Theme.radius - 4
                color: Theme.bg1

                TextInput {
                    id: filter
                    anchors {
                        fill: parent
                        leftMargin: 8
                        rightMargin: 8
                    }
                    verticalAlignment: TextInput.AlignVCenter
                    color: Theme.fg
                    font.family: Theme.font
                    font.pixelSize: Theme.fontSize
                    clip: true
                    Keys.onEscapePressed: root.collapse()
                    Keys.onReturnPressed: root.pickCurrent()
                    Keys.onEnterPressed:  root.pickCurrent()
                    Keys.onPressed: event => {
                        const down = event.key === Qt.Key_Down
                                  || (event.key === Qt.Key_N && (event.modifiers & Qt.ControlModifier));
                        const up = event.key === Qt.Key_Up
                                || (event.key === Qt.Key_P && (event.modifiers & Qt.ControlModifier));
                        if (down)      { list.nav(1);  event.accepted = true; }
                        else if (up)   { list.nav(-1); event.accepted = true; }
                    }

                    Label {
                        anchors.fill: parent
                        verticalAlignment: Text.AlignVCenter
                        text: "Filter…"
                        dim: true
                        visible: filter.text === ""
                    }
                }
            }

            ListView {
                id: list
                Layout.fillWidth: true
                implicitHeight: Math.min(contentHeight, root.maxListHeight)
                clip: true
                boundsBehavior: Flickable.StopAtBounds
                currentIndex: 0

                function nav(delta) {
                    if (count === 0) return;
                    currentIndex = Math.max(0, Math.min(count - 1, currentIndex + delta));
                    positionViewAtIndex(currentIndex, ListView.Contain);
                }

                onCountChanged: currentIndex = count > 0 ? Math.min(currentIndex, count - 1) : -1

                Keys.onEscapePressed: root.collapse()
                Keys.onReturnPressed: root.pickCurrent()
                Keys.onEnterPressed:  root.pickCurrent()
                Keys.onPressed: event => {
                    if (event.key === Qt.Key_J)      { list.nav(1);  event.accepted = true; }
                    else if (event.key === Qt.Key_K) { list.nav(-1); event.accepted = true; }
                }

                model: {
                    const q = filter.text.toLowerCase();
                    return root.items.filter(s => q === "" || s.toLowerCase().indexOf(q) !== -1);
                }
                delegate: Rectangle {
                    required property string modelData
                    required property int index
                    readonly property string value: modelData
                    readonly property bool navCursor: root.expanded && index === list.currentIndex
                    width: list.width
                    height: 28
                    radius: Theme.radius - 4
                    color: navCursor ? Qt.alpha(Theme.accent, 0.45)
                         : modelData === root.current ? Qt.alpha(Theme.accent, 0.25)
                         : (itemMouse.containsMouse ? Theme.bg3 : "transparent")

                    Label {
                        anchors {
                            fill: parent
                            leftMargin: 8
                        }
                        verticalAlignment: Text.AlignVCenter
                        text: modelData
                    }

                    MouseArea {
                        id: itemMouse
                        anchors.fill: parent
                        hoverEnabled: true
                        cursorShape: Qt.PointingHandCursor
                        onClicked: root.pick(modelData)
                    }
                }
            }
        }
    }

    function open() {
        root.expanded = true;
        list.currentIndex = list.count > 0 ? 0 : -1;
        if (root.filterable) filter.forceActiveFocus(Qt.TabFocusReason);
        else list.forceActiveFocus(Qt.TabFocusReason);
    }

    function collapse() {
        // Hand focus back to the head before hiding the popup, or it escapes
        // to the panel root and the j/k position is lost.
        if (filter.activeFocus || list.activeFocus) head.forceActiveFocus(Qt.TabFocusReason);
        root.expanded = false;
        filter.text = "";
    }

    function pickCurrent() {
        const item = list.currentIndex >= 0 ? list.itemAtIndex(list.currentIndex) : null;
        if (item) root.pick(item.value);
    }

    function pick(value) {
        root.collapse();
        root.selected(value);
    }
}
