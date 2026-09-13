import QtQuick
import qs

Text {
    property bool dim: false
    color: dim ? Theme.gray : Theme.fg
    font.family: Theme.font
    font.pixelSize: Theme.fontSize
    elide: Text.ElideRight
}
