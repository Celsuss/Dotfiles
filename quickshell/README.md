# quickshell

Control center + notification daemon for Hyprland. Config lives in
`.config/quickshell/` and is stowed from this directory: `stow . -t ~`
(`README.md` is excluded via `.stow-local-ignore`).

- `SUPER+N` toggles the panel (`quickshell ipc call controlCenter toggle`; also `open`/`close`).
- Started from `hyprland.conf` `exec-once` in place of swaync. Only one daemon can own
  `org.freedesktop.Notifications`; if quickshell is not running, D-Bus activation will
  start swaync on the next notification.
- Notification history: `~/.local/state/quickshell/by-shell/<id>/notifications.json`.
- Logs: `/run/user/1000/quickshell/by-id/<id>/log.qslog`.

Layout: `shell.qml` (entry + IPC) · `Theme.qml` (gruvbox) · `ShellState.qml` (panel/DND state)
· `ControlCenter/` (panel + cards) · `Notifications/` (toasts) · `Services/` (backends: Nord,
Tailscale, Audio, Mpd, Stats, Homelab, Notifs) · `Widgets/` (shared controls).

Develop without stowing: `quickshell -p quickshell/.config/quickshell` (then pass `-p` to `ipc` too).
