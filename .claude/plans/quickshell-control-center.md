# Quickshell Control Center — Plan

Status: complete 2026-09-13. All phases done; hyprland.conf + hyprland.lua updated (SUPER+N, exec-once quickshell), package stowed.

## Constraints
- Arch + Hyprland 0.56.2, Quickshell 0.3.1. No new packages without asking.
- Desktop, single 3440x1440 monitor (no battery/backlight widgets).
- Network is systemd-networkd only (no NM/iwd) -> no Wi-Fi management, status only.
- Theme: Gruvbox dark, JetBrainsMono Nerd Font, FontAwesome.
- Repo layout: `quickshell/.config/quickshell/` (stow-style, symlinked later).

## Decisions (approved by user)
- Quickshell replaces swaync as notification daemon (done in the last phase).
- VPN: NordVPN + Tailscale.
- Extras: MPD/radio control, power footer, system stats, homelab card.
- Layout: right-edge full-height slide-in panel, ~450px, toggled via IPC.

## Layout (top -> bottom)
1. Header: clock/date, DND toggle, close.
2. Quick toggles: DND · NordVPN · Tailscale · Bluetooth.
3. VPN card
   - NordVPN: status, server/country, connect/disconnect, country picker, kill-switch.
   - Tailscale: up/down, tailnet IP, exit-node picker, online peers count.
4. Audio card: sink volume/mute, sink picker, source mute, per-app streams.
5. Bluetooth card: adapter toggle, paired devices connect/disconnect, battery.
6. Media card: MPRIS now-playing (Spotify etc.) + MPD radio station switcher.
7. System stats: CPU/GPU temp, mem, GPU util (nvidia-smi).
8. Homelab: k3s pod health summary + Tailscale-ingress services with open-in-browser.
9. Notifications: grouped list, dismiss one/all, actions, history; popup toasts.
10. Footer: lock · suspend · reboot · shutdown · logout.

## Backends
| Section | Backend |
|---|---|
| Audio | Quickshell.Services.Pipewire |
| Bluetooth | Quickshell.Bluetooth |
| MPRIS | Quickshell.Services.Mpris |
| MPD | QML Socket -> localhost:6600 (MPD protocol) |
| Notifications | Quickshell.Services.Notifications |
| NordVPN | `nordvpn status/connect/disconnect/countries`, polled Process |
| Tailscale | `tailscale status --json`, `tailscale up/down`, `tailscale set --exit-node` |
| Stats | /proc, /sys hwmon, `nvidia-smi --query-gpu ... --format=csv` |
| Homelab | `kubectl get pods -A -o json`, `kubectl get ingress -A -o json` (class tailscale) |
| Power | hyprlock, `systemctl suspend/reboot/poweroff`, `hyprctl dispatch exit` |
| Toggle | `quickshell ipc call controlCenter toggle` |

## File structure
```
quickshell/.config/quickshell/
  shell.qml                 # entry: ShellRoot, IPC handler, panel + toasts
  Theme.qml                 # Gruvbox palette singleton
  ControlCenter/
    Panel.qml               # layer-shell window, slide animation, sections
    Header.qml, QuickToggles.qml, VpnCard.qml, AudioCard.qml,
    BluetoothCard.qml, MediaCard.qml, StatsCard.qml, HomelabCard.qml,
    NotificationList.qml, PowerFooter.qml
  Notifications/
    Toast.qml               # popup notifications (top-right)
  Services/                 # QML singletons
    Nord.qml, Tailscale.qml, Mpd.qml, Stats.qml, Homelab.qml, Notifs.qml
  Widgets/                  # shared: Card, Toggle, Slider, IconButton
```

## Phases
1. Skeleton: shell.qml, Theme, Panel with IPC toggle, header, power footer. Run with `quickshell -p quickshell/.config/quickshell`.
2. VPN card (Nord + Tailscale) + quick toggles.
3. Audio + Bluetooth cards.
4. Media (MPRIS + MPD) + Stats + Homelab cards.
5. Notifications: server + list + toasts; then swap swaync -> quickshell in hyprland.lua/hyprland.conf and add keybind.
