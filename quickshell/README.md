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

## AI dashboard

Centered overlay showing live AI agent sessions, Claude Code token usage, Ollama
(local + k3s) with GPU meters, and gptel/ellama/agent-shell state from Emacs.

- `SUPER+A` toggles it (`quickshell ipc call aiDash toggle`; also `open`/`close`).
- Sessions come from a registry of JSON files in `$XDG_RUNTIME_DIR/ai-dash/sessions/`,
  one per session (schema documented in `.local/bin/ai-dash-hook`). Files whose `pid`
  is gone are pruned. Claude Code writes them through `ai-dash-hook`, registered as a
  hook in `~/.claude/settings.json` (SessionStart, UserPromptSubmit, PreToolUse,
  PostToolUse, Notification, PermissionRequest, Stop, SessionEnd). Any other tool can
  write the same schema and will show up. OpenCode is found by process scan only.
- Token usage is aggregated from `~/.claude/projects/**.jsonl` with `jq`; per-session
  context fill uses the last assistant turn's input side vs. a 200k (1M for `[1m]`) window.
- Emacs state is read with `emacsclient --eval` of `Services/ai-dash-status.el`; nothing
  is added to the Emacs config.
- Ollama hosts are listed in `Services/Ollama.qml` (the k3s instance by ClusterIP).

Layout: `AiDash/` (Dashboard window + cards) · `Services/` (Sessions, ClaudeUsage,
Ollama, EmacsAi, Launcher) · `.local/bin/ai-dash-hook`.
