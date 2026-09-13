# Quickshell AI Agent Dashboard — Plan

Status: complete 2026-09-13. Hooks in ~/.claude/settings.json and SUPER+A bind applied. User to: chmod +x quickshell/.local/bin/ai-dash-hook, `just quickshell::restow`, restart quickshell.

## Goal
One toggleable Quickshell surface showing all AI activity on this machine:
live agent sessions (Claude Code in a terminal, Claude Code via Emacs agent-shell,
OpenCode, future harnesses such as Pi), token/context usage, Ollama + GPU state,
and quick actions. Built as a sibling of the existing control center, reusing
`Theme.qml` and `Widgets/`.

## Constraints (carried over from the control center)
- Arch + Hyprland, Quickshell 0.3.1. No new packages without asking.
- Gruvbox theme, JetBrainsMono Nerd Font / Symbols Nerd Font.
- Phase-gated. `hyprland.conf`/`hyprland.lua`, `~/.claude/settings.json` and the Emacs
  config are only touched after an explicit go for that specific change.
- No new long-running daemon: everything is Quickshell `Process`/`FileView` polling
  plus one small shell hook script that Claude Code invokes.

## Decisions (answered 2026-09-13)
- Form factor: **separate panel**, `quickshell ipc call aiDash toggle` (+ `open`/`close`).
- v1 content: **live sessions, tokens/cost, Ollama + GPU, quick actions**.
- Claude Code source: **hooks + transcript jsonl**.
- Emacs: **`emacsclient --eval`** against a small elisp status function.

## Decisions round 2 (2026-09-13)
1. Keybind: `SUPER+A`.
2. Layout: centered two-column overlay ~1000×640.
3. Emacs config: none needed — the elisp is sent inline via `emacsclient --eval` from `Services/ai-dash-status.el`.
4. Ollama: both native `localhost:11434` and the k3s instance behind the `ts-ollama` Tailscale ingress.
5. Terminal: `kitty` (`$terminal` in hyprland.conf).
6. Cost: tokens + context fill only, no `$`.
7. Hook script: `quickshell/.local/bin/ai-dash-hook`.

## Architecture

### Session registry (the extension point)
`$XDG_RUNTIME_DIR/ai-dash/sessions/<tool>-<id>.json`, one file per live session.
Anything that writes this schema shows up in the dashboard — this is how Pi/OpenCode/
anything else is added later without touching QML.

```json
{
  "tool": "claude-code",           // claude-code | opencode | pi | ...
  "id": "session uuid",
  "pid": 1123260,                  // dashboard prunes files whose pid is gone
  "cwd": "/home/celsuss/workspace/dotfiles",
  "frontend": "terminal",          // terminal | emacs
  "state": "working",              // working | waiting | idle
  "started": 1757780000,
  "updated": 1757780123,
  "last_tool": "Bash",
  "last_prompt": "first 80 chars of the last user prompt",
  "transcript": "~/.claude/projects/<proj>/<id>.jsonl"
}
```

### Producers
| Source | Mechanism |
|---|---|
| Claude Code (terminal + Emacs agent-shell) | `ai-dash-hook` registered in `~/.claude/settings.json` for `SessionStart`, `UserPromptSubmit`, `PreToolUse`, `PostToolUse`, `Notification`, `Stop`, `SessionEnd`. Reads the hook JSON on stdin (`session_id`, `cwd`, `hook_event_name`, `tool_name`, notification type, `transcript_path`) and rewrites the session file. `Notification` of type permission prompt → `waiting`; `Stop` → `idle`; `UserPromptSubmit`/`PreToolUse` → `working`; `SessionEnd` → delete. `frontend` = `emacs` when the parent `claude` cmdline contains `--input-format stream-json` (ACP), else `terminal`. Model is read from the transcript by the dashboard (last assistant message's `message.model`). |
| OpenCode | v1: process scan (`pgrep -x opencode` + `/proc/<pid>/cwd`) → synthetic file, state `unknown`. v2 (phase 7): an OpenCode plugin in `~/.config/opencode/plugin/` emitting the same schema from its event stream. |
| Emacs gptel / ellama / agent-shell | Not via files. `Services/EmacsAi.qml` runs `emacsclient --eval '(ai-dash-status)'` every 5 s while the panel is open; `ai-dash.el` returns JSON: gptel buffers (name, backend, model, request in flight?), ellama provider/model + chat buffers, agent-shell buffers (agent, busy?). agent-shell's `claude` subprocess also appears via the hook; the two are merged by matching the hook's pid against Emacs' child pids. |

### Consumers (QML singletons in `Services/`)
| Service | Backend |
|---|---|
| `Sessions.qml` | Lists/reads `$XDG_RUNTIME_DIR/ai-dash/sessions/*.json` (Process `cat` poll every 2 s while open, `inotifywait` if installed — verified in phase 0), prunes dead pids, adds OpenCode scan results. |
| `ClaudeUsage.qml` | Per live session: tail the transcript, last assistant `usage` → context fill (`input + cache_read + cache_creation` vs. model window); sum of all assistant `usage` → session totals. Daily/7-day totals: aggregate `~/.claude/projects/*/*.jsonl` modified in the last 7 days with a `jq` one-liner in a Process (or `~/.claude/stats-cache.json` if it already holds daily totals — verified in phase 0). |
| `Ollama.qml` | `curl` (or bash `/dev/tcp`, same trick as `Mpd.qml`) to `/api/ps` (loaded models, VRAM, expiry), `/api/tags` (installed), `/api/version`. Actions: unload (`POST /api/generate {"model":m,"keep_alive":0}`), keep warm (`keep_alive:-1`), load (`keep_alive` default). |
| `Gpu.qml` (or extend `Stats.qml`) | `nvidia-smi --query-gpu=utilization.gpu,memory.used,memory.total,temperature.gpu` + `--query-compute-apps=pid,used_memory` so Ollama's VRAM share is shown. |
| `Launcher` (functions in `Sessions.qml`) | Focus: `hyprctl clients -j`, pick the client whose pid is an ancestor of the session pid, `hyprctl dispatch focuswindow address:…`. Kill: `kill -TERM <pid>`. New session: `<terminal> -e claude` with `--cwd` from a recent-projects picker (decoded from `~/.claude/projects/` dir names). Emacs: `emacsclient -c` / focus `class:emacs`. Restart Ollama: `systemctl restart ollama` (may need polkit — verified in phase 0). |

## UI (assuming layout option a)
```
┌ AI Dashboard        3 sessions · 1 waiting · today 1.2M tok            [x] ┐
│ SESSIONS                              │ OLLAMA                            │
│ ● dotfiles   claude-code · terminal   │ gpu ▓▓▓░░ 34%  vram 6.1/12 GB 61°C │
│   opus-5 · working · 12m · ctx ▓▓░ 41%│ qwen2.5-coder:14b  5.9 GB  ⏱ 4m   │
│   ▸ Bash: git status         [⌖][✕]   │   [keep warm] [unload]            │
│ ◐ Ratatoskr  claude-code · emacs      │ installed ▾  [load]               │
│   sonnet-5 · WAITING · 3m · ctx 12%   ├───────────────────────────────────┤
│ ○ Home-Lab   opencode                 │ USAGE   today  7d                 │
│                                       │ in/out/cache … ▁▃▅▂▇▆▃            │
│                                       ├───────────────────────────────────┤
│                                       │ EMACS  gptel: ollama/qwen (idle)  │
│                                       │        agent-shell: claude (busy) │
├───────────────────────────────────────┴───────────────────────────────────┤
│ [+ claude in …▾]  [emacs]  [restart ollama]                               │
└───────────────────────────────────────────────────────────────────────────┘
```
- State pill colours: waiting = `Theme.orange` (pulsing), working = `Theme.green`,
  idle = `Theme.gray`, unknown = `Theme.blue`.
- Header count of `waiting` sessions is the thing you glance at.
- Optional (phase 7): `notify-send` from the hook when a session enters `waiting`,
  routed through the existing notification daemon.

## Files
```
quickshell/.config/quickshell/
  shell.qml                    # + AiDash Panel, IpcHandler target "aiDash"
  ShellState.qml               # + aiDashOpen / openAiDash() / …
  AiDash/
    Panel.qml                  # window, animation, two-column layout
    Header.qml
    SessionList.qml, SessionRow.qml
    OllamaCard.qml, UsageCard.qml, EmacsCard.qml
    ActionsFooter.qml
  Services/
    Sessions.qml, ClaudeUsage.qml, Ollama.qml, Gpu.qml, EmacsAi.qml
  Widgets/
    StatePill.qml, BarStrip.qml   # only if Meter/Label don't cover it
quickshell/.local/bin/ai-dash-hook           # POSIX sh, stowed to ~/.local/bin (Q7)
<emacs config>/ai-dash.el                    # (Q3)
~/.claude/settings.json                      # hooks block — proposed as a diff, applied on approval
hyprland/hypr/hyprland.conf (+ .lua)         # bind — applied on approval
quickshell/README.md                         # dashboard section
```

## Phases (propose → approve → implement → you test with `quickshell -p quickshell/.config/quickshell`)
0. **Verify (read-only)** — needs Bash grants that were denied this session: shape of
   `~/.claude/stats-cache.json`, hook stdin fields on this Claude Code version, `jq` /
   `inotifywait` / `curl` availability, Ollama port + `OLLAMA_HOST`, `nvidia-smi` fields,
   `systemctl restart ollama` permissions. Resolves Q4/Q5 defaults.
1. **Skeleton + registry** — `AiDash/Panel.qml` toggling via IPC, `Sessions.qml`,
   `ai-dash-hook` script tested by piping sample hook JSON into it. No config edits.
2. **Claude Code live** — hooks added to `~/.claude/settings.json` (approval), `SessionRow`
   with state pills, model, elapsed, last tool; focus + kill actions.
3. **Ollama + GPU card** — loaded/installed models, VRAM per model, load/unload/keep-warm.
4. **Usage card** — per-session context fill, session totals, today/7-day aggregates.
5. **Emacs card** — `ai-dash.el` (approval, Q3) + `EmacsAi.qml`, merge with agent-shell sessions.
6. **Actions footer + integration** — project picker launcher (Q5), hyprland bind (approval, Q1),
   README, restow.
7. **Later / optional** — OpenCode plugin producer, Pi adapter, `waiting` desktop notification,
   `$` estimate toggle (Q6).
