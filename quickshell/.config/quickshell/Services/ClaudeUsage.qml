pragma Singleton
import QtQuick
import Quickshell
import Quickshell.Io
import qs

// Token usage parsed from Claude Code transcripts (~/.claude/projects/**.jsonl).
//  - per live session: context fill (last assistant turn's input side) and
//    session totals, re-read only when the transcript mtime changes;
//  - daily totals for the last 7 days, re-aggregated every few minutes.
// Assistant turns are split into one line per content block sharing the same
// message id and usage, so both aggregations dedupe on id.
Singleton {
    id: root

    property var bySession: ({})    // session id -> { model, ctx, in, out, cr, cc, turns }
    property var daily: []          // [{ date, in, out, cr, cc, turns, models: { model: outTokens } }], oldest first
    property var today: null        // entry of `daily` for today's date, or null
    property bool loading: false

    property var _mtimes: ({})      // session id -> transcript mtime already parsed

    // Context window per model. Claude Code shows "[1m]" for the 1M variants.
    function contextWindow(model) {
        return /\[1m\]|-1m/.test(model || "") ? 1000000 : 200000;
    }

    function fmt(n) {
        n = Number(n) || 0;
        if (n >= 1e6) return (n / 1e6).toFixed(n >= 1e7 ? 0 : 1) + "M";
        if (n >= 1e3) return (n / 1e3).toFixed(n >= 1e5 ? 0 : 1) + "k";
        return String(n);
    }

    function total(e) { return e ? (e.in + e.out + e.cr + e.cc) : 0; }

    function shortModel(m) {
        return (m || "").replace(/^claude-/, "").replace(/-\d{8}$/, "");
    }

    // ---- per session ------------------------------------------------------

    readonly property string sessionScript: "
while [ $# -ge 2 ]; do
  id=$1; f=$2; shift 2
  [ -f \"$f\" ] || continue
  printf '%s\\t' \"$id\"
  jq -n -c 'reduce (inputs | select(.type == \"assistant\" and (.message.model // \"\") != \"<synthetic>\") | .message) as $m
    ({model: \"\", ctx: 0, in: 0, out: 0, cr: 0, cc: 0, turns: 0, last: \"\"};
     if $m.id == .last then . else
       .last = $m.id | .turns += 1 | .model = ($m.model // .model)
       | .in += ($m.usage.input_tokens // 0) | .out += ($m.usage.output_tokens // 0)
       | .cr += ($m.usage.cache_read_input_tokens // 0) | .cc += ($m.usage.cache_creation_input_tokens // 0)
       | .ctx = (($m.usage.input_tokens // 0) + ($m.usage.cache_read_input_tokens // 0) + ($m.usage.cache_creation_input_tokens // 0))
     end) | del(.last)' \"$f\" 2>/dev/null || echo '{}'
done"

    function refreshSessions() {
        if (sessionProc.running) return;
        const args = [];
        const seen = {};
        for (const s of Sessions.sessions) {
            if (!s.transcript || !s.transcript_mtime) continue;
            seen[s.id] = true;
            if (_mtimes[s.id] === s.transcript_mtime) continue;
            args.push(s.id, s.transcript);
        }
        // Drop stats of sessions that are gone.
        const kept = {};
        for (const id in bySession) if (seen[id]) kept[id] = bySession[id];
        if (Object.keys(kept).length !== Object.keys(bySession).length) bySession = kept;
        if (args.length === 0) return;
        sessionProc.pending = args;
        sessionProc.command = ["sh", "-c", root.sessionScript, "sh"].concat(args);
        sessionProc.running = true;
    }

    Process {
        id: sessionProc
        property var pending: []
        stdout: StdioCollector {
            onStreamFinished: {
                const next = Object.assign({}, root.bySession);
                const mt = Object.assign({}, root._mtimes);
                const live = {};
                for (const s of Sessions.sessions) live[s.id] = s.transcript_mtime;
                for (const line of text.split("\n")) {
                    const tab = line.indexOf("\t");
                    if (tab < 0) continue;
                    const id = line.substring(0, tab);
                    try {
                        next[id] = JSON.parse(line.substring(tab + 1));
                        mt[id] = live[id];
                    } catch (e) {}
                }
                root.bySession = next;
                root._mtimes = mt;
            }
        }
    }

    Connections {
        target: Sessions
        function onSessionsChanged() { root.refreshSessions() }
    }

    // ---- daily totals ------------------------------------------------------

    readonly property string dailyScript: "
find \"$HOME/.claude/projects\" -name '*.jsonl' -mtime -8 -print0 2>/dev/null \\
| xargs -0 -r nice jq -c 'select(.type == \"assistant\" and (.message.model // \"\") != \"<synthetic>\" and .message.usage != null)
    | {d: (try (.timestamp | sub(\"\\\\.[0-9]+Z$\"; \"Z\") | fromdate | localtime | strftime(\"%Y-%m-%d\")) catch .timestamp[0:10]),
       id: .message.id, m: .message.model, u: .message.usage}' 2>/dev/null \\
| jq -s -c 'unique_by(.id) | group_by(.d) | map({
    date: .[0].d, turns: length,
    in: (map(.u.input_tokens // 0) | add), out: (map(.u.output_tokens // 0) | add),
    cr: (map(.u.cache_read_input_tokens // 0) | add), cc: (map(.u.cache_creation_input_tokens // 0) | add),
    models: (group_by(.m) | map({(.[0].m): (map(.u.output_tokens // 0) | add)}) | add)
  })'"

    function refreshDaily() {
        if (dailyProc.running) return;
        loading = true;
        dailyProc.running = true;
    }

    Process {
        id: dailyProc
        command: ["sh", "-c", root.dailyScript]
        stdout: StdioCollector {
            onStreamFinished: {
                let parsed = [];
                try { parsed = JSON.parse(text) || []; } catch (e) {}
                // Always seven consecutive days ending today, zero-filled.
                const list = [];
                for (let i = 6; i >= 0; i--) {
                    const d = new Date();
                    d.setDate(d.getDate() - i);
                    const date = Qt.formatDate(d, "yyyy-MM-dd");
                    list.push(parsed.find(e => e.date === date)
                              || { date: date, turns: 0, in: 0, out: 0, cr: 0, cc: 0, models: null });
                }
                root.daily = list;
                root.today = list[list.length - 1];
            }
        }
        onExited: root.loading = false
    }

    Timer {
        interval: 300000
        running: ShellState.aiDashOpen
        repeat: true
        triggeredOnStart: true
        onTriggered: root.refreshDaily()
    }
}
