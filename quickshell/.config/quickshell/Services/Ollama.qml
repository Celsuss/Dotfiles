pragma Singleton
import QtQuick
import Quickshell
import Quickshell.Io
import qs

// Ollama instances polled over their HTTP API with curl. `hosts` lists every
// instance to show; the k3s one is reached on its ClusterIP because the
// Tailscale ingress hostname only resolves while Tailscale is up.
Singleton {
    id: root

    readonly property var hosts: [
        { name: "local", url: "http://127.0.0.1:11434" },
        { name: "k3s",   url: "http://10.43.203.193:11434" }
    ]

    // host name -> { online, version, loaded: [{ name, vram, size, expires, until }],
    //                installed: [{ name, size, params, quant, family }] }
    property var status: ({})
    property string current: hosts[0].name
    property bool loading: false
    property string lastError: ""

    readonly property var currentStatus: status[current] || { online: false, loaded: [], installed: [] }
    readonly property int loadedTotal: {
        let n = 0;
        for (const h in status) n += (status[h].loaded || []).length;
        return n;
    }

    readonly property string script: "
for h in \"$@\"; do
  name=${h%%=*}; url=${h#*=}
  v=$(curl -s --max-time 2 \"$url/api/version\") || v=''
  printf '%s\\tversion\\t%s\\n' \"$name\" \"$v\"
  [ -n \"$v\" ] || continue
  printf '%s\\tps\\t%s\\n'   \"$name\" \"$(curl -s --max-time 2 \"$url/api/ps\" | tr -d '\\n')\"
  printf '%s\\ttags\\t%s\\n' \"$name\" \"$(curl -s --max-time 2 \"$url/api/tags\" | tr -d '\\n')\"
done"

    function refresh() {
        if (proc.running) return;
        loading = true;
        proc.running = true;
    }

    function hostUrl(name) {
        const h = hosts.find(x => x.name === name);
        return h ? h.url : "";
    }

    function gib(bytes) { return ((Number(bytes) || 0) / 1073741824).toFixed(1) + " GB"; }

    // Remaining keep-alive as a short string ("4m", "∞", "").
    function expiresIn(m) {
        if (!m.expires) return "";
        const t = Date.parse(m.expires);
        if (isNaN(t)) return "";
        const secs = Math.round((t - Date.now()) / 1000);
        if (secs > 3600 * 24 * 365) return "∞";
        if (secs <= 0) return "0s";
        if (secs < 60) return secs + "s";
        if (secs < 3600) return Math.round(secs / 60) + "m";
        return Math.round(secs / 3600) + "h";
    }

    // keep_alive: 0 unloads, -1 keeps the model resident, "5m" is the default.
    function setKeepAlive(model, keepAlive) {
        const url = hostUrl(current);
        if (!url) return;
        action.command = ["curl", "-s", "--max-time", "120", "-X", "POST", url + "/api/generate",
                          "-d", JSON.stringify({ model: model, keep_alive: keepAlive })];
        action.running = true;
    }
    function load(model)     { setKeepAlive(model, "5m"); }
    function keepWarm(model) { setKeepAlive(model, -1); }
    function unload(model)   { setKeepAlive(model, 0); }

    function restart() {
        restartProc.running = true;
    }

    Process {
        id: proc
        command: ["sh", "-c", root.script, "sh"].concat(root.hosts.map(h => h.name + "=" + h.url))
        stdout: StdioCollector {
            onStreamFinished: {
                const next = {};
                for (const h of root.hosts) next[h.name] = { online: false, version: "", loaded: [], installed: [] };
                for (const line of text.split("\n")) {
                    const f = line.split("\t");
                    if (f.length < 3 || !next[f[0]]) continue;
                    const st = next[f[0]];
                    let body = null;
                    try { body = f[2] ? JSON.parse(f[2]) : null; } catch (e) {}
                    if (f[1] === "version") {
                        st.online = !!body;
                        st.version = body ? body.version : "";
                    } else if (f[1] === "ps" && body) {
                        st.loaded = (body.models || []).map(m => ({
                            name: m.name, vram: m.size_vram, size: m.size, expires: m.expires_at,
                            ctx: m.context_length || 0
                        }));
                    } else if (f[1] === "tags" && body) {
                        st.installed = (body.models || []).map(m => ({
                            name: m.name, size: m.size,
                            params: m.details ? m.details.parameter_size : "",
                            quant: m.details ? m.details.quantization_level : "",
                            family: m.details ? m.details.family : ""
                        })).sort((a, b) => a.name.localeCompare(b.name));
                    }
                }
                root.status = next;
            }
        }
        onExited: root.loading = false
    }

    Process {
        id: action
        stdout: StdioCollector { onStreamFinished: root.refresh() }
        stderr: StdioCollector { onStreamFinished: root.lastError = text.trim() }
    }

    Process {
        id: restartProc
        command: ["systemctl", "restart", "ollama"]
        stderr: StdioCollector { onStreamFinished: root.lastError = text.trim() }
        onExited: root.refresh()
    }

    Timer {
        interval: 5000
        running: ShellState.aiDashOpen
        repeat: true
        triggeredOnStart: true
        onTriggered: root.refresh()
    }
}
