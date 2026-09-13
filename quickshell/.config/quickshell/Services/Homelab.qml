pragma Singleton
import QtQuick
import Quickshell
import Quickshell.Io
import qs

// Local k3s cluster health and the services exposed through Tailscale
// ingresses, via kubectl (already configured for this user).
Singleton {
    id: root

    property bool available: false
    property string error: ""
    property var nodes: []          // [{ name, ready }]
    property int podsTotal: 0
    property int podsHealthy: 0
    property var unhealthyPods: []  // [{ ns, name, status }]
    property var services: []       // [{ name, ns, host, url }]
    property bool loading: false

    readonly property bool allReady: nodes.length > 0 && nodes.every(n => n.ready) && unhealthyPods.length === 0

    readonly property string script: "
echo '#nodes'
kubectl get nodes --no-headers -o custom-columns='NAME:.metadata.name,READY:.status.conditions[?(@.type==\"Ready\")].status' 2>&1
echo '#pods'
kubectl get pods -A --no-headers -o custom-columns='NS:.metadata.namespace,NAME:.metadata.name,PHASE:.status.phase,READY:.status.containerStatuses[*].ready,WAIT:.status.containerStatuses[*].state.waiting.reason,RESTARTS:.status.containerStatuses[*].restartCount' 2>&1
echo '#ingress'
kubectl get ingress -A --no-headers -o custom-columns='NS:.metadata.namespace,NAME:.metadata.name,CLASS:.spec.ingressClassName,HOST:.status.loadBalancer.ingress[0].hostname' 2>&1"

    function refresh() {
        if (proc.running) return;
        loading = true;
        proc.running = true;
    }

    function open(svc) { Quickshell.execDetached(["xdg-open", svc.url]); }

    // "immich-tailscale-ingress" -> "immich"
    function prettyName(name) {
        return name.replace(/-tailscale-ingress$/, "").replace(/-ingress$/, "").replace(/-app$/, "");
    }

    Process {
        id: proc
        command: ["sh", "-c", root.script]
        stdout: StdioCollector {
            onStreamFinished: {
                let section = "";
                const nodes = [], unhealthy = [], services = [];
                let total = 0, healthy = 0;
                let err = "";
                for (const raw of text.split("\n")) {
                    const line = raw.trim();
                    if (line === "") continue;
                    if (line[0] === "#") { section = line.substring(1); continue; }
                    if (/^(error|Unable|The connection)/i.test(line)) { err = line; continue; }
                    const f = line.split(/\s+/);
                    if (section === "nodes") {
                        nodes.push({ name: f[0], ready: f[1] === "True" });
                    } else if (section === "pods") {
                        total++;
                        const phase = f[2], ready = f[3] || "";
                        const ok = phase === "Succeeded" || (phase === "Running" && ready.indexOf("false") === -1);
                        if (ok) { healthy++; continue; }
                        // Multi-container pods give comma lists; report the first real reason.
                        const wait = (f[4] || "").split(",").find(r => r !== "" && r !== "<none>") || phase;
                        const restarts = (f[5] || "").split(",").map(Number).reduce((a, b) => a + (b || 0), 0);
                        unhealthy.push({ ns: f[0], name: f[1], status: wait + (restarts > 0 ? " ×" + restarts : "") });
                    } else if (section === "ingress") {
                        if (f[2] === "tailscale" && f[3] && f[3] !== "<none>")
                            services.push({ ns: f[0], name: root.prettyName(f[1]), host: f[3], url: "https://" + f[3] });
                    }
                }
                root.error = err;
                root.available = err === "" && nodes.length > 0;
                root.nodes = nodes;
                root.podsTotal = total;
                root.podsHealthy = healthy;
                root.unhealthyPods = unhealthy;
                root.services = services.sort((a, b) => a.name.localeCompare(b.name));
            }
        }
        onExited: root.loading = false
    }

    Timer {
        interval: 30000
        running: ShellState.panelOpen
        repeat: true
        triggeredOnStart: true
        onTriggered: root.refresh()
    }
}
