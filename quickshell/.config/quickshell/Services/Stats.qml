pragma Singleton
import QtQuick
import Quickshell
import Quickshell.Io
import qs

// CPU / memory / GPU sampling. Only polls while the panel is open.
Singleton {
    id: root

    property real cpuUsage: 0      // 0..1
    property int cpuTemp: 0        // °C (k10temp Tctl)
    property real memUsed: 0       // GiB
    property real memTotal: 0
    property int gpuUsage: 0       // %
    property int gpuTemp: 0
    property real vramUsed: 0      // GiB
    property real vramTotal: 0
    property bool hasGpu: false

    property var _lastCpu: null    // [idle, total]

    readonly property string script: "
head -1 /proc/stat
grep -E '^(MemTotal|MemAvailable):' /proc/meminfo
for h in /sys/class/hwmon/*; do
  [ \"$(cat $h/name 2>/dev/null)\" = k10temp ] && echo \"cputemp $(cat $h/temp1_input)\"
done
command -v nvidia-smi >/dev/null && echo \"gpu $(nvidia-smi --query-gpu=utilization.gpu,temperature.gpu,memory.used,memory.total --format=csv,noheader,nounits)\""

    function refresh() { if (!proc.running) proc.running = true; }

    Process {
        id: proc
        command: ["sh", "-c", root.script]
        stdout: StdioCollector {
            onStreamFinished: {
                let total = 0, avail = 0;
                for (const line of text.split("\n")) {
                    const f = line.trim().split(/\s+/);
                    if (f[0] === "cpu") {
                        const nums = f.slice(1).map(Number);
                        const idle = nums[3] + nums[4];
                        const sum = nums.reduce((a, b) => a + b, 0);
                        if (root._lastCpu) {
                            const dt = sum - root._lastCpu[1];
                            if (dt > 0) root.cpuUsage = 1 - (idle - root._lastCpu[0]) / dt;
                        }
                        root._lastCpu = [idle, sum];
                    } else if (f[0] === "MemTotal:") total = Number(f[1]);
                    else if (f[0] === "MemAvailable:") avail = Number(f[1]);
                    else if (f[0] === "cputemp") root.cpuTemp = Math.round(Number(f[1]) / 1000);
                    else if (f[0] === "gpu") {
                        const g = line.substring(4).split(",").map(s => Number(s.trim()));
                        if (g.length >= 4) {
                            root.hasGpu = true;
                            root.gpuUsage = g[0];
                            root.gpuTemp = g[1];
                            root.vramUsed = g[2] / 1024;
                            root.vramTotal = g[3] / 1024;
                        }
                    }
                }
                if (total > 0) {
                    root.memTotal = total / 1048576;
                    root.memUsed = (total - avail) / 1048576;
                }
            }
        }
    }

    Timer {
        interval: 2000
        running: ShellState.panelOpen
        repeat: true
        triggeredOnStart: true
        onTriggered: root.refresh()
    }
}
