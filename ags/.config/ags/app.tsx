import { createState } from "ags"
import { Astal, Gtk } from "ags/gtk4"
import app from "ags/gtk4/app"
import { execAsync } from "ags/process"
import Gdk from "gi://Gdk?version=4.0"
import GLib from "gi://GLib?version=2.0"
import css from "./style.css"
import { fraction, GPU_QUERY, parseGpuSnapshot, reading, type GpuSnapshot } from "./gpu"

const POLL_MS = 2000
const CONNECTOR = "HDMI-A-1"
let openedAt = 0
let outsideClosedAt = 0

type Layer = { x: number; y: number; w: number; h: number; namespace: string }

async function clickedOutsideGpu(): Promise<boolean> {
  const [positionText, layersText] = await Promise.all([
    execAsync(["hyprctl", "cursorpos", "-j"]),
    execAsync(["hyprctl", "layers", "-j"]),
  ])
  const position = JSON.parse(positionText) as { x: number; y: number }
  const outputs = JSON.parse(layersText) as Record<string, { levels: Record<string, Layer[]> }>
  const gpuLayer = Object.values(outputs)
    .flatMap((output) => Object.values(output.levels).flat())
    .find((layer) => layer.namespace === "gpu-popup")
  if (!gpuLayer) throw new Error("GPU layer was not found")

  return position.x < gpuLayer.x || position.x >= gpuLayer.x + gpuLayer.w ||
    position.y < gpuLayer.y || position.y >= gpuLayer.y + gpuLayer.h
}

function gpuMonitor(): Gdk.Monitor | null {
  const monitors = Gdk.Display.get_default()?.get_monitors()
  if (!monitors) return null
  for (let index = 0; index < monitors.get_n_items(); index++) {
    const monitor = monitors.get_item(index) as Gdk.Monitor
    if (monitor.get_connector() === CONNECTOR) return monitor
  }
  console.warn(`GPU popup: ${CONNECTOR} is unavailable; using the first monitor`)
  return monitors.get_item(0) as Gdk.Monitor | null
}

app.start({
  instanceName: "gpu-popup",
  css,
  requestHandler(argv, response) {
    const window = app.get_window("gpu")
    if (!window) return response("GPU popup is unavailable")

    if (argv[0] === "waybar-click") {
      if (window.visible) {
        window.set_visible(false)
        return response("closed")
      }
      if (Date.now() - outsideClosedAt < 500) return response("already closed")
      window.set_visible(true)
      return response("opened")
    }

    if (argv[0] === "outside-click") {
      if (!window.visible || Date.now() - openedAt < 250) return response("ignored")
      void clickedOutsideGpu().then((outside) => {
        if (outside && window.visible) {
          outsideClosedAt = Date.now()
          window.set_visible(false)
        }
        response(outside ? "outside" : "inside")
      }).catch((error) => {
        console.error(`GPU popup click check: ${error}`)
        response("click check failed")
      })
      return
    }

    response("unknown command")
  },
  main() {
    const [gpu, setGpu] = createState<GpuSnapshot | null>(null)
    const [error, setError] = createState("")
    let pollSource = 0
    let refreshPending = false
    let generation = 0
    let closeButton: Gtk.Button | null = null
    async function refresh() {
      if (refreshPending) return
      refreshPending = true
      const currentGeneration = generation
      try {
        const snapshot = parseGpuSnapshot(await execAsync(GPU_QUERY))
        if (currentGeneration === generation) {
          setGpu(snapshot)
          setError("")
        }
      } catch (cause) {
        if (currentGeneration === generation) {
          setGpu(null)
          setError("Unable to read GPU telemetry from nvidia-smi")
          console.error(`GPU popup: ${cause}`)
        }
      } finally {
        refreshPending = false
      }
    }

    function onVisible(window: Astal.Window) {
      if (window.visible) {
        openedAt = Date.now()
        setError("")
        void refresh()
        pollSource = GLib.timeout_add(GLib.PRIORITY_DEFAULT, POLL_MS, () => {
          void refresh()
          return GLib.SOURCE_CONTINUE
        })
        GLib.idle_add(GLib.PRIORITY_DEFAULT_IDLE, () => {
          if (window.visible) {
            window.present()
            closeButton?.grab_focus()
          }
          return GLib.SOURCE_REMOVE
        })
      } else {
        generation++
        if (pollSource) GLib.Source.remove(pollSource)
        pollSource = 0
      }
    }

    const window = (
      <window
        name="gpu"
        application={app}
        namespace="gpu-popup"
        class="gpu-window"
        visible={false}
        gdkmonitor={gpuMonitor()}
        anchor={Astal.WindowAnchor.BOTTOM | Astal.WindowAnchor.RIGHT}
        exclusivity={Astal.Exclusivity.NORMAL}
        layer={Astal.Layer.TOP}
        keymode={Astal.Keymode.ON_DEMAND}
        marginBottom={44}
        marginRight={12}
        onNotifyVisible={onVisible}
      >
        <Gtk.EventControllerKey
          propagationPhase={Gtk.PropagationPhase.CAPTURE}
          onKeyPressed={({ widget }, keyval: number) => {
            if (keyval === Gdk.KEY_Escape) {
              widget.get_root()?.set_visible(false)
              return true
            }
            return false
          }}
        />
        <box class="panel" orientation={Gtk.Orientation.VERTICAL} spacing={18}>
          <box class="header" orientation={Gtk.Orientation.HORIZONTAL} spacing={12}>
            <box orientation={Gtk.Orientation.VERTICAL} hexpand>
              <label class="eyebrow" halign={Gtk.Align.START} label="GRAPHICS / GPU 0" />
              <label class="gpu-name" halign={Gtk.Align.START} label={gpu((data) => data?.name ?? "NVIDIA GPU")} />
            </box>
            <button class="close-button" tooltipText="Close GPU details" $={(self) => { closeButton = self }} onClicked={() => window.set_visible(false)}>
              <label label="×" />
            </button>
          </box>

          <box class="error" visible={error((message) => Boolean(message))}>
            <label wrap label={error} />
          </box>

          <box class="meters" orientation={Gtk.Orientation.VERTICAL} spacing={16}>
            <box orientation={Gtk.Orientation.VERTICAL} spacing={7}>
              <box orientation={Gtk.Orientation.HORIZONTAL}>
                <label class="metric-label" hexpand halign={Gtk.Align.START} label="GPU load" />
                <label class="metric-value" label={gpu((data) => reading(data?.utilization ?? null, "%"))} />
              </box>
              <levelbar class="gpu-meter" minValue={0} maxValue={1} value={gpu((data) => fraction(data?.utilization ?? null))} />
            </box>
            <box orientation={Gtk.Orientation.VERTICAL} spacing={7}>
              <box orientation={Gtk.Orientation.HORIZONTAL}>
                <label class="metric-label" hexpand halign={Gtk.Align.START} label="VRAM" />
                <label class="metric-value" label={gpu((data) => `${reading(data?.memoryUsed ?? null, " MiB")} / ${reading(data?.memoryTotal ?? null, " MiB")}`)} />
              </box>
              <levelbar class="vram-meter" minValue={0} maxValue={1} value={gpu((data) => fraction(data?.memoryUsed ?? null, data?.memoryTotal ?? 0))} />
            </box>
          </box>

          <box class="stats" orientation={Gtk.Orientation.HORIZONTAL} spacing={10}>
            <box class="stat" orientation={Gtk.Orientation.VERTICAL} hexpand>
              <label class="stat-label" halign={Gtk.Align.START} label="TEMP" />
              <label class="stat-value" halign={Gtk.Align.START} label={gpu((data) => reading(data?.temperature ?? null, "°C"))} />
            </box>
            <box class="stat" orientation={Gtk.Orientation.VERTICAL} hexpand>
              <label class="stat-label" halign={Gtk.Align.START} label="POWER" />
              <label class="stat-value" halign={Gtk.Align.START} label={gpu((data) => `${reading(data?.powerDraw ?? null, " W", 1)} / ${reading(data?.powerLimit ?? null, " W", 0)}`)} />
            </box>
          </box>
          <box class="stats" orientation={Gtk.Orientation.HORIZONTAL} spacing={10}>
            <box class="stat" orientation={Gtk.Orientation.VERTICAL} hexpand>
              <label class="stat-label" halign={Gtk.Align.START} label="FAN" />
              <label class="stat-value" halign={Gtk.Align.START} label={gpu((data) => reading(data?.fanSpeed ?? null, "%"))} />
            </box>
            <box class="stat" orientation={Gtk.Orientation.VERTICAL} hexpand>
              <label class="stat-label" halign={Gtk.Align.START} label="GRAPHICS CLOCK" />
              <label class="stat-value" halign={Gtk.Align.START} label={gpu((data) => reading(data?.graphicsClock ?? null, " MHz"))} />
            </box>
          </box>
          <label class="footer" halign={Gtk.Align.START} label="LIVE TELEMETRY  ·  2s REFRESH" />
        </box>
      </window>
    )

    return window
  },
})
