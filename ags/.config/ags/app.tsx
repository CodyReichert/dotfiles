import { createState } from "ags"
import { Astal, Gtk } from "ags/gtk4"
import app from "ags/gtk4/app"
import { execAsync } from "ags/process"
import Gdk from "gi://Gdk?version=4.0"
import GLib from "gi://GLib?version=2.0"
import css from "./style.css"
import {
  buildCpuSnapshot,
  CPU_INFO_QUERY,
  CPU_LOAD_QUERY,
  CPU_STAT_QUERY,
  cpuFraction,
  cpuText,
  parseCpuStat,
  type CpuSnapshot,
  type CpuStatSample,
} from "./cpu"
import {
  CONFIGURED_VOLUMES,
  diskFraction,
  formatBytes,
  loadDiskSnapshot,
  type DiskSnapshot,
  type VolumeConfig,
} from "./disk"
import { fraction, GPU_QUERY, parseGpuSnapshot, reading, type GpuSnapshot } from "./gpu"
import {
  buildMemorySnapshot,
  MEMORY_INFO_QUERY,
  MEMORY_PRESSURE_QUERY,
  MEMORY_PROCESS_QUERY,
  memoryFraction,
  type MemorySnapshot,
} from "./memory"
import {
  loadTemperatureSnapshot,
  temperatureFraction,
  temperatureText,
  type TemperatureSnapshot,
} from "./temperature"

const GPU_POLL_MS = 2000
const DISK_POLL_MS = 5000
const TEMPERATURE_POLL_MS = 1000
const TEMPERATURE_HISTORY_POINTS = 60
const CPU_POLL_MS = 1000
const CPU_HISTORY_POINTS = 60
const MEMORY_POLL_MS = 2000
const MEMORY_HISTORY_POINTS = 30
const CONNECTOR = "HDMI-A-1"
const POPUP_NAMES = ["gpu", "disk", "temperature", "cpu", "memory"] as const
type PopupName = (typeof POPUP_NAMES)[number]

let openedAt = 0
let outsideClosedAt = 0
let outsideClicksPaused = false

type Layer = { x: number; y: number; w: number; h: number; namespace: string }

function popupWindow(name: PopupName): Astal.Window | null {
  return app.get_window(name) as Astal.Window | null
}

function visiblePopup(): { name: PopupName; window: Astal.Window } | null {
  for (const name of POPUP_NAMES) {
    const window = popupWindow(name)
    if (window?.visible) return { name, window }
  }
  return null
}

function hidePopups(except?: PopupName) {
  for (const name of POPUP_NAMES) {
    if (name !== except) popupWindow(name)?.set_visible(false)
  }
}

function togglePopup(name: PopupName): string {
  const target = popupWindow(name)
  if (!target) return `${name} popup is unavailable`
  if (target.visible) {
    target.set_visible(false)
    return "closed"
  }
  if (Date.now() - outsideClosedAt < 500) return "already closed"
  hidePopups(name)
  target.set_visible(true)
  return "opened"
}

async function clickedOutsidePopup(name: PopupName): Promise<boolean> {
  const [positionText, layersText] = await Promise.all([
    execAsync(["hyprctl", "cursorpos", "-j"]),
    execAsync(["hyprctl", "layers", "-j"]),
  ])
  const position = JSON.parse(positionText) as { x: number; y: number }
  const outputs = JSON.parse(layersText) as Record<string, { levels: Record<string, Layer[]> }>
  const popupLayer = Object.values(outputs)
    .flatMap((output) => Object.values(output.levels).flat())
    .find((layer) => layer.namespace === `${name}-popup`)
  if (!popupLayer) throw new Error(`${name} popup layer was not found`)

  return position.x < popupLayer.x || position.x >= popupLayer.x + popupLayer.w ||
    position.y < popupLayer.y || position.y >= popupLayer.y + popupLayer.h
}

function popupMonitor(): Gdk.Monitor | null {
  const monitors = Gdk.Display.get_default()?.get_monitors()
  if (!monitors) return null
  for (let index = 0; index < monitors.get_n_items(); index++) {
    const monitor = monitors.get_item(index) as Gdk.Monitor
    if (monitor.get_connector() === CONNECTOR) return monitor
  }
  console.warn(`Desktop popups: ${CONNECTOR} is unavailable; using the first monitor`)
  return monitors.get_item(0) as Gdk.Monitor | null
}

function closeOnEscape() {
  return (
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
  )
}

function GpuWindow() {
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
      pollSource = GLib.timeout_add(GLib.PRIORITY_DEFAULT, GPU_POLL_MS, () => {
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

  return (
    <window
      name="gpu"
      application={app}
      namespace="gpu-popup"
      class="popup-window gpu-window"
      visible={false}
      gdkmonitor={popupMonitor()}
      anchor={Astal.WindowAnchor.BOTTOM | Astal.WindowAnchor.RIGHT}
      exclusivity={Astal.Exclusivity.NORMAL}
      layer={Astal.Layer.TOP}
      keymode={Astal.Keymode.ON_DEMAND}
      marginBottom={44}
      marginRight={12}
      onNotifyVisible={onVisible}
    >
      {closeOnEscape()}
      <box class="panel gpu-panel" orientation={Gtk.Orientation.VERTICAL} spacing={18}>
        <box class="header" orientation={Gtk.Orientation.HORIZONTAL} spacing={12}>
          <box orientation={Gtk.Orientation.VERTICAL} hexpand>
            <label class="eyebrow gpu-accent" halign={Gtk.Align.START} label="GRAPHICS / GPU 0" />
            <label class="popup-title" halign={Gtk.Align.START} label={gpu((data) => data?.name ?? "NVIDIA GPU")} />
          </box>
          <button class="close-button" tooltipText="Close GPU details" $={(self) => { closeButton = self }} onClicked={() => popupWindow("gpu")?.set_visible(false)}>
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
}

function DiskWindow() {
  const [disks, setDisks] = createState<DiskSnapshot[]>([])
  const [error, setError] = createState("")
  let pollSource = 0
  let refreshPending = false
  let generation = 0
  let closeButton: Gtk.Button | null = null

  function diskValue<T>(mountPoint: string, transform: (disk: DiskSnapshot) => T) {
    return disks((items) => {
      const config = CONFIGURED_VOLUMES.find((item) => item.mountPoint === mountPoint)
      const disk = items.find((item) => item.mountPoint === mountPoint) ?? {
        name: config?.name ?? mountPoint,
        mountPoint,
        mounted: false,
        source: null,
        filesystem: null,
        size: null,
        used: null,
        available: null,
        usePercent: null,
      }
      return transform(disk)
    })
  }

  function diskCard(config: VolumeConfig) {
    const value = <T,>(transform: (disk: DiskSnapshot) => T) => diskValue(config.mountPoint, transform)
    return (
      <box class="disk-card" orientation={Gtk.Orientation.VERTICAL} spacing={10}>
        <box orientation={Gtk.Orientation.HORIZONTAL}>
          <box orientation={Gtk.Orientation.VERTICAL} hexpand>
            <label class="disk-name" halign={Gtk.Align.START} label={config.name} />
            <label class="disk-mount" halign={Gtk.Align.START} label={value((disk) => disk.mounted ? `${disk.mountPoint}  ·  ${disk.filesystem ?? "unknown"}` : `${disk.mountPoint}  ·  not mounted`)} />
          </box>
          <label class="disk-percent" valign={Gtk.Align.CENTER} label={value((disk) => disk.usePercent === null ? "N/A" : `${disk.usePercent.toFixed(0)}%`)} />
        </box>

        <levelbar class="disk-meter" minValue={0} maxValue={1} value={value(diskFraction)} />

        <box class="disk-values" orientation={Gtk.Orientation.HORIZONTAL} spacing={10}>
          <box orientation={Gtk.Orientation.VERTICAL} hexpand>
            <label class="stat-label" halign={Gtk.Align.START} label="USED / TOTAL" />
            <label class="disk-value" halign={Gtk.Align.START} label={value((disk) => `${formatBytes(disk.used)} / ${formatBytes(disk.size)}`)} />
          </box>
          <box orientation={Gtk.Orientation.VERTICAL} hexpand>
            <label class="stat-label" halign={Gtk.Align.START} label="AVAILABLE" />
            <label class="disk-value" halign={Gtk.Align.START} label={value((disk) => formatBytes(disk.available))} />
          </box>
        </box>

        <label class="disk-source" halign={Gtk.Align.START} ellipsize={3} label={value((disk) => disk.source ?? "Device unavailable")} />
      </box>
    )
  }

  async function refresh() {
    if (refreshPending) return
    refreshPending = true
    const currentGeneration = generation
    try {
      const snapshot = await loadDiskSnapshot((command) => execAsync(command))
      if (currentGeneration === generation) {
        setDisks(snapshot)
        setError("")
      }
    } catch (cause) {
      if (currentGeneration === generation) {
        setDisks([])
        setError("Unable to read filesystem usage from findmnt")
        console.error(`Disk popup: ${cause}`)
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
      pollSource = GLib.timeout_add(GLib.PRIORITY_DEFAULT, DISK_POLL_MS, () => {
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

  return (
    <window
      name="disk"
      application={app}
      namespace="disk-popup"
      class="popup-window disk-window"
      visible={false}
      gdkmonitor={popupMonitor()}
      anchor={Astal.WindowAnchor.BOTTOM | Astal.WindowAnchor.RIGHT}
      exclusivity={Astal.Exclusivity.NORMAL}
      layer={Astal.Layer.TOP}
      keymode={Astal.Keymode.ON_DEMAND}
      marginBottom={44}
      marginRight={12}
      onNotifyVisible={onVisible}
    >
      {closeOnEscape()}
      <box class="panel disk-panel" orientation={Gtk.Orientation.VERTICAL} spacing={16}>
        <box class="header" orientation={Gtk.Orientation.HORIZONTAL} spacing={12}>
          <box orientation={Gtk.Orientation.VERTICAL} hexpand>
            <label class="eyebrow disk-accent" halign={Gtk.Align.START} label="STORAGE / FILESYSTEMS" />
            <label class="popup-title" halign={Gtk.Align.START} label="Disk usage" />
          </box>
          <button class="close-button disk-close-button" tooltipText="Close disk details" $={(self) => { closeButton = self }} onClicked={() => popupWindow("disk")?.set_visible(false)}>
            <label label="×" />
          </button>
        </box>

        <box class="error" visible={error((message) => Boolean(message))}>
          <label wrap label={error} />
        </box>

        <box class="disk-list" orientation={Gtk.Orientation.VERTICAL} spacing={10}>
          {diskCard(CONFIGURED_VOLUMES[0])}
          {diskCard(CONFIGURED_VOLUMES[1])}
          {diskCard(CONFIGURED_VOLUMES[2])}
        </box>
        <label class="footer" halign={Gtk.Align.START} label="FILESYSTEM USAGE  ·  5s REFRESH" />
      </box>
    </window>
  )
}

function drawTemperatureChart(context: any, width: number, height: number, values: number[]) {
  const left = 12
  const right = 10
  const top = 10
  const bottom = 12
  const plotWidth = width - left - right
  const plotHeight = height - top - bottom
  const graphBottom = top + plotHeight
  const pointX = (index: number) =>
    left + ((TEMPERATURE_HISTORY_POINTS - values.length + index) / (TEMPERATURE_HISTORY_POINTS - 1)) * plotWidth
  const pointY = (value: number) =>
    top + ((100 - Math.max(20, Math.min(100, value))) / 80) * plotHeight

  context.setSourceRGBA(0.09, 0.09, 0.14, 1)
  context.rectangle(0, 0, width, height)
  context.fill()

  context.setLineWidth(1)
  for (const temperature of [40, 60, 80]) {
    const y = pointY(temperature)
    context.setSourceRGBA(0.45, 0.46, 0.54, 0.22)
    context.moveTo(left, y)
    context.lineTo(width - right, y)
    context.stroke()
  }

  if (values.length === 0) return

  context.moveTo(pointX(0), graphBottom)
  values.forEach((value, index) => context.lineTo(pointX(index), pointY(value)))
  context.lineTo(pointX(values.length - 1), graphBottom)
  context.closePath()
  context.setSourceRGBA(0.43, 0.50, 0, 0.18)
  context.fill()

  context.setLineWidth(2.25)
  context.setLineJoin(1)
  context.setLineCap(1)
  values.forEach((value, index) => {
    const x = pointX(index)
    const y = pointY(value)
    if (index === 0) context.moveTo(x, y)
    else context.lineTo(x, y)
  })
  context.setSourceRGBA(0.72, 0.80, 0.12, 1)
  context.stroke()

  if (values.length === 1) {
    context.arc(pointX(0), pointY(values[0]), 3, 0, Math.PI * 2)
    context.fill()
  }
}

function TemperatureWindow() {
  const [temperature, setTemperature] = createState<TemperatureSnapshot | null>(null)
  const [history, setHistory] = createState<number[]>([])
  const [error, setError] = createState("")
  let historyValues: number[] = []
  let chart: Gtk.DrawingArea | null = null
  let pollSource = 0
  let refreshPending = false
  let generation = 0
  let closeButton: Gtk.Button | null = null

  function sensorValue<T>(key: string, transform: (value: number | null) => T) {
    return temperature((snapshot) =>
      transform(snapshot?.readings.find((reading) => reading.key === key)?.value ?? null))
  }

  function sensorRow(key: string, label: string) {
    return (
      <box class="sensor-row" orientation={Gtk.Orientation.HORIZONTAL} spacing={10}>
        <label class="sensor-name" halign={Gtk.Align.START} label={label} />
        <levelbar class="temperature-meter" hexpand minValue={0} maxValue={1} value={sensorValue(key, temperatureFraction)} />
        <label class="sensor-value" halign={Gtk.Align.END} label={sensorValue(key, temperatureText)} />
      </box>
    )
  }

  async function refresh() {
    if (refreshPending) return
    refreshPending = true
    const currentGeneration = generation
    try {
      const snapshot = await loadTemperatureSnapshot((command) => execAsync(command))
      if (currentGeneration === generation) {
        setTemperature(snapshot)
        setError("")
        if (snapshot.cpuPackage !== null) {
          historyValues = [...historyValues, snapshot.cpuPackage].slice(-TEMPERATURE_HISTORY_POINTS)
          setHistory(historyValues)
          chart?.queue_draw()
        }
      }
    } catch (cause) {
      if (currentGeneration === generation) {
        setTemperature(null)
        setError("Unable to read temperature sensors")
        console.error(`Temperature popup: ${cause}`)
      }
    } finally {
      refreshPending = false
    }
  }

  function onVisible(window: Astal.Window) {
    if (window.visible) {
      openedAt = Date.now()
      historyValues = []
      setHistory([])
      setError("")
      chart?.queue_draw()
      void refresh()
      pollSource = GLib.timeout_add(GLib.PRIORITY_DEFAULT, TEMPERATURE_POLL_MS, () => {
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

  return (
    <window
      name="temperature"
      application={app}
      namespace="temperature-popup"
      class="popup-window temperature-window"
      visible={false}
      gdkmonitor={popupMonitor()}
      anchor={Astal.WindowAnchor.BOTTOM | Astal.WindowAnchor.RIGHT}
      exclusivity={Astal.Exclusivity.NORMAL}
      layer={Astal.Layer.TOP}
      keymode={Astal.Keymode.ON_DEMAND}
      marginBottom={44}
      marginRight={12}
      onNotifyVisible={onVisible}
    >
      {closeOnEscape()}
      <box class="panel temperature-panel" orientation={Gtk.Orientation.VERTICAL} spacing={15}>
        <box class="header" orientation={Gtk.Orientation.HORIZONTAL} spacing={12}>
          <box orientation={Gtk.Orientation.VERTICAL} hexpand>
            <label class="eyebrow temperature-accent" halign={Gtk.Align.START} label="THERMALS / CPU PACKAGE" />
            <label class="popup-title" halign={Gtk.Align.START} label="Temperature monitor" />
          </box>
          <label class="temperature-current" valign={Gtk.Align.CENTER} label={temperature((snapshot) => temperatureText(snapshot?.cpuPackage ?? null))} />
          <button class="close-button temperature-close-button" tooltipText="Close temperature details" $={(self) => { closeButton = self }} onClicked={() => popupWindow("temperature")?.set_visible(false)}>
            <label label="×" />
          </button>
        </box>

        <box class="error" visible={error((message) => Boolean(message))}>
          <label wrap label={error} />
        </box>

        <box class="chart-card" orientation={Gtk.Orientation.VERTICAL} spacing={8}>
          <box orientation={Gtk.Orientation.HORIZONTAL}>
            <label class="stat-label" hexpand halign={Gtk.Align.START} label="CPU PACKAGE · LAST 60 SECONDS" />
            <label class="chart-range" label={history((values) => {
              if (values.length === 0) return "MIN N/A  ·  MAX N/A"
              return `MIN ${Math.min(...values).toFixed(1)}°  ·  MAX ${Math.max(...values).toFixed(1)}°`
            })} />
          </box>
          <drawingarea
            class="temperature-chart"
            contentWidth={390}
            contentHeight={118}
            $={(self) => {
              chart = self
              self.set_draw_func((_area, context, width, height) =>
                drawTemperatureChart(context, width, height, historyValues))
            }}
          />
          <box orientation={Gtk.Orientation.HORIZONTAL}>
            <label class="chart-scale" hexpand halign={Gtk.Align.START} label="20°C" />
            <label class="chart-scale" label="60°C" />
            <label class="chart-scale" hexpand halign={Gtk.Align.END} label="100°C" />
          </box>
        </box>

        <box class="sensor-list" orientation={Gtk.Orientation.VERTICAL} spacing={7}>
          {sensorRow("ccd1", "CCD 1")}
          {sensorRow("ccd2", "CCD 2")}
          {sensorRow("motherboard", "Motherboard")}
          {sensorRow("vrm", "VRM")}
          {sensorRow("system-nvme", "System NVMe")}
          {sensorRow("data-nvme", "DataStorage NVMe")}
          {sensorRow("memory-a", "Memory A")}
          {sensorRow("memory-b", "Memory B")}
          {sensorRow("igpu", "Integrated GPU")}
        </box>
        <label class="footer" halign={Gtk.Align.START} label="LIVE SENSORS  ·  1s REFRESH  ·  65°C WARN / 90°C CRITICAL" />
      </box>
    </window>
  )
}

function drawCpuChart(context: any, width: number, height: number, values: number[]) {
  const left = 12
  const right = 10
  const top = 10
  const bottom = 12
  const plotWidth = width - left - right
  const plotHeight = height - top - bottom
  const graphBottom = top + plotHeight
  const pointX = (index: number) =>
    left + ((CPU_HISTORY_POINTS - values.length + index) / (CPU_HISTORY_POINTS - 1)) * plotWidth
  const pointY = (value: number) =>
    top + ((100 - Math.max(0, Math.min(100, value))) / 100) * plotHeight

  context.setSourceRGBA(0.09, 0.09, 0.14, 1)
  context.rectangle(0, 0, width, height)
  context.fill()

  context.setLineWidth(1)
  for (const utilization of [25, 50, 75]) {
    const y = pointY(utilization)
    context.setSourceRGBA(0.45, 0.46, 0.54, 0.22)
    context.moveTo(left, y)
    context.lineTo(width - right, y)
    context.stroke()
  }

  if (values.length === 0) return

  context.moveTo(pointX(0), graphBottom)
  values.forEach((value, index) => context.lineTo(pointX(index), pointY(value)))
  context.lineTo(pointX(values.length - 1), graphBottom)
  context.closePath()
  context.setSourceRGBA(0.45, 0.42, 0.89, 0.2)
  context.fill()

  context.setLineWidth(2.25)
  context.setLineJoin(1)
  context.setLineCap(1)
  values.forEach((value, index) => {
    const x = pointX(index)
    const y = pointY(value)
    if (index === 0) context.moveTo(x, y)
    else context.lineTo(x, y)
  })
  context.setSourceRGBA(0.60, 0.57, 1, 1)
  context.stroke()

  if (values.length === 1) {
    context.arc(pointX(0), pointY(values[0]), 3, 0, Math.PI * 2)
    context.fill()
  }
}

function CpuWindow() {
  const [cpu, setCpu] = createState<CpuSnapshot | null>(null)
  const [history, setHistory] = createState<number[]>([])
  const [error, setError] = createState("")
  let previousStat: CpuStatSample | null = null
  let historyValues: number[] = []
  let chart: Gtk.DrawingArea | null = null
  let pollSource = 0
  let refreshPending = false
  let generation = 0
  let closeButton: Gtk.Button | null = null

  function coreValue<T>(index: number, transform: (value: number | null) => T) {
    return cpu((snapshot) => transform(snapshot?.cores[index] ?? null))
  }

  function coreTile(index: number) {
    return (
      <box class="core-tile" orientation={Gtk.Orientation.VERTICAL} spacing={4} hexpand>
        <label class="core-name" label={`C${String(index + 1).padStart(2, "0")}`} />
        <levelbar
          class="core-meter"
          orientation={Gtk.Orientation.VERTICAL}
          inverted
          minValue={0}
          maxValue={1}
          value={coreValue(index, cpuFraction)}
        />
        <label class="core-value" label={coreValue(index, cpuText)} />
      </box>
    )
  }

  async function refresh() {
    if (refreshPending) return
    refreshPending = true
    const currentGeneration = generation
    try {
      const [statOutput, loadOutput, infoOutput] = await Promise.all([
        execAsync(CPU_STAT_QUERY),
        execAsync(CPU_LOAD_QUERY),
        execAsync(CPU_INFO_QUERY),
      ])
      const currentStat = parseCpuStat(statOutput)
      const snapshot = buildCpuSnapshot(previousStat, currentStat, loadOutput, infoOutput)
      if (currentGeneration === generation) {
        previousStat = currentStat
        setCpu(snapshot)
        setError("")
        if (snapshot.overall !== null) {
          historyValues = [...historyValues, snapshot.overall].slice(-CPU_HISTORY_POINTS)
          setHistory(historyValues)
          chart?.queue_draw()
        }
      }
    } catch (cause) {
      if (currentGeneration === generation) {
        setCpu(null)
        setError("Unable to read CPU utilization")
        console.error(`CPU popup: ${cause}`)
      }
    } finally {
      refreshPending = false
    }
  }

  function onVisible(window: Astal.Window) {
    if (window.visible) {
      openedAt = Date.now()
      previousStat = null
      historyValues = []
      setHistory([])
      setError("")
      chart?.queue_draw()
      void refresh()
      pollSource = GLib.timeout_add(GLib.PRIORITY_DEFAULT, CPU_POLL_MS, () => {
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

  return (
    <window
      name="cpu"
      application={app}
      namespace="cpu-popup"
      class="popup-window cpu-window"
      visible={false}
      gdkmonitor={popupMonitor()}
      anchor={Astal.WindowAnchor.BOTTOM | Astal.WindowAnchor.RIGHT}
      exclusivity={Astal.Exclusivity.NORMAL}
      layer={Astal.Layer.TOP}
      keymode={Astal.Keymode.ON_DEMAND}
      marginBottom={44}
      marginRight={12}
      onNotifyVisible={onVisible}
    >
      {closeOnEscape()}
      <box class="panel cpu-panel" orientation={Gtk.Orientation.VERTICAL} spacing={15}>
        <box class="header" orientation={Gtk.Orientation.HORIZONTAL} spacing={12}>
          <box orientation={Gtk.Orientation.VERTICAL} hexpand>
            <label class="eyebrow cpu-accent" halign={Gtk.Align.START} label="PROCESSOR / 12 CORES · 24 THREADS" />
            <label class="popup-title" halign={Gtk.Align.START} label={cpu((snapshot) =>
              snapshot?.model.replace(/^AMD\s+/, "").replace(/\s+\d+-Core Processor$/, "") ?? "CPU usage")} />
          </box>
          <label class="cpu-current" valign={Gtk.Align.CENTER} label={cpu((snapshot) => cpuText(snapshot?.overall ?? null))} />
          <button class="close-button cpu-close-button" tooltipText="Close CPU details" $={(self) => { closeButton = self }} onClicked={() => popupWindow("cpu")?.set_visible(false)}>
            <label label="×" />
          </button>
        </box>

        <box class="error" visible={error((message) => Boolean(message))}>
          <label wrap label={error} />
        </box>

        <box class="chart-card cpu-chart-card" orientation={Gtk.Orientation.VERTICAL} spacing={8}>
          <box orientation={Gtk.Orientation.HORIZONTAL}>
            <label class="stat-label" hexpand halign={Gtk.Align.START} label="TOTAL UTILIZATION · LAST 60 SECONDS" />
            <label class="chart-range" label={history((values) => {
              if (values.length === 0) return "AVG N/A  ·  MAX N/A"
              const average = values.reduce((sum, value) => sum + value, 0) / values.length
              return `AVG ${average.toFixed(0)}%  ·  MAX ${Math.max(...values).toFixed(0)}%`
            })} />
          </box>
          <drawingarea
            class="cpu-chart"
            contentWidth={390}
            contentHeight={112}
            $={(self) => {
              chart = self
              self.set_draw_func((_area, context, width, height) =>
                drawCpuChart(context, width, height, historyValues))
            }}
          />
          <box orientation={Gtk.Orientation.HORIZONTAL}>
            <label class="chart-scale" hexpand halign={Gtk.Align.START} label="0%" />
            <label class="chart-scale" label="50%" />
            <label class="chart-scale" hexpand halign={Gtk.Align.END} label="100%" />
          </box>
        </box>

        <box class="core-card" orientation={Gtk.Orientation.VERTICAL} spacing={9}>
          <box orientation={Gtk.Orientation.HORIZONTAL}>
            <label class="stat-label" hexpand halign={Gtk.Align.START} label="PHYSICAL CORE ACTIVITY" />
            <label class="chart-range" label="SMT THREADS COMBINED" />
          </box>
          <box class="core-row" orientation={Gtk.Orientation.HORIZONTAL} spacing={7}>
            {coreTile(0)}{coreTile(1)}{coreTile(2)}{coreTile(3)}{coreTile(4)}{coreTile(5)}
          </box>
          <box class="core-row" orientation={Gtk.Orientation.HORIZONTAL} spacing={7}>
            {coreTile(6)}{coreTile(7)}{coreTile(8)}{coreTile(9)}{coreTile(10)}{coreTile(11)}
          </box>
        </box>

        <box class="cpu-stats" orientation={Gtk.Orientation.HORIZONTAL} spacing={9}>
          <box class="cpu-stat" orientation={Gtk.Orientation.VERTICAL} hexpand>
            <label class="stat-label" halign={Gtk.Align.START} label="LOAD · 1 / 5 / 15 MIN" />
            <label class="cpu-stat-value" halign={Gtk.Align.START} label={cpu((snapshot) =>
              snapshot ? snapshot.load.map((value) => value.toFixed(2)).join("  /  ") : "N/A")} />
          </box>
          <box class="cpu-stat" orientation={Gtk.Orientation.VERTICAL}>
            <label class="stat-label" halign={Gtk.Align.START} label="AVG CLOCK" />
            <label class="cpu-stat-value" halign={Gtk.Align.START} label={cpu((snapshot) =>
              snapshot?.averageFrequencyGHz === null || snapshot?.averageFrequencyGHz === undefined
                ? "N/A"
                : `${snapshot.averageFrequencyGHz.toFixed(2)} GHz`)} />
          </box>
          <box class="cpu-stat" orientation={Gtk.Orientation.VERTICAL}>
            <label class="stat-label" halign={Gtk.Align.START} label="TASKS" />
            <label class="cpu-stat-value" halign={Gtk.Align.START} label={cpu((snapshot) =>
              snapshot ? `${snapshot.runningTasks} / ${snapshot.totalTasks}` : "N/A")} />
          </box>
        </box>
        <label class="footer" halign={Gtk.Align.START} label="LIVE CPU UTILIZATION  ·  1s REFRESH" />
      </box>
    </window>
  )
}

function drawMemoryChart(context: any, width: number, height: number, values: number[]) {
  const left = 12
  const right = 10
  const top = 10
  const bottom = 12
  const plotWidth = width - left - right
  const plotHeight = height - top - bottom
  const graphBottom = top + plotHeight
  const pointX = (index: number) =>
    left + ((MEMORY_HISTORY_POINTS - values.length + index) / (MEMORY_HISTORY_POINTS - 1)) * plotWidth
  const pointY = (value: number) =>
    top + ((100 - Math.max(0, Math.min(100, value))) / 100) * plotHeight

  context.setSourceRGBA(0.09, 0.09, 0.14, 1)
  context.rectangle(0, 0, width, height)
  context.fill()

  context.setLineWidth(1)
  for (const utilization of [25, 50, 75]) {
    const y = pointY(utilization)
    context.setSourceRGBA(0.45, 0.46, 0.54, 0.22)
    context.moveTo(left, y)
    context.lineTo(width - right, y)
    context.stroke()
  }

  if (values.length === 0) return

  context.moveTo(pointX(0), graphBottom)
  values.forEach((value, index) => context.lineTo(pointX(index), pointY(value)))
  context.lineTo(pointX(values.length - 1), graphBottom)
  context.closePath()
  context.setSourceRGBA(0.13, 0.52, 0.89, 0.2)
  context.fill()

  context.setLineWidth(2.25)
  context.setLineJoin(1)
  context.setLineCap(1)
  values.forEach((value, index) => {
    const x = pointX(index)
    const y = pointY(value)
    if (index === 0) context.moveTo(x, y)
    else context.lineTo(x, y)
  })
  context.setSourceRGBA(0.28, 0.65, 1, 1)
  context.stroke()

  if (values.length === 1) {
    context.arc(pointX(0), pointY(values[0]), 3, 0, Math.PI * 2)
    context.fill()
  }
}

function drawMemoryComposition(context: any, width: number, height: number, memory: MemorySnapshot | null) {
  context.setSourceRGBA(0.16, 0.16, 0.22, 1)
  context.rectangle(0, 0, width, height)
  context.fill()
  if (!memory || memory.total <= 0) return

  const segments = [
    { value: memory.compositionUsed, color: [0.13, 0.52, 0.89] },
    { value: memory.cache + memory.buffers, color: [0.0, 0.61, 0.55] },
    { value: memory.free, color: [0.36, 0.37, 0.43] },
  ]
  let x = 0
  for (const segment of segments) {
    const segmentWidth = width * memoryFraction(segment.value, memory.total)
    context.setSourceRGBA(segment.color[0], segment.color[1], segment.color[2], 1)
    context.rectangle(x, 0, segmentWidth, height)
    context.fill()
    x += segmentWidth
  }
}

function MemoryWindow() {
  const [memory, setMemory] = createState<MemorySnapshot | null>(null)
  const [history, setHistory] = createState<number[]>([])
  const [error, setError] = createState("")
  let latestSnapshot: MemorySnapshot | null = null
  let historyValues: number[] = []
  let chart: Gtk.DrawingArea | null = null
  let composition: Gtk.DrawingArea | null = null
  let pollSource = 0
  let refreshPending = false
  let generation = 0
  let closeButton: Gtk.Button | null = null

  function processValue<T>(index: number, transform: (name: string | null, bytes: number | null) => T) {
    return memory((snapshot) => {
      const process = snapshot?.processes[index]
      return transform(process?.name ?? null, process?.residentBytes ?? null)
    })
  }

  function processRow(index: number) {
    return (
      <box class="memory-process-row" orientation={Gtk.Orientation.HORIZONTAL} spacing={10}>
        <label class="memory-process-rank" label={String(index + 1).padStart(2, "0")} />
        <label class="memory-process-name" hexpand halign={Gtk.Align.START} ellipsize={3}
          label={processValue(index, (name) => name ?? "—")} />
        <label class="memory-process-value" halign={Gtk.Align.END}
          label={processValue(index, (_name, bytes) => formatBytes(bytes))} />
      </box>
    )
  }

  async function refresh() {
    if (refreshPending) return
    refreshPending = true
    const currentGeneration = generation
    try {
      const [infoOutput, pressureOutput, processOutput] = await Promise.all([
        execAsync(MEMORY_INFO_QUERY),
        execAsync(MEMORY_PRESSURE_QUERY),
        execAsync(MEMORY_PROCESS_QUERY),
      ])
      const snapshot = buildMemorySnapshot(infoOutput, pressureOutput, processOutput)
      if (currentGeneration === generation) {
        latestSnapshot = snapshot
        setMemory(snapshot)
        setError("")
        historyValues = [...historyValues, snapshot.usedPercent].slice(-MEMORY_HISTORY_POINTS)
        setHistory(historyValues)
        chart?.queue_draw()
        composition?.queue_draw()
      }
    } catch (cause) {
      if (currentGeneration === generation) {
        latestSnapshot = null
        setMemory(null)
        setError("Unable to read memory utilization")
        chart?.queue_draw()
        composition?.queue_draw()
        console.error(`Memory popup: ${cause}`)
      }
    } finally {
      refreshPending = false
    }
  }

  function onVisible(window: Astal.Window) {
    if (window.visible) {
      openedAt = Date.now()
      historyValues = []
      setHistory([])
      setError("")
      chart?.queue_draw()
      void refresh()
      pollSource = GLib.timeout_add(GLib.PRIORITY_DEFAULT, MEMORY_POLL_MS, () => {
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

  return (
    <window
      name="memory"
      application={app}
      namespace="memory-popup"
      class="popup-window memory-window"
      visible={false}
      gdkmonitor={popupMonitor()}
      anchor={Astal.WindowAnchor.BOTTOM | Astal.WindowAnchor.RIGHT}
      exclusivity={Astal.Exclusivity.NORMAL}
      layer={Astal.Layer.TOP}
      keymode={Astal.Keymode.ON_DEMAND}
      marginBottom={44}
      marginRight={12}
      onNotifyVisible={onVisible}
    >
      {closeOnEscape()}
      <box class="panel memory-panel" orientation={Gtk.Orientation.VERTICAL} spacing={15}>
        <box class="header" orientation={Gtk.Orientation.HORIZONTAL} spacing={12}>
          <box orientation={Gtk.Orientation.VERTICAL} hexpand>
            <label class="eyebrow memory-accent" halign={Gtk.Align.START} label="SYSTEM / PHYSICAL MEMORY" />
            <label class="popup-title" halign={Gtk.Align.START} label="Memory monitor" />
          </box>
          <label class="memory-current" valign={Gtk.Align.CENTER} label={memory((snapshot) =>
            snapshot ? `${snapshot.usedPercent.toFixed(0)}%` : "N/A")} />
          <button class="close-button memory-close-button" tooltipText="Close memory details" $={(self) => { closeButton = self }} onClicked={() => popupWindow("memory")?.set_visible(false)}>
            <label label="×" />
          </button>
        </box>

        <box class="error" visible={error((message) => Boolean(message))}>
          <label wrap label={error} />
        </box>

        <box class="chart-card memory-chart-card" orientation={Gtk.Orientation.VERTICAL} spacing={8}>
          <box orientation={Gtk.Orientation.HORIZONTAL}>
            <label class="stat-label" hexpand halign={Gtk.Align.START} label="MEMORY USED · LAST 60 SECONDS" />
            <label class="chart-range" label={history((values) => {
              if (values.length === 0) return "AVG N/A  ·  MAX N/A"
              const average = values.reduce((sum, value) => sum + value, 0) / values.length
              return `AVG ${average.toFixed(0)}%  ·  MAX ${Math.max(...values).toFixed(0)}%`
            })} />
          </box>
          <drawingarea
            class="memory-chart"
            contentWidth={390}
            contentHeight={105}
            $={(self) => {
              chart = self
              self.set_draw_func((_area, context, width, height) =>
                drawMemoryChart(context, width, height, historyValues))
            }}
          />
          <box orientation={Gtk.Orientation.HORIZONTAL}>
            <label class="chart-scale" hexpand halign={Gtk.Align.START} label="0%" />
            <label class="chart-scale" label="50%" />
            <label class="chart-scale" hexpand halign={Gtk.Align.END} label="100%" />
          </box>
        </box>

        <box class="memory-composition-card" orientation={Gtk.Orientation.VERTICAL} spacing={9}>
          <label class="stat-label" halign={Gtk.Align.START} label="PHYSICAL MEMORY COMPOSITION" />
          <drawingarea
            class="memory-composition"
            contentWidth={390}
            contentHeight={14}
            $={(self) => {
              composition = self
              self.set_draw_func((_area, context, width, height) =>
                drawMemoryComposition(context, width, height, latestSnapshot))
            }}
          />
          <box orientation={Gtk.Orientation.HORIZONTAL} spacing={12}>
            <label class="memory-legend memory-in-use" hexpand halign={Gtk.Align.START}
              label={memory((snapshot) => `● IN USE  ${formatBytes(snapshot?.compositionUsed ?? null)}`)} />
            <label class="memory-legend memory-cache" hexpand
              label={memory((snapshot) => `● CACHE  ${formatBytes(snapshot ? snapshot.cache + snapshot.buffers : null)}`)} />
            <label class="memory-legend memory-free" hexpand halign={Gtk.Align.END}
              label={memory((snapshot) => `● FREE  ${formatBytes(snapshot?.free ?? null)}`)} />
          </box>
        </box>

        <box class="memory-stats" orientation={Gtk.Orientation.HORIZONTAL} spacing={9}>
          <box class="memory-stat" orientation={Gtk.Orientation.VERTICAL} hexpand>
            <label class="stat-label" halign={Gtk.Align.START} label="USED / TOTAL" />
            <label class="memory-stat-value" halign={Gtk.Align.START} label={memory((snapshot) =>
              `${formatBytes(snapshot?.used ?? null)} / ${formatBytes(snapshot?.total ?? null)}`)} />
          </box>
          <box class="memory-stat" orientation={Gtk.Orientation.VERTICAL} hexpand>
            <label class="stat-label" halign={Gtk.Align.START} label="AVAILABLE" />
            <label class="memory-stat-value" halign={Gtk.Align.START}
              label={memory((snapshot) => formatBytes(snapshot?.available ?? null))} />
          </box>
          <box class="memory-stat" orientation={Gtk.Orientation.VERTICAL} hexpand>
            <label class="stat-label" halign={Gtk.Align.START} label="SWAP" />
            <label class="memory-stat-value" halign={Gtk.Align.START} label={memory((snapshot) =>
              !snapshot ? "N/A" : snapshot.swapTotal === 0
                ? "Disabled"
                : `${formatBytes(snapshot.swapUsed)} / ${formatBytes(snapshot.swapTotal)}`)} />
          </box>
        </box>

        <box class="memory-process-card" orientation={Gtk.Orientation.VERTICAL} spacing={6}>
          <box orientation={Gtk.Orientation.HORIZONTAL}>
            <label class="stat-label" hexpand halign={Gtk.Align.START} label="TOP PROCESS GROUPS · RSS" />
            <label class="memory-pressure" label={memory((snapshot) =>
              snapshot ? `PSI SOME / FULL · ${snapshot.pressureSome.toFixed(2)}% / ${snapshot.pressureFull.toFixed(2)}%` : "PSI N/A")} />
          </box>
          {processRow(0)}
          {processRow(1)}
          {processRow(2)}
          {processRow(3)}
        </box>
        <label class="footer" halign={Gtk.Align.START} label="LIVE MEMORY TELEMETRY  ·  2s REFRESH" />
      </box>
    </window>
  )
}

app.start({
  instanceName: "desktop-shell",
  css,
  requestHandler(argv, response) {
    if (argv[0] === "pause-outside-clicks") {
      outsideClicksPaused = true
      return response("paused")
    }

    if (argv[0] === "resume-outside-clicks") {
      outsideClicksPaused = false
      return response("resumed")
    }

    if (argv[0] === "toggle" && (argv[1] === "gpu" || argv[1] === "disk" || argv[1] === "temperature" || argv[1] === "cpu" || argv[1] === "memory")) {
      return response(togglePopup(argv[1]))
    }

    if (argv[0] === "outside-click") {
      const visible = visiblePopup()
      if (outsideClicksPaused || !visible || Date.now() - openedAt < 250) return response("ignored")
      void clickedOutsidePopup(visible.name).then((outside) => {
        if (outside && visible.window.visible) {
          outsideClosedAt = Date.now()
          visible.window.set_visible(false)
        }
        response(outside ? "outside" : "inside")
      }).catch((error) => {
        console.error(`Popup click check: ${error}`)
        response("click check failed")
      })
      return
    }

    response("unknown command")
  },
  main() {
    GpuWindow()
    DiskWindow()
    TemperatureWindow()
    CpuWindow()
    MemoryWindow()
  },
})
