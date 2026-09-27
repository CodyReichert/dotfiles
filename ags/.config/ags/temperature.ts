export const TEMPERATURE_QUERY = ["sensors", "-j"]

export type TemperatureReading = {
  key: string
  label: string
  value: number | null
}

export type TemperatureSnapshot = {
  cpuPackage: number | null
  readings: TemperatureReading[]
}

type SensorsJson = Record<string, Record<string, unknown>>

function chip(data: SensorsJson, prefix: string): Record<string, unknown> | undefined {
  const name = Object.keys(data).find((key) => key.startsWith(prefix))
  return name ? data[name] : undefined
}

function channelValue(device: Record<string, unknown> | undefined, channel: string): number | null {
  const values = device?.[channel]
  if (!values || typeof values !== "object") return null
  const input = Object.entries(values as Record<string, unknown>)
    .find(([key, value]) => key.endsWith("_input") && typeof value === "number")?.[1]
  return typeof input === "number" && Number.isFinite(input) ? input : null
}

export function parseTemperatureSnapshot(output: string): TemperatureSnapshot {
  const parsed = JSON.parse(output) as unknown
  if (!parsed || typeof parsed !== "object" || Array.isArray(parsed)) {
    throw new Error("Unexpected sensors data")
  }

  const data = parsed as SensorsJson
  const cpu = chip(data, "k10temp-")
  const board = chip(data, "asusec-")
  const igpu = chip(data, "amdgpu-")
  const systemNvme = chip(data, "nvme-pci-0500")
  const dataNvme = chip(data, "nvme-pci-0200")
  const memory = Object.keys(data)
    .filter((key) => key.startsWith("spd5118-"))
    .sort()
    .map((key) => data[key])

  return {
    cpuPackage: channelValue(cpu, "Tctl"),
    readings: [
      { key: "ccd1", label: "CCD 1", value: channelValue(cpu, "Tccd1") },
      { key: "ccd2", label: "CCD 2", value: channelValue(cpu, "Tccd2") },
      { key: "motherboard", label: "Motherboard", value: channelValue(board, "Motherboard") },
      { key: "vrm", label: "VRM", value: channelValue(board, "VRM") },
      { key: "system-nvme", label: "System NVMe", value: channelValue(systemNvme, "Composite") },
      { key: "data-nvme", label: "DataStorage NVMe", value: channelValue(dataNvme, "Composite") },
      { key: "memory-a", label: "Memory A", value: channelValue(memory[0], "temp1") },
      { key: "memory-b", label: "Memory B", value: channelValue(memory[1], "temp1") },
      { key: "igpu", label: "Integrated GPU", value: channelValue(igpu, "edge") },
    ],
  }
}

export async function loadTemperatureSnapshot(
  run: (command: string[]) => Promise<string>,
): Promise<TemperatureSnapshot> {
  return parseTemperatureSnapshot(await run([...TEMPERATURE_QUERY]))
}

export function temperatureText(value: number | null): string {
  return value === null ? "N/A" : `${value.toFixed(1)}°C`
}

export function temperatureFraction(value: number | null): number {
  if (value === null) return 0
  return Math.max(0, Math.min(1, (value - 20) / 80))
}
