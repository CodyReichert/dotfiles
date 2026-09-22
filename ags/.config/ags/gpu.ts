export const GPU_QUERY = [
  "nvidia-smi",
  "--id=0",
  "--query-gpu=name,utilization.gpu,temperature.gpu,memory.used,memory.total,power.draw,power.limit,fan.speed,clocks.gr",
  "--format=csv,noheader,nounits",
]

export type GpuSnapshot = {
  name: string
  utilization: number | null
  temperature: number | null
  memoryUsed: number | null
  memoryTotal: number | null
  powerDraw: number | null
  powerLimit: number | null
  fanSpeed: number | null
  graphicsClock: number | null
}

function optionalNumber(value: string): number | null {
  const trimmed = value.trim()
  if (!trimmed || trimmed === "N/A" || trimmed === "[N/A]") return null
  const number = Number(trimmed)
  return Number.isFinite(number) ? number : null
}

export function parseGpuSnapshot(output: string): GpuSnapshot {
  const line = output.trim().split("\n")[0]
  const fields = line?.split(",").map((field) => field.trim()) ?? []
  if (fields.length !== 9 || !fields[0]) {
    throw new Error("Unexpected nvidia-smi GPU data")
  }

  const [name, utilization, temperature, memoryUsed, memoryTotal, powerDraw, powerLimit, fanSpeed, graphicsClock] = fields
  return {
    name,
    utilization: optionalNumber(utilization),
    temperature: optionalNumber(temperature),
    memoryUsed: optionalNumber(memoryUsed),
    memoryTotal: optionalNumber(memoryTotal),
    powerDraw: optionalNumber(powerDraw),
    powerLimit: optionalNumber(powerLimit),
    fanSpeed: optionalNumber(fanSpeed),
    graphicsClock: optionalNumber(graphicsClock),
  }
}

export function fraction(value: number | null, total = 100): number {
  if (value === null || !Number.isFinite(total) || total <= 0) return 0
  return Math.max(0, Math.min(1, value / total))
}

export function reading(value: number | null, unit: string, digits = 0): string {
  return value === null ? "N/A" : `${value.toFixed(digits)}${unit}`
}
