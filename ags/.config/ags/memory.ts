export const MEMORY_INFO_QUERY = ["cat", "/proc/meminfo"]
export const MEMORY_PRESSURE_QUERY = ["cat", "/proc/pressure/memory"]
export const MEMORY_PROCESS_QUERY = ["ps", "-eo", "comm=,rss=", "--sort=-rss"]

export type MemoryProcess = {
  name: string
  residentBytes: number
}

export type MemorySnapshot = {
  total: number
  used: number
  available: number
  free: number
  cache: number
  buffers: number
  compositionUsed: number
  swapTotal: number
  swapUsed: number
  usedPercent: number
  pressureSome: number
  pressureFull: number
  processes: MemoryProcess[]
}

function parseKilobytes(output: string): Map<string, number> {
  const values = new Map<string, number>()
  for (const line of output.split("\n")) {
    const match = line.match(/^([^:]+):\s+(\d+)\s+kB$/)
    if (match) values.set(match[1], Number(match[2]) * 1024)
  }
  return values
}

function required(values: Map<string, number>, key: string): number {
  const value = values.get(key)
  if (value === undefined) throw new Error(`Missing ${key} in /proc/meminfo`)
  return value
}

export function parseMemoryInfo(output: string) {
  const values = parseKilobytes(output)
  const total = required(values, "MemTotal")
  const free = required(values, "MemFree")
  const available = required(values, "MemAvailable")
  const buffers = required(values, "Buffers")
  const cache = Math.max(0,
    required(values, "Cached") + required(values, "SReclaimable") - required(values, "Shmem"))
  const swapTotal = required(values, "SwapTotal")
  const swapFree = required(values, "SwapFree")
  const used = Math.max(0, total - available)
  return {
    total,
    used,
    available,
    free,
    cache,
    buffers,
    compositionUsed: Math.max(0, total - free - cache - buffers),
    swapTotal,
    swapUsed: Math.max(0, swapTotal - swapFree),
    usedPercent: total > 0 ? (used / total) * 100 : 0,
  }
}

export function parseMemoryPressure(output: string): { pressureSome: number; pressureFull: number } {
  const read = (kind: string) => {
    const line = output.split("\n").find((entry) => entry.startsWith(`${kind} `))
    const value = line?.match(/\bavg10=([\d.]+)/)?.[1]
    return value === undefined ? null : Number(value)
  }
  const pressureSome = read("some")
  const pressureFull = read("full")
  if (pressureSome === null || pressureFull === null || !Number.isFinite(pressureSome) || !Number.isFinite(pressureFull)) {
    throw new Error("Unexpected memory pressure data")
  }
  return { pressureSome, pressureFull }
}

export function parseMemoryProcesses(output: string, limit = 4): MemoryProcess[] {
  const grouped = new Map<string, number>()
  for (const line of output.split("\n")) {
    const match = line.trim().match(/^(.+?)\s+(\d+)$/)
    if (!match) continue
    const bytes = Number(match[2]) * 1024
    if (!Number.isFinite(bytes) || bytes <= 0) continue
    grouped.set(match[1], (grouped.get(match[1]) ?? 0) + bytes)
  }
  return [...grouped.entries()]
    .map(([name, residentBytes]) => ({ name, residentBytes }))
    .sort((left, right) => right.residentBytes - left.residentBytes)
    .slice(0, limit)
}

export function buildMemorySnapshot(infoOutput: string, pressureOutput: string, processOutput: string): MemorySnapshot {
  return {
    ...parseMemoryInfo(infoOutput),
    ...parseMemoryPressure(pressureOutput),
    processes: parseMemoryProcesses(processOutput),
  }
}

export function memoryFraction(value: number, total: number): number {
  if (!Number.isFinite(value) || !Number.isFinite(total) || total <= 0) return 0
  return Math.max(0, Math.min(1, value / total))
}
