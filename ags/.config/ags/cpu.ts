export const CPU_STAT_QUERY = ["cat", "/proc/stat"]
export const CPU_LOAD_QUERY = ["cat", "/proc/loadavg"]
export const CPU_INFO_QUERY = ["cat", "/proc/cpuinfo"]

export const CPU_THREAD_PAIRS = Array.from({ length: 12 }, (_, core) => [core, core + 12] as const)

export type CpuTimes = {
  id: number | null
  total: number
  idle: number
}

export type CpuStatSample = {
  overall: CpuTimes
  logical: CpuTimes[]
}

export type CpuSnapshot = {
  model: string
  overall: number | null
  cores: Array<number | null>
  load: [number, number, number]
  runningTasks: number
  totalTasks: number
  averageFrequencyGHz: number | null
}

export function parseCpuStat(output: string): CpuStatSample {
  const times = output.split("\n").flatMap((line) => {
    const match = line.match(/^cpu(\d*)\s+(.+)$/)
    if (!match) return []
    const counters = match[2].trim().split(/\s+/).slice(0, 8).map(Number)
    if (counters.length < 4 || counters.some((value) => !Number.isFinite(value))) return []
    return [{
      id: match[1] === "" ? null : Number(match[1]),
      total: counters.reduce((sum, value) => sum + value, 0),
      idle: counters[3] + (counters[4] ?? 0),
    }]
  })
  const overall = times.find((entry) => entry.id === null)
  if (!overall) throw new Error("Unexpected /proc/stat CPU data")
  return {
    overall,
    logical: times.filter((entry): entry is CpuTimes & { id: number } => entry.id !== null)
      .sort((left, right) => left.id - right.id),
  }
}

export function usageBetween(previous: CpuTimes | undefined, current: CpuTimes | undefined): number | null {
  if (!previous || !current) return null
  const total = current.total - previous.total
  const idle = current.idle - previous.idle
  if (total <= 0 || idle < 0) return null
  return Math.max(0, Math.min(100, ((total - idle) / total) * 100))
}

export function parseLoadAverage(output: string): {
  load: [number, number, number]
  runningTasks: number
  totalTasks: number
} {
  const fields = output.trim().split(/\s+/)
  const load = fields.slice(0, 3).map(Number)
  const tasks = fields[3]?.match(/^(\d+)\/(\d+)$/)
  if (load.length !== 3 || load.some((value) => !Number.isFinite(value)) || !tasks) {
    throw new Error("Unexpected /proc/loadavg data")
  }
  return {
    load: load as [number, number, number],
    runningTasks: Number(tasks[1]),
    totalTasks: Number(tasks[2]),
  }
}

export function parseCpuInfo(output: string): { model: string; averageFrequencyGHz: number | null } {
  const model = output.match(/^model name\s*:\s*(.+)$/m)?.[1]?.trim() ?? "CPU"
  const frequencies = [...output.matchAll(/^cpu MHz\s*:\s*([\d.]+)$/gm)]
    .map((match) => Number(match[1]))
    .filter(Number.isFinite)
  const averageFrequencyGHz = frequencies.length > 0
    ? frequencies.reduce((sum, value) => sum + value, 0) / frequencies.length / 1000
    : null
  return { model, averageFrequencyGHz }
}

export function buildCpuSnapshot(
  previous: CpuStatSample | null,
  current: CpuStatSample,
  loadOutput: string,
  infoOutput: string,
): CpuSnapshot {
  const load = parseLoadAverage(loadOutput)
  const info = parseCpuInfo(infoOutput)
  const logicalUsage = new Map(current.logical.map((cpu) => [
    cpu.id,
    usageBetween(previous?.logical.find((old) => old.id === cpu.id), cpu),
  ]))

  return {
    ...load,
    ...info,
    overall: usageBetween(previous?.overall, current.overall),
    cores: CPU_THREAD_PAIRS.map((threads) => {
      const values = threads.map((thread) => logicalUsage.get(thread) ?? null)
        .filter((value): value is number => value !== null)
      return values.length > 0 ? values.reduce((sum, value) => sum + value, 0) / values.length : null
    }),
  }
}

export function cpuText(value: number | null): string {
  return value === null ? "N/A" : `${value.toFixed(0)}%`
}

export function cpuFraction(value: number | null): number {
  return value === null ? 0 : Math.max(0, Math.min(1, value / 100))
}
