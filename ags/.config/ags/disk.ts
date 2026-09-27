export const CONFIGURED_VOLUMES = [
  { name: "System", mountPoint: "/" },
  { name: "DataStorage", mountPoint: "/data" },
  { name: "BigBetty", mountPoint: "/betty" },
] as const

export const FINDMNT_QUERY = [
  "findmnt",
  "--json",
  "--bytes",
  "--list",
  "--output",
  "SOURCE,TARGET,FSTYPE,SIZE,USED,AVAIL,USE%,LABEL",
]

export type VolumeConfig = (typeof CONFIGURED_VOLUMES)[number]

export type DiskSnapshot = {
  name: string
  mountPoint: string
  mounted: boolean
  source: string | null
  filesystem: string | null
  size: number | null
  used: number | null
  available: number | null
  usePercent: number | null
}

type FindmntFilesystem = {
  source?: unknown
  target?: unknown
  fstype?: unknown
  size?: unknown
  used?: unknown
  avail?: unknown
  "use%"?: unknown
}

function optionalNumber(value: unknown): number | null {
  const number = typeof value === "number" ? value : Number(value)
  return Number.isFinite(number) && number >= 0 ? number : null
}

function percentage(value: unknown): number | null {
  if (typeof value !== "string" && typeof value !== "number") return null
  return optionalNumber(String(value).replace(/%$/, ""))
}

function unavailableVolume(config: VolumeConfig): DiskSnapshot {
  return {
    ...config,
    mounted: false,
    source: null,
    filesystem: null,
    size: null,
    used: null,
    available: null,
    usePercent: null,
  }
}

export function parseDiskSnapshot(output: string): DiskSnapshot[] {
  const parsed = JSON.parse(output) as { filesystems?: unknown }
  if (!Array.isArray(parsed.filesystems)) {
    throw new Error("Unexpected findmnt data")
  }

  const filesystems = parsed.filesystems as FindmntFilesystem[]
  return CONFIGURED_VOLUMES.map((config) => {
    const filesystem = filesystems.find((entry) => entry.target === config.mountPoint)
    if (!filesystem) return unavailableVolume(config)

    const source = typeof filesystem.source === "string"
      ? filesystem.source.replace(/\[[^\]]+\]$/, "")
      : null

    return {
      ...config,
      mounted: true,
      source,
      filesystem: typeof filesystem.fstype === "string" ? filesystem.fstype : null,
      size: optionalNumber(filesystem.size),
      used: optionalNumber(filesystem.used),
      available: optionalNumber(filesystem.avail),
      usePercent: percentage(filesystem["use%"]),
    }
  })
}

export async function loadDiskSnapshot(
  run: (command: string[]) => Promise<string>,
): Promise<DiskSnapshot[]> {
  return parseDiskSnapshot(await run([...FINDMNT_QUERY]))
}

export function formatBytes(bytes: number | null): string {
  if (bytes === null || !Number.isFinite(bytes) || bytes < 0) return "N/A"
  const units = ["B", "KiB", "MiB", "GiB", "TiB", "PiB"]
  let amount = bytes
  let unit = 0
  while (amount >= 1024 && unit < units.length - 1) {
    amount /= 1024
    unit++
  }
  if (unit === 0) return `${amount.toFixed(0)} ${units[unit]}`
  return `${amount.toFixed(1).replace(/\.0$/, "")} ${units[unit]}`
}

export function diskFraction(disk: DiskSnapshot): number {
  const fraction = disk.usePercent !== null
    ? disk.usePercent / 100
    : disk.used !== null && disk.size !== null && disk.size > 0
      ? disk.used / disk.size
      : 0
  return Math.max(0, Math.min(1, fraction))
}
