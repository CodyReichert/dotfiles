import { describe, expect, test } from "bun:test"
import {
  buildMemorySnapshot,
  memoryFraction,
  parseMemoryInfo,
  parseMemoryPressure,
  parseMemoryProcesses,
} from "./memory"

const meminfo = `MemTotal:       65536000 kB
MemFree:         4194304 kB
MemAvailable:   41943040 kB
Buffers:            1024 kB
Cached:         37748736 kB
SReclaimable:    1048576 kB
Shmem:           1048576 kB
SwapTotal:             0 kB
SwapFree:              0 kB
`

describe("memory telemetry parsing", () => {
  test("calculates used, cache, composition, and disabled swap", () => {
    const memory = parseMemoryInfo(meminfo)
    expect(memory.total).toBe(65536000 * 1024)
    expect(memory.used).toBe((65536000 - 41943040) * 1024)
    expect(memory.cache).toBe(37748736 * 1024)
    expect(memory.swapTotal).toBe(0)
    expect(memory.usedPercent).toBeCloseTo(36, 5)
    expect(memory.compositionUsed + memory.cache + memory.buffers + memory.free).toBe(memory.total)
  })

  test("parses ten-second PSI averages", () => {
    expect(parseMemoryPressure("some avg10=0.25 avg60=0.10 total=1\nfull avg10=0.05 avg60=0.01 total=2\n")).toEqual({
      pressureSome: 0.25,
      pressureFull: 0.05,
    })
  })

  test("groups process names and orders them by total RSS", () => {
    const processes = parseMemoryProcesses("chrome 1000\ncode 900\nchrome 800\nkitty 200\n", 3)
    expect(processes).toEqual([
      { name: "chrome", residentBytes: 1800 * 1024 },
      { name: "code", residentBytes: 900 * 1024 },
      { name: "kitty", residentBytes: 200 * 1024 },
    ])
  })

  test("builds a complete snapshot and rejects missing fields", () => {
    const snapshot = buildMemorySnapshot(meminfo, "some avg10=0.00\nfull avg10=0.00\n", "app 100\n")
    expect(snapshot.processes[0].name).toBe("app")
    expect(() => parseMemoryInfo("MemTotal: 100 kB\n")).toThrow("Missing MemFree")
  })
})

describe("memory display helpers", () => {
  test("clamps fractions", () => {
    expect(memoryFraction(25, 100)).toBe(0.25)
    expect(memoryFraction(125, 100)).toBe(1)
    expect(memoryFraction(10, 0)).toBe(0)
  })
})
