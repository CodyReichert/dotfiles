import { describe, expect, test } from "bun:test"
import {
  buildCpuSnapshot,
  cpuFraction,
  cpuText,
  parseCpuInfo,
  parseCpuStat,
  parseLoadAverage,
  usageBetween,
} from "./cpu"

function stat(overall: string, logical: string[]) {
  return `cpu  ${overall}\n${logical.map((values, cpu) => `cpu${cpu} ${values}`).join("\n")}\n`
}

const previousText = stat("100 0 50 850 0 0 0 0", Array.from({ length: 24 }, () => "10 0 5 85 0 0 0 0"))
const currentText = stat("140 0 70 890 0 0 0 0", Array.from({ length: 24 }, (_, cpu) =>
  cpu < 12 ? "18 0 7 95 0 0 0 0" : "14 0 6 100 0 0 0 0"))

describe("CPU utilization parsing", () => {
  test("calculates deltas instead of lifetime counters", () => {
    const previous = parseCpuStat(previousText)
    const current = parseCpuStat(currentText)
    expect(usageBetween(previous.overall, current.overall)).toBe(60)
    expect(usageBetween(undefined, current.overall)).toBeNull()
  })

  test("aggregates sibling threads into twelve physical cores", () => {
    const snapshot = buildCpuSnapshot(
      parseCpuStat(previousText),
      parseCpuStat(currentText),
      "1.25 0.75 0.50 4/1200 99\n",
      "model name : AMD Ryzen Test CPU\ncpu MHz : 5000\ncpu MHz : 4000\n",
    )
    expect(snapshot.cores).toHaveLength(12)
    expect(snapshot.cores[0]).toBe(37.5)
    expect(snapshot.load).toEqual([1.25, 0.75, 0.5])
    expect(snapshot.runningTasks).toBe(4)
    expect(snapshot.averageFrequencyGHz).toBe(4.5)
  })

  test("rejects malformed stat and load data", () => {
    expect(() => parseCpuStat("intr 123")).toThrow("Unexpected /proc/stat CPU data")
    expect(() => parseLoadAverage("bad data")).toThrow("Unexpected /proc/loadavg data")
  })
})

describe("CPU metadata and display helpers", () => {
  test("parses model and average frequency", () => {
    expect(parseCpuInfo("model name : Example CPU\ncpu MHz : 5200\ncpu MHz : 4800\n")).toEqual({
      model: "Example CPU",
      averageFrequencyGHz: 5,
    })
  })

  test("formats and clamps utilization", () => {
    expect(cpuText(42.4)).toBe("42%")
    expect(cpuText(null)).toBe("N/A")
    expect(cpuFraction(125)).toBe(1)
    expect(cpuFraction(null)).toBe(0)
  })
})
