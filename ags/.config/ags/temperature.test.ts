import { describe, expect, test } from "bun:test"
import {
  loadTemperatureSnapshot,
  parseTemperatureSnapshot,
  temperatureFraction,
  temperatureText,
} from "./temperature"

const sensorOutput = JSON.stringify({
  "k10temp-pci-00c3": {
    Adapter: "PCI adapter",
    Tctl: { temp1_input: 59.875 },
    Tccd1: { temp3_input: 58.25 },
    Tccd2: { temp4_input: 46.75 },
  },
  "asusec-isa-000a": {
    Adapter: "ISA adapter",
    Motherboard: { temp3_input: 33 },
    VRM: { temp5_input: 45 },
  },
  "amdgpu-pci-4500": { edge: { temp1_input: 44 } },
  "nvme-pci-0500": { Composite: { temp1_input: 35.85 } },
  "nvme-pci-0200": { Composite: { temp1_input: 37.85 } },
  "spd5118-i2c-6-51": { temp1: { temp1_input: 37.5 } },
  "spd5118-i2c-6-53": { temp1: { temp1_input: 38.5 } },
})

describe("temperature snapshot parsing", () => {
  test("extracts CPU, board, storage, memory, and iGPU sensors", () => {
    const snapshot = parseTemperatureSnapshot(sensorOutput)
    expect(snapshot.cpuPackage).toBe(59.875)
    expect(Object.fromEntries(snapshot.readings.map((reading) => [reading.key, reading.value]))).toEqual({
      ccd1: 58.25,
      ccd2: 46.75,
      motherboard: 33,
      vrm: 45,
      "system-nvme": 35.85,
      "data-nvme": 37.85,
      "memory-a": 37.5,
      "memory-b": 38.5,
      igpu: 44,
    })
  })

  test("keeps missing sensors as unavailable", () => {
    const snapshot = parseTemperatureSnapshot(JSON.stringify({ "k10temp-pci-00c3": { Tctl: { temp1_input: 52 } } }))
    expect(snapshot.cpuPackage).toBe(52)
    expect(snapshot.readings.every((reading) => reading.value === null)).toBe(true)
  })

  test("rejects invalid data and propagates query failure", async () => {
    expect(() => parseTemperatureSnapshot("[]")).toThrow("Unexpected sensors data")
    expect(() => parseTemperatureSnapshot("not json")).toThrow()
    await expect(loadTemperatureSnapshot(async () => { throw new Error("sensors failed") })).rejects.toThrow("sensors failed")
  })
})

describe("temperature display helpers", () => {
  test("formats readings and clamps the 20–100 degree scale", () => {
    expect(temperatureText(59.875)).toBe("59.9°C")
    expect(temperatureText(null)).toBe("N/A")
    expect(temperatureFraction(20)).toBe(0)
    expect(temperatureFraction(60)).toBe(0.5)
    expect(temperatureFraction(120)).toBe(1)
  })
})
