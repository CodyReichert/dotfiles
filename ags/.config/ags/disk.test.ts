import { describe, expect, test } from "bun:test"
import { diskFraction, formatBytes, loadDiskSnapshot, parseDiskSnapshot } from "./disk"

const normalOutput = JSON.stringify({
  filesystems: [
    { source: "/dev/nvme1n1p2[/@]", target: "/", fstype: "btrfs", size: 999129350144, used: 505097822208, avail: 488933130240, "use%": "51%" },
    { source: "/dev/nvme0n1p1", target: "/data", fstype: "btrfs", size: 4000785104896, used: 178714558464, avail: 3820638158848, "use%": "4%" },
    { source: "/dev/mapper/big_betty_crypt", target: "/betty", fstype: "xfs", size: 17998052065280, used: 1329116327936, avail: 16668935737344, "use%": "7%" },
  ],
})

describe("disk snapshot parsing", () => {
  test("parses configured Btrfs and XFS mounts", () => {
    const disks = parseDiskSnapshot(normalOutput)
    expect(disks.map(({ name, mountPoint, mounted }) => ({ name, mountPoint, mounted }))).toEqual([
      { name: "System", mountPoint: "/", mounted: true },
      { name: "DataStorage", mountPoint: "/data", mounted: true },
      { name: "BigBetty", mountPoint: "/betty", mounted: true },
    ])
    expect(disks[0].source).toBe("/dev/nvme1n1p2")
    expect(disks[2].filesystem).toBe("xfs")
  })

  test("keeps a configured card when its mount is missing", () => {
    const parsed = JSON.parse(normalOutput)
    parsed.filesystems = parsed.filesystems.filter((disk: { target: string }) => disk.target !== "/betty")
    const betty = parseDiskSnapshot(JSON.stringify(parsed))[2]
    expect(betty).toMatchObject({ name: "BigBetty", mountPoint: "/betty", mounted: false })
    expect(betty.size).toBeNull()
  })

  test("rejects malformed and structurally invalid JSON", () => {
    expect(() => parseDiskSnapshot("not json")).toThrow()
    expect(() => parseDiskSnapshot("{}" )).toThrow("Unexpected findmnt data")
  })

  test("propagates a failed findmnt command", async () => {
    await expect(loadDiskSnapshot(async () => { throw new Error("findmnt failed") })).rejects.toThrow("findmnt failed")
  })
})

describe("disk display helpers", () => {
  test("formats binary byte units", () => {
    expect(formatBytes(505097822208)).toBe("470.4 GiB")
    expect(formatBytes(1329116327936)).toBe("1.2 TiB")
    expect(formatBytes(null)).toBe("N/A")
  })

  test("clamps usage meters", () => {
    const [disk] = parseDiskSnapshot(normalOutput)
    expect(diskFraction(disk)).toBe(0.51)
    expect(diskFraction({ ...disk, usePercent: 120 })).toBe(1)
    expect(diskFraction({ ...disk, usePercent: null, used: null })).toBe(0)
  })
})
