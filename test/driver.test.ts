// The impure driver POC (src/driver.ts): spawn the CLI on the example
// programs and observe real stdout/stdin behavior. The kernel-side story
// (mock handlers instead of the driver) is pinned in lib/tests; these two
// are the boundary's only host-facing tests.
import { describe, it, expect } from "vitest"
import { execFileSync } from "node:child_process"

const run = (file: string, input?: string) =>
  execFileSync("npx", ["tsx", "src/run.ts", file], { encoding: "utf-8" as const, input, timeout: 180_000 })

describe("driver", () => {
  it("hello world prints to stdout", () => {
    expect(run("examples/hello.disp")).toContain("Hello, world!")
  }, 200_000)

  it("echo reads stdin and prints it back", () => {
    expect(run("examples/echo.disp", "hi driver\n")).toContain("hi driver")
  }, 200_000)
})
