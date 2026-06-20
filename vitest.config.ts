import { defineConfig, configDefaults } from "vitest/config";

export default defineConfig({
  test: {
    exclude: [...configDefaults.exclude, "**/.claude/**"],
    // NOTE: the heap bump for the strict universe (Type := StrictType) lives in the
    // package.json "test" script as NODE_OPTIONS=--max-old-space-size=8192 — it propagates
    // to the worker processes, whereas poolOptions.execArgv did not take effect here.
    // disp.test.ts loads the kernel ~29x per worker; the strict check retains ~3-4x more
    // per load than the old lean Type (4GB OOMs; 8GB ok). Real programs load the kernel once.
  },
});
