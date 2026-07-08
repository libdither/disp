import { sveltekit } from '@sveltejs/kit/vite'
import { defineConfig, type Plugin } from 'vite'
import { fileURLToPath } from 'node:url'

// The playground bundles the real disp compiler (../src) into a web worker.
// Two impedance mismatches to bridge:
//   1. Compiler sources import each other with `.js` specifiers (node ESM
//      style) but live as `.ts` files.
//   2. They import node:fs / node:path, which the browser lacks. We swap in
//      shims (a virtual filesystem preloaded with lib/**/*.disp, and a posix
//      path implementation) — but ONLY when the importer is a compiler file,
//      so SvelteKit's own node:path use at build time stays untouched.
const repoSrc = fileURLToPath(new URL('../src', import.meta.url))
const shims: Record<string, string> = {
  'node:fs': fileURLToPath(new URL('./src/lib/disp/shims/fs.ts', import.meta.url)),
  'node:path': fileURLToPath(new URL('./src/lib/disp/shims/path.ts', import.meta.url))
}

function dispCompilerBridge(): Plugin {
  return {
    name: 'disp-compiler-bridge',
    enforce: 'pre',
    async resolveId(source, importer, options) {
      if (!importer || !importer.startsWith(repoSrc)) return null
      if (source in shims) return shims[source]
      if (source.startsWith('.') && source.endsWith('.js')) {
        const r = await this.resolve(source.replace(/\.js$/, '.ts'), importer, {
          ...options,
          skipSelf: true
        })
        return r ?? null
      }
      return null
    }
  }
}

export default defineConfig({
  plugins: [dispCompilerBridge(), sveltekit()],
  server: { fs: { allow: ['..'] } },
  worker: {
    format: 'es',
    plugins: () => [dispCompilerBridge()]
  },
  build: {
    target: 'es2022'
  }
})
