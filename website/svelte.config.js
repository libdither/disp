import adapter from '@sveltejs/adapter-static'
import { vitePreprocess } from '@sveltejs/vite-plugin-svelte'

/** @type {import('@sveltejs/kit').Config} */
const config = {
  preprocess: vitePreprocess(),
  kit: {
    adapter: adapter({
      pages: 'build',
      assets: 'build',
      fallback: undefined,
      precompress: false,
      strict: true
    }),
    paths: {
      // GitHub Pages serves the site at https://libdither.github.io/disp
      base: process.env.BASE_PATH ?? ''
    },
    prerender: {
      // hash links may target anchors rendered client-side (Learn's sections)
      handleMissingId: 'warn'
    }
  }
}

export default config
