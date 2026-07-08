// See https://svelte.dev/docs/kit/types#app.d.ts
declare global {
  namespace App {}
}

declare module '*.disp?raw' {
  const src: string
  export default src
}

export {}
