// The comparison data assembled from research/awesome-langs at build time by
// loader.server.ts. An ambient declaration: no top-level import, or it would
// stop being global.
declare module 'virtual:awesome-langs' {
  const data: import('./types').LangsData
  export default data
}
