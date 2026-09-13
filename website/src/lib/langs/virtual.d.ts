// The comparison data assembled from research/awesome-langs at build time by
// loader.server.ts. Ambient declarations: no top-level import, or they would
// stop being global.
declare module 'virtual:awesome-langs' {
  const data: import('./types').LangsData
  export default data
}
declare module 'virtual:awesome-langs/summary' {
  const data: import('./types').LangsSummary
  export default data
}
