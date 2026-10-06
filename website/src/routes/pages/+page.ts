import { base } from '$app/paths'

// static/repo/pages.json, written by scripts/link-pages.mts
export async function load({ fetch }) {
  const res = await fetch(`${base}/repo/pages.json`)
  const pages: { path: string; title: string }[] = await res.json()
  return { pages }
}
