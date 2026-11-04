/// Calls `read` every frame while the page eases to a new palette (app.css transitions --mix over 0.9s);
/// returns a cancel, so an effect can hand it back as its cleanup.
export function followTheme(read: () => void, ms = 1000): () => void {
  const until = performance.now() + ms
  let raf = 0
  const tick = () => {
    read()
    if (performance.now() < until) raf = requestAnimationFrame(tick)
  }
  tick()
  return () => cancelAnimationFrame(raf)
}
