// Going back (index.html): the run as it was at some clocks is saved in the engine (wasm.rs
// `save_state`), and any clock already reached is shown by putting back the nearest saved state
// before it and running on. That repeats the run exactly, since every move is a hash of its site
// and its clock. Saved states thin out with distance from the clock on show: near it there is one
// every clock or so, far away about one every tenth of the distance, all within `budget` bytes. So
// stepping back costs little, a jump costs a replay of about a tenth of its length, and a larger
// budget only makes the spacing finer. Cooling, once only the answer is left, starts at a clock
// the run records, and every replay or step through that clock cools there too.
const T = { saved: [], frontier: 0, budget: 64 << 20, coolAt: null, settleAt: null, job: null, savedAt: 0, drawn: "" };
let settled = false;

const clockNow = () => stats()[1];
/// The clocks wanted between saved states at distance d from the clock on show.
const gapAt = d => Math.max(1, d / 8);
/// The last saved state at or before clock c (its index; -1 if none).
function tlIndex(c) {
  let lo = 0, hi = T.saved.length - 1, r = -1;
  while (lo <= hi) { const m = (lo + hi) >> 1; if (T.saved[m].clock <= c) { r = m; lo = m + 1; } else hi = m - 1; }
  return r;
}

/// A new run: nothing saved but its start.
function tlReset() {
  T.saved = []; T.frontier = 0; T.coolAt = null; T.settleAt = null; T.job = null; settled = false;
  tlSave(); tlShow();
}
function tlSave() {
  const c = clockNow(), i = tlIndex(c);
  if (i >= 0 && T.saved[i].clock === c) return;
  const id = E.save_state();
  T.saved.splice(i + 1, 0, { clock: c, id, bytes: E.state_bytes(id) });
}
/// Forget saved states until they fit the budget, each time the one whose neighbours are nearest
/// for its distance from clock `head`, so what is kept thins out geometrically away from it.
function tlThin(head) {
  let total = T.saved.reduce((a, s) => a + s.bytes, 0);
  while (total > T.budget && T.saved.length > 2) {
    let worst = -1, score = Infinity;
    for (let i = 1; i < T.saved.length; i++) {
      const prev = T.saved[i - 1].clock, next = i + 1 < T.saved.length ? T.saved[i + 1].clock : T.frontier;
      const v = (next - prev) / (Math.abs(T.saved[i].clock - head) + 1);
      if (v < score) { score = v; worst = i; }
    }
    E.drop_state(T.saved[worst].id); total -= T.saved[worst].bytes; T.saved.splice(worst, 1);
  }
}

/// After clocks ran forward on show: the furthest clock reached, and a saved state (while playing,
/// at most every 100 ms; stepping back from there replays at most that much, saving as it goes).
function tlNote() {
  const c = clockNow(), now = performance.now();
  T.frontier = Math.max(T.frontier, c);
  if (T.coolAt !== null && c >= T.coolAt) cleaned = true;
  if (!playing || now - T.savedAt >= 100) { T.savedAt = now; tlSave(); tlThin(c); }
  tlShow();
}

/// One clock, told in words (the events go to `story`); cooling starts here if the run cooled here.
function tlStepClock() {
  if (clockNow() === T.coolAt) E.strands_cool();
  const r = E.strands_step(1);
  story = Array.from({ length: E.events_len() }, (_, i) => Array.from(new Uint32Array(E.memory.buffer, E.events_ptr() + i * 32, 8)));
  return r;
}
/// Up to n clocks unnarrated, stopping at the clock the run cooled at to cool there.
function tlClocks(n) {
  const c = clockNow();
  if (c === T.coolAt) E.strands_cool();
  if (T.coolAt !== null && c < T.coolAt) n = Math.min(n, T.coolAt - c);
  return E.strands_clocks(n);
}

/// Show clock `target`, from 0 to the furthest reached: put back the nearest saved state before
/// it, unless running on from the clock on show is nearer, then run on (`tlPump`, a frame at a
/// time), saving states more densely as the target nears.
function tlSeek(target) {
  target = Math.max(0, Math.min(Math.round(target), T.frontier));
  story = [];
  dropGpu();
  if (playing) setPlaying(false);
  const c = clockNow(), s = T.saved[tlIndex(target)];
  if (!(target >= c && s.clock <= c) && s.clock !== c) E.restore_state(s.id);
  T.job = { target };
  tlPump();
}
/// Run a seek on for about 20 ms; done when the target is reached.
function tlPump() {
  const t0 = performance.now(), target = T.job.target;
  for (let c = clockNow(); c < target && performance.now() - t0 < 20; c = clockNow()) {
    const i = tlIndex(c), d = target - c;
    tlClocks(Math.max(1, Math.min(d, Math.ceil(d / 16))));
    if (clockNow() - (i >= 0 ? T.saved[i].clock : -Infinity) >= gapAt(target - clockNow())) tlSave();
  }
  if (clockNow() >= target) { T.job = null; tlThin(target); tlArrived(); }
  else { refreshGarbage(true); V3.dirty = true; tlShow(); }
}
/// The player's view of a clock it jumped to.
function tlArrived() {
  const c = clockNow();
  finished = E.strands_clocks(0) === 1;
  cleaned = T.coolAt !== null && c >= T.coolAt;
  settled = T.settleAt !== null && c >= T.settleAt;
  hist = hist.filter(h => h.sw <= c); bursts = [];
  E.fire_log_clear();
  refreshGarbage(true); showStory(); panel(true); V3.dirty = true; tlShow();
}

/// One clock back, told in words; with `toRewrite`, back to just after the last rewrite before it.
function tlBack(toRewrite) {
  if (!L || T.job || G?.busy) return;
  const c = clockNow();
  if (c === 0) return;
  let to = c - 1;
  if (toRewrite) { const x = tlLastRewrite(c - 1); if (x === null) return; to = x + 1; }
  tlSeek(Math.max(0, to - 1));
  while (T.job) tlPump();
  if (to > 0) { tlStepClock(); tlArrived(); }
}
/// The latest clock x < hi whose next clock fired a rewrite (in the graph view, or collected garbage:
/// anything that changes the net), null if none, found by replaying back from saved states.
function tlLastRewrite(hi) {
  const changes = () => { const st = stats(); return st[2] + (viewMode === "graph" ? st[16] : 0); };
  for (let top = hi; top > 0;) {
    const s = T.saved[tlIndex(top - 1)];
    E.restore_state(s.id);
    let last = null;
    for (let x = s.clock; x < top; x++) { const f = changes(); tlClocks(1); if (changes() > f) last = x; }
    if (last !== null) return last;
    top = s.clock;
  }
  return null;
}

/// The time bar: the clock on show among those reached, and where states are saved.
function tlShow() {
  const r = $("time"), c = clockNow();
  r.max = T.frontier; r.value = T.job ? T.job.target : c;
  const mb = T.saved.reduce((a, s) => a + s.bytes, 0) / (1 << 20);
  $("timev").textContent = `clock ${fmt(c)} / ${fmt(T.frontier)}`;
  $("timev").title = `${T.saved.length} states saved (${mb.toFixed(1)} MB)`;
  // The ticks: drawn again only when they or the canvas changed.
  const cv = $("ticks"), w = Math.round(cv.clientWidth * devicePixelRatio), h = Math.round(cv.clientHeight * devicePixelRatio);
  const key = `${w} ${h} ${T.frontier} ${T.saved.length} ${T.saved[T.saved.length - 1]?.clock} ${answerClock} ${T.coolAt}`;
  if (key === T.drawn) return;
  T.drawn = key;
  if (cv.width !== w || cv.height !== h) { cv.width = w; cv.height = h; }
  const g = cv.getContext("2d");
  g.clearRect(0, 0, w, h);
  const x = k => T.frontier ? 6 * devicePixelRatio + (w - 12 * devicePixelRatio) * k / T.frontier : 0;
  g.fillStyle = "rgba(133,149,173,.35)";
  for (const s of T.saved) g.fillRect(x(s.clock), 0, Math.max(1, devicePixelRatio), h);
  if (finished || answerClock) { g.fillStyle = "#ffd866"; g.fillRect(x(answerClock), 0, 2 * devicePixelRatio, h); }
  if (T.coolAt !== null) { g.fillStyle = "#8bddff"; g.fillRect(x(T.coolAt), 0, 2 * devicePixelRatio, h); }
}
