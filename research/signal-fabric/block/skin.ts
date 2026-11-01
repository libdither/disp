// How design.bend's cells look on block.html, by the fields its top comment lists:
// [kind, class, symbol, arity, orient, state]. A new cell format brings a new skin.

const KIND = ['empty', 'wire down', 'wire across', 'tape', 'node', 'spare']
const CLASS = ['', 'going down', 'going up', 'inner']
const SYMBOL = ['', 'L', 'R', 'S', 'X', 'U', 'M', 'T', 'N', 'A', 'Q', 'a', 'b', 'c']
const ARITY = ['leaf', 'stem', 'fork']
const STATE = ['idle', 'routed down', 'routed across', 'refusing']
const SYM_COLOR: Record<string, string> = { L: '#7aa2f7', R: '#ffb454', S: '#a9b6c8', X: '#ffd866', U: '#56c8ff', M: '#3fdc8f', N: '#3fdc8f', T: '#ff6b6b', A: '#ff6b6b', Q: '#3fdc8f', '': '#c8d3e0' }
const WIRE = '#3a4a63', LEFT = '#7aa2f7', RIGHT = '#ffb454'

export function describe(c: number[]): string {
  const [kind, cls, sym, arity, orient, state] = c
  if (!kind && !cls) return 'empty'
  const parts = [KIND[kind] ?? `kind ${kind}`]
  if (kind === 4) parts.push(ARITY[arity] ?? `arity ${arity}`)
  if (kind === 4 && arity === 2 && orient) parts.push('parts swapped')
  if (kind === 4 && state) parts.push(STATE[state] ?? `state ${state}`)
  if (kind === 5) parts.push(orient ? 'upper, with a box' : 'lower')
  if (cls) parts.push(`signal ${SYMBOL[sym] || '·'} ${CLASS[cls] ?? cls}`)
  return parts.join(', ') + `  [${c.join(', ')}]`
}

function line(ctx: CanvasRenderingContext2D, x0: number, y0: number, x1: number, y1: number, color: string, w: number) {
  ctx.strokeStyle = color
  ctx.lineWidth = w
  ctx.beginPath()
  ctx.moveTo(x0, y0)
  ctx.lineTo(x1, y1)
  ctx.stroke()
}

// the cell at (cx, cy), z wide; nb(dx, dy) is a neighbour's fields
export function draw(ctx: CanvasRenderingContext2D, c: number[], cx: number, cy: number, z: number, nb: (dx: number, dy: number) => number[] | null) {
  const [kind, cls, sym, arity, orient, state] = c
  const h = z / 2, w = Math.max(1, z * 0.12)
  if (kind === 0) {
    // the empty corner where a wire across turns down into a fork's right part
    if (nb(-1, 0)?.[0] === 2 && nb(0, 1)?.[0] === 1) {
      ctx.setLineDash([z * 0.12, z * 0.12])
      line(ctx, cx - h, cy, cx, cy, WIRE, w)
      line(ctx, cx, cy, cx, cy + h, WIRE, w)
      ctx.setLineDash([])
    }
  } else if (kind === 1) line(ctx, cx, cy - h, cx, cy + h, WIRE, w)
  else if (kind === 2) line(ctx, cx - h, cy, cx + h, cy, WIRE, w)
  else if (kind === 3) {
    ctx.strokeStyle = '#2a3a52'
    ctx.lineWidth = Math.max(1, z * 0.06)
    ctx.strokeRect(cx - z * 0.3, cy - z * 0.3, z * 0.6, z * 0.6)
  } else if (kind === 4) {
    line(ctx, cx, cy - h, cx, cy, WIRE, w)
    if (arity === 1) line(ctx, cx, cy, cx, cy + h, '#a9b6c8', w * 1.2)
    if (arity === 2) {
      line(ctx, cx, cy, cx, cy + h, orient ? RIGHT : LEFT, w * 1.2)
      line(ctx, cx, cy, cx + h, cy, orient ? LEFT : RIGHT, w * 1.2)
    }
    ctx.fillStyle = arity === 0 ? '#3fdc8f' : arity === 1 ? '#a9b6c8' : '#e2e8f0'
    ctx.beginPath()
    ctx.arc(cx, cy, z * (arity === 0 ? 0.2 : 0.3), 0, 7)
    ctx.fill()
    if (state) {
      ctx.strokeStyle = state === 3 ? '#ff6b6b' : '#b39cff'
      ctx.lineWidth = Math.max(1.5, z * 0.1)
      ctx.beginPath()
      ctx.arc(cx, cy, z * 0.42, 0, 7)
      ctx.stroke()
      if (state === 1 || state === 2) {
        const [dx, dy] = state === 1 ? [0, 1] : [1, 0]
        line(ctx, cx + dx * z * 0.42, cy + dy * z * 0.42, cx + dx * h, cy + dy * h, '#b39cff', Math.max(1.5, z * 0.1))
      }
    }
  }
  else if (kind === 5) {
    // reads as plain wire: slack a stem or a fork can be made from
    line(ctx, cx, cy - h, cx, cy + h, WIRE, w)
    ctx.strokeStyle = '#5b6a80'
    ctx.lineWidth = Math.max(1, z * 0.06)
    ctx.beginPath()
    ctx.arc(cx, cy, z * 0.2, 0, 7)
    ctx.stroke()
    if (orient) line(ctx, cx + z * 0.2, cy, cx + h, cy, '#5b6a80', w)
  }
  if (cls) badge(ctx, cx, cy, z, SYMBOL[sym] ?? '?', cls)
}

// a signal: filled going down, outlined going up, dashed for a probe or a let-go
function badge(ctx: CanvasRenderingContext2D, cx: number, cy: number, z: number, s: string, cls: number) {
  const color = SYM_COLOR[s] ?? '#c8d3e0'
  const bw = z * 0.56, bh = z * 0.42, x = cx + z * 0.18, y = cy - z * 0.2
  ctx.beginPath()
  ctx.roundRect(x - bw / 2, y - bh / 2, bw, bh, bh / 3)
  if (cls === 1) {
    ctx.fillStyle = color
    ctx.fill()
  } else {
    ctx.fillStyle = '#0b0e14'
    ctx.fill()
    ctx.strokeStyle = color
    ctx.lineWidth = Math.max(1, z * 0.07)
    if (cls === 3) ctx.setLineDash([z * 0.08, z * 0.06])
    ctx.stroke()
    ctx.setLineDash([])
  }
  if (z >= 12) {
    ctx.fillStyle = cls === 1 ? '#0b0e14' : color
    ctx.font = `bold ${Math.round(bh * 0.75)}px ui-monospace, monospace`
    ctx.textAlign = 'center'
    ctx.textBaseline = 'middle'
    ctx.fillText(s || '·', x, y + 0.5)
  }
}

export const LEGEND = `
  <i style="width:16px;height:3px;background:${WIRE}"></i><span>a wire cell (down or across); a dashed corner is the empty cell where a wire across turns down</span>
  <i style="width:12px;height:12px;border-radius:50%;background:#e2e8f0"></i><span>fork: its <b style="color:${LEFT}">left</b> part hangs below, its <b style="color:${RIGHT}">right</b> part is across to the right (they trade colours when a swap turns the fork)</span>
  <i style="width:12px;height:12px;border-radius:50%;background:#a9b6c8"></i><span>stem, its part below</span>
  <i style="width:8px;height:8px;border-radius:50%;background:#3fdc8f"></i><span>leaf</span>
  <i style="width:12px;height:12px;border:1px solid #2a3a52"></i><span>tape: a held token waiting its turn above the tree</span>
  <i style="width:16px;height:10px;border-radius:3px;background:#ffd866"></i><span>a token going down (filled) or its echo going up (outlined): <b style="color:#7aa2f7">L</b> <b style="color:#ffb454">R</b> <b style="color:#a9b6c8">S</b> a step into a fork's left or right part or a stem's part, <b style="color:#ffd866">X</b> swap here, <b style="color:#56c8ff">U</b> one step back up, <b style="color:#3fdc8f">M</b> make a stem, <b style="color:#3fdc8f">N</b> make a unit, <b style="color:#ff6b6b">T</b> take a stem back, <b style="color:#ff6b6b">A</b> take a unit back, <b style="color:#3fdc8f">Q</b> a stem on a stem's part; dashed: an inner signal (a probe, a let-go, or a step on its way)</span>
  <i style="width:12px;height:12px;border-radius:50%;border:1px solid #5b6a80"></i><span>spare: slack that reads as plain wire, which a stem or (the upper one, with a box on its right) a unit is made from; a program that needs more than a place has bounces forever</span>
  <i style="width:10px;height:10px;border-radius:50%;border:2px solid #b39cff"></i><span>a node routed toward a part (the stub shows which); red: refusing a packet that does not fit it</span>`
