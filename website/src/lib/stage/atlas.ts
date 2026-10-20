// Text for the stage: every string drawn once, white, into one texture, and tinted where it is used.

import { CanvasTexture, LinearFilter, LinearMipmapLinearFilter } from 'three'

/** Where a string sits in the texture, and its measures in pixels at the size it was drawn. */
export interface Glyphs {
  u0: number
  v0: number
  u1: number
  v1: number
  /** Advance, ascent and descent, and the padding drawn around them. */
  width: number
  ascent: number
  descent: number
  pad: number
}

/** The size strings are drawn at: big enough to stay sharp zoomed in, small enough that a few hundred fit. */
export const ATLAS_PX = 64

export class Atlas {
  readonly size = 2048
  readonly canvas: HTMLCanvasElement
  readonly texture: CanvasTexture
  private ctx: CanvasRenderingContext2D
  private entries = new Map<string, Glyphs>()
  private x = 0
  private y = 0
  private row = 0
  /** Bumped when the texture is cleared, so the places handed out before are stale. */
  generation = 0
  dirty = false

  constructor(private family: string) {
    this.canvas = document.createElement('canvas')
    this.canvas.width = this.canvas.height = this.size
    this.ctx = this.canvas.getContext('2d', { willReadFrequently: false })!
    this.texture = new CanvasTexture(this.canvas)
    this.texture.generateMipmaps = true
    this.texture.minFilter = LinearMipmapLinearFilter
    this.texture.magFilter = LinearFilter
  }

  /** The texture starts over: after the font loads, or when it is full. */
  clear() {
    this.ctx.clearRect(0, 0, this.size, this.size)
    this.entries.clear()
    this.x = this.y = this.row = 0
    this.generation++
    this.dirty = true
  }

  get(text: string, weight = 400): Glyphs {
    const key = `${weight}|${text}`
    let e = this.entries.get(key)
    if (e) return e
    const g = this.ctx
    g.font = `${weight} ${ATLAS_PX}px ${this.family}`
    const m = g.measureText(text)
    const pad = 4
    const ascent = Math.ceil(m.fontBoundingBoxAscent ?? ATLAS_PX * 0.8)
    const descent = Math.ceil(m.fontBoundingBoxDescent ?? ATLAS_PX * 0.2)
    const w = Math.ceil(m.width) + 2 * pad
    const h = ascent + descent + 2 * pad
    if (this.x + w > this.size) {
      this.x = 0
      this.y += this.row
      this.row = 0
    }
    if (this.y + h > this.size) {
      this.clear()
      return this.get(text, weight)
    }
    g.fillStyle = '#fff'
    g.textBaseline = 'alphabetic'
    g.fillText(text, this.x + pad, this.y + pad + ascent)
    const S = this.size
    // the texture is flipped as it is uploaded: canvas row y is v = 1 - y / S
    e = { u0: this.x / S, u1: (this.x + w) / S, v0: 1 - (this.y + h) / S, v1: 1 - this.y / S, width: m.width, ascent, descent, pad }
    this.entries.set(key, e)
    this.x += w
    this.row = Math.max(this.row, h)
    this.dirty = true
    return e
  }

  /** Uploads what was drawn since the last frame. */
  flush() {
    if (!this.dirty) return
    this.texture.needsUpdate = true
    this.dirty = false
  }
}
