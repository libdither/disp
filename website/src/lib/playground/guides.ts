// Where each kind of step sends things, so the drawing can show the move
// instead of jumping: the binder copied down both sides of an S, a binder
// knocking its variable off the tree, the argument K drops.

import { R_K, R_F, type Store } from './store.ts'
import type { Trace } from './trace.ts'
import type { Guide } from './layout.ts'

/// The guide for going from `frame` to the next one.
export function stepGuide(store: Store, trace: Trace, frame: number): Guide | undefined {
  const root = trace.frames[frame]
  if (root === undefined || frame >= trace.last) return undefined
  const next = trace.mode === 'parallel' ? trace.readyPaths(root) : [trace.nextPath(root)]
  const guide: Guide = { origin: new Map(), sinks: [] }
  for (const path of next) {
    if (!path) continue
    const key = 'r0' + path.join('')
    const site = trace.subterm(root, path)
    switch (store.bindKind(site)) {
      case 'split':
        // the binder goes down both sides: both copies are it, starting as one and coming apart
        guide.moves = new Map([...(guide.moves ?? []), [key + '00', key], [key + '1', key]])
        guide.sinks.push([key, 'stay'])
        ;(guide.stage ??= []).push(key)
        break
      case 'const':
        // the binder shrinks into the leaf of the K that takes its place
        guide.sinks.push([key, key + '0'])
        ;(guide.stage ??= []).push(key)
        break
      case 'id':
        // the binder knocks its variable off the tree, and I grows where the binder was
        guide.origin.set(key, [key])
        guide.sinks.push([key + '0', 'stay'], [key, 'stay'])
        ;(guide.knock ??= []).push([key, key + '0'])
        ;(guide.stage ??= []).push(key)
        break
      case 'eta':
        // the binder knocks the variable it was handed off the tree, the application holding them fades,
        // and the function moves into the binder's place, staying itself on the way
        guide.moves = new Map([...(guide.moves ?? []), [key, key + '00']])
        guide.sinks.push([key + '01', 'stay'], [key + '0', 'fade'], [key, 'stay'])
        ;(guide.knock ??= []).push([key, key + '01'])
        ;(guide.stage ??= []).push(key)
        break
      case null:
        if (store.rule[site] === R_K) guide.sinks.push([key + '1', 'drop'])
        // triage keeps one branch where it is; the branches it didn't pick fade where they stand
        guide.sinks.push([key, store.rule[site] === R_F ? 'fade' : key])
        break
      default:
        guide.sinks.push([key, key])
    }
  }
  return guide.sinks.length ? guide : undefined
}
