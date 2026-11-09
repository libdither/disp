// Where each kind of step sends things, so the drawing can show the move instead of jumping. Kept subtrees
// travel on their own (the drawing matches them by id); these hints add what matching can't see: the binder
// moving down into the side that holds its variable (both sides for W), a binder knocking its variable off,
// the argument K drops, the second copy W makes, the leaf triage eats.

import { R_K, R_W, R_T0, type Store } from './store.ts'
import type { Trace } from './trace.ts'
import type { Guide } from './layout.ts'

/// The guide for going from `frame` to the next one.
export function stepGuide(store: Store, trace: Trace, frame: number): Guide | undefined {
  const root = trace.frames[frame]
  if (root === undefined || frame >= trace.last) return undefined
  const next = trace.mode === 'parallel' ? trace.readyPaths(root) : [trace.nextPath(root)]
  const guide: Guide = { origin: new Map(), sinks: [] }
  const move = (to: string, from: string) => (guide.moves ??= new Map()).set(to, from)
  for (const path of next) {
    if (!path) continue
    const key = 'r0' + path.join('')
    const site = trace.subterm(root, path)
    switch (store.bindKind(site)) {
      case 'id':
        // the binder knocks its variable off the tree, and I grows where the binder was
        guide.origin.set(key, [key])
        guide.sinks.push([key + '0', 'stay'], [key, 'stay'])
        ;(guide.knock ??= []).push([key, key + '0'])
        ;(guide.stage ??= []).push(key)
        break
      case 'const':
        // the binder shrinks into the leaf of the K that takes its place
        guide.sinks.push([key, key + '0'])
        ;(guide.stage ??= []).push(key)
        break
      case 'eta':
        // the binder knocks off the variable it was handed, and the function moves into its place
        move(key, key + '00')
        guide.sinks.push([key + '01', 'stay'], [key + '0', 'fade'], [key, 'stay'])
        ;(guide.knock ??= []).push([key, key + '01'])
        ;(guide.stage ??= []).push(key)
        break
      case 'right':
        // B M (λx. N): the binder goes down the right side
        move(key + '1', key)
        guide.sinks.push([key, 'stay'])
        ;(guide.stage ??= []).push(key)
        break
      case 'left':
        // C (λx. M) N: the binder goes down the left side
        move(key + '10', key)
        guide.sinks.push([key, 'stay'])
        ;(guide.stage ??= []).push(key)
        break
      case 'both':
        // W (λx. λx′. M N′): the binder becomes two, one for each side
        move(key + '1', key)
        move(key + '10', key)
        guide.sinks.push([key, 'stay'])
        ;(guide.stage ??= []).push(key)
        break
      case 'chain':
        guide.sinks.push([key, key])
        ;(guide.stage ??= []).push(key)
        break
      case null:
        // K drops its argument: it falls off the tree
        if (store.rule[site] === R_K) guide.sinks.push([key + '1', 'drop'])
        // W: the first copy comes from the argument too (the second stays where it was)
        if (store.rule[site] === R_W) move(key + '01', key + '1')
        // triage on a leaf eats the leaf
        if (store.rule[site] === R_T0) guide.sinks.push([key + '1', 'fade'])
        guide.sinks.push([key, key])
        break
    }
  }
  return guide.sinks.length ? guide : undefined
}
