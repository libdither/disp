// Where each kind of step sends things, so the drawing can show the move
// instead of jumping: the binder copied down both sides of an S, a binder and
// its variable meeting where I takes their place, the argument K drops.

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
        // the binder goes down both sides
        guide.origin.set(key + '00', [key])
        guide.origin.set(key + '1', [key])
        guide.sinks.push([key, 'stay'])
        break
      case 'const':
        guide.sinks.push([key, 'stay'])
        break
      case 'id':
        // the binder comes down and its variable up; I appears where they meet
        guide.origin.set(key, [key, key + '0'])
        guide.sinks.push([key, key])
        break
      case 'eta':
        // the function rises into the binder's place (from the body, when it is a new node rather than a kept subtree);
        // the binder and the variable it was handed meet it there and cancel out, the application fades first
        guide.origin.set(key, [key + '00'])
        guide.sinks.push([key + '01', key], [key + '0', 'fade'], [key, key])
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
