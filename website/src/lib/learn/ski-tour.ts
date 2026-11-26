// The SKI walkthrough in Learn §2.1: each step loads an expression into the
// playground's tree viewer, moves to a spot in its reel, and says what is
// there. `expect` is what the text claims the viewer shows; `mismatches`
// checks it against the viewer itself, so a change to the viewer that makes
// the text wrong shows up as an error on the page instead of a quiet lie.
import type { ViewerState } from '$lib/playground/Inspector.svelte'

export interface TourStep {
  title: string
  expr: string
  /// a frame of the binder phase, a frame of the run, or the run's last frame
  at: { binders: number } | { run: number } | 'end'
  /// read the tree as S, K and I (the viewer's SK button); on unless said otherwise
  sk?: boolean
  /// the data-tour name of the control the text points at
  point?: string
  /// HTML
  text: string
  expect: {
    phase?: ViewerState['phase']
    from?: string
    to?: string
    /// readings that must be drawn; [] means none
    combs?: string[]
    /// the rule about to fire
    next?: string
    rewrites?: number
    runSteps?: number
    symbols?: string[]
    end?: ViewerState['end']
  }
}

export const steps: TourStep[] = [
  {
    title: 'I, the identity',
    expr: '{x} => x',
    at: { binders: 0 },
    point: 'caption',
    text: `The viewer starts on the code as written, binder and all: <code>x</code> sits above the body that uses it. The line under the tree names the next rewrite. Here the body is <code>x</code> itself, so the whole function becomes <code>I</code>.`,
    expect: { phase: 'binders', from: '{x} => x', to: 'I', rewrites: 1 }
  },
  {
    title: 'Then it runs',
    expr: '{x} => x',
    at: { run: 0 },
    point: 'step',
    text: `With the binder gone, the viewer calls the result on one symbol per parameter, here <code>x</code>, and runs it. The step button fires one rule: <code>I(x)</code> gives back <code>x</code>.`,
    expect: { phase: 'run', from: 'I(x)', to: 'x', symbols: ['x'] }
  },
  {
    title: 'K keeps one, drops one',
    expr: '{x, y} => x',
    at: { binders: 0 },
    point: 'binders',
    text: `Binders come off inside out. <code>y</code> isn't used in the body, so <code>{y} => x</code> becomes <code>K(x)</code>, which ignores its argument.`,
    expect: { phase: 'binders', from: '{y} => x', to: 'K(x)' }
  },
  {
    title: 'A shortcut',
    expr: '{x, y} => x',
    at: { binders: 1 },
    text: `<code>{x} => K(x)</code> only passes <code>x</code> straight on, so it is just <code>K</code>. Disp's compiler takes this shortcut (η-reduction, §2.4) wherever it can.`,
    expect: { from: '{x} => K(x)', to: 'K', rewrites: 2 }
  },
  {
    title: 'K at work',
    expr: '{x, y} => x',
    at: { run: 1 },
    point: 'tree',
    text: `Called on <code>x</code> and <code>y</code>, the K rule fires: <code>K(x)(y)</code> becomes <code>x</code>, and <code>y</code> drops off the tree.`,
    expect: { phase: 'run', from: 'K(x)(y)', to: 'x' }
  },
  {
    title: 'S shares its argument',
    expr: '{f, g, x} => f(x, g(x))',
    at: { binders: 0 },
    text: `Here <code>x</code> is used on both sides of an application, so the compiler splits it: <code>S</code> hands its argument to both halves.`,
    expect: { phase: 'binders', from: '{x} => f(x, g(x))', to: 'S({x} => f(x), {x} => g(x))' }
  },
  {
    title: 'All of it is S',
    expr: '{f, g, x} => f(x, g(x))',
    at: { binders: 5 },
    point: 'tree',
    text: `Four shortcuts later the whole function is <code>S</code>. The tinted band marks a part of the tree that runs the S rule once it's called; K and I get colours of their own.`,
    expect: { phase: 'run', from: 'S(f)', combs: ['S'], rewrites: 5 }
  },
  {
    title: 'S at work',
    expr: '{f, g, x} => f(x, g(x))',
    at: { run: 5 },
    point: 'caption',
    text: `Called on <code>f</code>, <code>g</code> and <code>x</code>, the S rule fires: <code>S(f, g)(x)</code> becomes <code>f(x, g(x))</code>. The steps before it only built <code>S(f, g)</code> up from its parts.`,
    expect: { phase: 'run', from: 'S(f, g)(x)', to: 'f(x, g(x))' }
  },
  {
    title: 'S is a tree',
    expr: '{f, g, x} => f(x, g(x))',
    at: { run: 5 },
    sk: false,
    point: 'sk',
    text: `The SK button turns the reading off. S, K and I aren't built in: each is a small tree made of <code>t</code>, and <code>S(f, g)</code> is <code>t(t(f), g)</code>. The S, K and I bands were the viewer reading shapes. The S still at the top marks the rule about to fire, which works on shapes too.`,
    expect: { from: 't(t(f), g)(x)', to: 'f(x, g(x))', combs: [], next: 'S' }
  },
  {
    title: 'What the detour costs',
    expr: '{f, x, y} => f(y, x)',
    at: 'end',
    point: 'steps',
    text: `Swapping two arguments takes 13 rewrites to compile and 22 steps to run: with no variables, S and K have to route every value to its place. The list button shows every rewrite or call.`,
    expect: { phase: 'run', from: 'f(y, x)', rewrites: 13, runSteps: 22, end: 'stuck' }
  }
]

/// Where in the viewer's reel a step sits.
export function place(step: TourStep, s: ViewerState): number {
  if (step.at === 'end') return s.lead + s.runSteps
  return 'binders' in step.at ? step.at.binders : s.lead + step.at.run
}

/// Every way the viewer disagrees with what the step says it shows.
export function mismatches(step: TourStep, s: ViewerState, root: HTMLElement): string[] {
  if (s.error) return [`the viewer can't show ${step.expr}: ${s.error}`]
  const out: string[] = []
  const e = step.expect
  const differ = (what: string, got: unknown, want: unknown) => {
    if (JSON.stringify(got) !== JSON.stringify(want)) out.push(`${what} is ${JSON.stringify(got)}, the text says ${JSON.stringify(want)}`)
  }
  if (step.point && !root.querySelector(`[data-tour="${step.point}"]`)) out.push(`the viewer has no control named "${step.point}" (data-tour)`)
  differ('the position', s.at, place(step, s))
  if (e.phase) differ('the phase', s.phase, e.phase)
  if (e.from !== undefined) differ('the rewrite\'s left side', s.caption.from, e.from)
  if (e.to !== undefined) differ('the rewrite\'s right side', s.caption.to, e.to)
  if (e.rewrites !== undefined) differ('the number of binder rewrites', s.rewrites, e.rewrites)
  if (e.runSteps !== undefined) differ('the number of run steps', s.runSteps, e.runSteps)
  if (e.symbols) differ('the symbols', s.symbols, e.symbols)
  if (e.end !== undefined) differ('how the run ends', s.end, e.end)
  if (e.next !== undefined) differ('the rule about to fire', s.next, e.next)
  if (e.combs) {
    if (e.combs.length === 0) differ('the S, K, I readings drawn', s.combs, [])
    for (const c of e.combs) if (!s.combs.includes(c)) out.push(`no ${c} is drawn on the tree, the text says there is`)
  }
  return out
}
