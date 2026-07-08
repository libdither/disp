export interface TocChild {
  id: string
  title: string
}
export interface TocEntry {
  id: string
  title: string
  children?: TocChild[]
}

export const toc: TocEntry[] = [
  {
    id: 'motivation',
    title: 'Motivation',
    children: [
      { id: 'what-is-disp', title: 'What is disp?' },
      { id: 'why-this-design', title: 'Why does this design exist?' }
    ]
  },
  {
    id: 'trees',
    title: 'From λ-calculus to trees',
    children: [
      { id: 'ski', title: 'The SKI detour' },
      { id: 'tree-calculus', title: 'Tree calculus' },
      { id: 'five-rules', title: 'The five rules' },
      { id: 'bracket', title: 'Bracket abstraction' },
      { id: 'hash-consing', title: 'Hash-consing & O(1) equality' }
    ]
  },
  {
    id: 'source',
    title: 'A first look at disp',
    children: [
      { id: 'binders', title: 'Binders & application' },
      { id: 'declarations', title: 'Declarations & tests' },
      { id: 'modules', title: 'Modules' }
    ]
  },
  {
    id: 'predicates',
    title: 'Types as predicates',
    children: [
      { id: 'traditions', title: 'Four traditions' },
      { id: 'booleans', title: 'Booleans are shapes' },
      { id: 'central-equation', title: 'The central equation, running' }
    ]
  },
  {
    id: 'kernel',
    title: 'The two-operation kernel',
    children: [
      { id: 'hypothesis-problem', title: 'The hypothesis problem' },
      { id: 'dispatcher', title: 'The dispatcher & the walker' },
      { id: 'sigma-ops', title: 'bind_hyp and hyp_reduce' },
      { id: 'spines', title: 'Spines' },
      { id: 'who-checks', title: 'Three layers of soundness' }
    ]
  },
  {
    id: 'library',
    title: 'Library types',
    children: [
      { id: 'telescope', title: 'The telescope' },
      { id: 'coproduct', title: 'Sums & recursion' },
      { id: 'equality', title: 'Propositional equality' },
      { id: 'universe', title: 'The universe checks itself' }
    ]
  },
  {
    id: 'future',
    title: 'Future directions',
    children: [
      { id: 'landed', title: 'What has landed' },
      { id: 'cubical', title: 'Cubical type theory' },
      { id: 'long-arc', title: 'The long arc' }
    ]
  },
  { id: 'references', title: 'References' },
  { id: 'glossary', title: 'Glossary' }
]
