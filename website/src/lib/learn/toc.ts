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
    id: 'claims',
    title: 'Claims: a type is a test',
    children: [
      { id: 'traditions', title: 'Four traditions' },
      { id: 'booleans', title: 'Booleans are shapes' },
      { id: 'honest', title: 'Asking every tree' }
    ]
  },
  {
    id: 'lenses',
    title: 'Checkers are lenses',
    children: [
      { id: 'finite', title: 'Proved needs a finite list' },
      { id: 'lens', title: 'Lenses and their law' },
      { id: 'rules', title: 'The five rules you trust' },
      { id: 'floor', title: "Why the floor can't go lower" },
      { id: 'today', title: 'What runs today' }
    ]
  },
  {
    id: 'saying',
    title: 'What claims can say',
    children: [
      { id: 'function-types', title: 'Function types' },
      { id: 'equality', title: 'Equality' },
      { id: 'induction', title: 'Induction' },
      { id: 'search', title: 'Search' }
    ]
  },
  {
    id: 'future',
    title: 'Where it is going',
    children: [
      { id: 'pinned', title: 'What runs' },
      { id: 'designed', title: 'What is designed' },
      { id: 'open', title: 'What has no argument yet' }
    ]
  },
  { id: 'references', title: 'References' },
  { id: 'glossary', title: 'Glossary' }
]
