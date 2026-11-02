/// What the inspector is looking at: a definition, a test, or a typed or selected expression.
export interface Subject {
  key: string
  kind: 'def' | 'test' | 'expr'
  title: string
  line?: number
  endLine?: number
  h?: number
  lhsH?: number
  rhsH?: number
  pass?: boolean
  term: { expr?: string; path?: string; text?: string; line?: number; side?: 'lhs' | 'rhs'; value?: number }
}
