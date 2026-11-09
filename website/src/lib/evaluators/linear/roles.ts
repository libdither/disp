// The role-mark types the cloned canvas can draw (copied from the playground's roles.ts; this visualizer passes none).

export type RoleKind = 'copy' | 'left' | 'right' | 'drop' | 'look'

export interface RoleMark {
  /// the drawing key of the deciding fork in the finished tree
  key: string
  kind: RoleKind
  input: string
  /// the Try it frame where it happens, the step about to fire
  frame: number
  /// it happens in the step about to fire on the frame showing
  now?: boolean
}

/// a role mark under the pointer, or the inputs a leaf ends up holding
export type RoleHit = { mark: RoleMark; hold?: undefined } | { mark?: undefined; hold: { key: string; inputs: string[] } }

export interface Roles {
  marks: RoleMark[]
  /// where inputs land, by key: an I they pass through, or a leaf that ends up holding them in the result
  holds: Map<string, string[]>
  /// the input each branch handles, by the key of the node the branch leads to
  tint: Map<string, string>
  inputs: string[]
  /// the run went past the limit, so some inputs may have more to do
  partial: boolean
  /// the input the step about to fire acts on, whose branches are shaded
  active?: string | null
}
