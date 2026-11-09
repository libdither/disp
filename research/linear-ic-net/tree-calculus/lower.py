"""Source programs use tree calculus's triage (leaf -> c, stem u -> d u, fork u v -> b u v).
For LTC2 each arm is wrapped to drop the two arms it beat (twice K, or once K when packed)."""
from rules import L, A
from ltc2 import tri2, wait2
def sp(t):
    args = []
    while isinstance(t, tuple) and len(t) == 2: args.append(t[1]); t = t[0]
    return t, args[::-1]
def lower(t, packed=False):
    if isinstance(t, tuple) and len(t) == 3: return ('λ', t[1], lower(t[2], packed))
    if isinstance(t, tuple):
        h, args = sp(t)
        if h == L and len(args) >= 3:
            _, cd = sp(args[0])
            c, d = cd
            r = tri2(*(lower(z, packed) for z in (c, d, args[1], args[2])), packed=packed)
            return A(r, *[lower(a, packed) for a in args[3:]])
        return (lower(t[0], packed), lower(t[1], packed))
    return t
