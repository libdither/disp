"""The programs: check.PROGRAMS, then the counting programs written here.

Every counting program here reads the same code in the same order of reads as `size` and counts the same
binders. Arms are named for what they are handed: f o a v p for the other arms (fun, opened, app, var, prim),
then the parts (F, L, g b, w), then what the program passes after the arms."""
import sys
sys.path.insert(0, '..')
from check import PROGRAMS

MINE = r'''
-- closed arms, self handed in after the choice
size2 := Z (\self c. ρ c
   (\o a v p F self. suc (self (F ρ)))
   (\f a v p L self. self L)
   (\f o v p g b self. add (self g) (self b))
   (\f o a p w self. zero)
   (\f o a v self. zero) self)

-- count_a: the arms are threaded. The taken arm passes on the four arms it is handed and a new copy of
-- itself, so no arm is erased and only the taken one is copied. Each arm is a template applied to itself
-- (me me makes the next copy), so there is no fixpoint. The walk is one loop: an application reads its
-- left part with a continuation that reads the right part. The count is a linear unary number.
-- A leaf drops what it read (w), and the end drops the arms.
afun := \me o a v p F n k. ρ (F ρ) (me me) o a v p (lsuc n) k
aopn := \me f a v p L n k. ρ L f (me me) a v p n k
aapp := \me f o v p g b n k. ρ g f o (me me) v p n (\f o a v p n. ρ b f o a v p n k)
avar := \me f o a p w n k. k f o a (me me) p n
aprm := \me f o a v n k. k f o a v (me me) n
astop := \f o a v p n. n
count_a := \c. ρ c (afun afun) (aopn aopn) (aapp aapp) (avar avar) (aprm aprm) lzero astop

-- count_b: count_a made linear: what a leaf reads goes on a pile j, and the end hands back the count, the
-- pile and the five arms. No eraser anywhere in the program; copies only of me.
bfun := \me o a v p F n j k. ρ (F ρ) (me me) o a v p (lsuc n) j k
bopn := \me f a v p L n j k. ρ L f (me me) a v p n j k
bapp := \me f o v p g b n j k. ρ g f o (me me) v p n j (\f o a v p n j. ρ b f o a v p n j k)
bvar := \me f o a p w n j k. k f o a (me me) p n (\r. r w j)
bprm := \me f o a v n j k. k f o a v (me me) n j
bstop := \f o a v p n j r. r n j f o a v p
count_b := \c. ρ c (bfun bfun) (bopn bopn) (bapp bapp) (bvar bvar) (bprm bprm) lzero (\x. x) bstop

-- count_c: count_b with the successor written in place (\s z. s (n s z)) instead of called (lsuc n)
cfun := \me o a v p F n j k. ρ (F ρ) (me me) o a v p (\s z. s (n s z)) j k
count_c := \c. ρ c (cfun cfun) (bopn bopn) (bapp bapp) (bvar bvar) (bprm bprm) lzero (\x. x) bstop

-- count_d: count_c with a stack of views in place of continuations. An application pushes the pair of
-- its right part's view (ρ b, a value λk1…k5) and the rest of the stack; a leaf pops it and hands the
-- view the five arms. The bottom of the stack hands the leaf a stand-in for a view that takes the arms and
-- the count and gives the answer: the count, the pile, the five arms and what stood for the rest.
dfun := \me o a v p F n j k. ρ (F ρ) (me me) o a v p (\s z. s (n s z)) j k
dopn := \me f a v p L n j k. ρ L f (me me) a v p n j k
dapp := \me f o v p g b n j k. ρ g f o (me me) v p n j (\r. r (ρ b) k)
dvar := \me f o a p w n j k. k (\vb k. vb f o a (me me) p n (\r. r w j) k)
dprm := \me f o a v n j k. k (\vb k. vb f o a v (me me) n j k)
dstop := \r. r (\f o a v p n j k z. z n j f o a v p k) (\x. x)
count_d := \c. ρ c (dfun dfun) (dopn dopn) (dapp dapp) (dvar dvar) (dprm dprm) lzero (\x. x) dstop

-- walk: count_d with nothing counted and nothing kept: the walk alone (it drops what a leaf reads, and
-- the end drops the arms). Not a counting program; it measures what reading every node costs by itself.
wfun := \me o a v p F k. ρ (F ρ) (me me) o a v p k
wopn := \me f a v p L k. ρ L f (me me) a v p k
wapp := \me f o v p g b k. ρ g f o (me me) v p (\r. r (ρ b) k)
wvar := \me f o a p w k. k (\vb k. vb f o a (me me) p k)
wprm := \me f o a v k. k (\vb k. vb f o a v (me me) k)
wstop := \r. r (\f o a v p k. k) (\x. x)
walk := \c. ρ c (wfun wfun) (wopn wopn) (wapp wapp) (wvar wvar) (wprm wprm) wstop
'''

SRC = PROGRAMS + MINE

# how to get a linear unary number out of each program's answer (run apart from the measured run)
TO_NUMBER = {
    'size': '\\r. r lsuc lzero',
    'size2': '\\r. r lsuc lzero',
    'count_a': '\\r. r',
    'count_b': '\\r. r (\\n j f o a v p. n)',
    'count_c': '\\r. r (\\n j f o a v p. n)',
    'count_d': '\\r. r (\\n j f o a v p k. n)',
    'walk': None,
}
