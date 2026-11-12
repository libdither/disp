"""Differential tests: the nets against the term interpreter, on random programs and on hand-written reflective ones."""
import sys, random
from collections import Counter
from net import Net, readback, ARITY, PARTS, arms
from ref import Machine, Stuck, Fuel, canon, parse

sys.setrecursionlimit(100000)


def gen(rng, n, env, rho, readers=False):
    """a random closed term with about n nodes whose names come from env; with rho, ρ and opened functions
    appear too; with readers, some subterms read a value and use its parts in every arm"""
    if n == 1:
        pool = env + (['ρ', 'ρ'] if rho else [])
        return rng.choice(pool) if pool else 'ρ' if rho else ('λ', 'z', 'z')
    if readers and n >= 3 and rng.random() < 0.2:
        return reader(rng, n, env)
    if n == 2 or rng.random() < 0.3:
        x = f'v{len(env)}'
        binder = 'Λ' if rho and rng.random() < 0.15 else 'λ'
        return (binder, x, gen(rng, n - 1, env + [x], rho, readers))
    k = rng.randint(1, n - 2)
    return (gen(rng, k, env, rho, readers), gen(rng, n - 1 - k, env, rho, readers))


def reader(rng, n, env):
    """ρ on a random term, with an arm for every kind of value: each takes the other arms and the parts,
    and its body is a small random term over them"""
    t = ('ρ', gen(rng, max(1, n // 2), env, True, True))
    for kind in arms():
        xs = [f'v{len(env) + i}' for i in range(len(arms()) - 1 + len(PARTS[kind]))]
        body = gen(rng, rng.randint(1, 4), env + xs, True, True)
        for x in reversed(xs): body = ('λ', x, body)
        t = (t, body)
    return t


def reachable(n, root):
    seen, todo = set(), [root]
    while todo:
        a = todo.pop()
        if a in seen: continue
        seen.add(a)
        for i in range(ARITY[n.kind[a]] + 1):
            m = n.mate.get((a, i))
            if m is not None: todo.append(m[0])
    return len(seen)


def run_net(t, seed=None, limit=100000):
    n = Net(seed)
    root = n.load(t)
    status = n.run(limit)
    res = None
    if status == 'done':
        res = canon(readback(n, n.mate[(root, 0)]))
        garbage = len(n.kind) - reachable(n, root)
        if garbage: status = f'garbage {garbage}'
    return status, res, n.stats


def run_ref(t, fuel=4000):
    m = Machine(fuel)
    try:
        return 'value', canon(m.run(t)), m.betas
    except Stuck:
        return 'stuck', None, m.betas
    except Fuel:
        return 'fuel', None, m.betas


def compare(t, seeds=(None, 1, 2)):
    """None when both agree, else a description"""
    kind, want, _ = run_ref(t)
    if kind == 'fuel': return 'skip'
    for s in seeds:
        status, got, stats = run_net(t, s)
        if stats['annihilated across two copies']: return f'annihilated across copies (seed {s})'
        live = [k for k in stats if k.split()[0] in ('copy', 'erase') and k.split()[1] in ('O', 'T', 'R', 'root', 'ph')]
        if live: return f'live agents copied or erased: {live}'
        if kind == 'value' and (status != 'done' or got != want):
            return f'seed {s}: net {status} {got}\n   ref {want}'
        if kind == 'stuck' and status not in ('stuck', 'limit'):
            return f'seed {s}: ref stuck, net {status} {got}'
    return None


PROGRAMS = r'''
-- Church numerals and booleans, a fixpoint, and reflective programs reading code
zero := \f x. x
suc := \n f x. f (n f x)
add := \m n f x. m f (n f x)
true := \t e. t
false := \t e. e
Z := \g. (\h. g (\v. h h v)) (\h. g (\v. h h v))
-- Reading a value: ρ c fun opened app var prim. The arm for c's kind runs, handed the other four arms
-- and then the parts. Arms below name what they are handed: o f a v p for the arms, then the parts.
-- is it a function? each arm drops the arms it is handed and the parts
kindOf := \c. ρ c (\o a v p F. true) (\f a v p L. true) (\f o v p g b. false) (\f o a p w. false) (\f o a v. false)
-- count binders in code: open each binder on ρ, recurse into both sides of an application
size := Z (\self c. ρ c
   (\o a v p F. suc (self (F ρ)))
   (\f a v p L. self L)
   (\f o v p g b. add (self g) (self b))
   (\f o a p w. zero)
   (\f o a v. zero))
-- an arm that is never taken: any value will do
no := ρ
-- an error: a piece of code that is not a function, applied
err := (^d. d) ρ ρ
-- reading a function opens it; reading the opened function closes it again
close := \F. ρ F no (\f a v p L. L) no no no
open := \L. ρ L (\o a v p F. F) no no no no
-- An interpreter for the calculus. Functions, opened functions, spliced values and ρ are values already.
-- An application is interpreted: a function is opened on the argument and its body is interpreted, an
-- opened function hands back its body as code, ρ reads. Applying anything else is an error (err is stuck).
interp := Z (\self c. ρ c
   (\o a v p F. close F)
   (\f a v p L. open L)
   (\f o v p g b. (\x y. ρ x
        (\o a v p F. self (F y))
        (\f a v p L. open L y)
        (\f o v p g b. err)
        (\f o a p w. err)
        (\f o a v. ρ y)) (self g) (self b))
   (\f o a p w. w)
   (\f o a v. ρ))
-- The adds. Each is named for the kind of program it is, the shape of its numbers, and the way it adds.
--   linear      every variable is used, so nothing is erased, and the only copies are the ones the
--               program asks for by using a variable twice
--   ordinary    the usual way: a fixpoint or a test, and what is not picked is dropped
--   reversible  linear, and the answer holds the first number too, so the second can be had back:
--               a subtract undoes the add
-- Numbers are unary, a list of bits, or a tree of bits. A tree is added one bit after another, or in
-- parallel.
--
-- Unary. A linear number is its own loop: each successor copies the step and passes it on, and zero
-- hands it back. 3u is written for lsuc (lsuc (lsuc lzero)).
lzero := \s z. z s
lsuc := \n s z. s (n s z)
-- linear_unary_add m n: the sum, and the step handed back
linear_unary_add := \m n. m (\k n. k (lsuc n)) (\s n k. k n s) n
-- The usual way: a number says whether it is zero or the successor of another, and a fixpoint goes round
-- once for each successor of m. 3s is written for \z s. s (\z s. s (\z s. s (\z s. z))).
ordinary_unary_add := Z (\self m. m (\n. n) (\p n. self p (\z s. s n)))
-- A bit hands two things on: in order, or swapped.
b0 := \p q k. k p q
b1 := \p q k. k q p
-- Lists of bits. A linear one is a loop over its bits, lowest first: each cell copies the step and hands
-- it the bit, and the end hands the step back. 5 is written for cell b1 (cell b0 (cell b1 (cell b0 wend))):
-- every list of bits in a program has one bit more than the largest needs, so a sum of two fits.
wend := \f n. n f
cell := \x w f n. f x (w f n)
-- a chain of pairs: what a number's loop gives when it is run with pair
pair := \x r k. k x r
-- One bit of the add: x is a's bit, k the rest of a's loop, b the rest of the other number as a
-- chain, c the carry. Three compare-and-swaps put the three bits in order (x x y uses x up, and its
-- copy takes its place). Then the middle one swaps the outer two: the sum bit and the next carry.
addbit := \x k b c ret. b (\y rb. x x y (\p q. q q c (\m hi. p p m (\lo mid. mid hi lo (\s cn. k rb cn (\rs. ret (cell s rs)))))))
addend := \f b c ret. ret wend b c f
-- linear_list_add a b: the sum, the carry out, and what is handed back: the end of b's chain and the step
linear_list_add := \a b. a addbit addend (b pair (\p. p)) b0 (\s rest c f k. k s c rest f)
-- Cheaper, with two shapes: a's bits are the cells of its loop (each cell copies the arm for its bit,
-- add0 or add1; 5a is written for w1 (w0 (w1 (w0 (\o i n. n o i))))), and b is a chain of pairs (5p).
-- With a's bit known, one compare-and-swap and one more swap do. The answer holds a again, then the sum
-- as a chain, which cannot be the a of another add, the carry out, and the two arms.
w0 := \w o i n. o (w o i n)
w1 := \w o i n. i (w o i n)
add0 := \k b c ret. b (\y rb. y y c (\lo hi. lo hi b0 (\s cn. k rb cn (\ra rs. ret (w0 ra) (pair s rs)))))
add1 := \k b c ret. b (\y rb. y y c (\lo hi. hi b1 lo (\s cn. k rb cn (\ra rs. ret (w1 ra) (pair s rs)))))
linear_list_add_two_shapes := \a b. a add0 add1 (\o i b c ret. ret (\o i n. n o i) b c o i) b b0 (\a s c o i k. k a s c o i)
-- The usual way: a list says whether it is its end or a bit and a rest, the bits are true and false,
-- and a fixpoint goes down both lists. 5c is written for such a list of the bits of 5.
bool_xor := \x y. x (y false true) y
bool_maj := \x y c. x (y true c) (y c false)
ordinary_list_add_carry := Z (\self a b c. a (\n c. n) (\x ra. b (\n c. n) (\y rb. (\s r n c. c s r) (bool_xor (bool_xor x y) c) (self ra rb (bool_maj x y c)))))
ordinary_list_add := \a b. ordinary_list_add_carry a b false
-- Trees of bits. A tree number is a pair: its bits as a tree, and its depth as a loop. The tree is pairs
-- of pairs with a bit at each leaf, the low half first; the loop runs a step once for each level. 5t is
-- written for pair (pair (pair b1 b0) (pair b1 b0)) (\s z. s (s z)): every tree number in a program has
-- the same depth, the least whose leaves hold one bit more than the largest needs.
-- Three bits added: the sum bit and the carry. The three compare-and-swaps of addbit, then the swap.
add3 := \x y c ret. x x y (\p q. q q c (\m hi. p p m (\lo mid. mid hi lo ret)))
-- An adder for trees twice as large, from one for the halves: the low halves first, then the high
-- halves with the carry. It copies the adder it is given, and that is its only copy.
double := \f a b c ret. a (\alo ahi. b (\blo bhi. f alo blo c (\slo c1. f ahi bhi c1 (\shi c2. ret (pair slo shi) c2))))
-- linear_tree_add a b: the sum and the carry out, one bit after another. a's depth doubles add3 once
-- for each level, and the sum takes b's depth. A linear program cannot ask a tree whether it is a
-- leaf: the arm for the other answer would be left over. So the depth says where the leaves are.
linear_tree_add := \a b. a (\ta la. b (\tb lb. la double add3 ta tb b0 (\s c. pair (pair s lb) c)))
-- The usual way, on the same trees: a bit is tested and the arm not taken is dropped, a bit used twice
-- is copied, a's depth is used twice and b's is dropped.
pick := \x zero one. x zero one (\p q. p)
oxor := \x y. pick x y (pick y b1 b0)
omaj := \x y c. pick x (pick y b0 c) (pick y c b1)
oadd3 := \x y c. pair (oxor (oxor x y) c) (omaj x y c)
odouble := \f a b c. a (\alo ahi. b (\blo bhi. f alo blo c (\slo c1. f ahi bhi c1 (\shi c2. pair (pair slo shi) c2))))
ordinary_tree_add := \a b. a (\ta la. b (\tb lb. la odouble oadd3 ta tb b0 (\s c. pair (pair s la) c)))
-- In parallel, on the same trees. linear_tree_add runs one bit after another, since each waits for the
-- carry before it. Here every part of a tree first works out its carry out for a carry in of 0 and for
-- one of 1 (g and h; g is never more than h). A leaf has them from its two bits, sorted. A pair has them
-- from its halves: the low half's answer picks one of the high half's two. None of that waits for a
-- carry, so all the parts work at once. Then the carry goes down: a pair hands it to its low half and
-- picks the low half's answer for its high half, and a leaf flips its sum bit by it.
-- forget v x: x, with the bit v used up: v swaps two things that are the same, so nothing is erased
forget := \v x. v (\z. z) (\z. z) (\a b. a (b x))
-- a leaf, once its two bits are sorted: g, h, and the sum bit for a carry in c. Each of g, h and c that
-- is 1 swaps b0 and b1, so b1 comes out first when an odd number of them are
pleaf_sorted := \g h k. k g h (\c. g b0 b1 (\u v. h u v (\w z. c w z (\s t. forget t s))))
pleaf := \x y. x x y pleaf_sorted
-- two halves joined. A bit picks one of two by swapping them, and the one not picked is forgotten
join := \lo hi. lo (\gl hl sl. hi (\gh hh sh k. k (gl gh hh (\u v. forget v u)) (hl gh hh (\u v. forget v u))
   (\c. c gl hl (\ch v. forget v (pair (sl c) (sh ch))))))
pdouble := \f a b. a (\alo ahi. b (\blo bhi. join (f alo blo) (f ahi bhi)))
-- linear_tree_add_parallel a b: the sum and the carry out, as linear_tree_add gives them
linear_tree_add_parallel := \a b. a (\ta la. b (\tb lb. la pdouble pleaf ta tb (\g h s. forget h (pair (pair (s b0) lb) g))))
-- the same the usual way: the bit not picked is dropped
opleaf := \x y. (\g h k. k g h (\c. oxor (oxor g h) c)) (pick x b0 y) (pick x y b1)
ojoin := \lo hi. lo (\gl hl sl. hi (\gh hh sh k. k (pick gl gh hh) (pick hl gh hh) (\c. pair (sl c) (sh (pick c gl hl)))))
opdouble := \f a b. a (\alo ahi. b (\blo bhi. ojoin (f alo blo) (f ahi bhi)))
ordinary_tree_add_parallel := \a b. a (\ta la. b (\tb lb. la opdouble opleaf ta tb (\g h s. pair (pair (s b0) la) g)))
-- Reversible, on the same trees. An add alone cannot be undone: 1 + 2 and 2 + 1 give the same sum.
-- reversible_tree_add a b hands back a and the sum, in as many bits as the numbers have: a is used
-- twice, so it is copied once, and the carry out is forgotten (the sum and a say what it was).
-- reversible_tree_sub a s hands back a and s - a, so each undoes the other:
-- reversible_tree_add a b reversible_tree_sub is the pair of a and b again.
reversible_tree_add := \a b. a (\ta la. b (\tb lb. la double add3 ta tb b0 (\s c. forget c (pair a (pair s lb)))))
-- s - a is s + (a with every bit flipped) + 1. A bit that swaps b1 and b0 hands on its flip and itself
-- (nx and x2), with no copy: so the first compare-and-swap of add3, on the flipped bit, is x2 y nx.
sub3 := \x y c ret. x b1 b0 (\nx x2. x2 y nx (\p q. q q c (\m hi. p p m (\lo mid. mid hi lo ret))))
reversible_tree_sub := \a s. a (\ta la. s (\ts ls. la double sub3 ta ts b1 (\d c. forget c (pair a (pair d ls)))))
-- the same pair in parallel
reversible_tree_add_parallel := \a b. a (\ta la. b (\tb lb. la pdouble pleaf ta tb (\g h s. forget g (forget h (pair a (pair (s b0) lb))))))
pleaf_sub := \x y. x b1 b0 (\nx x2. x2 y nx pleaf_sorted)
reversible_tree_sub_parallel := \a s. a (\ta la. s (\ts ls. la pdouble pleaf_sub ta ts (\g h d. forget g (forget h (pair a (pair (d b1) ls))))))
'''

def program(body):
    return parse(PROGRAMS + '\n' + body)

def quote(src):
    '''source of a program whose value is the code of src: open λd. src on ρ'''
    return f'(ρ (\\d. {src}) (\\o a v p F. F ρ) no no no no)'

BODY = '(\\o a v p F. F ρ) no no no no'      # arms that open a function on ρ and hand back its body
OPENED = '(\\o a v p F. F) no no no no'      # arms that hand back the opened function
TESTS = [
    ('kind of a function', 'kindOf (\\x. x)'),
    ('kind of ρ', 'kindOf ρ'),
    ('binders in λx. x', 'size (\\x. x)'),
    ('binders in λx y. y x', 'size (\\x y. y x)'),
    ('binders in a closure', 'size ((\\x y. x) (\\a b c. c))'),
    ('binders in suc', 'size suc'),
    ('binders in add 2 2', 'size (add (suc (suc zero)) (suc (suc zero)))'),
    ('binders in size itself', 'size size'),
    ('run after copying a closure', '(\\f. f (f zero)) (add (suc zero))'),
    ('open then read the body', f'ρ (\\x. x x) {BODY}'),
    ('read an application part', f'ρ ((\\x. \\y. x y) zero) {BODY}'),
    ('share a piece of code', f'(\\c k. k c c) (ρ (\\x. x x) {BODY})'),
    ('share a spliced value', f'(\\c k. k c c) (ρ (\\x. x) {BODY})'),
    ('share an opened binder', f'(\\g k. k g g) (ρ (\\x y. y x) {OPENED})'),
    ('reflect an opened binder', f'kindOf (ρ (\\x. x) {OPENED})'),
    ('open a shared copy twice', f'(\\g. g ρ (g zero)) (ρ (\\x y. x) {OPENED})'),
    ('read, close, then run', 'close (open suc) zero'),
    ('binders in an opened function', f'size (ρ (\\x y. y x) {OPENED})'),
    ('an opened function as a quote', '(^d. (\\x. x x) (\\y. y)) ρ'),
    ('interpret a quote', 'interp ((^d. add (suc zero) (suc zero)) ρ)'),
    ('interpret: identity applied', 'interp ' + quote('(\\x. x) (\\y. y)')),
    ('interpret: two plus two', 'interp ' + quote('add (suc (suc zero)) (suc (suc zero))')),
    ('interpret: binders in suc', 'interp ' + quote('size suc')),
    ('interpret: open then read the body', 'interp ' + quote(f'ρ (\\x. x x) {BODY}')),
    ('interpret: open a shared copy twice', 'interp ' + quote(f'(\\g. g ρ (g zero)) (ρ (\\x y. x) {OPENED})')),
    ('interpret the interpreter', 'interp ' + quote('interp ' + quote('(\\x. x) (\\y. y)'))),
    ('interpret the interpreter, reading', 'interp ' + quote('interp ' + quote(f'ρ (\\x. x x) {BODY}'))),
    ('linear unary add: 2 + 3', 'linear_unary_add 2u 3u'),
    ('linear unary add, numbers built by lsuc', 'linear_unary_add (lsuc (lsuc lzero)) (lsuc 2u)'),
    ('ordinary unary add: 2 + 3', 'ordinary_unary_add 2s 3s'),
    ('linear list add: 3 + 1', 'linear_list_add 3 1'),
    ('linear list add: 5 + 6', 'linear_list_add 5 6'),
    ('linear list add, numbers built by cell', 'linear_list_add (cell b1 (cell b0 wend)) (cell b1 (cell b1 wend))'),
    ('linear list add, two shapes: 5 + 6', 'linear_list_add_two_shapes 5a 6p'),
    ('ordinary list add: 5 + 6', 'ordinary_list_add 5c 6c'),
    ('linear tree add: 3 + 1', 'linear_tree_add 3t 1t'),
    ('linear tree add: 1014 + 2413', 'linear_tree_add 1014t 2413t'),
    ('linear tree add, numbers built by pair', 'linear_tree_add (pair (pair b1 b0) (\\s z. s z)) (pair (pair b1 b1) (\\s z. s z))'),
    ('ordinary tree add: 1014 + 2413', 'ordinary_tree_add 1014t 2413t'),
    ('linear parallel tree add: 3 + 1', 'linear_tree_add_parallel 3t 1t'),
    ('linear parallel tree add: 1014 + 2413', 'linear_tree_add_parallel 1014t 2413t'),
    ('ordinary parallel tree add: 1014 + 2413', 'ordinary_tree_add_parallel 1014t 2413t'),
    ('reversible tree add: 1014 + 2413', 'reversible_tree_add 1014t 2413t'),
    ('reversible tree subtract: 3427 - 1014', 'reversible_tree_sub 1014t 3427t'),
    ('reversible tree add, then subtract', 'reversible_tree_add 1014t 2413t reversible_tree_sub'),
    ('reversible parallel tree add: 1014 + 2413', 'reversible_tree_add_parallel 1014t 2413t'),
    ('reversible parallel tree subtract, then add', 'reversible_tree_sub_parallel 1014t 3427t reversible_tree_add_parallel'),
]


def main():
    rng = random.Random(7)
    tally = Counter()
    bad = []
    for family in ('plain', 'with ρ', 'readers'):
        for i in range(4000):
            size = rng.randint(3, 40)
            t = gen(rng, size, [], family != 'plain', family == 'readers')
            r = compare(t)
            outcome = run_ref(t)[0]
            tally[(family, outcome, 'skip' if r == 'skip' else 'ok' if r is None else 'BAD')] += 1
            if r not in (None, 'skip') and len(bad) < 5: bad.append((t, r))
    print('random programs:')
    for k in sorted(tally): print('  ', *k, tally[k])
    for t, r in bad: print('  MISMATCH', t, '\n  ', r)
    for name, src in TESTS:
        t = program(src)
        kind, want, betas = run_ref(t, 3000000)
        status, got, stats = run_net(t, None, 60000000)
        ok = kind == 'value' and status == 'done' and got == want and not stats['annihilated across two copies']
        others = [run_net(t, s, 60000000)[1] == got for s in (3, 4)]
        print(f"{'ok ' if ok and all(others) else 'BAD'} {name:44} ref {kind} {betas:6} β | net {status} {stats['interactions']:8} interactions"
              + ('' if ok else f'\n   net {got}\n   ref {want}'))


if __name__ == '__main__':
    main()
