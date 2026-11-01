"""How many wires must cross if every live @ has its own wire from a program part to a data part?
Program and data are laid by one function at even offsets, so they have the same handedness. Walk the outline of the two
trees clockwise: program leaves come out in reverse-reading order, then data leaves in reverse-reading order. Each @ joins
program leaf i to data leaf i. Two chords on a circle cross iff their ends alternate. Run: python chem/proto_planar.py"""
def crossings(pairs):
    return sum(1 for i in range(len(pairs)) for j in range(i + 1, len(pairs))
               if (pairs[i][0] < pairs[j][0] < pairs[i][1]) != (pairs[i][0] < pairs[j][1] < pairs[i][1]))
print('leaves  same hand (the law)  mirrored (not allowed)  matter cells at 6 per node, both trees')
for k in range(1, 8):
    n = 2 ** k
    same = [(i, n + i) for i in range(n)]          # p1..pn then d1..dn around the circle
    mirr = [(i, 2 * n - 1 - i) for i in range(n)]  # p1..pn then dn..d1
    print('%-7d %-20d %-23d %d' % (n, crossings(same), crossings(mirr), 2 * 6 * (2 * n - 1)))
