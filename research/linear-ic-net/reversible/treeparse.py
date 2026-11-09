"""Parse printed trees '(△ X Y)', '(△ X)', '△', 'NAME' into prototype terms, substituting names."""
def parse(s, env=None):
    env = env or {}
    toks = s.replace('(', ' ( ').replace(')', ' ) ').split()
    pos = [0]
    def node():
        t = toks[pos[0]]; pos[0] += 1
        if t == '△': return 'L'
        if t != '(':
            return env[t] if t in env else t
        assert toks[pos[0]] == '△'; pos[0] += 1
        kids = []
        while toks[pos[0]] != ')': kids.append(node())
        pos[0] += 1
        if len(kids) == 0: return 'L'
        if len(kids) == 1: return ('L', kids[0])
        assert len(kids) == 2
        return (('L', kids[0]), kids[1])
    r = node(); assert pos[0] == len(toks)
    return r
