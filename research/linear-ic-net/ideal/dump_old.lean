import Tables
/-! Prints the reflective-nets rule table (`Tables.lean`) as JSON lines for `score.py`. -/
open K End

def kname (k : K) : String := (reprStr k).replace "K." ""
def endJ : End → String
  | .left n => s!"[\"L\",{n}]"
  | .right n => s!"[\"R\",{n}]"
  | .new j n => s!"[\"N\",{j},{n}]"
def polJ (p : Pol) : String := if p == .i then "\"i\"" else "\"o\""

#eval do
  for k in K.all do
    IO.println s!"\{\"kind\":\"{kname k}\",\"ports\":[{",".intercalate (k.ports.map polJ)}]}"
  for r in table do
    let made := ",".intercalate (r.made.map fun k => s!"\"{kname k}\"")
    let ws := ",".intercalate (r.wires.map fun (w : End × End) => s!"[{endJ w.1},{endJ w.2}]")
    IO.println s!"\{\"a\":\"{kname r.a}\",\"b\":\"{kname r.b}\",\"made\":[{made}],\"wires\":[{ws}]}"
  for p in stuck do
    IO.println s!"\{\"stuck\":[\"{kname p.1}\",\"{kname p.2}\"]}"
