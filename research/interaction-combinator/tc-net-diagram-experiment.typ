#import "@preview/cetz:0.4.2"

#set page(width: 11in, height: 8.5in, margin: 0.35in)
#set text(font: "New Computer Modern", size: 9pt)
#set par(justify: false)

#align(center)[
  #text(size: 16pt, weight: "bold")[TC-Net Interaction Reduction Sketches]
  #v(0.2em)
  #text(size: 9pt)[Lafont-style triangular agents. Filled ports are principal; open ports are auxiliary.]
]

#v(0.2em)

#cetz.canvas(length: 1cm, {
  import cetz.draw: *

  let agent(x, y, label, dir: "down", w: 1.05, h: 0.74) = {
    let pts = if dir == "down" {
      ((x - w / 2, y + h / 2), (x + w / 2, y + h / 2), (x, y - h / 2))
    } else {
      ((x - w / 2, y - h / 2), (x + w / 2, y - h / 2), (x, y + h / 2))
    }
    line(pts.at(0), pts.at(1), pts.at(2), close: true, stroke: 0.8pt, fill: white)
    content((x, y - 0.02), label)
  }

  let port(x, y, kind: "aux") = {
    if kind == "principal" {
      circle((x, y), radius: 0.055, fill: black, stroke: none)
    } else {
      circle((x, y), radius: 0.06, fill: white, stroke: 0.6pt)
    }
  }

  let wire(a, b, active: false) = {
    line(a, b, stroke: if active { 1.2pt } else { 0.5pt })
  }

  let arrow(x1, y, x2) = {
    line((x1, y), (x2, y), mark: (end: ">"), stroke: 0.75pt)
  }

  let txt(x, y, body, size: 8pt, weight: "regular") = {
    content((x, y), text(size: size, weight: weight, body))
  }

  let panel(x, y, w, h, title) = {
    rect((x, y), (x + w, y + h), stroke: 0.45pt + luma(150), radius: 3pt)
    txt(x + 0.15, y + h + 0.18, title, size: 9pt, weight: "bold")
  }

  let active_pair(x, y, top, bot) = {
    agent(x, y + 0.43, top, dir: "down")
    agent(x, y - 0.43, bot, dir: "up")
    port(x, y + 0.06, kind: "principal")
    port(x, y - 0.06, kind: "principal")
    wire((x, y + 0.06), (x, y - 0.06), active: true)
  }

  // Legend
  panel(0.1, 8.2, 3.7, 2.2, [Port notation])
  port(0.55, 9.85, kind: "principal")
  txt(1.2, 9.85, [principal])
  port(0.55, 9.45)
  txt(1.2, 9.45, [auxiliary])
  wire((0.32, 9.03), (0.92, 9.03), active: true)
  txt(1.35, 9.03, [active pair])
  wire((0.32, 8.62), (0.92, 8.62))
  txt(1.35, 8.62, [ordinary wire])

  // A x L -> S(c)
  panel(4.2, 8.2, 6.2, 2.2, [Apply leaf])
  active_pair(5.25, 9.12, [$A$], [$L$])
  port(4.73, 9.49)
  port(5.77, 9.49)
  wire((4.73, 9.49), (4.35, 9.8))
  wire((5.77, 9.49), (6.15, 9.8))
  txt(4.25, 9.95, [$c$])
  txt(6.28, 9.95, [$r$])
  arrow(6.45, 9.12, 7.35)
  agent(8.25, 9.12, [$S$], dir: "up")
  port(8.25, 9.49, kind: "principal")
  port(7.72, 8.75)
  wire((8.25, 9.49), (8.25, 9.88))
  wire((7.72, 8.75), (7.35, 8.48))
  txt(8.25, 10.02, [$r$])
  txt(7.2, 8.32, [$c$])

  // A x F -> T1
  panel(11.0, 8.2, 9.3, 2.2, [Apply fork])
  active_pair(12.05, 9.12, [$A$], [$F$])
  port(11.53, 9.49)
  port(12.57, 9.49)
  port(11.53, 8.75)
  port(12.57, 8.75)
  wire((11.53, 9.49), (11.15, 9.8))
  wire((12.57, 9.49), (12.95, 9.8))
  wire((11.53, 8.75), (11.15, 8.45))
  wire((12.57, 8.75), (12.95, 8.45))
  txt(11.02, 9.95, [$c$])
  txt(13.07, 9.95, [$r$])
  txt(11.0, 8.3, [$a$])
  txt(13.12, 8.3, [$b$])
  arrow(13.45, 9.12, 14.55)
  agent(15.75, 9.12, [$T_1$], dir: "down")
  port(15.75, 8.75, kind: "principal")
  port(15.23, 9.49)
  port(15.75, 9.49)
  port(16.27, 9.49)
  wire((15.75, 8.75), (15.75, 8.35), active: true)
  wire((15.23, 9.49), (14.9, 9.8))
  wire((15.75, 9.49), (15.75, 9.88))
  wire((16.27, 9.49), (16.6, 9.8))
  txt(15.75, 8.2, [$a$])
  txt(14.77, 9.95, [$b$])
  txt(15.75, 10.02, [$c$])
  txt(16.72, 9.95, [$r$])

  // T1 x S -> S rule fan-out
  panel(0.1, 4.0, 10.5, 3.5, [S-rule fan-out])
  active_pair(1.45, 5.65, [$T_1$], [$S$])
  port(0.92, 6.02)
  port(1.45, 6.02)
  port(1.98, 6.02)
  port(0.92, 5.28)
  wire((0.92, 6.02), (0.45, 6.35))
  wire((1.45, 6.02), (1.45, 6.45))
  wire((1.98, 6.02), (2.45, 6.35))
  wire((0.92, 5.28), (0.45, 4.95))
  txt(0.3, 6.5, [$b$])
  txt(1.45, 6.6, [$c$])
  txt(2.6, 6.5, [$r$])
  txt(0.3, 4.8, [$x$])
  arrow(2.75, 5.65, 3.75)
  agent(4.55, 6.25, [$delta$], dir: "down", w: 1.18)
  port(4.55, 5.88, kind: "principal")
  port(3.96, 6.62)
  port(5.14, 6.62)
  wire((4.55, 5.88), (4.55, 5.45), active: true)
  txt(4.55, 5.28, [$c$])
  agent(6.0, 6.5, [$A$], dir: "down")
  agent(6.0, 5.4, [$A$], dir: "down")
  agent(7.65, 5.95, [$A$], dir: "down")
  wire((5.14, 6.62), (5.48, 6.86))
  wire((3.96, 6.62), (5.48, 5.76))
  wire((6.52, 6.86), (7.14, 6.32))
  wire((6.52, 5.76), (7.14, 5.58))
  wire((8.17, 5.95), (8.8, 5.95))
  txt(5.25, 7.08, [$c_1$])
  txt(5.12, 5.55, [$c_2$])
  txt(5.82, 7.25, [$x$])
  txt(5.82, 4.95, [$b$])
  txt(8.98, 5.95, [$r$])

  // delta x F copies fork
  panel(11.0, 4.0, 9.3, 3.5, [Copy fork])
  active_pair(12.1, 5.65, [$delta$], [$F$])
  port(11.57, 5.28)
  port(12.63, 5.28)
  wire((11.57, 5.28), (11.05, 4.95))
  wire((12.63, 5.28), (13.15, 4.95))
  txt(10.88, 4.78, [$a$])
  txt(13.32, 4.78, [$b$])
  arrow(13.55, 5.65, 14.65)
  agent(15.55, 6.25, [$delta_a$], dir: "down", w: 1.22)
  agent(15.55, 5.05, [$delta_b$], dir: "down", w: 1.22)
  port(15.55, 5.88, kind: "principal")
  port(15.55, 4.68, kind: "principal")
  wire((15.55, 5.88), (15.55, 5.55), active: true)
  wire((15.55, 4.68), (15.55, 4.35), active: true)
  txt(15.55, 5.42, [$a$])
  txt(15.55, 4.18, [$b$])
  agent(17.45, 6.25, [$F_1$], dir: "up", w: 1.12)
  agent(17.45, 5.05, [$F_2$], dir: "up", w: 1.12)
  port(17.45, 6.62, kind: "principal")
  port(17.45, 5.42, kind: "principal")
  wire((16.16, 6.62), (16.9, 5.88))
  wire((16.16, 5.42), (16.9, 5.88))
  wire((16.16, 5.42), (16.9, 4.68))
  wire((16.16, 4.22), (16.9, 4.68))
  wire((17.45, 6.62), (17.45, 7.0))
  wire((17.45, 5.42), (17.45, 4.55))
  txt(17.45, 7.18, [$"delta.l"$])
  txt(17.45, 4.38, [$"delta.r"$])
})
