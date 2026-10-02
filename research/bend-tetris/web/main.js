// The browser's App.run. The game is ../tetris.bend, compiled to JavaScript by
// Bend's loader: every 1/60 s this hands `update` that frame's key presses, and
// paints each new `picture` (an Image quadtree) on the canvas. Bend decides
// everything; this file only carries keys in and pixels out.

import Tetris from "../tetris.bend";

const STEP = 1000 / 60;
const canvas = document.getElementById("screen");
const ctx = canvas.getContext("2d");
const veil = document.getElementById("veil");

// Bend values in JavaScript: a constructor is {$: "Name", ...fields}, U32 a number.
const list = (items) => items.reduceRight((tail, head) => ({ $: "Con", head, tail }), { $: "Nil" });
const press = (code) => ({ $: "Key", code, down: true });
const seed = () => crypto.getRandomValues(new Uint32Array(1))[0];

// The key codes of Bend's native window: arrows from 63232, letters and space as ASCII.
const ARROWS = { ArrowUp: 63232, ArrowDown: 63233, ArrowLeft: 63234, ArrowRight: 63235 };
const codeOf = (key) => ARROWS[key] ?? (key.length === 1 ? key.toLowerCase().charCodeAt(0) : null);

let game = Tetris.start(seed());
let keys = [];
let playing = false;
let last = performance.now();
let behind = 0;
let shown = "";
let frames = 0;

// Squares snap to whole device pixels, so the picture stays crisp at any size.
function paint(image, x, y, size) {
  if (image.$ === "Pix") {
    const [x0, y0] = [Math.round(x), Math.round(y)];
    ctx.fillStyle = "#" + image.color.toString(16).padStart(6, "0");
    ctx.fillRect(x0, y0, Math.round(x + size) - x0, Math.round(y + size) - y0);
    return 1;
  }
  const h = size / 2;
  return paint(image.tl, x, y, h) + paint(image.tr, x + h, y, h) + paint(image.bl, x, y + h, h) + paint(image.br, x + h, y + h, h);
}

// Everything the picture shows except the board, which only changes when a piece
// locks, and that also brings in a new piece.
const looks = ({ piece, next, score, lines, mode }) => JSON.stringify([piece, next, score, lines, mode.$]);

function fit() {
  const size = Math.round(canvas.getBoundingClientRect().width * devicePixelRatio);
  if (size === canvas.width) return;
  canvas.width = canvas.height = size;
  shown = "";
}

function draw() {
  const started = performance.now();
  const image = Tetris.picture(game);
  const ms = performance.now() - started;
  const squares = paint(image, 0, 0, canvas.width);
  frames++;
  document.getElementById("m-ms").textContent = `${ms.toFixed(1)} ms`;
  document.getElementById("m-squares").textContent = squares.toLocaleString();
  document.getElementById("m-frames").textContent = frames.toLocaleString();
}

function frame(now) {
  behind = playing ? Math.min(behind + now - last, 100) : 0;
  last = now;
  while (behind >= STEP) {
    const next = Tetris.update(list(keys.map(press)), game);
    game = next.$ === "Some" ? next.value : Tetris.start(seed());
    keys = [];
    behind -= STEP;
  }
  const seen = looks(game);
  if (seen !== shown) {
    shown = seen;
    draw();
  }
  requestAnimationFrame(frame);
}

function play() {
  playing = true;
  veil.hidden = true;
}

function hold(title, text) {
  playing = false;
  document.getElementById("veil-title").textContent = title;
  document.getElementById("veil-text").textContent = text;
  veil.hidden = false;
}

document.addEventListener("keydown", (event) => {
  const code = codeOf(event.key);
  if (code == null || event.ctrlKey || event.metaKey || event.altKey) return;
  event.preventDefault();
  if (!playing) play();
  keys.push(code);
});
document.getElementById("stage").addEventListener("pointerdown", play);
window.addEventListener("blur", () => hold("Paused", "Click to keep playing."));

// Touch controls send the same codes as keys; moves repeat while held.
let repeat = null;
for (const button of document.querySelectorAll("#pad button")) {
  const code = Number(button.dataset.code);
  button.addEventListener("pointerdown", (event) => {
    event.preventDefault();
    play();
    keys.push(code);
    if (!("repeat" in button.dataset)) return;
    const again = (delay) => { repeat = setTimeout(() => { keys.push(code); again(60); }, delay); };
    again(180);
  });
  for (const type of ["pointerup", "pointerleave", "pointercancel"]) button.addEventListener(type, () => clearTimeout(repeat));
}

new ResizeObserver(fit).observe(canvas);
hold("Click to play", "Arrow keys move and rotate, space drops.");
requestAnimationFrame(frame);
