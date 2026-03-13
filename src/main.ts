#!/usr/bin/env node
import * as fs from "node:fs"
import { runRepl, initialState, loadFile } from "./repl.js"

const file = process.argv[2]

if (file) {
  // File execution mode
  const state = initialState()
  const result = loadFile(state, file)
  if (result) console.log(result)
} else {
  runRepl().catch(console.error)
}
