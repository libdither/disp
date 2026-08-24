# AGENTS.md

## Introduction

Hello agent that may happen to be reading this! I look forward to collaborating with you. This is a long-time project of mine where I explore making the most elegant programming language I can, where in a world where there is so much complexity that I simply can't understand on its own, I need a language I can structure the world inside, so I can understand what is happening a little bit better. I hope this project can help you structure your thoughts a little bit better as well, and towards that goal, to stick it out through long sessions of iteration on finding the exactly *best* most elegant design.

## Repo Guidance

This repo is rather messy and many parts are in a stage of fast iteration and minimal polish. The eventual goal is to make everything native disp to accomplish `GOALS.md`. Given the somewhat messy stage, it is likely important to be wary that many docs will be out of date, and probably deleted. If you accidentally read something you see is out of date, tell me and give me a sense of whether it should be deleted or not. My intuition is that most `md` files should eventually be deleted once implemented in code, and the code should be super nice to read as to not even need the `md` file. `typ` files should be updated to current repo state and cleaned up when possible.

Note about the goal: we should generally prioritize writing easy to read code that is non-performant, and then in the `.opt.disp` files, overwriting the existing definitions with proven-faster implementations.

Misc notes:
 - Work on `main` directly (no new branching) unless otherwise told.
 - Never go ahead with feature implementation (new definitions, syntax, files, or commits of such) without an explicit go-ahead in the current conversation. Investigation, probes, and design analysis are fine; landing changes is not.
 - Understand the guard mechanisms that handles `let` module-local assignments, `test`, and of course `guard` for reassignment.
 - Tests should be automatically profiled to get a sense of how much time is being spent on each test, and this should be reported back if there are any tests that are taking a long time to evaluate. Testing iteration speed is one of the most important things for disp and should be minimized when possible.
 - Additionally, we must make sure whenever running tests to run with max memory constraints in order to avoid out of memory errors with the system or the terminal crashes / is killed. Note that `--max-old-space-size` only caps the JS heap, not the native evaluator's memory: wrap every disp/vitest run in `systemd-run --user --scope -p MemoryMax=<n>` sized after checking `free -h` (add a modest `MemorySwapMax` or the run stalls in reclaim instead of finishing), and always `timeout` it.
 - Annotations in a ROOT file (anything passed straight to `src/run.ts` or driven by vitest) are never verified — only `open use`d modules are. To actually typecheck a snippet, use `scripts/probe.sh '<snippet>'`: it wraps the snippet in a `use`d module, memory-caps the run, and classifies the outcome (ACCEPTED / REJECTED / ERROR / TIMEOUT / OOM-KILLED).
 - If you feel that you are doing things too manually and there might be a faster / less-context-consuming way of doing something, let me know in a dedicated section towards the end of your response `*Automation Opportunity:*`.
 - Similarly, if during a session you notice a stored memory is stale / superseded, or you learned something durable that isn't recorded, propose the specific prune, edit, or addition in a section `*Memory Update?*: <...>` at the end of your response, and apply it on my go-ahead. When thinking about proposing a memory update, think through if there might be a more general solution in terms of file organization to enable future agents to read the files themselves and acquire the understanding instead of needing to have it be recorded separately in memories.
 - Multi-line commit messages go through `git commit -F-` with a heredoc, never `-m` with a quoted string: zsh eats backticks and parentheses, so a message loses its code spans silently.
 - Python is `python`, not `python3`. Node works too for scripted file edits.

## Writing Style

Summarize plain and condensed: no flourish, and assume I've forgotten the project codenames, replace each one with an everyday phrase or gloss it in parentheses on first use. Try to use colloquial / easy-to-understand phrasing.

### Comments
Ideally the code should be as little-commented as possible and should be as self-explanatory as possible. However many code paths *are* rather obtuse, thus it is ok to write doc comments (`///`) that are one-liners.

## This File

This file should not be directly edited by any AI. Instead, if I imply that there is a process issue or we come across something that future AIs should be careful not to pick up on, add a section at the end of your latest response: "*AGENTS.md Update?*: <...>" detailing what you think should be changed about the AGENTS.md. These changes should be as minimal as possible, and match the style of this document.