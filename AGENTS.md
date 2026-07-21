# AGENTS.md

## Repo Guidance

This repo is rather messy and many parts are in a stage of fast iteration and minimal polish. The eventual goal is to make everything native disp to accomplish `GOALS.md`. Given the somewhat messy stage, it is likely important to be wary that many docs will be out of date, and probably deleted. If you accidentally read something you see is out of date, tell me and give me a sense of whether it should be deleted or not. My intuition is that most `md` files should eventually be deleted once implemented in code, and the code should be super nice to read as to not even need the `md` file. `typ` files should be updated to current repo state and cleaned up when possible.

Note about the goal: we should generally prioritize writing easy to read code that is non-performant, and then in the `.opt.disp` files, overwriting the existing definitions with proven-faster implementations.

Misc notes:
 - Work on `main` directly (no new branching) unless otherwise told.
 - Understand the guard mechanisms that handles `let` module-local assignments, `test`, and of course `guard` for reassignment.
 - Tests should be automatically profiled to get a sense of how much time is being spent on each test, and this should be reported back if there are any tests that are taking a long time to evaluate. Testing iteration speed is one of the most important things for disp and should be minimized when possible.
 - Additionally, we must make sure whenever running tests to run with max memory constraints in order to avoid out of memory errors with the system or the terminal crashes / is killed.
 - If you feel that you are doing things too manually and there might be a faster / less-context-consuming way of doing something, let me know in a dedicated section towards the end of your response `*Automation Opportunity:*`.

## Writing Style

Summarize plain and condensed: no flourish, and assume I've forgotten the project codenames, replace each one with an everyday phrase or gloss it in parentheses on first use. Try to use colloquial / easy-to-understand phrasing.

### Comments
Ideally the code should be as little-commented as possible and should be as self-explanatory as possible. However many code paths *are* rather obtuse, thus it is ok to write doc comments (`///`) that are one-liners.

## This File

This file should not be directly edited by any AI. Instead, if I imply that there is a process issue or we come across something that future AIs should be careful not to pick up on, add a section at the end of your latest response: "*AGENTS.md Update?*: <...>" detailing what you think should be changed about the AGENTS.md. These changes should be as minimal as possible, and match the style of this document.