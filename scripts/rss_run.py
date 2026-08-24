"""Run a command, pass its output through, then report peak RSS + wall time on stderr.

usage: python scripts/rss_run.py <cmd> [args...]     (exit status = the command's)
GNU time is not installed here; this is the portable stand-in probe.sh/experiment.sh use.
"""
import resource, subprocess, sys, time

t = time.time()
code = subprocess.call(sys.argv[1:])
wall = time.time() - t
rss_mb = resource.getrusage(resource.RUSAGE_CHILDREN).ru_maxrss // 1024
print(f"[rss] maxRSS={rss_mb}MB wall={wall:.1f}s exit={code}", file=sys.stderr)
sys.exit(code)
