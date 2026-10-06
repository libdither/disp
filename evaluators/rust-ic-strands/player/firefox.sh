#!/usr/bin/env bash
# firefox-check.html in headless Firefox ($FIREFOX, else firefox or floorp on the path), whose
# WebGPU reads buffers back on a timer of about 100 ms, unlike Dawn's (dawn.sh):
#   player/firefox.sh check 'src=sort:1'            gpu-check.js's check on Firefox's WebGPU
#   player/firefox.sh player 'p=disp:add&a=3 4'     the player from clock 0, on the CPU then the GPU
# Each run has a profile of its own; the page posts what it finds to a small server here. Exits
# non-zero unless the last line starts with "ok".
set -euo pipefail
here="$(cd "$(dirname "$0")" && pwd)"
ff=${FIREFOX:-$(command -v firefox || command -v floorp || true)}
[ -n "$ff" ] || { echo "FAIL no firefox or floorp on the path (set FIREFOX)"; exit 2; }
mode=${1:-check} hash=${2:-}
work=$(mktemp -d)
srv='' browser=''
trap 'kill $srv $browser 2>/dev/null; rm -rf "$work"' EXIT
mkdir "$work/profile"
cat > "$work/profile/user.js" <<'EOF'
user_pref("dom.webgpu.enabled", true);
user_pref("gfx.webgpu.ignore-blocklist", true);
user_pref("browser.shell.checkDefaultBrowser", false);
user_pref("browser.sessionstore.resume_from_crash", false);
user_pref("browser.startup.page", 0);
user_pref("datareporting.policy.dataSubmissionEnabled", false);
EOF
touch "$work/report"
port=$(python -c 'import socket; s = socket.socket(); s.bind(("127.0.0.1", 0)); print(s.getsockname()[1])')
python - "$here" "$port" "$work/report" <<'EOF' &
import http.server, sys
root, port, report = sys.argv[1], int(sys.argv[2]), sys.argv[3]
class Handler(http.server.SimpleHTTPRequestHandler):
    def __init__(self, *a, **k): super().__init__(*a, directory=root, **k)
    def end_headers(self):
        self.send_header("Cache-Control", "no-store")
        super().end_headers()
    def do_POST(self):
        body = self.rfile.read(int(self.headers.get("Content-Length", 0)))
        with open(report, "ab") as f: f.write(body + b"\n")
        self.send_response(200)
        self.end_headers()
    def log_message(self, *a): pass
http.server.ThreadingHTTPServer(("127.0.0.1", port), Handler).serve_forever()
EOF
srv=$!
until python -c "import socket; socket.create_connection(('127.0.0.1', $port))" 2>/dev/null; do sleep 0.1; done
"$ff" --headless --no-remote --profile "$work/profile" "http://127.0.0.1:$port/firefox-check.html#mode=$mode&$hash" >/dev/null 2>&1 &
browser=$!
# Print the page's lines as they come, until the last one.
shown=0
while :; do
  n=$(wc -l < "$work/report")
  if [ "$n" -gt "$shown" ]; then sed -n "$((shown + 1)),${n}p" "$work/report"; shown=$n; fi
  last=$(tail -1 "$work/report")
  case "$last" in ok*) exit 0 ;; FAIL*) exit 1 ;; esac
  kill -0 "$browser" 2>/dev/null || { echo "FAIL the browser quit"; exit 1; }
  sleep 0.5
done
