#!/usr/bin/env bash
# The always-on dev servers, systemd user services on the tailnet address (restarted on failure, started at boot):
#   disp-files  the main checkout's files, untracked ones too, with listings and no caching   port 5180
#   disp-site   the website's vite dev server                                                 port 5174
# no argument: each server's state and URL · install: write the units and (re)start both
# restart [unit] · logs [unit]
set -euo pipefail
root=$(cd "$(dirname "$0")/.." && pwd)
units="$HOME/.config/systemd/user"
name=$(tailscale status --json | python -c 'import json, sys; print(json.load(sys.stdin)["Self"]["DNSName"].rstrip("."))')
url() { echo "http://$name:$1/"; }

unit() { # name, description, memory cap, working dir, pre-start command, command, environment
  cat >"$units/$1.service" <<EOF
[Unit]
Description=$2
After=network-online.target

[Service]
Environment=PATH=%h/.nix-profile/bin:/etc/profiles/per-user/%u/bin:/run/current-system/sw/bin:/run/wrappers/bin
WorkingDirectory=$4
${7:+Environment=$7}
${5:+ExecStartPre=$5}
ExecStart=$6
Restart=always
RestartSec=10
MemoryMax=$3

[Install]
WantedBy=default.target
EOF
}

case "${1:-}" in
  install)
    host=$(tailscale ip -4 | head -1)
    mkdir -p "$units"
    unit disp-files "disp repo files (tailnet)" 256M "$root" "" "$(command -v python) $root/scripts/serve.py $host 5180 $root"
    unit disp-site "disp website dev server (tailnet)" 2G "$root/website" "$(command -v npm) run pages" \
      "$root/website/node_modules/.bin/vite dev --host $host --port 5174 --strictPort" "DISP_DEV_HOSTS=$(hostname),$name,disp.tn.zyancraft.net"
    systemctl --user daemon-reload
    systemctl --user enable disp-files disp-site
    systemctl --user restart disp-files disp-site
    exec "$0" ;;
  restart) systemctl --user restart ${2:-disp-files disp-site} ;;
  logs) journalctl --user -u "${2:-disp-site}" -n 50 --no-pager ;;
  "")
    for s in disp-files:5180 disp-site:5174; do
      printf '%-11s %-9s %s\n' "${s%:*}" "$(systemctl --user is-active "${s%:*}" || true)" "$(url "${s#*:}")"
    done ;;
  *) sed -n '2,6p' "$0" >&2; exit 1 ;;
esac
