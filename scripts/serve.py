"""Serve a directory with listings and no browser caching, so a rebuilt page shows on reload.
A PUT saves a new file into a directory that holds an `.inbox` marker (a page's upload drop), and nowhere else.

python scripts/serve.py <host> <port> <dir>
"""
import sys
from functools import partial
from http.server import SimpleHTTPRequestHandler, ThreadingHTTPServer
from pathlib import Path

MAX_UPLOAD = 200 << 20


class NoCache(SimpleHTTPRequestHandler):
    def end_headers(self):
        self.send_header("Cache-Control", "no-store")
        super().end_headers()

    def do_PUT(self):
        path = Path(self.translate_path(self.path))
        size = int(self.headers.get("Content-Length") or 0)
        if not (path.parent / ".inbox").is_file() or path.name.startswith(".") or path.exists() or not 0 < size <= MAX_UPLOAD:
            return self.send_error(403)
        path.write_bytes(self.rfile.read(size))
        self.send_response(201)
        self.end_headers()


host, port, root = sys.argv[1], int(sys.argv[2]), sys.argv[3]
ThreadingHTTPServer((host, port), partial(NoCache, directory=root)).serve_forever()
