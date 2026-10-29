"""Serve a directory with listings and no browser caching, so a rebuilt page shows on reload.

python scripts/serve.py <host> <port> <dir>
"""
import sys
from functools import partial
from http.server import SimpleHTTPRequestHandler, ThreadingHTTPServer


class NoCache(SimpleHTTPRequestHandler):
    def end_headers(self):
        self.send_header("Cache-Control", "no-store")
        super().end_headers()


host, port, root = sys.argv[1], int(sys.argv[2]), sys.argv[3]
ThreadingHTTPServer((host, port), partial(NoCache, directory=root)).serve_forever()
