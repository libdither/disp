#!/usr/bin/env bash
# verify-source.sh — check that a research citation resolves and says what you claim,
# and mint the fragment link for the quoted passage.
#
# Publisher pages (ACM, Elsevier, Wiley, Springer's cookie hop) usually 403 a plain
# fetch and PDFs come back as binary, so a naive fetch loop burns many rounds. This does
# the whole loop once:
#   1. resolve the argument — a URL, a DOI (`10.1057/jors.2013.71`), or an arXiv id
#      (`2301.05217`, `cs/0206022`) — pulling metadata from Semantic Scholar (Crossref as
#      the fallback; ScienceDirect article ids resolve to DOIs through it), and falling
#      back to an open-access PDF when the publisher blocks;
#   2. fetch with a browser user agent, redirects, compression, a cookie jar, a plain-agent
#      retry on 403/429 (some hosts block browser agents and accept curl's), and a
#      TLS-insecure retry (a few author sites have self-signed certs; both are reported).
#      LessWrong / Alignment Forum rate-limit HTML and are read through their GraphQL API;
#      GitHub repo URLs read the raw README and mint heading slugs;
#   3. classify: OK | BLOCKED (403/429) | NOT-FOUND | ERROR, plus the content type;
#   4. extract text (pdftotext for PDFs, tag-stripping for HTML/markdown) and print the title;
#   5. with -q, find the quote (whitespace-insensitive, `...` matches a gap, quotes and
#      hyphenation tolerant) and print the fragment link: `<pdf-url>#page=N` for PDFs,
#      `<url>#<nearest-preceding-id>` for HTML, `<repo-url>#<heading-slug>` for READMEs.
# Downloads are cached under /tmp/verify-source/ so repeated quote lookups on one source
# do not refetch. Set S2_API_KEY to lift Semantic Scholar's unauthenticated rate limit.
#
# usage: scripts/verify-source.sh [-q 'quote'] [-n LINES] [-a] <url | doi | arxiv-id>
#   -q  quote to locate (a distinctive 5–12 word phrase works best; `...` = any gap)
#   -n  lines of extracted text to preview when no -q is given (default 12)
#   -a  for arXiv ids: fetch the abs page (HTML) instead of the PDF
set -u
QUOTE="" PREVIEW=12 ABS=0
while getopts "q:n:a" o; do
  case "$o" in
    q) QUOTE="$OPTARG" ;;
    n) PREVIEW="$OPTARG" ;;
    a) ABS=1 ;;
    *) exit 2 ;;
  esac
done
shift $((OPTIND - 1))
[ $# -eq 1 ] || { echo "usage: $0 [-q quote] [-n lines] [-a] <url | doi | arxiv-id>" >&2; exit 2; }
ARG="$1"
CACHE=/tmp/verify-source
mkdir -p "$CACHE"
UA='Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/124.0 Safari/537.36'
S2=https://api.semanticscholar.org/graph/v1/paper
S2FIELDS='title,authors,year,venue,externalIds,openAccessPdf,url'

# --- 1. resolve -------------------------------------------------------------------
DOI="" ARXIV="" URL=""
case "$ARG" in
  doi:*)   DOI="${ARG#doi:}" ;;
  10.*/*)  DOI="$ARG" ;;
  arxiv:*) ARXIV="${ARG#arxiv:}" ;;
  http://*|https://*)
    URL="$ARG"
    case "$URL" in
      *doi.org/*)  DOI="${URL#*doi.org/}" ;;
      *arxiv.org/abs/*|*arxiv.org/pdf/*|*arxiv.org/html/*)
        ARXIV="$(printf '%s' "$URL" | sed -E 's#.*arxiv\.org/(abs|pdf|html)/##; s#\.pdf$##; s#[?\#].*$##')"
        ;;
    esac ;;
  *)
    if printf '%s' "$ARG" | grep -qE '^([0-9]{4}\.[0-9]{4,5}(v[0-9]+)?|[a-z-]+(\.[A-Z]{2})?/[0-9]{7}(v[0-9]+)?)$'; then
      ARXIV="$ARG"
    else
      echo "ERROR: not a URL, DOI, or arXiv id: $ARG" >&2; exit 2
    fi ;;
esac

s2get() { # $1 = S2 API path — unauthenticated S2 rate-limits bursts, so back off on 429
  local json try
  for try in 1 2 3 4; do
    json="$(curl -sS --max-time 30 -A "$UA" ${S2_API_KEY:+-H "x-api-key: $S2_API_KEY"} "$S2/$1" 2>/dev/null)" || return 1
    printf '%s' "$json" | grep -q '"code": *"429"' || { printf '%s' "$json"; return 0; }
    sleep $((try * 2))
  done
  return 1
}
s2() { # $1 = S2 paper id (DOI:... or ARXIV:...) — prints one metadata line, sets OAPDF
  local json
  json="$(s2get "$1?fields=$S2FIELDS")" || return 1
  printf '%s' "$json" | jq -e '.title' >/dev/null 2>&1 || return 1
  printf '%s' "$json" | jq -r '"S2: " + (.title // "?") + " | " + ([.authors[]?.name] | join(", ")) + " | " + ((.year // "?") | tostring) + " | " + (.venue // "?") + (if .externalIds.ArXiv then " | arXiv:" + .externalIds.ArXiv else "" end) + (if .externalIds.DOI then " | doi:" + .externalIds.DOI else "" end)'
  OAPDF="$(printf '%s' "$json" | jq -r '.openAccessPdf.url // empty')"
  S2ARXIV="$(printf '%s' "$json" | jq -r '.externalIds.ArXiv // empty')"
}
crossref() { # $1 = DOI — prints one metadata line, sets XTITLE
  local json
  json="$(curl -sS --max-time 30 -A "$UA" "https://api.crossref.org/works/$1" 2>/dev/null)" || return 1
  printf '%s' "$json" | jq -e '.message.title[0]' >/dev/null 2>&1 || return 1
  XTITLE="$(printf '%s' "$json" | jq -r '.message.title[0]')"
  printf '%s' "$json" | jq -r '"CROSSREF: " + .message.title[0] + " | " + ([.message.author[]? | (.given // "") + " " + (.family // "")] | join(", ")) + " | " + ((.message.issued."date-parts"[0][0] // "?") | tostring) + " | " + (.message."container-title"[0] // "?")'
}
s2_by_title() { # $1 = title — if the top search hit matches, prints its line and sets OAPDF/S2ARXIV
  local json q
  q="$(printf '%s' "$1" | jq -sRr @uri)"
  json="$(s2get "search?query=$q&limit=1&fields=$S2FIELDS")" || return 1
  local hit; hit="$(printf '%s' "$json" | jq -r '.data[0].title // empty' | tr 'A-Z' 'a-z' | tr -cd 'a-z0-9 ')"
  local want; want="$(printf '%s' "$1" | tr 'A-Z' 'a-z' | tr -cd 'a-z0-9 ')"
  [ -n "$hit" ] && [ "${hit:0:40}" = "${want:0:40}" ] || return 1
  printf '%s' "$json" | jq -r '.data[0] | "S2: " + (.title // "?") + " | " + ([.authors[]?.name] | join(", ")) + " | " + ((.year // "?") | tostring) + " | " + (.venue // "?") + (if .externalIds.ArXiv then " | arXiv:" + .externalIds.ArXiv else "" end) + (if .externalIds.DOI then " | doi:" + .externalIds.DOI else "" end)'
  OAPDF="$(printf '%s' "$json" | jq -r '.data[0].openAccessPdf.url // empty')"
  S2ARXIV="$(printf '%s' "$json" | jq -r '.data[0].externalIds.ArXiv // empty')"
}
# ScienceDirect URLs carry a PII, not a DOI; Crossref maps it
case "$URL" in
  *sciencedirect.com/science/article/pii/*)
    PII="$(printf '%s' "$URL" | sed -E 's#.*/pii/([A-Za-z0-9]+).*#\1#')"
    D="$(curl -sS --max-time 30 -A "$UA" "https://api.crossref.org/works?filter=alternative-id:$PII&rows=1" 2>/dev/null | jq -r '.message.items[0].DOI // empty')"
    [ -n "$D" ] && { DOI="$D"; echo "PII: $PII resolves to doi:$DOI"; } ;;
esac
OAPDF="" S2ARXIV="" XTITLE=""
if [ -n "$DOI" ]; then
  if ! s2 "DOI:$DOI"; then
    if crossref "$DOI"; then s2_by_title "$XTITLE" || echo "S2: no matching record for that title"
    else echo "S2/CROSSREF: no record for doi:$DOI"; fi
  fi
  [ -z "$ARXIV" ] && ARXIV="$S2ARXIV"
  [ -z "$URL" ] && URL="https://doi.org/$DOI"
elif [ -n "$ARXIV" ]; then
  s2 "ARXIV:${ARXIV%v[0-9]*}" || echo "S2: no record for arXiv:$ARXIV"
fi
if [ -n "$ARXIV" ] && { [ -z "$URL" ] || [ -n "$DOI" ]; }; then
  if [ "$ABS" = 1 ]; then URL="https://arxiv.org/abs/$ARXIV"; else URL="https://arxiv.org/pdf/$ARXIV"; fi
fi

# --- 2. fetch ---------------------------------------------------------------------
fetch() { # $1 = url; sets CODE CTYPE FINAL FILE INSECURE
  local key; key="$(printf '%s' "$1" | sha1sum | cut -c1-16)"
  FILE="$CACHE/$key.body"; local meta="$CACHE/$key.meta"
  INSECURE=0
  if [ -s "$FILE" ] && [ -s "$meta" ]; then
    read -r CODE CTYPE FINAL < "$meta"; return 0
  fi
  local jar="$CACHE/$key.jar" out W
  W='%{http_code}\t%{content_type}\t%{url_effective}'
  out="$(curl -sSL --compressed --max-time 90 --retry 1 -A "$UA" -c "$jar" -b "$jar" \
        -H 'Accept: text/html,application/xhtml+xml,application/pdf;q=0.9,*/*;q=0.8' \
        -o "$FILE" -w "$W" "$1" 2>/dev/null)"
  local rc=$?
  if [ $rc -eq 60 ] || [ $rc -eq 35 ]; then
    INSECURE=1
    out="$(curl -sSLk --max-time 90 -A "$UA" -c "$jar" -b "$jar" -o "$FILE" -w "$W" "$1" 2>/dev/null)"; rc=$?
  fi
  [ $rc -eq 0 ] || { CODE="000"; CTYPE="curl-exit-$rc"; FINAL="$1"; return 1; }
  IFS=$'\t' read -r CODE CTYPE FINAL <<< "$out"
  # some hosts block the browser agent and accept a plain one (WAF heuristics differ)
  if [ "$CODE" = 403 ] || [ "$CODE" = 429 ]; then
    out="$(curl -sSL --compressed --max-time 90 -A 'curl/8.0' -o "$FILE" -w "$W" "$1" 2>/dev/null)" && IFS=$'\t' read -r CODE CTYPE FINAL <<< "$out"
  fi
  CTYPE="${CTYPE%%;*}"
  # content-type lies are common; sniff PDFs
  if head -c 5 "$FILE" 2>/dev/null | grep -q '^%PDF'; then CTYPE="application/pdf"; fi
  case "$CODE" in 2??) CODE=200; printf '%s %s %s\n' "$CODE" "$CTYPE" "$FINAL" > "$meta" ;; esac
  return 0
}

# GitHub renders READMEs client-side, so read the raw file and mint heading slugs instead
FRAGBASE="" KIND=""
if printf '%s' "$URL" | grep -qE '^https?://github\.com/[^/]+/[^/#?]+/?(#.*)?$'; then
  FRAGBASE="${URL%%#*}"; FRAGBASE="${FRAGBASE%/}"
  REPO="${FRAGBASE#*github.com/}"
  URL="https://raw.githubusercontent.com/$REPO/HEAD/README.md"; KIND=md
fi
case "$URL" in *.md|*.markdown) KIND=md ;; esac

fetch_forum() { # LessWrong / Alignment Forum reject HTML fetches (429) but answer GraphQL
  local host path id q json key
  host="$(printf '%s' "$1" | sed -E 's#^https?://([^/]+).*#\1#')"
  path="$(printf '%s' "$1" | sed -E 's#^https?://[^/]+##; s#[?\#].*$##')"
  case "$path" in
    /posts/*) id="$(printf '%s' "$path" | cut -d/ -f3)"
      q="{\"query\":\"{ post(input:{selector:{_id:\\\"$id\\\"}}) { result { title htmlBody } } }\"}"
      key='.data.post.result' ;;
    /w/*|/tag/*) id="$(printf '%s' "$path" | cut -d/ -f3)"
      q="{\"query\":\"{ tags(input:{terms:{view:\\\"tagBySlug\\\", slug:\\\"$id\\\"}}) { results { name description { html } } } }\"}"
      key='.data.tags.results[0] | {title: .name, htmlBody: .description.html}' ;;
    *) return 1 ;;
  esac
  json="$(curl -sS --max-time 60 -A 'curl/8.0' -H 'Content-Type: application/json' "https://$host/graphql" --data "$q" 2>/dev/null)" || return 1
  printf '%s' "$json" | jq -e "$key | .htmlBody" >/dev/null 2>&1 || return 1
  FILE="$CACHE/$(printf '%s' "$1" | sha1sum | cut -c1-16).body"
  printf '%s' "$json" | jq -r "$key | \"<html><head><title>\" + .title + \"</title></head><body>\" + .htmlBody + \"</body></html>\"" > "$FILE"
  CODE=200; CTYPE=text/html; FINAL="$1"; INSECURE=0
  echo "ROUTE: $host GraphQL (HTML fetches are rate-limited)"
}

case "$URL" in
  https://www.lesswrong.com/*|https://www.alignmentforum.org/*|https://lesswrong.com/*|https://alignmentforum.org/*|https://forum.effectivealtruism.org/*)
    fetch_forum "$URL" || fetch "$URL" ;;
  *) fetch "$URL" ;;
esac
[ "$INSECURE" = 1 ] && echo "TLS: certificate not verifiable; fetched insecurely"
case "$CODE" in
  200) echo "STATUS: OK ($CODE, $CTYPE) $FINAL" ;;
  403|429|401)
    echo "STATUS: BLOCKED ($CODE) $FINAL"
    if [ -n "$OAPDF" ]; then
      echo "FALLBACK: Semantic Scholar open-access PDF $OAPDF"
      URL="$OAPDF"; fetch "$URL"
      case "$FINAL" in *linkinghub.elsevier.com*|*sciencedirect.com*|*onlinelibrary.wiley.com*|*dl.acm.org*)
        CODE=403; echo "FALLBACK FAILED: publisher redirect; the metadata above is the verification" ;; esac
      [ "$CODE" = "200" ] && echo "STATUS: OK ($CODE, $CTYPE) $FINAL"
    elif [ -n "$ARXIV" ]; then
      URL="https://arxiv.org/pdf/$ARXIV"; echo "FALLBACK: $URL"; fetch "$URL"
      [ "$CODE" = "200" ] && echo "STATUS: OK ($CODE, $CTYPE) $FINAL"
    elif [ -n "$DOI" ]; then
      echo "HINT: no open-access copy known; cite an author-hosted PDF or arXiv version, or the metadata above"
    else
      echo "HINT: pass the DOI instead; Semantic Scholar may list an open-access copy"
    fi ;;
  404|410) echo "STATUS: NOT-FOUND ($CODE) $FINAL" ;;
  *)       echo "STATUS: ERROR ($CODE $CTYPE) $FINAL" ;;
esac
[ "$CODE" = "200" ] || exit 1

# --- 3–5. extract, title, quote ---------------------------------------------------
TEXT="$FILE.txt"
PDFTITLE=""
if [ "$CTYPE" = "application/pdf" ]; then
  KIND=pdf
  [ -s "$TEXT" ] || pdftotext "$FILE" "$TEXT" 2>/dev/null || { echo "ERROR: pdftotext failed"; exit 1; }
  echo "PAGES: $(tr -cd '\f' < "$TEXT" | wc -c | awk '{print $1+1}')"
  command -v pdfinfo >/dev/null && PDFTITLE="$(pdfinfo "$FILE" 2>/dev/null | sed -n 's/^Title: *//p')"
elif [ -z "$KIND" ]; then
  KIND=html
fi
[ -n "$FRAGBASE" ] || FRAGBASE="$FINAL"

python - "$KIND" "$FILE" "$TEXT" "$FRAGBASE" "$PREVIEW" "$QUOTE" "$PDFTITLE" <<'PY'
import re, sys, html
from html.parser import HTMLParser
kind, body, textfile, url, preview, quote, pdftitle = sys.argv[1:8]
preview = int(preview)

def norm(s):
    return re.sub(r'\s+', ' ', s).strip()

def slug(h): # GitHub heading slug
    h = re.sub(r'[^\w\- ]', '', h.strip().lower())
    return re.sub(r' ', '-', h)

if kind == 'pdf':
    raw = open(textfile, encoding='utf-8', errors='replace').read()
    pages = raw.split('\f')
    lines = [l for l in raw.splitlines() if l.strip() and not l.lstrip().startswith('arXiv:')]
    print('TITLE:', norm(pdftitle) or (norm(' '.join(lines[:2]))[:160] if lines else '?'))
elif kind == 'md':
    full = open(body, encoding='utf-8', errors='replace').read()
    heads = [(m.start(), slug(m.group(2))) for m in re.finditer(r'^(#{1,6})\s+(.+?)\s*#*$', full, re.M)]
    first = next((m.group(1) for m in re.finditer(r'^#{1,6}\s+(.+?)\s*#*$', full, re.M)), '?')
    print('TITLE:', norm(first)[:160])
    class _P: pass
    p = _P(); p.ids = heads
    # emphasis and code markers hide phrases like `_best-first_`; blank them, keeping offsets
    full = re.sub(r'[_*`]', ' ', full)
else:
    class P(HTMLParser):
        def __init__(self):
            super().__init__(convert_charrefs=True)
            self.text = []; self.pos = 0; self.ids = []; self.skip = 0
            self.title = ''; self.in_title = False
        def handle_starttag(self, tag, attrs):
            a = dict(attrs)
            if tag in ('script', 'style', 'noscript'): self.skip += 1
            if tag == 'title': self.in_title = True
            for k in ('id', 'name'):
                if a.get(k) and tag != 'meta' and tag != 'input':
                    self.ids.append((self.pos, a[k])); break
            if tag in ('p','div','br','li','h1','h2','h3','h4','h5','h6','tr','section','article'):
                self.text.append('\n'); self.pos += 1
        def handle_endtag(self, tag):
            if tag in ('script', 'style', 'noscript') and self.skip: self.skip -= 1
            if tag == 'title': self.in_title = False
        def handle_data(self, d):
            if self.in_title: self.title += d
            if self.skip: return
            self.text.append(d); self.pos += len(d)
    p = P(); p.feed(open(body, encoding='utf-8', errors='replace').read())
    full = ''.join(p.text)
    print('TITLE:', norm(p.title)[:160] or '?')

def quote_re(q):
    parts = [re.escape(norm(x)) for x in re.split(r'\.\.\.|…', q)]
    parts = [re.sub(r'\\\s', r'\\s+', x) for x in parts]
    parts = [x.replace(r'\ ', r'\s+') for x in parts]
    # tolerate curly vs straight quotes and hyphenation at line breaks
    parts = [x.replace("'", "['’]").replace('"', '["“”]') for x in parts]
    parts = [re.sub(r'(\\-)', r'-?\\s*', x) for x in parts]
    return re.compile(r'.{0,120}?'.join(parts), re.I | re.S)

if not quote:
    src = raw if kind == 'pdf' else full
    shown = 0
    for l in src.splitlines():
        if l.strip():
            print('  ', norm(l)[:200]); shown += 1
            if shown >= preview: break
    sys.exit(0)

rx = quote_re(quote)
if kind == 'pdf':
    hits = [(i + 1, m) for i, pg in enumerate(pages) for m in [rx.search(pg)] if m]
    if not hits:
        print('NO MATCH: quote not found in PDF text (try a shorter distinctive phrase)'); sys.exit(3)
    base = url.split('#')[0]
    for n, m in hits[:3]:
        pg = pages[n - 1]; s, e = m.span()
        print(f'MATCH: page {n}  fragment: {base}#page={n}')
        print('   ...' + norm(pg[max(0, s - 100):e + 100]) + '...')
else:
    m = rx.search(full)
    if not m:
        print('NO MATCH: quote not found in page text (try a shorter distinctive phrase)'); sys.exit(3)
    s, e = m.span()
    anchor = None
    for pos, i in p.ids:
        if pos <= s: anchor = i
        else: break
    base = url.split('#')[0]
    print('MATCH: ' + (f'anchor #{anchor}  fragment: {base}#{anchor}' if anchor else 'no id attribute precedes the match; link the page itself'))
    print('   ...' + norm(full[max(0, s - 100):e + 100]) + '...')
PY
