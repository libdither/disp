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
#   3. classify: OK | BLOCKED (403/429) | NOT-FOUND | ERROR, plus the content type; a dead
#      link (NOT-FOUND, ERROR, or a BLOCKED plain URL) falls back to the Wayback Machine's
#      latest good capture, printed as `ARCHIVE: <url>` so a doc can link that instead;
#   4. extract text (pdftotext for PDFs, tesseract OCR for pages without a text layer,
#      tag-stripping for HTML/markdown) and print the title;
#   5. with -q, find the quote (case- and whitespace-insensitive, `...` matches a gap; both
#      sides fold ligatures, curly quotes, dashes, soft hyphens, and words hyphenated across
#      lines) and print the fragment link: `<pdf-url>#page=N` for PDFs,
#      `<url>#<nearest-preceding-id>` for HTML, `<repo-url>#<heading-slug>` for READMEs.
# Downloads, extracted text, and OCR pages are cached under ~/.cache/verify-source/, so
# repeated lookups on one source do not refetch; -g searches that cached text to find the
# exact wording when a quote misses. Set S2_API_KEY to lift Semantic Scholar's rate limit.
#
# usage: scripts/verify-source.sh [-q 'quote'] [-g regex] [-n LINES] [-a] <url | doi | arxiv-id>
#        scripts/verify-source.sh -Q FILE      (each line `quote<TAB>source`; prints one ok/MISS line each)
#   -q  quote to locate (a distinctive 5–12 word phrase works best; `...` = any gap)
#   -g  list every passage matching a case-insensitive regex, with its page or anchor
#   -n  lines of extracted text to preview when no -q or -g is given (default 12)
#   -a  for arXiv ids: fetch the abs page (HTML) instead of the PDF
#   -Q  check every `quote<TAB>source` line of FILE (blank lines and `#` lines skipped); exits 1 on any miss
set -u
QUOTE="" GREP="" PREVIEW=12 ABS=0 BATCH=""
while getopts "q:g:n:aQ:" o; do
  case "$o" in
    q) QUOTE="$OPTARG" ;;
    Q) BATCH="$OPTARG" ;;
    g) GREP="$OPTARG" ;;
    n) PREVIEW="$OPTARG" ;;
    a) ABS=1 ;;
    *) exit 2 ;;
  esac
done
shift $((OPTIND - 1))
if [ -n "$BATCH" ]; then
  miss=0
  while IFS=$'\t' read -r q src || [ -n "$q" ]; do
    case "$q" in ''|'#'*) continue ;; esac
    out="$("$0" -q "$q" "$src" 2>&1)"
    if frag="$(grep -m1 '^MATCH:' <<< "$out")"; then
      printf 'ok    %s  %s\n' "${frag#*fragment: }" "$q"
    else
      miss=1; printf 'MISS  %s  %s  (%s)\n' "$src" "$q" "$(grep -m1 -E '^(STATUS|NO MATCH|HINT)' <<< "$out" | cut -c1-100)"
    fi
  done < "$BATCH"
  exit $miss
fi
[ $# -eq 1 ] || { echo "usage: $0 [-q quote] [-g regex] [-n lines] [-a] <url | doi | arxiv-id>" >&2; exit 2; }
ARG="$1"
CACHE="${XDG_CACHE_HOME:-$HOME/.cache}/verify-source"
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
# -g reruns skip resolution and read what an earlier run fetched for the same argument
REC="$CACHE/$(printf '%s %s' "$ARG" "$ABS" | sha1sum | cut -c1-16).src" RECBASE=""
if [ -n "$GREP" ] && [ -s "$REC" ]; then
  { read -r URL; read -r RECBASE; } < "$REC"; DOI="" ARXIV=""
  echo "CACHED: searching the text fetched earlier from $URL"
fi
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
  out="$(curl -sSL --compressed --connect-timeout 20 --max-time 90 --retry 1 -A "$UA" -c "$jar" -b "$jar" \
        -H 'Accept: text/html,application/xhtml+xml,application/pdf;q=0.9,*/*;q=0.8' \
        -o "$FILE" -w "$W" "$1" 2>/dev/null)"
  local rc=$?
  if [ $rc -eq 60 ] || [ $rc -eq 35 ]; then
    INSECURE=1
    out="$(curl -sSLk --connect-timeout 20 --max-time 90 -A "$UA" -c "$jar" -b "$jar" -o "$FILE" -w "$W" "$1" 2>/dev/null)"; rc=$?
  fi
  [ $rc -eq 0 ] || { CODE="000"; CTYPE="curl-exit-$rc"; FINAL="$1"; return 1; }
  IFS=$'\t' read -r CODE CTYPE FINAL <<< "$out"
  # some hosts block the browser agent and accept a plain one (WAF heuristics differ)
  if [ "$CODE" = 403 ] || [ "$CODE" = 429 ]; then
    out="$(curl -sSL --compressed --connect-timeout 20 --max-time 90 -A 'curl/8.0' -o "$FILE" -w "$W" "$1" 2>/dev/null)" && IFS=$'\t' read -r CODE CTYPE FINAL <<< "$out"
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

wayback() { # $1 = dead url — fetch the Wayback Machine's latest capture that answered 200
  local u ts saved
  u="$(printf '%s' "$1" | jq -sRr @uri)"; saved="$CACHE/$(printf '%s' "$1" | sha1sum | cut -c1-16).wayback"
  ts="$(cat "$saved" 2>/dev/null)"
  # the availability API is quick but misses many captures; the CDX index is slow but complete
  [ -n "$ts" ] || ts="$(curl -sS --max-time 30 "https://archive.org/wayback/available?url=$u" 2>/dev/null | jq -r '.archived_snapshots.closest | select(.status == "200") | .timestamp' 2>/dev/null)"
  [ -n "$ts" ] || ts="$(curl -sS --max-time 60 "https://web.archive.org/cdx/search/cdx?url=$u&filter=statuscode:200&fl=timestamp&limit=-1&fastLatest=true" 2>/dev/null | tail -n1)"
  case "$ts" in [12][0-9][0-9][0-9]*) ;; *) echo "ARCHIVE: the Wayback Machine has no good capture of $1"; return 1 ;; esac
  printf '%s\n' "$ts" > "$saved"
  URL="https://web.archive.org/web/${ts}id_/$1"; fetch "$URL"
  [ "$CODE" = 200 ] || { echo "ARCHIVE: Wayback Machine capture $ts did not load ($CODE)"; return 1; }
  ARCHIVED="https://web.archive.org/web/$ts/$1"
  echo "ARCHIVE: live copy unavailable; using the Wayback Machine capture of ${ts:0:4}-${ts:4:2}-${ts:6:2}: $ARCHIVED"
  echo "STATUS: OK ($CODE, $CTYPE) $FINAL"
}

ARCHIVED=""
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
      wayback "$URL" || echo "HINT: pass the DOI instead; Semantic Scholar may list an open-access copy"
    fi ;;
  404|410) echo "STATUS: NOT-FOUND ($CODE) $FINAL"; wayback "$URL" ;;
  *)       echo "STATUS: ERROR ($CODE $CTYPE) $FINAL"; wayback "$URL" ;;
esac
[ "$CODE" = "200" ] || exit 1

# --- 3–5. extract, title, quote ---------------------------------------------------
have() { # $1 = command, $2 = nixpkgs attribute — when not installed, borrow it from the nix store
  command -v "$1" >/dev/null && return 0
  local p; p="$(nix build --no-link --print-out-paths "nixpkgs#$2" 2>/dev/null)" || return 1
  PATH="$PATH:$(printf '%s/bin:' $p)"
  command -v "$1" >/dev/null
}
ocr_page() { # $1 = pdf, $2 = page, $3 = dir — tesseract's reading of one page, kept as $3/$2.txt
  [ -s "$3/$2.txt" ] && return 0
  # render at the scan's own resolution: resampling a 200 ppi bitonal scan to 300 costs words
  local ppi; ppi="$(pdfimages -list -f "$2" -l "$2" "$1" 2>/dev/null | awk 'NR > 2 && $13 > p { p = $13 } END { print p ? (p > 400 ? 400 : p < 150 ? 150 : p) : 300 }')"
  pdftoppm -f "$2" -l "$2" -r "${ppi:-300}" -gray -png -singlefile "$1" "$3/$2" 2>/dev/null &&
    tesseract "$3/$2.png" "$3/$2.part" >/dev/null 2>&1 && mv "$3/$2.part.txt" "$3/$2.txt"
  rm -f "$3/$2.png"
}
ocr() { # $1 = pdf, $2 = its pdftotext output, $3 = page cache — OCR the pages that have almost no text
  local n thin
  n="$(tr -cd '\f' < "$2" | wc -c)"
  thin="$(awk -v n="$n" 'BEGIN { RS = "\f" } NR <= n { gsub(/[[:space:]]/, ""); if (length($0) < 200) print NR }' "$2")"
  [ -n "$thin" ] || return 0
  n="$(wc -w <<< "$thin")"
  have pdftoppm poppler-utils && have tesseract tesseract || { echo "OCR: $n pages have little or no text layer, and tesseract is unavailable"; return 0; }
  echo "OCR: $n pages have little or no text layer; reading them with tesseract (cached for later runs)"
  mkdir -p "$3"; export -f ocr_page
  printf '%s\n' $thin | OMP_THREAD_LIMIT=1 xargs -P 8 -I{} bash -c 'ocr_page "$@"' _ "$1" {} "$3"
  python -I - "$2" "$3" <<'PY'
import os, sys
textfile, d = sys.argv[1:3]
pages = open(textfile, encoding='utf-8', errors='replace').read().split('\f')
for i, pg in enumerate(pages):
    f = f'{d}/{i + 1}.txt'
    if os.path.exists(f):
        o = open(f, encoding='utf-8', errors='replace').read().replace('\f', '')
        if len(o.split()) > len(pg.split()): pages[i] = o
open(textfile, 'w', encoding='utf-8').write('\f'.join(pages))
PY
}

TEXT="$FILE.txt"
PDFTITLE=""
if [ "$CTYPE" = "application/pdf" ]; then
  KIND=pdf
  have pdftotext poppler-utils || { echo "ERROR: pdftotext not available"; exit 1; }
  if [ ! -s "$TEXT" ]; then
    pdftotext "$FILE" "$TEXT.part" 2>/dev/null || { echo "ERROR: pdftotext failed"; exit 1; }
    ocr "$FILE" "$TEXT.part" "$FILE.ocr"
    mv "$TEXT.part" "$TEXT"
  elif [ -d "$FILE.ocr" ]; then
    echo "OCR: $(ls "$FILE.ocr" | grep -c '\.txt$') pages with little or no text layer were read by tesseract (cached)"
  fi
  echo "PAGES: $(tr -cd '\f' < "$TEXT" | wc -c)"
  command -v pdfinfo >/dev/null && PDFTITLE="$(pdfinfo "$FILE" 2>/dev/null | sed -n 's/^Title: *//p')"
elif [ -z "$KIND" ]; then
  KIND=html
fi
[ -n "$FRAGBASE" ] || FRAGBASE="$FINAL"
# a raw `id_` capture keeps a PDF's #page=N working; an HTML capture reads better in the Wayback viewer
[ -n "$ARCHIVED" ] && [ "$KIND" != pdf ] && FRAGBASE="$ARCHIVED"
[ -n "$RECBASE" ] && FRAGBASE="$RECBASE"
printf '%s\n%s\n' "$URL" "$FRAGBASE" > "$REC"

python -I - "$KIND" "$FILE" "$TEXT" "$FRAGBASE" "$PREVIEW" "$QUOTE" "$PDFTITLE" "$GREP" <<'PY'
import re, sys
from html.parser import HTMLParser
kind, body, textfile, url, preview, quote, pdftitle, grep = sys.argv[1:9]
preview = int(preview)

LIG = {'\ufb00': 'ff', '\ufb01': 'fi', '\ufb02': 'fl', '\ufb03': 'ffi', '\ufb04': 'ffl', '\ufb05': 'st', '\ufb06': 'st'}
TYPO = str.maketrans({**dict.fromkeys('\u2018\u2019\u201a\u201b\u2032', "'"), **dict.fromkeys('\u201c\u201d\u201e\u201f\u2033', '"'), **dict.fromkeys('\u2010\u2011\u2012\u2013\u2014\u2015\u2212', '-')})
# a dash ending a line inside a word (OCR often reads hyphens as dashes), a soft hyphen, or a zero-width space is cut
TOK = re.compile(r'(?P<cut>(?<=\w)[-\u2010-\u2015\u00ad][ \t]*\n\s*(?=\w)|[\u00ad\u200b])|(?P<space>\s+)|(?P<lig>[\ufb00-\ufb06])'
                 r'|[^\s\u00ad\u200b\u2010-\u2015\ufb00-\ufb06-]+|.', re.S)

def normalize(s, at=None): # fold typography to plain text; `at` collects each output char's source offset
    out = []
    for m in TOK.finditer(s):
        k = m.lastgroup
        r = '' if k == 'cut' else ' ' if k == 'space' else LIG[m.group()] if k == 'lig' else m.group().translate(TYPO)
        out.append(r)
        if at is not None: at.extend(range(m.start(), m.end()) if k is None else [m.start()] * len(r))
    return ''.join(out)

def norm(s):
    return normalize(s).strip()

def read_text(path): # a page's text, decoded by the charset it declares (Shift_JIS, EUC-JP, …), else UTF-8
    raw = open(path, 'rb').read()
    m = re.search(rb'<meta[^>]+charset=["\']?([\w-]+)', raw[:8192], re.I)
    for enc in ([m.group(1).decode('ascii', 'replace')] if m else []) + ['utf-8']:
        try: return raw.decode(enc)
        except (LookupError, UnicodeDecodeError): pass
    return raw.decode('utf-8', 'replace')

def slug(h): # GitHub heading slug
    h = re.sub(r'[^\w\- ]', '', h.strip().lower())
    return re.sub(r' ', '-', h)

if kind == 'pdf':
    src = open(textfile, encoding='utf-8', errors='replace').read()
    pages = [normalize(pg) for pg in src.split('\f')]
    lines = [l for l in src.splitlines() if l.strip() and not l.lstrip().startswith('arXiv:')]
    print('TITLE:', norm(pdftitle) or (norm(' '.join(lines[:2]))[:160] if lines else '?'))
elif kind == 'md':
    src = read_text(body)
    heads = [(m.start(), slug(m.group(2))) for m in re.finditer(r'^(#{1,6})\s+(.+?)\s*#*$', src, re.M)]
    first = next((m.group(1) for m in re.finditer(r'^#{1,6}\s+(.+?)\s*#*$', src, re.M)), '?')
    print('TITLE:', norm(first)[:160])
    class _P: pass
    p = _P(); p.ids = heads
    # emphasis and code markers hide phrases like `_best-first_`; blank them, keeping offsets
    full = re.sub(r'[_*`]', ' ', src)
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
    p = P(); p.feed(read_text(body))
    full = src = ''.join(p.text)
    print('TITLE:', norm(p.title)[:160] or '?')
if kind != 'pdf':
    at = []; full = normalize(full, at)

GENERATED = re.compile(r'\d{4,}|[0-9a-f]{8,}|^ref-link|-d\d+e\d+')
def anchor(s): # nearest id at or before normalized offset s; one that looks generated only if no stable one precedes it
    o = at[min(s, len(at) - 1)] if at else 0
    a = stable = None
    for pos, i in p.ids:
        if pos > o: break
        a = i
        if not GENERATED.search(i): stable = i
    return stable or a

def quote_re(q):
    parts = [re.escape(norm(x)) for x in re.split(r'\.\.\.|…', q)]
    parts = [re.sub(r'\\\s', r'\\s+', x) for x in parts]
    parts = [x.replace(r'\ ', r'\s+') for x in parts]
    # a hyphen may have been cut at a line break (`two-\ndimensional` reads `twodimensional`)
    parts = [re.sub(r'(\\-)', r'-?\\s*', x) for x in parts]
    return re.compile(r'.{0,120}?'.join(parts), re.I | re.S)

if grep:
    try: rx = re.compile(norm(grep), re.I)
    except re.error: rx = re.compile(re.escape(norm(grep)), re.I)
    if kind == 'pdf':
        hits = [(f'page {i + 1}', pg, m) for i, pg in enumerate(pages) for m in rx.finditer(pg)]
    else:
        hits = [(f'#{a}' if (a := anchor(m.start())) else 'no anchor', full, m) for m in rx.finditer(full)]
    hits = [h for h in hits if h[2].end() > h[2].start()]
    print(f'GREP: /{grep}/ has {len(hits)} ' + ('match' if len(hits) == 1 else 'matches') + (', first 30 shown' if len(hits) > 30 else ''))
    for where, t, m in hits[:30]:
        s, e = m.span()
        print(f'  {where}: ...{t[max(0, s - 80):e + 80]}...')
    if not quote: sys.exit(0 if hits else 3)

if not quote:
    shown = 0
    for l in src.splitlines():
        if l.strip():
            print('  ', norm(l)[:200]); shown += 1
            if shown >= preview: break
    sys.exit(0)

rx = quote_re(quote)
miss = "try a shorter distinctive phrase, or -g 'word' to see how the source words it"
if kind == 'pdf':
    hits = [(i + 1, m) for i, pg in enumerate(pages) for m in [rx.search(pg)] if m]
    if not hits:
        print(f'NO MATCH: quote not found in PDF text ({miss})'); sys.exit(3)
    base = url.split('#')[0]
    for n, m in hits[:3]:
        pg = pages[n - 1]; s, e = m.span()
        print(f'MATCH: page {n}  fragment: {base}#page={n}')
        print('   ...' + pg[max(0, s - 100):e + 100] + '...')
else:
    m = rx.search(full)
    if not m:
        print(f'NO MATCH: quote not found in page text ({miss})'); sys.exit(3)
    s, e = m.span()
    a = anchor(s)
    base = url.split('#')[0]
    print('MATCH: ' + (f'anchor #{a}  fragment: {base}#{a}' if a else 'no id attribute precedes the match; link the page itself'))
    print('   ...' + full[max(0, s - 100):e + 100] + '...')
PY
