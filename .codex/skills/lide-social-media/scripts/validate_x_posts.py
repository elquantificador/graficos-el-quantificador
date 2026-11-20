#!/usr/bin/env python3
"""Validate standard X posts against X's 280 weighted-character rule.

Usage:
    python validate_x_posts.py posts.json

Input JSON can be either:
    ["post one", "post two"]
or:
    {"posts": ["post one", "post two"]}

Implements X's published v3 weighted-count rules for text used by this skill:
- NFC normalization
- URLs count as 23
- emoji grapheme clusters count as 2
- Unicode ranges 0-4351, 8192-8205, 8208-8223, 8242-8247 count as 1
- other Unicode code points count as 2

The validator is intended for standard posts, not Premium long-form posts.
"""

from __future__ import annotations

import json
import re
import sys
import unicodedata
from pathlib import Path

try:
    import regex as uregex
except ImportError as exc:
    raise SystemExit("This validator requires the Python 'regex' package.") from exc

MAX_WEIGHTED = 280
URL_WEIGHT = 23

# Conservative URL matching suited to social copy. Put URLs on their own or followed by
# whitespace for maximum predictability.
URL_RE = re.compile(r"(?i)\b(?:https?://|www\.)[^\s]+")

ONE_WEIGHT_RANGES = (
    (0, 4351),
    (8192, 8205),
    (8208, 8223),
    (8242, 8247),
)

# Matches an extended grapheme cluster containing an emoji/pictograph. X counts an emoji
# sequence as weight 2 regardless of code-point length.
GRAPHEME_RE = uregex.compile(r"\X")
EP_RE = uregex.compile(r"\p{Extended_Pictographic}")
REGIONAL_RE = uregex.compile(r"\p{Regional_Indicator}")
KEYCAP_RE = uregex.compile(r"^[0-9#*]\ufe0f?\u20e3$")


def cp_weight(ch: str) -> int:
    cp = ord(ch)
    for start, end in ONE_WEIGHT_RANGES:
        if start <= cp <= end:
            return 1
    return 2


def grapheme_weight(cluster: str) -> int:
    if EP_RE.search(cluster) or REGIONAL_RE.search(cluster) or KEYCAP_RE.match(cluster):
        return 2
    return sum(cp_weight(ch) for ch in cluster)


def strip_trailing_url_punctuation(url: str) -> tuple[str, str]:
    """Separate common sentence punctuation from a URL match.

    This keeps punctuation outside the URL's fixed 23-character weight. Parentheses are
    preserved when balanced inside the URL.
    """
    trailing = ""
    while url and url[-1] in ".,;:!?":
        trailing = url[-1] + trailing
        url = url[:-1]
    # Remove a closing bracket only when it is unmatched in the URL.
    pairs = {')': '(', ']': '[', '}': '{'}
    while url and url[-1] in pairs and url.count(url[-1]) > url.count(pairs[url[-1]]):
        trailing = url[-1] + trailing
        url = url[:-1]
    return url, trailing


def weighted_length(text: str) -> int:
    text = unicodedata.normalize("NFC", text)
    total = 0
    pos = 0

    for match in URL_RE.finditer(text):
        start, end = match.span()
        if start > pos:
            for cluster in GRAPHEME_RE.findall(text[pos:start]):
                total += grapheme_weight(cluster)

        matched = match.group(0)
        url, trailing = strip_trailing_url_punctuation(matched)
        if url:
            total += URL_WEIGHT
        for cluster in GRAPHEME_RE.findall(trailing):
            total += grapheme_weight(cluster)
        pos = end

    if pos < len(text):
        for cluster in GRAPHEME_RE.findall(text[pos:]):
            total += grapheme_weight(cluster)

    return total


def load_posts(path: Path) -> list[str]:
    data = json.loads(path.read_text(encoding="utf-8"))
    if isinstance(data, dict):
        data = data.get("posts")
    if not isinstance(data, list) or not all(isinstance(x, str) for x in data):
        raise ValueError("JSON must be an array of strings or an object with a 'posts' array.")
    return data


def main() -> int:
    if len(sys.argv) != 2:
        print("Usage: python validate_x_posts.py posts.json", file=sys.stderr)
        return 2

    posts = load_posts(Path(sys.argv[1]))
    all_valid = True

    for idx, post in enumerate(posts, 1):
        length = weighted_length(post)
        valid = 0 < length <= MAX_WEIGHTED
        all_valid = all_valid and valid
        status = "OK" if valid else "OVER LIMIT" if length > MAX_WEIGHTED else "EMPTY"
        print(f"{idx}: {length}/{MAX_WEIGHTED} {status}")

    return 0 if all_valid else 1


if __name__ == "__main__":
    raise SystemExit(main())
