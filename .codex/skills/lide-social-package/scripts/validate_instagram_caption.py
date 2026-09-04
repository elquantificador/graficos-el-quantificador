#!/usr/bin/env python3
"""Validate LIDE / El Quantificador Instagram captions.

Usage:
    python validate_instagram_caption.py caption.txt

Default house rules enforced by this skill:
- no raw URLs (http://, https://, or www.)
- 3 to 5 hashtags
- maximum 5 hashtags, matching Instagram's current post/Reel cap

The validator checks the caption only. It does not inspect image text, alt text,
or platform metadata.
"""

from __future__ import annotations

import re
import sys
from pathlib import Path

MIN_HASHTAGS = 3
MAX_HASHTAGS = 5

URL_RE = re.compile(r"(?i)(?:https?://|www\.)\S+")
# Python's Unicode-aware \w handles accented Spanish letters and digits.
HASHTAG_RE = re.compile(r"(?<!\w)#([\w]+)", re.UNICODE)


def main() -> int:
    if len(sys.argv) != 2:
        print("Usage: python validate_instagram_caption.py caption.txt", file=sys.stderr)
        return 2

    path = Path(sys.argv[1])
    text = path.read_text(encoding="utf-8")

    hashtags = HASHTAG_RE.findall(text)
    urls = URL_RE.findall(text)

    valid = True

    print(f"Hashtags: {len(hashtags)}/{MAX_HASHTAGS} (target {MIN_HASHTAGS}-{MAX_HASHTAGS})")
    if hashtags:
        print("Tags: " + ", ".join(f"#{tag}" for tag in hashtags))

    if len(hashtags) < MIN_HASHTAGS:
        print(f"ERROR: use at least {MIN_HASHTAGS} targeted hashtags for the default LIDE package.")
        valid = False
    if len(hashtags) > MAX_HASHTAGS:
        print(f"ERROR: Instagram caption exceeds the {MAX_HASHTAGS}-hashtag maximum.")
        valid = False

    if urls:
        print("ERROR: raw URL detected in Instagram caption. Use a link-in-bio CTA instead.")
        valid = False
    else:
        print("Raw URLs: none")

    return 0 if valid else 1


if __name__ == "__main__":
    raise SystemExit(main())
