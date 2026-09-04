---
name: lide-social-package
description: Produce the complete LIDE / El Quantificador social-media package from charts, analyses, article drafts, or research findings. Use when asked to draft or adapt El Quantificador content for LinkedIn, X/Twitter, Instagram, and Facebook, including multi-chart posts, author attribution, source notes, article CTAs, hashtags, and optional New Dimensions promotion. Enforce LIDE's concise Ecuador-focused quantitative voice, validate every standard X post against the 280 weighted-character limit, and validate Instagram captions for the 5-hashtag maximum and no raw URLs.
---

# LIDE Social Package

Produce platform-native copy for LIDE / El Quantificador. Treat the chart or analysis as the evidence and the caption as the interpretation.

Read `references/platform-spec.md` before drafting. If producing X/Twitter copy, validate every post with `scripts/validate_x_posts.py`. If producing Instagram copy, validate it with `scripts/validate_instagram_caption.py`.

## Inputs

Accept any combination of the following. Do not demand fields that can be reliably inferred from supplied charts or text.

### Required substantive input
At least one of:
- chart image(s) or chart text/data;
- analytical finding(s);
- article/draft/report containing the findings;
- link or connected document containing the analysis.

### Preferred metadata
- `topic`: subject of the analysis.
- `chart_order`: intended sequence of charts, if multiple.
- `primary_finding`: main empirical takeaway.
- `secondary_findings`: supporting findings by chart.
- `source`: dataset/institution, e.g. INEC, ENEMDU, BCE.
- `period`: reference period.
- `author`: analyst/researcher to credit.
- `author_profile`: optional title, affiliation, or social handle if supplied or verified.
- `article_url`: optional full-analysis URL. Use it on Facebook, LinkedIn, and X when useful; never place it raw in Instagram copy.
- `instagram_cta`: default to `Lee el análisis completo en el link de nuestra bio.` when an article exists.
- `promotion`: optional separate institutional/commercial plug, commonly New Dimensions.
- `promotion_url`: optional URL for the promotional X/Facebook/LinkedIn copy. Never place it raw in Instagram copy.
- `hashtags`: optional explicit hashtags. For Instagram, select at most 5 and use 3-5 targeted hashtags by default.
- `language`: default Spanish used naturally in Ecuador.

### Output controls
- `platforms`: default `linkedin, x, instagram, facebook`.
- `x_thread_length`: default determined by chart count plus metadata and optional promotion.
- `include_counts`: default `true` for X; report weighted character counts outside the post text.
- `include_source_line`: default `true` when source is known.
- `include_author_credit`: default `true` when author is known.
- `instagram_hashtag_count`: default `3-5`; hard maximum `5`.
- `tone_override`: only when the user explicitly requests a different tone.

## Workflow

1. Extract the evidence.
   - Identify the main finding and one useful supporting finding per chart.
   - Preserve denominators, units, dates, populations, and comparison groups.
   - Never infer causality from descriptive evidence.
   - Never invent a value, source, author, date, or methodological detail.

2. Find the narrative.
   - State what the graph means, not everything the graph contains.
   - Prefer a concrete empirical tension or comparison as the hook.
   - Keep institutional self-reference secondary.

3. Draft each platform independently.
   - Do not copy-paste one platform's caption into another.
   - Always include Facebook in the full package.
   - Treat Facebook as the normal link-bearing counterpart to Instagram when `article_url` exists.
   - Follow `references/platform-spec.md` exactly.

4. Validate X/Twitter.
   - Draft standard posts, not Premium long-form posts, unless explicitly requested.
   - Write the proposed posts to a JSON file as an array of strings.
   - Run:
     `python scripts/validate_x_posts.py posts.json`
   - If any post exceeds 280 weighted characters, revise and rerun until all are valid.
   - Never return an unvalidated X thread.

5. Validate Instagram.
   - Never include `http://`, `https://`, or `www.` in the caption.
   - Include 3-5 targeted hashtags by default.
   - Never exceed 5 hashtags in a post or Reel caption.
   - Put the hashtags at the end of the caption unless the user explicitly requests another placement.
   - Write the proposed caption to a UTF-8 text file and run:
     `python scripts/validate_instagram_caption.py caption.txt`
   - If validation fails, revise and rerun until valid.

6. Final factual and platform check.
   - Confirm every number and claim is supported by the supplied evidence.
   - Confirm chart order matches X thread order.
   - Confirm authorship/source/promotion are separated correctly.
   - Confirm Instagram has no raw URL and has 3-5 targeted hashtags, maximum 5.
   - Confirm Facebook contains the direct article link when one is supplied and useful.
   - Remove filler, duplicated claims, and generic hashtags.

## Output

Return the requested package in this order.

### LinkedIn
One finished post. No character-count annotation.

### X / Twitter
Return each post separately as `1/N`, `2/N`, etc. Do not put the numbering inside the post unless the user asks for visible numbering.
After each post, show `Weighted characters: N/280` as metadata outside the post.

### Instagram
One finished caption by default, even for a carousel. Never include a raw URL. When an article exists, use `link in bio` wording. End with 3-5 targeted hashtags, never more than 5.

### Facebook
One finished Facebook post. When an article exists, include the direct URL in the post so Facebook carries the clickable destination that Instagram cannot. Do not simply clone the Instagram caption.

### Optional platforms
Only add Threads, newsletter, or other channels when explicitly requested. Adapt them natively rather than cloning another platform.

## Quality bar

A successful package:
- opens with the empirical insight;
- is concise enough that the chart remains the evidence-bearing object;
- distinguishes result from interpretation;
- credits the analyst when known;
- names the source when useful;
- avoids generic NGO/marketing language;
- uses natural Ecuadorian Spanish;
- keeps promotional material separate from the analytical finding;
- contains no unsupported causal or policy claim;
- contains no X post above 280 weighted characters;
- contains no raw URL in Instagram;
- contains 3-5 relevant Instagram hashtags and never more than 5;
- includes a Facebook version in the default full package.
