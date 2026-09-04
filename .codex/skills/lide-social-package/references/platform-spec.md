# Platform specification

## 1. Shared editorial voice

Use a serious, quantitative, accessible, Ecuador-focused register.

Core rule: **tell people what the graph means, not what the graph contains.**

For every empirical pattern, research and state the strongest supported
explanation before drafting. Present the explanation directly. Do not use
meta-writing such as `el mapa no explica`, `los datos no permiten`, or generic
caveats in place of an explanation. If no credible explanatory source is
available, ask the user for one or restrict the copy to the verified finding.

Prefer:
- concrete findings;
- short declarative hooks;
- explicit comparisons;
- restrained interpretation;
- precise dates, units, groups, and sources when material.

Avoid:
- `Desde LIDE queremos compartir...` openings;
- generic NGO language;
- `¿Sabías que...?` unless genuinely stronger than the finding itself;
- motivational or inflated marketing language;
- copying every visible number from the chart;
- claiming causality from correlations or descriptive comparisons;
- long hashtag blocks;
- excessive emoji use;
- identical copy across platforms.

Write Spanish naturally for Ecuador. Do not use Rioplatense forms or vocabulary.

## 2. LinkedIn

### Purpose
Give the fullest social explanation while staying compact. The chart should carry most of the evidence.

### Default structure
1. Hook: one short sentence stating the main empirical finding or tension.
2. Explanation: 1-2 short paragraphs with the most important result(s).
3. Interpretation: at most one restrained paragraph/sentence if it adds value.
4. Attribution/source: concise closing line when known.
5. CTA: optional, only when there is a real destination such as the full article.

### Rules
- Usually 80-180 words. Shorter is acceptable when the chart is self-explanatory.
- Do not narrate every chart value.
- Use bullets only when they materially improve comparison.
- If using hashtags, prefer 1-3 relevant tags. Do not append a generic hashtag cloud.
- Keep New Dimensions or another promotion outside the analytical body whenever possible. If the user wants it attached, use a clearly separate final paragraph or recommend it as a comment.
- If an article URL is supplied, a direct link is acceptable and usually useful.

## 3. X / Twitter

### Hard technical rule
Default to standard X posts: **maximum 280 weighted characters per post.** X uses weighted counting, not naive string length. URLs count as 23 characters; emoji count as 2; some Unicode characters count as 2. Validate with `scripts/validate_x_posts.py`.

Do not rely on Premium long-form posting unless explicitly requested.

### Thread architecture
Use one substantive job per tweet.

#### Two-chart default
- Tweet 1: Chart A finding + brief interpretation.
- Tweet 2: Chart B finding + brief interpretation.
- Tweet 3: metadata/credit/source/article destination.
- Tweet 4: optional promotion, only if promotion input exists.

#### One-chart default
- Tweet 1: main chart finding.
- Tweet 2: attribution/source/article destination.
- Tweet 3: optional promotion.

#### Three or more charts
- Normally one tweet per chart, then one metadata tweet, then optional promotion.
- If two charts make the same point, combine only when the resulting tweet remains clear and <=280 weighted characters.
- Do not exceed 6 analytical/metadata tweets without a clear reason. Compress or prioritize instead.

### Content rules
- Each tweet must stand on its own enough to make sense when seen separately.
- Do not split a single sentence arbitrarily across tweets.
- Tweet 1 must contain the strongest finding, not housekeeping.
- Each chart tweet should identify the comparison or result, not merely say `Gráfico 1`.
- Metadata tweet may include author, LIDE/El Quantificador, source, and article URL.
- Promotional tweet must be analytically separate. Do not smuggle a course ad into a statistical finding.
- Avoid repeating the same hook or statistic across tweets.
- Prefer no more than 1-2 emojis per tweet; zero is often better.
- Prefer 0-2 hashtags per tweet, and usually none unless strategically useful.
- A URL on its own or embedded in a tweet counts as 23 weighted characters.
- Do not include internal `1/4` numbering in the tweet text by default because it consumes characters and is unnecessary when posted as a thread.

### Character target
- Hard maximum: 280 weighted characters.
- Preferred target for analytical tweets: 190-255 weighted characters.
- Preferred target for metadata/promotional tweets: 160-245 weighted characters.
- Leaving margin is preferred to writing exactly 280.

## 4. Instagram

### Current hard limits and house rules
- As of August 2026, Instagram caps posts and Reels at **5 hashtags**.
- Use **3-5 targeted hashtags by default**. Never exceed 5.
- Never put a raw URL in an Instagram caption. Do not write `https://`, `http://`, `www.`, or a bare article URL.
- If the post has a full article or external destination, use a link-in-bio CTA.

### Purpose
Give one compact interpretation of the carousel/post and direct readers to the full analysis through the bio when available.

### Default structure
1. Hook: strongest finding.
2. Synthesis: 1-3 short sentences covering the common story across the charts.
3. Attribution/source if useful.
4. CTA: when an article exists, default to `Lee el análisis completo en el link de nuestra bio.`
5. Hashtags: 3-5 targeted tags on the final line.

### Hashtag selection
Use hashtags as classification metadata, not filler.

Prefer a mix of:
- one country/geography tag when relevant, usually `#Ecuador`;
- one or two topic tags, e.g. `#Empleo`, `#Inflación`, `#Educación`, `#Economía`;
- one institutional/project tag when useful, e.g. `#ElQuantificador`;
- one additional precise subject tag if it adds information.

Do not use broad engagement bait such as `#viral`, `#fyp`, `#explore`, `#instagood`, or unrelated trending tags.

### Rules
- Default to one caption, not one mini-caption per slide.
- Usually 50-120 words before hashtags.
- Never include a raw URL.
- Do not exceed 5 hashtags for a post or Reel.
- Default to 3-5 hashtags rather than zero.
- Put hashtags at the end, separated cleanly from the prose.
- Keep the analytical voice primary. Promotion should be separate unless the post itself is promotional.
- Validate with `scripts/validate_instagram_caption.py` before returning.

## 5. Facebook

### Purpose
Provide the link-bearing version of the social package for a broad audience. Facebook should carry the direct article destination that Instagram cannot.

### Default structure
1. Hook: strongest empirical finding.
2. Explanation: 1-3 short paragraphs, accessible but precise.
3. Attribution/source when known.
4. Direct article URL or destination when supplied.
5. Optional 1-3 relevant hashtags.

### Rules
- Always include Facebook in the default full package.
- Usually 70-160 words, excluding the URL.
- Do not simply copy the Instagram caption.
- A direct article URL is expected when `article_url` is supplied and the post points to the full analysis.
- Do not say `link in bio` on Facebook.
- Keep wording somewhat more explanatory than X and somewhat less professional/formal than LinkedIn.
- Use 0-3 relevant hashtags. Do not append a large hashtag block.
- Keep New Dimensions or another promotion separate from analytical copy unless the Facebook post itself is promotional.

## 6. Attribution and institutional hierarchy

When an identifiable analyst exists, give visible credit. Preferred conceptual hierarchy:

**finding -> analysis -> author -> El Quantificador / LIDE**

Do not make LIDE the protagonist of an empirical post.

Acceptable attribution patterns include:
- `Elaborado por [Nombre].`
- `Análisis de [Nombre] para El Quantificador.`
- `Fuente: [institución/dataset]. Elaborado por [Nombre].`

Use the shortest form that preserves useful credit and provenance.

## 7. New Dimensions and other promotion

Treat promotion as a distinct content object.

For New Dimensions:
- state the concrete offer/course/price when supplied;
- never invent or assume a price;
- do not claim a `new price` or `price reduction` unless the input confirms it;
- avoid generic `transforma tu futuro` style language;
- prefer a separate X tweet and, on LinkedIn/Facebook, a separate closing paragraph or standalone post when the main content is analytical;
- on Instagram, never include a raw promotional URL. Use the bio destination if applicable.

## 8. Fact discipline

Use the chart, article, or supplied analysis as the source of truth.

If multiple numbers are available, select only those needed to support the message. Preserve:
- units and currency;
- percentage vs percentage-point distinctions;
- nominal vs adjusted measures only when the source itself supports the distinction;
- subgroup definitions;
- time period;
- sample/population scope.

When a chart is descriptive, distinguish the observed pattern from the
external evidence used to explain it. State the supported explanation directly;
do not convert descriptive evidence into an unsupported causal claim.
