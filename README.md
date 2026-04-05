# Jays Matchup Intel

Daily Statcast matchup intelligence for the Toronto Blue Jays. Built in R, published by [@scherrology](https://x.com/scherrology) under the **Arm Chair Analyst** brand.

## Live Site

[scherrology-sys.github.io/jays-matchup-intel](https://scherrology-sys.github.io/jays-matchup-intel)

---

## What This Is

Before every Jays game, a full matchup breakdown goes up covering the opposing starter. After the game, a retrospective scores the read against what actually happened. The analysis builds on itself as the season goes on, getting sharper with each start by a pitcher the system has seen before.

Each preview covers the pitcher's arsenal and movement profile, how the mix shifts by batter handedness, expected production for each hitter with uncertainty ranges, three things to watch during the game, and a plain-language bottom line.

Each retrospective covers what he actually threw versus what was projected, a hitter-by-hitter scorecard, whether the pre-game assumptions held, and what changes next time.

---

## V2: The Self-Learning System

**April 5, 2026 is the public launch of V2**, the version of this analysis that learns and builds on each game.

### What changed from V1 to V2

**V1** covered the Colorado and Chicago White Sox series from March 30 through April 4. It was a strong analytical start, but every game was analyzed in isolation. Pitch mix assumptions were set manually. Hitter exposure weights were entered by hand based on judgment. There was no mechanism for the analysis to remember what happened last time, score itself against the result, or adjust going forward. Each preview started from the same place regardless of what had already been learned.

The specific problems V2 addresses:

**Exposure weights derived from data, not intuition.** In V1, the four most prominent right-handed hitters were manually assigned higher Sweeper exposure weights on the reasonable assumption that RHH face more Sweepers. When we ran the actual 2025 Statcast data, it said Ernie Clement had the highest Sweeper PA share in this lineup at 9.3%, not the group we assumed. V2 computes those weights from the historical pitch-type data for each hitter. The data gets to tell the story.

**Assumption actuals auto-derived from the game pitch file.** In V1, after each game the Changeup-to-LHH percentage and Sweeper usage had to be calculated manually before the retro could run. V2 pulls those directly from the pitch file using a named pitch type mapping, so the retro produces the same output every time with a single function call.

**A reliability feedback loop.** In V1, if an assumption failed there was no mechanism to carry that forward. In V2, every failure reduces that assumption's reliability weight in the next preview for the same pitcher type. The Sweeper usage assumption failed on the first Kay game scored, 26% actual versus 17% expected. The next time Kay pitches, the projection starts closer to 22 to 25%, not 17%.

**Hitter-specific attribution.** In V1, if any assumption failed, every hitter in the lineup got the assumption miss tag regardless of whether the failed assumption actually applied to them. In V2, the retro only attributes the assumption miss to hitters who were actually exposed to that assumption based on handedness and pitch-type history. Springer getting the Changeup-to-LHH assumption miss tag made no sense because he is right-handed.

**The fastball zone rate bug.** The execution flag for poor fastball command was checking `zone == 1`, which is the upper-left quadrant only. Statcast zones 1 through 9 are in-zone. V2 uses `zone %in% 1:9`. This changes what the execution flag actually measures.

**CI inflation prevention.** Multiple failing assumptions could compound the CI widening multiplier without limit in V1. V2 caps it at 2.0, preserving the directional signal without producing unreasonably wide intervals.

**Fragility score in output.** The exposure-weighted assumption sensitivity score was computed in V1 but discarded before output. In V2 it appears in the hitter expectations table and the retro scorecard, so the reader can see at a glance which hitters are most sensitive to tonight's pitch mix deviating from projection.

### What did not change

The core expectation engine is the same in V1 and V2: an arsenal-weighted expected wOBA built from each hitter's 2025 pitch-type performance, weighted by the pitcher's actual handedness-split mix from Statcast. The Beta-Binomial credible intervals, wOBA weights, league baseline, and confidence tier thresholds are unchanged. V2 improves what surrounds the expectation engine, not the engine itself.

### What I learned between V1 and V2

The biggest lesson: manually entered assumptions do not survive contact with data. The Sweeper exposure assumption was defensible on its face, but the data contradicted it. Building a system that derives those inputs automatically removes one more layer of human judgment between the data and the output.

The second lesson: the assumption tracking framework is the most defensible part of this whole approach. It transforms "we were wrong" from a conversation-ending statement into a productive one. When the Sweeper assumption failed on April 4, the retro could say exactly which hitters it applied to, in which direction it moved their expectations, and what changes next start. That specificity is what makes the analysis improvable rather than just reactive.

The third lesson: the retro is where the real work happens. The preview gets the attention. The retro is where the analysis earns the right to make the next one.

---

## How the Loop Works

```
Preview  →  Game  →  Retro  →  Memory  →  Next Preview
```

Each preview names the assumptions it is relying on, with expected values, tolerances, and importance weights. The retro scores each assumption against what actually happened, computes execution flags from the pitch file, and attributes each hitter's result to the most specific explanation available. Everything writes to a persistent memory file by pitcher archetype. The next preview reads from memory and adjusts accordingly.

### Confidence tiers

Tiers reflect both sample size and how well this type of pitcher has been predicted in the past.

- **LOW** — fewer than 200 pitches in the 2026 sample, or archetype historical error above 0.080
- **MODERATE** — 200 to 700 pitches, historical error at or below 0.060
- **HIGH** — 700+ pitches and historical error at or below 0.040

Historical error is recency-weighted: starts from earlier in the season count less than recent ones as the profile develops.

### Scope

Analysis covers the bulk pitcher's plate appearances only. Opener innings are logged but not scored. This is a design decision: modeling relief usage from public data pre-game is not defensible.

---

## Site Structure

```
index.html                           landing hub, all games
games/YYYY-MM-DD-pitcher/            game previews
retro/YYYY-MM-DD-pitcher/            retrospectives
analysis/                            R scripts
model_memory/                        assumption and hitter audit memory
```

## Reproducibility

All analytical R code is published in `/analysis`. The system runs on three scripts:

- `jays_matchup_intel_self_learning.R` — full system functions
- `preview_runner_YYYY-MM-DD.R` — game-specific preview run script
- `retro_runner_YYYY-MM-DD_pitcher.R` — game-specific retro run script

Data: [Baseball Savant](https://baseballsavant.mlb.com) Statcast exports.

---

## 2026 Season Log

| Date | Opp | Pitcher | Result | Top Pick | Links |
|------|-----|---------|--------|----------|-------|
| 4-5-26  | @ CWS | TBD · G. Taylor opens | — | — | [Preview](https://scherrology-sys.github.io/jays-matchup-intel/games/2026-04-05/) |
| 4-4-26  | @ CWS | A. Kay LHP (bulk) · G. Taylor (opener) | CWS 6, TOR 3 | G. Springer (.43) | [Preview](https://scherrology-sys.github.io/jays-matchup-intel/games/2026-04-04-kay-taylor/) · [Retro](https://scherrology-sys.github.io/jays-matchup-intel/retro/2026-04-04-kay-taylor/) |
| 4-3-26  | @ CWS | S. Burke RHP (bulk) · G. Taylor (opener) | CWS 5, TOR 4 F/10 | G. Springer (.41) | [Preview](https://scherrology-sys.github.io/jays-matchup-intel/games/2026-04-03-burke-taylor/) · [Retro](https://scherrology-sys.github.io/jays-matchup-intel/retro/2026-04-03-burke-taylor/) |
| 4-1-26  | COL | K. Freeland LHP | COL 2, TOR 1 | G. Springer (.44) | [Preview](https://scherrology-sys.github.io/jays-matchup-intel/games/2026-04-01-freeland/) · [Retro](https://scherrology-sys.github.io/jays-matchup-intel/retro/2026-04-01-freeland/) |
| 3-31-26 | COL | R. Feltner RHP | TOR 5, COL 1 | G. Springer (.42) | [Preview](https://scherrology-sys.github.io/jays-matchup-intel/games/2026-03-31-feltner/) · [Retro](https://scherrology-sys.github.io/jays-matchup-intel/retro/2026-03-31-feltner/) |
| 3-30-26 | COL | T. Sugano RHP | COL 14, TOR 5 | G. Springer (.40) | [Preview](https://scherrology-sys.github.io/jays-matchup-intel/games/2026-03-30-sugano/) · [Retro](https://scherrology-sys.github.io/jays-matchup-intel/retro/2026-03-30-sugano/) |

**2026 record: 4-3 · Colorado: TOR 1-2 · Chicago White Sox: TOR 0-2, series ongoing**

*Last updated: April 5, 2026 · V2 launch*

---

## Known Limitations

- Public Statcast data only. Internal scouting and organizational context are not reflected.
- First-exposure pitchers produce prior-only estimates with LOW confidence and wide uncertainty ranges. That is intentional and disclosed.
- Opener-plus-bulk games score the bulk pitcher's PA only. The opener's inning is logged but excluded from retro scoring and memory.
- Off-season development introduces prior uncertainty that narrows as 2026 data accumulates.

---

Built by **Arm Chair Analyst** · [@scherrology](https://x.com/scherrology) · [GitHub](https://github.com/scherrology-sys)
