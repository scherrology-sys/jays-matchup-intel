# jays-matchup-intel

Daily Statcast matchup intelligence for the Toronto Blue Jays. Bayesian pitcher
previews, pitch arsenal breakdowns, and model retrospectives. Built in R,
branded for [@scherrology](https://x.com/scherrology).

## Live Site

[scherrology-sys.github.io/jays-matchup-intel](https://scherrology-sys.github.io/jays-matchup-intel)

## What This Is

Every Blue Jays starter gets a full analytical breakdown before first pitch:

- **Pitch arsenal** — usage mix, velocity, movement profile, whiff and CSW rates
- **Handedness splits** — how the pitch mix changes vs LHH and RHH
- **Zone tendency maps** — where the pitcher locates by batter handedness
- **Head-to-head history** — Blue Jays hitters vs this pitcher, outcome by outcome
- **Bayesian wOBA model** — Beta-Binomial posteriors with 90% credible intervals, prior anchored to each hitter's full season wOBA, updated with pitcher-specific plate appearance history
- **Tonight's call** — top targets, pitch to hunt, and the social narrative

The morning after each start, a retrospective is published evaluating the scouting report against what actually happened:

- **Arsenal report card** — projected vs actual pitch mix and velocity, flagging deviations
- **Zone comparison** — predicted vs actual location by handedness
- **Model scorecard** — posterior predictions vs actual wOBA against the starter
- **Deep dive** — pitch-level analysis of notable model hits and misses

## Site Structure

```
games/YYYY-MM-DD-pitcher/   preview for each start
retro/YYYY-MM-DD-pitcher/   retrospective for each start
analysis/                   R scripts for peer review
```

## Model

**Prior (v2, Arsenal-Weighted):** Each hitter's prior is no longer anchored to their flat overall 2025 wOBA. Instead, it is weighted by the pitcher's actual pitch mix against that hitter's handedness, derived from the pitcher's Statcast file. Each hitter's wOBA against each pitch type is pulled from the batter-level Statcast data already in the pipeline, requiring no additional export. Where a hitter has fewer than 10 PA against a specific pitch type, the overall wOBA fills in as a fallback.

**Likelihood:** wOBA-weighted plate appearance observations from the hitter's actual history against this specific pitcher.

**Posterior:** Updated wOBA estimate with 90% credible interval.

**Confidence tiers:**
- **T1** — Arsenal-weighted prior + matchup history update. Tightest credible intervals.
- **T2** — Arsenal-weighted prior only. No matchup history available. Wider intervals.
- **T3** — No 2025 MLB data at all. Model cannot score. Flagged explicitly.

**Scope:** Model is evaluated against starter plate appearances only. Relief pitchers are excluded by design, as pre-game modeling of bullpen usage is not defensible with public data.

## Reproducibility

All analytical code (R) is published in the `/analysis` folder of this repository for peer review. The visualization and front-end implementation are proprietary.

Analysts who wish to reproduce or challenge the model outputs can run the R scripts against the same Statcast exports from [Baseball Savant](https://baseballsavant.mlb.com).

## Known Limitations

- Model is built on public Statcast data only. Spring training development, internal scouting reports, and organizational intelligence are not reflected.
- Off-season pitcher development introduces prior uncertainty that widens with recency of the data and narrows as 2026 starts accumulate.
- Interleague opponents, particularly NL West teams, carry limited matchup history and wider credible intervals by design.
- First-exposure pitchers (no prior matchup data) are handled with prior-only estimates, flagged clearly in every report.

## Tech Stack

- **R** — tidyverse, ggplot2, base Beta-Binomial inference via qbeta()
- **Data** — Baseball Savant Statcast exports
- **Hosting** — GitHub Pages

## 2026 Season Log

| Date | Opponent | Pitcher | Top Pick | Links |
|------|----------|---------|----------|-------|
| 4-2-26  | CWS | Burke (bulk) / Taylor (opener) RHP | G. Springer (.410) | [Preview](https://scherrology-sys.github.io/jays-matchup-intel/games/2026-04-02-burke-taylor/) |
| 4-1-26  | COL | Kyle Freeland LHP    | G. Springer (.440) | [Preview](https://scherrology-sys.github.io/jays-matchup-intel/games/2026-04-01-freeland/) · [Retro](https://scherrology-sys.github.io/jays-matchup-intel/retro/2026-04-01-freeland/) |
| 3-31-26 | COL | Ryan Feltner RHP     | G. Springer (.421) | [Preview](https://scherrology-sys.github.io/jays-matchup-intel/games/2026-03-31-feltner/) · [Retro](https://scherrology-sys.github.io/jays-matchup-intel/retro/2026-03-31-feltner/) |
| 3-30-26 | COL | Tomoyuki Sugano RHP  | G. Springer (.404) | [Preview](https://scherrology-sys.github.io/jays-matchup-intel/games/2026-03-30-sugano/) · [Retro](https://scherrology-sys.github.io/jays-matchup-intel/retro/2026-03-30-sugano/) |

**Colorado series: TOR 1-2 · New York Yankees series begins 4-2-26**

## Brand

Built by **Arm Chair Analyst** · [@scherrology](https://x.com/scherrology)
