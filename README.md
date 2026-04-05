# Jays Matchup Intel

Daily pre-game matchup intelligence and post-game retrospective analysis for the Toronto Blue Jays. Built and published by [@scherrology](https://x.com/scherrology) under the **Arm Chair Analyst** brand.

**Live site:** [scherrology-sys.github.io/jays-matchup-intel](https://scherrology-sys.github.io/jays-matchup-intel)

---

## What This Is

Before every Jays game, a full pre-game matchup preview is published covering the opposing starter. After the game, a retrospective evaluates the pre-game read against observed outcomes and updates the system's working knowledge of that pitcher type going forward.

The analysis is built around a self-calibrating framework that names its assumptions explicitly, scores them after each game, and carries what it learns into the next preview. That loop is the point. A single preview is directional. A season of previews and retrospectives is a progressively sharper instrument.

---

## Analytical Framework

### Expected wOBA Computation

The core output for each hitter is an **arsenal-weighted expected wOBA (exp_wOBA)**: a pre-game estimate of offensive production against the specific pitch mix the opposing starter is projected to deploy, weighted by handedness.

For each hitter facing pitcher *p*:

```
exp_wOBA = Σ (pitch_weight_k × hitter_wOBA_vs_pitch_k)
```

where `pitch_weight_k` is the pitcher's projected usage fraction for pitch type *k* against the hitter's handedness, and `hitter_wOBA_vs_pitch_k` is the hitter's 2025 wOBA against pitch type *k* across all pitchers. A minimum of 10 plate appearances is required to use a hitter's pitch-type split; below that threshold the hitter's overall wOBA serves as a fallback.

### Pitcher Sample Weighting

The projected pitch mix is derived from all available Statcast pitch-level records for the pitcher across seasons, not just the current year. Each pitch is weighted by exponential recency decay from the game date:

```
recency_weight = 0.5^(age_in_days / 180)
```

A pitch thrown at mid-2025 carries approximately 26% the weight of a pitch thrown today. A pitch from mid-2024 carries approximately 9%. This produces a recency-weighted effective sample count used for confidence classification, and a recency-weighted pitch mix by batter handedness.

The half-life of 180 days is a principled default. It is not empirically validated against prediction accuracy and represents an assumption about the rate at which pitcher approaches stabilize across seasons.

### Credible Intervals

Uncertainty around each exp_wOBA point estimate is expressed as a 90% credible interval derived from a Beta conjugate prior. The prior is parameterized with a strength of K = 60 pseudo-observations anchored to the computed exp_wOBA:

```
alpha_0 = exp_wOBA × K
beta_0  = (1 − exp_wOBA) × K
CI_90   = [Beta(0.05, α₀, β₀), Beta(0.95, α₀, β₀)]
```

For a hitter with an exp_wOBA of .350, this produces a 90% CI of approximately [.252, .453], a width of .201. The interval is wide by design at small effective sample sizes; it narrows as the pitcher's career sample grows and as assumption reliability accumulates in the memory system. K = 60 is asserted, not derived from calibration.

**Important distinction:** The credible intervals are genuine Bayesian posterior quantiles. The point estimate (exp_wOBA) is an empirical Bayes approximation: a weighted average computed before inference, not a posterior mean. These are not equivalent. A fully Bayesian formulation would propagate uncertainty through the pitch mix itself and produce the point estimate as a posterior mean. That is the named long-term direction for this framework.

### Confidence Classification

Confidence tiers are assigned by effective weighted sample size and, where available, historical mean absolute error (MAE) for the pitcher archetype:

| Tier | Effective sample | Historical MAE |
|------|-----------------|----------------|
| LOW | < 200 | any |
| MODERATE | 200 – 700 | ≤ 0.060 |
| HIGH | ≥ 700 | ≤ 0.040 |

Historical MAE is itself recency-weighted (half-life 30 days) from the memory system, and initializes as missing for new pitcher archetypes.

### Assumption Tracking

Each preview names two to three pre-game assumptions with explicit expected values, tolerances, and hitter-level sensitivity weights. After each game the retrospective:

1. Derives actual values for quantifiable assumptions directly from the pitch file (Changeup-to-LHH%, Sweeper usage%)
2. Scores each assumption: held / partial / failed
3. Attributes each hitter's residual to the most specific available explanation, in priority order: within expected variance, insufficient PA sample, no pre-game expectation, assumption miss, execution miss, or variance miss
4. Updates a persistent assumption audit file, which feeds the reliability weighting for the next preview involving the same pitcher type

Assumption reliability is recency-weighted (half-life 30 days). An assumption that held six months ago carries less weight than one that failed last week.

---

## Real Limitations

These are honest. Some are fixable with more data or a more sophisticated statistical formulation. Some are structural to the problem.

### The single-game PA problem is severe

This is the most important limitation. The standard error of observed wOBA in a 2-PA sample is approximately **0.337**. The retrospective's threshold for calling a result outside expected variance is a residual of 0.050. That threshold is well below one standard deviation of measurement noise at this sample size. In practical terms: most single-game misses in the scorecard are indistinguishable from random variation, regardless of whether the pre-game expectation was accurate. The scorecard is useful for identifying systematic bias across many games and for attributing assumption failures, but individual game verdicts should be interpreted with appropriate skepticism. A season-level MAE is more informative than any single game's score.

### The point estimate is not a posterior

As described above, exp_wOBA is a weighted average, not a Bayesian posterior mean. The credible intervals are computed correctly but they are placed around a frequentist point estimate. The practical consequence: the point estimate does not update as a pitcher accumulates 2026 starts. If Martin throws 200 pitches in 2026, his pitch mix estimate remains a career-weighted average rather than a posterior that genuinely reflects the current season. This is a known limitation of the empirical Bayes approximation. The planned resolution is a full Bayesian hierarchical formulation with partial pooling across pitcher archetypes.

### Pitch-type wOBA does not distinguish between pitchers

The hitter's performance vs "Changeup" is aggregated across every pitcher who has thrown a Changeup to that hitter in the prior season. Davis Martin's Changeup moves differently from any other pitcher's Changeup. The framework treats these as interchangeable. In a more complete formulation, pitch characteristics (velocity, movement, release point) would be incorporated into the hitter-pitch match, not just the categorical pitch type label.

### No head-to-head history with this lineup

The Jays have zero career plate appearances against Davis Martin. Every number in tonight's preview is built entirely from Martin's tendencies vs all opponents and the Jays hitters' tendencies vs all pitchers of a given type. This is a structural limitation of new matchups that cannot be resolved without accumulated history.

### K = 60 and the half-life of 180 days are asserted defaults

The prior strength of 60 pseudo-observations and the recency decay half-life of 180 days are principled choices, not empirically derived ones. The correct values depend on the actual distributional properties of pitcher evolution and hitter variance, which require calibration against historical prediction accuracy. Neither parameter has been validated. Both represent assumptions that may be meaningfully wrong.

### Pitch classification is not perfectly stable year over year

Statcast pitch classification algorithms are updated periodically. A pitch labeled "Cutter" in 2026 may have been labeled "Slider" in 2025, or may be a genuinely new pitch. The framework cannot distinguish between a true repertoire change and a reclassification artifact. The Cutter emergence in Martin's 2026 start is a real analytical uncertainty that may partially reflect classification differences rather than a new pitch.

### Hitter independence is assumed

The framework treats each hitter's outcome as conditionally independent given the pre-game expectation. In practice, outcomes within a game are correlated: if Martin's Changeup is working well, it is working well against all hitters, not just some of them. This correlation structure inflates the apparent power of the retrospective scorecard. A hierarchical model with a game-level effect would address this correctly.

### No park factor, umpire, or weather adjustment

The league-average wOBA baseline of .320 is applied uniformly regardless of venue. Guaranteed Rate Field and Rogers Centre have different run environments. Umpire tendencies affect strike zone width and walk rates in ways that interact with pitcher command assumptions. Weather conditions (wind, temperature) affect batted ball outcomes. None of these are incorporated.

### The opener innings are excluded by design

The top of the order faces the opener before the bulk pitcher takes over. The analysis covers the bulk pitcher's plate appearances only, which means the top three hitters in the lineup may have fewer opportunities in the scoring window than the full lineup would suggest. The PA distribution across the lineup is not adjusted to account for this.

### Assumption framework inputs are analyst-determined

The named assumptions, their expected values, tolerances, and importance weights are specified by the analyst before each game. There is no objective process for setting these values. A different analyst would make different choices. The assumption framework is the most transparent part of the system, but also the most subjective.

---

## Scope

Analysis covers the **opposing bulk pitcher's plate appearances only.** Opener innings are logged in the retrospective but excluded from scoring and memory. Relief pitcher usage, pinch-hitting sequences, and bullpen game structures are excluded. Modeling these pre-game from public data is not defensible.

---

## Site Structure

```
index.html                           game hub, all previews and retrospectives
games/YYYY-MM-DD-pitcher/            pre-game matchup previews
retro/YYYY-MM-DD-pitcher/            post-game retrospectives
analysis/                            R source code
model_memory/                        persistent assumption and hitter audit memory
```

## Reproducibility

Source code is published in `/analysis`. The framework runs on three scripts:

- `jays_matchup_intel_self_learning.R` — all core functions
- `preview_runner_YYYY-MM-DD.R` — game-specific preview execution
- `retro_runner_YYYY-MM-DD_pitcher.R` — game-specific retrospective execution

Statcast pitch-level exports sourced from [Baseball Savant](https://baseballsavant.mlb.com).

---

## 2026 Season Log

| Date | Opp | Pitcher | Result | Top Pick | Links |
|------|-----|---------|--------|----------|-------|
| 4-5-26  | @ CWS | D. Martin RHP · G. Taylor (opener) | — | G. Springer (.43) | [Preview](https://scherrology-sys.github.io/jays-matchup-intel/games/2026-04-05-martin-taylor/) |
| 4-4-26  | @ CWS | A. Kay LHP · G. Taylor (opener) | CWS 6, TOR 3 | G. Springer (.43) | [Preview](https://scherrology-sys.github.io/jays-matchup-intel/games/2026-04-04-kay-taylor/) · [Retro](https://scherrology-sys.github.io/jays-matchup-intel/retro/2026-04-04-kay-taylor/) |
| 4-3-26  | @ CWS | S. Burke RHP · G. Taylor (opener) | CWS 5, TOR 4 F/10 | G. Springer (.41) | [Preview](https://scherrology-sys.github.io/jays-matchup-intel/games/2026-04-03-burke-taylor/) · [Retro](https://scherrology-sys.github.io/jays-matchup-intel/retro/2026-04-03-burke-taylor/) |
| 4-1-26  | COL | K. Freeland LHP | COL 2, TOR 1 | G. Springer (.44) | [Preview](https://scherrology-sys.github.io/jays-matchup-intel/games/2026-04-01-freeland/) · [Retro](https://scherrology-sys.github.io/jays-matchup-intel/retro/2026-04-01-freeland/) |
| 3-31-26 | COL | R. Feltner RHP | TOR 5, COL 1 | G. Springer (.42) | [Preview](https://scherrology-sys.github.io/jays-matchup-intel/games/2026-03-31-feltner/) · [Retro](https://scherrology-sys.github.io/jays-matchup-intel/retro/2026-03-31-feltner/) |
| 3-30-26 | COL | T. Sugano RHP | COL 14, TOR 5 | G. Springer (.40) | [Preview](https://scherrology-sys.github.io/jays-matchup-intel/games/2026-03-30-sugano/) · [Retro](https://scherrology-sys.github.io/jays-matchup-intel/retro/2026-03-30-sugano/) |

**2026 record: 4-3 · Colorado: TOR 1-2 · Chicago White Sox: TOR 0-2, series finale April 5**

*Updated: April 5, 2026*

---

Built by **Arm Chair Analyst** · [@scherrology](https://x.com/scherrology) · [GitHub](https://github.com/scherrology-sys)
