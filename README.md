# Jays Matchup Intel

Daily pre-game matchup intelligence and post-game retrospective analysis for the Toronto Blue Jays. Built and published by [@scherrology](https://x.com/scherrology) under the **Arm Chair Analyst** brand.

**Live site:** [scherrology-sys.github.io/jays-matchup-intel](https://scherrology-sys.github.io/jays-matchup-intel)

---

## What This Is

Before every Jays game, a full pre-game matchup preview is published covering the opposing starter. After the game, a retrospective evaluates the pre-game read against observed outcomes and updates the system's working knowledge of that pitcher type going forward.

The analysis is built around a self-calibrating framework that names its assumptions explicitly, scores them after each game, and carries what it learns into the next preview. That loop is the point. A single preview is directional. A season of previews and retrospectives is a progressively sharper instrument, provided the self-learning layer actually improves forecast accuracy. Demonstrating that it does is the primary validation objective for the 2026 season.

---

## Analytical Framework

### Expected wOBA Computation

The core output for each hitter is an **arsenal-weighted expected wOBA (exp_wOBA)**: a pre-game estimate of offensive production against the specific pitch mix the opposing starter is projected to deploy, weighted by handedness.

For each hitter facing pitcher *p*:

```
exp_wOBA = Σ (pitch_weight_k × hitter_wOBA_vs_pitch_k)
```

where `pitch_weight_k` is the pitcher's projected usage fraction for pitch type *k* against the hitter's handedness, and `hitter_wOBA_vs_pitch_k` is the hitter's prior-season wOBA against pitch type *k* across all pitchers. A minimum of 10 plate appearances is required to use a hitter's pitch-type split; below that threshold the hitter's overall wOBA serves as a fallback.

### Pitcher Sample Weighting

The projected pitch mix is derived from all available Statcast pitch-level records for the pitcher across seasons, not just the current year. Each pitch is weighted by exponential recency decay from the game date:

```
recency_weight = 0.5^(age_in_days / 180)
```

A pitch thrown at mid-2025 carries approximately 26% the weight of a pitch thrown today. A pitch from mid-2024 carries approximately 9%. This produces a recency-weighted effective sample count used for confidence classification, and a recency-weighted pitch mix by batter handedness.

The half-life of 180 days is a principled default. It has not been empirically validated against prediction accuracy. Calibration against historical data is a named objective in the validation roadmap below.

### Credible Intervals

Uncertainty around each exp_wOBA point estimate is expressed as a 90% credible interval derived from a Beta conjugate prior, parameterized with strength K = 60 pseudo-observations anchored to the computed exp_wOBA:

```
alpha_0 = exp_wOBA × K
beta_0  = (1 − exp_wOBA) × K
CI_90   = [Beta(0.05, α₀, β₀), Beta(0.95, α₀, β₀)]
```

For a hitter with an exp_wOBA of .350 this produces a 90% CI of approximately [.252, .453], a width of .201. K = 60 is asserted, not derived from calibration. The validation roadmap below includes a grid search over K and half-life values to identify parameters that minimize out-of-sample error on historical data.

**Important distinction:** The credible intervals are genuine Bayesian posterior quantiles. The point estimate (exp_wOBA) is an empirical Bayes approximation, a weighted average computed before inference, not a posterior mean. These are not equivalent. A fully Bayesian formulation would propagate uncertainty through the pitch mix itself and produce the point estimate as a posterior mean with partial pooling across pitcher archetypes. That is the named long-term direction for this framework.

### Confidence Classification

| Tier | Effective weighted sample | Historical MAE |
|------|--------------------------|----------------|
| LOW | < 200 | any |
| MODERATE | 200 – 700 | ≤ 0.060 |
| HIGH | ≥ 700 | ≤ 0.040 |

Historical MAE is recency-weighted (half-life 30 days) from the memory system and initializes as missing for new pitcher archetypes.

### Assumption Tracking and Memory

Each preview names two to three pre-game assumptions with explicit expected values, tolerances, and hitter-level sensitivity weights. After each game the retrospective:

1. Derives actual values for quantifiable assumptions directly from the pitch file
2. Scores each assumption: held / partial / failed
3. Attributes each hitter's residual to the most specific available explanation, in priority order: within expected variance, insufficient PA sample, no pre-game expectation, assumption miss, execution miss, or variance miss
4. Updates a persistent memory file by pitcher archetype

Assumption reliability is recency-weighted (half-life 30 days). Failed assumptions reduce the reliability weight for that assumption in the next preview for the same pitcher type, which widens credible intervals for exposed hitters and applies directional corrections to point estimates.

Whether this layer improves forecast accuracy over a static prior is the central empirical question the validation notebook is designed to answer.

### Pitch Sequencing

Starting with the April 5 Davis Martin game, pitch sequencing analysis is incorporated into retrospectives and previews where career sample size permits. The implementation uses pitch transition matrices by count state and handedness, computed from the full career Statcast file for each pitcher. Sparse cells are flagged rather than pooled. Partial pooling by pitcher archetype is a planned extension that pairs with the Bayesian hierarchical rebuild.

### Hitter In-Season Updating (added April 6, 2026)

The original framework used prior-season pitch-type splits for hitters and held them fixed through the current season. The model was blind to how pitchers adapt to individual hitters as 2026 data accumulates.

The updated framework adds a bidirectional adaptation loop:

**In-season hitter splits.** After each game, every hitter's 2026 pitch-type outcomes are accumulated with exponential recency decay (half-life 90 days, shorter than the pitcher half-life of 180 days because hitter approach adjustments happen faster than full repertoire changes).

**Blended priors.** Prior-season and in-season splits are blended by effective sample size. At 6 games into the season, the prior-season signal carries roughly 85-95% of the weight. By July it carries roughly 60%. The blend is continuous and automatic.

**Vulnerability detection.** A pitch type is flagged as a hitter vulnerability when the 2026 wOBA against that pitch falls 80 points or more below the prior-season baseline, with at least 3 effective plate appearances in the current season. Early-season flags are low-confidence by design.

**Pitcher targeting detection.** A pitcher is flagged as targeting a vulnerability when they throw the flagged pitch type at 1.25× or more of their overall usage rate against that hitter's handedness. This closes the loop: the framework now detects not just what the hitter is struggling with, but whether tonight's pitcher is specifically exploiting it.

**What this changes in the preview.** Top picks and suppressed hitters shift as in-season signals emerge. In the April 6 Wrobleski preview, Guerrero moves to the top of the lineup projection after his strong early-season Slider performance, while Springer drops based on 2026 struggles against the 4-Seam Fastball. These are low-confidence adjustments at 6 games. They are disclosed as such in the preview.

**Validation plan.** The backtest will be run with and without in-season updating to measure whether blending adds MAE lift over static prior-season splits. This is the A/B test the self-learning layer requires.

---

## Validation Roadmap

This section documents what has been demonstrated, what is in progress, and what has not yet been tested. The distinction matters.

### Current results (April 6, 2026)

**Games scored:** 7 (Feltner Apr 2024, Freeland Aug 2025, Sugano / Freeland / Burke / Kay / Martin in 2026)

**Hitter-games:** 48 (hitters with ≥ 2 PA vs starter)

| Metric | Framework | Naive-1 (hitter wOBA) | Naive-2 (handedness avg) |
|--------|-----------|----------------------|--------------------------|
| MAE | **0.278** | 0.279 | 0.259 |
| Lift | — | +0.1% | -7.6% |
| Win rate | — | 45.8% | 41.7% |
| Bias | +0.019 | — | — |

**Three-model comparison (added April 6, 2026):**

| Model | MAE | RMSE | Bias | Lift vs A |
|-------|-----|------|------|-----------|
| A: Static prior | 0.278 | 0.351 | +0.019 | — |
| B: Blended in-season | 0.275 | 0.349 | +0.015 | +1.3% |
| C: B + exploit score (λ=0.78) | 0.275 | 0.348 | +0.022 | +1.2% |
| Naive-1 (hitter wOBA) | 0.279 | 0.351 | +0.017 | -0.1% |
| Naive-2 (handedness avg) | 0.259 | 0.339 | +0.005 | +7.0% |

**The honest interpretation of these numbers:**

The framework does not yet demonstrate statistically significant lift over any baseline. Mean PA per hitter-game is 2.9. The approximate standard error of observed wOBA at n=2.9 PA is 0.278, equal to the framework MAE. Single-game wOBA outcomes at this plate appearance count are dominated by sampling noise, not prediction quality.

Model C introduces a continuous exploit score: the product of a hitter's vulnerability magnitude on a given pitch type and the pitcher's usage rate of that pitch against the hitter's handedness. The exploit feature has near-zero activation at 6 games of season data, with only 6 of 48 hitter-game observations generating a non-zero score. The estimated λ=0.78 is in-sample optimized and cannot be interpreted as out-of-sample lift. This is disclosed explicitly in the backtest notebook.

n=48 is insufficient to reject the null hypothesis that the framework performs identically to a naive baseline. Power analysis: approximately 200 hitter-game observations are required for 80% power at alpha=0.05 to detect a 5-10% MAE reduction. That requires roughly 25 additional scored starts.

**What the current evidence supports:**
- The framework is not obviously worse than a naive hitter-baseline
- No systematic bias (+0.019 mean residual)
- Framework outperformed naive-1 in 45.8% of individual hitter-games, consistent with noise

**What the current evidence does not yet support:**
- Framework demonstrably outperforms naive baselines on MAE
- Pitch-type weighting adds signal beyond the hitter's prior overall wOBA
- The self-learning memory layer improves accuracy vs a static prior

These are open empirical questions. The backtest will accumulate through the 2026 season.

### Planned: Historical backtest notebook

A backtest notebook covering approximately 50 starts from the 2025 season is in development. The methodology:

**Setup:** For each start in the backtest sample, generate exp_wOBA predictions using only information available before that game: the pitcher's Statcast history through the day before, and the lineup's prior-season pitch-type splits. No forward-looking data.

**Primary metric:** Mean absolute error between predicted and observed wOBA at the hitter-game level, for hitters with at least 2 PA against the starter.

**Baseline comparisons:**

- *Naive baseline 1:* The hitter's overall prior-season wOBA, ignoring the pitcher entirely.
- *Naive baseline 2:* League average wOBA by handedness matchup, a single number for LHH vs LHP, LHH vs RHP, etc.
- *Naive baseline 3:* The hitter's prior-season wOBA against the pitcher's primary handedness (LHP or RHP) with no pitch-type specificity.

The framework earns its complexity only if it reduces MAE relative to all three baselines. If it does not outperform naive baseline 1 on aggregate, there is no case for the pitch-type weighting. That result, whatever it is, will be documented.

**Error decomposition:**

- MAE by confidence tier (LOW / MODERATE / HIGH)
- MAE by pitcher archetype (LHP starter, RHP bulk, high-Slider, Sinker-heavy, etc.)
- MAE by handedness matchup (LHH vs LHP being the theoretically hardest case)
- Residual distribution to check for systematic bias

**Self-learning layer evaluation:** The backtest will be run twice, once with the memory system active and once with a static prior that does not update from game to game. If the memory layer does not improve MAE over the static version in the out-of-sample period, that is a finding worth documenting, not suppressing.

### Planned: Parameter calibration

A grid search over K (prior strength: 30, 60, 100, 200) and half-life (90, 180, 270, 365 days) using the backtest sample to identify combinations that minimize out-of-sample MAE. Results will replace the current asserted defaults with empirically grounded values, or will document that the results are not sensitive to parameter choice within a reasonable range.

### Planned: Sparse-input stress test

Systematic evaluation of framework behavior as effective sample decreases: pitcher with one 2026 start only, pitcher with no MLB Statcast history, hitter with fewer than 10 PA against a given pitch type across the prior season. The goal is to confirm that the framework degrades gracefully rather than producing confidently wrong outputs.

### Planned: Bayesian hierarchical rebuild

The long-term formulation replaces the frequentist weighted average with a proper hierarchical model: pitcher-type priors, pitch-type priors at the hitter level, and partial pooling by handedness. This addresses the core limitation that the current point estimate is not a posterior mean. Pitch sequencing assumptions will integrate naturally as priors over transition matrices. Target implementation: offseason 2026.

---

## Addressed Methodological Challenges

These are problems the framework has directly resolved, not merely acknowledged. The distinction matters.

### Hitter independence within a game

**The problem:** Hitter outcomes within a game are correlated. If a pitcher's breaking ball is sharp on a given night it is sharp against the whole lineup, not a random subset. Treating each hitter-game observation as independent inflates the apparent informativeness of single-game retrospective scores.

**The fix:** The primary validation metric is now game-level MAE, the absolute error between the predicted mean team wOBA and the observed mean team wOBA. This aggregation absorbs the within-game correlation structure automatically. Hitter-level rows remain in the retrospective as diagnostic decomposition, showing which specific matchups drove the game-level error. They are not the primary validation claim.

**In numbers:** Game-level MAE over 8 scored games is 0.109. The hitter-level figure of 0.278 was inflated by the independence assumption. The game-level figure is the honest headline metric.

### Pitch classification instability

**The problem:** Statcast classification algorithms update periodically. Large year-over-year shifts in pitch type usage that are not accompanied by corresponding movement profile shifts are reclassification artifacts, not repertoire changes. The framework previously had no way to distinguish the two.

**The fix:** The `check_pitch_classification_stability()` function flags pitches where year-over-year usage shifts exceed 15 percentage points AND the pfx_x movement drift is under 0.5 feet. Movement-stable label shifts are flagged as likely reclassifications. Flagged pitches are disclosed in the preview output.

**Example from the live system:** Justin Wrobleski's 4-Seam Fastball fell from 59% (2024) to 28.5% (2025) while his Sinker rose from 2.5% to 20.5%. Both pitches showed pfx_x drift under 0.06 feet between seasons. Statcast likely reclassified the same physical pitch. The framework now treats his 4-Seam and Sinker as a classification-uncertain cluster for exp_wOBA matching rather than treating them as distinct pitch types.

### Analyst-determined assumption inputs

**The problem:** Named assumptions, expected values, tolerances, and importance weights were analyst-specified before each game. Different analysts would produce different values. There was no objective derivation process for the numeric components.

**The fix:** The `derive_pitcher_assumptions()` function computes both expected values and tolerances from the recency-weighted Statcast sample. Expected values are weighted means. Tolerances are derived from observed game-to-game standard deviation, capped at 12 percentage points for pitch usage and 2.0 mph for velocity. The analyst specifies which assumptions to track. All numeric values come from the data.

### No park factor adjustment

**The problem:** The league-average wOBA baseline of .320 was applied uniformly regardless of park. Coors Field and Dodger Stadium have measurably different run environments. This was an unacknowledged systematic error in the fallback baseline used when hitter pitch-type splits fall below the minimum PA threshold.

**The fix:** The `park_adjusted_baseline()` function applies a park factor multiplier to the handedness-specific baseline. Source: Baseball Reference 3-year rolling wOBA park factors. The adjusted baseline replaces the flat .320/.325 whenever a fallback is used. Current park factors: Rogers Centre 0.99, Coors 1.15, Guaranteed Rate 0.98, Dodger Stadium 0.97.

---

## Remaining Limitations

These are honest constraints the framework does not yet resolve.

**Ranking tiers replace strict ordering at low confidence (added April 7, 2026).** Previews now output Edge / Neutral / Suppressed tiers rather than a ranked list 1 through N. Tier thresholds are derived from pitch-mix uncertainty: at LOW confidence (eff_sample < 200), the SE of the mix estimate propagates approximately ±0.045 of uncertainty onto exp_wOBA. Hitters within that band of the team mean are placed in the Neutral tier because the data cannot distinguish them. Thresholds by confidence tier: HIGH ±0.018, MODERATE ±0.030, LOW ±0.045.

**Uncertainty propagation into credible intervals (added April 7, 2026).** The Beta prior strength K is now scaled by pitcher confidence tier rather than held fixed at 60. K_adj = 60 at HIGH, 45 at MODERATE, 30 at LOW. This widens the CI to reflect pitch-mix uncertainty in the input, not just hitter sampling noise in the output. At LOW confidence the 90% CI width is 0.282 vs 0.201 at HIGH.

**Baseline D uses an explicit formula (added April 7, 2026).** ERA-to-wOBA-allowed: wOBA ≈ 0.195 + (ERA × 0.027), park-adjusted. Anchored at ERA=2.0→wOBA=0.249 and ERA=5.0→wOBA=0.330. FIP-based conversion is preferred when FIP is available. ERA is a proxy with luck confounding.

**The point estimate is not a posterior.** exp_wOBA is a weighted average computed before inference, not a Bayesian posterior mean. The credible intervals are genuine Beta posterior quantiles placed around a frequentist point estimate. A fully Bayesian formulation would propagate uncertainty through the pitch mix itself. That is the named long-term direction.

**K = 60 and the recency half-life of 180 days are not yet calibrated at sufficient n.** Grid search at n=56 showed MAE range of 0.0013 across all parameter combinations. Differences are not statistically meaningful. Both will be estimated when n ≥ 200.

**Pitch-type wOBA does not distinguish between pitchers.** A hitter's performance against a Changeup is aggregated across every pitcher who has thrown that pitch to them. Pitch velocity, movement profile, and location are not incorporated into the hitter-pitch match. This is the next feature expansion frontier.

**No adjustment for umpire or weather.** Park factors are now applied. Umpire strike-zone tendencies and weather conditions remain unmodeled. Both affect outcomes in ways that interact with pitcher command and batted ball results.

---

## Scope

Analysis covers the opposing starter's plate appearances. Opener innings are logged in the retrospective but excluded from scoring and memory. Modeling relief pitcher usage or bullpen structure pre-game from public data is not defensible.

---

## Site Structure

```
index.html                           game hub
games/YYYY-MM-DD-pitcher/            pre-game matchup previews
retro/YYYY-MM-DD-pitcher/            post-game retrospectives
analysis/                            R source code
model_memory/                        persistent assumption and hitter audit memory
```

## Reproducibility

Source code is published in `/analysis`. The framework runs on three core scripts:

- `jays_matchup_intel_self_learning.R` — all core functions including multi-year weighted sampling, assumption tracking, and pitch sequencing
- `preview_runner_YYYY-MM-DD.R` — game-specific preview execution
- `retro_runner_YYYY-MM-DD_pitcher.R` — game-specific retrospective execution

Input data: [Baseball Savant](https://baseballsavant.mlb.com) Statcast pitch-level exports. No proprietary data sources.

---

## 2026 Season Log

| Date | Opp | Pitcher | Result | Top Pick | Links |
|------|-----|---------|--------|----------|-------|
| 4-7-26  | LAD | Y. Yamamoto RHP | — | A. Barger (.44) | [Preview](https://scherrology-sys.github.io/jays-matchup-intel/games/2026-04-07-yamamoto/) |
| 4-6-26  | LAD | J. Wrobleski LHP | LAD 14, TOR 2 | V. Guerrero (.42) | [Preview](https://scherrology-sys.github.io/jays-matchup-intel/games/2026-04-06-wrobleski/) · [Retro](https://scherrology-sys.github.io/jays-matchup-intel/retro/2026-04-06-wrobleski/) |
| 4-5-26  | @ CWS | D. Martin RHP | CWS 3, TOR 0 | G. Springer (.43) | [Preview](https://scherrology-sys.github.io/jays-matchup-intel/games/2026-04-05-martin/) · [Retro](https://scherrology-sys.github.io/jays-matchup-intel/retro/2026-04-05-martin/) |
| 4-4-26  | @ CWS | A. Kay LHP · G. Taylor (opener) | CWS 6, TOR 3 | G. Springer (.43) | [Preview](https://scherrology-sys.github.io/jays-matchup-intel/games/2026-04-04-kay-taylor/) · [Retro](https://scherrology-sys.github.io/jays-matchup-intel/retro/2026-04-04-kay-taylor/) |
| 4-3-26  | @ CWS | S. Burke RHP · G. Taylor (opener) | CWS 5, TOR 4 F/10 | G. Springer (.41) | [Preview](https://scherrology-sys.github.io/jays-matchup-intel/games/2026-04-03-burke-taylor/) · [Retro](https://scherrology-sys.github.io/jays-matchup-intel/retro/2026-04-03-burke-taylor/) |
| 4-1-26  | COL | K. Freeland LHP | COL 2, TOR 1 | G. Springer (.44) | [Preview](https://scherrology-sys.github.io/jays-matchup-intel/games/2026-04-01-freeland/) · [Retro](https://scherrology-sys.github.io/jays-matchup-intel/retro/2026-04-01-freeland/) |
| 3-31-26 | COL | R. Feltner RHP | TOR 5, COL 1 | G. Springer (.42) | [Preview](https://scherrology-sys.github.io/jays-matchup-intel/games/2026-03-31-feltner/) · [Retro](https://scherrology-sys.github.io/jays-matchup-intel/retro/2026-03-31-feltner/) |
| 3-30-26 | COL | T. Sugano RHP | COL 14, TOR 5 | G. Springer (.40) | [Preview](https://scherrology-sys.github.io/jays-matchup-intel/games/2026-03-30-sugano/) · [Retro](https://scherrology-sys.github.io/jays-matchup-intel/retro/2026-03-30-sugano/) |

**2026 record: 4-6 · Colorado: TOR 1-2 · Chicago White Sox: TOR 0-3 · Los Angeles Dodgers: TOR 0-1**

*Updated: April 7, 2026*

---

Built by **Arm Chair Analyst** · [@scherrology](https://x.com/scherrology) · [GitHub](https://github.com/scherrology-sys)
