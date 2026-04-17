# Jays Matchup Intel

**Daily pre-game matchup intelligence and post-game retrospective analysis for the Toronto Blue Jays.**
Built and published by [@scherrology](https://x.com/scherrology) under the **Arm Chair Analyst** brand.

**Live site:** [scherrology-sys.github.io/jays-matchup-intel](https://scherrology-sys.github.io/jays-matchup-intel)

---

## TL;DR

- Pitch-mix aware model predicting lineup offensive output against a specific starting pitcher
- Model locked April 7, 2026. No mid-season tuning. Season is the data collection period.
- **H1 supported:** +1.7% MAE lift over naive baseline (n=569, 81 games), strongest signal in LHH matchups (+4-5%)
- **H2 partial:** In-season updating improves hitter ranking (+62% Spearman rho) but not calibration error
- **Bayesian hierarchical challenger** in development to replace the frequentist point estimate with a proper posterior
- Live site: [scherrology-sys.github.io/jays-matchup-intel](https://scherrology-sys.github.io/jays-matchup-intel)

---

## BLUF

This project tests whether a pitch-mix aware model can improve estimates of lineup offensive output against a specific starting pitcher. The model estimates lineup performance conditional on a pitcher's arsenal, not just hitter talent.

This is a prospective, pre-registered evaluation. The model is fixed and evaluated over the 2026 season without adjustment.

**Current answer from 569 historical observations (2025 season):** H1 supported at +1.7% MAE lift over naive baseline, strongest signal in LHH matchups (+4-5%). H2 shows ranking improvement (+62% Spearman rho) without MAE reduction, suggesting the model is more valuable as a ranking instrument than a prediction instrument.

---

## Why This Matters

Traditional projections treat hitters as static. In reality, performance is conditional on how pitchers attack them. A hitter with strong fastball splits facing a pitcher who throws 60% breaking balls is in a fundamentally different environment than his season line suggests.

This system tests whether incorporating pitcher-specific pitch mix provides a measurable edge in predicting lineup performance and hitter ordering. If it does, it suggests matchup-aware modeling is a more effective framework than talent-only baselines. If it does not, that is an equally important finding and it will be documented as such.

---

## What This Is

Before every Jays game, a pre-game matchup preview is published. After the game, a retrospective scores the prediction against observed outcomes and updates the system's working knowledge of that pitcher going forward.

That loop is the point. Prediction, assumption audit, scoring, memory. A single preview is directional. The season is the validation.

---

## Research Framework

**Model locked: April 7, 2026.** Parameters do not change mid-season. The season is the data collection period. No tweaking.

**H1:** A pitch-mix weighted expectation model produces more accurate estimates of lineup offensive output against a given starter than a naive hitter-baseline that ignores the pitcher's arsenal.

**H2:** As in-season data accumulates, updating hitter pitch-type performance toward 2026 actuals improves those estimates beyond what the prior-season baseline alone produces.

Target n=200 for H1/H2 inference. All outcomes — confirmation, rejection, inconclusive — are valid findings.

---

### Bayesian hierarchical challenger: in development (started April 7, 2026)

Model specified, governed, and waiting for sufficient data. First fit at n=200 hitter-game observations (~25 scored games). Running n is currently 63.

Why not run yet: at n=63 the confidence interval around any MAE comparison is wider than any plausible effect size. The governance rule that protects the champion from premature replacement applies equally to the challenger evaluation. Both directions enforced.

**v1 structure (brms):** Normal likelihood on hitter-game wOBA, heteroskedastic variance tied to PA. Hitter and pitcher random intercepts (partially pooled). Hitter × pitch-family random slopes. Weakly informative priors centered at league average wOBA (.320).

**Pitch families:** Fastball (4-Seam, Sinker, Cutter) · Breaking (Slider, Curveball, Sweeper) · Offspeed (Changeup, Splitter) · Undefined. Statistical pooling scaffold for inference stability, not claims of pitch identity.

**Promotion criteria:** Beats champion on game-level MAE, improves ranking accuracy (Spearman rho, top-3 overlap), holds across n≥200, no systematic bias introduced. No exceptions.

**v2 direction:** PA-level model where event outcomes are modeled directly and wOBA is posterior-predictive rather than directly modeled. Triggered if v1 shows meaningful lift.


---

## Validation Roadmap

### H1 — Historical backtest: complete (2025 season, 81 games, n=569)

Full pitcher career Statcast data pulled for 40 starters (2022-2025). Hitter priors from 2024 season. No forward-looking data at any step.

| Model | MAE | Lift vs N1 |
|----|---|------|
| Champion (pitch-mix weighted) | 0.268 | +1.7% |
| Naive-1 (hitter prior wOBA) | 0.272 | — |
| Naive-2 (platoon + park) | 0.269 | +1.4% |

Game-level MAE: **0.105** · Spearman rho: **+0.075** · Top-3 overlap: **43.9%** (chance: 33%) · 57% HIGH confidence

**H1 verdict: SUPPORTS H1.** Lift is concentrated in LHH matchups (+4-5%). RHH vs RHP shows near-zero lift. Full handedness breakdown and confidence tier decomposition in [`/docs/validation.md`](docs/validation.md).

---

### H2 — In-season updating backtest: complete (2025 season, n=569)

Same 81-game sample. Static 2024 prior (H1) versus in-season blended prior incorporating each hitter's 2025 actuals before each game date.

| Model | MAE | Spearman rho | Top-3 overlap |
|----|---|-------|--------|
| H1 static prior | 0.268 | +0.075 | 43.9% |
| H2 in-season blend | 0.269 | +0.122 | 47.7% |

**H2 on calibration: DOES NOT SUPPORT.** Mean prediction shift of 0.019 is below the noise floor.

**H2 on ranking: SIGNAL PRESENT.** Spearman rho +62% relative. Top-3 overlap +3.8pp. Effect strengthens late in season as in-season data accumulates.

H2 separates into two distinct questions with different answers. Calibration error does not improve. Hitter ordering does. Full monthly breakdown and verification notes in [`/docs/validation.md`](docs/validation.md).

---

### 2026 in-season running backtest (8 games, n=63)

| Model | MAE | Bias | Lift vs N1 |
|----|---|---|------|
| Champion | 0.255 | +0.045 | −0.0% |
| Naive-1 | 0.254 | +0.043 | — |
| Naive-2 | 0.238 | +0.027 | +6.4% |

Game-level MAE: **0.108** · Mean Spearman rho: **−0.159**

Positive bias (+0.045) driven by 2025 prior being too generous for the 2026 Jays roster. Ranking metrics negative at n=63 — noise at this sample size. Consistent with the historical backtest showing ranking signal emerging later in the season as in-season data builds.

**Combined study n: 569 (historical) + 63 (2026 season) = 632 hitter-games across 89 games.**

---

### Parameter calibration and sparse-input stress test: complete

Grid search confirmed current defaults (HL=180, MinPA=10) are defensible at n=63. MAE range across 12 parameter combinations: 0.0013. Sparse-input test passed — framework degrades gracefully to park baseline at eff=0. Full detail in [`/docs/validation.md`](docs/validation.md).

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
|---|-------------|--------|
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
| 4-17-26 | ARI | M. Soroka RHP | — | V. Guerrero Jr. (.413) | [Preview](https://scherrology-sys.github.io/jays-matchup-intel/games/2026-04-17-soroka/) |
| 4-16-26 | MIL | B. Sproat RHP | MIL 2, TOR 1 | V. Guerrero Jr. (.391) | [Preview](https://scherrology-sys.github.io/jays-matchup-intel/games/2026-04-16-sproat/) · [Retro](https://scherrology-sys.github.io/jays-matchup-intel/retro/2026-04-16-sproat/) |
| 4-15-26 | MIL | C. Patrick RHP | MIL 2, TOR 1 | V. Guerrero Jr. (.358) | [Preview](https://scherrology-sys.github.io/jays-matchup-intel/games/2026-04-15-patrick/) · [Retro](https://scherrology-sys.github.io/jays-matchup-intel/retro/2026-04-15-patrick/) |
| 4-14-26 | MIL | J. Misiorowski RHP | TOR 9, MIL 7 (F/10) | V. Guerrero Jr. (.355) | [Preview](https://scherrology-sys.github.io/jays-matchup-intel/games/2026-04-14-misiorowski/) · [Retro](https://scherrology-sys.github.io/jays-matchup-intel/retro/2026-04-14-misiorowski/) |
| 4-12-26 | MIN | T. Bradley RHP | MIN 8, TOR 2 | V. Guerrero Jr. (.375) | [Preview](https://scherrology-sys.github.io/jays-matchup-intel/games/2026-04-12-bradley/) · [Retro](https://scherrology-sys.github.io/jays-matchup-intel/retro/2026-04-12-bradley/) |
| 4-11-26 | MIN | J. Ryan RHP | MIN 7, TOR 4 | V. Guerrero Jr. (.402) | [Preview](https://scherrology-sys.github.io/jays-matchup-intel/games/2026-04-11-ryan/) · [Retro](https://scherrology-sys.github.io/jays-matchup-intel/retro/2026-04-11-ryan/) |
| 4-10-26 | MIN | S. Woods Richardson RHP | TOR 10, MIN 4 | V. Guerrero Jr. (.360) | [Preview](https://scherrology-sys.github.io/jays-matchup-intel/games/2026-04-10-woods-richardson/) · [Retro](https://scherrology-sys.github.io/jays-matchup-intel/retro/2026-04-10-woods-richardson/) |
| 4-8-26  | LAD | S. Ohtani RHP | TOR 4, LAD 3 | G. Springer (.44) | [Preview](https://scherrology-sys.github.io/jays-matchup-intel/games/2026-04-08-ohtani/) · [Retro](https://scherrology-sys.github.io/jays-matchup-intel/retro/2026-04-08-ohtani/) |
| 4-7-26  | LAD | Y. Yamamoto RHP | LAD 4, TOR 1 | G. Springer (.42) | [Preview](https://scherrology-sys.github.io/jays-matchup-intel/games/2026-04-07-yamamoto/) · [Retro](https://scherrology-sys.github.io/jays-matchup-intel/retro/2026-04-07-yamamoto/) |
| 4-6-26  | LAD | J. Wrobleski LHP | LAD 14, TOR 2 | V. Guerrero (.42) | [Preview](https://scherrology-sys.github.io/jays-matchup-intel/games/2026-04-06-wrobleski/) · [Retro](https://scherrology-sys.github.io/jays-matchup-intel/retro/2026-04-06-wrobleski/) |
| 4-5-26  | @ CWS | D. Martin RHP | CWS 3, TOR 0 | G. Springer (.43) | [Preview](https://scherrology-sys.github.io/jays-matchup-intel/games/2026-04-05-martin/) · [Retro](https://scherrology-sys.github.io/jays-matchup-intel/retro/2026-04-05-martin/) |
| 4-4-26  | @ CWS | A. Kay LHP · G. Taylor (opener) | CWS 6, TOR 3 | G. Springer (.43) | [Preview](https://scherrology-sys.github.io/jays-matchup-intel/games/2026-04-04-kay-taylor/) · [Retro](https://scherrology-sys.github.io/jays-matchup-intel/retro/2026-04-04-kay-taylor/) |
| 4-3-26  | @ CWS | S. Burke RHP · G. Taylor (opener) | CWS 5, TOR 4 F/10 | G. Springer (.41) | [Preview](https://scherrology-sys.github.io/jays-matchup-intel/games/2026-04-03-burke-taylor/) · [Retro](https://scherrology-sys.github.io/jays-matchup-intel/retro/2026-04-03-burke-taylor/) |
| 4-1-26  | COL | K. Freeland LHP | COL 2, TOR 1 | G. Springer (.44) | [Preview](https://scherrology-sys.github.io/jays-matchup-intel/games/2026-04-01-freeland/) · [Retro](https://scherrology-sys.github.io/jays-matchup-intel/retro/2026-04-01-freeland/) |
| 3-31-26 | COL | R. Feltner RHP | TOR 5, COL 1 | G. Springer (.42) | [Preview](https://scherrology-sys.github.io/jays-matchup-intel/games/2026-03-31-feltner/) · [Retro](https://scherrology-sys.github.io/jays-matchup-intel/retro/2026-03-31-feltner/) |
| 3-30-26 | COL | T. Sugano RHP | COL 14, TOR 5 | G. Springer (.40) | [Preview](https://scherrology-sys.github.io/jays-matchup-intel/games/2026-03-30-sugano/) · [Retro](https://scherrology-sys.github.io/jays-matchup-intel/retro/2026-03-30-sugano/) |

**2026 record: 7-11 · Colorado: TOR 1-2 · Chicago White Sox: TOR 0-3 · Los Angeles Dodgers: TOR 1-2 · Minnesota: TOR 1-2 · Milwaukee: TOR 1-2**

*Updated: April 17, 2026*

---

Built by **Arm Chair Analyst** · [@scherrology](https://x.com/scherrology) · [GitHub](https://github.com/scherrology-sys)
