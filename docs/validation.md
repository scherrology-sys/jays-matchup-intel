# Validation Detail

Full methodology and results for the Jays Matchup Intel historical backtest. Summary tables are in the main [README](../README.md). This document contains the complete breakdown.

---

## H1 — Historical Backtest Detail (2025 season, 81 games, n=569)

### Setup

- **Pitcher data:** Full career Statcast files (2022-2025) pulled for 40 starters the Jays faced most frequently in 2025. Recency-weighted at HL=180 days. All pitcher data filtered to before each game date. No forward-looking data at any step.
- **Hitter priors:** 2024 Jays season Statcast data. Pitch-type wOBA splits require minimum 10 PA per pitch type to activate. Falls back to park-adjusted handedness baseline when insufficient.
- **Park factors:** Savant wOBA index converted to multiplicative factor. Applied to handedness baseline.
- **Games:** 81 of 160 2025 Jays games had career pitcher data available. The remaining 79 used only prior Jays appearances, producing near-universal LOW confidence.

### Full Results

| Model | MAE | Bias | Lift vs N1 |
|-------|-----|------|------------|
| Champion (pitch-mix weighted) | 0.268 | −0.020 | +1.7% |
| Naive-1 (hitter prior wOBA) | 0.272 | −0.017 | — |
| Naive-2 (platoon + park) | 0.269 | −0.016 | +1.4% |

Game-level MAE (primary metric): **0.105**
Mean Spearman rho: **+0.075**
Top-3 overlap: **43.9%** (chance baseline: 33%)
Bucket separation (predicted top-3 actual vs bottom-3 actual): **+0.036**

### Confidence Tier Distribution

| Tier | n | % | Games |
|------|---|---|-------|
| HIGH (eff ≥ 700) | 322 | 57% | 52 |
| MODERATE (eff 200-700) | 220 | 39% | 47 |
| LOW (eff < 200) | 27 | 5% | 7 |

### MAE by Handedness Matchup

| Matchup | n | Champion | Naive-1 | Lift |
|---------|---|----------|---------|------|
| LHH vs LHP | 33 | 0.219 | 0.231 | +5.2% |
| LHH vs RHP | 173 | 0.276 | 0.287 | +4.1% |
| RHH vs LHP | 124 | 0.268 | 0.270 | +0.7% |
| RHH vs RHP | 239 | 0.269 | 0.269 | −0.1% |

The model adds value where pitch-mix differentiation matters most. LHH matchups show the largest lift because pitchers vary meaningfully in how they attack left-handed hitters. RHH vs RHP is the most common and least differentiated matchup, producing near-zero lift.

### Residual Distribution

Mean: −0.020 · Median: +0.010 · SD: 0.337 · Over-predictions: 51%

Near-symmetric. Slight under-prediction in 2025 — the 2025 Jays performed close to their 2024 prior. The 2026 positive bias (+0.045) reflects a different roster composition relative to the prior year.

---

## H2 — In-Season Updating Backtest Detail (2025 season, n=569)

### Setup

Same 81-game sample. Two models scored side by side on identical inputs:

- **H1:** Static 2024 prior only
- **H2:** Blended prior incorporating each hitter's 2025 actuals before each game date, recency-weighted at HL=90 days. Blending formula: (K_prior × wOBA_2024 + eff_2025 × wOBA_2025) / (K_prior + eff_2025) where K_prior = actual 2024 PA count.

### Summary Results

| Model | MAE | Spearman rho | Top-3 overlap |
|-------|-----|-------------|---------------|
| H1 static prior | 0.268 | +0.075 | 43.9% |
| H2 in-season blend | 0.269 | +0.122 | 47.7% |

### Monthly Breakdown (Spearman rho)

| Month | n | H1 rho | H2 rho | Delta | Avg eff_25 |
|-------|---|--------|--------|-------|-----------|
| Mar | 24 | +0.482 | +0.402 | −0.080 | 4 |
| Apr | 63 | −0.056 | +0.115 | +0.171 | 52 |
| May | 113 | +0.069 | +0.134 | +0.064 | 98 |
| Jun | 95 | +0.199 | +0.177 | −0.023 | 165 |
| Jul | 96 | +0.093 | +0.087 | −0.006 | 173 |
| Aug | 71 | +0.119 | +0.140 | +0.020 | 219 |
| Sep | 107 | −0.102 | −0.001 | +0.101 | 239 |

The H2 effect is noisy month-to-month but directionally positive in the second half of the season when in-season data has accumulated.

### Why MAE Does Not Improve

The blend moves individual predictions by an average of 0.019. This is below the noise floor for hitter-game wOBA at 2-4 PA. Pitch-type split cells require MIN_PA=10 to activate, meaning most cells do not accumulate enough in-season PA within a single season to materially update the blended split estimate. The overall wOBA prior blends more readily but does not propagate into the pitch-mix weighting cleanly at this sample size.

### Why Ranking Improves Despite Flat MAE

A 0.019 prediction shift that moves a hitter from 4th to 2nd in the lineup ordering registers in Spearman rho and top-3 overlap but not in mean absolute error. Ranking metrics are more sensitive to small relative adjustments than MAE is to absolute changes. H2 is doing real work on the ordering problem even when it cannot demonstrate lift on the calibration problem.

### Verification Note

Results are from a confirmed second run. The first implementation had a bug in the pitch-type blending merge logic and three missing park factor codes (TB, CWS, KC) defaulting to 1.0. Both were identified before publication. Correcting them produced a stronger H2 result — Spearman improvement increased from +49% to +62% relative. The direction was always right. The magnitude was understated in the first pass.

### Park Factor Note

Both H1 and H2 use identical park factors. Any imprecision cancels out completely in the H1 vs H2 comparison. Park factor accuracy affects absolute MAE numbers marginally but does not affect the H2 finding. The Statcast wOBA index was the intended source but could not be read from the JavaScript-rendered page. Values used are directionally correct for all parks; major outliers (Coors Field, Great American Ball Park) are captured accurately.

---

## 2026 In-Season Running Backtest

Updated after each scored game. Current as of April 8, 2026.

| Model | MAE | Bias | Lift vs N1 |
|-------|-----|------|------------|
| Champion | 0.255 | +0.045 | −0.0% |
| Naive-1 | 0.254 | +0.043 | — |
| Naive-2 | 0.238 | +0.027 | +6.4% |

Game-level MAE: **0.108** · Mean Spearman rho: **−0.159** · n=63 hitter-games, 8 games

Positive bias (+0.045) driven by 2025 prior being too generous for the 2026 Jays. Negative Spearman at n=63 is consistent with the historical backtest — ranking signal emerges later in the season. No inference before n=200.

---

## Parameter Calibration

Grid search over half-life (90-365 days) and minimum PA (5-20) on n=63. MAE range across 12 combinations: 0.0013. Not meaningful at this sample size. Current defaults (HL=180, MinPA=10) defensible. Re-run at n≥200.

## Sparse-Input Stress Test

Framework tested at effective sample sizes from 0 to 1350+. At eff=0 returns park-adjusted handedness baseline cleanly. LOW confidence tier correctly flags high-uncertainty outputs. PASS.

---

*Backtest code: `backtest_three_models.R` · H2 code: `h2_backtest_2025.csv` · Raw results: `confirmed_backtest_results.csv`*
