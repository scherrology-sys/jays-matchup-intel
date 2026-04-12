# Jays Matchup Intel

**[@scherrology](https://x.com/scherrology) · Arm Chair Analyst**
**Live site:** [scherrology-sys.github.io/jays-matchup-intel](https://scherrology-sys.github.io/jays-matchup-intel)

---

## BLUF

This project tests whether a pitch-mix aware model improves estimates of lineup offensive output against a specific starting pitcher.

Result (2025, n=569):
+1.7% MAE lift vs hitter-only baseline
Signal concentrated in LHH matchups (+4–5%)

Ranking signal:
+62% Spearman rho with in-season updating
No calibration improvement

Conclusion:
Pitch-mix adds value as a ranking tool, not a prediction improvement

Model locked April 7, 2026. No mid-season tuning. The 2026 season is a prospective evaluation.

A Bayesian hierarchical challenger is in development. Promotion requires pre-defined criteria at n ≥ 200 out-of-sample hitter-games.

---

## What Is Being Tested

Do hitters perform differently when evaluated against how pitchers attack them, not just who they are?

- If yes → matchup-aware modeling adds signal beyond talent-only baselines
- If no → hitter-only projections are sufficient

Both outcomes are valid.

---

## Validation

**H1 · Historical backtest (2025, n=569, 81 games)**

| Metric | Champion | Naive-1 | Lift |
|--------|----------|---------|------|
| Hitter MAE | 0.268 | 0.272 | +1.7% |
| Game-level MAE | 0.105 | — | — |
| Top-3 overlap | 43.9% | 33.3% (chance) | +10.6pp |

Verdict: **Supported.** Signal concentrated in LHH matchups (+4–5%). Near zero for RHH vs RHP.

**H2 · In-season updating (same n=569)**

| Metric | Static prior | In-season blend | Change |
|--------|-------------|----------------|--------|
| MAE | 0.268 | 0.269 | none |
| Spearman rho | +0.075 | +0.122 | +62% |
| Top-3 overlap | 43.9% | 47.7% | +3.8pp |

Verdict: **Calibration not supported. Ranking signal present.**

**Key distinction:** Calibration and discrimination are separate objectives. This framework shows limited calibration gain but meaningful improvement in hitter ordering.

**2026 in-season (n≈83)**

No inference before n ≥ 200. Early results show no lift and unstable ranking metrics, consistent with expected small-sample noise.

---

## Methodological Challenges Addressed

These are problems the framework has directly resolved.

**Within-game hitter correlation.** Hitter outcomes within a game are correlated. The primary validation metric is game-level MAE, which absorbs within-game correlation automatically. Hitter-level rows are diagnostic decomposition, not the primary claim.

**Pitch classification instability.** Year-over-year usage shifts not accompanied by movement profile changes are reclassification artifacts. A stability check flags pitches where usage shifts exceed 15pp and pfx_x drift is under 0.5 feet. Flagged pitches are disclosed in preview output.

**Analyst-determined assumption inputs.** Expected values and tolerances are computed from the recency-weighted Statcast sample. Expected values are weighted means. Tolerances derive from observed game-to-game standard deviation, capped at 12pp for pitch usage and 2.0 mph for velocity.

**No park factor adjustment.** The `park_adjusted_baseline()` function applies park factor multipliers to handedness-specific baselines. Source: Baseball Reference 3-year rolling wOBA park factors.

**In-season pitch mix blending weights are empirically derived via grid search (n=628).** MAE sensitivity is negligible (Δ=0.0005), while Spearman rho improves +23% with current-year weighting. Blending rule: n_curr < 30 → career only; 30–74 → 50/50; 75–149 → 70/30; 150+ → 90/10.

---

## Bayesian Hierarchical Challenger

The current champion uses a frequentist point estimate with Beta-Binomial credible intervals. The Bayesian hierarchical challenger replaces this with a proper posterior.

**Specified architecture (brms):** Normal likelihood, hitter and pitcher random intercepts, hitter × pitch-family random slopes, weakly informative priors centered at .320.

**Promotion criteria (all required, no exceptions):**
- Beats champion on game-level MAE
- Improves ranking accuracy (Spearman rho, top-3 overlap)
- Holds across n ≥ 200 out-of-sample hitter-game observations
- No systematic bias

**Status:** In development. First fit at n=200. Running n currently ~83.

---

## Falsification Criteria

The pitch-mix framework is rejected if:

- It fails to outperform a simple team-level wOBA baseline at n ≥ 200
- Lift does not persist as sample size grows
- Ranking improvements disappear with sufficient in-season data
- Gains are fully explained by handedness splits alone

This is a test of whether the framework works, not a proof that it does.

---

## Remaining Limitations

**The point estimate is not a posterior.** exp_wOBA is a weighted average placed before inference, not a Bayesian posterior mean. The credible intervals are Beta posterior quantiles around a frequentist point estimate. The Bayesian challenger addresses this.

**Pitch-type wOBA does not distinguish between pitchers.** A hitter's Slider performance is aggregated across every pitcher who has thrown that pitch to them. Velocity, movement, and location are not incorporated into the hitter-pitch match.

**K=60 and half-life of 180 days are not calibrated at sufficient n.** Grid search at current n showed MAE range of 0.0013 across all parameter combinations. Not meaningful at this sample size. Both will be estimated at n ≥ 200.

**No adjustment for umpire or weather.** Park factors are applied. Umpire tendencies and weather remain unmodeled.

---

## Scope

Analysis covers the opposing starter's plate appearances only. Openers are logged but excluded from scoring. Modeling bullpen usage pre-game from public data is not defensible.

---

## Reproducibility

```
code/
  jays_matchup_helpers.R          public constants and utilities
  preview_runner_YYYY-MM-DD.R     game-specific preview execution
  retro_runner_YYYY-MM-DD.R       game-specific retro execution
```

Core engine is not included. Runners are self-contained for execution, data loading, and assumption specification. Input data: [Baseball Savant](https://baseballsavant.mlb.com) Statcast pitch-level exports. No proprietary data sources.

---

## Site Structure

```
index.html                     game hub
games/YYYY-MM-DD-pitcher/      pre-game matchup previews
retro/YYYY-MM-DD-pitcher/      post-game retrospectives
docs/validation.md             full methodology detail
```

---

## 2026 Season Log

| Date | Opp | Pitcher | Result | Top Pick | Links |
|------|-----|---------|--------|----------|-------|
| 4-12-26 | MIN | T. Bradley RHP | — | V. Guerrero Jr. (.375) | [Preview](https://scherrology-sys.github.io/jays-matchup-intel/games/2026-04-12-bradley/) |
| 4-11-26 | MIN | J. Ryan RHP | MIN 7, TOR 4 | V. Guerrero Jr. (.409) | [Preview](https://scherrology-sys.github.io/jays-matchup-intel/games/2026-04-11-ryan/) · [Retro](https://scherrology-sys.github.io/jays-matchup-intel/retro/2026-04-11-ryan/) |
| 4-10-26 | MIN | S. Woods Richardson RHP | TOR 10, MIN 4 | V. Guerrero Jr. (.397) | [Preview](https://scherrology-sys.github.io/jays-matchup-intel/games/2026-04-10-woods-richardson/) · [Retro](https://scherrology-sys.github.io/jays-matchup-intel/retro/2026-04-10-woods-richardson/) |
| 4-8-26 | LAD | S. Ohtani RHP | TOR 4, LAD 3 | G. Springer (.44) | [Preview](https://scherrology-sys.github.io/jays-matchup-intel/games/2026-04-08-ohtani/) · [Retro](https://scherrology-sys.github.io/jays-matchup-intel/retro/2026-04-08-ohtani/) |
| 4-7-26 | LAD | Y. Yamamoto RHP | LAD 4, TOR 1 | G. Springer (.42) | [Preview](https://scherrology-sys.github.io/jays-matchup-intel/games/2026-04-07-yamamoto/) · [Retro](https://scherrology-sys.github.io/jays-matchup-intel/retro/2026-04-07-yamamoto/) |
| 4-6-26 | LAD | J. Wrobleski LHP | LAD 14, TOR 2 | V. Guerrero (.42) | [Preview](https://scherrology-sys.github.io/jays-matchup-intel/games/2026-04-06-wrobleski/) · [Retro](https://scherrology-sys.github.io/jays-matchup-intel/retro/2026-04-06-wrobleski/) |
| 4-5-26 | @ CWS | D. Martin RHP | CWS 3, TOR 0 | G. Springer (.43) | [Preview](https://scherrology-sys.github.io/jays-matchup-intel/games/2026-04-05-martin/) · [Retro](https://scherrology-sys.github.io/jays-matchup-intel/retro/2026-04-05-martin/) |
| 4-4-26 | @ CWS | A. Kay LHP | CWS 6, TOR 3 | G. Springer (.43) | [Preview](https://scherrology-sys.github.io/jays-matchup-intel/games/2026-04-04-kay-taylor/) · [Retro](https://scherrology-sys.github.io/jays-matchup-intel/retro/2026-04-04-kay-taylor/) |
| 4-3-26 | @ CWS | S. Burke RHP | CWS 5, TOR 4 F/10 | G. Springer (.41) | [Preview](https://scherrology-sys.github.io/jays-matchup-intel/games/2026-04-03-burke-taylor/) · [Retro](https://scherrology-sys.github.io/jays-matchup-intel/retro/2026-04-03-burke-taylor/) |
| 4-1-26 | COL | K. Freeland LHP | COL 2, TOR 1 | G. Springer (.44) | [Preview](https://scherrology-sys.github.io/jays-matchup-intel/games/2026-04-01-freeland/) · [Retro](https://scherrology-sys.github.io/jays-matchup-intel/retro/2026-04-01-freeland/) |
| 3-31-26 | COL | R. Feltner RHP | TOR 5, COL 1 | G. Springer (.42) | [Preview](https://scherrology-sys.github.io/jays-matchup-intel/games/2026-03-31-feltner/) · [Retro](https://scherrology-sys.github.io/jays-matchup-intel/retro/2026-03-31-feltner/) |
| 3-30-26 | COL | T. Sugano RHP | COL 14, TOR 5 | G. Springer (.40) | [Preview](https://scherrology-sys.github.io/jays-matchup-intel/games/2026-03-30-sugano/) · [Retro](https://scherrology-sys.github.io/jays-matchup-intel/retro/2026-03-30-sugano/) |

**2026 record: 6-8 · Colorado: TOR 1-2 · Chicago White Sox: TOR 0-3 · Los Angeles Dodgers: TOR 1-2 · Minnesota: TOR 1-1**

*Updated: April 12, 2026*

---

Built by **Arm Chair Analyst** · [@scherrology](https://x.com/scherrology) · [GitHub](https://github.com/scherrology-sys)
