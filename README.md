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

## Limitations

These are honest. Some are addressable with a more complete statistical formulation. Some are structural to the problem.

**The point estimate is not a posterior.** exp_wOBA is a weighted average computed before inference, not a Bayesian posterior mean. The credible intervals are genuine Beta posterior quantiles, but they are placed around a frequentist point estimate. The two are not equivalent. A fully Bayesian formulation would propagate uncertainty through the pitch mix itself and produce the point estimate as a posterior mean with partial pooling across pitcher archetypes. That is the named long-term direction for this framework.

**The prior strength K = 60 and the recency half-life of 180 days are asserted defaults, not calibrated parameters.** The correct values depend on the distributional properties of pitcher evolution and hitter variance across seasons. Neither has been validated against historical prediction accuracy. Both represent assumptions that may be meaningfully wrong.

**Single-game scoring is dominated by measurement noise.** The standard error of observed wOBA in a 2-plate-appearance sample is approximately 0.337. The retrospective threshold for flagging an outcome as outside expected variance is a residual of 0.050, well below one standard deviation of sampling noise at this scale. Individual game verdicts are directional signals at best. Season-level mean absolute error is the appropriate unit of evaluation.

**Pitch-type wOBA does not distinguish between pitchers.** A hitter's performance against "Changeup" is aggregated across every pitcher who has thrown that pitch to them. No two pitchers throw the same Changeup. Pitch velocity, movement, and release point are not incorporated into the hitter-pitch match; only the categorical pitch type label is used.

**Hitter outcomes within a game are treated as independent.** In practice they are correlated. If a pitcher's Changeup is effective on a given day, it is effective against the entire lineup, not a random subset. This correlation structure is not modeled, which inflates the apparent informativeness of single-game retrospective scores.

**Pitch classification is not perfectly stable across seasons.** Statcast classification algorithms are periodically updated. A pitch recorded as "Cutter" in one season may have been recorded as "Slider" in another. The framework cannot distinguish a genuine repertoire change from a reclassification artifact.

**Assumption inputs are analyst-determined.** Named assumptions, expected values, tolerances, and importance weights are specified by the analyst before each game. There is no objective derivation process. A different analyst would make different choices. The assumption framework is the most transparent component of the system and also the most subjective.

**No adjustment for run environment, umpire, or weather.** The league-average wOBA baseline of .320 is applied uniformly. Park factors, umpire strike-zone tendencies, and weather conditions (wind, temperature) affect outcomes in ways that interact with pitcher command and batted ball results. None are incorporated.

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
| 4-5-26  | @ CWS | D. Martin RHP | CWS 3, TOR 0 | G. Springer (.43) | [Preview](https://scherrology-sys.github.io/jays-matchup-intel/games/2026-04-05-martin/) · [Retro](https://scherrology-sys.github.io/jays-matchup-intel/retro/2026-04-05-martin/) |
| 4-4-26  | @ CWS | A. Kay LHP · G. Taylor (opener) | CWS 6, TOR 3 | G. Springer (.43) | [Preview](https://scherrology-sys.github.io/jays-matchup-intel/games/2026-04-04-kay-taylor/) · [Retro](https://scherrology-sys.github.io/jays-matchup-intel/retro/2026-04-04-kay-taylor/) |
| 4-3-26  | @ CWS | S. Burke RHP · G. Taylor (opener) | CWS 5, TOR 4 F/10 | G. Springer (.41) | [Preview](https://scherrology-sys.github.io/jays-matchup-intel/games/2026-04-03-burke-taylor/) · [Retro](https://scherrology-sys.github.io/jays-matchup-intel/retro/2026-04-03-burke-taylor/) |
| 4-1-26  | COL | K. Freeland LHP | COL 2, TOR 1 | G. Springer (.44) | [Preview](https://scherrology-sys.github.io/jays-matchup-intel/games/2026-04-01-freeland/) · [Retro](https://scherrology-sys.github.io/jays-matchup-intel/retro/2026-04-01-freeland/) |
| 3-31-26 | COL | R. Feltner RHP | TOR 5, COL 1 | G. Springer (.42) | [Preview](https://scherrology-sys.github.io/jays-matchup-intel/games/2026-03-31-feltner/) · [Retro](https://scherrology-sys.github.io/jays-matchup-intel/retro/2026-03-31-feltner/) |
| 3-30-26 | COL | T. Sugano RHP | COL 14, TOR 5 | G. Springer (.40) | [Preview](https://scherrology-sys.github.io/jays-matchup-intel/games/2026-03-30-sugano/) · [Retro](https://scherrology-sys.github.io/jays-matchup-intel/retro/2026-03-30-sugano/) |

**2026 record: 4-6 · Colorado: TOR 1-2 · Chicago White Sox: TOR 0-3 · Series lost 0-3**

*Updated: April 5, 2026*

---

Built by **Arm Chair Analyst** · [@scherrology](https://x.com/scherrology) · [GitHub](https://github.com/scherrology-sys)
