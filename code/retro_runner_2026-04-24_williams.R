# =============================================================================
# Jays Matchup Intel V3.2
# Retro Runner · TOR vs CLE · April 24, 2026
# Pitcher: Gavin Williams (RHP) · ID: 668909
# Result: CLE 8, TOR 6 · Observation 23
# @scherrology | Arm Chair Analyst
# =============================================================================
# source("analysis/jays_matchup_intel_v3_2.R")
# =============================================================================
# KEY FINDINGS:
#   Naive beats champion by 0.002 (MAE 0.312 vs 0.310).
#   Sweeper ran 32.1% vs RHH vs 42.0% expected, PARTIAL.
#   Walk rate HELD (7.4% · cleanest command start of 2026 season).
#   K rate PARTIAL (14.8% vs 34.8% · directly tied to Sweeper reduction).
#   Okamoto and Sanchez (both league fallback .320) each hit home runs.
#   Schneider Edge call (.365) matched closely at .348 actual (resid -0.017).
#   Notable: Sweeper ran elevated vs LHH (32.6% vs 17.4% expected).
#     Williams used the Sweeper symmetrically tonight rather than RHH-heavy.
# =============================================================================
# WILLIAMS OUTING:
#   6 IP · 96 pitches · 27 PA · 4K · 2BB · 7H · 6R
#   Sweeper:   32.3% (31) · whiff% 38.5%
#   Curveball: 26.0% (25) · whiff% 11.8%
#   4-Seam:    26.0% (25) · whiff% 37.5%
#   Sinker:    10.4% (10) · whiff% 0.0%
#   Cutter:     5.2%  (5) · whiff% 0.0%
#   vs LHH (n=43): ST 32.6%, CU 30.2%, FF 23.3%, SI 7.0%, FC 7.0%
#   vs RHH (n=53): ST 32.1%, FF 28.3%, CU 22.6%, SI 13.2%, FC 3.8%
# =============================================================================
# ASSUMPTION ACTUALS:
#   Sweeper vs RHH: expected 42.0%  actual 32.1% (17/53)  PARTIAL (-9.9pp, tol +-8pp)
#   Walk rate:      expected 14.8%  actual  7.4% (2/27)   HELD   (-7.4pp, tol +-10pp)
#   K rate:         expected 34.8%  actual 14.8% (4/27)   PARTIAL (-20.0pp, tol +-10pp)
# =============================================================================
# HITTER RESULTS (scoreable, PA >= 2 vs Williams):
#
# Hitter              Hand  PA  H  K  BB  actual_wOBA  exp_wOBA  resid   Tier
# Okamoto             R     3   1  1   1    0.932        0.320    +0.612  Neutral (HR · league fallback)
# Giménez, A.         L     3   2  0   0    0.720        0.305    +0.415  Neutral
# Sánchez             L     3   1  1   0    0.700        0.320    +0.380  Neutral (HR · league fallback)
# Clement             R     3   1  0   0    0.424        0.268    +0.156  Supp (did not hold)
# Schneider           R     2   0  1   1    0.348        0.365    -0.017  Edge (near-exact match)
# Guerrero Jr.        R     3   0  0   0    0.000        0.340    -0.340  Neutral
# Varsho              L     3   0  0   0    0.000        0.326    -0.326  Neutral
# Heineman            L     3   0  0   0    0.000        0.253    -0.253  Supp (held)
#
# Not scored: Lukes (1 PA, double) · Barger (0 PA)
# Sosa (3 PA, not on 2026 roster): excluded
#
# n=8  MAE=0.312  Bias=+0.078  Naive=0.310  Spearman rho=-0.310
# Champion beats naive: FALSE
# =============================================================================

library(tidyverse)

# =============================================================================
# 1. LOAD DATA
# =============================================================================

full_game_df   <- safe_read_csv("savant_data__79_.csv")
hitter_game_df <- safe_read_csv("savant_data__80_.csv")

pitch_by_pitch_game_df <- hitter_game_df |> filter(pitcher == 668909L)

williams_baseline_df <- safe_read_csv("savant_data__32_.csv") |>
  filter(game_year == 2026)

# =============================================================================
# 2. LOAD PREVIEW OUTPUTS
# =============================================================================

preview_expectations_df  <- safe_read_csv(
  "outputs/preview_v32/preview_expectations_v32_2026-04-24_668909.csv")
preview_assumptions_df   <- safe_read_csv(
  "outputs/preview_v32/preview_assumptions_v32_2026-04-24_668909.csv")
hitter_assumption_map_df <- safe_read_csv(
  "outputs/preview_v32/preview_hitter_map_v32_2026-04-24_668909.csv")

# =============================================================================
# 3. ACTUAL ASSUMPTION VALUES
# =============================================================================

PA_EVENTS <- c("single","double","triple","home_run","field_out","strikeout",
               "walk","hit_by_pitch","grounded_into_double_play","force_out",
               "sac_fly","fielders_choice","fielders_choice_out","double_play",
               "strikeout_double_play","sac_bunt","field_error")

williams_game <- full_game_df  |> filter(pitcher == 668909L)
valid_pts     <- williams_game |> filter(pitch_type %in% c("FF","SI","ST","FC","CU"))
rhh_pitches   <- valid_pts     |> filter(stand == "R")
pa_total      <- pitch_by_pitch_game_df |> filter(events %in% PA_EVENTS) |> nrow()

# Sweeper vs RHH: 17/53 = 32.1% · PARTIAL (expected 42.0%, tol +-8pp, delta -9.9pp)
st_rhh_pct <- nrow(rhh_pitches |> filter(pitch_type == "ST")) / nrow(rhh_pitches)
# 0.321

# Walk rate: 2/27 = 7.4% · HELD (expected 14.8%, tol +-10pp, delta -7.4pp)
bb_rate <- (pitch_by_pitch_game_df |>
              filter(events %in% c("walk","intent_walk")) |> nrow()) / pa_total
# 0.074

# K rate: 4/27 = 14.8% · PARTIAL (expected 34.8%, tol +-10pp, delta -20.0pp)
k_rate <- (pitch_by_pitch_game_df |>
             filter(events %in% c("strikeout","strikeout_double_play")) |> nrow()) / pa_total
# 0.148

actual_assumption_values_df <- tibble(
  assumption_name = c("sweeper_vs_rhh_pct", "walk_rate", "k_rate"),
  actual_value    = c(st_rhh_pct,            bb_rate,     k_rate)
  # 0.321, 0.074, 0.148
)

# =============================================================================
# 4. RUN RETRO
# =============================================================================

retro_out <- build_retro_v32(
  game_date                   = "2026-04-24",
  team                        = "TOR",
  opponent                    = "CLE",
  pitcher_name                = "Gavin Williams",
  pitcher_id                  = 668909L,
  pitcher_archetype           = "RHP_starter_cle_sweeper_4seam",
  pitch_by_pitch_game_df      = pitch_by_pitch_game_df,
  preview_expectations_df     = preview_expectations_df,
  preview_assumptions_df      = preview_assumptions_df,
  hitter_assumption_map_df    = hitter_assumption_map_df,
  actual_assumption_values_df = actual_assumption_values_df,
  pitcher_baseline_df         = williams_baseline_df,
  memory_dir                  = "model_memory",
  output_dir                  = "outputs/retro_v32"
)

# =============================================================================
# 5. REVIEW
# =============================================================================

retro_out$hitter_audit |>
  select(hitter, hand, PA, exp_wOBA, actual_wOBA, residual,
         abs_error, audit_flag, miss_type) |>
  print(n = Inf)

retro_out$assumption_audit |>
  select(assumption_name, expected_value, actual_value,
         tolerance, audit_result, assumption_error) |>
  print(n = Inf)

retro_out$model_error |> print()
retro_out$learned     |> print(n = Inf)

# =============================================================================
# 6. MEMORY NOTES
# =============================================================================
#
# Sweeper vs RHH PARTIAL: ran 32.1% tonight vs 42.0% expected.
#   Sweeper deployed symmetrically (32.6% vs LHH, 32.1% vs RHH).
#   2026 average pattern: Sweeper heavy vs RHH. Tonight was an exception.
#   Adjust tolerance upward slightly for next Williams preview.
#
# Walk rate HELD: 7.4%. Best command start of 2026 season.
#   Cleanest version of his profile. No command concern tonight.
#
# K rate PARTIAL: 14.8% vs 34.8% expected.
#   Directly tied to Sweeper PARTIAL. Fewer Sweepers = fewer whiffs.
#   Contact rate was high: 15 field outs in 27 PA.
#
# League-fallback hitters (Okamoto, Sanchez) hit home runs.
#   The model cannot price variance on hitters with zero split history.
#   Wide CI on .320 fallback calls is correct; outcomes will be volatile.
#
# Schneider Edge call: exp .365, actual .348, resid -0.017.
#   Closest Edge-to-actual match of the season. 4-Seam split (.538/34PA)
#   was the driver and 4-Seam was his primary pitch tonight.
#
# Heineman Suppressed held: .253 exp, .000 actual. Three field outs.
#
# RUNNING N: ~158 hitter-games after observation 23.
# H1 this game: FALSE (naive wins 0.310 vs 0.312).
# H2 this game: FALSE (rho -0.310).
#
# =============================================================================
# END · retro_runner_2026-04-24_williams.R
# =============================================================================
