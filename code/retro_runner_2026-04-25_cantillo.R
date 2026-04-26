# =============================================================================
# Jays Matchup Intel V3.2
# Retro Runner · TOR vs CLE · April 25, 2026
# Pitcher: Joey Cantillo (LHP) · ID: 676282
# Result: TOR 5, CLE 2 · Observation 24
# @scherrology | Arm Chair Analyst
# =============================================================================
# source("analysis/jays_matchup_intel_v3_2.R")
# =============================================================================
# KEY FINDINGS:
#   Naive wins by 0.001 (essentially tied). rho +0.217, first positive in 3 games.
#   Heineman: exp .445, actual .444. Nearest Edge match of the season.
#   Gimenez Suppressed: exp .177, actual .000. Held for 2nd consecutive game.
#   Both pitch assumptions FAILED: Cantillo swapped CH and SL deployment.
#   Okamoto (league fallback .320) hit home run again, 3rd such event in 4 games.
# =============================================================================
# CANTILLO OUTING:
#   6 IP · 89 pitches · 22 PA · 4K · 1BB · 6H · 2R
#   Changeup:  37.1% (33) · whiff% 36.8%
#   Curveball: 30.3% (27) · whiff% 14.3%
#   4-Seam:    30.3% (27) · whiff% 11.1%
#   Slider:     2.2%  (2) · whiff% 0.0%
#   vs LHH (n=19): CU 47.4%, FF 26.3%, CH 15.8%, SL 10.5%
#   vs RHH (n=70): CH 42.9%, FF 31.4%, CU 25.7% (SL 0%)
# =============================================================================
# ASSUMPTION ACTUALS:
#   Changeup vs RHH: expected 24.5%  actual 42.9% (30/70)  FAILED (+18.4pp, tol +-6pp)
#   Slider vs LHH:   expected 37.2%  actual 10.5%  (2/19)  FAILED (-26.7pp, tol +-8pp)
#   K rate:          expected 29.1%  actual 18.2%  (4/22)  PARTIAL (-10.9pp, tol +-10pp)
#   NOTE: Both pitch assumptions failed simultaneously in opposite directions.
#         Cantillo replaced Slider vs LHH with Curveball (47.4% of LHH pitches).
#         Changeup nearly doubled vs RHH relative to 2026 average.
#         For next Cantillo preview: widen tolerances on both CH and SL assumptions.
# =============================================================================
# HITTER RESULTS (scoreable, PA >= 2 vs Cantillo):
#
# Hitter              Hand  PA  H  K  BB  actual_wOBA  exp_wOBA  resid   Tier
# Okamoto             R     3   2  1   0    0.996        0.320    +0.676  Neutral (HR)
# Straw               R     3   1  0   0    0.424        0.255    +0.169  Supp (did not hold)
# Clement             R     3   1  1   0    0.424        0.334    +0.090  Neutral
# Schneider           R     2   0  0   1    0.348        0.279    +0.069  Supp (did not hold)
# Heineman            R     2   1  1   0    0.444        0.445    -0.001  Edge (near-exact)
# Guerrero Jr.        R     3   1  0   0    0.296        0.424    -0.128  Edge
# Varsho              L     2   0  0   0    0.000        0.391    -0.391  Edge (missed)
# Eloy                R     2   0  0   0    0.000        0.320    -0.320  Neutral
# Giménez, A.         L     2   0  1   0    0.000        0.177    -0.177  Supp (held)
#
# Not scored (<2 PA): Lukes (1 PA) · Barger (0 PA) · Sanchez (0 PA)
#
# n=9  MAE=0.225  Bias=-0.001  Naive=0.224  Spearman rho=+0.217
# Champion beats naive: FALSE (margin 0.001)
# =============================================================================

library(tidyverse)

# =============================================================================
# 1. LOAD DATA
# =============================================================================

full_game_df   <- safe_read_csv("savant_data__83_.csv")
hitter_game_df <- safe_read_csv("savant_data__84_.csv")

pitch_by_pitch_game_df <- hitter_game_df |> filter(pitcher == 676282L)

cantillo_baseline_df <- safe_read_csv("savant_data__82_.csv") |>
  filter(game_year == 2026)

# =============================================================================
# 2. LOAD PREVIEW OUTPUTS
# =============================================================================

preview_expectations_df  <- safe_read_csv(
  "outputs/preview_v32/preview_expectations_v32_2026-04-25_676282.csv")
preview_assumptions_df   <- safe_read_csv(
  "outputs/preview_v32/preview_assumptions_v32_2026-04-25_676282.csv")
hitter_assumption_map_df <- safe_read_csv(
  "outputs/preview_v32/preview_hitter_map_v32_2026-04-25_676282.csv")

# =============================================================================
# 3. ACTUAL ASSUMPTION VALUES
# =============================================================================

PA_EVENTS <- c("single","double","triple","home_run","field_out","strikeout",
               "walk","hit_by_pitch","grounded_into_double_play","force_out",
               "sac_fly","fielders_choice","fielders_choice_out","double_play",
               "strikeout_double_play","sac_bunt","field_error")

cantillo_game <- full_game_df  |> filter(pitcher == 676282L)
valid_pts     <- cantillo_game |> filter(pitch_type %in% c("FF","CH","SL","CU"))
lhh_pitches   <- valid_pts     |> filter(stand == "L")
rhh_pitches   <- valid_pts     |> filter(stand == "R")
pa_total      <- pitch_by_pitch_game_df |> filter(events %in% PA_EVENTS) |> nrow()

# Changeup vs RHH: 30/70 = 42.9% · FAILED (expected 24.5%, tol +-6pp, delta +18.4pp)
ch_rhh_pct <- nrow(rhh_pitches |> filter(pitch_type == "CH")) / nrow(rhh_pitches)
# 0.429

# Slider vs LHH: 2/19 = 10.5% · FAILED (expected 37.2%, tol +-8pp, delta -26.7pp)
# Curveball replaced Slider as primary LHH off-speed at 47.4%
sl_lhh_pct <- nrow(lhh_pitches |> filter(pitch_type == "SL")) / nrow(lhh_pitches)
# 0.105

# K rate: 4/22 = 18.2% · PARTIAL (expected 29.1%, tol +-10pp, delta -10.9pp)
k_rate <- (pitch_by_pitch_game_df |>
             filter(events %in% c("strikeout","strikeout_double_play")) |> nrow()) / pa_total
# 0.182

actual_assumption_values_df <- tibble(
  assumption_name = c("changeup_vs_rhh_pct", "slider_vs_lhh_pct", "k_rate"),
  actual_value    = c(ch_rhh_pct,             sl_lhh_pct,           k_rate)
  # 0.429, 0.105, 0.182
)

# =============================================================================
# 4. RUN RETRO
# =============================================================================

retro_out <- build_retro_v32(
  game_date                   = "2026-04-25",
  team                        = "TOR",
  opponent                    = "CLE",
  pitcher_name                = "Joey Cantillo",
  pitcher_id                  = 676282L,
  pitcher_archetype           = "LHP_starter_cle_changeup_4seam",
  pitch_by_pitch_game_df      = pitch_by_pitch_game_df,
  preview_expectations_df     = preview_expectations_df,
  preview_assumptions_df      = preview_assumptions_df,
  hitter_assumption_map_df    = hitter_assumption_map_df,
  actual_assumption_values_df = actual_assumption_values_df,
  pitcher_baseline_df         = cantillo_baseline_df,
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
# Both pitch assumptions FAILED in opposite directions:
#   Changeup vs RHH: actual 42.9% vs expected 24.5%. Elevated by 18pp.
#   Slider vs LHH: actual 10.5% vs expected 37.2%. Dropped by 27pp.
#   Curveball replaced Slider as primary LHH off-speed (47.4% vs 7.1% expected).
#
# For next Cantillo preview:
#   Widen Changeup vs RHH tolerance to +-10pp or higher.
#   Widen Slider vs LHH tolerance to +-12pp or higher.
#   Consider flagging Curveball vs LHH as a potential alternative assumption.
#   Note that Cantillo may adapt his LHH plan based on opponent.
#
# Heineman Edge match: exp .445, actual .444. Near-exact.
#   4-Seam split (.493/13PA) drove the call. 4-Seam was ~31% of RHH pitches.
#   Prior-driven Edge from high LHP wOBA held perfectly despite assumption failures.
#
# Gimenez Suppressed held: 2-for-2 in CLE series.
#   FF .000/15PA vs LHP is a real and durable signal.
#   Even when Slider assumption failed (dropped from expected 37% to 10%),
#   the 4-Seam signal was tested and validated.
#
# Okamoto league fallback home run: 3rd HR in 4 games from .320 fallback hitters.
#   No LHP history in file. Wide CI on .320 prior is correct.
#   Pattern worth monitoring as 2026 data accumulates.
#
# RUNNING N: ~167 hitter-games after observation 24.
# H1 this game: FALSE (naive wins 0.224 vs 0.225, margin 0.001).
# H2 this game: TRUE (rho +0.217).
# Consecutive H1 losses: 3 (obs 22, 23, 24). All by margins of 0.001-0.030.
#
# =============================================================================
# END · retro_runner_2026-04-25_cantillo.R
# =============================================================================
