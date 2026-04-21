# =============================================================================
# Jays Matchup Intel V3.2
# Retro Runner · TOR @ LAA · April 20, 2026
# Pitcher: Reid Detmers (LHP) · ID: 672282
# Result: TOR 5, LAA 2 · Observation 20
# @scherrology | Arm Chair Analyst
# =============================================================================
# source("analysis/jays_matchup_intel_v3_2.R")
# =============================================================================
# KEY FINDINGS:
#   Champion beats naive for the first time (MAE 0.301 vs 0.303).
#   Guerrero hit Changeup twice (single + HR). His CH split .328/35PA validated.
#   Varsho actual wOBA = .363, exp = .363. Exact match.
#   Slider vs LHH assumption FAILED (60% actual) but not interpretable:
#     Varsho was the only LHH batter — 10 pitches total. Not a real signal.
# =============================================================================
# DETMERS OUTING:
#   7 IP · 90 pitches · 26 PA · 5K · 2BB · 5H · 1HR
#   Slider:   41.1% (37) · velo 86.5 · whiff% 42.1% · best pitch tonight
#   4-Seam:   32.2% (29) · velo 93.9 · whiff% 10.0%
#   Changeup: 13.3% (12) · velo 83.8 · whiff% 25.0%
#   Curveball:12.2% (11) · velo 72.7 · whiff% 14.3%
#   Sinker:    1.1%  (1)
#   vs LHH (n=10, Varsho only): SL 60.0%, CU 20.0%, SI 10.0%, FF 10.0%
#   vs RHH (n=80): SL 38.8%, FF 35.0%, CH 15.0%, CU 11.2%
#   Strikeouts: Varsho (SL 0-2), Okamoto (FF 3-2), Schneider (SL 3-2),
#               Heineman (CU 2-2), Sosa (SL 0-2, not on preview roster)
# =============================================================================
# ASSUMPTION ACTUALS:
#   Slider vs LHH:     expected 37.1%  actual 60.0%  FAILED*
#   Changeup vs RHH:   expected 13.9%  actual 15.0%  HELD (+1.1pp)
#   K rate:            expected 27.7%  actual 19.2%  HELD (-8.5pp, tol ±12pp)
#   *Varsho was the only LHH batter. 10-pitch sample, not interpretable.
#   Retro memory: do NOT update Slider vs LHH assumption from this game.
# =============================================================================
# HITTER RESULTS (scoreable, PA >= 2 vs Detmers):
#
# Hitter              Hand  PA  H  K  BB  actual_wOBA  exp_wOBA  resid   Tier
# Guerrero Jr.        R     3   3  0   0    1.292        0.433    +0.859  Edge
# Jiménez, Eloy       R     3   2  0   0    0.592        0.320    +0.272  Neutral
# Varsho              L     3   0  1   0    0.363*       0.363     0.000  Edge  *HBP
# Schneider           R     3   0  1   1    0.232        0.313    -0.081  Neutral
# Straw               R     3   0  0   1    0.232        0.288    -0.056  Supp
# Okamoto             R     3   0  1   0    0.000        0.320    -0.320  Neutral
# Clement             R     3   0  0   0    0.000        0.375    -0.375  Edge
# Heineman            R     2   0  1   0    0.000        0.442    -0.442  Edge
#
# Not scored (0 PA vs Detmers): Lukes, Barger, Giménez
# Sosa (3 PA, sac fly + FO + K): not on preview roster, not scored
#
# n=8  MAE=0.301  Bias=-0.018  Naive=0.303  Spearman rho=-0.238
# Champion beats naive: TRUE (first time this season)
# =============================================================================

library(tidyverse)

# =============================================================================
# 1. LOAD DATA
# =============================================================================

full_game_df   <- safe_read_csv("savant_data__73_.csv")
hitter_game_df <- safe_read_csv("savant_data__74_.csv")

# Detmers pitches only (ID: 672282, LHP)
pitch_by_pitch_game_df <- hitter_game_df |> filter(pitcher == 672282L)

# Detmers 2026 baseline (from savant_data__72_.csv)
detmers_baseline_df <- safe_read_csv("savant_data__72_.csv") |>
  filter(game_year == 2026)

# =============================================================================
# 2. LOAD PREVIEW OUTPUTS
# =============================================================================

preview_expectations_df  <- safe_read_csv(
  "outputs/preview_v32/preview_expectations_v32_2026-04-20_672282.csv")
preview_assumptions_df   <- safe_read_csv(
  "outputs/preview_v32/preview_assumptions_v32_2026-04-20_672282.csv")
hitter_assumption_map_df <- safe_read_csv(
  "outputs/preview_v32/preview_hitter_map_v32_2026-04-20_672282.csv")

# =============================================================================
# 3. ACTUAL ASSUMPTION VALUES
# =============================================================================

PA_EVENTS <- c("single","double","triple","home_run","field_out","strikeout",
               "walk","hit_by_pitch","grounded_into_double_play","force_out",
               "sac_fly","fielders_choice","fielders_choice_out","double_play",
               "strikeout_double_play","sac_bunt","field_error")

detmers_game <- full_game_df |> filter(pitcher == 672282L)
valid_pts    <- detmers_game |> filter(pitch_type %in% c("FF","SL","CU","CH","SI"))
lhh_pitches  <- valid_pts   |> filter(stand == "L")
rhh_pitches  <- valid_pts   |> filter(stand == "R")
pa_total     <- pitch_by_pitch_game_df |> filter(events %in% PA_EVENTS) |> nrow()

# NOTE: Varsho was the ONLY LHH batter tonight (10 pitches).
# Slider vs LHH actual = 6/10 = 60.0%.
# This is not a meaningful signal. The assumption was written for a full
# game context. With a single LHH batter, no update is made to retro memory.
# Scored as FAILED by the function but flagged as non-interpretable.

sl_lhh_pct <- nrow(lhh_pitches |> filter(pitch_type == "SL")) / nrow(lhh_pitches)
# 0.600 — context caveat: single batter sample

ch_rhh_pct <- nrow(rhh_pitches |> filter(pitch_type == "CH")) / nrow(rhh_pitches)
# 0.150 — HELD (expected 0.139, tol ±0.060)

k_pa <- pitch_by_pitch_game_df |>
  filter(events %in% PA_EVENTS) |> nrow()
k_count <- pitch_by_pitch_game_df |>
  filter(events %in% c("strikeout","strikeout_double_play")) |> nrow()
k_rate <- k_count / k_pa
# 0.192 — HELD (expected 0.277, tol ±0.120)

actual_assumption_values_df <- tibble(
  assumption_name = c("slider_vs_lhh_pct", "changeup_vs_rhh_pct", "k_rate"),
  actual_value    = c(sl_lhh_pct,           ch_rhh_pct,             k_rate)
  # 0.600, 0.150, 0.192
)

# =============================================================================
# 4. RUN RETRO
# =============================================================================

retro_out <- build_retro_v32(
  game_date                   = "2026-04-20",
  team                        = "TOR",
  opponent                    = "LAA",
  pitcher_name                = "Reid Detmers",
  pitcher_id                  = 672282L,
  pitcher_archetype           = "LHP_starter_laa_slider",
  pitch_by_pitch_game_df      = pitch_by_pitch_game_df,
  preview_expectations_df     = preview_expectations_df,
  preview_assumptions_df      = preview_assumptions_df,
  hitter_assumption_map_df    = hitter_assumption_map_df,
  actual_assumption_values_df = actual_assumption_values_df,
  pitcher_baseline_df         = detmers_baseline_df,
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

retro_out$execution_flags |> filter(flag_value) |> print(n = Inf)
retro_out$model_error |> print()
retro_out$learned |> print(n = Inf)

# NOTE on slider_vs_lhh_pct FAILED result:
# build_retro_v32() will log this as FAILED. However the context caveat
# means retro memory should NOT penalize this assumption's reliability weight.
# The single-batter sample (Varsho, 10 pitches) is not a valid test of the
# assumption. If using model_memory in future, manually review this entry
# before relying on get_assumption_reliability("slider_vs_lhh_pct").

# =============================================================================
# 6. MEMORY NOTES
# =============================================================================
#
# Guerrero CH split confirmed: .328/35PA vs LHP Changeup, single + HR tonight.
#   The Changeup computation entered correctly and produced the right result.
#
# Varsho exact match: exp .363 actual .363. HBP arithmetic coincidence.
#   Prior .318 anchored the call with Slider/CU/CH fallback. Stable signal.
#
# Heineman Edge miss: .442 exp, 0.000 actual (K on CU, GIDP).
#   His Edge call had only 4-Seam pitch-split data entering the computation.
#   High prior drove the call into Edge without Slider/CH signal to support it.
#   For future Detmers previews: Heineman's Slider and Changeup splits vs LHP
#   need to be checked. If still below MIN_PA=10, the Edge call will carry
#   the same uncertainty.
#
# Slider vs LHH assumption: do NOT update from this game. Single-batter sample.
# Changeup vs RHH: 15.0% confirmed. Assumption stable.
# K rate: 19.2% tonight. Within expected low-K range.
#
# RUNNING N: ~134 hitter-games after observation 20.
# FIRST GAME where champion beats naive: MAE 0.301 vs 0.303.
#
# =============================================================================
# END · retro_runner_2026-04-20_detmers.R
# =============================================================================
