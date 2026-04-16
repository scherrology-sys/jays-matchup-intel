# =============================================================================
# Jays Matchup Intel V3.2
# Retro Runner · TOR @ MIL · April 15, 2026
# Pitcher: Chad Patrick (RHP) · ID: 694477
# Result: MIL 2, TOR 1 · Observation 15
# @scherrology | Arm Chair Analyst
# =============================================================================
# source("analysis/jays_matchup_intel_v3_2.R")
# =============================================================================

library(tidyverse)

# =============================================================================
# 1. LOAD DATA
# =============================================================================

# savant_data__53_.csv · full game pitch file (all pitchers)
# savant_data__54_.csv · hitter game file (all batters)

full_game_df   <- safe_read_csv("savant_data__53_.csv")
hitter_game_df <- safe_read_csv("savant_data__54_.csv")

# Patrick pitches only (ID: 694477)
# Note: pitcher ID confirmed from game file, not 669062 as originally estimated
pitch_by_pitch_game_df <- hitter_game_df |> filter(pitcher == 694477L)
patrick_game_df        <- full_game_df   |> filter(pitcher == 694477L)

# Patrick 2026 baseline (prior starts, exclude tonight)
patrick_baseline_df <- safe_read_csv("savant_data__51_.csv") |>
  filter(game_year == 2026)

# =============================================================================
# 2. LOAD PREVIEW OUTPUTS
# =============================================================================

preview_expectations_df  <- safe_read_csv(
  "outputs/preview_v32/preview_expectations_v32_2026-04-15_694477.csv")
preview_assumptions_df   <- safe_read_csv(
  "outputs/preview_v32/preview_assumptions_v32_2026-04-15_694477.csv")
hitter_assumption_map_df <- safe_read_csv(
  "outputs/preview_v32/preview_hitter_map_v32_2026-04-15_694477.csv")

# =============================================================================
# 3. ACTUAL ASSUMPTION VALUES
# =============================================================================

PA_EVENTS <- c("single","double","triple","home_run","field_out","strikeout",
               "walk","hit_by_pitch","grounded_into_double_play","force_out",
               "sac_fly","fielders_choice","fielders_choice_out","double_play",
               "strikeout_double_play")

# --- Slurve to LHH ---
lhh_valid <- patrick_game_df |>
  filter(stand == "L", pitch_type %in% c("FF","FC","SI","SV","SL","CH"))
sv_lhh_pct <- nrow(lhh_valid |> filter(pitch_type == "SV")) / nrow(lhh_valid)
# Result: 6/46 = 0.130 (expected 0.147) · HELD

# --- 4-Seam velocity ---
ff_velo <- patrick_game_df |>
  filter(pitch_type == "FF", !is.na(release_speed)) |>
  summarise(mean(release_speed)) |> pull()
# Result: 94.6 mph (expected 94.2) · HELD

# --- Cutter to RHH ---
rhh_valid <- patrick_game_df |>
  filter(stand == "R", pitch_type %in% c("FF","FC","SI","SV","SL","CH"))
fc_rhh_pct <- nrow(rhh_valid |> filter(pitch_type == "FC")) / nrow(rhh_valid)
# Result: 17/35 = 0.486 (expected 0.426, tolerance ±0.070) · PARTIAL

actual_assumption_values_df <- tibble(
  assumption_name = c("slurve_usage_pct", "fastball_velo", "cutter_to_rhh_pct"),
  actual_value    = c(sv_lhh_pct, ff_velo, fc_rhh_pct)
  # 0.130, 94.6, 0.486
)

# =============================================================================
# 4. RUN RETRO
# =============================================================================

retro_out <- build_retro_v32(
  game_date                   = "2026-04-15",
  team                        = "TOR",
  opponent                    = "MIL",
  pitcher_name                = "Chad Patrick",
  pitcher_id                  = 694477L,
  pitcher_archetype           = "RHP_starter_mil_contact",
  pitch_by_pitch_game_df      = pitch_by_pitch_game_df,
  preview_expectations_df     = preview_expectations_df,
  preview_assumptions_df      = preview_assumptions_df,
  hitter_assumption_map_df    = hitter_assumption_map_df,
  actual_assumption_values_df = actual_assumption_values_df,
  pitcher_baseline_df         = patrick_baseline_df,
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

# =============================================================================
# 6. MEMORY NOTES
# =============================================================================
#
# PITCHER ID CORRECTION:
# The preview runner used ID 669062 (estimated). Actual game file confirms
# Patrick's Statcast pitcher ID is 694477. Update future preview runners.
#
# CUTTER PERFORMANCE:
# 34 Cutters, 42.9% whiff rate, zero hits. Career xwOBA .299 understates
# what the pitch does against this specific lineup. Flag: Cutter against
# TOR RHH is more effective than career baseline suggests. Apply directional
# adjustment in next Patrick preview when BVP accumulates.
#
# LOW CONFIDENCE PATTERN (obs 14-15):
# Two consecutive LOW confidence games. Both: naive beats champion on MAE,
# ranking holds (rho +0.857 tonight, obs 15 best of season).
# Emerging signal: LOW conf model cannot calibrate magnitude but orders correctly.
# H1 fails at LOW conf. H2 holds. Track through season.
#
# VARSHO:
# .232 actual vs .310 projected. Closest to expectation in five games.
# Four-game overperformance pattern may be breaking. Continue monitoring.
# Do not remove the directional correction flag yet.
#
# RUNNING N: ~100 hitter-games after observation 15.
#
# =============================================================================
# END · retro_runner_2026-04-15_patrick.R
# =============================================================================
