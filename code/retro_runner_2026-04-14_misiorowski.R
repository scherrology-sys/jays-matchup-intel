# =============================================================================
# Jays Matchup Intel V3.2
# Retro Runner — TOR @ MIL, April 14, 2026
# Pitcher: Jacob Misiorowski (RHP) · ID: 694819
# Result: TOR 9, MIL 7 (F/10) · Observation 14
# @scherrology | Arm Chair Analyst
# =============================================================================
# source("analysis/jays_matchup_intel_v3_2.R")
# =============================================================================

library(tidyverse)

# =============================================================================
# 1. LOAD DATA
# =============================================================================

# savant_data__49_.csv — full game pitch file (all pitchers)
# savant_data__50_.csv — hitter-level game data (all batters)

full_game_df   <- safe_read_csv("savant_data__49_.csv")
hitter_game_df <- safe_read_csv("savant_data__50_.csv")

# Misiorowski pitches only (ID: 694819)
misi_game_df <- full_game_df |> filter(pitcher == 694819L)

# Hitter PAs vs Misiorowski only
pitch_by_pitch_game_df <- hitter_game_df |> filter(pitcher == 694819L)

# Misiorowski 2026 baseline (prior starts, exclude tonight)
misi_baseline_df <- safe_read_csv("savant_data__48_.csv") |>
  filter(game_year == 2026, game_date != "2026-04-14")

# =============================================================================
# 2. LOAD PREVIEW OUTPUTS
# =============================================================================

preview_expectations_df  <- safe_read_csv(
  "outputs/preview_v32/preview_expectations_v32_2026-04-14_694819.csv")
preview_assumptions_df   <- safe_read_csv(
  "outputs/preview_v32/preview_assumptions_v32_2026-04-14_694819.csv")
hitter_assumption_map_df <- safe_read_csv(
  "outputs/preview_v32/preview_hitter_map_v32_2026-04-14_694819.csv")

# =============================================================================
# 3. ACTUAL ASSUMPTION VALUES
# Derived from savant_data__49_.csv (Misiorowski pitches only).
# =============================================================================

SWING_DESCS <- c("swinging_strike","foul","foul_tip",
                 "hit_into_play","swinging_strike_blocked","foul_bunt")
PA_EVENTS <- c("single","double","triple","home_run","field_out","strikeout",
               "walk","hit_by_pitch","grounded_into_double_play","force_out",
               "sac_fly","fielders_choice","fielders_choice_out","double_play",
               "strikeout_double_play")

# --- Changeup to LHH ---
lhh_pitches <- misi_game_df |> filter(stand == "L")
ch_lhh_actual <- nrow(lhh_pitches |> filter(pitch_type == "CH")) / nrow(lhh_pitches)
# Result: 1/39 = 0.026 (expected 0.097) — FAILED

# --- 4-Seam velocity ---
ff_velo_actual <- misi_game_df |>
  filter(pitch_type == "FF", !is.na(release_speed)) |>
  summarise(mean_velo = mean(release_speed)) |>
  pull(mean_velo)
# Result: 98.0 mph (expected 99.1) — FAILED (delta -1.1, tolerance ±1.0)

# --- Walk rate ---
pa_in_game <- misi_game_df |> filter(events %in% PA_EVENTS)
bb_actual   <- nrow(pa_in_game |> filter(events %in% c("walk","intent_walk")))
bb_rate_actual <- bb_actual / nrow(pa_in_game)
# Result: 0/21 = 0.000 (expected 0.115) — FAILED

actual_assumption_values_df <- tibble(
  assumption_name = c("changeup_to_lhh_pct", "fastball_velo", "walk_rate"),
  actual_value    = c(ch_lhh_actual, ff_velo_actual, bb_rate_actual)
  # 0.026, 98.0, 0.000
)

# =============================================================================
# 4. RUN RETRO
# =============================================================================

retro_out <- build_retro_v32(
  game_date                   = "2026-04-14",
  team                        = "TOR",
  opponent                    = "MIL",
  pitcher_name                = "Jacob Misiorowski",
  pitcher_id                  = 694819L,
  pitcher_archetype           = "RHP_starter_mil_power",
  pitch_by_pitch_game_df      = pitch_by_pitch_game_df,
  preview_expectations_df     = preview_expectations_df,
  preview_assumptions_df      = preview_assumptions_df,
  hitter_assumption_map_df    = hitter_assumption_map_df,
  actual_assumption_values_df = actual_assumption_values_df,
  pitcher_baseline_df         = misi_baseline_df,
  memory_dir                  = "model_memory",
  output_dir                  = "outputs/retro_v32"
)

# =============================================================================
# 5. REVIEW
# =============================================================================

retro_out$hitter_audit |>
  select(hitter, hand, PA, exp_wOBA, actual_wOBA, residual, abs_error,
         audit_flag, miss_type) |>
  print(n = Inf)

retro_out$assumption_audit |>
  select(assumption_name, expected_value, actual_value, tolerance,
         audit_result, assumption_error) |>
  print(n = Inf)

retro_out$execution_flags |> filter(flag_value) |> print(n = Inf)
retro_out$model_error |> print()
retro_out$learned |> print(n = Inf)

# =============================================================================
# 6. MANUAL MEMORY NOTES
# =============================================================================
#
# VARSHO PATTERN — four consecutive games of significant overperformance:
#   Obs 11 (Woods-Richardson): Neutral, outperformed
#   Obs 12 (Ryan): Suppressed .307, actual .700
#   Obs 13 (Bradley): Suppressed .296, actual 1.016
#   Obs 14 (Misiorowski): Neutral .299, actual .700
#   Action: Flag Varsho's prior-season wOBA as understating 2026 form.
#   Apply a +0.020 directional correction to exp_wOBA in next preview.
#   Revisit at n=10 Varsho-game observations.
#
# WALK RATE TOLERANCE:
#   Three games with significant walk rate assumption failure.
#   Widen default walk rate tolerance from ±5pp to ±7pp for all future previews.
#   Pattern: pitchers commanding the zone better than career rates in
#   high-stakes appearances against a lineup seeing them fresh.
#
# MISIOROWSKI VELOCITY:
#   3-start 2026 average was 99.1 mph.
#   Tonight: 98.0 mph. Two starts below the season average.
#   Treat as 98-99 mph range, not 99.1 stable, for next start.
#
# MISIOROWSKI CHANGEUP TO LHH:
#   Expected 9.7%, actual 2.6% tonight.
#   Reduce expected rate to ~5% until further evidence.
#   Career file may overstate how frequently he deploys it against LHH
#   in specific game situations.
#
# FIRST EXPOSURE GAMES (LOW confidence):
#   Champion MAE (0.283) and Naive-1 MAE (0.274) converge at LOW confidence.
#   This is expected behavior. The model has nothing to differentiate on.
#   First-exposure retro data is the most valuable observation in the system
#   because it anchors all future BVP for this pitcher against this lineup.
#
# GAME NOTE:
#   Jays won 9-7 in 10 innings. Most of the run scoring came off the bullpen
#   (Megill 3H in 9th, Anderson 2H in 10th). Guerrero doubled home the winning
#   run in the 10th — not scored in this retro (not vs Misiorowski).
#   Bauers hit a 3-run HR off Gausman in the 4th (opposing starter context).
#
# RUNNING N: ~93 hitter-games after observation 14.
#
# =============================================================================
# END — retro_runner_2026-04-14_misiorowski.R
# =============================================================================
