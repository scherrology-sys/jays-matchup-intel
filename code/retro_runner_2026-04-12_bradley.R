# =============================================================================
# Jays Matchup Intel V3.2
# Retro Runner — TOR vs MIN, April 12, 2026
# Pitcher: Taj Bradley (RHP) · ID: 671737
# Result: MIN 8, TOR 2 · Observation 13
# @scherrology | Arm Chair Analyst
# =============================================================================
# source("analysis/jays_matchup_intel_v3_2.R")
# =============================================================================

library(tidyverse)

# =============================================================================
# 1. LOAD DATA
# =============================================================================

# savant_data__45_.csv  — Bradley pitches only (game day, pitcher filter)
# savant_data__46_.csv  — full game pitch file (all pitchers)
# savant_data__47_.csv  — hitter-level game data (all batters)

bradley_game_df <- safe_read_csv("savant_data__45_.csv")   # Bradley pitches
full_game_df    <- safe_read_csv("savant_data__46_.csv")   # All pitchers
hitter_game_df  <- safe_read_csv("savant_data__47_.csv")   # All hitters

# Filter to Bradley plate appearances only for retro scoring
pitch_by_pitch_game_df <- hitter_game_df |>
  filter(pitcher == 671737L)

# Bradley 2026 baseline (for execution flag comparison)
# Same file used in the preview runner
bradley_baseline_df <- safe_read_csv("savant_data__43_.csv") |>
  filter(game_year == 2026, game_date != "2026-04-12")  # exclude tonight

# =============================================================================
# 2. LOAD PREVIEW OUTPUTS
# =============================================================================
# These were written by build_preview_v32() on April 12.
# Adjust paths to match your output_dir.

preview_expectations_df  <- safe_read_csv("outputs/preview_v32/preview_expectations_v32_2026-04-12_671737.csv")
preview_assumptions_df   <- safe_read_csv("outputs/preview_v32/preview_assumptions_v32_2026-04-12_671737.csv")
hitter_assumption_map_df <- safe_read_csv("outputs/preview_v32/preview_hitter_map_v32_2026-04-12_671737.csv")

# =============================================================================
# 3. ACTUAL ASSUMPTION VALUES
# Derived from tonight's pitch file.
# =============================================================================

# --- Splitter whiff rate vs LHH ---
split_lhh <- bradley_game_df |>
  filter(pitch_type == "FS", stand == "L")
split_swings <- split_lhh |>
  filter(description %in% c("swinging_strike","foul","foul_tip",
                             "hit_into_play","swinging_strike_blocked","foul_bunt"))
split_whiffs <- split_lhh |>
  filter(description %in% c("swinging_strike","swinging_strike_blocked","foul_tip"))
splitter_whiff_lhh_actual <- if (nrow(split_swings) > 0)
  nrow(split_whiffs) / nrow(split_swings) else NA_real_
# Result: 0.000 (3 swings, 0 whiffs) — UNTESTABLE at n=3

# --- 4-Seam velocity ---
ff_velo_actual <- bradley_game_df |>
  filter(pitch_type == "FF", !is.na(release_speed)) |>
  summarise(mean_velo = mean(release_speed)) |>
  pull(mean_velo)
# Result: 95.9 mph (expected 97.0) — FAILED

# --- Walk rate ---
pa_events <- c("single","double","triple","home_run","field_out","strikeout",
               "walk","hit_by_pitch","grounded_into_double_play","force_out",
               "sac_fly","fielders_choice","fielders_choice_out","double_play")
total_pa <- bradley_game_df |> filter(events %in% pa_events) |> nrow()
walks    <- bradley_game_df |> filter(events %in% c("walk","intent_walk")) |> nrow()
bb_rate_actual <- walks / total_pa
# Result: 4/23 = 0.174 (expected 0.083) — FAILED (elevated, opposite direction)

# --- Cutter usage vs RHH ---
rhh_pitches <- bradley_game_df |>
  filter(stand == "R", pitch_type %in% c("FF","FC","FS","CU","SI"))
cutter_rhh_actual <- rhh_pitches |>
  summarise(pct = mean(pitch_type == "FC")) |>
  pull(pct)
# Result: 0.429 (expected 0.289) — FAILED (+14pp)

# Build actual values tibble
actual_assumption_values_df <- tibble(
  assumption_name = c(
    "splitter_whiff_lhh_pct",
    "fastball_velo",
    "walk_rate",
    "cutter_to_rhh_pct"
  ),
  actual_value = c(
    splitter_whiff_lhh_actual,   # 0.000 — UNTESTABLE (n=3 swings)
    ff_velo_actual,               # 0.959 as fraction? No — keep as mph: 95.9
    bb_rate_actual,               # 0.174
    cutter_rhh_actual             # 0.429
  )
)

# Note: splitter_whiff_lhh_pct actual is 0.000 from 3 swings.
# The retro scorer will flag this as untestable when comparing to
# expected 0.562. Manually set to NA to suppress scoring if preferred:
# actual_assumption_values_df$actual_value[1] <- NA_real_

# =============================================================================
# 4. RUN RETRO
# =============================================================================

retro_out <- build_retro_v32(
  game_date                   = "2026-04-12",
  team                        = "TOR",
  opponent                    = "MIN",
  pitcher_name                = "Taj Bradley",
  pitcher_id                  = 671737L,
  pitcher_archetype           = "RHP_starter_min",
  pitch_by_pitch_game_df      = pitch_by_pitch_game_df,
  preview_expectations_df     = preview_expectations_df,
  preview_assumptions_df      = preview_assumptions_df,
  hitter_assumption_map_df    = hitter_assumption_map_df,
  actual_assumption_values_df = actual_assumption_values_df,
  pitcher_baseline_df         = bradley_baseline_df,
  memory_dir                  = "model_memory",
  output_dir                  = "outputs/retro_v32"
)

# =============================================================================
# 5. REVIEW OUTPUTS
# =============================================================================

# Hitter audit (4 scoreable hitters — Bradley 5 IP only)
retro_out$hitter_audit |>
  select(hitter, hand, PA, exp_wOBA, actual_wOBA, residual, abs_error,
         audit_flag, miss_type, affected_assumptions) |>
  print(n = Inf)

# Assumption audit
retro_out$assumption_audit |>
  select(assumption_name, expected_value, actual_value, tolerance,
         audit_result, assumption_error) |>
  print(n = Inf)

# Execution flags
retro_out$execution_flags |>
  filter(flag_value) |>
  print(n = Inf)

# Model error summary
retro_out$model_error |> print()

# What the model learned
retro_out$learned |> print(n = Inf)

# =============================================================================
# 6. MANUAL NOTES FOR MEMORY UPDATE
# =============================================================================
#
# Varsho suppression pattern: 2 consecutive games (Ryan Obs 12, Bradley Obs 13)
# projected Suppressed, actual well above. Flag for review in next game.
#
# Bradley velocity: 95.9 actual vs 97.0 expected. Three-start 2026 sample
# likely overstated. Flag as uncertain until >= 5 starts in 2026.
#
# Bradley Splitter whiff vs LHH: untestable (n=3 LHH swings tonight).
# Revert to career 31.3% for next Bradley preview. Do not carry forward
# 56.2% as a reliable estimate.
#
# Eloy Jimenez: debut April 12. 1/2 vs Bradley (single off FF, K on FC).
# Add to batter_pool for all future previews. No prior-season split data
# available yet — will use overall wOBA fallback until PA accumulates.
#
# Pitch over-reliance prior: 5 of 13 games have shown a pitcher exceeding
# expected usage on primary weapon by >= 13pp. Consider adding a prior
# adjustment to assumption expected values that widens the tolerance for
# pitch mix divergence.
#
# =============================================================================
# END — retro_runner_2026-04-12_bradley.R
# =============================================================================
