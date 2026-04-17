# =============================================================================
# Jays Matchup Intel V3.2
# Retro Runner · TOR @ MIL · April 16, 2026
# Pitcher: Brandon Sproat (RHP) · ID: 687075
# Result: MIL 2, TOR 1 · Observation 16
# @scherrology | Arm Chair Analyst
# =============================================================================
# source("analysis/jays_matchup_intel_v3_2.R")
# =============================================================================

library(tidyverse)

# =============================================================================
# 1. LOAD DATA
# =============================================================================

# savant_data__56_.csv · full game pitch file (all pitchers)
# savant_data__57_.csv · hitter game file (all batters)

full_game_df   <- safe_read_csv("savant_data__56_.csv")
hitter_game_df <- safe_read_csv("savant_data__57_.csv")

# Sproat pitches only (ID: 687075, confirmed from game file)
sproat_game_df         <- full_game_df   |> filter(pitcher == 687075L)
pitch_by_pitch_game_df <- hitter_game_df |> filter(pitcher == 687075L)

# Sproat 2026 baseline (prior starts)
sproat_baseline_df <- safe_read_csv("savant_data__55_.csv") |>
  filter(game_year == 2026)

# =============================================================================
# 2. LOAD PREVIEW OUTPUTS
# =============================================================================

preview_expectations_df  <- safe_read_csv(
  "outputs/preview_v32/preview_expectations_v32_2026-04-16_687075.csv")
preview_assumptions_df   <- safe_read_csv(
  "outputs/preview_v32/preview_assumptions_v32_2026-04-16_687075.csv")
hitter_assumption_map_df <- safe_read_csv(
  "outputs/preview_v32/preview_hitter_map_v32_2026-04-16_687075.csv")

# =============================================================================
# 3. ACTUAL ASSUMPTION VALUES
# =============================================================================

PA_EVENTS <- c("single","double","triple","home_run","field_out","strikeout",
               "walk","hit_by_pitch","grounded_into_double_play","force_out",
               "sac_fly","fielders_choice","fielders_choice_out","double_play",
               "strikeout_double_play","sac_bunt")

# --- Walk rate ---
pa_tonight <- sproat_game_df |> filter(events %in% PA_EVENTS)
bb_tonight <- pa_tonight |> filter(events %in% c("walk","intent_walk")) |> nrow()
bb_rate_actual <- bb_tonight / nrow(pa_tonight)
# Result: 1/24 = 0.042 (expected 0.200) · FAILED

# --- Sweeper to RHH ---
rhh_valid <- sproat_game_df |>
  filter(stand == "R", pitch_type %in% c("FF","FC","SI","CH","ST","CU","SL"))
st_rhh_pct <- nrow(rhh_valid |> filter(pitch_type == "ST")) / nrow(rhh_valid)
# Result: 14/45 = 0.311 (expected 0.233, tolerance ±0.070) · PARTIAL

# --- Cutter contact quality (pfx_x as proxy for "real" cut) ---
fc_tonight <- sproat_game_df |> filter(pitch_type == "FC", !is.na(pfx_x))
fc_pfx_avg <- mean(fc_tonight$pfx_x)
# Result: 0.035 (career 2026 trend: flat/no-cut pattern confirmed)

actual_assumption_values_df <- tibble(
  assumption_name = c("walk_rate", "sweeper_vs_rhh_pct", "cutter_pfx_x"),
  actual_value    = c(bb_rate_actual, st_rhh_pct, fc_pfx_avg)
  # 0.042, 0.311, 0.035
)

# =============================================================================
# 4. RUN RETRO
# =============================================================================

retro_out <- build_retro_v32(
  game_date                   = "2026-04-16",
  team                        = "TOR",
  opponent                    = "MIL",
  pitcher_name                = "Brandon Sproat",
  pitcher_id                  = 687075L,
  pitcher_archetype           = "RHP_starter_mil_sinker",
  pitch_by_pitch_game_df      = pitch_by_pitch_game_df,
  preview_expectations_df     = preview_expectations_df,
  preview_assumptions_df      = preview_assumptions_df,
  hitter_assumption_map_df    = hitter_assumption_map_df,
  actual_assumption_values_df = actual_assumption_values_df,
  pitcher_baseline_df         = sproat_baseline_df,
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
# WALK RATE PATTERN:
# Three games, same direction: Bradley, Misiorowski, Sproat.
# All three walked far fewer hitters than their prior data suggested.
# The ±7pp tolerance did not catch Sproat (delta -15.8pp).
# Question to investigate: is this lineup swinging at first pitches at an
# above-average rate in 2026? If so, pitchers will consistently look like
# better zone commanders than their prior data indicates.
# Sproat walk rate for next preview: use 12-13% range, not 20%.
#
# SPROAT SWEEPER vs RHH:
# Expected 23.3%, actual 31.1%. 50% whiff rate.
# Got Guerrero twice (pfx_x 1.09 and 1.35 on both Ks).
# BVP flagged Leo Jiménez as Sweeper-vulnerable. Sweeper also dangerous
# for Guerrero. Flag both for next Sproat preview.
#
# CUTTER PROFILE:
# pfx_x = 0.035 tonight. Consistent flat/no-cut pattern across all 2026.
# One hit allowed: Heineman double, pfx_x = 0.17 (slight cut).
# Jays have zero hits on FC pitches with pfx_x > 0.20 all season.
# Cutter vulnerability tracking: now spans 89 FC pitches, 2 games,
# 0 hits on pitches with real horizontal movement.
#
# GUERRERO SWEEPER:
# 2 strikeouts on Sweeper tonight. Not in the BVP flag going in.
# Add to retro memory: Guerrero has now seen 6 Sweeper pitches in this
# game, 2 strikeouts. Watch in future matchups vs Sweeper-heavy pitchers.
#
# SERIES RECAP:
# TOR 1-2 vs MIL. All three offensive blueprints failed on the same
# assumption class: pitchers commanded the zone better than expected.
# Common thread may be lineup approach, not pitcher improvement.
#
# RUNNING N: ~108 hitter-games after observation 16.
#
# =============================================================================
# END · retro_runner_2026-04-16_sproat.R
# =============================================================================
