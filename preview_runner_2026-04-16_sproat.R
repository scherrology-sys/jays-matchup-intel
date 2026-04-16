# =============================================================================
# Jays Matchup Intel V3.2
# Preview Runner · MIL @ TOR · April 16, 2026
# Pitcher: Brandon Sproat (RHP) · ID: TBD (confirm from game file)
# Observation 16 · LOW confidence
# @scherrology | Arm Chair Analyst
# =============================================================================
# source("analysis/jays_matchup_intel_v3_2.R")
# =============================================================================
# KEY CONTEXT:
# Sproat's 2026 walk rate is 20.0% across three starts (22%, 18%, 20%).
# His Cutter, added in 2026, has a .511 xwOBA. Most hittable pitch in arsenal.
# Pitch-mix baseline projects .417 vs LHH and .391 vs RHH, both above league avg.
# This is the most favorable matchup projected for the Jays this season.
# =============================================================================

library(tidyverse)

# =============================================================================
# 1. LOAD DATA
# =============================================================================

# savant_data__55_.csv · Sproat pitcher file (2025-2026)
sproat_raw <- safe_read_csv("savant_data__55_.csv")

# 2026 starts only (Mar 29, Apr 4, Apr 11)
sproat_2026 <- sproat_raw |> filter(game_year == 2026)
pitcher_sample <- nrow(sproat_2026)
# 222 pitches across 3 starts

# Full Jays hitter history
jays_raw  <- safe_read_csv("blue-jays-hitters-25-26.csv")
eloy_raw  <- safe_read_csv("eloy-jimenez-25-26.csv")
andres_raw <- safe_read_csv("andres-gimenez-25-26.csv")

hitter_history_df <- bind_rows(
  jays_raw   |> filter(game_year %in% c(2024, 2025)),
  eloy_raw   |> filter(game_year %in% c(2024, 2025))
)

# =============================================================================
# 2. PITCHER INPUTS · 2026 primary signal
# =============================================================================

PITCHER_NAME <- "Brandon Sproat"
PITCHER_HAND <- "R"
PITCHER_ARCH <- "RHP_starter_mil_sinker"
# PITCHER_ID: confirm from tonight's game file after the game

# 2026 pitch mix by handedness
# vs LHH: Sinker 35.9%, Cutter 26.7%, Changeup 16.0%, Curveball 12.2%, 4-Seam 6.1%, Sweeper 3.1%
# vs RHH: Sinker 37.8%, Sweeper 23.3%, Cutter 16.7%, Curveball 13.3%, 4-Seam 5.6%, Changeup 3.3%

pitcher_mix_lhh <- c(
  "Sinker"          = 0.359,
  "Cutter"          = 0.267,
  "Changeup"        = 0.160,
  "Curveball"       = 0.122,
  "4-Seam Fastball" = 0.061,
  "Sweeper"         = 0.031
)

pitcher_mix_rhh <- c(
  "Sinker"          = 0.378,
  "Sweeper"         = 0.233,
  "Cutter"          = 0.167,
  "Curveball"       = 0.133,
  "4-Seam Fastball" = 0.056,
  "Changeup"        = 0.033
)

# NOTE: "Sweeper" is Statcast pitch_type "ST". Map accordingly.
# The Cutter (.511 xwOBA) and Sinker (.409 xwOBA) are well above league avg.
# This produces a pitch-mix baseline of .417 vs LHH and .391 vs RHH.

# =============================================================================
# 3. CURRENT ROSTER
# Springer (IL), Kirk (IL) excluded.
# =============================================================================

batter_pool <- c(
  "Guerrero Jr., Vladimir",
  "Jiménez, Eloy",
  "Okamoto, Kazuma",
  "Barger, Addison",
  "Lukes, Nathan",
  "Schneider, Davis",
  "Giménez, Andrés",
  "Varsho, Daulton",
  "Sánchez, Jesús",
  "Jiménez, Leo",
  "Clement, Ernie",
  "Heineman, Tyler",
  "Straw, Myles"
)

# =============================================================================
# 4. ASSUMPTIONS
# =============================================================================

assumptions_df <- tibble(
  assumption_name   = c(
    "walk_rate",
    "cutter_contact_quality",
    "sweeper_vs_rhh_pct"
  ),
  expected_value    = c(
    0.200,   # Walk rate: 20.0% in 2026 (consistent across all 3 starts)
    0.511,   # Cutter xwOBA: .511 career, most hittable pitch
    0.233    # Sweeper to RHH: 23.3% in 2026
  ),
  tolerance         = c(
    0.070,   # ±7pp on walk rate (widened from prior games)
    0.060,   # ±0.060 xwOBA on Cutter contact quality
    0.070    # ±7pp on Sweeper usage
  ),
  importance_weight = c(
    0.95,    # Walk rate is the defining variable tonight
    0.80,    # Cutter quality determines RHH/LHH damage potential
    0.70     # Sweeper is RHH suppression tool
  ),
  affected_hand     = c("ALL", "ALL", "R"),
  impact_direction  = c("up", "up", "down"),
  default_shift     = c(0.010, 0.008, 0.000),
  ci_widen_factor   = c(1.15, 1.10, 1.10),
  description       = c(
    "Walk rate 20% in 2026 consistently — primary offensive path for the lineup",
    "Cutter .511 xwOBA — most hittable pitch, 22.6% of all pitches in 2026",
    "Sweeper to RHH at 23.3% — best pitch, suppresses Leo Jimenez (0/2 BVP)"
  )
)

# =============================================================================
# 5. RUN PREVIEW
# =============================================================================

sproat_preview <- build_preview_v32(
  game_date               = "2026-04-16",
  team                    = "TOR",
  opponent                = "MIL",
  pitcher_name            = PITCHER_NAME,
  pitcher_id              = NA_integer_,  # confirm from tonight's game file
  pitcher_hand            = PITCHER_HAND,
  pitcher_sample          = pitcher_sample,
  batter_pool             = batter_pool,
  pitcher_mix_lhh         = pitcher_mix_lhh,
  pitcher_mix_rhh         = pitcher_mix_rhh,
  hitter_history_df       = hitter_history_df,
  assumptions_df          = assumptions_df,
  hitter_pitch_exposure_df = NULL,
  pitcher_archetype       = PITCHER_ARCH,
  memory_dir              = "model_memory",
  output_dir              = "outputs/preview_v32"
)

# =============================================================================
# 6. REVIEW
# =============================================================================

sproat_preview$preview_df |>
  select(player_name, hand, exp_wOBA, CI_lo_90, CI_hi_90,
         prior_delta, fragility_score, confidence) |>
  mutate(across(where(is.numeric), \(x) round(x, 3))) |>
  print(n = Inf)

sproat_preview$preview_meta |> print()

# =============================================================================
# NOTES:
# Pitch-mix baseline is above league average for both handedness groups.
# First game this season where champion model projects above the hitter prior.
# Guerrero (.391) and Barger (.359) are Edge. Lukes (.356) near the line.
# Leo Jimenez 0/2 BVP, both strikeouts on the Sweeper. Flagged.
# Confirm pitcher ID from tonight's game file for the retro runner.
# Varsho: applying +0.020 directional correction from retro memory flag.
# Running n: ~100 hitter-games entering observation 16.
# =============================================================================
