# =============================================================================
# Jays Matchup Intel V3.2
# Preview Runner — MIL @ TOR, April 15, 2026
# Pitcher: Chad Patrick (RHP) · ID: 669062
# Observation 15 · LOW confidence · First exposure game
# @scherrology | Arm Chair Analyst
# =============================================================================
# source("analysis/jays_matchup_intel_v3_2.R")
# =============================================================================
# NOTES:
# Patrick was a 2025 rookie. The Jays did not face Milwaukee in 2025.
# Only Sánchez, Jesús has career PA against Patrick (2 PA, directional only).
# All other hitters fall back to overall wOBA prior.
# Pitch-mix baseline lands at .310 for both handedness groups.
# Guerrero is the only Edge hitter, at .358, driven by prior alone.
# =============================================================================

library(tidyverse)

# =============================================================================
# 1. LOAD DATA
# =============================================================================

# savant_data__51_.csv — Patrick pitcher file (2025-2026)
# savant_data__52_.csv — Patrick batter file (confirms BVP)
patrick_raw <- safe_read_csv("savant_data__51_.csv")
batter_raw  <- safe_read_csv("savant_data__52_.csv")

# 2026 starts (Mar 28, Apr 4, Apr 10)
patrick_2026 <- patrick_raw |> filter(game_year == 2026)
pitcher_sample <- nrow(patrick_2026)
# 210 pitches across 3 starts

# Full Jays hitter history
jays_raw  <- safe_read_csv("blue-jays-hitters-25-26.csv")
eloy_raw  <- safe_read_csv("eloy-jimenez-25-26.csv")
andres_raw <- safe_read_csv("andres-gimenez-25-26.csv")  # if available

hitter_history_df <- bind_rows(
  jays_raw   |> filter(game_year %in% c(2024, 2025)),
  eloy_raw   |> filter(game_year %in% c(2024, 2025))
)

# =============================================================================
# 2. PITCHER INPUTS — 2026 primary signal
# =============================================================================

PITCHER_ID   <- 669062L
PITCHER_NAME <- "Chad Patrick"
PITCHER_HAND <- "R"
PITCHER_ARCH <- "RHP_starter_mil_contact"

# 2026 pitch mix by handedness (from savant_data__51_.csv)
# vs LHH: 4-Seam 29.5%, Cutter 27.4%, Sinker 26.3%, Slurve 14.7%, Changeup 2.1%
# vs RHH: Cutter 42.6%, Sinker 28.7%, 4-Seam 16.5%, Slurve 10.4%, Changeup 1.7%

pitcher_mix_lhh <- c(
  "4-Seam Fastball" = 0.295,
  "Cutter"          = 0.274,
  "Sinker"          = 0.263,
  "Slurve"          = 0.147,
  "Changeup"        = 0.021
)

pitcher_mix_rhh <- c(
  "Cutter"          = 0.426,
  "Sinker"          = 0.287,
  "4-Seam Fastball" = 0.165,
  "Slurve"          = 0.104,
  "Changeup"        = 0.017
)

# NOTE: "Slurve" is Statcast pitch_type "SV". If hitter pitch-type splits
# use different naming, map SV -> Slurve before running build_preview_v32().
# The fallback to overall wOBA will apply for Slurve since hitter split data
# is unlikely to have sufficient PA against this specific pitch type.

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
    "slurve_usage_pct",
    "fastball_contact_quality",
    "cutter_to_rhh_pct"
  ),
  expected_value    = c(
    0.124,   # Slurve: 12.4% overall in 2026 (LHH 14.7%, RHH 10.4%)
    0.275,   # 4-Seam xwOBA: expected to hold near career .275
    0.426    # Cutter to RHH: 42.6% in 2026
  ),
  tolerance         = c(
    0.060,   # ±6pp on Slurve (new weapon, wider tolerance)
    0.040,   # ±0.040 xwOBA (contact quality)
    0.070    # ±7pp on Cutter rate (walk-rate lesson: widen pitch-mix tolerances)
  ),
  importance_weight = c(
    0.80,    # Slurve is the 2026 unknown variable
    0.85,    # 4-Seam xwOBA drives contact quality story
    0.75     # Cutter rate determines RHH assignment difficulty
  ),
  affected_hand     = c("ALL", "ALL", "R"),
  impact_direction  = c("widen", "widen", "widen"),
  default_shift     = c(0.000, 0.000, 0.000),
  ci_widen_factor   = c(1.20, 1.15, 1.10),
  description       = c(
    "Slurve at 12.4% in 2026, up from 4.4% career weighted — new primary off-speed",
    "4-Seam at .275 xwOBA despite 94 mph — movement or location suppressing contact",
    "Cutter-heavy to RHH at 42.6% — primary weapon for right side of lineup"
  )
)

# =============================================================================
# 5. RUN PREVIEW
# =============================================================================

patrick_preview <- build_preview_v32(
  game_date               = "2026-04-15",
  team                    = "TOR",
  opponent                = "MIL",
  pitcher_name            = PITCHER_NAME,
  pitcher_id              = PITCHER_ID,
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

patrick_preview$preview_df |>
  select(player_name, hand, exp_wOBA, CI_lo_90, CI_hi_90,
         prior_delta, fragility_score, confidence) |>
  mutate(across(where(is.numeric), \(x) round(x, 3))) |>
  print(n = Inf)

patrick_preview$preview_meta |> print()

# =============================================================================
# NOTES FOR THIS PREVIEW:
#
# BVP CONTEXT:
# Sánchez, Jesús: 2 PA (field_out on Changeup 0-1 count, single on Cutter 2-2).
# wOBA = .444 but xwOBA = .207 — the hit was weak contact, the out was hard.
# 2 PA is directional only. Does not move tier placement.
#
# SLURVE MAPPING:
# Statcast codes the pitch as "SV". It does not appear in standard pitch-type
# split lookups as "Slurve". The model will fall back to overall wOBA for this
# pitch type in the hitter split computation. This is the correct behavior —
# no hitter has meaningful Slurve splits against Patrick specifically.
#
# VARSHO NOTE:
# Four consecutive games of significant overperformance vs model expectations.
# A +0.020 directional correction has been flagged in retro memory.
# Apply if running the full in-season updating layer.
#
# RUNNING N: ~93 hitter-games entering observation 15.
# =============================================================================
