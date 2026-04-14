# =============================================================================
# Jays Matchup Intel V3.2
# Preview Runner — MIL @ TOR, April 14, 2026
# Pitcher: Jacob Misiorowski (RHP) · ID: 669743
# Observation 14 · LOW confidence · First exposure game
# @scherrology | Arm Chair Analyst
# =============================================================================
# source("analysis/jays_matchup_intel_v3_2.R")
# =============================================================================
# NOTE ON CONFIDENCE:
# No Jays hitter has career plate appearances against Misiorowski.
# All exp_wOBA values are driven by hitter prior wOBA blended with
# the pitcher pitch-mix baseline. Pitch-type splits default to
# the hitter's overall wOBA fallback for every pitch type.
# Tier threshold: ±0.045 (LOW confidence band). All hitters fall Neutral.
# =============================================================================

library(tidyverse)

# =============================================================================
# 1. LOAD DATA
# =============================================================================

# savant_data__48_.csv — Misiorowski 2025-2026 pitch file
misi_raw <- safe_read_csv("savant_data__48_.csv")

# Full Jays hitter history file (2024-2025)
# Upload the full hitter file for proper pitch-type split computation.
# Without it, all pitch-type lookups fall back to overall wOBA.
jays_raw    <- safe_read_csv("blue-jays-hitters-25-26.csv")
eloy_raw    <- safe_read_csv("eloy-jimenez-25-26.csv")   # Eloy just called up

# Misiorowski 2026 starts
misi_2026 <- misi_raw |> filter(game_year == 2026)
pitcher_sample <- nrow(misi_2026)
# Result: 290 pitches across 3 starts (Mar 26, Apr 1, Apr 7)

hitter_history_df <- bind_rows(
  jays_raw |> filter(game_year %in% c(2024, 2025)),
  eloy_raw |> filter(game_year %in% c(2024, 2025))
)

# =============================================================================
# 2. PITCHER INPUTS
# 2026 mix used as primary signal (3 starts = 290 pitches)
# =============================================================================

PITCHER_ID   <- 669743L
PITCHER_NAME <- "Jacob Misiorowski"
PITCHER_HAND <- "R"
PITCHER_ARCH <- "RHP_starter_mil_power"

# 2026 pitch mix by handedness (from savant_data__48_.csv analysis)
# vs LHH: 4-Seam 59.3%, Curveball 16.2%, Slider 14.8%, Changeup 9.7%
# vs RHH: 4-Seam 53.5%, Slider 32.2%, Curveball 13.7%, Changeup 0.7%

pitcher_mix_lhh <- c(
  "4-Seam Fastball" = 0.593,
  "Curveball"       = 0.162,
  "Slider"          = 0.148,
  "Changeup"        = 0.097
)

pitcher_mix_rhh <- c(
  "4-Seam Fastball" = 0.535,
  "Slider"          = 0.322,
  "Curveball"       = 0.137,
  "Changeup"        = 0.007
)

# =============================================================================
# 3. CURRENT ROSTER
# Springer (IL), Kirk (IL) excluded.
# Eloy Jimenez added after April 12 callup.
# =============================================================================

batter_pool <- c(
  "Guerrero Jr., Vladimir",
  "Jiménez, Eloy",
  "Okamoto, Kazuma",
  "Barger, Addison",
  "Lukes, Nathan",
  "Schneider, Davis",
  "Varsho, Daulton",
  "Jiménez, Leo",
  "Clement, Ernie",
  "Heineman, Tyler",
  "Straw, Myles"
)

# =============================================================================
# 4. ASSUMPTIONS
# Three testable assumptions for the retro.
# All derived from career/2026 Statcast data.
# =============================================================================

assumptions_df <- tibble(
  assumption_name   = c(
    "changeup_to_lhh_pct",
    "fastball_velo",
    "walk_rate"
  ),
  expected_value    = c(
    0.097,   # Changeup to LHH: 9.7% in 2026
    99.1,    # 4-Seam velocity: 99.1 mph career/2026
    0.115    # Walk rate: 11.5% career weighted
  ),
  tolerance         = c(
    0.05,    # ±5pp on Changeup usage
    1.0,     # ±1.0 mph on velocity
    0.05     # ±5pp on walk rate
  ),
  importance_weight = c(
    0.90,    # Changeup to LHH is primary weapon for that group
    0.70,    # Velocity drives the timing problem for all hitters
    0.80     # Walk rate is the primary offensive path
  ),
  affected_hand     = c("L", "ALL", "ALL"),
  impact_direction  = c("down", "widen", "widen"),
  default_shift     = c(0.010, 0.000, 0.000),
  ci_widen_factor   = c(1.20,  1.25,  1.15),
  description       = c(
    "Misiorowski throws Changeup ~9.7% to LHH — his primary weapon vs lefties",
    "4-Seam velocity stable at 99.1 mph — gap off Curveball (12 mph) drives timing conflict",
    "Walk rate holds near career 11.5% — primary baserunner path for the lineup"
  )
)

# No hitter-pitch exposure overrides — all hitters first exposure

# =============================================================================
# 5. RUN PREVIEW
# =============================================================================

misi_preview <- build_preview_v32(
  game_date               = "2026-04-14",
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

misi_preview$preview_df |>
  select(player_name, hand, exp_wOBA, CI_lo_90, CI_hi_90,
         prior_delta, fragility_score, confidence) |>
  mutate(across(where(is.numeric), \(x) round(x, 3))) |>
  print(n = Inf)

misi_preview$preview_meta |> print()

# =============================================================================
# NOTE: All hitters will show LOW confidence and Neutral placement.
# The exp_wOBA values are prior-only estimates — no pitch-type split
# data against Misiorowski exists for any current Jays hitter.
# The CI widths will be the widest of the season (K=30 at LOW confidence).
# This is the honest output. First exposure game.
# Run the retro after the game. The assumption audit will be the
# first real data point about how this lineup handles Misiorowski.
# =============================================================================
