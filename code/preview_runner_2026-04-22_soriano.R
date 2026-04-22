# =============================================================================
# Jays Matchup Intel V3.2
# Preview Runner · TOR @ LAA · April 22, 2026
# Pitcher: José Soriano (RHP) · ID: 667755
# Observation 22 · MODERATE confidence
# @scherrology | Arm Chair Analyst
# =============================================================================
# source("analysis/jays_matchup_intel_v3_2.R")
# =============================================================================
# MODEL CONSTANTS (V3.2):
#   LEAGUE_WOBA = 0.320
#   K_PRIOR     = 60
#   MIN_PA      = 10
#   MODERATE: 487 pitches (200-700 band) · threshold ±0.030
#   Edge > 0.350  |  Suppressed < 0.290
# =============================================================================
# CRITICAL LIMITATION — SPLITTER BLIND SPOT:
#   The Splitter is Soriano's best pitch (.218 xwOBA, 37.6% whiff).
#   It appears 16.9% of LHH pitches and 11.0% of RHH pitches in 2026.
#   No current Jay has ≥10 qualifying PA vs Splitter in hitter history.
#   Every hitter falls back to wOBA_overall for the Splitter weight.
#   This is the largest source of structural uncertainty in this preview.
# =============================================================================
# 2026 CONTEXT:
#   K% jumped from career 22.0% to 32.5% across 5 starts (range 19%-38.5%)
#   BB% 10.8% (career 10.4%) — consistent, no command concern
#   4-Seam up significantly: 32.4% in 2026 vs 19.0% career-weighted
#   Sinker down: 42.5% in 2026 vs 59.3% career-weighted
#   Velocity: Sinker 97.4 mph, 4-Seam 98.3 mph — highest-velocity profile this season
#   vs LHH: SI 37.4%, FF 34.2%, FS 24.5%, SL 3.9%
#   vs RHH: SI 46.5%, FF 31.0%, SL 11.5%, FS 11.0%
#   BVP file (savant_data__28_): aggregated summary stats only (pitches, wOBA, BA, SLG)
#     Not pitch-by-pitch. Below MIN_PA for any pitch type. Directional only.
#     Guerrero: 10 pitches wOBA .819 · Varsho: 9 pitches wOBA .711
#     Barger: 14 pitches wOBA .525 · Lukes: 8 pitches wOBA .000
# =============================================================================
# VALIDATED EXP_WOBA (compute_arsenal_expectation, 2024-25 RHP splits):
#   Guerrero Jr.  R  .409  Edge    [SI .440/124, FF .408/117, SL .330/81 · FS fallback]
#   Heineman      L  .378  Edge    [LHH · SI/FF entered · FS fallback (24.5% LHH) · SL fallback]
#   Barger        L  .350  Neutral [exactly at threshold · SI .423/42, FF .277/141, SL .374/49 · FS fallback]
#   Schneider     R  .343  Neutral [SI .177/19, FF .538/34, SL .428/16 · FS fallback]
#   Clement       R  .311  Neutral [SI/FF/SL all entered · FS fallback · prior .283]
#   Lukes         L  .306  Neutral [LHH · SI/FF/SL entered · FS fallback · prior .320]
#   Varsho        L  .301  Neutral [LHH · SI .283/20, FF .269/91, SL .385/19 · FS fallback]
#   Okamoto       R  .320  Neutral [no 2024-25 history · league fallback]
#   Sánchez       L  .320  Neutral [no 2024-25 history · league fallback]
#   Jiménez, Eloy R  .320  Neutral [no 2024-25 history · league fallback]
#   Straw         R  .264  Supp    [SI/FF/SL entered · FS fallback · below-avg RHH splits]
#   Giménez, A.   L  .240  Supp    [LHH · SI .132/24PA primary signal · FF .332/93 · SL .235/31]
# =============================================================================

library(tidyverse)

# =============================================================================
# 1. LOAD DATA
# =============================================================================

# savant_data__29_.csv · Soriano pitcher file (2024-2026)
# savant_data__28_.csv · Jays BVP vs Soriano (aggregated summary — NOT pitch-by-pitch)
# savant_data__60_.csv · Blue Jays hitter history 2024-25
soriano_raw <- safe_read_csv("savant_data__29_.csv")
bvp_raw     <- safe_read_csv("savant_data__28_.csv")
hitter_raw  <- safe_read_csv("savant_data__60_.csv")

# NOTE: bvp_raw has columns: pitches, player_id, player_name, total_pitches,
#   pitch_percent, ba, iso, babip, slg, woba
#   This is an aggregated Savant stat export — not pitch-by-pitch game data.
#   It cannot be used for pitch-split computation. Directional context only.

# 2026 starts (Mar 26, Mar 31, Apr 6, Apr 12, Apr 17)
soriano_2026   <- soriano_raw |> filter(game_year == 2026)
pitcher_sample <- nrow(soriano_2026)
# 487 pitches → MODERATE confidence

# Hitter history: 2024-2025, vs RHP (p_throws = "R")
# ONLY 2026 Blue Jays hitters — Springer and Kirk currently out
ROSTER_2026 <- c(
  "Guerrero Jr., Vladimir", "Jiménez, Eloy", "Okamoto, Kazuma",
  "Barger, Addison", "Lukes, Nathan", "Schneider, Davis",
  "Giménez, Andrés", "Varsho, Daulton", "Sánchez, Jesús",
  "Clement, Ernie", "Heineman, Tyler", "Straw, Myles"
)

hitter_history_df <- hitter_raw |>
  filter(
    game_year   %in% c(2024, 2025),
    p_throws    == "R",
    player_name %in% ROSTER_2026
  )

# =============================================================================
# 2. PITCHER INPUTS · 2026 primary signal
# =============================================================================

PITCHER_ID   <- 667755L
PITCHER_NAME <- "José Soriano"
PITCHER_HAND <- "R"
PITCHER_ARCH <- "RHP_starter_laa_sinker_splitter"

# 2026 mix by handedness
# 4-Seam up from 19.0% career to 32.4% in 2026
# Sinker down from 59.3% career to 42.5% in 2026
# Splitter: 16.9% LHH, 11.0% RHH — BLIND SPOT: 0 PA for any Jay
pitcher_mix_lhh <- c(
  "Sinker"          = 0.374,
  "4-Seam Fastball" = 0.342,
  "Splitter"        = 0.245,
  "Slider"          = 0.039
)

pitcher_mix_rhh <- c(
  "Sinker"          = 0.465,
  "4-Seam Fastball" = 0.310,
  "Slider"          = 0.115,
  "Splitter"        = 0.110
)

# Year-weighted xwOBA by pitch:
#   Splitter: .218 (best, 37.6% whiff) — NO Jay has ≥10 PA vs this pitch
#   Slider:   .258 (39.3% whiff)
#   Sinker:   .331 (97.4 mph)
#   4-Seam:   .381 (98.3 mph)

# =============================================================================
# 3. BATTER POOL
# =============================================================================

batter_pool <- ROSTER_2026
# Springer: OUT (excluded from batter pool)
# Kirk: OUT (excluded from batter pool)
# Eloy, Okamoto, Sánchez: no 2024-25 RHP history → LEAGUE_WOBA fallback (.320)
# Heineman: bats L vs RHP (switch hitter)
# Straw: bats R vs RHP

# =============================================================================
# 4. ASSUMPTIONS
# =============================================================================

assumptions_df <- tibble(
  assumption_name   = c(
    "splitter_vs_lhh_pct",
    "foursm_pct",
    "k_rate"
  ),
  expected_value    = c(
    0.245,   # Splitter to LHH: 24.5% in 2026. Primary blind spot pitch.
    0.324,   # 4-Seam overall: 32.4% in 2026 (up from 19.0% career).
    0.325    # K rate: 32.5% avg (range 19.0%-38.5%)
  ),
  tolerance         = c(
    0.060,   # ±6pp on Splitter to LHH
    0.060,   # ±6pp on 4-Seam rate
    0.100    # ±10pp on K rate (19%-38.5% range)
  ),
  importance_weight = c(
    1.00,    # Splitter is best pitch and full blind spot — highest importance
    0.70,    # 4-Seam usage affects RHH matchup quality
    0.80     # K rate is downstream signal of Splitter/Slider effectiveness
  ),
  affected_hand     = c("L", "ALL", "ALL"),
  impact_direction  = c("widen", "widen", "widen"),
  default_shift     = c(0.000, 0.000, 0.000),
  ci_widen_factor   = c(1.20, 1.10, 1.10),
  description       = c(
    "Splitter 24.5% vs LHH. No Jay has ≥10 PA vs this pitch. Full fallback to prior.",
    "4-Seam up from 19.0% career to 32.4% in 2026. Structural 2026 change.",
    "K% 32.5% in 2026 vs career 22.0%. Range 19%-38.5% across 5 starts."
  )
)

hitter_pitch_exposure_df <- tibble(
  player_name     = c("Guerrero Jr., Vladimir","Heineman, Tyler","Barger, Addison",
                      "Varsho, Daulton","Giménez, Andrés"),
  assumption_name = rep("splitter_vs_lhh_pct", 5),
  exposure_weight = c(0.60, 0.95, 0.95, 0.95, 0.90)
  # Guerrero is RHH so lower LHH Splitter exposure; LHH hitters are fully exposed
)

# =============================================================================
# 5. RUN PREVIEW
# =============================================================================

soriano_preview <- build_preview_v32(
  game_date                = "2026-04-22",
  team                     = "TOR",
  opponent                 = "LAA",
  pitcher_name             = PITCHER_NAME,
  pitcher_id               = PITCHER_ID,
  pitcher_hand             = PITCHER_HAND,
  pitcher_sample           = pitcher_sample,
  batter_pool              = batter_pool,
  pitcher_mix_lhh          = pitcher_mix_lhh,
  pitcher_mix_rhh          = pitcher_mix_rhh,
  hitter_history_df        = hitter_history_df,
  assumptions_df           = assumptions_df,
  hitter_pitch_exposure_df = hitter_pitch_exposure_df,
  pitcher_archetype        = PITCHER_ARCH,
  memory_dir               = "model_memory",
  output_dir               = "outputs/preview_v32"
)

# =============================================================================
# 6. REVIEW
# =============================================================================

soriano_preview$preview_df |>
  select(player_name, hand, exp_wOBA, CI_lo_90, CI_hi_90,
         prior_delta, fragility_score, confidence) |>
  mutate(across(where(is.numeric), \(x) round(x, 3))) |>
  print(n = Inf)

soriano_preview$preview_meta |> print()

# Note: CI widening for Splitter-exposed hitters is set to 1.20 factor
# because the model has zero pitch-specific signal for the primary weapon.
# The fragility_score for LHH hitters will be elevated.

# =============================================================================
# END · preview_runner_2026-04-22_soriano.R
# =============================================================================
