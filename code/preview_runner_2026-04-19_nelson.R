# =============================================================================
# Jays Matchup Intel V3.2
# Preview Runner · TOR @ ARI · April 19, 2026
# Pitcher: Ryne Nelson (RHP) · ID: 669194
# Observation 19 · MODERATE confidence
# @scherrology | Arm Chair Analyst
# =============================================================================
# source("analysis/jays_matchup_intel_v3_2.R")
# =============================================================================
# MODEL CONSTANTS (V3.2):
#   LEAGUE_WOBA = 0.320
#   K_PRIOR     = 60
#   MIN_PA      = 10
#   MODERATE: 341 pitches (200-700 band) · threshold ±0.030
#   Edge > 0.350  |  Suppressed < 0.290
# =============================================================================
# 2026 CONTEXT:
#   Changeup: zero pitches in 2026 across 341 attempts (career-weighted 4.0%)
#   Slider: 20.5% overall, 32.9% vs RHH (career-weighted 14.2%)
#   4-Seam: 61.6% of pitches at 95.5 mph
#   K%: 21.1%, 14.3%, 20.8%, 33.3% per start · avg 22.4% (career 20.8%)
#   BB%: 15.8%, 14.3%, 0.0%, 9.5% · high variance · avg 9.4% (career 6.2%)
#   BVP: savant_data__67_.csv (2024-2025 games)
#   Guerrero: 5PA wOBA .178 (strikeout, single, three field outs)
#   Varsho:   3PA wOBA .539 (triple, K, field out) · directional only
#   Clement:  5PA wOBA .318 (double, sac fly, GIDP, field outs)
#   Barger:   3PA wOBA .000 (three outs)
# =============================================================================
# VALIDATED EXP_WOBA (compute_arsenal_expectation, 2024-25 RHP splits):
#   Schneider      R  .491  Edge    [4-Seam .538/34, Slider .428/16; CU fallback · SEASON HIGH]
#   Guerrero Jr.   R  .380  Edge    [4-Seam .408/117, Slider .330/81, Cutter .361/47, CU .313/14]
#   Varsho         L  .321  Neutral [4-Seam .269/91, CU .312/11, Slider .385/19, Cutter .639/15]
#   Giménez, A.    L  .316  Neutral [4-Seam .332/93, Slider .252/29, Cutter .278/15; CU fallback]
#   Sánchez        L  .320  Neutral [no 2024-25 history, league fallback]
#   Okamoto        R  .320  Neutral [no 2024-25 history, league fallback]
#   Jiménez, Eloy  R  .320  Neutral [no 2024-25 history, league fallback]
#   Lukes          L  .325  Neutral [4-Seam .370/135, Cutter .232/26; CU + Slider fallback]
#   Straw          R  .303  Neutral [4-Seam .379/50, Slider .???/16, Cutter .290/14; CU fallback]
#   Clement        R  .288  Supp    [4-Seam .312/90, Slider .243/85, Cutter .252/38, CU .281/14]
#   Heineman       L  .268  Supp    [4-Seam .277/35, Cutter .298/16; CU + Slider fallback]
#   Barger         L  .264  Supp    [4-Seam .277/141, Cutter .374/42; CU + Slider fallback]
#   Jiménez, Leo   R  .000  Supp    [11 PA vs RHP 2024-25, all zeros · EXTREME FLAG]
# =============================================================================

library(tidyverse)

# =============================================================================
# 1. LOAD DATA
# =============================================================================

# savant_data__68_.csv · Nelson pitcher file (2024-2026)
# savant_data__67_.csv · Jays BVP vs Nelson (2024-2025 games)
# savant_data__60_.csv · Blue Jays hitter history 2024-25
nelson_raw  <- safe_read_csv("savant_data__68_.csv")
bvp_raw     <- safe_read_csv("savant_data__67_.csv")
hitter_raw  <- safe_read_csv("savant_data__60_.csv")

# 2026 starts (Mar 27, Apr 2, Apr 8, Apr 13)
nelson_2026    <- nelson_raw |> filter(game_year == 2026)
pitcher_sample <- nrow(nelson_2026)
# 341 pitches → MODERATE confidence

# Hitter history: 2024-2025, vs RHP only
hitter_history_df <- hitter_raw |>
  filter(
    game_year %in% c(2024, 2025),
    p_throws  == "R"
  )

# =============================================================================
# 2. PITCHER INPUTS · 2026 primary signal
# =============================================================================

PITCHER_ID   <- 669194L
PITCHER_NAME <- "Ryne Nelson"
PITCHER_HAND <- "R"
PITCHER_ARCH <- "RHP_starter_ari_4seam_slider"

# 2026 mix by handedness (from savant_data__68_.csv, game_year=2026)
# Changeup: zero pitches. Slider doubled from career rate.
# vs LHH: 4-Seam 60.1%, Curveball 20.2%, Slider 11.6%, Cutter 8.1%
# vs RHH: 4-Seam 63.6%, Slider 32.9%, Cutter 2.1%, Curveball 1.4%
pitcher_mix_lhh <- c(
  "4-Seam Fastball" = 0.601,
  "Curveball"       = 0.202,
  "Slider"          = 0.116,
  "Cutter"          = 0.081
)

pitcher_mix_rhh <- c(
  "4-Seam Fastball" = 0.636,
  "Slider"          = 0.329,
  "Cutter"          = 0.021,
  "Curveball"       = 0.014
)

# Year-weighted xwOBA by pitch:
#   Slider: .248  Curveball: .271  4-Seam: .329  Cutter: .342  Changeup: .380 (gone)

# =============================================================================
# 3. CURRENT ROSTER
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

# NOTE: Eloy, Okamoto, Sánchez no 2024-25 history → LEAGUE_WOBA fallback (.320)
# NOTE: Leo 11 PA all zeros → exp_wOBA = 0.000. Extreme flag. Treat directionally only.

# =============================================================================
# 4. ASSUMPTIONS
# =============================================================================

assumptions_df <- tibble(
  assumption_name   = c(
    "changeup_usage",
    "slider_vs_rhh_pct",
    "walk_rate"
  ),
  expected_value    = c(
    0.000,   # Changeup: zero in 2026. Any appearance is notable.
    0.329,   # Slider to RHH: 32.9% in 2026.
    0.094    # BB%: 9.4% avg across 4 starts (range 0%-15.8%)
  ),
  tolerance         = c(
    0.030,   # Flag any Changeup usage > 3%.
    0.080,   # ±8pp on Slider rate to RHH.
    0.080    # ±8pp on walk rate (wide given 0%-15.8% range)
  ),
  importance_weight = c(
    0.70,    # Changeup absence is structural; its return would matter for LHH
    1.00,    # Slider rate drives RHH tier placements
    0.80     # Walk rate affects run environment materially
  ),
  affected_hand     = c("L", "R", "ALL"),
  impact_direction  = c("widen", "widen", "up"),
  default_shift     = c(0.000, 0.000, 0.010),
  ci_widen_factor   = c(1.10, 1.15, 1.10),
  description       = c(
    "Changeup zero in 2026. Was 4.0% career-weighted. Slider absorbed usage.",
    "Slider 32.9% to RHH in 2026 — primary driver of Schneider Edge call at .491.",
    "BB% range 0%-15.8% across 4 starts. Command variance shifts run environment."
  )
)

hitter_pitch_exposure_df <- tibble(
  player_name     = c("Schneider, Davis","Guerrero Jr., Vladimir",
                      "Straw, Myles","Okamoto, Kazuma","Jiménez, Eloy"),
  assumption_name = rep("slider_vs_rhh_pct", 5),
  exposure_weight = c(1.00, 0.90, 0.85, 0.80, 0.80)
)

# =============================================================================
# 5. RUN PREVIEW
# =============================================================================

nelson_preview <- build_preview_v32(
  game_date                = "2026-04-19",
  team                     = "TOR",
  opponent                 = "ARI",
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

nelson_preview$preview_df |>
  select(player_name, hand, exp_wOBA, CI_lo_90, CI_hi_90,
         prior_delta, fragility_score, confidence) |>
  mutate(across(where(is.numeric), \(x) round(x, 3))) |>
  print(n = Inf)

nelson_preview$preview_meta |> print()

# =============================================================================
# END · preview_runner_2026-04-19_nelson.R
# =============================================================================
