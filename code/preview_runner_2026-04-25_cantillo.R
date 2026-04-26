# =============================================================================
# Jays Matchup Intel V3.2
# Preview Runner · TOR vs CLE · April 25, 2026
# Pitcher: Joey Cantillo (LHP) · ID: 676282
# Observation 24 · MODERATE confidence
# @scherrology | Arm Chair Analyst
# =============================================================================
# source("analysis/jays_matchup_intel_v3_2.R")
# =============================================================================
# MODEL CONSTANTS (V3.2):
#   LEAGUE_WOBA = 0.320
#   K_PRIOR     = 60
#   MIN_PA      = 10
#   MODERATE: 432 pitches (200-700 band) · threshold +-0.030
#   Edge > 0.350  |  Suppressed < 0.290
# =============================================================================
# CRITICAL: CANTILLO IS LHP (p_throws = "L")
#   hitter_history_df must filter to p_throws == "L" NOT "R"
#   Switch hitters Heineman and Straw bat RIGHT vs LHP
#   Their RHH splits vs LHP and LHP priors enter the computation
# =============================================================================
# 2026 CONTEXT:
#   Arsenal: FF 48.6%, CH 20.8%, SL 16.7%, CU 13.9% · 4 pitches only
#   Changeup: .199 xwOBA, 48.3% whiff · best pitch in file by xwOBA
#   Curveball: .218 xwOBA, 26.3% whiff · second best
#   4-Seam: 91.8 mph · below-average velocity but effective with off-speed
#   K%: 29.1% avg (range 19.0%-42.9%) vs career 27.3% · consistent
#   BB%: 11.7% avg (range 9.5%-16.7%) vs career 10.3% · slightly elevated
#   vs LHH: FF 45.1%, SL 37.2%, CH 10.6%, CU 7.1%
#   vs RHH: FF 49.8%, CH 24.5%, CU 16.3%, SL 9.4%
#   BVP (file 81): Gimenez 1 PA field out only. Not meaningful.
# =============================================================================
# VALIDATED EXP_WOBA (compute_arsenal_expectation, 2024-25 LHP splits):
#   Heineman      R  .445  Edge    [bats R vs LHP · FF .493/13PA entered · CH/CU/SL fallback · prior .398]
#   Guerrero Jr.  R  .424  Edge    [FF .479/42PA · CH .328/35PA · SL .421/11PA · CU fallback (9PA) · prior .399]
#   Varsho        L  .391  Edge    [LHH · FF .479/18PA entered · SL/CH/CU fallback · prior .318]
#   Lukes         L  .360  Edge    [LHH · FF entered · SL/CH/CU fallback · prior .301]
#   Barger        L  .358  Edge    [LHH · FF + SL entered · CH/CU fallback · prior .270]
#   Clement       R  .334  Neutral [all 4 RHH pitches entered · prior .380]
#   Okamoto       R  .320  Neutral [no LHP history · league fallback]
#   Sánchez       L  .320  Neutral [no LHP history · league fallback]
#   Jiménez, Eloy R  .320  Neutral [no LHP history · league fallback]
#   Schneider     R  .279  Supp    [FF .248/35PA · CH .383/22PA · CU .132/12PA · SL .426/13PA · all 4 entered]
#   Straw         R  .255  Supp    [bats R vs LHP · FF/CH/SL entered · CU fallback · below-avg LHP splits]
#   Giménez, A.   L  .177  Supp    [LHH · FF .000/15PA (primary signal) · SL .371/13PA · CH+CU fallback]
# =============================================================================

library(tidyverse)

# =============================================================================
# 1. LOAD DATA
# =============================================================================

# savant_data__82_.csv · Cantillo pitcher file (2024-2026)
# savant_data__81_.csv · BVP (Gimenez 1 PA only · not meaningful)
# savant_data__60_.csv · Blue Jays hitter history 2024-25
cantillo_raw <- safe_read_csv("savant_data__82_.csv")
bvp_raw      <- safe_read_csv("savant_data__81_.csv")
hitter_raw   <- safe_read_csv("savant_data__60_.csv")

# 2026 starts (Mar 28, Apr 3, Apr 8, Apr 14, Apr 19)
cantillo_2026  <- cantillo_raw |> filter(game_year == 2026)
pitcher_sample <- nrow(cantillo_2026)
# 432 pitches → MODERATE confidence

# CRITICAL: p_throws == "L" for LHP
# Heineman and Straw appear with stand = "R" in LHP history (switch hitters)
hitter_history_df <- hitter_raw |>
  filter(
    game_year   %in% c(2024, 2025),
    p_throws    == "L",
    player_name %in% c(
      "Guerrero Jr., Vladimir", "Jiménez, Eloy", "Okamoto, Kazuma",
      "Barger, Addison", "Lukes, Nathan", "Schneider, Davis",
      "Giménez, Andrés", "Varsho, Daulton", "Sánchez, Jesús",
      "Clement, Ernie", "Heineman, Tyler", "Straw, Myles"
    )
  )

# =============================================================================
# 2. PITCHER INPUTS · 2026 primary signal
# =============================================================================

PITCHER_ID   <- 676282L
PITCHER_NAME <- "Joey Cantillo"
PITCHER_HAND <- "L"
PITCHER_ARCH <- "LHP_starter_cle_changeup_4seam"

pitcher_mix_lhh <- c(
  "4-Seam Fastball" = 0.451,
  "Slider"          = 0.372,
  "Changeup"        = 0.106,
  "Curveball"       = 0.071
)

pitcher_mix_rhh <- c(
  "4-Seam Fastball" = 0.498,
  "Changeup"        = 0.245,
  "Curveball"       = 0.163,
  "Slider"          = 0.094
)

# Year-weighted xwOBA:
#   Changeup: .199 (48.3% whiff, 78.4 mph) · best pitch
#   Curveball: .218 (26.3% whiff, 77.3 mph)
#   Slider: .363 (32.1% whiff, 84.0 mph)
#   4-Seam: .374 (16.9% whiff, 91.8 mph)

# =============================================================================
# 3. BATTER POOL
# =============================================================================

batter_pool <- c(
  "Guerrero Jr., Vladimir", "Jiménez, Eloy", "Okamoto, Kazuma",
  "Barger, Addison", "Lukes, Nathan", "Schneider, Davis",
  "Giménez, Andrés", "Varsho, Daulton", "Sánchez, Jesús",
  "Clement, Ernie", "Heineman, Tyler", "Straw, Myles"
)
# Springer: OUT · Kirk: OUT
# Eloy, Okamoto, Sanchez: no LHP history → LEAGUE_WOBA fallback (.320)
# Heineman: bats R vs LHP (switch hitter)
# Straw: bats R vs LHP (switch hitter)
# Gimenez: FF .000/15PA vs LHP · most extreme Suppressed call of season

# =============================================================================
# 4. ASSUMPTIONS
# =============================================================================

assumptions_df <- tibble(
  assumption_name   = c(
    "changeup_vs_rhh_pct",
    "slider_vs_lhh_pct",
    "k_rate"
  ),
  expected_value    = c(
    0.245,   # Changeup to RHH: 24.5% · best pitch and primary Supp driver
    0.372,   # Slider to LHH: 37.2% · primary LHH weapon
    0.291    # K rate: 29.1% avg (range 19.0%-42.9%)
  ),
  tolerance         = c(
    0.060,   # +-6pp on Changeup to RHH
    0.080,   # +-8pp on Slider to LHH
    0.100    # +-10pp on K rate
  ),
  importance_weight = c(
    1.00,    # Changeup rate drives Schneider/Straw Suppressed and Guerrero Edge
    0.90,    # Slider rate drives LHH matchup quality for Gimenez, Varsho
    0.70     # K rate downstream signal
  ),
  affected_hand     = c("R", "L", "ALL"),
  impact_direction  = c("widen", "widen", "widen"),
  default_shift     = c(0.000, 0.000, 0.000),
  ci_widen_factor   = c(1.10, 1.15, 1.10),
  description       = c(
    "Changeup 24.5% vs RHH · .199 xwOBA · 48.3% whiff · Schneider CH split .383/22PA.",
    "Slider 37.2% vs LHH · most LHH hitters fall back to prior on this pitch.",
    "K% 29.1% avg (range 19%-43%) · consistent with career 27.3%."
  )
)

hitter_pitch_exposure_df <- tibble(
  player_name     = c("Schneider, Davis","Straw, Myles","Guerrero Jr., Vladimir",
                      "Clement, Ernie","Okamoto, Kazuma"),
  assumption_name = rep("changeup_vs_rhh_pct", 5),
  exposure_weight = c(0.95, 0.90, 0.85, 0.80, 0.75)
)

# =============================================================================
# 5. RUN PREVIEW
# =============================================================================

cantillo_preview <- build_preview_v32(
  game_date                = "2026-04-25",
  team                     = "TOR",
  opponent                 = "CLE",
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

cantillo_preview$preview_df |>
  select(player_name, hand, exp_wOBA, CI_lo_90, CI_hi_90,
         prior_delta, fragility_score, confidence) |>
  mutate(across(where(is.numeric), \(x) round(x, 3))) |>
  print(n = Inf)

cantillo_preview$preview_meta |> print()

# =============================================================================
# END · preview_runner_2026-04-25_cantillo.R
# =============================================================================
