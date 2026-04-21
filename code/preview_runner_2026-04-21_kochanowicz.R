# =============================================================================
# Jays Matchup Intel V3.2
# Preview Runner · TOR @ LAA · April 21, 2026
# Pitcher: Jack Kochanowicz (RHP) · ID: 686799
# Observation 21 · MODERATE confidence
# @scherrology | Arm Chair Analyst
# =============================================================================
# source("analysis/jays_matchup_intel_v3_2.R")
# =============================================================================
# MODEL CONSTANTS (V3.2):
#   LEAGUE_WOBA = 0.320
#   K_PRIOR     = 60
#   MIN_PA      = 10
#   MODERATE: 373 pitches (200-700 band) · threshold ±0.030
#   Edge > 0.350  |  Suppressed < 0.290
# =============================================================================
# ROSTER NOTE:
#   Only 2026 Blue Jays hitters used. Batter pool includes Kirk and Springer
#   (on roster, currently IL). Wagner Will and other former Jays excluded.
#   Eloy, Okamoto, Sanchez: no 2024-25 history in hitter file → league fallback.
# =============================================================================
# 2026 CONTEXT:
#   Changeup: 27.2% (from 17.6% career-weighted) · best pitch .312 xwOBA · 30% whiff
#   Sinker: 35.2% (from 48.1% career-weighted) · still primary pitch
#   2026 BB%: 15.2% avg (range 8.7%-22.7%) vs career 9.5% · command concern
#   2026 K%: 18.2% avg (range 7.4%-30.4%) vs career 13.1%
#   Slider xwOBA .401 on contact — weakest pitch in file despite 28.1% whiff
#   vs LHH (n=189): CH 32.3%, FF 31.2%, SI 27.5%, SL 9.0%
#   vs RHH (n=183): SI 43.2%, SL 23.0%, CH 21.9%, FF 12.0%
# =============================================================================
# VALIDATED EXP_WOBA (compute_arsenal_expectation, 2024-25 RHP splits):
#   Springer      R  .446  Edge    [OUT — removed from batter pool]
#   Varsho        L  .382  Edge    [CH .575/32, FF .269/91, SI .283/20, SL .385/19]
#   Kirk          R  .373  Edge    [SI .343/88, SL .254/71, CH .542/19, FF .403/85 · IL]
#   Guerrero Jr.  R  .370  Edge    [SI .440/124, FF .408/117, SL .330/81, CH .255/41]
#   Barger        L  .337  Neutral [LHH · CH/FF/SI entered · prior .338]
#   Heineman      L  .334  Neutral [LHH · CH/FF/SI entered · SL fallback · prior .316]
#   Schneider     R  .329  Neutral [SI .177/19, SL .428/16, FF .538/34 · CH fallback]
#   Lukes         L  .297  Neutral [LHH · prior .320 · BvP: 3PA wOBA .528]
#   Okamoto       R  .320  Neutral [no 2024-25 history · league fallback]
#   Sánchez       L  .320  Neutral [no 2024-25 history · league fallback]
#   Jiménez, Eloy R  .320  Neutral [no 2024-25 history · league fallback]
#   Clement       R  .279  Supp    [all 4 pitches entered · BvP 5PA wOBA .178 · prior .283]
#   Straw         R  .240  Supp    [SI/SL/FF entered · CH fallback · Sinker-heavy mix hurts]
#   Giménez, A.   L  .216  Supp    [LHH · CH/FF/SI splits pull exp from prior .276]
# =============================================================================

library(tidyverse)

# =============================================================================
# 1. LOAD DATA
# =============================================================================

# savant_data__76_.csv · Kochanowicz pitcher file (2024-2026)
# savant_data__75_.csv · Jays BVP vs Kochanowicz (current roster only)
# savant_data__60_.csv · Blue Jays hitter history 2024-25
kochi_raw   <- safe_read_csv("savant_data__76_.csv")
bvp_raw     <- safe_read_csv("savant_data__75_.csv")
hitter_raw  <- safe_read_csv("savant_data__60_.csv")

# 2026 starts (Mar 29, Apr 4, Apr 10, Apr 15)
kochi_2026     <- kochi_raw |> filter(game_year == 2026)
pitcher_sample <- nrow(kochi_2026)
# 373 pitches → MODERATE confidence

# Hitter history: 2024-2025, vs RHP (p_throws = "R")
# ONLY 2026 Blue Jays hitters — excludes Wagner, Bichette, Horwitz etc.
ROSTER_2026 <- c(
  "Guerrero Jr., Vladimir", "Jiménez, Eloy", "Okamoto, Kazuma",
  "Barger, Addison", "Lukes, Nathan", "Schneider, Davis",
  "Giménez, Andrés", "Varsho, Daulton", "Sánchez, Jesús",
  "Clement, Ernie", "Heineman, Tyler", "Straw, Myles",
  "Kirk, Alejandro"
  # NOTE: Springer out — removed from batter pool
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

PITCHER_ID   <- 686799L
PITCHER_NAME <- "Jack Kochanowicz"
PITCHER_HAND <- "R"
PITCHER_ARCH <- "RHP_starter_laa_sinker_changeup"

# 2026 mix by handedness
# Changeup up significantly (17.6% career → 27.2% in 2026)
# Sinker down (48.1% career → 35.2% in 2026)
pitcher_mix_lhh <- c(
  "Changeup"        = 0.323,
  "4-Seam Fastball" = 0.312,
  "Sinker"          = 0.275,
  "Slider"          = 0.090
)

pitcher_mix_rhh <- c(
  "Sinker"          = 0.432,
  "Slider"          = 0.230,
  "Changeup"        = 0.219,
  "4-Seam Fastball" = 0.120
)

# Year-weighted xwOBA: CH .312 (best), FF .350, SI .374, SL .401 (worst on contact)

# =============================================================================
# 3. BATTER POOL
# =============================================================================

batter_pool <- ROSTER_2026

# NOTE: Kirk (IL) and Springer (IL) included — on 2026 roster with meaningful
#   BVP history (6 PA each). Active status determines lineup inclusion game-day.
# NOTE: Eloy, Okamoto, Sánchez → LEAGUE_WOBA fallback (.320)
# NOTE: Heineman bats L vs RHP (switch hitter)
# NOTE: Straw bats R vs RHP (confirmed from data)

# =============================================================================
# 4. ASSUMPTIONS
# =============================================================================

assumptions_df <- tibble(
  assumption_name   = c(
    "changeup_vs_lhh_pct",
    "walk_rate",
    "sinker_vs_rhh_pct"
  ),
  expected_value    = c(
    0.323,   # Changeup to LHH: 32.3% in 2026. Primary driver of LHH tier calls.
    0.152,   # BB rate: 15.2% avg (range 8.7%-22.7%). Wide tolerance needed.
    0.432    # Sinker to RHH: 43.2%. Primary RHH pitch.
  ),
  tolerance         = c(
    0.070,   # ±7pp on Changeup to LHH
    0.080,   # ±8pp on walk rate (wide: 8.7%-22.7% range in 2026)
    0.080    # ±8pp on Sinker to RHH
  ),
  importance_weight = c(
    1.00,    # Changeup rate drives Varsho Edge and Gimenez Suppressed calls
    0.90,    # Walk rate shifts run environment significantly
    0.80     # Sinker rate affects Straw and Clement Suppressed calls
  ),
  affected_hand     = c("L", "ALL", "R"),
  impact_direction  = c("widen", "up", "widen"),
  default_shift     = c(0.000, 0.010, 0.000),
  ci_widen_factor   = c(1.15, 1.10, 1.10),
  description       = c(
    "Changeup to LHH 32.3% in 2026. Varsho .575/32PA on CH. Drives LHH Edge call.",
    "BB% 15.2% avg vs career 9.5%. Range 8.7%-22.7%. Command is the game.",
    "Sinker to RHH 43.2%. .374 xwOBA. Straw and Clement Suppressed driven by Sinker splits."
  )
)

hitter_pitch_exposure_df <- tibble(
  player_name     = c("Varsho, Daulton","Barger, Addison","Giménez, Andrés",
                      "Lukes, Nathan","Sánchez, Jesús"),
  assumption_name = rep("changeup_vs_lhh_pct", 5),
  exposure_weight = c(0.95, 0.85, 0.90, 0.80, 0.75)
)

# =============================================================================
# 5. RUN PREVIEW
# =============================================================================

kochi_preview <- build_preview_v32(
  game_date                = "2026-04-21",
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

kochi_preview$preview_df |>
  select(player_name, hand, exp_wOBA, CI_lo_90, CI_hi_90,
         prior_delta, fragility_score, confidence) |>
  mutate(across(where(is.numeric), \(x) round(x, 3))) |>
  print(n = Inf)

kochi_preview$preview_meta |> print()

# =============================================================================
# END · preview_runner_2026-04-21_kochanowicz.R
# =============================================================================
