# =============================================================================
# Jays Matchup Intel V3.2
# Preview Runner · TOR vs CLE · April 26, 2026
# Pitcher: Slade Cecconi (RHP) · ID: 677944
# Observation 25 · MODERATE confidence
# @scherrology | Arm Chair Analyst
# =============================================================================
# source("analysis/jays_matchup_intel_v3_2.R")
# =============================================================================
# MODEL CONSTANTS (V3.2):
#   LEAGUE_WOBA = 0.320
#   K_PRIOR     = 60
#   MIN_PA      = 10
#   MODERATE: 445 pitches (200-700 band) · threshold +-0.030
#   Edge > 0.350  |  Suppressed < 0.290
# =============================================================================
# PT_MAP NOTE:
#   Complete PT_MAP used: FF, SI, FC, CU, ST, SL, CH, FS, KC
#   2026 arsenal: FF, FC, CU, ST, SI only
#   SL: 0.2% in 2026 (13.1% career-weighted) · effectively gone
#   CH: 0.0% in 2026 (5.8% career-weighted) · completely gone
#   Hitter Slider and Changeup splits are NOT entered in computation
# =============================================================================
# 2026 CONTEXT:
#   Overall: FF 35.4%, FC 23.9%, CU 19.1%, ST 11.7%, SI 9.7%
#   vs LHH: FF 41.9%, CU 25.1%, FC 22.5%, ST 6.6%, SI 3.5%
#   vs RHH: FF 28.6%, FC 25.3%, ST 17.1%, SI 16.1%, CU 12.9%
#   Cutter: 9.4% career-weighted · jumped to 23.9% in 2026 · major change
#   Sweeper: 4.0% career-weighted · jumped to 11.7% in 2026
#   K%: 17.4% avg (range 7.4%-28.6%) vs career 19.4%
#   BB%: 10.4% avg (range 4.3%-25.0%) vs career 6.1% · elevated
#   No BVP file for this game
# =============================================================================
# VALIDATED EXP_WOBA (compute_arsenal_expectation, 2024-25 RHP splits):
#   Guerrero Jr.  R  .361  Edge    [FF .408/117, FC .361/47, ST .245/34, SI .440/124, CU .313/14 · all 5 entered]
#   Varsho        L  .351  Edge    [LHH · FC .639/15(key at 22.5% LHH) · FF .269/91 · CU .312/11 · ST .070/10 · SI .283/20]
#   Schneider     R  .340  Neutral [FF .538/34 · FC .139/10 · SI .177/19 · ST+CU fallback · prior .410]
#   Lukes         L  .331  Neutral [LHH · FF/CU/FC/SI entered · ST fallback · prior .320]
#   Giménez, A.   L  .309  Neutral [LHH · FF .332/93 · CU .332/16 · FC .278/15 · SI .132/24 · ST fallback]
#   Okamoto       R  .320  Neutral [no 2024-25 history · league fallback]
#   Sánchez       L  .320  Neutral [no 2024-25 history · league fallback]
#   Jiménez, Eloy R  .320  Neutral [no 2024-25 history · league fallback]
#   Barger        L  .286  Supp    [LHH · CU .118/28(primary) at 25.1% LHH · FF .277/141 · FC .374/42 · ST .606/17 · SI .423/42]
#   Straw         R  .281  Supp    [RHH · FF/FC/SI entered · ST+CU fallback · below-avg RHH splits]
#   Clement       R  .278  Supp    [RHH · all pitches entered · combined exp below .290 · prior .283]
#   Heineman      L  .248  Supp    [LHH · FF/FC/SI entered · ST .070/10 entered · CU fallback · prior .316]
# =============================================================================

library(tidyverse)

# =============================================================================
# 1. LOAD DATA
# =============================================================================

# savant_data__85_.csv · Cecconi pitcher file (2024-2026)
# No BVP file for this game
# savant_data__60_.csv · Blue Jays hitter history 2024-25
cecconi_raw <- safe_read_csv("savant_data__85_.csv")
hitter_raw  <- safe_read_csv("savant_data__60_.csv")

# 2026 starts (Mar 29, Apr 5, Apr 10, Apr 15, Apr 20)
cecconi_2026   <- cecconi_raw |> filter(game_year == 2026)
pitcher_sample <- nrow(cecconi_2026)
# 445 pitches · MODERATE confidence

ROSTER_2026 <- c(
  "Guerrero Jr., Vladimir", "Jiménez, Eloy", "Okamoto, Kazuma",
  "Barger, Addison", "Lukes, Nathan", "Schneider, Davis",
  "Giménez, Andrés", "Varsho, Daulton", "Sánchez, Jesús",
  "Clement, Ernie", "Heineman, Tyler", "Straw, Myles"
)

# Hitter history: 2024-2025, vs RHP
hitter_history_df <- hitter_raw |>
  filter(
    game_year   %in% c(2024, 2025),
    p_throws    == "R",
    player_name %in% ROSTER_2026
  )

# =============================================================================
# 2. PITCHER INPUTS · 2026 primary signal
# =============================================================================

PITCHER_ID   <- 677944L
PITCHER_NAME <- "Slade Cecconi"
PITCHER_HAND <- "R"
PITCHER_ARCH <- "RHP_starter_cle_4seam_cutter"

# 2026 mix: SL (0.2%) and CH (0%) are gone · not included
pitcher_mix_lhh <- c(
  "4-Seam Fastball" = 0.419,
  "Curveball"       = 0.251,
  "Cutter"          = 0.225,
  "Sweeper"         = 0.066,
  "Sinker"          = 0.035
)

pitcher_mix_rhh <- c(
  "4-Seam Fastball" = 0.286,
  "Cutter"          = 0.253,
  "Sweeper"         = 0.171,
  "Sinker"          = 0.161,
  "Curveball"       = 0.129
)

# Year-weighted xwOBA by pitch:
#   Curveball: .306 (32.6% whiff, 75.2 mph) · best pitch
#   Sweeper:   .269 (24.0% whiff, 81.2 mph)
#   Cutter:    .341 (15.3% whiff, 87.6 mph)
#   4-Seam:    .390 (18.0% whiff, 94.2 mph)
#   Sinker:    .392 (4.5% whiff, 93.5 mph)
#   Slider:    .315 · GONE in 2026 · not in mix
#   Changeup:  .336 · GONE in 2026 · not in mix

# =============================================================================
# 3. BATTER POOL
# =============================================================================

batter_pool <- ROSTER_2026
# Springer: OUT · Kirk: OUT
# Eloy, Okamoto, Sanchez: no 2024-25 RHP history · LEAGUE_WOBA fallback
# Heineman: bats L vs RHP
# NOTE: Slider and Changeup splits in hitter history are not entered
#   (neither pitch appears in 2026 Cecconi mix)
# NOTE: Varsho Cutter split .639/15PA · thin but over MIN_PA · entered at 22.5% weight
# NOTE: Barger Curveball split .118/28PA · 28PA is solid · primary Suppressed driver

# =============================================================================
# 4. ASSUMPTIONS
# =============================================================================

assumptions_df <- tibble(
  assumption_name   = c(
    "cutter_vs_lhh_pct",
    "curveball_vs_lhh_pct",
    "bb_rate"
  ),
  expected_value    = c(
    0.225,   # Cutter to LHH: 22.5% · key driver of Varsho Edge call (.639 split)
    0.251,   # Curveball to LHH: 25.1% · key driver of Barger Suppressed (.118/28PA)
    0.104    # BB rate: 10.4% avg (range 4.3%-25.0%) · elevated vs career 6.1%
  ),
  tolerance         = c(
    0.070,   # +-7pp on Cutter to LHH
    0.070,   # +-7pp on Curveball to LHH
    0.100    # +-10pp on BB rate (wide: career avg 6.1% · 2026 range large)
  ),
  importance_weight = c(
    1.00,    # Cutter rate drives Varsho Edge call (thin but over MIN_PA)
    0.95,    # Curveball rate drives Barger Suppressed (28PA · solid signal)
    0.80     # BB rate changes run environment
  ),
  affected_hand     = c("L", "L", "ALL"),
  impact_direction  = c("widen", "widen", "up"),
  default_shift     = c(0.000, 0.000, 0.005),
  ci_widen_factor   = c(1.10, 1.10, 1.10),
  description       = c(
    "Cutter 22.5% vs LHH · jumped from 9.4% career · Varsho FC split .639/15PA.",
    "Curveball 25.1% vs LHH · Barger CU split .118/28PA is primary Suppressed driver.",
    "BB% 10.4% avg vs career 6.1% · range 4.3%-25.0% · elevated since 2026 start."
  )
)

hitter_pitch_exposure_df <- tibble(
  player_name     = c("Varsho, Daulton","Barger, Addison","Lukes, Nathan",
                      "Giménez, Andrés","Heineman, Tyler"),
  assumption_name = c("cutter_vs_lhh_pct","curveball_vs_lhh_pct",
                      "cutter_vs_lhh_pct","curveball_vs_lhh_pct","cutter_vs_lhh_pct"),
  exposure_weight = c(0.95, 0.95, 0.70, 0.75, 0.70)
)

# =============================================================================
# 5. RUN PREVIEW
# =============================================================================

cecconi_preview <- build_preview_v32(
  game_date                = "2026-04-26",
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

cecconi_preview$preview_df |>
  select(player_name, hand, exp_wOBA, CI_lo_90, CI_hi_90,
         prior_delta, fragility_score, confidence) |>
  mutate(across(where(is.numeric), \(x) round(x, 3))) |>
  print(n = Inf)

cecconi_preview$preview_meta |> print()

# =============================================================================
# END · preview_runner_2026-04-26_cecconi.R
# =============================================================================
