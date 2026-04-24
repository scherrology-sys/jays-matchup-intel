# =============================================================================
# Jays Matchup Intel V3.2
# Preview Runner · TOR vs CLE · April 23, 2026
# Pitcher: Gavin Williams (RHP) · ID: 668909
# Observation 23 · MODERATE confidence
# @scherrology | Arm Chair Analyst
# =============================================================================
# source("analysis/jays_matchup_intel_v3_2.R")
# =============================================================================
# MODEL CONSTANTS (V3.2):
#   LEAGUE_WOBA = 0.320
#   K_PRIOR     = 60
#   MIN_PA      = 10
#   MODERATE: 469 pitches (200-700 band) · threshold +-0.030
#   Edge > 0.350  |  Suppressed < 0.290
# =============================================================================
# PT_MAP NOTE:
#   Complete PT_MAP used: FF, SI, ST, FC, CU, SL, CH, CS, KC
#   2026 arsenal: FF, ST, CU, FC, SI only
#   SL and CH: 0 pitches in 2026 (gone entirely)
# =============================================================================
# 2026 CONTEXT:
#   Sweeper (ST): 27.3% overall, 42.0% vs RHH · up from 14% career-weighted
#   4-Seam: 28.1% · Curveball: 17.9% · Cutter: 13.9% · Sinker: 12.8%
#   Slider: GONE in 2026 · Changeup: GONE in 2026
#   K%: 34.8% avg (range 18.2%-43.5%) vs career 25.4%
#   BB%: 14.8% avg (range 3.8%-27.3%) vs career 11.5%
#   Most recent start Apr 18: K%=42.3%, BB%=3.8% · cleanest of season
#   vs LHH: FF 33.5%, CU 24.6%, FC 21.7%, ST 17.4%, SI 2.8%
#   vs RHH: ST 42.0%, SI 27.7%, FF 20.2%, CU 8.0%, FC 2.1%
#   BVP file (savant_data__31_): pitch-by-pitch 2025 games only
#     Contains former Jays (Bichette, Kirk, Springer, Santander, Roden)
#     EXCLUDED from computation , not on 2026 roster
#     Current roster BVP: Guerrero 6PA wOBA .614, Lukes 5PA wOBA .278
#     Gimenez 4PA wOBA .540, Barger 5PA wOBA .317
# =============================================================================
# VALIDATED EXP_WOBA (compute_arsenal_expectation, 2024-25 RHP splits):
#   Schneider     R  .365  Edge    [SI .177/19, FF .538/34, FC .139/10 · ST+CU fallback]
#   Lukes         L  .359  Edge    [all 5 LHH pitches entered · prior .320 · BvP .278 caution]
#   Guerrero Jr.  R  .340  Neutral [ST .245/34(key), SI .440/124, FF .408/117, CU .313/14, FC .361/47]
#   Varsho        L  .326  Neutral [FF .269/91, CU .312/11, FC .639/15, ST .070/10(thin), SI .283/20]
#   Barger        L  .320  Neutral [FF .277/141, CU .118/28, FC .374/42, ST .606/17, SI .423/42]
#   Giménez, A.   L  .305  Neutral [FF .332/93, CU .332/16, FC .278/15, SI .132/24 · ST fallback]
#   Okamoto       R  .320  Neutral [no 2024-25 history · league fallback]
#   Sánchez       L  .320  Neutral [no 2024-25 history · league fallback]
#   Jiménez, Eloy R  .320  Neutral [no 2024-25 history · league fallback]
#   Straw         R  .279  Supp    [SI/FF/FC entered · ST fallback · below-avg RHH splits]
#   Clement       R  .268  Supp    [all RHH pitches entered · combined exp below .290]
#   Heineman      L  .253  Supp    [LHH · FF/FC/SI entered · ST+CU fallback · BvP 2K in 2PA]
# =============================================================================

library(tidyverse)

# =============================================================================
# 1. LOAD DATA
# =============================================================================

# savant_data__32_.csv · Williams pitcher file (2024-2026)
# savant_data__31_.csv · BVP vs Williams (2025 games, current roster filtered)
# savant_data__60_.csv · Blue Jays hitter history 2024-25
williams_raw <- safe_read_csv("savant_data__32_.csv")
bvp_raw      <- safe_read_csv("savant_data__31_.csv")
hitter_raw   <- safe_read_csv("savant_data__60_.csv")

# 2026 starts (Mar 27, Apr 1, Apr 7, Apr 13, Apr 18)
williams_2026  <- williams_raw |> filter(game_year == 2026)
pitcher_sample <- nrow(williams_2026)
# 469 pitches → MODERATE confidence

# BVP: filter to current 2026 roster only
# Bichette, Kirk, Springer, Santander, Roden NOT on 2026 roster , excluded
ROSTER_2026 <- c(
  "Guerrero Jr., Vladimir", "Jiménez, Eloy", "Okamoto, Kazuma",
  "Barger, Addison", "Lukes, Nathan", "Schneider, Davis",
  "Giménez, Andrés", "Varsho, Daulton", "Sánchez, Jesús",
  "Clement, Ernie", "Heineman, Tyler", "Straw, Myles"
)

bvp_current <- bvp_raw |> filter(player_name %in% ROSTER_2026)

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

PITCHER_ID   <- 668909L
PITCHER_NAME <- "Gavin Williams"
PITCHER_HAND <- "R"
PITCHER_ARCH <- "RHP_starter_cle_sweeper_4seam"

# 2026 mix: SL and CH completely absent (0 pitches across 469)
pitcher_mix_lhh <- c(
  "4-Seam Fastball" = 0.335,
  "Curveball"       = 0.246,
  "Cutter"          = 0.217,
  "Sweeper"         = 0.174,
  "Sinker"          = 0.028
)

pitcher_mix_rhh <- c(
  "Sweeper"         = 0.420,
  "Sinker"          = 0.277,
  "4-Seam Fastball" = 0.202,
  "Curveball"       = 0.080,
  "Cutter"          = 0.021
)

# Year-weighted xwOBA by pitch:
#   Curveball: .224 (32.8% whiff, 81.4 mph)
#   Sweeper:   .241 (44.8% whiff, 86.6 mph) · best put-away pitch
#   Slider:    .269 (gone in 2026)
#   Sinker:    .333 (9.4% whiff, 95.9 mph)
#   Cutter:    .361 (22.4% whiff, 91.8 mph)
#   4-Seam:    .383 (23.1% whiff, 96.6 mph)

# =============================================================================
# 3. BATTER POOL
# =============================================================================

batter_pool <- ROSTER_2026
# Springer: OUT · Kirk: OUT
# Eloy, Okamoto, Sanchez: no 2024-25 RHP history → LEAGUE_WOBA fallback (.320)
# Heineman: bats L vs RHP
# Straw: bats R vs RHP
# NOTE: Varsho Sweeper split .070/10PA (thin, barely clears MIN_PA)
# NOTE: Guerrero Sweeper split .245/34PA , 34PA is solid, enters at 42% weight

# =============================================================================
# 4. ASSUMPTIONS
# =============================================================================

assumptions_df <- tibble(
  assumption_name   = c(
    "sweeper_vs_rhh_pct",
    "walk_rate",
    "k_rate"
  ),
  expected_value    = c(
    0.420,   # Sweeper to RHH: 42.0% in 2026. Primary driver of RHH tier calls.
    0.148,   # BB rate: 14.8% avg (range 3.8%-27.3%). Wide tolerance needed.
    0.348    # K rate: 34.8% avg (range 18.2%-43.5%).
  ),
  tolerance         = c(
    0.080,   # +-8pp on Sweeper to RHH
    0.100,   # +-10pp on BB rate (wide: 3.8%-27.3% range)
    0.100    # +-10pp on K rate
  ),
  importance_weight = c(
    1.00,    # Sweeper rate drives all RHH tier placements
    0.90,    # Walk rate shifts run environment significantly
    0.80     # K rate downstream of Sweeper/Curveball effectiveness
  ),
  affected_hand     = c("R", "ALL", "ALL"),
  impact_direction  = c("widen", "up", "widen"),
  default_shift     = c(0.000, 0.010, 0.000),
  ci_widen_factor   = c(1.15, 1.10, 1.10),
  description       = c(
    "Sweeper 42% vs RHH in 2026. Guerrero .245/34PA · Straw/Schneider affected.",
    "BB% 14.8% avg vs career 11.5%. Range 3.8%-27.3%. Apr 18 was 3.8%.",
    "K% 34.8% avg vs career 25.4%. Sweeper 44.8% whiff driving the gain."
  )
)

hitter_pitch_exposure_df <- tibble(
  player_name     = c("Guerrero Jr., Vladimir","Straw, Myles","Clement, Ernie",
                      "Okamoto, Kazuma","Jiménez, Eloy"),
  assumption_name = rep("sweeper_vs_rhh_pct", 5),
  exposure_weight = c(0.95, 0.90, 0.85, 0.80, 0.80)
)

# =============================================================================
# 5. RUN PREVIEW
# =============================================================================

williams_preview <- build_preview_v32(
  game_date                = "2026-04-23",
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

williams_preview$preview_df |>
  select(player_name, hand, exp_wOBA, CI_lo_90, CI_hi_90,
         prior_delta, fragility_score, confidence) |>
  mutate(across(where(is.numeric), \(x) round(x, 3))) |>
  print(n = Inf)

williams_preview$preview_meta |> print()

# =============================================================================
# END · preview_runner_2026-04-23_williams.R
# =============================================================================
