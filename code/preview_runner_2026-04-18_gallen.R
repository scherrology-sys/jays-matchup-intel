# =============================================================================
# Jays Matchup Intel V3.2
# Preview Runner · TOR @ ARI · April 18, 2026
# Pitcher: Zac Gallen (RHP) · ID: 668678
# Observation 18 · MODERATE confidence
# @scherrology | Arm Chair Analyst
# =============================================================================
# source("analysis/jays_matchup_intel_v3_2.R")
# =============================================================================
# MODEL CONSTANTS (V3.2):
#   LEAGUE_WOBA = 0.320
#   K_PRIOR     = 60
#   MIN_PA      = 10
#   MODERATE: 343 pitches (200-700 band) · threshold ±0.030
#   Edge > 0.350  |  Suppressed < 0.290
# =============================================================================
# 2026 CONTEXT:
#   Slider jumped to 25.9% overall, 44.7% vs RHH (career-weighted: 14.3%)
#   Knuckle Curve dropped, now generating .608 xwOBA in 2026 (career .289)
#   K%: 10.5%, 9.5%, 21.7%, 9.1% per start · career 22.5%
#   BVP: one 2024 game only (savant_data__63_.csv, Jul 14 2024)
#   Guerrero: 2PA (single, double) · Varsho: 2PA (single, triple)
#   Leo Jimenez: 2PA (walk, GIDP)
# =============================================================================
# VALIDATED EXP_WOBA (compute_arsenal_expectation, 2024-25 RHP splits):
#   Schneider      R  .439  Edge    [Slider .428/16, 4-Seam .538/34, Sinker .177/19]
#   Guerrero Jr.   R  .373  Edge    [Slider .330/81, 4-Seam .408/117, KC .438/11, Sinker .444/123, CH .255/41]
#   Varsho         L  .366  Edge    [4-Seam .269/91, Changeup .575/32, Slider .385/19, Sinker .283/20; KC fallback]
#   Lukes          L  .330  Neutral [4-Seam .370/135, Changeup .297/49, Sinker .260/48; Slider/KC fallback]
#   Sánchez        L  .320  Neutral [no 2024-25 Jays history, league fallback]
#   Okamoto        R  .320  Neutral [no 2024-25 Jays history, league fallback]
#   Jiménez, Eloy  R  .320  Neutral [no 2024-25 Jays history, league fallback]
#   Barger         L  .308  Neutral [4-Seam .277/141, Changeup .311/55, Sinker .423/42; Slider/KC fallback]
#   Heineman       L  .288  Supp    [KC fallback at 21.2% of LHH mix; 4-Seam .277/35, CH .249/21, SI .525/16]
#   Giménez, A.    L  .277  Supp    [4-Seam .332/93, Changeup .170/50, Slider .252/29, Sinker .138/23; KC fallback]
#   Clement        R  .281  Supp    [4-Seam .312/90, Sinker .339/70, Changeup .???/entered; Slider fallback]
#   Straw          R  .270  Supp    [4-Seam .379/50, Sinker .224/31, Slider .???/16; Slider-heavy RHH mix]
#   Jiménez, Leo   R  .000  Supp    [11 PA vs RHP 2024-25, all zeros · EXTREME FLAG, unreliable]
# =============================================================================

library(tidyverse)

# =============================================================================
# 1. LOAD DATA
# =============================================================================

# savant_data__64_.csv · Gallen pitcher file (2024-2026)
# savant_data__63_.csv · Jays BVP vs Gallen (Jul 14 2024 game only)
# savant_data__60_.csv · Blue Jays hitter history 2024-25
gallen_raw  <- safe_read_csv("savant_data__64_.csv")
bvp_raw     <- safe_read_csv("savant_data__63_.csv")
hitter_raw  <- safe_read_csv("savant_data__60_.csv")

# 2026 starts (Mar 26, Apr 1, Apr 7, Apr 12)
gallen_2026    <- gallen_raw |> filter(game_year == 2026)
pitcher_sample <- nrow(gallen_2026)
# 343 pitches → MODERATE confidence

# Hitter history: 2024-2025, vs RHP only
hitter_history_df <- hitter_raw |>
  filter(
    game_year %in% c(2024, 2025),
    p_throws  == "R"
  )

# =============================================================================
# 2. PITCHER INPUTS · 2026 primary signal
# =============================================================================

PITCHER_ID   <- 668678L
PITCHER_NAME <- "Zac Gallen"
PITCHER_HAND <- "R"
PITCHER_ARCH <- "RHP_starter_ari_slider"

# 2026 mix by handedness (from savant_data__64_.csv, game_year=2026)
# Significant shift: Slider jumped to 25.9% overall, 44.7% vs RHH
# Knuckle Curve dropped from 23.2% career-weighted to 16.6% in 2026
pitcher_mix_lhh <- c(
  "4-Seam Fastball" = 0.467,
  "Changeup"        = 0.217,
  "Knuckle Curve"   = 0.212,
  "Slider"          = 0.098,
  "Sinker"          = 0.005
)

pitcher_mix_rhh <- c(
  "Slider"          = 0.447,
  "4-Seam Fastball" = 0.314,
  "Knuckle Curve"   = 0.113,
  "Sinker"          = 0.082,
  "Changeup"        = 0.044
)

# Year-weighted xwOBA by pitch:
#   Changeup: .286  Knuckle Curve: .289  Sinker: .310  Slider: .351  4-Seam: .366
# NOTE: 2026 Knuckle Curve xwOBA = .608 (in-season signal, unstable)

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

# NOTE: Jiménez Eloy, Okamoto, Sánchez have no 2024-25 history in
# savant_data__60_.csv → assigned LEAGUE_WOBA = 0.320 as fallback.
# NOTE: Jiménez Leo has 11 PA vs RHP, all zeros → exp_wOBA = 0.000.
#   This is an extreme artifact of tiny sample. Flag as unreliable.
#   Treat as Suppressed directionally but do not act on the specific value.

# =============================================================================
# 4. ASSUMPTIONS
# =============================================================================

assumptions_df <- tibble(
  assumption_name   = c(
    "slider_vs_rhh_pct",
    "knuckle_curve_quality",
    "k_rate"
  ),
  expected_value    = c(
    0.447,   # Slider to RHH: 44.7% in 2026 · primary assumption
    0.608,   # KC xwOBA: .608 in 2026 (vs .289 career) · tracking whether it stabilizes
    0.127    # K rate: 12.7% avg across 4 2026 starts (10.5, 9.5, 21.7, 9.1%)
  ),
  tolerance         = c(
    0.080,   # ±8pp on Slider rate to RHH
    0.080,   # ±0.080 xwOBA on KC quality
    0.100    # ±10pp on K rate (wide: single start range is 9-22%)
  ),
  importance_weight = c(
    1.00,    # Slider rate drives RHH tier placements
    0.80,    # KC quality determines whether Gallen has a put-away pitch
    0.75     # K rate downstream of the above
  ),
  affected_hand     = c("R", "ALL", "ALL"),
  impact_direction  = c("widen", "widen", "widen"),
  default_shift     = c(0.000, 0.000, 0.000),
  ci_widen_factor   = c(1.15, 1.20, 1.15),
  description       = c(
    "Slider to RHH 44.7% in 2026, up from 14.3% career-weighted — primary RHH weapon",
    "Knuckle Curve xwOBA .608 in 2026 vs .289 career — major regression from best pitch",
    "K rate 12.7% avg in 2026 vs 22.5% career — driven by KC deterioration"
  )
)

hitter_pitch_exposure_df <- tibble(
  player_name     = c("Schneider, Davis","Guerrero Jr., Vladimir",
                      "Straw, Myles","Okamoto, Kazuma","Jiménez, Eloy"),
  assumption_name = rep("slider_vs_rhh_pct", 5),
  exposure_weight = c(1.00, 0.90, 0.90, 0.85, 0.85)
)

# =============================================================================
# 5. RUN PREVIEW
# =============================================================================

gallen_preview <- build_preview_v32(
  game_date                = "2026-04-18",
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

gallen_preview$preview_df |>
  select(player_name, hand, exp_wOBA, CI_lo_90, CI_hi_90,
         prior_delta, fragility_score, confidence) |>
  mutate(across(where(is.numeric), \(x) round(x, 3))) |>
  print(n = Inf)

gallen_preview$preview_meta |> print()

# =============================================================================
# END · preview_runner_2026-04-18_gallen.R
# =============================================================================
