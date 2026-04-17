# =============================================================================
# Jays Matchup Intel V3.2
# Preview Runner · TOR @ ARI · April 17, 2026
# Pitcher: Michael Soroka (RHP) · ID: 647336
# Observation 17 · MODERATE confidence
# @scherrology | Arm Chair Analyst
# =============================================================================
# source("analysis/jays_matchup_intel_v3_2.R")
# =============================================================================
# MODEL CONSTANTS (from jays_matchup_intel_v3_2.R):
#   LEAGUE_WOBA = 0.320
#   K_PRIOR     = 60
#   MIN_PA      = 10
#   MODERATE: 200-700 pitches, threshold ±0.030
#   Edge > 0.350  |  Suppressed < 0.290
# =============================================================================
# KEY CONTEXT:
#   Slider: zero pitches in 273 2026 attempts. Career best pitch (.253 xwOBA,
#           42.3% whiff). Replaced by Cutter at 10%.
#   K%: 50%, 13.6%, 40% across three 2026 starts. High variance.
#   Changeup to LHH at 30.9% in 2026 is the K% tell early in the game.
# =============================================================================
# VALIDATED EXP_WOBA (compute_arsenal_expectation, 2024-25 RHP splits):
#   Guerrero Jr.   R  .413  Edge    [4-Seam .408/117, Sinker .444/123, Cutter .361/47]
#   Varsho         L  .398  Edge    [4-Seam .269/91, Changeup .575/32, Cutter .639/15, Sinker .283/20]
#   Schneider      R  .384  Edge    [4-Seam .538/34, Sinker .177/19, Cutter .139/10]
#   Lukes          L  .326  Neutral [4-Seam .370/135, Changeup .297/49, Cutter .232/26, Sinker .260/48]
#   Straw          R  .322  Neutral [bats R vs RHP. 4-Seam .379/50, Sinker .224/31, Cutter .290/14]
#   Sánchez        L  .320  Neutral [no 2024-25 Jays history, league fallback]
#   Okamoto        R  .320  Neutral [no 2024-25 Jays history, league fallback]
#   Jiménez, Eloy  R  .320  Neutral [no 2024-25 Jays history, league fallback]
#   Clement        R  .315  Neutral [4-Seam .312/90, Sinker .339/70, Cutter .266/36]
#   Heineman       L  .295  Neutral [4-Seam .277/35, Changeup .249/21, Sinker .525/16, Cutter fallback <10PA]
#   Barger         L  .308  Neutral [4-Seam .277/141, Changeup .311/55, Cutter .374/42, Sinker .423/42]
#   Giménez, A.    L  .261  Supp    [4-Seam .332/93, Changeup .170/50, Cutter .278/15, Sinker .138/23]
#   Jiménez, Leo   R  .146  Supp    [30 PA history only, all pitches fall back to prior .146]
# =============================================================================

library(tidyverse)

# =============================================================================
# 1. LOAD DATA
# =============================================================================

# savant_data__59_.csv · Soroka pitcher file (2024-2026)
# savant_data__58_.csv · Jays BVP vs Soroka (career)
# savant_data__60_.csv · Blue Jays hitter history 2024-25
soroka_raw <- safe_read_csv("savant_data__59_.csv")
bvp_raw    <- safe_read_csv("savant_data__58_.csv")
hitter_raw <- safe_read_csv("savant_data__60_.csv")

# 2026 Soroka starts only
soroka_2026    <- soroka_raw |> filter(game_year == 2026)
pitcher_sample <- nrow(soroka_2026)
# 273 pitches → MODERATE confidence

# Hitter history: 2024-2025, vs RHP only (p_throws = "R")
# Filters to correct batting stance for each hitter vs a RHP.
# Straw bats R vs RHP (confirmed from data). Heineman bats L vs RHP (switch hitter).
hitter_history_df <- hitter_raw |>
  filter(
    game_year %in% c(2024, 2025),
    p_throws  == "R"
  )

# =============================================================================
# 2. PITCHER INPUTS · 2026 primary signal
# =============================================================================

PITCHER_ID   <- 647336L
PITCHER_NAME <- "Michael Soroka"
PITCHER_HAND <- "R"
PITCHER_ARCH <- "RHP_starter_ari_4seam"

# 2026 mix by handedness. Slider = zero pitches. Cutter replaced it.
pitcher_mix_lhh <- c(
  "4-Seam Fastball" = 0.518,
  "Changeup"        = 0.309,
  "Cutter"          = 0.091,
  "Sinker"          = 0.082
)

pitcher_mix_rhh <- c(
  "4-Seam Fastball" = 0.586,
  "Sinker"          = 0.300,
  "Cutter"          = 0.114
)

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

# NOTE: Jiménez Eloy, Okamoto, Sánchez have no 2024-25 data in savant_data__60_.csv.
# build_preview_v32() assigns LEAGUE_WOBA = 0.320 as fallback for these hitters.

# =============================================================================
# 4. ASSUMPTIONS
# =============================================================================

assumptions_df <- tibble(
  assumption_name   = c(
    "slider_usage",
    "changeup_to_lhh_pct",
    "cutter_usage_pct"
  ),
  expected_value    = c(0.000, 0.309, 0.100),
  tolerance         = c(0.030, 0.070, 0.060),
  importance_weight = c(1.00,  0.90,  0.60),
  affected_hand     = c("ALL", "L",   "ALL"),
  impact_direction  = c("suppress", "widen", "widen"),
  default_shift     = c(0.000, 0.000, 0.000),
  ci_widen_factor   = c(1.00,  1.20,  1.10),
  description       = c(
    "Slider zero in 2026. Any appearance changes profile significantly.",
    "Changeup to LHH 30.9% — Varsho (.575 wOBA/32PA) is the primary exposure.",
    "Cutter at 10% replacing Slider. Small 2026 sample."
  )
)

hitter_pitch_exposure_df <- tibble(
  player_name     = c("Varsho, Daulton","Lukes, Nathan","Barger, Addison",
                      "Heineman, Tyler","Giménez, Andrés","Sánchez, Jesús"),
  assumption_name = rep("changeup_to_lhh_pct", 6),
  exposure_weight = c(0.95, 0.85, 0.80, 0.75, 0.80, 0.70)
)

# =============================================================================
# 5. RUN PREVIEW
# =============================================================================

soroka_preview <- build_preview_v32(
  game_date                = "2026-04-17",
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

soroka_preview$preview_df |>
  select(player_name, hand, exp_wOBA, CI_lo_90, CI_hi_90,
         prior_delta, fragility_score, confidence) |>
  mutate(across(where(is.numeric), \(x) round(x, 3))) |>
  print(n = Inf)

soroka_preview$preview_meta |> print()

# =============================================================================
# END · preview_runner_2026-04-17_soroka.R
# =============================================================================
