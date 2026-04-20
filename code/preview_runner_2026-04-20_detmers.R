# =============================================================================
# Jays Matchup Intel V3.2
# Preview Runner · TOR @ LAA · April 20, 2026
# Pitcher: Reid Detmers (LHP) · ID: 672282
# Observation 20 · MODERATE confidence
# @scherrology | Arm Chair Analyst
# =============================================================================
# source("analysis/jays_matchup_intel_v3_2.R")
# =============================================================================
# MODEL CONSTANTS (V3.2):
#   LEAGUE_WOBA = 0.320
#   K_PRIOR     = 60
#   MIN_PA      = 10
#   MODERATE: 391 pitches (200-700 band) · threshold ±0.030
#   Edge > 0.350  |  Suppressed < 0.290
# =============================================================================
# CRITICAL: DETMERS IS LHP (p_throws = "L")
#   hitter_history_df must filter to p_throws == "L" NOT "R"
#   Switch hitters (Heineman, Straw) bat RIGHT against LHP
#   Their LHP priors and RHH pitch splits enter the computation
# =============================================================================
# 2026 CONTEXT:
#   Arsenal consistent with career — no structural 2026 changes
#   Slider: 33.3% overall (30.3% career-weighted) · .240 xwOBA · 37.5% whiff · best pitch
#   Changeup: 10.3% · .266 xwOBA · 34.3% whiff · second weapon
#   vs LHH: Slider 37.1%, 4-Seam 35.5%, Sinker 13.7%, CU 11.3%, CH 2.4%
#   vs RHH: 4-Seam 46.2%, Slider 31.6%, CH 13.9%, CU 8.3%
#   K%: 42.9%, 15.4%, 18.2%, 36.0% per start · avg 27.7% (career 28.6%)
#   BB%: 0%, 15.4%, 9.1%, 0% per start · avg 6.4% (career 9.2%)
#   BVP (2025, file 71): Clement 2PA single+double, Straw 2PA two singles, Heineman 1PA K
# =============================================================================
# VALIDATED EXP_WOBA (compute_arsenal_expectation, 2024-25 LHP splits):
#   Heineman      R  .442  Edge    [bats R vs LHP · 4-Seam entered · Slider/CH/CU fallback]
#   Guerrero Jr.  R  .433  Edge    [4-Seam .479/42, Slider .421/11, CH .328/35 · CU fallback]
#   Clement       R  .375  Edge    [all 4 RHH pitches entered · BvP single+double]
#   Varsho        L  .363  Edge    [LHH · 4-Seam .479/18, Sinker .228/15 · SL/CU/CH fallback]
#   Lukes         L  .346  Neutral [LHH · 4-Seam + Sinker entered · SL/CU/CH fallback]
#   Barger        L  .330  Neutral [LHH · Slider .254/17, 4-Seam .477/19, Sinker .214/24]
#   Sánchez       L  .320  Neutral [no LHP history, league fallback]
#   Okamoto       R  .320  Neutral [no LHP history, league fallback]
#   Jiménez, Eloy R  .320  Neutral [no LHP history, league fallback]
#   Schneider     R  .313  Neutral [4-Seam .248/35, Slider .426/13, CH .383/22, CU .132/12]
#   Straw         R  .288  Supp    [bats R vs LHP · prior .290 · Slider/CH pull exp down]
#   Giménez, A.   L  .214  Supp    [LHH · 4-Seam .000/15PA · Slider .371/13 · Sinker .340/24]
# =============================================================================

library(tidyverse)

# =============================================================================
# 1. LOAD DATA
# =============================================================================

# savant_data__72_.csv · Detmers pitcher file (2024-2026)
# savant_data__71_.csv · Jays BVP vs Detmers (2025 games only)
# savant_data__60_.csv · Blue Jays hitter history 2024-25
detmers_raw <- safe_read_csv("savant_data__72_.csv")
bvp_raw     <- safe_read_csv("savant_data__71_.csv")
hitter_raw  <- safe_read_csv("savant_data__60_.csv")

# 2026 starts (Mar 28, Apr 3, Apr 8, Apr 14)
detmers_2026   <- detmers_raw |> filter(game_year == 2026)
pitcher_sample <- nrow(detmers_2026)
# 391 pitches → MODERATE confidence

# CRITICAL: filter to p_throws == "L" for LHP history
# This gives each hitter their correct batting stance vs a left-handed pitcher
# Switch hitters Heineman and Straw appear with stand = "R" in this filter
hitter_history_df <- hitter_raw |>
  filter(
    game_year %in% c(2024, 2025),
    p_throws  == "L"          # LHP, not RHP
  )

# =============================================================================
# 2. PITCHER INPUTS · 2026 primary signal
# =============================================================================

PITCHER_ID   <- 672282L
PITCHER_NAME <- "Reid Detmers"
PITCHER_HAND <- "L"           # LHP
PITCHER_ARCH <- "LHP_starter_laa_slider"

# 2026 mix by handedness
pitcher_mix_lhh <- c(
  "Slider"          = 0.371,
  "4-Seam Fastball" = 0.355,
  "Sinker"          = 0.137,
  "Curveball"       = 0.113,
  "Changeup"        = 0.024
)

pitcher_mix_rhh <- c(
  "4-Seam Fastball" = 0.462,
  "Slider"          = 0.316,
  "Changeup"        = 0.139,
  "Curveball"       = 0.083
)

# Year-weighted xwOBA:
#   Slider: .240  Changeup: .266  Curveball: .280  4-Seam: .326  Sinker: .366

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
  "Clement, Ernie",
  "Heineman, Tyler",
  "Straw, Myles"
)

# NOTE: Eloy, Okamoto, Sánchez have no LHP history → LEAGUE_WOBA fallback (.320)
# NOTE: Heineman and Straw are switch hitters, batting R vs LHP.

# =============================================================================
# 4. ASSUMPTIONS
# =============================================================================

assumptions_df <- tibble(
  assumption_name   = c(
    "slider_vs_lhh_pct",
    "changeup_vs_rhh_pct",
    "k_rate"
  ),
  expected_value    = c(
    0.371,   # Slider to LHH: 37.1% in 2026. Primary pitch for same-hand matchup.
    0.139,   # Changeup to RHH: 13.9%. Guerrero's CH split .328 in 35 PA.
    0.277    # K rate: 27.7% avg in 2026 (range 15%-43%)
  ),
  tolerance         = c(
    0.080,   # ±8pp on Slider rate to LHH
    0.060,   # ±6pp on Changeup rate to RHH
    0.120    # ±12pp on K rate (wide: 15%-43% range)
  ),
  importance_weight = c(
    1.00,    # Slider rate drives LHH tier placements (Varsho, Barger, Gimenez)
    0.80,    # Changeup rate affects Guerrero exp meaningfully at .328 split
    0.70     # K rate is downstream of pitch quality
  ),
  affected_hand     = c("L", "R", "ALL"),
  impact_direction  = c("widen", "widen", "widen"),
  default_shift     = c(0.000, 0.000, 0.000),
  ci_widen_factor   = c(1.15, 1.10, 1.10),
  description       = c(
    "Slider primary weapon vs LHH at 37.1%. Drives Varsho, Barger, Gimenez placements.",
    "Changeup to RHH at 13.9%. Guerrero Changeup split .328/35PA in computation.",
    "K% range 15%-43% in 2026. High-K starts had 0% walk rate."
  )
)

hitter_pitch_exposure_df <- tibble(
  player_name     = c("Varsho, Daulton","Barger, Addison","Giménez, Andrés",
                      "Lukes, Nathan","Sánchez, Jesús"),
  assumption_name = rep("slider_vs_lhh_pct", 5),
  exposure_weight = c(0.90, 0.85, 0.95, 0.80, 0.75)
)

# =============================================================================
# 5. RUN PREVIEW
# =============================================================================

detmers_preview <- build_preview_v32(
  game_date                = "2026-04-20",
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

detmers_preview$preview_df |>
  select(player_name, hand, exp_wOBA, CI_lo_90, CI_hi_90,
         prior_delta, fragility_score, confidence) |>
  mutate(across(where(is.numeric), \(x) round(x, 3))) |>
  print(n = Inf)

detmers_preview$preview_meta |> print()

# =============================================================================
# END · preview_runner_2026-04-20_detmers.R
# =============================================================================
