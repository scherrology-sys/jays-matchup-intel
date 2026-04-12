# =============================================================================
# Jays Matchup Intel
# April 10, 2026 · MIN @ TOR · Rogers Centre
# =============================================================================

source("jays_matchup_intel_v3_2.R")    # private engine
source("code/jays_matchup_helpers.R")  # public constants

# =============================================================================
# 1. LOAD DATA
# =============================================================================

wr_career         <- safe_read_csv("savant_data__wr_career_.csv")
h24               <- safe_read_csv("savant_data__19_.csv")
h25               <- safe_read_csv("savant_data__18_.csv")

WR_ID             <- 680573L
GAME_DATE         <- "2026-04-10"

hitter_history_df <- bind_rows(h24, h25)

# =============================================================================
# 2. PITCHER MIX — empirical blending rule (grid search April 11 2026, n=628)
# =============================================================================

mix_lhh        <- blended_mix(wr_career, GAME_DATE, batter_hand = "L")
mix_rhh        <- blended_mix(wr_career, GAME_DATE, batter_hand = "R")
pitcher_sample <- nrow(wr_career |>
  filter(as.Date(game_date) < as.Date(GAME_DATE),
         !pitch_name %in% c("", "Unknown", NA)))

cat(sprintf("WR mix: LHH n_curr=%d w=%.1f | RHH n_curr=%d w=%.1f\n",
    mix_lhh$n_curr, mix_lhh$w_curr, mix_rhh$n_curr, mix_rhh$w_curr))

# =============================================================================
# 3. ASSUMPTIONS
# =============================================================================

assumptions_df <- tibble(
  assumption_name   = c("slider_to_rhh_pct","fourzeam_to_lhh_pct",
                        "splitfinger_to_lhh_pct","fastball_velo"),
  expected_value    = c(0.351, 0.488, 0.157, 93.1),
  tolerance         = c(0.12,  0.12,  0.12,  1.5),
  importance_weight = c(0.90,  0.70,  0.60,  0.50),
  affected_hand     = c("R",   "L",   "L",   "all"),
  impact_direction  = c("widen","widen","widen","widen"),
  default_shift     = c(0, 0, 0, 0),
  ci_widen_factor   = c(1.20, 1.10, 1.10, 1.05),
  description       = c(
    "WR Slider to RHH near career 35.1%",
    "WR 4-Seam to LHH near career 48.8%",
    "WR Split-Finger to LHH near career 15.7%",
    "WR fastball velo stable near 93.1 mph"
  )
)

# =============================================================================
# 4. RUN PREVIEW
# =============================================================================

wr_preview <- build_preview_v32(
  game_date         = GAME_DATE,
  team              = "TOR",
  opponent          = "MIN",
  pitcher_name      = "Woods Richardson, Simeon",
  pitcher_id        = WR_ID,
  pitcher_hand      = "R",
  pitcher_sample    = pitcher_sample,
  batter_pool       = c(
    "Barger, Addison","Clement, Ernie","Giménez, Andrés",
    "Guerrero Jr., Vladimir","Heineman, Tyler","Kirk, Alejandro",
    "Lukes, Nathan","Schneider, Davis","Springer, George",
    "Straw, Myles","Sánchez, Jesús","Varsho, Daulton"
  ),
  pitcher_mix_lhh   = mix_lhh$mix,
  pitcher_mix_rhh   = mix_rhh$mix,
  hitter_history_df = hitter_history_df,
  assumptions_df    = assumptions_df,
  pitcher_archetype = "RHP_mid_rotation",
  memory_dir        = "model_memory",
  output_dir        = "outputs/preview_v32"
)
