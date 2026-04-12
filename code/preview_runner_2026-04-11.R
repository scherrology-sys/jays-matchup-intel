# =============================================================================
# Jays Matchup Intel
# April 11, 2026 · MIN @ TOR · Rogers Centre
# =============================================================================

source("jays_matchup_intel_v3_2.R")
source("code/jays_matchup_helpers.R")

# =============================================================================
# 1. LOAD DATA
# =============================================================================

jr_career         <- safe_read_csv("savant_data__40_.csv")
h24               <- safe_read_csv("savant_data__19_.csv")
h25               <- safe_read_csv("savant_data__18_.csv")

JR_ID             <- 657746L
GAME_DATE         <- "2026-04-11"

hitter_history_df <- bind_rows(h24, h25)

# =============================================================================
# 2. PITCHER MIX — empirical blending rule
# Ryan 2026: 3 starts, ~125 LHH, ~138 RHH pitches -> w=0.7
# =============================================================================

mix_lhh        <- blended_mix(jr_career, GAME_DATE, batter_hand = "L")
mix_rhh        <- blended_mix(jr_career, GAME_DATE, batter_hand = "R")
pitcher_sample <- nrow(jr_career |>
  filter(as.Date(game_date) < as.Date(GAME_DATE),
         !pitch_name %in% c("", "Unknown", NA)))

cat(sprintf("Ryan mix: LHH n_curr=%d w=%.1f | RHH n_curr=%d w=%.1f\n",
    mix_lhh$n_curr, mix_lhh$w_curr, mix_rhh$n_curr, mix_rhh$w_curr))

# =============================================================================
# 3. ASSUMPTIONS
# =============================================================================

assumptions_df <- tibble(
  assumption_name   = c("fourzeam_to_lhh_pct","kc_to_lhh_pct",
                        "sinker_to_rhh_pct","sweeper_to_rhh_pct","fastball_velo"),
  expected_value    = c(0.525, 0.099, 0.149, 0.143, 92.7),
  tolerance         = c(0.12,  0.12,  0.12,  0.12,  1.5),
  importance_weight = c(0.70,  0.80,  0.90,  0.70,  0.50),
  affected_hand     = c("L",   "L",   "R",   "R",   "all"),
  impact_direction  = c("widen","widen","widen","widen","widen"),
  default_shift     = c(0, 0, 0, 0, 0),
  ci_widen_factor   = c(1.10, 1.20, 1.20, 1.10, 1.05),
  description       = c(
    "Ryan 4-Seam to LHH near blended 52.5%",
    "Ryan KC to LHH near blended 9.9% (up from career 5%)",
    "Ryan Sinker to RHH near blended 14.9%",
    "Ryan Sweeper to RHH near blended 14.3%",
    "Ryan fastball velo stable near 92.7 mph (2026)"
  )
)

# =============================================================================
# 4. RUN PREVIEW
# =============================================================================

ryan_preview <- build_preview_v32(
  game_date         = GAME_DATE,
  team              = "TOR",
  opponent          = "MIN",
  pitcher_name      = "Ryan, Joe",
  pitcher_id        = JR_ID,
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
