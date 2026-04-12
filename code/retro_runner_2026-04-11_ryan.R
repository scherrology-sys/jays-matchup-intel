# =============================================================================
# Jays Matchup Intel
# April 11, 2026 · MIN @ TOR · Rogers Centre · Retro
# =============================================================================

source("jays_matchup_intel_v3_2.R")
source("code/jays_matchup_helpers.R")

# =============================================================================
# 1. LOAD DATA
# =============================================================================

game_df   <- safe_read_csv("savant_data__42_.csv")
jr_career <- safe_read_csv("savant_data__40_.csv")

JR_ID     <- 657746L
GAME_DATE <- "2026-04-11"

game_ryan <- game_df |> filter(pitcher == JR_ID)

# =============================================================================
# 2. ACTUAL ASSUMPTION VALUES FROM GAME DATA
# =============================================================================

lhh <- game_ryan |> filter(stand == "L")
rhh <- game_ryan |> filter(stand == "R")
ff  <- game_ryan |> filter(pitch_name == "4-Seam Fastball")

actual_assumption_values_df <- tibble(
  assumption_name = c("fourzeam_to_lhh_pct","kc_to_lhh_pct",
                      "sinker_to_rhh_pct","sweeper_to_rhh_pct","fastball_velo"),
  actual_value    = c(
    round(mean(lhh$pitch_name == "4-Seam Fastball", na.rm = TRUE), 3),
    round(mean(lhh$pitch_name == "Knuckle Curve",   na.rm = TRUE), 3),
    round(mean(rhh$pitch_name == "Sinker",           na.rm = TRUE), 3),
    round(mean(rhh$pitch_name == "Sweeper",          na.rm = TRUE), 3),
    round(mean(ff$release_speed,                     na.rm = TRUE), 1)
  )
)

cat("Actual assumption values:\n")
print(actual_assumption_values_df)

# =============================================================================
# 3. LOAD PREVIEW OUTPUTS
# =============================================================================

preview_expectations_df <- safe_read_csv(
  "outputs/preview_v32/preview_expectations_v32_2026-04-11_657746.csv")
preview_assumptions_df  <- safe_read_csv(
  "outputs/preview_v32/preview_assumptions_v32_2026-04-11_657746.csv")
hitter_assumption_map   <- safe_read_csv(
  "outputs/preview_v32/preview_hitter_map_v32_2026-04-11_657746.csv")

# =============================================================================
# 4. RUN RETRO
# =============================================================================

ryan_retro <- build_retro_v32(
  game_date                   = GAME_DATE,
  team                        = "TOR",
  opponent                    = "MIN",
  pitcher_name                = "Ryan, Joe",
  pitcher_id                  = JR_ID,
  pitcher_archetype           = "RHP_mid_rotation",
  pitch_by_pitch_game_df      = game_ryan,
  preview_expectations_df     = preview_expectations_df,
  preview_assumptions_df      = preview_assumptions_df,
  hitter_assumption_map_df    = hitter_assumption_map,
  actual_assumption_values_df = actual_assumption_values_df,
  pitcher_baseline_df         = jr_career |>
    filter(as.Date(game_date) < as.Date(GAME_DATE)),
  memory_dir                  = "model_memory",
  output_dir                  = "outputs/retro_v32"
)

# Result: MIN 7, TOR 4
# Ryan: 7 IP effective
# Sinker to RHH failed +18.4pp — assumption miss
# Varsho Suppressed, actual .700 — variance miss
# Springer injured 2nd PA, both PAs complete
