# =============================================================================
# Jays Matchup Intel
# April 10, 2026 · MIN @ TOR · Rogers Centre · Retro
# =============================================================================

source("jays_matchup_intel_v3_2.R")
source("code/jays_matchup_helpers.R")

# =============================================================================
# 1. LOAD DATA
# =============================================================================

game_df    <- safe_read_csv("savant_data__38_.csv")   # Jays batting Apr 10
wr_career  <- safe_read_csv("savant_data__wr_career_.csv")

WR_ID      <- 680573L
GAME_DATE  <- "2026-04-10"

game_wr    <- game_df |> filter(pitcher == WR_ID)

# =============================================================================
# 2. ACTUAL ASSUMPTION VALUES FROM GAME DATA
# =============================================================================

lhh <- game_wr |> filter(stand == "L")
rhh <- game_wr |> filter(stand == "R")
ff  <- game_wr |> filter(pitch_name == "4-Seam Fastball")

actual_assumption_values_df <- tibble(
  assumption_name = c("slider_to_rhh_pct","fourzeam_to_lhh_pct",
                      "splitfinger_to_lhh_pct","fastball_velo"),
  actual_value    = c(
    round(mean(rhh$pitch_name == "Slider",         na.rm = TRUE), 3),
    round(mean(lhh$pitch_name == "4-Seam Fastball",na.rm = TRUE), 3),
    round(mean(lhh$pitch_name == "Split-Finger",   na.rm = TRUE), 3),
    round(mean(ff$release_speed,                   na.rm = TRUE), 1)
  )
)

cat("Actual assumption values:\n")
print(actual_assumption_values_df)

# =============================================================================
# 3. LOAD PREVIEW OUTPUTS (produced by preview runner)
# =============================================================================

preview_expectations_df <- safe_read_csv(
  "outputs/preview_v32/preview_expectations_v32_2026-04-10_680573.csv")
preview_assumptions_df  <- safe_read_csv(
  "outputs/preview_v32/preview_assumptions_v32_2026-04-10_680573.csv")
hitter_assumption_map   <- safe_read_csv(
  "outputs/preview_v32/preview_hitter_map_v32_2026-04-10_680573.csv")

# =============================================================================
# 4. RUN RETRO
# =============================================================================

wr_retro <- build_retro_v32(
  game_date                   = GAME_DATE,
  team                        = "TOR",
  opponent                    = "MIN",
  pitcher_name                = "Woods Richardson, Simeon",
  pitcher_id                  = WR_ID,
  pitcher_archetype           = "RHP_mid_rotation",
  pitch_by_pitch_game_df      = game_wr,
  preview_expectations_df     = preview_expectations_df,
  preview_assumptions_df      = preview_assumptions_df,
  hitter_assumption_map_df    = hitter_assumption_map,
  actual_assumption_values_df = actual_assumption_values_df,
  pitcher_baseline_df         = wr_career |>
    filter(as.Date(game_date) < as.Date(GAME_DATE)),
  memory_dir                  = "model_memory",
  output_dir                  = "outputs/retro_v32"
)

# Result: TOR 10, MIN 4
# WR: 4 IP 68P knocked out in 4th
# Slider to RHH failed +13.7pp — assumption miss category
