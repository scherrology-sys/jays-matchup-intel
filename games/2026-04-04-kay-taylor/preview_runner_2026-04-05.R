# =============================================================================
# Jays Matchup Intel
# Game Preview Runner
# TOR @ CWS, April 5, 2026
# Opener: G. Taylor (RHP) · Bulk: TBD
# @scherrology | Arm Chair Analyst
# =============================================================================
# Source the self-learning system before running this script:
#   source("jays_matchup_intel_self_learning.R")
# =============================================================================

library(tidyverse)

# =============================================================================
# 1. LOAD DATA
# =============================================================================

# Pull today's bulk pitcher Statcast file from Baseball Savant
# Search > Player = [bulk pitcher] > Season Type = Regular > 2026
# Export as CSV, update path below
bulk_raw     <- safe_read_csv("savant_data_pitcher_2026-04-05.csv")
jays_raw     <- safe_read_csv("blue-jays-hitters-25-26.csv")
sanchez_raw  <- safe_read_csv("sanchez-24-26.csv")

# 2026 pitches only for sample count
pitcher_2026 <- bulk_raw |> filter(game_year == 2026)
pitcher_sample <- nrow(pitcher_2026)

# Hitter history: 2025 seasons
hitter_history_df <- bind_rows(
  jays_raw   |> filter(game_year == 2025),
  sanchez_raw |> filter(game_year == 2025)
)

# =============================================================================
# 2. PITCHER INPUTS
# Fill these from Statcast after reviewing the pitch mix
# =============================================================================

# Confirm pitcher ID from Savant URL
PITCHER_ID   <- NA_integer_   # e.g. 641743L for Kay
PITCHER_NAME <- "TBD"
PITCHER_HAND <- "R"           # "L" or "R"
PITCHER_ARCH <- "RHP_bulk_cws_2026"  # update when pitcher confirmed

# Pitch mix by handedness
# Derive from pitcher_2026 or enter manually after reviewing Statcast
pitcher_mix_lhh <- c(
  # "4-Seam Fastball" = 0.xxx,
  # "Slider"          = 0.xxx,
  # etc.
)

pitcher_mix_rhh <- c(
  # "4-Seam Fastball" = 0.xxx,
  # etc.
)

# =============================================================================
# 3. CURRENT ROSTER
# =============================================================================

batter_pool <- c(
  "Barger, Addison",
  "Clement, Ernie",
  "Giménez, Andrés",
  "Guerrero Jr., Vladimir",
  "Heineman, Tyler",
  "Kirk, Alejandro",
  "Lukes, Nathan",
  "Okamoto, Kazuma",
  "Schneider, Davis",
  "Springer, George",
  "Straw, Myles",
  "Sánchez, Jesús",
  "Varsho, Daulton"
)

# =============================================================================
# 4. PITCH TYPE MAPPING
# Update pitch names to match Statcast pitch_name values for this pitcher
# =============================================================================

ptm <- build_pitch_type_mapping(
  sweeper_names   = c("Sweeper"),
  changeup_names  = c("Changeup"),
  fastball_names  = c("4-Seam Fastball", "Fastball", "Sinker")
)

# =============================================================================
# 5. NAMED ASSUMPTIONS
# Review the pitch mix, then name 2-3 assumptions
# =============================================================================

assumptions_df <- tibble(
  assumption_name   = c(
    "assumption_1",    # e.g. "changeup_usage_vs_lhh"
    "assumption_2"     # e.g. "fastball_command_risk"
  ),
  expected_value    = c(NA_real_, NA_real_),   # numeric where possible
  tolerance         = c(0.05,     NA_real_),
  importance_weight = c(0.80,     0.90),
  affected_hand     = c("all",    "all"),       # "L", "R", or "all"
  impact_direction  = c("widen",  "widen"),     # "down", "up", or "widen"
  default_shift     = c(0.000,    0.000),
  ci_widen_factor   = c(1.20,     1.25),
  description       = c(
    "Description of assumption 1",
    "Description of assumption 2"
  )
)

# =============================================================================
# 6. RUN PREVIEW
# =============================================================================

game_preview <- build_preview(
  game_date    = "2026-04-05",
  team         = "TOR",
  opponent     = "CWS",
  pitcher_name = PITCHER_NAME,
  pitcher_id   = PITCHER_ID,
  pitcher_hand = PITCHER_HAND,
  pitcher_sample = pitcher_sample,
  batter_pool  = batter_pool,
  pitcher_mix_lhh = pitcher_mix_lhh,
  pitcher_mix_rhh = pitcher_mix_rhh,
  hitter_history_df = hitter_history_df,
  assumptions_df    = assumptions_df,
  pitch_type_mapping = ptm,
  hitter_pitch_exposure_df = NULL,   # auto-derived
  pitcher_archetype = PITCHER_ARCH,
  memory_dir  = "model_memory",
  output_dir  = "outputs/preview"
)

# =============================================================================
# 7. INSPECT OUTPUTS
# =============================================================================

# Top 5 expectations
game_preview$preview_df |>
  arrange(desc(exp_wOBA)) |>
  select(player_name, hand, exp_wOBA, CI_lo_90, CI_hi_90, CI_width,
         fragility_score, prior_delta, exp_runs_per_pa) |>
  slice_head(n = 5) |>
  mutate(across(where(is.numeric), \(x) round(x, 3))) |>
  print(n = 5)

# Assumption reliability going in (will be 1.0 for new archetypes)
game_preview$preview_assumptions |>
  select(assumption_name, expected_value, tolerance, reliability_weight) |>
  print()

# Lineup summary
game_preview$preview_meta |>
  select(confidence, pitcher_sample, historical_mae, memory_game_count,
         mean_exp_wOBA_top9, lineup_edge_vs_league, exp_runs_top9) |>
  print()

# =============================================================================
# END
# =============================================================================
