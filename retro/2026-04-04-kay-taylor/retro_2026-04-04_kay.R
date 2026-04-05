# =============================================================================
# Jays Matchup Intel
# Game Retro Runner
# TOR @ CWS, April 4, 2026
# Bulk: A. Kay (LHP, inn 2-6) · Opener: G. Taylor (RHP, inn 1, not scored)
# @scherrology | Arm Chair Analyst
#
# Key findings going in:
#   Assumption 1: Changeup to LHH at 0%        → HELD (actual: 0.000)
#   Assumption 2: Sweeper usage near 17%        → FAILED (actual: 0.263)
#   Execution:    All flags clear
#   Result:       CWS 6, TOR 3
# =============================================================================
# Source the self-learning system before running this script:
#   source("jays_matchup_intel_self_learning.R")
# =============================================================================

library(tidyverse)

# =============================================================================
# 1. LOAD DATA
# =============================================================================

# Game pitch file: Jays batting 4-4-26 (from Savant)
game_df  <- safe_read_csv("savant_data__17_.csv")

# Kay's 2026 file for movement baseline comparison
kay_raw      <- safe_read_csv("savant_data__13_.csv")
kay_baseline <- kay_raw |> filter(game_year == 2026)

# Load preview outputs produced by build_preview() on 4-4-26
preview_df   <- safe_read_csv("outputs/preview/expectations_2026-04-04_641743.csv")
asmp_df      <- safe_read_csv("outputs/preview/assumptions_2026-04-04_641743.csv")
map_df       <- safe_read_csv("outputs/preview/hitter_map_2026-04-04_641743.csv")

# =============================================================================
# 2. PITCH TYPE MAPPING
# =============================================================================

ptm <- build_pitch_type_mapping(
  sweeper_names   = c("Sweeper"),
  changeup_names  = c("Changeup"),
  fastball_names  = c("4-Seam Fastball", "Fastball", "Sinker")
)

# =============================================================================
# 3. FILTER TO KAY PAs ONLY
# Kay pitched innings 2-6 (pitcher ID 641743)
# Taylor opened inning 1 — excluded from scoring by design
# =============================================================================

KAY_ID  <- 641743L
kay_game <- game_df |> filter(pitcher == KAY_ID)

cat(sprintf(
  "\nKay pitch data: %d pitches across innings %s\n",
  nrow(kay_game),
  paste(sort(unique(kay_game$inning)), collapse = ", ")
))

# =============================================================================
# 4. ASSUMPTION ACTUALS
# Auto-derived from pitch file via pitch_type_mapping.
# Changeup-to-LHH% and Sweeper usage% computed directly.
# fastball_command_risk is qualitative, scored via execution flags only.
# =============================================================================

actual_values <- compute_assumption_actuals_from_pitchfile(
  pitch_by_pitch_game_df = kay_game,
  pitch_type_mapping     = ptm
)

# Qualitative flag not derivable from pitch type mapping:
# Add fastball_command_risk as not scored (NA)
actual_values <- bind_rows(
  actual_values,
  tibble(assumption_name = "fastball_command_risk",
         actual_value    = NA_real_,
         derived         = FALSE)
)

cat("\nAssumption actuals (auto-derived where possible):\n")
print(actual_values)

# =============================================================================
# 5. RUN RETRO
# =============================================================================

retro_out <- build_retro(
  game_date    = "2026-04-04",
  team         = "TOR",
  opponent     = "CWS",
  pitcher_name = "Anthony Kay",
  pitcher_id   = KAY_ID,
  pitcher_archetype = "LHP_bulk_one_start",
  pitch_by_pitch_game_df      = kay_game,
  preview_expectations_df     = preview_df,
  preview_assumptions_df      = asmp_df,
  hitter_assumption_map_df    = map_df,
  actual_assumption_values_df = actual_values,
  pitch_type_mapping          = ptm,
  pitcher_baseline_df         = kay_baseline,
  memory_dir   = "model_memory",
  output_dir   = "outputs/retro"
)

# =============================================================================
# 6. INSPECT RETRO OUTPUTS
# =============================================================================

cat("\n=== SCORECARD ===\n")
retro_out$hitter_audit |>
  filter(!is.na(exp_wOBA)) |>
  select(hitter, hand, PA, exp_wOBA, actual_wOBA, residual,
         audit_flag, miss_type, affected_assumptions) |>
  arrange(desc(exp_wOBA)) |>
  print(n = Inf)

cat("\n=== ASSUMPTION AUDIT ===\n")
retro_out$assumption_audit |>
  select(assumption_name, expected_value, actual_value,
         tolerance, audit_result, assumption_error) |>
  print(n = Inf)

cat("\n=== EXECUTION FLAGS ===\n")
retro_out$execution_flags |> print(n = Inf)

cat("\n=== MODEL ERROR ===\n")
retro_out$model_error |> print()

cat("\n=== LINEUP AUDIT ===\n")
retro_out$lineup_audit |> print()

cat("\n=== LEARNED ===\n")
retro_out$learned |> print(n = Inf)

# =============================================================================
# 7. VERIFY MEMORY WAS WRITTEN
# =============================================================================

cat("\n=== MEMORY CHECK ===\n")
mem_sum <- summarise_model_memory("model_memory")
cat("Hitter memory:\n"); print(mem_sum$hitter_summary)
cat("\nAssumption memory:\n"); print(mem_sum$assumption_summary, n = Inf)

# =============================================================================
# END
# Result: CWS 6, TOR 3
# Kay: 5 IP, all pitches on profile, Sweeper 26% (assumption failed)
# Changeup to LHH: 0% (assumption held)
# Execution flags: all clear
# =============================================================================
