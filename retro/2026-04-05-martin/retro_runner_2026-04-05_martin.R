# =============================================================================
# Jays Matchup Intel
# Game Retro Runner · Davis Martin
# TOR @ CWS, April 5, 2026
# Martin: 6 IP, 85 pitches, innings 1-6
# Result: CWS 3, TOR 0
#
# Key findings:
#   Assumption 1: Changeup to LHH at 32%   → FAILED (actual: 6.4%)
#   Assumption 2: Cutter usage at ~6%       → FAILED (actual: 15.3%)
#   Execution:    All flags clear
#   New: Pitch sequencing analysis built in (first game with sequencing)
# =============================================================================
# source("jays_matchup_intel_self_learning.R")
# =============================================================================

library(tidyverse)

# =============================================================================
# 1. LOAD DATA
# =============================================================================

game_df    <- safe_read_csv("savant_data__24_.csv")   # Jays batting 4-5-26
martin_raw <- safe_read_csv("savant_data__19_.csv")   # Martin career (all years)

MARTIN_ID <- 663436L

martin_game <- game_df |> filter(pitcher == MARTIN_ID)

cat(sprintf(
  "Martin: %d pitches, innings %s\n",
  nrow(martin_game),
  paste(sort(unique(martin_game$inning)), collapse = ", ")
))

# =============================================================================
# 2. LOAD PREVIEW OUTPUTS
# =============================================================================

preview_df  <- safe_read_csv("outputs/preview/expectations_2026-04-05_663436.csv")
asmp_df     <- safe_read_csv("outputs/preview/assumptions_2026-04-05_663436.csv")
map_df      <- safe_read_csv("outputs/preview/hitter_map_2026-04-05_663436.csv")

# =============================================================================
# 3. PITCH TYPE MAPPING
# =============================================================================

ptm <- build_pitch_type_mapping(
  changeup_names = c("Changeup"),
  sweeper_names  = c("Sweeper")
)

# Martin-specific: add Cutter as a named derivable assumption
# The mapping below adds cutter_usage_pct to the auto-derivation
ptm_martin <- c(ptm, list(
  cutter_usage_pct = list(
    pitches         = c("Cutter"),
    hand_filter     = NULL,
    denominator     = "all",
    derive_exposure = FALSE
  )
))

# =============================================================================
# 4. ASSUMPTION ACTUALS (auto-derived from pitch file)
# =============================================================================

actual_values <- compute_assumption_actuals_from_pitchfile(
  pitch_by_pitch_game_df = martin_game,
  pitch_type_mapping     = ptm_martin
)

# fastball_command_risk is qualitative, not derivable
actual_values <- bind_rows(
  actual_values,
  tibble(assumption_name = "fastball_command_risk",
         actual_value    = NA_real_,
         derived         = FALSE)
)

cat("\nAssumption actuals:\n")
print(actual_values)

# =============================================================================
# 5. PITCH SEQUENCING ANALYSIS  [NEW]
# Build career transition matrix for Martin.
# Saved to outputs for use in the next preview.
# =============================================================================

compute_pitch_transitions <- function(
  pitcher_raw_df,
  hand_filter    = NULL,   # NULL = all, "L" = vs LHH only, "R" = vs RHH only
  min_n          = 10      # minimum transitions to report a cell
) {
  df <- pitcher_raw_df |>
    filter(!pitch_name %in% c("Unknown","")) |>
    arrange(game_date, at_bat_number, pitch_number)

  if (!is.null(hand_filter)) df <- df |> filter(stand == hand_filter)

  transitions <- df |>
    group_by(game_date, at_bat_number) |>
    mutate(
      next_pitch = lead(pitch_name),
      next_hand  = lead(stand)
    ) |>
    ungroup() |>
    filter(!is.na(next_pitch))

  mat <- transitions |>
    group_by(pitch_name, next_pitch) |>
    summarise(n = n(), .groups = "drop") |>
    group_by(pitch_name) |>
    mutate(pct = round(n / sum(n) * 100, 1)) |>
    ungroup() |>
    filter(n >= min_n)

  mat
}

compute_count_state_mix <- function(
  pitcher_raw_df,
  hand_filter = NULL,
  min_n       = 10
) {
  df <- pitcher_raw_df |>
    filter(!pitch_name %in% c("Unknown","")) |>
    mutate(count_state = paste0(balls, "-", strikes))

  if (!is.null(hand_filter)) df <- df |> filter(stand == hand_filter)

  df |>
    group_by(count_state, pitch_name) |>
    summarise(n = n(), .groups = "drop") |>
    group_by(count_state) |>
    mutate(pct = round(n / sum(n) * 100, 1)) |>
    ungroup() |>
    filter(n >= min_n) |>
    arrange(count_state, desc(pct))
}

cat("\n=== PITCH SEQUENCING: CAREER TRANSITION MATRIX ===\n")
cat("\nvs LHH:\n")
tr_lhh <- compute_pitch_transitions(martin_raw, hand_filter = "L")
print(tidyr::pivot_wider(tr_lhh |> select(pitch_name, next_pitch, pct),
                          names_from = next_pitch, values_from = pct, values_fill = 0),
      n = Inf)

cat("\nvs RHH:\n")
tr_rhh <- compute_pitch_transitions(martin_raw, hand_filter = "R")
print(tidyr::pivot_wider(tr_rhh |> select(pitch_name, next_pitch, pct),
                          names_from = next_pitch, values_from = pct, values_fill = 0),
      n = Inf)

cat("\n=== COUNT-STATE PITCH SELECTION ===\n")
cat("\nvs LHH (top 3 per count):\n")
cs_lhh <- compute_count_state_mix(martin_raw, "L")
cs_lhh |>
  group_by(count_state) |>
  slice_head(n = 3) |>
  print(n = Inf)

cat("\nvs RHH (top 3 per count):\n")
cs_rhh <- compute_count_state_mix(martin_raw, "R")
cs_rhh |>
  group_by(count_state) |>
  slice_head(n = 3) |>
  print(n = Inf)

# Key sequencing insight
cat("\n=== CUTTER AS SETUP PITCH (career, vs LHH) ===\n")
tr_lhh |> filter(pitch_name == "Cutter") |> arrange(desc(pct)) |> print()
cat("Interpretation: Cutter -> Changeup at 45% career rate. Tonight: Cutter at 15%, Changeup at 6%. Setup without payoff.\n")

# Save sequencing outputs
safe_dir_create("outputs/sequencing")
write_csv(tr_lhh, "outputs/sequencing/transitions_lhh_663436.csv")
write_csv(tr_rhh, "outputs/sequencing/transitions_rhh_663436.csv")
write_csv(cs_lhh, "outputs/sequencing/count_state_lhh_663436.csv")
write_csv(cs_rhh, "outputs/sequencing/count_state_rhh_663436.csv")
cat("\nSequencing outputs saved to outputs/sequencing/\n")

# =============================================================================
# 6. RUN RETRO
# =============================================================================

retro_out <- build_retro(
  game_date    = "2026-04-05",
  team         = "TOR",
  opponent     = "CWS",
  pitcher_name = "Davis Martin",
  pitcher_id   = MARTIN_ID,
  pitcher_archetype = "RHP_bulk_cws_2026",
  pitch_by_pitch_game_df      = martin_game,
  preview_expectations_df     = preview_df,
  preview_assumptions_df      = asmp_df,
  hitter_assumption_map_df    = map_df,
  actual_assumption_values_df = actual_values,
  pitch_type_mapping          = ptm_martin,
  pitcher_baseline_df         = martin_raw |> filter(game_year == 2026),
  memory_dir   = "model_memory",
  output_dir   = "outputs/retro"
)

# =============================================================================
# 7. INSPECT OUTPUTS
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
retro_out$execution_flags |>
  filter(flag_value | !is.na(metric_value)) |>
  print(n = Inf)

cat("\n=== MODEL ERROR ===\n")
retro_out$model_error |> print()

cat("\n=== LINEUP AUDIT ===\n")
retro_out$lineup_audit |> print()

cat("\n=== WHAT THE ANALYSIS LEARNED ===\n")
retro_out$learned |> print(n = Inf)

# =============================================================================
# 8. MEMORY SUMMARY (cumulative)
# =============================================================================

cat("\n=== MEMORY SUMMARY (all games to date) ===\n")
mem_sum <- summarise_model_memory("model_memory")
cat("Hitter-level:\n"); print(mem_sum$hitter_summary)
cat("\nAssumption-level:\n"); print(mem_sum$assumption_summary, n = Inf)

# =============================================================================
# END
# Result:       CWS 3, TOR 0
# Martin:       6 IP · 85 pitches · 0 R · all flags clear
# Assumption 1: changeup_to_lhh_pct FAILED (32% → 6%)
# Assumption 2: cutter_usage_pct FAILED (6% → 15%)
# Sequencing:   Cutter setup pitch for Changeup (45% career) - payoff absent tonight
# Next Martin:  Sinker/Cutter/Curveball LHH approach, Changeup secondary or absent
# =============================================================================
