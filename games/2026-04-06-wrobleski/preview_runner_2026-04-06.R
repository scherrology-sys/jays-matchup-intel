# =============================================================================
# Jays Matchup Intel
# Game Preview Runner · Justin Wrobleski
# LAD @ TOR, April 6, 2026 · 7:07 PM ET · Rogers Centre
# =============================================================================
# source("jays_matchup_intel_self_learning.R")
# =============================================================================

library(tidyverse)

# =============================================================================
# 1. LOAD DATA
# =============================================================================

wrob_raw  <- safe_read_csv("savant_data__14_.csv")   # Wrobleski career all years
bvp_raw   <- safe_read_csv("savant_data__15_.csv")   # Jays BvP vs Wrobleski (Aug 2025, 3 PA)
jays_raw  <- safe_read_csv("blue-jays-hitters-25-26-filtered.csv")
sanchez_raw <- safe_read_csv("sanchez-24-26.csv")

WROB_ID <- 680736L

cat(sprintf("Wrobleski: %d pitches, %d seasons\n",
            nrow(wrob_raw), n_distinct(wrob_raw$game_year)))
cat(sprintf("BvP vs Jays: %d pitches, %d PA (Aug 10 2025 only)\n",
            nrow(bvp_raw),
            nrow(filter(bvp_raw, events %in% PA_EVENTS))))

# =============================================================================
# 2. HITTER HISTORY
# =============================================================================

hitter_history_df <- bind_rows(
  jays_raw    |> filter(game_year == 2025),
  sanchez_raw |> filter(game_year == 2025)
)

# =============================================================================
# 3. PITCH TYPE MAPPING
# =============================================================================

ptm <- build_pitch_type_mapping(
  changeup_names = c("Changeup"),
  sweeper_names  = c("Sweeper")
)

# =============================================================================
# 4. ASSUMPTIONS
# =============================================================================
# Wrobleski's defining characteristic: Changeup to LHH at 0.3% career.
# This is not a deviation, it is the established pattern.
# The key questions are Slider command and fastball velocity.

assumptions_df <- tibble(
  assumption_name   = c(
    "slider_command_vs_lhh",
    "fastball_velo_drop",
    "sinker_leads_to_slider_sequence"
  ),
  expected_value    = c(
    0.339,   # Slider usage to LHH at career weighted rate
    95.7,    # Career weighted fastball velo (2026 debut was 94.0)
    0.479    # Sinker -> Slider transition rate vs LHH (career)
  ),
  tolerance         = c(0.06, 1.5, 0.10),
  importance_weight = c(1.00, 0.80, 0.70),
  affected_hand     = c("L",  "all", "L"),
  impact_direction  = c("widen","widen","widen"),
  default_shift     = c(0.000, 0.000, 0.000),
  ci_widen_factor   = c(1.30,  1.20,  1.15),
  description       = c(
    "Slider usage to LHH holds near career rate of 33.9%",
    "Fastball velo holds near career avg 95.7 mph (2026 debut: 94.0)",
    "Sinker -> Slider sequencing vs LHH holds at ~48% career rate"
  )
)

# =============================================================================
# 5. RUN PREVIEW
# =============================================================================

wrob_preview <- build_preview(
  game_date        = "2026-04-06",
  team             = "TOR",
  opponent         = "LAD",
  pitcher_name     = "Justin Wrobleski",
  pitcher_id       = WROB_ID,
  pitcher_hand     = "L",
  pitcher_raw_df   = wrob_raw,
  batter_pool      = c(
    "Barger, Addison", "Clement, Ernie", "Giménez, Andrés",
    "Guerrero Jr., Vladimir", "Heineman, Tyler", "Kirk, Alejandro",
    "Lukes, Nathan", "Okamoto, Kazuma", "Schneider, Davis",
    "Springer, George", "Straw, Myles", "Sánchez, Jesús", "Varsho, Daulton"
  ),
  hitter_history_df        = hitter_history_df,
  assumptions_df           = assumptions_df,
  pitch_type_mapping       = ptm,
  hitter_pitch_exposure_df = NULL,
  pitcher_archetype        = "LHP_starter_lad",
  half_life_days           = 180,
  memory_dir               = "model_memory",
  output_dir               = "outputs/preview"
)

# =============================================================================
# 6. INSPECT
# =============================================================================

cat("\n=== TOP EXPECTATIONS ===\n")
wrob_preview$preview_df |>
  arrange(desc(exp_wOBA)) |>
  select(player_name, hand, exp_wOBA, CI_lo_90, CI_hi_90, prior_delta, fragility_score) |>
  mutate(across(where(is.numeric), \(x) round(x, 3))) |>
  print(n = Inf)

cat("\n=== SAMPLE WEIGHTS ===\n")
wrob_preview$weighted_sample$raw_by_year |> print()
cat(sprintf("Effective sample: %d  Confidence: %s\n",
            wrob_preview$weighted_sample$effective_sample,
            wrob_preview$preview_meta$confidence))

cat("\n=== ASSUMPTIONS (reliability at 1.0 for new archetype) ===\n")
wrob_preview$preview_assumptions |>
  select(assumption_name, expected_value, tolerance, reliability_weight) |>
  print()

# =============================================================================
# 7. PITCH SEQUENCING (preview version)
# =============================================================================

compute_pitch_transitions <- function(df, hand_filter=NULL, min_n=10) {
  df <- df |> filter(!pitch_name %in% c("Unknown","")) |>
    arrange(game_date, at_bat_number, pitch_number)
  if (!is.null(hand_filter)) df <- df |> filter(stand == hand_filter)
  df |>
    group_by(game_date, at_bat_number) |>
    mutate(next_pitch = lead(pitch_name)) |>
    ungroup() |>
    filter(!is.na(next_pitch)) |>
    group_by(pitch_name, next_pitch) |>
    summarise(n=n(), .groups="drop") |>
    group_by(pitch_name) |>
    mutate(pct=round(n/sum(n)*100,1)) |>
    ungroup() |>
    filter(n >= min_n)
}

compute_count_state_mix <- function(df, hand_filter=NULL, min_n=10) {
  df <- df |> filter(!pitch_name %in% c("Unknown","")) |>
    mutate(count_state=paste0(balls,"-",strikes))
  if (!is.null(hand_filter)) df <- df |> filter(stand == hand_filter)
  df |>
    group_by(count_state, pitch_name) |>
    summarise(n=n(), .groups="drop") |>
    group_by(count_state) |>
    mutate(pct=round(n/sum(n)*100,1)) |>
    ungroup() |>
    filter(n >= min_n) |>
    arrange(count_state, desc(pct))
}

cat("\n=== PITCH SEQUENCING: vs LHH ===\n")
tr_lhh <- compute_pitch_transitions(wrob_raw, "L")
print(tidyr::pivot_wider(tr_lhh |> select(pitch_name, next_pitch, pct),
                          names_from=next_pitch, values_from=pct, values_fill=0), n=Inf)

cat("\n=== KEY COUNT STATES vs LHH (top 3 per count) ===\n")
compute_count_state_mix(wrob_raw, "L") |>
  group_by(count_state) |> slice_head(n=3) |>
  filter(count_state %in% c("0-0","1-0","0-1","1-1","0-2","3-2")) |>
  print(n=Inf)

cat(sprintf("\nKey sequencing note: Sinker → Slider transition vs LHH: %.1f%% of Sinker pitches\n",
            tr_lhh |> filter(pitch_name=="Sinker", next_pitch=="Slider") |>
              pull(pct) |> (function(x) if(length(x)==0) 0 else x)()))

# Save sequencing
safe_dir_create("outputs/sequencing")
write_csv(tr_lhh, sprintf("outputs/sequencing/transitions_lhh_%d.csv", WROB_ID))
compute_pitch_transitions(wrob_raw, "R") |>
  write_csv(sprintf("outputs/sequencing/transitions_rhh_%d.csv", WROB_ID))

# =============================================================================
# END
# =============================================================================
# Key findings:
#   LHH: Slider (33.9%) + Sinker (33.0%) = 67%. Changeup 0.3%.
#   RHH: 4-Seam (36.0%) + Slider (23.2%) + Cutter (14.4%)
#   Fastball velo: 95.7 career, 94.0 in 2026 debut (-1.7 mph)
#   Slider xwOBA: .253 (best pitch)
#   Sinker → Slider at ~48% of transitions vs LHH
#   Heineman: +.055 vs baseline (second highest)
#   Varsho: -.040 vs baseline (near bottom)
#   Springer: .422 leads for 4th consecutive night
# =============================================================================
