# =============================================================================
# Jays Matchup Intel V3.2
# Retro Runner · TOR @ ARI · April 19, 2026
# Pitcher: Ryne Nelson (RHP) · ID: 669194
# Result: TOR 10, ARI 4 · Observation 19
# @scherrology | Arm Chair Analyst
# =============================================================================
# source("analysis/jays_matchup_intel_v3_2.R")
# =============================================================================
# KEY NOTE: Nelson was pulled after 30 pitches without recording the third out
# in the first inning. Only Lukes batted twice (n=1 scoreable). Model not
# meaningfully evaluated this game. All three assumptions held.
# =============================================================================
# NELSON OUTING:
#   0.1 IP · 30 pitches · 10 PA · 8H · 1BB · 1K · 8R
#   Pitch mix: FF 53.3% (16), SL 26.7% (8), CU 16.7% (5), FC 3.3% (1)
#   vs LHH (n=15): FF 46.7%, CU 33.3%, SL 20.0%
#   vs RHH (n=15): FF 60.0%, SL 33.3%, FC 6.7%
# =============================================================================
# ASSUMPTION ACTUALS — ALL HELD:
#   Changeup:       expected 0.0%   actual 0.0%   HELD
#   Slider vs RHH:  expected 32.9%  actual 33.3%  HELD (+0.4pp)
#   Walk rate:      expected 9.4%   actual 10.0%  HELD (+0.6pp)
# =============================================================================
# HITTER RESULTS (vs Nelson only):
#   Scoreable (>=2 PA): Lukes only
#     Lukes:   L  2PA  double + single  wOBA 1.079  exp .325  resid +0.754
#   Single PA (not scored):
#     Guerrero  R  single   (FF, 1-2)
#     Clement   R  double   (FF, 1-0)
#     Sanchez   L  single   (FF, 0-0)
#     Eloy      R  single   (SL, 0-1)
#     Gimenez   L  single   (FF, 2-2)
#     Okamoto   R  double   (FF, 1-1)
#     Straw     R  walk     (FF, 3-0)
#     Valenzuela L strikeout (SL, 1-2) — not on roster
# =============================================================================

library(tidyverse)

# =============================================================================
# 1. LOAD DATA
# =============================================================================

# savant_data__69_.csv · full game pitch file (all pitchers)
# savant_data__70_.csv · hitter game file (all batters)

full_game_df   <- safe_read_csv("savant_data__69_.csv")
hitter_game_df <- safe_read_csv("savant_data__70_.csv")

# Nelson pitches only (ID: 669194)
pitch_by_pitch_game_df <- hitter_game_df |> filter(pitcher == 669194L)

# Nelson 2026 baseline (from savant_data__68_.csv)
nelson_baseline_df <- safe_read_csv("savant_data__68_.csv") |>
  filter(game_year == 2026)

# =============================================================================
# 2. LOAD PREVIEW OUTPUTS
# =============================================================================

preview_expectations_df  <- safe_read_csv(
  "outputs/preview_v32/preview_expectations_v32_2026-04-19_669194.csv")
preview_assumptions_df   <- safe_read_csv(
  "outputs/preview_v32/preview_assumptions_v32_2026-04-19_669194.csv")
hitter_assumption_map_df <- safe_read_csv(
  "outputs/preview_v32/preview_hitter_map_v32_2026-04-19_669194.csv")

# =============================================================================
# 3. ACTUAL ASSUMPTION VALUES
# =============================================================================

PA_EVENTS <- c("single","double","triple","home_run","field_out","strikeout",
               "walk","hit_by_pitch","grounded_into_double_play","force_out",
               "sac_fly","fielders_choice","fielders_choice_out","double_play",
               "strikeout_double_play","sac_bunt","field_error")

nelson_game <- full_game_df |> filter(pitcher == 669194L)
valid_pts   <- nelson_game |> filter(pitch_type %in% c("FF","SL","CU","FC","CH"))
rhh_pitches <- valid_pts   |> filter(stand == "R")
pa_total    <- pitch_by_pitch_game_df |> filter(events %in% PA_EVENTS) |> nrow()

# Assumption actuals:
# 1. Changeup: 0 pitches / 30 total = 0.0% — HELD
ch_pct <- 0.000

# 2. Slider vs RHH: 5/15 = 33.3% — HELD (expected 32.9%, tol ±8pp)
sl_rhh_pct <- nrow(rhh_pitches |> filter(pitch_type == "SL")) / nrow(rhh_pitches)
# 0.333

# 3. Walk rate: 1/10 = 10.0% — HELD (expected 9.4%, tol ±8pp)
bb_rate <- (pitch_by_pitch_game_df |>
              filter(events %in% c("walk","intent_walk")) |> nrow()) / pa_total
# 0.100

actual_assumption_values_df <- tibble(
  assumption_name = c("changeup_usage", "slider_vs_rhh_pct", "walk_rate"),
  actual_value    = c(ch_pct,           sl_rhh_pct,           bb_rate)
  # 0.000, 0.333, 0.100
)

# =============================================================================
# 4. RUN RETRO
# NOTE: With only 1 scoreable hitter (Lukes, 2 PA), the model error metrics
# are not interpretable. build_retro_v32() will still run and log the single
# hitter-game to memory. The MAE of 0.754 and Spearman rho of NA are flagged
# as non-meaningful in the retro output and study log.
# =============================================================================

retro_out <- build_retro_v32(
  game_date                   = "2026-04-19",
  team                        = "TOR",
  opponent                    = "ARI",
  pitcher_name                = "Ryne Nelson",
  pitcher_id                  = 669194L,
  pitcher_archetype           = "RHP_starter_ari_4seam_slider",
  pitch_by_pitch_game_df      = pitch_by_pitch_game_df,
  preview_expectations_df     = preview_expectations_df,
  preview_assumptions_df      = preview_assumptions_df,
  hitter_assumption_map_df    = hitter_assumption_map_df,
  actual_assumption_values_df = actual_assumption_values_df,
  pitcher_baseline_df         = nelson_baseline_df,
  memory_dir                  = "model_memory",
  output_dir                  = "outputs/retro_v32"
)

# =============================================================================
# 5. REVIEW
# =============================================================================

retro_out$hitter_audit |>
  select(hitter, hand, PA, exp_wOBA, actual_wOBA, residual,
         abs_error, audit_flag, miss_type) |>
  print(n = Inf)

retro_out$assumption_audit |>
  select(assumption_name, expected_value, actual_value,
         tolerance, audit_result, assumption_error) |>
  print(n = Inf)

# n=1 warning — flag in output
cat("NOTE: n=1 scoreable hitter (Lukes only). MAE and rho are not",
    "interpretable at this sample size.\n")

retro_out$model_error |> print()

# =============================================================================
# 6. MEMORY NOTES
# =============================================================================
#
# Nelson profile: UNCHANGED. All three assumptions held exactly.
#   No new structural signal from this outing.
#   Changeup: still zero (5th consecutive start without one).
#   Slider vs RHH: 33.3% confirmed, assumption is stable.
#
# Eight hits in nine balls in play is a contact quality event.
#   The model correctly described the pitcher. The lineup mashed him anyway.
#   Pre-game pitch models do not capture per-contact quality variance.
#
# If Jays face Nelson again: use same preview inputs unchanged.
#
# RUNNING N: ~126 hitter-games after observation 19.
# This game contributes only 1 hitter-game to memory (Lukes).
#
# =============================================================================
# END · retro_runner_2026-04-19_nelson.R
# =============================================================================
