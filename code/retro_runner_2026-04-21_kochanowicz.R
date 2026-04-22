# =============================================================================
# Jays Matchup Intel V3.2
# Retro Runner · TOR @ LAA · April 21, 2026
# Pitcher: Jack Kochanowicz (RHP) · ID: 686799
# Result: TOR 4, LAA 1 · Observation 21
# @scherrology | Arm Chair Analyst
# =============================================================================
# source("analysis/jays_matchup_intel_v3_2.R")
# =============================================================================
# KEY FINDINGS:
#   Champion beats naive: MAE 0.179 vs 0.192. Second consecutive win.
#   Rho +0.595: first positive ranking signal of the season.
#   All three assumptions held: cleanest assumption game of the season.
#   Varsho Edge (.382) validated: two singles, actual wOBA .888.
#   Gimenez Suppressed (.216) held: two field outs, actual wOBA .000.
# =============================================================================
# KOCHANOWICZ OUTING:
#   6 IP · 88 pitches · 23 PA · 1K · 2BB · 5H · 0HR · low-walk version
#   Sinker:   39.8% (35) · whiff% 10.0%
#   Changeup: 21.6% (19) · whiff% 20.0%
#   4-Seam:   20.5% (18) · whiff% 11.1%
#   Slider:   18.2% (16) · whiff% 20.0%
#   vs LHH (n=45): SI 31.1%, FF 26.7%, CH 26.7%, SL 15.6%
#   vs RHH (n=43): SI 48.8%, SL 20.9%, CH 16.3%, FF 14.0%
# =============================================================================
# ASSUMPTION ACTUALS — ALL HELD:
#   Changeup vs LHH:  expected 32.3%  actual 26.7%  HELD (-5.6pp, tol ±7pp)
#   Walk rate:        expected 15.2%  actual  8.7%  HELD (-6.5pp, tol ±8pp)
#   Sinker vs RHH:    expected 43.2%  actual 48.8%  HELD (+5.6pp, tol ±8pp)
# =============================================================================
# HITTER RESULTS (scoreable, PA >= 2 vs Kochanowicz):
#
# Hitter              Hand  PA  H  K  BB  actual_wOBA  exp_wOBA  resid   Tier
# Varsho              L     2   2  0   0    0.888        0.382    +0.506  Edge
# Sánchez             L     3   1  0   1    0.528        0.320    +0.208  Neutral
# Lukes               L     3   0  0   0    0.363*       0.297    +0.066  Neutral  *HBP
# Clement             R     3   1  1   0    0.296        0.279    +0.017  Supp
# Jiménez, Eloy       R     3   0  0   1    0.348**      0.320    +0.028  Neutral  **sac fly
# Guerrero Jr.        R     3   1  0   0    0.296        0.370    -0.074  Edge
# Okamoto             R     2   0  0   0    0.000        0.320    -0.320  Neutral
# Giménez, A.         L     2   0  0   0    0.000        0.216    -0.216  Supp
#
# Not scored (< 2 PA vs Kochanowicz): Straw, Barger, Heineman, Schneider
# Valenzuela (2 PA, field out x2): not on 2026 roster, excluded
#
# n=8  MAE=0.179  Bias=+0.027  Naive=0.192  Spearman rho=+0.595
# Champion beats naive: TRUE
# =============================================================================

library(tidyverse)

# =============================================================================
# 1. LOAD DATA
# =============================================================================

full_game_df   <- safe_read_csv("savant_data__26_.csv")
hitter_game_df <- safe_read_csv("savant_data__27_.csv")

pitch_by_pitch_game_df <- hitter_game_df |> filter(pitcher == 686799L)

kochi_baseline_df <- safe_read_csv("savant_data__76_.csv") |>
  filter(game_year == 2026)

# =============================================================================
# 2. LOAD PREVIEW OUTPUTS
# =============================================================================

preview_expectations_df  <- safe_read_csv(
  "outputs/preview_v32/preview_expectations_v32_2026-04-21_686799.csv")
preview_assumptions_df   <- safe_read_csv(
  "outputs/preview_v32/preview_assumptions_v32_2026-04-21_686799.csv")
hitter_assumption_map_df <- safe_read_csv(
  "outputs/preview_v32/preview_hitter_map_v32_2026-04-21_686799.csv")

# =============================================================================
# 3. ACTUAL ASSUMPTION VALUES
# =============================================================================

PA_EVENTS <- c("single","double","triple","home_run","field_out","strikeout",
               "walk","hit_by_pitch","grounded_into_double_play","force_out",
               "sac_fly","fielders_choice","fielders_choice_out","double_play",
               "strikeout_double_play","sac_bunt","field_error")

kochi_game  <- full_game_df  |> filter(pitcher == 686799L)
valid_pts   <- kochi_game    |> filter(pitch_type %in% c("FF","SI","CH","SL"))
lhh_pitches <- valid_pts     |> filter(stand == "L")
rhh_pitches <- valid_pts     |> filter(stand == "R")
pa_total    <- pitch_by_pitch_game_df |> filter(events %in% PA_EVENTS) |> nrow()

# Changeup vs LHH: 12/45 = 26.7% — HELD (expected 32.3%, tol ±7pp)
ch_lhh_pct <- nrow(lhh_pitches |> filter(pitch_type == "CH")) / nrow(lhh_pitches)

# Walk rate: 2/23 = 8.7% — HELD (expected 15.2%, tol ±8pp)
bb_rate <- (pitch_by_pitch_game_df |>
              filter(events %in% c("walk","intent_walk")) |> nrow()) / pa_total

# Sinker vs RHH: 21/43 = 48.8% — HELD (expected 43.2%, tol ±8pp)
si_rhh_pct <- nrow(rhh_pitches |> filter(pitch_type == "SI")) / nrow(rhh_pitches)

actual_assumption_values_df <- tibble(
  assumption_name = c("changeup_vs_lhh_pct", "walk_rate", "sinker_vs_rhh_pct"),
  actual_value    = c(ch_lhh_pct,             bb_rate,     si_rhh_pct)
  # 0.267, 0.087, 0.488
)

# =============================================================================
# 4. RUN RETRO
# =============================================================================

retro_out <- build_retro_v32(
  game_date                   = "2026-04-21",
  team                        = "TOR",
  opponent                    = "LAA",
  pitcher_name                = "Jack Kochanowicz",
  pitcher_id                  = 686799L,
  pitcher_archetype           = "RHP_starter_laa_sinker_changeup",
  pitch_by_pitch_game_df      = pitch_by_pitch_game_df,
  preview_expectations_df     = preview_expectations_df,
  preview_assumptions_df      = preview_assumptions_df,
  hitter_assumption_map_df    = hitter_assumption_map_df,
  actual_assumption_values_df = actual_assumption_values_df,
  pitcher_baseline_df         = kochi_baseline_df,
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

retro_out$execution_flags |> filter(flag_value) |> print(n = Inf)
retro_out$model_error |> print()
retro_out$learned |> print(n = Inf)

# =============================================================================
# 6. MEMORY NOTES
# =============================================================================
#
# All three assumptions held — update reliability weights positively.
#   changeup_vs_lhh_pct: HELD twice now (check memory for archetype)
#   walk_rate: low-walk version (8.7%) — same as April 4 start
#   sinker_vs_rhh_pct: HELD, 48.8% confirmed
#
# Varsho Edge validated: Changeup split .575/32PA drove the call.
#   Two singles tonight. Direction correct.
#
# Gimenez Suppressed held: .000 actual vs .216 exp. LHH splits
#   against this Sinker/Changeup mix are poor. Signal confirmed.
#
# Clement Suppressed nearly validated: .296 actual vs .279 exp (+0.017).
#   BVP (.178 in 5 PA) was slightly too pessimistic. Narrow miss.
#
# Low-walk Kochanowicz profile: 8.7% tonight (same as Apr 4).
#   Two versions documented: ~9% and ~15-23%. Watch first-inning
#   command to identify which version next time.
#
# RUNNING N: ~142 hitter-games after observation 21.
# CONSECUTIVE H1 WINS: 2 (obs 20 Detmers, obs 21 Kochanowicz).
# FIRST POSITIVE RHO: +0.595 (obs 21).
#
# =============================================================================
# END · retro_runner_2026-04-21_kochanowicz.R
# =============================================================================
