# =============================================================================
# Jays Matchup Intel V3.2
# Retro Runner · TOR @ ARI · April 18, 2026
# Pitcher: Zac Gallen (RHP) · ID: 668678
# Result: ARI 6, TOR 2 · Observation 18
# @scherrology | Arm Chair Analyst
# =============================================================================
# source("analysis/jays_matchup_intel_v3_2.R")
# =============================================================================
# KEY FINDING: Knuckle Curve recovered. 26 pitches, .200 xwOBA, 26.7% whiff.
# Preview expected .608 xwOBA from 2026 signal. Career number is .289.
# Slider to RHH dropped from 44.7% expected to 31.9% actual (PARTIAL).
# KC filled the gap, rising from 16.6% expected to 29.5% actual.
# =============================================================================

library(tidyverse)

# =============================================================================
# 1. LOAD DATA
# =============================================================================

# savant_data__65_.csv · full game pitch file (all pitchers)
# savant_data__66_.csv · hitter game file (all batters)

full_game_df   <- safe_read_csv("savant_data__65_.csv")
hitter_game_df <- safe_read_csv("savant_data__66_.csv")

# Gallen pitches only (ID: 668678)
pitch_by_pitch_game_df <- hitter_game_df |> filter(pitcher == 668678L)

# Gallen 2026 baseline (prior starts, from savant_data__64_.csv)
gallen_baseline_df <- safe_read_csv("savant_data__64_.csv") |>
  filter(game_year == 2026)

# =============================================================================
# 2. LOAD PREVIEW OUTPUTS
# =============================================================================

preview_expectations_df  <- safe_read_csv(
  "outputs/preview_v32/preview_expectations_v32_2026-04-18_668678.csv")
preview_assumptions_df   <- safe_read_csv(
  "outputs/preview_v32/preview_assumptions_v32_2026-04-18_668678.csv")
hitter_assumption_map_df <- safe_read_csv(
  "outputs/preview_v32/preview_hitter_map_v32_2026-04-18_668678.csv")

# =============================================================================
# 3. VERIFIED PITCH MIX TONIGHT
# (from full_game_df |> filter(pitcher==668678), 88 total pitches)
#
#   FF (4-Seam):      26 pitches = 29.5%   velo=93.0  whiff%=0.0%
#   KC (Knuckle Curve):26 pitches = 29.5%  velo=82.1  whiff%=26.7%
#   SL (Slider):      15 pitches = 17.0%   velo=88.5  whiff%=27.3%
#   CH (Changeup):    13 pitches = 14.8%   velo=85.7  whiff%=0.0%
#   SI (Sinker):       8 pitches =  9.1%   velo=92.2  whiff%=0.0%
#
# vs LHH (n=41): FF 43.9%, CH 29.3%, KC 26.8%
# vs RHH (n=47): KC 31.9%, SL 31.9%, FF 17.0%, SI 17.0%, CH 2.1%
# =============================================================================

# =============================================================================
# 4. ACTUAL ASSUMPTION VALUES
# =============================================================================

PA_EVENTS <- c("single","double","triple","home_run","field_out","strikeout",
               "walk","hit_by_pitch","grounded_into_double_play","force_out",
               "sac_fly","fielders_choice","fielders_choice_out","double_play",
               "strikeout_double_play","sac_bunt","field_error")

gallen_game <- full_game_df |> filter(pitcher == 668678L)
valid_pts    <- gallen_game |> filter(pitch_type %in% c("FF","SL","KC","CH","SI"))
rhh_pitches  <- valid_pts   |> filter(stand == "R")

# --- Slider vs RHH ---
# Expected 44.7%, actual 15/47 = 31.9% → PARTIAL (delta -12.8pp, tol ±8pp)
sl_rhh_pct <- nrow(rhh_pitches |> filter(pitch_type == "SL")) / nrow(rhh_pitches)
# 0.319

# --- Knuckle Curve xwOBA ---
# Expected .608, actual .200 (5 balls in play: 4 field outs, 1 single) → HELD
kc_xwoba_actual <- gallen_game |>
  filter(pitch_type == "KC",
         !is.na(estimated_woba_using_speedangle),
         estimated_woba_using_speedangle != "") |>
  summarise(xwoba = mean(as.numeric(estimated_woba_using_speedangle), na.rm=TRUE)) |>
  pull(xwoba)
# ~0.200

# --- K rate ---
# Expected 12.7%, actual 3/25 = 12.0% → HELD
k_rate_actual <- pitch_by_pitch_game_df |>
  filter(events %in% c("strikeout","strikeout_double_play")) |>
  nrow() /
  (pitch_by_pitch_game_df |> filter(events %in% PA_EVENTS) |> nrow())
# 0.120

actual_assumption_values_df <- tibble(
  assumption_name = c("slider_vs_rhh_pct", "knuckle_curve_quality", "k_rate"),
  actual_value    = c(sl_rhh_pct,           kc_xwoba_actual,         k_rate_actual)
  # 0.319, 0.200, 0.120
)

# =============================================================================
# 5. RUN RETRO
# =============================================================================

retro_out <- build_retro_v32(
  game_date                   = "2026-04-18",
  team                        = "TOR",
  opponent                    = "ARI",
  pitcher_name                = "Zac Gallen",
  pitcher_id                  = 668678L,
  pitcher_archetype           = "RHP_starter_ari_slider",
  pitch_by_pitch_game_df      = pitch_by_pitch_game_df,
  preview_expectations_df     = preview_expectations_df,
  preview_assumptions_df      = preview_assumptions_df,
  hitter_assumption_map_df    = hitter_assumption_map_df,
  actual_assumption_values_df = actual_assumption_values_df,
  pitcher_baseline_df         = gallen_baseline_df,
  memory_dir                  = "model_memory",
  output_dir                  = "outputs/retro_v32"
)

# =============================================================================
# 6. REVIEW
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
# 7. VERIFIED HITTER RESULTS (pre-computed from data)
# =============================================================================
# Scoreable hitters (PA >= 2 vs Gallen):
#
# Hitter              Hand  PA  H   K  BB  actual_wOBA  exp_wOBA  resid   Tier
# Guerrero Jr.        R     3   1   0   0    0.296        0.373    -0.077  Edge
# Straw               R     2   1   0   0    0.888        0.270    +0.618  Supp
# Clement             R     3   2   0   0    0.720        0.281    +0.439  Supp
# Okamoto             R     3   2   1   0    0.592        0.320    +0.272  Neutral
# Lukes               L     3   1   0   0    0.296        0.330    -0.034  Neutral
# Sánchez             L     3   1   0   0    0.296        0.320    -0.024  Neutral
# Jiménez, Eloy       R     3   1   1   0    0.296        0.320    -0.024  Neutral
# Heineman            L     2   0   0   0    0.000        0.288    -0.288  Supp
# Giménez, A.         L     3   0   1   0    0.000        0.277    -0.277  Supp
#
# Not scored (0 PA vs Gallen):
#   Schneider · Varsho · Barger · Jiménez Leo
#
# MAE: 0.228  Bias: +0.067  Naive: 0.220  Spearman rho: -0.233
# Champion beats naive: FALSE
#
# =============================================================================
# 8. MEMORY NOTES
# =============================================================================
#
# KC recovery: 2026 signal was .608 xwOBA (57 pitches, 4 starts).
#   Tonight .200 xwOBA (26 pitches). Career .289.
#   For next Gallen preview: weight KC assumption toward career .289.
#   The 2026 signal was a small-sample artifact.
#
# Slider vs RHH: expected 44.7%, actual 31.9%. PARTIAL.
#   He pivoted to KC when it was working. The in-game adjustment is
#   not capturable pre-game. Widen CI on Slider rate assumption.
#
# Changeup: 0% whiff on 13 pitches. All contact was outs (popup, ground ball).
#   Effective neutralizer tonight despite no swing-and-miss.
#
# RHH Suppressed calls (Clement .281, Straw .270):
#   Both outperformed when Slider rate dropped and KC rose.
#   The Slider-heavy RHH mix was the basis for their Suppressed tier.
#   When the actual mix differed, the suppression assumptions weakened.
#
# LHH Suppressed calls (Heineman .288, Gimenez .277):
#   Both held. LHH splits were the correct signal.
#
# RUNNING N: ~125 hitter-games after observation 18.
#
# =============================================================================
# END · retro_runner_2026-04-18_gallen.R
# =============================================================================
