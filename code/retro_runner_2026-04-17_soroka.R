# =============================================================================
# Jays Matchup Intel V3.2
# Retro Runner · TOR @ ARI · April 17, 2026
# Pitcher: Michael Soroka (RHP) · ID: 647336
# Result: ARI 6, TOR 3 · Observation 17
# @scherrology | Arm Chair Analyst
# =============================================================================
# source("analysis/jays_matchup_intel_v3_2.R")
# =============================================================================
# KEY FINDING: Sweeper (SV) appeared at 29.8% — not in preview mix.
# Zero Sweeper pitches in 273 prior 2026 pitches and career file.
# Cutter (FC) usage doubled: expected 10.0%, actual 20.2%.
# =============================================================================

library(tidyverse)

# =============================================================================
# 1. LOAD DATA
# =============================================================================

# savant_data__61_.csv · full game pitch file (all pitchers)
# savant_data__62_.csv · hitter game file (all batters)

full_game_df   <- safe_read_csv("savant_data__61_.csv")
hitter_game_df <- safe_read_csv("savant_data__62_.csv")

# Soroka pitches only (ID: 647336)
soroka_game_df         <- full_game_df   |> filter(pitcher == 647336L)
pitch_by_pitch_game_df <- hitter_game_df |> filter(pitcher == 647336L)

# Soroka 2026 baseline (prior starts, from savant_data__59_.csv)
soroka_baseline_df <- safe_read_csv("savant_data__59_.csv") |>
  filter(game_year == 2026)

# =============================================================================
# 2. LOAD PREVIEW OUTPUTS
# =============================================================================

preview_expectations_df  <- safe_read_csv(
  "outputs/preview_v32/preview_expectations_v32_2026-04-17_647336.csv")
preview_assumptions_df   <- safe_read_csv(
  "outputs/preview_v32/preview_assumptions_v32_2026-04-17_647336.csv")
hitter_assumption_map_df <- safe_read_csv(
  "outputs/preview_v32/preview_hitter_map_v32_2026-04-17_647336.csv")

# =============================================================================
# 3. VERIFIED PITCH MIX TONIGHT
# (from soroka_game_df, pitch_type counts across 94 total pitches)
#
#   SV (Sweeper):        28 pitches = 29.8% — NOT in preview mix
#   FF (4-Seam):         28 pitches = 29.8%
#   FC (Cutter):         19 pitches = 20.2%
#   CH (Changeup):       11 pitches = 11.7%
#   SI (Sinker):          8 pitches =  8.5%
#
# vs LHH (n=28): FF 53.6%, CH 32.1%, FC 14.3%
# vs RHH (n=38): FC 39.5%, FF 34.2%, SI 21.1%, CH 5.3%
#
# Sweeper by hand: LHH 10 pitches (35.7%), RHH 18 pitches (64.3%)
# Sweeper: velo 80.7 mph, pfx_x avg 0.862 ft, whiff% 20.0%
# =============================================================================

# =============================================================================
# 4. ACTUAL ASSUMPTION VALUES
# =============================================================================

PA_EVENTS <- c("single","double","triple","home_run","field_out","strikeout",
               "walk","hit_by_pitch","grounded_into_double_play","force_out",
               "sac_fly","fielders_choice","fielders_choice_out","double_play",
               "strikeout_double_play","sac_bunt","field_error")

# --- Slider usage ---
# SL (Slider) pitches tonight: 0. SV (Sweeper) is a separate pitch type.
# Slider assumption HELD as written.
sl_pct_actual <- 0.000
# HELD (tolerance ±0.030)

# --- Changeup to LHH ---
lhh_valid <- soroka_game_df |>
  filter(stand == "L", pitch_type %in% c("FF","SI","CH","FC","SV"))
ch_lhh_pct <- nrow(lhh_valid |> filter(pitch_type == "CH")) / nrow(lhh_valid)
# Result: 9/28 = 0.321 (expected 0.309, tolerance ±0.070) · HELD

# --- Cutter usage ---
all_valid <- soroka_game_df |>
  filter(pitch_type %in% c("FF","SI","CH","FC","SV"))
fc_pct <- nrow(all_valid |> filter(pitch_type == "FC")) / nrow(all_valid)
# Result: 19/94 = 0.202 (expected 0.100, tolerance ±0.060) · FAILED (+10.2pp)

actual_assumption_values_df <- tibble(
  assumption_name = c("slider_usage", "changeup_to_lhh_pct", "cutter_usage_pct"),
  actual_value    = c(sl_pct_actual,  ch_lhh_pct,            fc_pct)
  # 0.000, 0.321, 0.202
)

# =============================================================================
# 5. RUN RETRO
# =============================================================================

retro_out <- build_retro_v32(
  game_date                   = "2026-04-17",
  team                        = "TOR",
  opponent                    = "ARI",
  pitcher_name                = "Michael Soroka",
  pitcher_id                  = 647336L,
  pitcher_archetype           = "RHP_starter_ari_4seam",
  pitch_by_pitch_game_df      = pitch_by_pitch_game_df,
  preview_expectations_df     = preview_expectations_df,
  preview_assumptions_df      = preview_assumptions_df,
  hitter_assumption_map_df    = hitter_assumption_map_df,
  actual_assumption_values_df = actual_assumption_values_df,
  pitcher_baseline_df         = soroka_baseline_df,
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
# Scoreable hitters (PA >= 2 vs Soroka):
#
# Hitter              Hand  PA  H  K  BB  actual_wOBA  exp_wOBA  resid   Tier
# Guerrero Jr.        R     3   0  0   0    0.000        0.413    -0.413  Edge
# Schneider           R     3   0  1   0    0.000        0.384    -0.384  Edge
# Straw               R     2   2  0   0    1.494        0.322    +1.172  Neutral
# Sánchez             L     3   1  0   0    0.296        0.320    -0.024  Neutral
# Okamoto             R     3   0  0   0    0.000        0.320    -0.320  Neutral
# Clement             R     3   1  1   0    0.424        0.315    +0.109  Neutral
# Heineman            L     2   0  1   0    0.000        0.295    -0.295  Neutral
# Giménez, A.         L     3   1  1   0    0.424        0.261    +0.163  Supp
#
# Not scored (< 2 PA vs Soroka):
#   Varsho (1 PA, K) · Lukes (0 PA) · Barger (0 PA) · Eloy (0 PA) · Leo (0 PA)
#
# MAE: 0.360  Bias: +0.001  Naive: 0.336  Spearman rho: -0.167
# Champion beats naive: FALSE
#
# =============================================================================
# 8. MEMORY NOTES
# =============================================================================
#
# SWEEPER (SV):
#   29.8% of pitches tonight. Not present in any prior 2026 start data
#   (savant_data__59_.csv, 273 pitches) or 2024-25 hitter history.
#   Add to next Soroka preview mix if Jays rematched:
#   Approximate usage: ~30% overall, LHH ~35%, RHH ~65%.
#   Metrics: velo 80.7 mph, pfx_x 0.862 ft, whiff% 20.0%.
#
# CUTTER:
#   Expected 10.0%, actual 20.2% overall and 39.5% vs RHH.
#   Guerrero faced FC twice, produced field outs both times.
#   Update next Soroka preview: Cutter to RHH likely 25-40%, not 11.4%.
#
# STRAW HR:
#   Home run off Changeup at 2-2 count. wOBA_overall .302, Changeup split
#   vs RHP .225 in 31 PA. Small-sample variance. Note for retro memory.
#
# GIMÉNEZ ANDRES:
#   Suppressed at .261. Actual .424 (double, 3rd PA). Two PA is the minimum
#   threshold. Suppressed call remains defensible from the underlying splits.
#
# RUNNING N: ~116 hitter-games after observation 17.
#
# =============================================================================
# END · retro_runner_2026-04-17_soroka.R
# =============================================================================
