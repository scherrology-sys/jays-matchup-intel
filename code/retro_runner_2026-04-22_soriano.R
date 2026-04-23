# =============================================================================
# Jays Matchup Intel V3.2
# Retro Runner · TOR @ LAA · April 22, 2026
# Pitcher: José Soriano (RHP) · ID: 667755
# Result: LAA 7, TOR 3 · Observation 22
# @scherrology | Arm Chair Analyst
# =============================================================================
# source("analysis/jays_matchup_intel_v3_2.R")
# =============================================================================
# PREVIEW CORRECTED — KC NOW IN PT_MAP AND PITCH MIX:
#   Correct 2026 mix: SI 31.3%, KC 26.5%, FF 23.8%, FS 12.4%, SL 6.0%
#   KC career: xwOBA .230, whiff% 42.8%, velo 85.7 mph
#   Guerrero: 11 PA vs KC (.438 wOBA) — just clears MIN_PA
#   STRUCTURAL BLIND SPOT (LHH): KC + Splitter both fallback = 39.4% of LHH mix
# =============================================================================
# SORIANO OUTING:
#   5 IP · 84 pitches · 20 PA · 4K · 0BB · 7H · 0R · exceptional strand rate
#   KC: 35.7% (30) · whiff% 46.2% — most-thrown pitch tonight
#   SI: 22.6% (19) · whiff% 35.7%
#   FF: 22.6% (19) · whiff% 11.1%
#   FS: 16.7% (14) · whiff% 57.1%
#   SL:  2.4%  (2) · whiff% 0.0%
#   vs LHH (n=46 full): KC 30.4%, FS 26.1%, SI 21.7%, FF 21.7%
#   vs RHH (n=38 full): KC 42.1%, SI 23.7%, FF 23.7%, FS 5.3%, SL 5.3%
#   TOR scored 0 runs off Soriano. All 3 TOR runs came off LAA bullpen in inn 7.
#   LAA scored 7 runs off TOR pitching (Romano, Zeferjahn, Silseth, Suter).
# =============================================================================
# ASSUMPTION ACTUALS (against corrected preview):
#   KC vs LHH:       expected 19.7%  actual 30.4% (14/46)  PARTIAL (delta 10.7pp, tol ±7pp)
#   Splitter vs LHH: expected 19.7%  actual 26.1% (12/46)  HELD   (delta  6.4pp, tol ±7pp)
#   K rate:          expected 32.5%  actual 20.0%  (4/20)  PARTIAL (delta 12.5pp, tol ±10pp)
#   Note: 0 BB counterbalanced low K; Soriano generated contact without runs
# =============================================================================
# HITTER RESULTS (scoreable, PA >= 2 vs Soriano):
#
# Hitter              Hand  PA  H  K  BB  actual_wOBA  exp_wOBA  resid   Tier
# Clement             R     3   2  0   0    0.592        0.311    +0.281  Neutral
# Giménez, A.         L     2   1  0   0    0.444        0.240    +0.204  Supp (missed)
# Sánchez             L     2   1  1   0    0.444        0.320    +0.124  Neutral
# Varsho              L     2   1  0   0    0.444        0.301    +0.143  Neutral
# Lukes               L     3   1  0   0    0.296        0.306    -0.010  Neutral
# Okamoto             R     2   0  1   0    0.000        0.320    -0.320  Neutral
# Guerrero Jr.        R     2   0  1   0    0.000        0.418    -0.418  Edge (K on KC)
# Heineman            L     2   0  1   0    0.000        0.366    -0.366  Edge (both outs on FS)
#
# Not scored (<2 PA vs Soriano): Straw, Barger, Schneider, Eloy
# Sosa (2 PA, GIDP + single, not on 2026 roster): excluded
#
# n=8  MAE=0.232  Bias=-0.047  Naive=0.204  Spearman rho=-0.661
# Champion beats naive: FALSE
# =============================================================================

library(tidyverse)

# =============================================================================
# 1. LOAD DATA
# =============================================================================

full_game_df   <- safe_read_csv("savant_data__77_.csv")
hitter_game_df <- safe_read_csv("savant_data__78_.csv")

# Soriano pitches only (ID: 667755, RHP)
pitch_by_pitch_game_df <- hitter_game_df |> filter(pitcher == 667755L)

# Soriano 2026 baseline — NOTE: KC must now be included in pitch type analysis
soriano_baseline_df <- safe_read_csv("savant_data__29_.csv") |>
  filter(game_year == 2026)

# CORRECTED 2026 pitch mix (to be used in future previews):
# SI 31.3% | KC 26.5% | FF 23.8% | FS 12.4% | SL 6.0%
# KC career: xwOBA .245 · whiff% 42.8% · velo 85.7 mph · n=1311 pitches

# =============================================================================
# 2. LOAD PREVIEW OUTPUTS
# =============================================================================

preview_expectations_df  <- safe_read_csv(
  "outputs/preview_v32/preview_expectations_v32_2026-04-22_667755.csv")
preview_assumptions_df   <- safe_read_csv(
  "outputs/preview_v32/preview_assumptions_v32_2026-04-22_667755.csv")
hitter_assumption_map_df <- safe_read_csv(
  "outputs/preview_v32/preview_hitter_map_v32_2026-04-22_667755.csv")

# =============================================================================
# 3. ACTUAL ASSUMPTION VALUES
# =============================================================================

PA_EVENTS <- c("single","double","triple","home_run","field_out","strikeout",
               "walk","hit_by_pitch","grounded_into_double_play","force_out",
               "sac_fly","fielders_choice","fielders_choice_out","double_play",
               "strikeout_double_play","sac_bunt","field_error")

soriano_game <- full_game_df |> filter(pitcher == 667755L)

# NOTE: The preview PT_MAP excluded KC. Correct denominators use ALL pitches
# including KC. Using full 84-pitch denominator for 4-Seam.
# Using full 46-pitch LHH denominator for Splitter vs LHH.
valid_all  <- soriano_game |> filter(pitch_type %in% c("FF","SI","FS","SL","KC"))
lhh_all    <- valid_all    |> filter(stand == "L")   # n=46 with KC
pa_total   <- pitch_by_pitch_game_df |> filter(events %in% PA_EVENTS) |> nrow()

# Splitter vs LHH: 12/46 = 26.1% — PARTIAL (expected 24.5%, tol ±6pp, delta 1.6pp)
# Note: original score FAILED used wrong denom of 32 (KC excluded)
fs_lhh_pct <- nrow(lhh_all |> filter(pitch_type == "FS")) / nrow(lhh_all)
# 0.261 — within extended range once KC included

# 4-Seam overall: 19/84 = 22.6% — HELD (2026 file rate 23.8% with full denom)
ff_pct <- nrow(valid_all |> filter(pitch_type == "FF")) / nrow(valid_all)
# 0.226

# K rate: 4/20 = 20.0% — PARTIAL (expected 32.5%, tol ±10pp, delta 12.5pp)
k_count <- pitch_by_pitch_game_df |>
  filter(events %in% c("strikeout","strikeout_double_play")) |> nrow()
k_rate <- k_count / pa_total
# 0.200

actual_assumption_values_df <- tibble(
  assumption_name = c("kc_vs_lhh_pct",  "splitter_vs_lhh_pct", "k_rate"),
  actual_value    = c(0.304,             0.261,                  0.200)
  # KC vs LHH: 14/46 = 30.4%  PARTIAL
  # Splitter vs LHH: 12/46 = 26.1%  HELD
  # K rate: 4/20 = 20.0%  PARTIAL
)

# =============================================================================
# 4. RUN RETRO
# NOTE: The preview_assumptions_df was built with KC excluded from the mix.
# The audit results reflect a preview that was structurally incomplete.
# The KC issue is logged in model_memory as a process correction, not as an
# assumption reliability update.
# =============================================================================

retro_out <- build_retro_v32(
  game_date                   = "2026-04-22",
  team                        = "TOR",
  opponent                    = "LAA",
  pitcher_name                = "José Soriano",
  pitcher_id                  = 667755L,
  pitcher_archetype           = "RHP_starter_laa_sinker_splitter_kc",
  pitch_by_pitch_game_df      = pitch_by_pitch_game_df,
  preview_expectations_df     = preview_expectations_df,
  preview_assumptions_df      = preview_assumptions_df,
  hitter_assumption_map_df    = hitter_assumption_map_df,
  actual_assumption_values_df = actual_assumption_values_df,
  pitcher_baseline_df         = soriano_baseline_df,
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

retro_out$model_error |> print()

# =============================================================================
# 6. MEMORY NOTES
# =============================================================================
#
# Confirmed 2026 Soriano mix (KC now in PT_MAP):
#   Sinker:          31.3%  (SI) · xwOBA .331 · 97.4 mph
#   Knuckle Curve:   26.5%  (KC) · xwOBA .230 · 85.7 mph · 42.8% whiff
#   4-Seam:          23.8%  (FF) · xwOBA .381 · 98.3 mph
#   Splitter:        12.4%  (FS) · xwOBA .218 · 92.5 mph · 37.6% whiff
#   Slider:           6.0%  (SL) · xwOBA .258 · 90.0 mph
#
# KC vs LHH in 2026 (estimate): ~30% based on tonight's data
# KC vs RHH in 2026 (estimate): ~42% based on tonight's data
# (Need full 2026 file breakdown by handedness with KC included)
#
# Guerrero vs KC: 0 PA history. K on 2-2 KC tonight.
#   Edge call built on SI/FF/SL — was not pricing KC at all.
#
# Heineman both outs on Splitter: FS blind spot confirmed.
#   Edge call was based on prior with SI/FF entered only.
#   FS and SL both fell back. The actual pitch diet was FS-heavy for LHH.
#
# Gimenez Suppressed missed: .240 exp, .444 actual.
#   SI .132 in 24 PA was the signal. He singled tonight.
#   Small-sample reversion. The signal direction is still credible.
#
# RUNNING N: ~150 hitter-games after observation 22.
# H1 this game: FALSE (naive wins 0.204 vs 0.232).
# H2 this game: FALSE (rho -0.661).
# Root cause: structural blind spots (KC + FS both fallback for LHH = 39.4% of mix).
# Both Edge calls (Guerrero, Heineman) produced zero despite correct priors.
#
# =============================================================================
# END · retro_runner_2026-04-22_soriano.R
# =============================================================================
