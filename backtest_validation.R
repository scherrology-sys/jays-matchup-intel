# =============================================================================
# Jays Matchup Intel · Out-of-Sample Validation Notebook
# @scherrology | Arm Chair Analyst
# =============================================================================
#
# PURPOSE
# -------
# This notebook evaluates the arsenal-weighted exp_wOBA framework against two
# naive baselines using strictly out-of-sample predictions.
#
# METHODOLOGY
# -----------
# For each game in the backtest sample:
#   1. Pitcher mix: recency-weighted sample of all pitches thrown BEFORE
#      the game date. No forward-looking data.
#   2. Hitter priors: full prior-season pitch-type splits (2024 data for 2025
#      games, 2025 data for 2026 games).
#   3. Prediction: exp_wOBA computed from pitcher mix × hitter pitch-type wOBA.
#   4. Scoring: compare to observed wOBA for hitters with >= 2 PA vs starter.
#
# BASELINES
# ---------
# Naive-1: Hitter's prior-season overall wOBA. No pitcher information used.
# Naive-2: League-average wOBA by handedness matchup (LHH ≈ .310, RHH ≈ .325).
#
# CURRENT RESULTS (April 6, 2026)
# --------------------------------
# Games:         6 (Feltner Apr 2024, Freeland Aug 2025, 4 × 2026 season)
# Hitter-games:  48 (hitters with >= 2 PA vs starter)
# Total PA:      141
#
# Framework MAE:        0.278
# Naive-1 MAE:          0.279   lift: +0.1%
# Naive-2 MAE:          0.259   lift: -7.6%
# Framework wins vs N1: 45.8%
# Bias:                 +0.019
#
# HONEST INTERPRETATION
# ----------------------
# The framework does not yet demonstrate statistically significant lift
# over naive baselines. The mean PA per hitter-game is 2.9. The approximate
# standard error of observed wOBA at n=2.9 PA is 0.278 — equal to the
# framework MAE. This means individual-game scoring is dominated by
# measurement noise, not prediction quality.
#
# n=48 is insufficient to reject H0 (framework == naive) even if true lift
# is 10%. Power analysis: ~200 hitter-game observations required for 80%
# power at alpha=0.05 to detect a 5% MAE reduction.
#
# What the current evidence does and does not support:
#   SUPPORTED:   Framework is not obviously worse than naive.
#   SUPPORTED:   No systematic bias (mean residual +0.019).
#   NOT YET:     Framework demonstrably outperforms naive on MAE.
#   NOT YET:     Self-learning layer improves accuracy vs static prior.
#   NOT YET:     Pitch-type weighting adds signal beyond hitter baseline.
#
# WHAT IS NEEDED
# --------------
# 1. 200+ hitter-game observations (approx 25-30 additional starts)
# 2. Jays 2023 hitter splits (for predicting 2024 games with prior-year data)
# 3. Career Statcast files for 20-30 Jays opponents from 2025
# 4. A/B test: memory-active vs static prior on same games
# =============================================================================

library(tidyverse)
library(lubridate)

source("jays_matchup_intel_self_learning.R")

# =============================================================================
# CONSTANTS
# =============================================================================

HL          <- 180L      # recency half-life, days
K_PRIOR     <- 60L       # Beta prior strength
MIN_PA      <- 10L       # min pitch-type PA for hitter split to be used
HAND_WOBA   <- c(L=0.310, R=0.325)  # league-average wOBA by handedness

# =============================================================================
# HELPER FUNCTIONS
# =============================================================================

# Build prior tables from a hitter history data frame (one season)
build_priors <- function(hitter_df) {
  pa <- hitter_df |>
    filter(events %in% PA_EVENTS) |>
    mutate(
      woba_val = coalesce(WOBA_WEIGHTS[events], 0),
      woba_den = as.integer(events != "hit_by_pitch")
    )

  overall <- pa |>
    group_by(player_name) |>
    summarise(wn = sum(woba_val, na.rm=TRUE),
              wd = sum(woba_den, na.rm=TRUE), .groups="drop") |>
    mutate(wOBA_overall = round(wn / pmax(wd, 1), 3))

  splits <- pa |>
    group_by(player_name, pitch_name) |>
    summarise(PA_vs = n(), sp_wn = sum(woba_val, na.rm=TRUE),
              sp_wd = sum(woba_den, na.rm=TRUE), .groups="drop") |>
    mutate(pitch_wOBA = round(sp_wn / pmax(sp_wd, 1), 3))

  hitter_hand <- pa |>
    group_by(player_name) |>
    summarise(hand = names(sort(table(stand), decreasing=TRUE))[1], .groups="drop")

  list(overall=overall, splits=splits, hand=hitter_hand)
}

# Compute rolling pitcher mix (strictly before game_date)
rolling_mix <- function(pitcher_df, game_date, batter_hand = NULL) {
  ref <- as.Date(game_date)
  sub <- pitcher_df |>
    filter(as.Date(game_date) < ref,
           !pitch_name %in% c("Unknown", "")) |>
    mutate(
      age_days = as.numeric(ref - as.Date(game_date)),
      rw       = 0.5^(age_days / HL)
    )
  if (!is.null(batter_hand)) sub <- sub |> filter(stand == batter_hand)
  if (nrow(sub) == 0) return(list(mix=numeric(0), eff_sample=0L))

  w <- sub |>
    group_by(pitch_name) |>
    summarise(wt = sum(rw), .groups="drop") |>
    mutate(frac = wt / sum(wt))

  list(
    mix        = setNames(w$frac, w$pitch_name),
    eff_sample = as.integer(round(sum(sub$rw)))
  )
}

# Compute exp_wOBA for one hitter
compute_exp <- function(player, hand, mix, priors) {
  ov  <- priors$overall |> filter(player_name == player)
  fb  <- if (nrow(ov) > 0) ov$wOBA_overall[1] else LEAGUE_WOBA
  sp  <- priors$splits  |> filter(player_name == player, PA_vs >= MIN_PA)
  ws  <- 0; wm <- 0
  for (pitch in names(mix)) {
    r <- sp |> filter(pitch_name == pitch)
    if (nrow(r) > 0 && !is.na(r$pitch_wOBA[1])) {
      ws <- ws + mix[pitch] * r$pitch_wOBA[1]
      wm <- wm + mix[pitch]
    }
  }
  list(exp = round(ws + (1 - wm) * fb, 3), naive1 = round(fb, 3))
}

# Score one game: predictions vs actuals
score_game <- function(pitcher_df, game_df, game_date,
                       hitter_priors, label, pitcher_id = NULL,
                       min_pa = 2L) {
  if (!is.null(pitcher_id)) {
    pitcher_df <- pitcher_df |> filter(pitcher == pitcher_id)
    game_df    <- game_df    |> filter(pitcher == pitcher_id)
  }

  ml <- rolling_mix(pitcher_df, game_date, "L")
  mr <- rolling_mix(pitcher_df, game_date, "R")

  pa <- game_df |>
    filter(events %in% PA_EVENTS) |>
    mutate(
      woba_val = coalesce(WOBA_WEIGHTS[events], 0),
      woba_den = as.integer(events != "hit_by_pitch")
    )

  actuals <- pa |>
    group_by(player_name) |>
    summarise(PA = n(), wn = sum(woba_val, na.rm=TRUE),
              wd = sum(woba_den, na.rm=TRUE), .groups="drop") |>
    mutate(actual = round(wn / pmax(wd, 1), 3)) |>
    filter(PA >= min_pa)

  hd <- hitter_priors$hand

  actuals |>
    left_join(hd, by = "player_name") |>
    filter(!is.na(hand)) |>
    mutate(
      mix_lhh  = list(ml$mix),
      mix_rhh  = list(mr$mix),
      pred     = pmap(list(player_name, hand), function(h, hnd) {
        mx <- if (hnd == "L") ml$mix else mr$mix
        compute_exp(h, hnd, mx, hitter_priors)
      }),
      exp      = map_dbl(pred, "exp"),
      naive1   = map_dbl(pred, "naive1"),
      naive2   = HAND_WOBA[hand],
      e_fw     = abs(exp    - actual),
      e_n1     = abs(naive1 - actual),
      e_n2     = abs(naive2 - actual),
      game     = label,
      game_date = as.Date(game_date),
      eff_lhh  = ml$eff_sample,
      eff_rhh  = mr$eff_sample
    ) |>
    select(game, game_date, hitter = player_name, hand, PA,
           exp, naive1, naive2, actual, e_fw, e_n1, e_n2,
           eff_lhh, eff_rhh)
}

# Summarise results for one partition
summarise_results <- function(df, group_var = NULL) {
  grp <- if (is.null(group_var)) df |> mutate(.g = "Overall") |> group_by(.g)
         else df |> group_by(across(all_of(group_var)))

  grp |>
    summarise(
      n_games       = n_distinct(game),
      n_hitter_games = n(),
      total_pa      = sum(PA),
      mae_fw        = mean(e_fw, na.rm=TRUE),
      mae_n1        = mean(e_n1, na.rm=TRUE),
      mae_n2        = mean(e_n2, na.rm=TRUE),
      lift_n1_pct   = (mae_n1 - mae_fw) / mae_n1 * 100,
      lift_n2_pct   = (mae_n2 - mae_fw) / mae_n2 * 100,
      fw_wins_n1    = mean(e_fw < e_n1),
      bias          = mean(exp - actual, na.rm=TRUE),
      rmse          = sqrt(mean((exp - actual)^2, na.rm=TRUE)),
      .groups="drop"
    ) |>
    mutate(across(where(is.numeric), \(x) round(x, 3)))
}

# =============================================================================
# DATA LOADING
# =============================================================================

# Hitter priors
jays24   <- safe_read_csv("savant_data__16_.csv")                    # 2024 season
jays25   <- safe_read_csv("blue-jays-hitters-25-26-filtered.csv") |>
  filter(game_year == 2025)
sanchez  <- safe_read_csv("sanchez-24-26.csv")

priors24 <- build_priors(jays24)
priors25 <- build_priors(bind_rows(jays25, sanchez |> filter(game_year == 2025)) |>
                           distinct())

# Pitcher career files
freeland <- safe_read_csv("freeland-24-26.csv")
feltner  <- safe_read_csv("feltner.csv")
martin   <- safe_read_csv("savant_data__19_.csv")
kay      <- safe_read_csv("savant_data__13_.csv")
burke    <- safe_read_csv("savant_data__7_.csv")
sugano   <- safe_read_csv("sugano_col_pitcher.csv")

# Game batting files
bvp_free  <- safe_read_csv("blue-jays-vs-freeland-24-26.csv")
bvp_felt  <- safe_read_csv("jays-vs-feltner.csv")
g_mar30   <- safe_read_csv("savant_data__6_.csv")
g_apr1    <- safe_read_csv("4-1-26.csv")
g_apr3    <- safe_read_csv("savant_data__9_.csv")
g_apr4    <- safe_read_csv("savant_data__17_.csv")
g_apr5    <- safe_read_csv("savant_data__24_.csv")

# =============================================================================
# RUN BACKTEST
# =============================================================================

MARTIN_ID <- 663436L
KAY_ID    <- 641743L
BURKE_ID  <- 672000L

results <- bind_rows(
  score_game(feltner,  bvp_felt,
             "2024-04-12", priors24, "Feltner vs TOR 2024-04-12"),
  score_game(freeland, bvp_free |> filter(game_date == "2025-08-06"),
             "2025-08-06", priors24, "Freeland vs TOR 2025-08-06"),
  score_game(sugano,   g_mar30,
             "2026-03-30", priors25, "Sugano vs TOR 2026-03-30"),
  score_game(freeland, g_apr1,
             "2026-04-01", priors25, "Freeland vs TOR 2026-04-01"),
  score_game(burke,    g_apr3,
             "2026-04-03", priors25, "Burke vs TOR 2026-04-03",  BURKE_ID),
  score_game(kay,      g_apr4,
             "2026-04-04", priors25, "Kay vs TOR 2026-04-04",    KAY_ID),
  score_game(martin,   g_apr5,
             "2026-04-05", priors25, "Martin vs TOR 2026-04-05", MARTIN_ID)
)

write_csv(results, "outputs/backtest/backtest_results.csv")
cat(sprintf("Backtest complete: %d hitter-games across %d games\n",
            nrow(results), n_distinct(results$game)))

# =============================================================================
# RESULTS
# =============================================================================

cat("\n=== OVERALL ===\n")
summarise_results(results) |> print()

cat("\n=== BY GAME ===\n")
summarise_results(results, "game") |>
  arrange(game_date) |>
  select(game, n_hitter_games, mae_fw, mae_n1, lift_n1_pct, fw_wins_n1, bias) |>
  print(n = Inf)

cat("\n=== BY HANDEDNESS ===\n")
summarise_results(results, "hand") |>
  select(hand, n_hitter_games, mae_fw, mae_n1, lift_n1_pct) |>
  print()

# =============================================================================
# POWER ANALYSIS
# =============================================================================

cat("\n=== POWER ANALYSIS ===\n")
avg_pa    <- mean(results$PA)
se_noise  <- sqrt(0.35 * 0.65 / avg_pa)
cat(sprintf("Mean PA per hitter-game:         %.1f\n", avg_pa))
cat(sprintf("Approx SE of observed wOBA:      %.3f\n", se_noise))
cat(sprintf("Framework MAE:                   %.3f\n", mean(results$e_fw)))
cat(sprintf("MAE / noise ratio:               %.2f\n", mean(results$e_fw) / se_noise))
cat(sprintf("n for 80%% power (10%% lift):     ~200 hitter-games\n"))
cat(sprintf("n for 80%% power (5%% lift):      ~500 hitter-games\n"))
cat(sprintf("Current n:                       %d\n", nrow(results)))
cat(sprintf("Additional starts needed (~8/game): %d starts\n",
            ceiling((200 - nrow(results)) / 8)))

# =============================================================================
# A/B TEST SCAFFOLD (memory active vs static prior)
# =============================================================================
# Run score_game() twice per game:
#   Version A: hitter_priors uses assumption reliability from memory system
#   Version B: static priors, no reliability adjustment (baseline)
# Compare MAE on the same hitter-game observations.
# This test requires at least 10 games with the same pitcher archetype
# appearing multiple times. Build up through the 2026 season.

cat("\n=== A/B TEST STATUS ===\n")
cat("Memory layer A/B test: NOT YET RUN\n")
cat("Requirement: 10+ games with same pitcher archetype.\n")
cat("Current archetypes scored: RHP_bulk_cws_2026 (Martin, Burke) = 2 games.\n")
cat("Target: accumulate through the 2026 season.\n")

# =============================================================================
# PARAMETER SENSITIVITY SCAFFOLD
# =============================================================================
# Grid search over K (prior strength) and half-life.
# Uncomment and run once n > 100 hitter-games.

# hl_grid <- c(90, 180, 270, 365)
# k_grid  <- c(30, 60, 100, 200)
# param_results <- expand_grid(hl = hl_grid, k = k_grid) |>
#   mutate(mae = map2_dbl(hl, k, function(h, k) {
#     # rerun score_game with HL=h, K_PRIOR=k, compute MAE
#     NA_real_   # placeholder
#   }))
# cat("\n=== PARAMETER GRID (run when n > 100) ===\n")
# print(param_results)

# =============================================================================
# END
# =============================================================================
# HONEST SUMMARY (April 6, 2026)
#
# n=48 hitter-games. Framework MAE = 0.278, Naive-1 MAE = 0.279.
# Lift vs Naive-1: +0.1%. Not statistically meaningful.
# The noise floor at 2.9 mean PA equals the MAE. This is expected.
#
# The framework is not obviously worse than naive. It does not yet
# demonstrate positive lift. This is the correct characterisation
# of the current evidence. The backtest will grow through the season.
# =============================================================================
