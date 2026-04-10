# =============================================================================
# Jays Matchup Intel
# helpers.R
# =============================================================================

library(tidyverse)

# =============================================================================
# CONSTANTS
# =============================================================================

PA_EVENTS <- c(
  "single", "double", "triple", "home_run",
  "field_out", "strikeout", "walk", "hit_by_pitch",
  "grounded_into_double_play", "force_out", "sac_fly",
  "fielders_choice", "fielders_choice_out", "double_play"
)

WOBA_WEIGHTS <- c(
  walk                       = 0.696,
  hit_by_pitch               = 0.726,
  single                     = 0.888,
  double                     = 1.271,
  triple                     = 1.616,
  home_run                   = 2.101
)

HAND_BASELINES <- c(
  L = 0.307,
  R = 0.322
)

PARK_FACTORS <- c(
  TOR = 0.99, BOS = 1.02, NYY = 1.00, BAL = 1.01, TB  = 0.97,
  CLE = 0.98, CWS = 0.98, DET = 0.99, KC  = 1.00, MIN = 1.01,
  HOU = 1.00, LAA = 1.00, OAK = 0.98, ATH = 0.98, SEA = 0.97,
  TEX = 1.01, ATL = 1.01, MIA = 0.97, NYM = 1.00, PHI = 1.02,
  WSN = 1.00, CHC = 1.02, CIN = 1.05, MIL = 0.99, PIT = 0.99,
  STL = 0.99, ARI = 1.01, COL = 1.15, LAD = 0.97, SD  = 0.98,
  SF  = 0.97
)

RANKING_TIER_THRESHOLDS <- c(
  HIGH     = 0.018,
  MODERATE = 0.030,
  LOW      = 0.045
)

MIN_PA      <- 10L   # minimum PA vs a pitch type to use split
HALF_LIFE   <- 180   # days, pitcher recency weighting

# =============================================================================
# FILE UTILITIES
# =============================================================================

safe_read_csv <- function(path, ...) {
  if (!file.exists(path)) stop(sprintf("File not found: %s", path))
  read_csv(path, show_col_types = FALSE, ...)
}

safe_dir_create <- function(path) {
  if (!dir.exists(path)) dir.create(path, recursive = TRUE)
  invisible(path)
}

# =============================================================================
# PARK ADJUSTMENT
# =============================================================================

park_adjusted_baseline <- function(hand, park_code) {
  pf <- PARK_FACTORS[park_code]
  if (is.na(pf)) {
    warning(sprintf("No park factor for '%s' — using 1.00", park_code))
    pf <- 1.0
  }
  round(HAND_BASELINES[hand] * pf, 3)
}

# =============================================================================
# HITTER PRIORS
# Build overall wOBA and pitch-type splits from a Statcast batting data frame.
# Expects columns: player_name, events, pitch_name, stand
# =============================================================================

build_priors <- function(df) {
  pa <- df |>
    filter(events %in% PA_EVENTS) |>
    mutate(
      wv = coalesce(WOBA_WEIGHTS[events], 0),
      wd = as.integer(events != "hit_by_pitch")
    )

  overall <- pa |>
    group_by(player_name) |>
    summarise(
      wn          = sum(wv, na.rm = TRUE),
      wd          = sum(wd, na.rm = TRUE),
      n_pa        = n(),
      .groups     = "drop"
    ) |>
    mutate(wOBA_overall = round(wn / pmax(wd, 1), 3))

  splits <- pa |>
    filter(!is.na(pitch_name), pitch_name != "") |>
    group_by(player_name, pitch_name) |>
    summarise(
      PA_vs   = n(),
      sp_wn   = sum(wv, na.rm = TRUE),
      sp_wd   = sum(wd, na.rm = TRUE),
      .groups = "drop"
    ) |>
    mutate(pitch_wOBA = round(sp_wn / pmax(sp_wd, 1), 3))

  hand <- pa |>
    group_by(player_name) |>
    summarise(hand = names(which.max(table(stand))), .groups = "drop")

  list(overall = overall, splits = splits, hand = hand)
}

# =============================================================================
# PITCHER ROLLING MIX
# Recency-weighted pitch mix from a pitcher Statcast file.
# Returns named list: mix (named numeric), eff_sample (int)
# =============================================================================

rolling_mix <- function(df, before_date, batter_hand = NULL, hl = HALF_LIFE) {
  before_date <- as.Date(before_date)

  sub <- df |>
    filter(
      as.Date(game_date) < before_date,
      !pitch_name %in% c("", "Unknown", NA)
    )

  if (!is.null(batter_hand)) {
    sub <- sub |> filter(stand == batter_hand)
  }

  if (nrow(sub) == 0) return(list(mix = numeric(0), eff_sample = 0L))

  sub <- sub |>
    mutate(
      age = as.integer(before_date - as.Date(game_date)),
      rw  = 0.5^(age / hl)
    )

  w <- sub |>
    group_by(pitch_name) |>
    summarise(rw_sum = sum(rw), .groups = "drop") |>
    mutate(prop = rw_sum / sum(rw_sum))

  mix        <- setNames(w$prop, w$pitch_name)
  eff_sample <- as.integer(sum(sub$rw))

  list(mix = mix, eff_sample = eff_sample)
}

# =============================================================================
# CONFIDENCE TIER
# =============================================================================

confidence_tier <- function(eff_l, eff_r) {
  eff <- min(eff_l, eff_r)
  case_when(
    eff >= 700 ~ "HIGH",
    eff >= 200 ~ "MODERATE",
    TRUE       ~ "LOW"
  )
}

adjusted_K <- function(conf) {
  c(HIGH = 60L, MODERATE = 45L, LOW = 30L)[conf]
}

# =============================================================================
# RANKING TIERS
# Applies Edge / Neutral / Suppressed based on distance from team mean.
# =============================================================================

compute_ranking_tiers <- function(df, conf, team_mean = NULL) {
  thresh <- RANKING_TIER_THRESHOLDS[conf]
  if (is.null(team_mean)) team_mean <- mean(df$exp_wOBA, na.rm = TRUE)

  df |>
    mutate(
      team_mean = round(team_mean, 3),
      tier = case_when(
        exp_wOBA > team_mean + thresh ~ "Edge",
        exp_wOBA < team_mean - thresh ~ "Suppressed",
        TRUE                          ~ "Neutral"
      )
    )
}
