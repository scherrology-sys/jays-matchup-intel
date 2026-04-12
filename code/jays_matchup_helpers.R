# =============================================================================
# Jays Matchup Intel
# helpers.R — public constants and utilities
# Sources alongside jays_matchup_intel_v3_2.R (private)
# =============================================================================
# Contains ONLY what the engine does not provide:
#   PARK_FACTORS, HAND_BASELINES, park_adjusted_baseline()
#   rolling_mix() / blended_mix() for pitcher mix computation
#   Ranking tier thresholds and compute_ranking_tiers()
# All model logic, memory, and assumption handling lives in the engine.
# =============================================================================

library(tidyverse)

PARK_FACTORS <- c(
  TOR = 0.99, BOS = 1.02, NYY = 1.00, BAL = 1.01, TB  = 0.97,
  CLE = 0.98, CWS = 0.98, DET = 0.99, KC  = 1.00, MIN = 1.01,
  HOU = 1.00, LAA = 1.00, OAK = 0.98, ATH = 0.98, SEA = 0.97,
  TEX = 1.01, ATL = 1.01, MIA = 0.97, NYM = 1.00, PHI = 1.02,
  WSN = 1.00, CHC = 1.02, CIN = 1.05, MIL = 0.99, PIT = 0.99,
  STL = 0.99, ARI = 1.01, COL = 1.15, LAD = 0.97, SD  = 0.98,
  SF  = 0.97
)

HAND_BASELINES <- c(L = 0.307, R = 0.322)
HALF_LIFE      <- 180L

park_adjusted_baseline <- function(hand, park_code) {
  pf <- PARK_FACTORS[park_code]
  if (is.na(pf)) { warning(sprintf("No park factor for '%s'", park_code)); pf <- 1.0 }
  round(HAND_BASELINES[hand] * pf, 3)
}

# Recency-weighted mix (career file)
rolling_mix <- function(df, before_date, batter_hand = NULL, hl = HALF_LIFE) {
  before_date <- as.Date(before_date)
  sub <- df |> filter(as.Date(game_date) < before_date,
                      !pitch_name %in% c("", "Unknown", NA))
  if (!is.null(batter_hand)) sub <- sub |> filter(stand == batter_hand)
  if (nrow(sub) == 0) return(list(mix = numeric(0), eff_sample = 0L))
  sub <- sub |> mutate(age = as.integer(before_date - as.Date(game_date)),
                       rw  = 0.5^(age / hl))
  w <- sub |> group_by(pitch_name) |>
    summarise(rw_sum = sum(rw), .groups = "drop") |>
    mutate(prop = rw_sum / sum(rw_sum))
  list(mix = setNames(w$prop, w$pitch_name), eff_sample = as.integer(sum(sub$rw)))
}

# Empirical blending rule (grid search April 11 2026, n=628)
# n_curr < 30 -> w=0.0 | 30-74 -> w=0.5 | 75-149 -> w=0.7 | 150+ -> w=0.9
blended_mix <- function(df, before_date, batter_hand = NULL, hl = HALF_LIFE) {
  before_date <- as.Date(before_date)
  game_year   <- format(before_date, "%Y")
  sub <- df |> filter(!pitch_name %in% c("", "Unknown", NA)) |>
    mutate(game_date = as.Date(game_date))
  if (!is.null(batter_hand)) sub <- sub |> filter(stand == batter_hand)
  curr  <- sub |> filter(game_date < before_date, format(game_date,"%Y")==game_year)
  prior <- sub |> filter(game_date < before_date, format(game_date,"%Y")!=game_year) |>
    mutate(age = as.integer(before_date - game_date), rw = 0.5^(age/hl))
  n_curr <- nrow(curr)
  w_curr <- dplyr::case_when(n_curr<30~0.0, n_curr<75~0.5, n_curr<150~0.7, TRUE~0.9)
  mix_c <- if (n_curr>0) {v<-table(curr$pitch_name)/n_curr; setNames(as.numeric(v),names(v))} else numeric(0)
  mix_p <- if (nrow(prior)>0) {
    wp <- prior |> group_by(pitch_name) |> summarise(rs=sum(rw),.groups="drop") |> mutate(p=rs/sum(rs))
    setNames(wp$p, wp$pitch_name)
  } else numeric(0)
  if (w_curr==0||length(mix_c)==0) return(list(mix=mix_p, n_curr=n_curr, w_curr=0))
  pitches <- union(names(mix_c), names(mix_p))
  bl <- sapply(pitches, function(p) w_curr*(if(!is.na(mix_c[p]))mix_c[p] else 0) +
                 (1-w_curr)*(if(!is.na(mix_p[p]))mix_p[p] else 0))
  list(mix=bl/sum(bl), n_curr=n_curr, w_curr=w_curr)
}

RANKING_TIER_THRESHOLDS <- c(HIGH=0.018, MODERATE=0.030, LOW=0.045)

compute_ranking_tiers <- function(df, conf, team_mean=NULL) {
  thresh <- RANKING_TIER_THRESHOLDS[conf]
  if (is.null(team_mean)) team_mean <- mean(df$exp_wOBA, na.rm=TRUE)
  df |> mutate(team_mean=round(team_mean,3),
               tier=dplyr::case_when(exp_wOBA>team_mean+thresh~"Edge",
                                     exp_wOBA<team_mean-thresh~"Suppressed",TRUE~"Neutral"))
}
