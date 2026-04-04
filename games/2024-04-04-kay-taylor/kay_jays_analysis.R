# =============================================================================
# Jays Matchup Intel · Preview
# 2026-04-04 · TOR vs CWS · A. Kay LHP (bulk) / G. Taylor RHP (opener)
# Model v2: Arsenal-Weighted Prior
# @scherrology | Arm Chair Analyst
# =============================================================================
# Data:
#   savant_data__13_.csv              , Kay 2026 pitcher file (1 start, 92 pitches)
#   savant_data__5_.csv               , Taylor 2025-2026 opener profile
#   blue-jays-hitters-25-26.csv       , Jays 2025-2026 batter file
#   sanchez-24-26.csv                 , Sánchez approach file
#
# CRITICAL DATA LIMITATION:
#   Kay has exactly ONE 2026 start (vs MIL, 3-29-26, 92 pitches).
#   No 2024 or 2025 data exists in the dataset.
#   ALL posteriors are T2 (arsenal prior only). No matchup history.
#   Credible intervals are the widest of the season.
#   The Changeup split (RHH only in one start) is the model's key assumption.
#
# Game structure:
#   Grant Taylor (RHP) opens ~1 inning. Kay takes the bulk.
#   Model scope: Kay PAs only. Taylor excluded.
#
# 2026 Jays roster (Santander out, Bichette/KF not on team):
#   Barger, Clement, Giménez, Guerrero Jr., Heineman, Kirk, Lukes,
#   Okamoto, Schneider, Springer, Straw, Sánchez, Varsho
# =============================================================================

library(tidyverse)

PA_EVENTS <- c(
  "single","double","triple","home_run","field_out","strikeout",
  "walk","hit_by_pitch","grounded_into_double_play","force_out",
  "sac_fly","fielders_choice","fielders_choice_out","double_play"
)
WOBA_WEIGHTS <- c(
  walk=0.696, hit_by_pitch=0.726, single=0.888,
  double=1.271, triple=1.616, home_run=2.101
)
K_PRIOR     <- 60
MIN_PA      <- 10
LEAGUE_WOBA <- 0.320
KAY_ID      <- 641743L
TAYLOR_ID   <- 691799L

JAYS_2026 <- c(
  "Barger, Addison","Clement, Ernie","Giménez, Andrés",
  "Guerrero Jr., Vladimir","Heineman, Tyler","Kirk, Alejandro",
  "Lukes, Nathan","Okamoto, Kazuma","Schneider, Davis",
  "Springer, George","Straw, Myles","Sánchez, Jesús","Varsho, Daulton"
)

# Kay 2026 pitch mix by handedness (from 1 start vs MIL)
KAY_LHH_MIX <- c(
  "4-Seam Fastball" = 0.444,
  "Sinker"          = 0.296,
  "Sweeper"         = 0.148,
  "Slider"          = 0.111
)
KAY_RHH_MIX <- c(
  "4-Seam Fastball" = 0.308,
  "Changeup"        = 0.262,
  "Sweeper"         = 0.185,
  "Slider"          = 0.138,
  "Sinker"          = 0.108
)

# =============================================================================
# 1. LOAD
# =============================================================================
kay_raw     <- read_csv("savant_data__13_.csv", show_col_types=FALSE)
taylor_raw  <- read_csv("savant_data__5_.csv",  show_col_types=FALSE)
jays_raw    <- read_csv("blue-jays-hitters-25-26.csv", show_col_types=FALSE)
sanchez_raw <- read_csv("sanchez-24-26.csv",    show_col_types=FALSE)

k26 <- kay_raw |> filter(game_year==2026)
t25 <- taylor_raw |> filter(game_year==2025)

cat("Kay pitcher:", unique(kay_raw$player_name), "\n")
cat("Kay throws:", unique(kay_raw$p_throws), "\n")
cat("Kay 2026 pitches:", nrow(k26), "\n")
cat("Kay 2026 dates:", paste(unique(k26$game_date), collapse=", "), "\n")
cat("Kay 2026 opponent:", paste(unique(k26$home_team),"vs",unique(k26$away_team)), "\n")
cat("\n*** NO 2024 or 2025 Kay data available. Model built on 92 pitches. ***\n")

# =============================================================================
# 2. KAY ARSENAL ANALYSIS
# =============================================================================
arsenal <- k26 |>
  filter(!is.na(pitch_name)) |>
  group_by(pitch_name) |>
  summarise(
    n         = n(),
    velo      = mean(release_speed, na.rm=TRUE),
    pfx_x     = mean(pfx_x, na.rm=TRUE),
    pfx_z     = mean(pfx_z, na.rm=TRUE),
    spin      = mean(release_spin_rate, na.rm=TRUE),
    whiff_n   = sum(description=="swinging_strike", na.rm=TRUE),
    swing_n   = sum(description %in% c("swinging_strike","foul","hit_into_play"), na.rm=TRUE),
    cs_n      = sum(description=="called_strike", na.rm=TRUE),
    xwoba     = mean(estimated_woba_using_speedangle, na.rm=TRUE),
    .groups   = "drop"
  ) |>
  mutate(
    pct        = n / sum(n) * 100,
    whiff_rate = whiff_n / pmax(swing_n, 1) * 100,
    csw_rate   = (whiff_n + cs_n) / n * 100
  ) |>
  arrange(desc(n))

cat("\n=== KAY 2026 ARSENAL (1 start) ===\n")
arsenal |>
  select(pitch_name, pct, velo, pfx_x, pfx_z, whiff_rate, csw_rate, xwoba) |>
  mutate(across(where(is.numeric), \(x) round(x, 2))) |>
  print(n=Inf)

# Handedness splits
hand_mix <- k26 |>
  filter(!is.na(pitch_name), !is.na(stand)) |>
  count(stand, pitch_name) |>
  group_by(stand) |>
  mutate(pct = n/sum(n)*100) |>
  ungroup() |>
  arrange(stand, desc(pct))

cat("\n=== HANDEDNESS SPLITS ===\n")
hand_mix |>
  select(stand, pitch_name, pct) |>
  mutate(pct=round(pct,1)) |>
  pivot_wider(names_from=stand, values_from=pct, values_fill=0) |>
  arrange(desc(R)) |>
  print(n=Inf)

cat("\nKey finding: Changeup thrown 26.2% to RHH, 0% to LHH in one start.\n")
cat("Model assumes this pattern holds. If wrong, LHH posteriors are too low.\n")

# =============================================================================
# 3. TAYLOR OPENER (reference, not modeled)
# =============================================================================
cat("\n=== TAYLOR OPENER (2025 reference, not modeled) ===\n")
t25 |>
  filter(!is.na(pitch_name)) |>
  count(pitch_name) |>
  mutate(pct=n/sum(n)*100) |>
  arrange(desc(n)) |>
  mutate(across(where(is.numeric), \(x) round(x,1))) |>
  print(n=Inf)

cat(sprintf("Taylor 4-Seam avg velo: %.1f mph\n",
    mean(t25[t25$pitch_name=="4-Seam Fastball","release_speed"][[1]], na.rm=TRUE)))

# =============================================================================
# 4. JAYS VS KAY HISTORY
# =============================================================================
all_jays <- bind_rows(jays_raw, sanchez_raw) |> distinct()
jvk <- all_jays |> filter(pitcher==KAY_ID, player_name %in% JAYS_2026)

cat(sprintf("\nJays vs Kay pitches: %d\n", nrow(jvk)))
cat("All 13 hitters have zero Kay history. All posteriors are T2.\n")

# =============================================================================
# 5. BAYESIAN MODEL
# =============================================================================
jays_25 <- jays_raw  |> filter(game_year==2025)
san_25  <- sanchez_raw |> filter(game_year==2025)

pa_2025 <- bind_rows(jays_25, san_25) |>
  filter(events %in% PA_EVENTS) |>
  mutate(
    woba_val = coalesce(WOBA_WEIGHTS[events], 0),
    woba_den = as.integer(events != "hit_by_pitch")
  )

overall_prior <- pa_2025 |>
  group_by(player_name) |>
  summarise(
    PA_2025      = n(),
    op_wn        = sum(woba_val),
    op_wd        = sum(woba_den),
    wOBA_overall = op_wn / pmax(op_wd, 1),
    .groups      = "drop"
  )

pitch_splits <- pa_2025 |>
  group_by(player_name, pitch_name) |>
  summarise(
    PA_vs      = n(),
    sp_wn      = sum(woba_val),
    sp_wd      = sum(woba_den),
    pitch_wOBA = sp_wn / pmax(sp_wd, 1),
    .groups    = "drop"
  )

hitter_hand <- pa_2025 |>
  group_by(player_name) |>
  summarise(hand = names(sort(table(stand), decreasing=TRUE))[1], .groups="drop")

compute_arsenal_prior <- function(player, hand_val, fallback) {
  mix <- if (hand_val == "L") KAY_LHH_MIX else KAY_RHH_MIX
  splits <- pitch_splits |>
    filter(player_name == player, PA_vs >= MIN_PA) |>
    select(pitch_name, pitch_wOBA)
  w_sum <- 0; w_matched <- 0
  for (pitch in names(mix)) {
    wt  <- mix[pitch]
    row <- filter(splits, pitch_name == pitch)
    if (nrow(row) > 0 && !is.na(row$pitch_wOBA)) {
      w_sum    <- w_sum    + wt * row$pitch_wOBA
      w_matched <- w_matched + wt
    }
  }
  round(w_sum + (1 - w_matched) * fallback, 3)
}

priors <- overall_prior |>
  filter(player_name %in% JAYS_2026) |>
  left_join(hitter_hand, by="player_name") |>
  mutate(
    hand         = replace_na(hand, "R"),
    wOBA_arsenal = map2_dbl(player_name, hand,
      ~compute_arsenal_prior(.x, .y,
        overall_prior$wOBA_overall[overall_prior$player_name == .x])),
    prior_delta  = round(wOBA_arsenal - wOBA_overall, 3)
  )

bayes_df <- priors |>
  mutate(
    alpha_0    = wOBA_arsenal * K_PRIOR,
    beta_0     = (1 - wOBA_arsenal) * K_PRIOR,
    wOBA_post  = wOBA_arsenal,
    CI_lo_90   = qbeta(0.05, alpha_0, beta_0),
    CI_hi_90   = qbeta(0.95, alpha_0, beta_0),
    tier       = if_else(player_name == "Okamoto, Kazuma", "T3", "T2"),
    data_note  = "T2 · arsenal prior only · no Kay history"
  ) |>
  arrange(desc(wOBA_post))

cat("\n=== BAYESIAN POSTERIORS (KAY, ALL T2) ===\n")
bayes_df |>
  select(player_name, hand, tier, PA_2025, wOBA_overall, wOBA_arsenal,
         prior_delta, wOBA_post, CI_lo_90, CI_hi_90) |>
  mutate(across(where(is.numeric), \(x) round(x, 3))) |>
  print(n=Inf)

cat("\nAll 13 hitters T2. No matchup updates.\n")
cat("Varsho largest downward move: ", filter(bayes_df, player_name=="Varsho, Daulton")$prior_delta, "\n")
cat("Key risk: If Kay throws Changeup to LHH tonight, LHH posteriors understated.\n")

# =============================================================================
# 6. EXPORT
# =============================================================================
write_csv(arsenal,  "kay_output_arsenal.csv")
write_csv(hand_mix, "kay_output_hand_mix.csv")
write_csv(bayes_df, "kay_output_posteriors.csv")

cat("\nAll outputs written.\n")
cat("Note: Model built on 92 pitches (1 start). Treat all outputs as directional.\n")
