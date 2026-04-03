# =============================================================================
# Jays Matchup Intel · Preview
# 2026-04-02 · TOR vs CWS · Sean Burke RHP (Bulk) / Grant Taylor RHP (Opener)
# Model v2: Arsenal-Weighted Prior
# @scherrology | Arm Chair Analyst
# =============================================================================
# Data:
#   savant_data__7_.csv               , Burke 2024-2026 pitcher file
#   savant_data__5_.csv               , Taylor 2025-2026 pitcher file (opener)
#   savant_data__6_.csv               , Updated Jays 2026 hitting data (3 games)
#   blue-jays-hitters-25-26-filtered.csv, Jays 2025 batter file
#   sanchez-24-26.csv                 , Sánchez approach file
#
# Game structure note:
#   Grant Taylor (RP) opens, expected ~1 inning. Sean Burke takes the bulk.
#   Model scope covers Burke PAs only. Taylor is a relief pitcher deployed
#   as opener. His inning is outside model scope by design. This is a
#   structural limitation of a starter-focused model, not an error.
#
# Key findings:
#   - Burke 2025: 4-Seam 43.1%, KC 23.7%, Slider 21.9%. Heavy Slider vs RHH
#     (35.2%), Changeup/KC vs LHH (11.5% CH). Two different pitching problems.
#   - 2026 feel: KC borderline off profile (pfx_z +0.15). All others on profile.
#   - Sinker jumped 4.8% → 12.1% in 2026, changes RHH sequencing slightly.
#   - 7 hitters have 2025 matchup history. Lukes (HR+BB) pushed up to .364.
#   - Straw drops to .290 as RHH facing 35.2% Slider diet.
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
BURKE_ID    <- 680732L
TAYLOR_ID   <- 691799L

JAYS_2026 <- c(
  "Barger, Addison","Clement, Ernie","Giménez, Andrés",
  "Guerrero Jr., Vladimir","Heineman, Tyler","Kirk, Alejandro",
  "Lukes, Nathan","Okamoto, Kazuma","Schneider, Davis",
  "Springer, George","Straw, Myles","Sánchez, Jesús","Varsho, Daulton"
)

# Burke 2025 pitch mix by handedness (exact from data)
BURKE_LHH_MIX <- c(
  "4-Seam Fastball" = 0.489,
  "Knuckle Curve"   = 0.285,
  "Changeup"        = 0.115,
  "Slider"          = 0.105,
  "Sinker"          = 0.005
)
BURKE_RHH_MIX <- c(
  "4-Seam Fastball" = 0.361,
  "Slider"          = 0.352,
  "Knuckle Curve"   = 0.180,
  "Sinker"          = 0.100,
  "Changeup"        = 0.006
)

PITCH_COLORS <- c(
  "4-Seam Fastball" = "#E8002D",
  "Knuckle Curve"   = "#60a5fa",
  "Slider"          = "#2dd4a0",
  "Changeup"        = "#fb923c",
  "Sinker"          = "#f4b942",
  "Cutter"          = "#c084fc"
)

# =============================================================================
# 1. LOAD
# =============================================================================
burke_raw   <- read_csv("savant_data__7_.csv",               show_col_types=FALSE)
taylor_raw  <- read_csv("savant_data__5_.csv",               show_col_types=FALSE)
jays_2026   <- read_csv("savant_data__6_.csv",               show_col_types=FALSE)
jays_25     <- read_csv("blue-jays-hitters-25-26-filtered.csv", show_col_types=FALSE) |>
  filter(game_year == 2025)
sanchez_raw <- read_csv("sanchez-24-26.csv",                 show_col_types=FALSE) |>
  filter(game_year == 2025)

b24 <- burke_raw |> filter(game_year == 2024)
b25 <- burke_raw |> filter(game_year == 2025)
b26 <- burke_raw |> filter(game_year == 2026)
t25 <- taylor_raw |> filter(game_year == 2025)
t26 <- taylor_raw |> filter(game_year == 2026)

cat("Burke 2024 pitches:", nrow(b24), "\n")
cat("Burke 2025 pitches:", nrow(b25), "\n")
cat("Burke 2026 pitches:", nrow(b26), "across", n_distinct(b26$game_pk), "starts\n")
cat("Taylor 2025 pitches:", nrow(t25), "\n")

# =============================================================================
# 2. BURKE ARSENAL ANALYSIS
# =============================================================================
arsenal_25 <- b25 |>
  filter(!is.na(pitch_name)) |>
  group_by(pitch_name) |>
  summarise(
    n_25       = n(),
    velo_25    = mean(release_speed, na.rm=TRUE),
    pfx_x_25   = mean(pfx_x, na.rm=TRUE),
    pfx_z_25   = mean(pfx_z, na.rm=TRUE),
    spin_25    = mean(release_spin_rate, na.rm=TRUE),
    whiff_n    = sum(description == "swinging_strike", na.rm=TRUE),
    swing_n    = sum(description %in% c("swinging_strike","foul","hit_into_play"), na.rm=TRUE),
    cs_n       = sum(description == "called_strike", na.rm=TRUE),
    xwoba_25   = mean(estimated_woba_using_speedangle, na.rm=TRUE),
    .groups    = "drop"
  ) |>
  mutate(
    pct_25     = n_25 / sum(n_25) * 100,
    whiff_rate = whiff_n / pmax(swing_n, 1) * 100,
    csw_rate   = (whiff_n + cs_n) / n_25 * 100
  )

arsenal_24 <- b24 |>
  filter(!is.na(pitch_name)) |>
  count(pitch_name) |>
  mutate(pct_24 = n / sum(n) * 100)

arsenal_26 <- b26 |>
  filter(!is.na(pitch_name)) |>
  count(pitch_name) |>
  mutate(pct_26 = n / sum(n) * 100)

arsenal_comp <- arsenal_25 |>
  left_join(arsenal_24, by="pitch_name") |>
  left_join(arsenal_26, by="pitch_name") |>
  replace_na(list(pct_24=0, pct_26=0)) |>
  mutate(
    delta_26_25 = round(pct_26 - pct_25, 1),
    delta_25_24 = round(pct_25 - pct_24, 1)
  ) |>
  arrange(desc(n_25))

cat("\n=== BURKE ARSENAL (2024 / 2025 / 2026) ===\n")
arsenal_comp |>
  select(pitch_name, pct_24, pct_25, pct_26, delta_26_25,
         velo_25, pfx_x_25, pfx_z_25, whiff_rate, csw_rate, xwoba_25) |>
  mutate(across(where(is.numeric), \(x) round(x, 2))) |>
  print(n=Inf)

# Handedness splits 2025
hand_mix <- b25 |>
  filter(!is.na(pitch_name), !is.na(stand)) |>
  count(stand, pitch_name) |>
  group_by(stand) |>
  mutate(pct = n / sum(n) * 100) |>
  ungroup() |>
  arrange(stand, desc(pct))

cat("\n=== PITCH MIX BY HANDEDNESS 2025 ===\n")
hand_mix |>
  select(stand, pitch_name, pct) |>
  mutate(pct = round(pct, 1)) |>
  pivot_wider(names_from=stand, values_from=pct, values_fill=0) |>
  arrange(desc(R)) |>
  print(n=Inf)

# Stability
game_mix <- b25 |>
  filter(!is.na(pitch_name)) |>
  count(game_pk, pitch_name) |>
  group_by(game_pk) |>
  mutate(pct = n / sum(n) * 100) |>
  ungroup()

stability <- game_mix |>
  group_by(pitch_name) |>
  summarise(
    mean_pct = mean(pct), sd_pct = sd(pct), n_games = n(), .groups = "drop"
  ) |>
  mutate(
    cv   = round(sd_pct / mean_pct, 3),
    flag = case_when(cv >= 0.5 ~ "volatile", cv >= 0.25 ~ "moderate", TRUE ~ "stable")
  ) |>
  arrange(desc(cv))

cat("\n=== ARSENAL STABILITY ===\n")
stability |> mutate(across(where(is.numeric), \(x) round(x, 3))) |> print(n=Inf)

# Zone frequencies
cat("\n=== ZONE FREQUENCIES 2025 ===\n")
cat("LHH:", deparse(as.list(sort(table(b25[b25$stand=="L","zone"]), decreasing=TRUE))), "\n")
cat("RHH:", deparse(as.list(sort(table(b25[b25$stand=="R","zone"]), decreasing=TRUE))), "\n")

# =============================================================================
# 3. PITCH FEEL INDICATORS (2026 vs 2025)
# =============================================================================
proj_move <- b25 |>
  group_by(pitch_name) |>
  summarise(
    pfx_x_25 = mean(pfx_x, na.rm=TRUE),
    pfx_z_25 = mean(pfx_z, na.rm=TRUE),
    velo_25  = mean(release_speed, na.rm=TRUE),
    .groups  = "drop"
  )

act_move_26 <- b26 |>
  group_by(pitch_name) |>
  summarise(
    pfx_x_26 = mean(pfx_x, na.rm=TRUE),
    pfx_z_26 = mean(pfx_z, na.rm=TRUE),
    velo_26  = mean(release_speed, na.rm=TRUE),
    n_26     = n(),
    .groups  = "drop"
  )

feel <- proj_move |>
  inner_join(act_move_26, by="pitch_name") |>
  mutate(
    pfx_z_delta = round(pfx_z_26 - pfx_z_25, 2),
    pfx_x_delta = round(pfx_x_26 - pfx_x_25, 2),
    velo_delta  = round(velo_26 - velo_25, 1),
    feel_flag   = case_when(
      abs(pfx_z_delta) >= 0.15 | abs(pfx_x_delta) >= 0.20 ~ "off profile",
      TRUE ~ "on profile"
    )
  ) |>
  arrange(desc(abs(pfx_z_delta)))

cat("\n=== PITCH FEEL INDICATORS (2026 vs 2025) ===\n")
print(select(feel, pitch_name, n_26, pfx_z_25, pfx_z_26, pfx_z_delta,
             pfx_x_25, pfx_x_26, pfx_x_delta, velo_delta, feel_flag))

kc <- filter(feel, pitch_name == "Knuckle Curve")
cat(sprintf("\nKnuckle Curve flag: pfx_z %.2f → %.2f (delta %.2f) — %s\n",
    kc$pfx_z_25, kc$pfx_z_26, kc$pfx_z_delta, kc$feel_flag))
cat("Sinker 2025 usage: 4.8% → 2026: 12.1% · RHH sequencing note\n")

# =============================================================================
# 4. TAYLOR OPENER BRIEF PROFILE
# =============================================================================
cat("\n=== TAYLOR OPENER PROFILE (2025 reference, not modeled) ===\n")
t25_arsenal <- t25 |>
  filter(!is.na(pitch_name)) |>
  count(pitch_name) |>
  mutate(pct = n / sum(n) * 100)

t25_stats <- t25 |>
  filter(!is.na(pitch_name)) |>
  group_by(pitch_name) |>
  summarise(
    velo      = mean(release_speed, na.rm=TRUE),
    whiff_n   = sum(description == "swinging_strike", na.rm=TRUE),
    swing_n   = sum(description %in% c("swinging_strike","foul","hit_into_play"), na.rm=TRUE),
    xwoba     = mean(estimated_woba_using_speedangle, na.rm=TRUE),
    .groups   = "drop"
  ) |>
  mutate(whiff_rate = whiff_n / pmax(swing_n, 1) * 100)

t25_full <- t25_arsenal |> left_join(t25_stats, by="pitch_name")
cat("Taylor 4-Seam velo:", round(filter(t25_full, pitch_name=="4-Seam Fastball")$velo, 1), "mph\n")
cat("Taylor Curveball whiff rate:", round(filter(t25_full, pitch_name=="Curveball")$whiff_rate, 1), "%\n")
print(select(t25_full, pitch_name, pct, velo, whiff_rate, xwoba) |>
        mutate(across(where(is.numeric), \(x) round(x, 2))) |>
        arrange(desc(pct)))

# =============================================================================
# 5. JAYS VS BURKE MATCHUP HISTORY
# =============================================================================
all_jays <- bind_rows(jays_25, jays_2026, sanchez_raw) |> distinct()
jvb <- all_jays |> filter(pitcher == BURKE_ID, player_name %in% JAYS_2026)

cat(sprintf("\nJays vs Burke pitches: %d\n", nrow(jvb)))
cat("Hitters with history:", paste(unique(jvb$player_name), collapse=", "), "\n")
cat("No history:", paste(setdiff(JAYS_2026, unique(jvb$player_name)), collapse=", "), "\n")

pa_jvb <- jvb |>
  filter(events %in% PA_EVENTS) |>
  mutate(
    woba_val = coalesce(WOBA_WEIGHTS[events], 0),
    woba_den = as.integer(events != "hit_by_pitch")
  )

matchup_summary <- pa_jvb |>
  group_by(player_name) |>
  summarise(
    PA       = n(),
    H        = sum(events %in% c("single","double","triple","home_run")),
    HR       = sum(events == "home_run"),
    BB       = sum(events == "walk"),
    K        = sum(events == "strikeout"),
    woba_num = sum(woba_val),
    woba_den = sum(woba_den),
    wOBA     = woba_num / pmax(woba_den, 1),
    outcomes = paste(events, collapse=", "),
    .groups  = "drop"
  )

cat("\n=== MATCHUP SUMMARY ===\n")
matchup_summary |>
  select(player_name, PA, H, HR, BB, K, wOBA, outcomes) |>
  mutate(wOBA = round(wOBA, 3)) |>
  arrange(desc(wOBA)) |>
  print(n=Inf)

# =============================================================================
# 6. BAYESIAN MODEL
# =============================================================================
pa_2025 <- bind_rows(jays_25, sanchez_raw) |>
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
  mix <- if (hand_val == "L") BURKE_LHH_MIX else BURKE_RHH_MIX
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
  left_join(hitter_hand, by="player_name") |>
  mutate(
    hand         = replace_na(hand, "R"),
    wOBA_arsenal = map2_dbl(player_name, hand,
      ~compute_arsenal_prior(.x, .y,
        overall_prior$wOBA_overall[overall_prior$player_name == .x])),
    prior_delta  = round(wOBA_arsenal - wOBA_overall, 3)
  )

obs_tbl <- matchup_summary |>
  select(player_name, n_PA = PA, m_wn = woba_num, m_wd = woba_den)

bayes_df <- priors |>
  left_join(obs_tbl, by="player_name") |>
  replace_na(list(n_PA = 0, m_wn = 0, m_wd = 0)) |>
  mutate(
    alpha_0    = wOBA_arsenal * K_PRIOR,
    beta_0     = (1 - wOBA_arsenal) * K_PRIOR,
    alpha_post = alpha_0 + m_wn,
    beta_post  = beta_0  + (m_wd - m_wn),
    wOBA_post  = alpha_post / (alpha_post + beta_post),
    CI_lo_90   = qbeta(0.05, alpha_post, beta_post),
    CI_hi_90   = qbeta(0.95, alpha_post, beta_post),
    tier       = case_when(
      player_name == "Okamoto, Kazuma" ~ "T3",
      n_PA >= 2                        ~ "T1",
      TRUE                             ~ "T2"
    )
  ) |>
  arrange(desc(wOBA_post))

cat("\n=== BAYESIAN POSTERIORS (BURKE, MODEL V2) ===\n")
bayes_df |>
  select(player_name, hand, tier, PA_2025, wOBA_overall, wOBA_arsenal,
         prior_delta, n_PA, wOBA_post, CI_lo_90, CI_hi_90) |>
  mutate(across(where(is.numeric), \(x) round(x, 3))) |>
  print(n=Inf)

# 2026 early Jays data — context only, not used in prior
cat("\n=== 2026 EARLY JAYS DATA (3 games, context only) ===\n")
jays_2026 |>
  filter(events %in% PA_EVENTS, player_name %in% JAYS_2026) |>
  mutate(
    wv = coalesce(WOBA_WEIGHTS[events], 0),
    wd = as.integer(events != "hit_by_pitch")
  ) |>
  group_by(player_name) |>
  summarise(PA_2026 = n(), wOBA_2026 = sum(wv)/sum(wd), .groups="drop") |>
  mutate(wOBA_2026 = round(wOBA_2026, 3)) |>
  arrange(desc(PA_2026)) |>
  print(n=Inf)

# =============================================================================
# 7. EXPORT
# =============================================================================
write_csv(arsenal_comp,    "burke_output_arsenal.csv")
write_csv(feel,            "burke_output_feel.csv")
write_csv(hand_mix,        "burke_output_hand_mix.csv")
write_csv(stability,       "burke_output_stability.csv")
write_csv(matchup_summary, "burke_output_matchup_2025.csv")
write_csv(bayes_df,        "burke_output_posteriors.csv")

cat("\nAll outputs written.\n")
cat("Note: Taylor opener profile is printed above for reference only.\n")
cat("Model scope: Burke PAs only. Taylor's opener inning excluded.\n")
