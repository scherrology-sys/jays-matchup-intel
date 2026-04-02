# =============================================================================
# Jays Matchup Intel · Retrospective
# 2026-04-01 · TOR vs COL · Kyle Freeland LHP
# @scherrology | Arm Chair Analyst
# =============================================================================
# Data:
#   freeland-24-26.csv             , Freeland 2024-2025 pitcher file
#   freeland-2026.csv              , Freeland 2026 start vs MIA (pitch feel baseline)
#   4-1-26.csv                     , Tonight's game Statcast pitch log
#   blue-jays-hitters-25-26.csv    , Jays 2025-2026 batter file
#   sanchez-24-26.csv              , Sánchez approach file
#   blue-jays-vs-freeland-24-26.csv, Jays vs Freeland matchup history (2024 only)
#
# Result: COL 2, TOR 1. Freeland 5 IP, 0 ER, 88 pitches.
# Jays scored their only run against the Colorado bullpen.
#
# Scope: Freeland PAs only. Relief pitchers excluded.
# Key finding: Sweeper elevated to 20.5% (proj 12.9%), best movement of 2026.
#              Threw it 60% of the time to LHH. Model flagged this vulnerability.
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
FREELAND_ID <- 607536L

# =============================================================================
# 1. LOAD
# =============================================================================
f25      <- read_csv("freeland-24-26.csv",              show_col_types=FALSE) |> filter(game_year==2025)
f26_mia  <- read_csv("freeland-2026.csv",               show_col_types=FALSE)
tonight  <- read_csv("4-1-26.csv",                      show_col_types=FALSE) |> filter(pitcher==FREELAND_ID)
jays     <- read_csv("blue-jays-hitters-25-26.csv",     show_col_types=FALSE) |> filter(game_year==2025)
sanchez  <- read_csv("sanchez-24-26.csv",               show_col_types=FALSE) |> filter(game_year==2025)
jvf      <- read_csv("blue-jays-vs-freeland-24-26.csv", show_col_types=FALSE)

cat("Freeland pitches tonight:", nrow(tonight), "\n")
cat("Max inning:", max(tonight$inning), "\n")
cat(sprintf("Score: TOR %d - COL %d\n",
    max(tonight$post_bat_score), max(tonight$post_fld_score)))

# =============================================================================
# 2. ARSENAL COMPARISON (2025 projected vs tonight actual)
# =============================================================================
proj_mix <- f25 |>
  filter(!is.na(pitch_name)) |>
  count(pitch_name) |>
  mutate(pct_proj = n / sum(n) * 100)

act_mix <- tonight |>
  filter(!is.na(pitch_name)) |>
  count(pitch_name) |>
  mutate(pct_tonight = n / sum(n) * 100)

proj_velo <- f25 |> group_by(pitch_name) |>
  summarise(velo_proj = mean(release_speed, na.rm=TRUE), .groups="drop")
act_velo  <- tonight |> group_by(pitch_name) |>
  summarise(velo_tonight = mean(release_speed, na.rm=TRUE), .groups="drop")

arsenal_comp <- proj_mix |>
  full_join(act_mix,    by="pitch_name") |>
  left_join(proj_velo,  by="pitch_name") |>
  left_join(act_velo,   by="pitch_name") |>
  replace_na(list(pct_proj=0, pct_tonight=0)) |>
  mutate(pct_delta  = round(pct_tonight - pct_proj, 1),
         velo_delta = round(velo_tonight - velo_proj, 1)) |>
  arrange(desc(pct_proj))

cat("\n=== ARSENAL COMPARISON ===\n")
print(select(arsenal_comp, pitch_name, pct_proj, pct_tonight, pct_delta,
             velo_proj, velo_tonight, velo_delta))

# Handedness splits tonight
hand_mix <- tonight |>
  filter(!is.na(pitch_name), !is.na(stand)) |>
  count(stand, pitch_name) |>
  group_by(stand) |>
  mutate(pct = n / sum(n) * 100) |>
  ungroup() |>
  arrange(stand, desc(pct))

cat("\n=== TONIGHT MIX BY HANDEDNESS ===\n")
hand_mix |>
  select(stand, pitch_name, pct) |>
  mutate(pct = round(pct, 1)) |>
  pivot_wider(names_from=stand, values_from=pct, values_fill=0) |>
  print(n=Inf)

# =============================================================================
# 3. PITCH FEEL INDICATOR
# Compare tonight's movement to 2025 baseline and 2026 MIA start.
# Sweeper is the key pitch — flag any pfx deviation ≥ 0.15.
# =============================================================================
baseline_move <- f25 |>
  group_by(pitch_name) |>
  summarise(
    pfx_x_2025 = mean(pfx_x, na.rm=TRUE),
    pfx_z_2025 = mean(pfx_z, na.rm=TRUE),
    velo_2025  = mean(release_speed, na.rm=TRUE),
    .groups    = "drop"
  )

mia_move <- f26_mia |>
  group_by(pitch_name) |>
  summarise(
    pfx_x_mia = mean(pfx_x, na.rm=TRUE),
    pfx_z_mia = mean(pfx_z, na.rm=TRUE),
    .groups   = "drop"
  )

tonight_move <- tonight |>
  group_by(pitch_name) |>
  summarise(
    pfx_x_tonight = mean(pfx_x, na.rm=TRUE),
    pfx_z_tonight = mean(pfx_z, na.rm=TRUE),
    velo_tonight  = mean(release_speed, na.rm=TRUE),
    n_tonight     = n(),
    .groups       = "drop"
  )

feel <- baseline_move |>
  inner_join(tonight_move, by="pitch_name") |>
  left_join(mia_move, by="pitch_name") |>
  mutate(
    pfx_z_delta  = round(pfx_z_tonight - pfx_z_2025, 2),
    pfx_x_delta  = round(pfx_x_tonight - pfx_x_2025, 2),
    feel_flag    = case_when(
      abs(pfx_z_delta) >= 0.15 | abs(pfx_x_delta) >= 0.20 ~ "movement changed",
      TRUE ~ "on profile"
    )
  ) |>
  arrange(desc(abs(pfx_z_delta)))

cat("\n=== PITCH FEEL INDICATORS ===\n")
print(select(feel, pitch_name, n_tonight,
             pfx_z_2025, pfx_z_tonight, pfx_z_delta,
             pfx_x_2025, pfx_x_tonight, pfx_x_delta, feel_flag))

# Key findings
sweeper_feel <- filter(feel, pitch_name=="Sweeper")
cat(sprintf("\nSweeper pfx_z: 2025 avg %.2f → tonight %.2f (delta %.2f) — %s\n",
    sweeper_feel$pfx_z_2025, sweeper_feel$pfx_z_tonight,
    sweeper_feel$pfx_z_delta, sweeper_feel$feel_flag))
cat(sprintf("Sweeper pfx_x: 2025 avg %.2f → tonight %.2f (delta %.2f)\n",
    sweeper_feel$pfx_x_2025, sweeper_feel$pfx_x_tonight, sweeper_feel$pfx_x_delta))

kc_feel <- filter(feel, pitch_name=="Knuckle Curve")
cat(sprintf("Knuckle Curve pfx_z: 2025 avg %.2f → tonight %.2f — %s\n",
    kc_feel$pfx_z_2025, kc_feel$pfx_z_tonight, kc_feel$feel_flag))

# =============================================================================
# 4. SWING / CONTACT BREAKDOWN BY PITCH
# =============================================================================
contact_summary <- tonight |>
  filter(!is.na(pitch_name)) |>
  group_by(pitch_name) |>
  summarise(
    n_pitches    = n(),
    swings       = sum(description %in% c("swinging_strike","foul","hit_into_play",
                                          "swinging_strike_blocked")),
    whiffs       = sum(description %in% c("swinging_strike","swinging_strike_blocked")),
    called_str   = sum(description == "called_strike"),
    in_play      = sum(description == "hit_into_play"),
    pa_ending    = sum(events %in% PA_EVENTS, na.rm=TRUE),
    .groups      = "drop"
  ) |>
  mutate(whiff_rate = round(whiffs / pmax(swings,1) * 100, 1)) |>
  arrange(desc(n_pitches))

cat("\n=== CONTACT BREAKDOWN BY PITCH ===\n")
print(contact_summary)

# =============================================================================
# 5. MODEL SCORECARD
# =============================================================================
posteriors <- tribble(
  ~player_name,              ~posterior, ~tier,
  "Springer, George",        0.440,      "T1",
  "Guerrero Jr., Vladimir",  0.401,      "T1",
  "Kirk, Alejandro",         0.375,      "T1",
  "Barger, Addison",         0.365,      "T2",
  "Lukes, Nathan",           0.348,      "T1",
  "Heineman, Tyler",         0.348,      "T1",
  "Sánchez, Jesús",          0.327,      "T2",
  "Schneider, Davis",        0.320,      "T1",
  "Straw, Myles",            0.318,      "T1",
  "Giménez, Andrés",         0.314,      "T2",
  "Clement, Ernie",          0.312,      "T1",
  "Varsho, Daulton",         0.295,      "T2",
  "Okamoto, Kazuma",         NA_real_,   "T3"
)

pa_tonight <- tonight |>
  filter(events %in% PA_EVENTS) |>
  mutate(
    woba_val = coalesce(WOBA_WEIGHTS[events], 0),
    woba_den = as.integer(events != "hit_by_pitch")
  )

results <- pa_tonight |>
  group_by(player_name) |>
  summarise(
    PA       = n(),
    H        = sum(events %in% c("single","double","triple","home_run")),
    BB       = sum(events == "walk"),
    K        = sum(events == "strikeout"),
    HBP      = sum(events == "hit_by_pitch"),
    woba_num = sum(woba_val),
    woba_den = sum(woba_den),
    outcomes = paste(events, collapse=", "),
    .groups  = "drop"
  ) |>
  mutate(actual_wOBA = woba_num / pmax(woba_den, 1)) |>
  left_join(posteriors, by="player_name") |>
  mutate(
    delta   = round(actual_wOBA - posterior, 3),
    verdict = case_when(
      is.na(posterior)        ~ "no model",
      PA < 2                  ~ "1 PA only",
      actual_wOBA > posterior ~ "called",
      abs(delta) <= 0.050     ~ "called",
      TRUE                    ~ "miss"
    )
  )

# Hitters who didn't face Freeland
did_not_face <- posteriors |>
  filter(!player_name %in% results$player_name) |>
  pull(player_name)

cat("\n=== MODEL SCORECARD ===\n")
results |>
  arrange(desc(posterior)) |>
  select(player_name, tier, PA, H, BB, K, actual_wOBA, posterior, delta, verdict, outcomes) |>
  mutate(across(where(is.numeric), \(x) round(x,3))) |>
  print(n=Inf)

cat("\nDid not face Freeland:", paste(did_not_face, collapse=", "), "\n")
cat("\nCalled:", sum(results$verdict=="called"))
cat("\nMisses:", sum(results$verdict=="miss"))
cat("\nNo Model:", sum(results$verdict=="no model"))
cat("\n1 PA Only:", sum(results$verdict=="1 PA only"), "\n")

# =============================================================================
# 6. KEY PITCH SEQUENCES
# =============================================================================
key_hitters <- c("Springer, George","Giménez, Andrés","Straw, Myles")

for (h in key_hitters) {
  cat(glue::glue("\n=== {h} SEQUENCES ===\n"))
  tonight |>
    filter(player_name == h) |>
    arrange(at_bat_number, pitch_number) |>
    select(at_bat_number, pitch_number, pitch_name, description,
           events, release_speed, pfx_x, pfx_z, plate_x, plate_z,
           launch_speed, launch_angle) |>
    print(n=Inf)
}

# LHH Sweeper specifically
cat("\n=== ALL LHH SWEEPER PITCHES ===\n")
tonight |>
  filter(stand=="L", pitch_name=="Sweeper") |>
  select(player_name, description, events, release_speed, pfx_x, pfx_z, plate_x, plate_z) |>
  print(n=Inf)

# =============================================================================
# 7. EXPORT
# =============================================================================
write_csv(arsenal_comp,    "retro_freeland_arsenal.csv")
write_csv(feel,            "retro_freeland_feel.csv")
write_csv(contact_summary, "retro_freeland_contact.csv")
write_csv(results,         "retro_freeland_scorecard.csv")

cat("\nAll outputs written.\n")
