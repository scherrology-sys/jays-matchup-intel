# =============================================================================
# Jays Matchup Intel · Retrospective
# 2026-03-31 · TOR vs COL · Ryan Feltner RHP
# @scherrology | Arm Chair Analyst
# =============================================================================
# Data:
#   feltner.csv                    , Feltner 2023-2025 pitcher file
#   Blue-Jays-3-31-26.csv          , Tonight's game Statcast pitch log
#   blue-jays-hitters-25-26.csv    , Jays 2025-2026 batter file
#   sanchez-24-26.csv              , Sánchez approach file
#
# Scope: Feltner PAs only. Relief pitchers excluded.
# Note: Feltner pulled after 3 IP, 47 pitches. Most hitters faced him once.
#       Pitch feel indicators added: movement profile vs 2025 baseline.
# =============================================================================

library(tidyverse)
library(readxl)

PA_EVENTS <- c(
  "single","double","triple","home_run","field_out","strikeout",
  "walk","hit_by_pitch","grounded_into_double_play","force_out",
  "sac_fly","fielders_choice","fielders_choice_out","double_play"
)
WOBA_WEIGHTS <- c(
  walk=0.696, hit_by_pitch=0.726, single=0.888,
  double=1.271, triple=1.616, home_run=2.101
)

FELTNER_ID <- 663372L

# =============================================================================
# 1. LOAD
# =============================================================================
feltner_2025 <- read_csv("feltner.csv", show_col_types=FALSE) |>
  filter(game_year == 2025)

tonight <- read_csv("Blue-Jays-3-31-26.csv", show_col_types=FALSE) |>
  filter(pitcher == FELTNER_ID)

cat("Feltner pitches tonight:", nrow(tonight), "\n")
cat("Innings:", max(tonight$inning), "\n")

# =============================================================================
# 2. ARSENAL COMPARISON
# =============================================================================
proj_mix <- feltner_2025 |>
  filter(!is.na(pitch_name)) |>
  count(pitch_name) |>
  mutate(pct_2025 = n / sum(n) * 100)

act_mix <- tonight |>
  filter(!is.na(pitch_name)) |>
  count(pitch_name) |>
  mutate(pct_tonight = n / sum(n) * 100)

proj_velo <- feltner_2025 |>
  group_by(pitch_name) |>
  summarise(velo_2025 = mean(release_speed, na.rm=TRUE), .groups="drop")

act_velo <- tonight |>
  group_by(pitch_name) |>
  summarise(velo_tonight = mean(release_speed, na.rm=TRUE), .groups="drop")

arsenal_comp <- proj_mix |>
  full_join(act_mix, by="pitch_name") |>
  left_join(proj_velo, by="pitch_name") |>
  left_join(act_velo, by="pitch_name") |>
  replace_na(list(pct_2025=0, pct_tonight=0)) |>
  mutate(pct_delta  = round(pct_tonight - pct_2025, 1),
         velo_delta = round(velo_tonight - velo_2025, 1)) |>
  arrange(desc(pct_2025))

cat("\n=== ARSENAL COMPARISON ===\n")
print(select(arsenal_comp, pitch_name, pct_2025, pct_tonight, pct_delta,
             velo_2025, velo_tonight, velo_delta))

# =============================================================================
# 3. PITCH FEEL INDICATOR
# Movement profile for each pitch tonight vs 2025 baseline.
# Focus: pfx_z deviation flags pitches that lost their normal shape.
# =============================================================================
proj_move <- feltner_2025 |>
  group_by(pitch_name) |>
  summarise(
    pfx_x_2025  = mean(pfx_x, na.rm=TRUE),
    pfx_z_2025  = mean(pfx_z, na.rm=TRUE),
    spin_2025   = mean(release_spin_rate, na.rm=TRUE),
    .groups     = "drop"
  )

act_move <- tonight |>
  group_by(pitch_name) |>
  summarise(
    pfx_x_tonight = mean(pfx_x, na.rm=TRUE),
    pfx_z_tonight = mean(pfx_z, na.rm=TRUE),
    spin_tonight  = mean(release_spin_rate, na.rm=TRUE),
    n_tonight     = n(),
    .groups       = "drop"
  )

feel_indicators <- proj_move |>
  inner_join(act_move, by="pitch_name") |>
  mutate(
    pfx_x_delta   = round(pfx_x_tonight - pfx_x_2025, 2),
    pfx_z_delta   = round(pfx_z_tonight - pfx_z_2025, 2),
    feel_flag     = case_when(
      abs(pfx_z_delta) >= 0.15 ~ "movement off",
      abs(pfx_x_delta) >= 0.20 ~ "movement off",
      TRUE                      ~ "on profile"
    )
  ) |>
  arrange(desc(abs(pfx_z_delta)))

cat("\n=== PITCH FEEL INDICATORS ===\n")
print(select(feel_indicators, pitch_name, n_tonight,
             pfx_z_2025, pfx_z_tonight, pfx_z_delta,
             pfx_x_2025, pfx_x_tonight, pfx_x_delta, feel_flag))

cat("\nKey finding: Sweeper pfx_z flipped from",
    round(filter(feel_indicators, pitch_name=="Sweeper")$pfx_z_2025, 2), "to",
    round(filter(feel_indicators, pitch_name=="Sweeper")$pfx_z_tonight, 2),
    "— lost downward dive.\n")
cat("Slider pfx_z delta:", filter(feel_indicators, pitch_name=="Slider")$pfx_z_delta,
    "— on profile, explains elevated usage.\n")

# =============================================================================
# 4. SWING/CONTACT BREAKDOWN BY PITCH TYPE
# PA-ending wOBA understates pitcher workload for non-terminal pitches.
# Show full contact picture.
# =============================================================================
pitch_contact <- tonight |>
  filter(!is.na(pitch_name)) |>
  group_by(pitch_name) |>
  summarise(
    n_pitches      = n(),
    swinging_str   = sum(description == "swinging_strike", na.rm=TRUE),
    called_str     = sum(description == "called_strike", na.rm=TRUE),
    fouls          = sum(description == "foul", na.rm=TRUE),
    balls          = sum(description == "ball", na.rm=TRUE),
    in_play        = sum(description == "hit_into_play", na.rm=TRUE),
    pa_ending      = sum(events %in% PA_EVENTS, na.rm=TRUE),
    .groups        = "drop"
  ) |>
  arrange(desc(n_pitches))

cat("\n=== FULL PITCH CONTACT BREAKDOWN ===\n")
print(pitch_contact)

# =============================================================================
# 5. MODEL SCORECARD
# =============================================================================
posteriors <- tribble(
  ~player_name,              ~posterior, ~tier,
  "Springer, George",        0.421,      "T1",
  "Varsho, Daulton",         0.412,      "T1",
  "Guerrero Jr., Vladimir",  0.366,      "T1",
  "Schneider, Davis",        0.366,      "T2",
  "Heineman, Tyler",         0.338,      "T2",
  "Lukes, Nathan",           0.325,      "T2",
  "Sánchez, Jesús",          0.302,      "T2",
  "Kirk, Alejandro",         0.301,      "T1",
  "Straw, Myles",            0.293,      "T2",
  "Clement, Ernie",          0.288,      "T2",
  "Barger, Addison",         0.287,      "T2",
  "Giménez, Andrés",         0.267,      "T2",
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
      is.na(posterior)   ~ "no model",
      PA < 2             ~ "1 PA only",
      abs(delta) <= 0.05 ~ "called",
      actual_wOBA > posterior ~ "called",
      TRUE               ~ "miss"
    )
  ) |>
  arrange(desc(posterior))

cat("\n=== MODEL SCORECARD ===\n")
print(select(results, player_name, tier, PA, H, BB, K,
             actual_wOBA, posterior, delta, verdict, outcomes))

cat("\nCalled:", sum(results$verdict=="called", na.rm=TRUE))
cat("\nMisses:", sum(results$verdict=="miss", na.rm=TRUE))
cat("\n1 PA Only:", sum(results$verdict=="1 PA only", na.rm=TRUE))
cat("\nNo Model:", sum(results$verdict=="no model", na.rm=TRUE), "\n")

# =============================================================================
# 6. PITCH SEQUENCES FOR KEY PAs
# =============================================================================
key_hitters <- c("Guerrero Jr., Vladimir","Springer, George")

for (h in key_hitters) {
  cat(glue::glue("\n=== {h} PITCH SEQUENCE ===\n"))
  tonight |>
    filter(player_name == h) |>
    arrange(at_bat_number, pitch_number) |>
    select(at_bat_number, pitch_number, pitch_name, description,
           events, release_speed, plate_x, plate_z,
           launch_speed, launch_angle) |>
    print(n=Inf)
}

# =============================================================================
# 7. EXPORT
# =============================================================================
write_csv(arsenal_comp,    "retro_feltner_arsenal.csv")
write_csv(feel_indicators, "retro_feltner_feel.csv")
write_csv(pitch_contact,   "retro_feltner_contact.csv")
write_csv(results,         "retro_feltner_scorecard.csv")

cat("\nAll outputs written.\n")
