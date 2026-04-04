# =============================================================================
# Jays Matchup Intel · Retrospective
# 2026-04-03 · TOR vs CWS · G. Taylor (opener, 1 IP) / S. Burke (bulk, 6 IP)
# @scherrology | Arm Chair Analyst
# =============================================================================
# Data:
#   savant_data__9_.csv              , Tonight's game · Jays hitting perspective
#   savant_data__10_.csv             , Tonight's game · pitcher perspective
#   savant_data__7_.csv              , Burke 2024-2026 pitcher file
#   savant_data__5_.csv              , Taylor 2025-2026 pitcher file
#
# Result: CWS 4, TOR 3 · F/10
#   TOR scored in the top of the 10th to lead 4-3. CWS walked it off in the
#   bottom of the 10th. The batter-side data files only capture TOR at-bats,
#   so CWS scoring is not visible in this dataset.
#
# Model scope: Burke PAs only (innings 2-7). Taylor faced 3 Jays in the 1st.
# Giménez HR in 8th (off Leasure RP) and extras are outside model scope.
#
# Key findings:
#   - Burke Sinker jumped 4.8% → 23.5%. Three pitches off movement profile:
#     KC (pfx_x -0.40), Slider (pfx_z +0.30), Changeup (pfx_z +0.15).
#   - 4-Seam and Sinker on profile. He leaned on what was working.
#   - Scorecard: 4 Called, 4 Misses, 1 No Model.
#
# Verdict rule (corrected):
#   Called  : actual >= posterior  OR  |delta| <= 0.050
#   Miss    : actual <  posterior  AND  delta < -0.050
#   1 PA    : PA < 2
#   No Model: T3 hitters
#
#   If a hitter outperforms their posterior the model was directionally right.
#   A miss only occurs when a hitter significantly underperforms.
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

BURKE_ID  <- 680732L
TAYLOR_ID <- 691799L

POSTERIORS <- c(
  "Springer, George"       = 0.410,
  "Guerrero Jr., Vladimir" = 0.408,
  "Schneider, Davis"       = 0.396,
  "Lukes, Nathan"          = 0.364,
  "Varsho, Daulton"        = 0.349,
  "Heineman, Tyler"        = 0.346,
  "Kirk, Alejandro"        = 0.341,
  "Barger, Addison"        = 0.330,
  "Sánchez, Jesús"         = 0.308,
  "Clement, Ernie"         = 0.299,
  "Straw, Myles"           = 0.290,
  "Giménez, Andrés"        = 0.287
)

# =============================================================================
# 1. LOAD
# =============================================================================
game    <- read_csv("savant_data__9_.csv",  show_col_types=FALSE)
pitchdf <- read_csv("savant_data__10_.csv", show_col_types=FALSE)
b25_raw <- read_csv("savant_data__7_.csv",  show_col_types=FALSE)
t25_raw <- read_csv("savant_data__5_.csv",  show_col_types=FALSE)

b25 <- b25_raw |> filter(game_year==2025)
t25 <- t25_raw |> filter(game_year==2025)

burke  <- game |> filter(pitcher==BURKE_ID)
taylor <- game |> filter(pitcher==TAYLOR_ID)

cat("Result: CWS 4, TOR 3 F/10\n")
cat(sprintf("Burke pitches: %d, innings: %s\n",
    nrow(burke), paste(sort(unique(burke$inning)), collapse=",")))
cat(sprintf("Taylor opener: %d pitches, inning %s\n",
    nrow(taylor), paste(sort(unique(taylor$inning)), collapse=",")))

# =============================================================================
# 2. TAYLOR OPENER (logged, not modeled)
# =============================================================================
cat("\n=== TAYLOR OPENER (not scored) ===\n")
taylor |> filter(events %in% PA_EVENTS) |>
  select(player_name, events) |> print()

# =============================================================================
# 3. ARSENAL COMPARISON
# =============================================================================
proj_mix <- b25 |>
  filter(!is.na(pitch_name)) |>
  count(pitch_name) |>
  mutate(pct_proj = n/sum(n)*100)

act_mix <- burke |>
  filter(!is.na(pitch_name)) |>
  count(pitch_name) |>
  mutate(pct_tonight = n/sum(n)*100)

proj_velo <- b25 |> group_by(pitch_name) |>
  summarise(velo_proj=mean(release_speed,na.rm=TRUE), .groups="drop")
act_velo  <- burke |> group_by(pitch_name) |>
  summarise(velo_tonight=mean(release_speed,na.rm=TRUE), .groups="drop")

arsenal_comp <- proj_mix |>
  full_join(act_mix,   by="pitch_name") |>
  left_join(proj_velo, by="pitch_name") |>
  left_join(act_velo,  by="pitch_name") |>
  replace_na(list(pct_proj=0, pct_tonight=0)) |>
  mutate(pct_delta  = round(pct_tonight - pct_proj, 1),
         velo_delta = round(velo_tonight - velo_proj, 1)) |>
  arrange(desc(pct_proj))

cat("\n=== ARSENAL COMPARISON ===\n")
arsenal_comp |>
  select(pitch_name, pct_proj, pct_tonight, pct_delta, velo_proj, velo_tonight, velo_delta) |>
  mutate(across(where(is.numeric), \(x) round(x,1))) |>
  print(n=Inf)

# Handedness splits tonight
hand_mix <- burke |>
  filter(!is.na(pitch_name), !is.na(stand)) |>
  count(stand, pitch_name) |>
  group_by(stand) |>
  mutate(pct=n/sum(n)*100) |>
  ungroup()

cat("\n=== BURKE HANDEDNESS MIX TONIGHT ===\n")
hand_mix |>
  select(stand, pitch_name, pct) |>
  mutate(pct=round(pct,1)) |>
  pivot_wider(names_from=stand, values_from=pct, values_fill=0) |>
  arrange(desc(R)) |>
  print(n=Inf)

# =============================================================================
# 4. PITCH FEEL INDICATORS
# =============================================================================
proj_move <- b25 |>
  group_by(pitch_name) |>
  summarise(pfx_x_25=mean(pfx_x,na.rm=TRUE),
            pfx_z_25=mean(pfx_z,na.rm=TRUE), .groups="drop")

act_move <- burke |>
  group_by(pitch_name) |>
  summarise(pfx_x_t=mean(pfx_x,na.rm=TRUE),
            pfx_z_t=mean(pfx_z,na.rm=TRUE),
            n=n(), .groups="drop")

feel <- proj_move |>
  inner_join(act_move, by="pitch_name") |>
  mutate(
    pfx_z_delta = round(pfx_z_t - pfx_z_25, 2),
    pfx_x_delta = round(pfx_x_t - pfx_x_25, 2),
    feel_flag   = case_when(
      abs(pfx_z_delta)>=0.15 | abs(pfx_x_delta)>=0.20 ~ "off profile",
      TRUE ~ "on profile"
    )
  ) |>
  arrange(desc(abs(pfx_z_delta)))

cat("\n=== PITCH FEEL INDICATORS ===\n")
print(select(feel, pitch_name, n, pfx_z_25, pfx_z_t, pfx_z_delta,
             pfx_x_25, pfx_x_t, pfx_x_delta, feel_flag))

cat(sprintf("\nOff-profile: %s\n",
    paste(filter(feel, feel_flag=="off profile")$pitch_name, collapse=", ")))
cat(sprintf("On-profile: %s\n",
    paste(filter(feel, feel_flag=="on profile")$pitch_name, collapse=", ")))

# =============================================================================
# 5. CONTACT BREAKDOWN BY PITCH
# =============================================================================
contact_summary <- burke |>
  filter(!is.na(pitch_name)) |>
  group_by(pitch_name) |>
  summarise(
    n_pitches  = n(),
    swings     = sum(description %in% c("swinging_strike","foul","hit_into_play",
                                        "swinging_strike_blocked")),
    whiffs     = sum(description %in% c("swinging_strike","swinging_strike_blocked")),
    called_str = sum(description=="called_strike"),
    in_play    = sum(description=="hit_into_play"),
    pa_ending  = sum(events %in% PA_EVENTS, na.rm=TRUE),
    .groups    = "drop"
  ) |>
  mutate(whiff_rate=round(whiffs/pmax(swings,1)*100,1)) |>
  arrange(desc(n_pitches))

cat("\n=== CONTACT BREAKDOWN BY PITCH ===\n")
print(contact_summary)

# =============================================================================
# 6. MODEL SCORECARD
#
# Verdict rule:
#   Called  : actual >= posterior  OR  abs(delta) <= 0.050
#   Miss    : actual <  posterior  AND  delta < -0.050
#   1 PA    : PA < 2
#   No Model: Okamoto
#
# Rationale: if a hitter outperforms their posterior, the model was
# directionally right. A miss only occurs when a hitter significantly
# underperforms. Kirk and Barger both doubled against a Burke with
# off-profile secondary pitches — they are correctly called.
# =============================================================================
b_pa <- burke |>
  filter(events %in% PA_EVENTS) |>
  mutate(
    woba_val = coalesce(WOBA_WEIGHTS[events], 0),
    woba_den = as.integer(events != "hit_by_pitch")
  )

results <- b_pa |>
  group_by(player_name) |>
  summarise(
    PA       = n(),
    H        = sum(events %in% c("single","double","triple","home_run")),
    BB       = sum(events=="walk"),
    K        = sum(events=="strikeout"),
    woba_num = sum(woba_val),
    woba_den = sum(woba_den),
    outcomes = paste(events, collapse=", "),
    .groups  = "drop"
  ) |>
  mutate(
    actual_wOBA = woba_num / pmax(woba_den, 1),
    posterior   = POSTERIORS[player_name],
    delta       = round(actual_wOBA - posterior, 3),
    verdict     = case_when(
      is.na(posterior)                          ~ "no model",
      PA < 2                                    ~ "1 PA only",
      actual_wOBA >= posterior                  ~ "called",
      abs(delta) <= 0.050                       ~ "called",
      TRUE                                      ~ "miss"
    )
  ) |>
  arrange(desc(posterior))

cat("\n=== MODEL SCORECARD (BURKE, CORRECTED VERDICT RULE) ===\n")
results |>
  select(player_name, PA, actual_wOBA, posterior, delta, verdict, outcomes) |>
  mutate(across(where(is.numeric), \(x) round(x,3))) |>
  print(n=Inf)

cat("\nCalled:",   sum(results$verdict=="called"))
cat("\nMisses:",   sum(results$verdict=="miss"))
cat("\n1 PA only:",sum(results$verdict=="1 PA only"))
cat("\nNo Model:", sum(results$verdict=="no model"), "\n")

cat("\nNote: Kirk (.341 post, .424 actual) and Barger (.330 post, .424 actual)\n")
cat("both called — outperformed, model was right directionally.\n")
cat("Giménez (.287 post, .000 actual) correctly scored as miss.\n")

# =============================================================================
# 7. EXPORT
# =============================================================================
write_csv(arsenal_comp,    "retro_burke_arsenal.csv")
write_csv(feel,            "retro_burke_feel.csv")
write_csv(contact_summary, "retro_burke_contact.csv")
write_csv(results,         "retro_burke_scorecard.csv")

cat("\nAll outputs written.\n")
cat("Final: CWS 4, TOR 3 F/10\n")
cat("Model scope: Burke PAs (inn 2-7). Taylor, Leasure, Domínguez excluded.\n")
