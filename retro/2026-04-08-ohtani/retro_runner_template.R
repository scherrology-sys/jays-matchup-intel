# =============================================================================
# Jays Matchup Intel
# Retrospective Runner — Template
# =============================================================================
#
# USAGE
# -----
# Copy this file and rename:
#   retro_runner_YYYY-MM-DD_lastname.R
#
# Fill in the GAME CONFIGURATION section below.
# Upload the batting and pitching Statcast files for the game.
# Source jays_matchup_intel_self_learning.R first.
#
# H1: Pitch-mix weighted model produces more accurate team wOBA estimates
#     than a naive hitter-baseline ignoring the pitcher.
# H2: In-season updating improves estimates over the static prior. (Silent.)
#
# MODEL LOCK: April 7, 2026. No parameter changes based on retro results.
# =============================================================================

library(tidyverse)

# =============================================================================
# GAME CONFIGURATION — fill in before running
# =============================================================================

PITCHER_NAME <- "Last, First"          # e.g. "Burnes, Corbin"
PITCHER_ID   <- 000000L                # Statcast pitcher ID
PITCHER_HAND <- "R"                    # "L" or "R"
GAME_DATE    <- "2026-MM-DD"
PARK         <- "TOR"                  # home team abbreviation
OBS_N        <- 0L                     # observation number in study
RESULT       <- "TOR X, OPP Y"        # confirmed final score
PITCHER_LINE <- "X IP Y ER Z K"       # confirmed line — verify before logging
DATA_SOURCE  <- "career_file"          # "career_file" or "WS_BvP_only" or "limited"
CONFIDENCE   <- "MODERATE"             # HIGH / MODERATE / LOW

# Files to upload:
#   Jays batting Statcast CSV  → jays_batting_YYYY-MM-DD.csv
#   Dodgers/Opp pitching CSV   → opp_pitching_YYYY-MM-DD.csv
#
# NOTE: Use the PITCHING file for per-hitter actuals.
# player_name in the pitching file = batter name (correct).
# player_name in the batting file = pitcher name (wrong source for scoring).

jays_bat  <- safe_read_csv("jays_batting_YYYY-MM-DD.csv")
opp_pitch <- safe_read_csv("opp_pitching_YYYY-MM-DD.csv")

pitcher_game <- opp_pitch |> filter(pitcher == PITCHER_ID)
cat(sprintf("Pitcher: %s  Pitches: %d\n", PITCHER_NAME, nrow(pitcher_game)))

# =============================================================================
# 1. ACTUAL PITCH MIX
# =============================================================================

cat("\n=== ACTUAL PITCH MIX ===\n")
pitcher_game |> count(pitch_name) |>
  mutate(pct=round(n/sum(n)*100,1)) |> arrange(desc(pct)) |> print()

cat("\nvs LHH:\n")
pitcher_game |> filter(stand=="L") |> count(pitch_name) |>
  mutate(pct=round(n/sum(n)*100,1)) |> arrange(desc(pct)) |> print()

cat("\nvs RHH:\n")
pitcher_game |> filter(stand=="R") |> count(pitch_name) |>
  mutate(pct=round(n/sum(n)*100,1)) |> arrange(desc(pct)) |> print()

pitcher_game |>
  filter(!is.na(release_speed), !pitch_name %in% c("Unknown","")) |>
  group_by(pitch_name) |>
  summarise(velo=round(mean(release_speed,na.rm=TRUE),1), n=n(), .groups="drop") |>
  filter(n>=5) |> arrange(desc(velo)) |> print()

# =============================================================================
# 2. CHAMPION PREDICTIONS
# Fill in from the preview runner output — do not adjust post-game.
# =============================================================================

PREDICTIONS <- tribble(
  ~player_name,              ~hand, ~tier,        ~exp_wOBA, ~prior_wOBA,
  # "Springer, George",      "R",   "Edge",        0.000,     0.000,
  # "Guerrero Jr., Vladimir","R",   "Neutral",     0.000,     0.000,
  # ... add all hitters from preview
)

# =============================================================================
# 3. HITTER ACTUALS (from pitching file)
# =============================================================================

actuals <- pitcher_game |>
  filter(events %in% PA_EVENTS) |>
  mutate(woba_val=coalesce(WOBA_WEIGHTS[events],0),
         woba_den=as.integer(events!="hit_by_pitch")) |>
  group_by(player_name) |>
  summarise(PA=n(), wn=sum(woba_val,na.rm=T), wd=sum(woba_den,na.rm=T),
            .groups="drop") |>
  mutate(actual_wOBA=round(wn/pmax(wd,1),3)) |>
  filter(PA >= 2)

cat("\n=== HITTER ACTUALS ===\n")
print(actuals |> select(player_name, PA, actual_wOBA) |>
  arrange(desc(actual_wOBA)))

# =============================================================================
# 4. H1 TEST
# =============================================================================

results <- PREDICTIONS |>
  left_join(actuals |> select(player_name, PA, actual_wOBA), by="player_name") |>
  filter(!is.na(actual_wOBA)) |>
  mutate(
    err_champ  = round(abs(exp_wOBA - actual_wOBA), 3),
    err_naive  = round(abs(prior_wOBA - actual_wOBA), 3),
    champ_wins = err_champ < err_naive,
    residual   = round(exp_wOBA - actual_wOBA, 3)
  )

cat("\n=== H1 TEST ===\n")
print(results |>
  select(player_name, tier, exp_wOBA, prior_wOBA, actual_wOBA,
         err_champ, err_naive, champ_wins, PA), n=Inf)

mae_c    <- mean(results$err_champ)
mae_n    <- mean(results$err_naive)
bias     <- mean(results$residual)
game_mae <- abs(mean(results$exp_wOBA) - mean(results$actual_wOBA))
rho      <- cor(results$exp_wOBA, results$actual_wOBA,
                method="spearman", use="complete.obs")
pred_top3 <- results |> slice_max(exp_wOBA, n=3) |> pull(player_name)
act_top3  <- results |> slice_max(actual_wOBA, n=3) |> pull(player_name)

cat(sprintf("\nChampion MAE: %.3f  Naive-1: %.3f  H1: %s\n",
    mae_c, mae_n, ifelse(mae_c<mae_n,"SUPPORTS","DOES NOT SUPPORT")))
cat(sprintf("Bias: %+.3f  Game-level |err|: %.3f\n", bias, game_mae))
cat(sprintf("Spearman rho: %.3f  Top-3 overlap: %d/3\n",
    rho, length(intersect(pred_top3, act_top3))))

# =============================================================================
# 5. ASSUMPTION SCORING
# Fill in expected values from the preview assumptions table.
# =============================================================================

cat("\n=== ASSUMPTION AUDIT ===\n")

# Example — replace with actual named assumptions from preview:
# assumptions <- tribble(
#   ~assumption,        ~expected, ~actual,                       ~tolerance, ~unit,
#   "Pitch X to LHH",  00.0,      get_pct(lhh, "Pitch Name"),    12,         "pp",
#   "Fastball velo",    00.0,      get_velo("4-Seam Fastball"),    1.5,        "mph"
# ) |>
#   mutate(delta=round(actual-expected,1),
#          verdict=if_else(abs(delta)<=tolerance,"held","failed"))
# print(assumptions)

# =============================================================================
# 6. WRITE MEMORY
# =============================================================================

safe_dir_create("model_memory")

results |>
  mutate(game_date=as.Date(GAME_DATE), pitcher=PITCHER_NAME,
         pitcher_id=PITCHER_ID, park=PARK, obs_n=OBS_N,
         data_source=DATA_SOURCE, confidence=CONFIDENCE) |>
  select(game_date, pitcher, pitcher_id, park, obs_n,
         data_source, confidence, player_name, hand, tier,
         PA, exp_wOBA, actual_wOBA, err_champ, err_naive,
         champ_wins, residual) |>
  write_csv(sprintf("model_memory/hitter_obs_%s_%s.csv",
    GAME_DATE, tolower(gsub(", .*","",PITCHER_NAME))))

tibble(
  game_date     = as.Date(GAME_DATE), obs_n=OBS_N,
  pitcher       = PITCHER_NAME, pitcher_hand=PITCHER_HAND,
  park=PARK, confidence=CONFIDENCE, data_source=DATA_SOURCE,
  pitcher_line  = PITCHER_LINE, result=RESULT,
  n_hitters     = nrow(results),
  team_exp      = round(mean(results$exp_wOBA),3),
  team_actual   = round(mean(results$actual_wOBA),3),
  game_mae      = round(game_mae,3),
  mae_champion  = round(mae_c,3), mae_naive1=round(mae_n,3),
  bias          = round(bias,3),
  spearman_rho  = round(rho,3),
  top3_overlap  = length(intersect(pred_top3,act_top3)),
  h1_supported  = mae_c < mae_n,
  h2_testable   = FALSE,
  notable       = ""   # fill in key finding for this game
) |>
  write_csv(sprintf("model_memory/game_log_%s_%s.csv",
    GAME_DATE, tolower(gsub(", .*","",PITCHER_NAME))))

cat(sprintf("\nMemory written. Observation %d complete.\n", OBS_N))
cat("H1: INSUFFICIENT EVIDENCE until n=200.\n")
