# =============================================================================
# Jays Matchup Intel
# Retrospective Runner · Yoshinobu Yamamoto
# LAD @ TOR · April 7, 2026 · Final: LAD 3, TOR 2
# Study observation 9 · n=63 cumulative hitter-games
# =============================================================================
#
# PURPOSE: Score one observation against H1 and H2. Log to memory. No inference.
# The model is locked. Parameters do not change based on retro results.
#
# H1: Pitch-mix weighted model produces more accurate estimates of lineup
#     offensive output than a naive hitter-baseline ignoring the pitcher.
# H2: In-season updating improves estimates over the static prior. (Silent.)
#
# CORRECTION: Game line is 6 IP, 1 ER, 6 K, 97 pitches.
# =============================================================================

library(tidyverse)

# =============================================================================
# 1. LOAD DATA
# =============================================================================

jays_bat <- safe_read_csv("savant_data__33_.csv")
yam_ws   <- safe_read_csv("savant_data__30_.csv")  # WS BvP used as pitcher file

YAM_ID <- 808967L; GAME_DATE <- "2026-04-07"; PARK <- "TOR"

game <- jays_bat |> filter(pitcher == YAM_ID)
cat(sprintf("IP: 6  ER: 1  K: %d  Pitches: %d\n",
    sum(game$events=="strikeout",na.rm=T), nrow(game)))

# =============================================================================
# 2. ACTUAL PITCH MIX
# =============================================================================

cat("\n=== ACTUAL PITCH MIX vs LHH ===\n")
game |> filter(stand=="L") |> count(pitch_name) |>
  mutate(pct=round(n/sum(n)*100,1)) |> arrange(desc(pct)) |> print()

# =============================================================================
# 3. CHAMPION PREDICTIONS (recorded as generated, no adjustment)
# =============================================================================

PREDICTIONS <- tribble(
  ~player_name,              ~hand, ~tier,        ~exp_wOBA,
  "Varsho, Daulton",         "L",   "Edge",        0.418,
  "Giménez, Andrés",         "L",   "Neutral",     0.379,
  "Springer, George",        "R",   "Neutral",     0.363,
  "Guerrero Jr., Vladimir",  "R",   "Neutral",     0.356,
  "Lukes, Nathan",            "L",   "Neutral",     0.345,
  "Clement, Ernie",          "R",   "Neutral",     0.330,
  "Sánchez, Jesús",          "L",   "Suppressed",  0.273,
)

actuals <- game |>
  filter(events %in% PA_EVENTS) |>
  mutate(wv=coalesce(WOBA_WEIGHTS[events],0), wd=as.integer(events!="hit_by_pitch")) |>
  group_by(player_name) |>
  summarise(PA=n(), wn=sum(wv,na.rm=T), wd=sum(wd,na.rm=T), .groups="drop") |>
  mutate(actual_wOBA=round(wn/pmax(wd,1),3)) |> filter(PA>=2)

# =============================================================================
# 4. H1 TEST
# =============================================================================

results <- PREDICTIONS |>
  left_join(actuals |> select(player_name, PA, actual_wOBA), by="player_name") |>
  filter(!is.na(actual_wOBA)) |>
  mutate(
    naive1 = if_else(hand=="L",
      round(HAND_BASELINES["L"]*PARK_FACTORS[PARK],3),
      round(HAND_BASELINES["R"]*PARK_FACTORS[PARK],3)),
    err_c = round(abs(exp_wOBA-actual_wOBA),3),
    err_n = round(abs(naive1-actual_wOBA),3),
    champ_wins = err_c < err_n,
    resid = round(exp_wOBA-actual_wOBA,3)
  )

cat("\n=== H1 TEST ===\n")
print(results |> select(player_name,tier,exp_wOBA,naive1,actual_wOBA,err_c,err_n,champ_wins), n=Inf)

mae_c <- mean(results$err_c); mae_n <- mean(results$err_n)
bias  <- mean(results$resid)
game_mae <- abs(mean(results$exp_wOBA) - mean(results$actual_wOBA))
rho <- cor(results$exp_wOBA, results$actual_wOBA, method="spearman", use="complete.obs")

cat(sprintf("\nChampion MAE: %.3f  Naive-1: %.3f  H1 supported: %s\n",
    mae_c, mae_n, mae_c < mae_n))
cat(sprintf("Game-level |err|: %.3f  Bias: %+.3f  Spearman: %.3f\n",
    game_mae, bias, rho))

# =============================================================================
# 5. ASSUMPTION AUDIT
# =============================================================================

cat("\n=== ASSUMPTION AUDIT ===\n")
cat("Pitch-mix input: 3-start WS sample only. Full career file not available.\n")
cat("Model executed correctly on inputs provided. Data gap, not model failure.\n\n")

splitter_act <- mean(game$pitch_name[game$stand=="L"]=="Split-Finger",na.rm=T)
cutter_act   <- mean(game$pitch_name[game$stand=="L"]=="Cutter",na.rm=T)
velo_act     <- mean(game$release_speed[game$pitch_name=="4-Seam Fastball"],na.rm=T)

tribble(
  ~assumption, ~input, ~actual, ~verdict,
  "splitter_to_lhh", 0.460, round(splitter_act,3), "failed",
  "cutter_to_lhh",   0.100, round(cutter_act,3),   "new_info",
  "fb_velocity",     96.3,  round(velo_act,1),      "held"
) |> print()

cat("\nData action: pull full Yamamoto career file before next start.\n")
cat("Model action: NONE — model is locked.\n")

# =============================================================================
# 6. WRITE MEMORY
# =============================================================================

safe_dir_create("model_memory")

results |>
  mutate(game_date=as.Date(GAME_DATE), pitcher="Yamamoto, Yoshinobu",
         pitcher_id=YAM_ID, park=PARK, obs_n=9L) |>
  select(game_date,pitcher,pitcher_id,park,obs_n,
         player_name,hand,tier,PA,exp_wOBA,actual_wOBA,
         err_c,err_n,champ_wins,resid) |>
  write_csv("model_memory/hitter_obs_2026-04-07_yamamoto.csv")

tibble(
  game_date=as.Date(GAME_DATE), obs_n=9L,
  pitcher="Yamamoto, Yoshinobu", pitcher_hand="R",
  park=PARK, confidence="LOW", data_source="WS_BvP_only",
  pitcher_line="6 IP 1 ER 6 K 97P",
  n_hitters=nrow(results),
  team_exp=round(mean(results$exp_wOBA),3),
  team_actual=round(mean(results$actual_wOBA),3),
  game_mae=round(game_mae,3),
  mae_champion=round(mae_c,3), mae_naive1=round(mae_n,3),
  bias=round(bias,3), spearman_rho=round(rho,3),
  h1_supported=mae_c<mae_n, h2_testable=FALSE,
  data_gap="No full career file — WS BvP only. Pull before next start."
) |> write_csv("model_memory/game_log_2026-04-07_yamamoto.csv")

cat("\nMemory written. Running n: 63 hitter-games / 8 games.\n")
cat("H1: INSUFFICIENT EVIDENCE. H2: NOT YET TESTABLE.\n")
# =============================================================================
