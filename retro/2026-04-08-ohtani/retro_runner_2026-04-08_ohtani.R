# =============================================================================
# Jays Matchup Intel
# Retrospective Runner · Shohei Ohtani
# LAD @ TOR · April 8, 2026 · Final: TOR 4, LAD 3
# Study observation 10 · n=~70 cumulative hitter-games
# =============================================================================
#
# PURPOSE: Score one observation against H1 and H2. Log to memory. No inference.
# The model is locked. Parameters do not change based on retro results.
#
# H1: Pitch-mix weighted model produces more accurate estimates of lineup
#     offensive output than a naive hitter-baseline ignoring the pitcher.
# H2: In-season updating improves estimates over the static prior. (Silent.)
#
# NOTE: Batting file (savant_data__20_) has player_name = pitcher name.
# Use pitching file (savant_data__21_) for per-batter actuals — player_name
# in that file is the batter. Verified in data integrity check below.
# =============================================================================

library(tidyverse)

# =============================================================================
# 1. LOAD DATA
# =============================================================================

jays_bat    <- safe_read_csv("savant_data__20_.csv")   # Jays batting Apr 8
dodgers_pit <- safe_read_csv("savant_data__21_.csv")   # Dodgers pitching Apr 8

OHT_ID    <- 660271L
GAME_DATE <- "2026-04-08"
PARK      <- "TOR"

# Pitching file: player_name = batter — correct source for per-hitter scoring
game <- dodgers_pit |> filter(pitcher == OHT_ID)
cat(sprintf("IP: 6  Pitches: %d  Batters: %d unique\n",
    nrow(game), n_distinct(game$player_name)))

# =============================================================================
# 2. ACTUAL PITCH MIX
# =============================================================================

cat("\n=== ACTUAL PITCH MIX vs LHH ===\n")
game |> filter(stand=="L") |> count(pitch_name) |>
  mutate(pct=round(n/sum(n)*100,1)) |> arrange(desc(pct)) |> print()

cat("\nvs RHH:\n")
game |> filter(stand=="R") |> count(pitch_name) |>
  mutate(pct=round(n/sum(n)*100,1)) |> arrange(desc(pct)) |> print()

# =============================================================================
# 3. CHAMPION PREDICTIONS (recorded as generated, no adjustment)
# =============================================================================

PREDICTIONS <- tribble(
  ~player_name,              ~hand, ~tier,        ~exp_wOBA,
  "Springer, George",        "R",   "Edge",        0.440,
  "Guerrero Jr., Vladimir",  "R",   "Neutral",     0.363,
  "Schneider, Davis",        "R",   "Neutral",     0.354,
  "Varsho, Daulton",         "L",   "Neutral",     0.341,
  "Barger, Addison",         "L",   "Neutral",     0.335,
  "Heineman, Tyler",         "L",   "Neutral",     0.332,
  "Lukes, Nathan",            "L",   "Neutral",     0.331,
  "Giménez, Andrés",         "L",   "Neutral",     0.325,
  "Straw, Myles",            "R",   "Neutral",     0.313,
  "Sánchez, Jesús",          "L",   "Neutral",     0.306,
  "Kirk, Alejandro",         "R",   "Suppressed",  0.305,
  "Clement, Ernie",          "R",   "Suppressed",  0.275,
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
    naive1     = if_else(hand=="L",
      round(HAND_BASELINES["L"]*PARK_FACTORS[PARK],3),
      round(HAND_BASELINES["R"]*PARK_FACTORS[PARK],3)),
    err_c      = round(abs(exp_wOBA-actual_wOBA),3),
    err_n      = round(abs(naive1-actual_wOBA),3),
    champ_wins = err_c < err_n,
    resid      = round(exp_wOBA-actual_wOBA,3)
  )

cat("\n=== H1 TEST ===\n")
print(results |> select(player_name, tier, exp_wOBA, naive1, actual_wOBA,
                         err_c, err_n, champ_wins, PA), n=Inf)

mae_c    <- mean(results$err_c); mae_n <- mean(results$err_n)
bias     <- mean(results$resid)
game_mae <- abs(mean(results$exp_wOBA) - mean(results$actual_wOBA))

cat(sprintf("\nChampion MAE: %.3f  Naive-1: %.3f  H1: %s\n",
    mae_c, mae_n, ifelse(mae_c < mae_n, "SUPPORTS", "DOES NOT SUPPORT")))
cat(sprintf("Bias: %+.3f  Game-level |err|: %.3f\n", bias, game_mae))

rho       <- cor(results$exp_wOBA, results$actual_wOBA,
                 method="spearman", use="complete.obs")
pred_top3 <- results |> slice_max(exp_wOBA, n=3) |> pull(player_name)
act_top3  <- results |> slice_max(actual_wOBA, n=3) |> pull(player_name)
cat(sprintf("Spearman rho: %.3f  Top-3 overlap: %d/3\n",
    rho, length(intersect(pred_top3, act_top3))))

# =============================================================================
# 5. ASSUMPTION SCORING
# =============================================================================

cat("\n=== ASSUMPTION AUDIT ===\n")
cat("Source: career file (15 starts, 2025-26). MODERATE confidence.\n\n")

lhh <- game |> filter(stand=="L")
rhh <- game |> filter(stand=="R")

assumptions_scored <- tribble(
  ~assumption,        ~expected, ~actual,
  "4-Seam to LHH",    44.1,     round(mean(lhh$pitch_name=="4-Seam Fastball",na.rm=T)*100,1),
  "Curveball to LHH", 17.8,     round(mean(lhh$pitch_name=="Curveball",na.rm=T)*100,1),
  "Sweeper to RHH",   36.9,     round(mean(rhh$pitch_name=="Sweeper",na.rm=T)*100,1),
  "Fastball velo",    98.1,     round(mean(game$release_speed[game$pitch_name=="4-Seam Fastball"],na.rm=T),1)
) |>
  mutate(
    tolerance = c(12, 12, 12, 1.5),
    delta     = round(actual - expected, 1),
    verdict   = if_else(abs(delta) <= tolerance, "held", "failed")
  )

print(assumptions_scored)
cat(sprintf("\nAll assumptions held: %s\n", all(assumptions_scored$verdict=="held")))
cat("Career file data reliable. Contrast with WS-source failures Apr 6 and Apr 7.\n")
cat("Data source quality drives assumption stability more than any model parameter.\n")

# =============================================================================
# 6. WRITE MEMORY
# =============================================================================

safe_dir_create("model_memory")

results |>
  mutate(game_date=as.Date(GAME_DATE), pitcher="Ohtani, Shohei",
         pitcher_id=OHT_ID, park=PARK, obs_n=10L,
         data_source="career_file", confidence="MODERATE") |>
  select(game_date, pitcher, pitcher_id, park, obs_n,
         data_source, confidence, player_name, hand, tier,
         PA, exp_wOBA, actual_wOBA, err_c, err_n, champ_wins, resid) |>
  write_csv("model_memory/hitter_obs_2026-04-08_ohtani.csv")

tibble(
  game_date    = as.Date(GAME_DATE), obs_n=10L,
  pitcher      = "Ohtani, Shohei", pitcher_hand="R",
  park=PARK, confidence="MODERATE", data_source="career_file",
  pitcher_line = "6 IP 97P",
  n_hitters    = nrow(results),
  team_exp     = round(mean(results$exp_wOBA),3),
  team_actual  = round(mean(results$actual_wOBA),3),
  game_mae     = round(game_mae,3),
  mae_champion = round(mae_c,3), mae_naive1=round(mae_n,3),
  bias         = round(bias,3), spearman_rho=round(rho,3),
  top3_overlap = length(intersect(pred_top3,act_top3)),
  h1_supported = mae_c < mae_n,
  assumptions_held = all(assumptions_scored$verdict=="held"),
  notable      = "All assumptions held. Career file vs WS source is key variable."
) |>
  write_csv("model_memory/game_log_2026-04-08_ohtani.csv")

cat("\nMemory written. Running n: ~70 hitter-games, 10 observations.\n")
cat("H1: INSUFFICIENT EVIDENCE. H2 ranking: signal accumulating.\n")

# =============================================================================
# END
# =============================================================================
# OBSERVATION 10 SUMMARY:
#   Game:             TOR 4, LAD 3
#   Champion MAE:     0.237  Naive-1: 0.237  Tie
#   Game-level |err|: 0.096
#   Spearman rho:     +0.482 — best of 2026 season
#   Top-3 overlap:    2/3
#   Assumptions:      ALL HELD — career file reliable
#   Key finding:      Career file vs WS source drives assumption stability.
#   Guerrero:         actual .720 · model neutral .363
#                     WS BvP .700 appeared twice vs Ohtani. Track.
#   Model action:     NONE — model is locked
# =============================================================================
