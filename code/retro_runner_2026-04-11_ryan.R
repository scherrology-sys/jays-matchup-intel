# =============================================================================
# Jays Matchup Intel
# April 11, 2026 · MIN @ TOR · Rogers Centre · Retro
# =============================================================================

library(tidyverse)
source("code/jays_matchup_helpers.R")

# =============================================================================
# 1. LOAD DATA
# =============================================================================

jays_bat <- safe_read_csv("savant_data__42_.csv")   # Jays batting Apr 11
min_pit  <- safe_read_csv("savant_data__41_.csv")   # MIN pitching Apr 11
h24      <- safe_read_csv("savant_data__19_.csv")
h25      <- safe_read_csv("savant_data__18_.csv")

JR_ID     <- 657746L
GAME_DATE <- "2026-04-11"
PARK      <- "TOR"

game <- jays_bat |> filter(pitcher == JR_ID)
cat(sprintf("Ryan pitches: %d  Max inning: %d  Result: MIN 7, TOR 4\n",
    nrow(game), max(game$inning, na.rm=TRUE)))
cat("Springer: injured during game, 2 PA only\n")

# =============================================================================
# 2. ACTUAL PITCH MIX
# =============================================================================

cat("\n=== ACTUAL PITCH MIX ===\n")
cat("Overall:\n")
game |> count(pitch_name) |>
  mutate(pct=round(n/sum(n)*100,1)) |> arrange(desc(pct)) |> print()

cat("\nvs LHH:\n")
game |> filter(stand=="L") |> count(pitch_name) |>
  mutate(pct=round(n/sum(n)*100,1)) |> arrange(desc(pct)) |> print()

cat("\nvs RHH:\n")
game |> filter(stand=="R") |> count(pitch_name) |>
  mutate(pct=round(n/sum(n)*100,1)) |> arrange(desc(pct)) |> print()

# =============================================================================
# 3. CHAMPION PREDICTIONS (as generated, no adjustment)
# Empirical blending rule applied: n_curr ~125 LHH, ~138 RHH → w_curr = 0.7
# =============================================================================

PREDICTIONS <- tribble(
  ~player_name,              ~hand, ~tier,        ~exp_wOBA,
  "Guerrero Jr., Vladimir",  "R",   "Edge",        0.409,
  "Springer, George",        "R",   "Edge",        0.384,
  "Barger, Addison",         "L",   "Neutral",     0.369,
  "Giménez, Andrés",         "L",   "Neutral",     0.345,
  "Lukes, Nathan",            "L",   "Neutral",     0.343,
  "Heineman, Tyler",         "L",   "Neutral",     0.333,
  "Kirk, Alejandro",         "R",   "Neutral",     0.329,
  "Schneider, Davis",        "R",   "Neutral",     0.313,
  "Clement, Ernie",          "R",   "Neutral",     0.313,
  "Straw, Myles",            "R",   "Suppressed",  0.309,
  "Varsho, Daulton",         "L",   "Suppressed",  0.307,
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

priors <- build_priors(bind_rows(h24, h25))

results <- PREDICTIONS |>
  left_join(actuals |> select(player_name, PA, actual_wOBA), by="player_name") |>
  filter(!is.na(actual_wOBA)) |>
  left_join(priors$overall |> select(player_name, wOBA_overall), by="player_name") |>
  mutate(
    naive1     = round(wOBA_overall, 3),
    naive2     = map_dbl(hand, \(h) park_adjusted_baseline(h, PARK)),
    err_c      = round(abs(exp_wOBA - actual_wOBA), 3),
    err_n      = round(abs(naive1 - actual_wOBA), 3),
    champ_wins = err_c < err_n,
    resid      = round(exp_wOBA - actual_wOBA, 3)
  )

cat("\n=== H1 TEST ===\n")
print(results |> select(player_name, tier, exp_wOBA, naive1, actual_wOBA,
                         err_c, err_n, champ_wins, PA), n=Inf)

mae_c    <- mean(results$err_c)
mae_n    <- mean(results$err_n)
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
cat("Source: 2026-primary blended (w=0.7), career prior (w=0.3). MODERATE confidence.\n\n")

lhh <- game |> filter(stand=="L")
rhh <- game |> filter(stand=="R")

assumptions_scored <- tribble(
  ~assumption,           ~expected, ~actual,
  "4-Seam to LHH",       52.5,     round(mean(lhh$pitch_name=="4-Seam Fastball",na.rm=T)*100,1),
  "Knuckle Curve to LHH", 9.9,     round(mean(lhh$pitch_name=="Knuckle Curve",na.rm=T)*100,1),
  "Sinker to RHH",        14.9,    round(mean(rhh$pitch_name=="Sinker",na.rm=T)*100,1),
  "Sweeper to RHH",       14.3,    round(mean(rhh$pitch_name=="Sweeper",na.rm=T)*100,1),
  "Fastball velo",         92.7,   round(mean(game$release_speed[game$pitch_name=="4-Seam Fastball"],na.rm=T),1)
) |>
  mutate(
    tolerance = c(12, 12, 12, 12, 1.5),
    delta     = round(actual - expected, 1),
    verdict   = if_else(abs(delta) <= tolerance, "held", "failed")
  )

print(assumptions_scored)
cat(sprintf("\nAssumptions held: %d/5\n", sum(assumptions_scored$verdict=="held")))
cat("Sinker to RHH failed: +18.4pp. Ryan leaned on the Sinker heavily — career rate 9.6%.\n")
cat("This is the third assumption failure from a pitcher over-relying on one pitch.\n")

# =============================================================================
# 6. WRITE MEMORY
# =============================================================================

safe_dir_create("model_memory")

results |>
  mutate(game_date=as.Date(GAME_DATE), pitcher="Ryan, Joe",
         pitcher_id=JR_ID, park=PARK, obs_n=12L,
         data_source="career_file_2026_primary", confidence="MODERATE") |>
  select(game_date, pitcher, pitcher_id, park, obs_n,
         data_source, confidence, player_name, hand, tier,
         PA, exp_wOBA, actual_wOBA, err_c, err_n, champ_wins, resid) |>
  write_csv("model_memory/hitter_obs_2026-04-11_ryan.csv")

tibble(
  game_date        = as.Date(GAME_DATE), obs_n=12L,
  pitcher          = "Ryan, Joe", pitcher_hand="R",
  park=PARK, confidence="MODERATE",
  data_source      = "2026_primary_w0.7_career_w0.3",
  pitcher_line     = "7 IP",
  result           = "MIN 7, TOR 4",
  n_hitters        = nrow(results),
  team_exp         = round(mean(results$exp_wOBA),3),
  team_actual      = round(mean(results$actual_wOBA),3),
  game_mae         = round(game_mae,3),
  mae_champion     = round(mae_c,3),
  mae_naive1       = round(mae_n,3),
  bias             = round(bias,3),
  spearman_rho     = round(rho,3),
  top3_overlap     = length(intersect(pred_top3,act_top3)),
  h1_supported     = mae_c < mae_n,
  assumptions_held = sum(assumptions_scored$verdict=="held"),
  notable          = "Varsho Suppressed produced .700 actual. Sinker to RHH failed +18.4pp. Springer injured 2 PA. H1 does not support this game."
) |>
  write_csv("model_memory/game_log_2026-04-11_ryan.csv")

cat("\nMemory written. Running n: ~83 hitter-games, 12 observations.\n")

# =============================================================================
# END
# =============================================================================
# OBSERVATION 12 SUMMARY:
#   Game:             MIN 7, TOR 4
#   Ryan line:        7 IP — effective, Jays held down
#   Champion MAE:     0.219  Naive-1: 0.204  H1 DOES NOT SUPPORT
#   Game-level |err|: 0.101
#   Bias:             +0.101 (over-predicted)
#   Spearman rho:     -0.112
#   Top-3 overlap:    1/3
#   Assumptions:      4/5 held · Sinker to RHH failed (+18.4pp)
#   Varsho:           Suppressed .307 · actual .700 — biggest miss of season
#   Springer:         Injured · 2 PA only
#   Model note:       First preview using empirical blending rule (w=0.7 at n_curr~130)
#   Model action:     NONE — model is locked
# =============================================================================
