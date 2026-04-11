# =============================================================================
# Jays Matchup Intel
# April 10, 2026 · MIN @ TOR · Rogers Centre · Retro
# =============================================================================

library(tidyverse)
source("code/jays_matchup_helpers.R")

# =============================================================================
# 1. LOAD DATA
# =============================================================================

# savant_37: pitching file (player_name = pitcher)
# savant_38: batting file (player_name = batter) — use for per-hitter actuals
jays_bat    <- safe_read_csv("savant_data__38_.csv")
wr_pit_file <- safe_read_csv("savant_data__37_.csv")

WR_ID     <- 680573L
GAME_DATE <- "2026-04-10"
PARK      <- "TOR"

game <- jays_bat |> filter(pitcher == WR_ID)
cat(sprintf("WR pitches: %d  Max inning: %d\n",
    nrow(game), max(game$inning, na.rm = TRUE)))

# =============================================================================
# 2. ACTUAL PITCH MIX
# =============================================================================

cat("\n=== ACTUAL PITCH MIX ===\n")
cat("Overall:\n")
game |> count(pitch_name) |>
  mutate(pct = round(n / sum(n) * 100, 1)) |> arrange(desc(pct)) |> print()

cat("\nvs LHH:\n")
game |> filter(stand == "L") |> count(pitch_name) |>
  mutate(pct = round(n / sum(n) * 100, 1)) |> arrange(desc(pct)) |> print()

cat("\nvs RHH:\n")
game |> filter(stand == "R") |> count(pitch_name) |>
  mutate(pct = round(n / sum(n) * 100, 1)) |> arrange(desc(pct)) |> print()

# =============================================================================
# 3. CHAMPION PREDICTIONS (as generated — no adjustment)
# =============================================================================

PREDICTIONS <- tribble(
  ~player_name,              ~hand, ~tier,        ~exp_wOBA,
  "Guerrero Jr., Vladimir",  "R",   "Edge",        0.397,
  "Springer, George",        "R",   "Neutral",     0.350,
  "Lukes, Nathan",            "L",   "Neutral",     0.344,
  "Varsho, Daulton",         "L",   "Neutral",     0.336,
  "Heineman, Tyler",         "L",   "Neutral",     0.332,
  "Barger, Addison",         "L",   "Neutral",     0.326,
  "Kirk, Alejandro",         "R",   "Neutral",     0.324,
  "Giménez, Andrés",         "L",   "Neutral",     0.323,
  "Schneider, Davis",        "R",   "Neutral",     0.303,
  "Sánchez, Jesús",          "L",   "Neutral",     0.303,
  "Clement, Ernie",          "R",   "Neutral",     0.302,
  "Straw, Myles",            "R",   "Suppressed",  0.296,
)

actuals <- game |>
  filter(events %in% PA_EVENTS) |>
  mutate(wv = coalesce(WOBA_WEIGHTS[events], 0),
         wd = as.integer(events != "hit_by_pitch")) |>
  group_by(player_name) |>
  summarise(PA = n(), wn = sum(wv, na.rm = T), wd = sum(wd, na.rm = T),
            .groups = "drop") |>
  mutate(actual_wOBA = round(wn / pmax(wd, 1), 3)) |>
  filter(PA >= 2)

# =============================================================================
# 4. H1 TEST
# =============================================================================

# Load hitter priors for naive baseline
h24 <- safe_read_csv("savant_data__19_.csv")
h25 <- safe_read_csv("savant_data__18_.csv")
priors <- build_priors(bind_rows(h24, h25))

results <- PREDICTIONS |>
  left_join(actuals |> select(player_name, PA, actual_wOBA), by = "player_name") |>
  filter(!is.na(actual_wOBA)) |>
  left_join(priors$overall |> select(player_name, wOBA_overall), by = "player_name") |>
  mutate(
    naive1     = round(wOBA_overall, 3),
    naive2     = map_dbl(hand, \(h) park_adjusted_baseline(h, PARK)),
    err_c      = round(abs(exp_wOBA - actual_wOBA), 3),
    err_n      = round(abs(naive1 - actual_wOBA), 3),
    champ_wins = err_c < err_n,
    resid      = round(exp_wOBA - actual_wOBA, 3)
  )

cat("\n=== H1 TEST ===\n")
print(results |> select(player_name, tier, exp_wOBA, naive1,
                         actual_wOBA, err_c, err_n, champ_wins, PA), n = Inf)

mae_c    <- mean(results$err_c)
mae_n    <- mean(results$err_n)
bias     <- mean(results$resid)
game_mae <- abs(mean(results$exp_wOBA) - mean(results$actual_wOBA))

cat(sprintf("\nChampion MAE: %.3f  Naive-1: %.3f  H1: %s\n",
    mae_c, mae_n, ifelse(mae_c < mae_n, "SUPPORTS", "DOES NOT SUPPORT")))
cat(sprintf("Bias: %+.3f  Game-level |err|: %.3f\n", bias, game_mae))

rho       <- cor(results$exp_wOBA, results$actual_wOBA,
                 method = "spearman", use = "complete.obs")
pred_top3 <- results |> slice_max(exp_wOBA, n = 3) |> pull(player_name)
act_top3  <- results |> slice_max(actual_wOBA, n = 3) |> pull(player_name)
cat(sprintf("Spearman rho: %.3f  Top-3 overlap: %d/3\n",
    rho, length(intersect(pred_top3, act_top3))))

# =============================================================================
# 5. ASSUMPTION SCORING
# =============================================================================

cat("\n=== ASSUMPTION AUDIT ===\n")
cat("Source: career file (53 starts, 2022-2026). MODERATE confidence.\n\n")

lhh <- game |> filter(stand == "L")
rhh <- game |> filter(stand == "R")

assumptions_scored <- tribble(
  ~assumption,           ~expected, ~actual,
  "4-Seam to LHH",       48.8,     round(mean(lhh$pitch_name == "4-Seam Fastball", na.rm=T)*100, 1),
  "Split-Finger to LHH", 15.7,     round(mean(lhh$pitch_name == "Split-Finger", na.rm=T)*100, 1),
  "Slider to RHH",       35.1,     round(mean(rhh$pitch_name == "Slider", na.rm=T)*100, 1),
  "Fastball velo",        93.1,     round(mean(game$release_speed[game$pitch_name == "4-Seam Fastball"], na.rm=T), 1)
) |>
  mutate(
    tolerance = c(12, 12, 12, 1.5),
    delta     = round(actual - expected, 1),
    verdict   = if_else(abs(delta) <= tolerance, "held", "failed")
  )

print(assumptions_scored)
cat(sprintf("\nAssumptions held: %d/4\n", sum(assumptions_scored$verdict == "held")))
cat("Slider to RHH failed: +13.7pp over tolerance.\n")
cat("WR over-relied on his best pitch as the game unraveled — behavioral response to pressure.\n")

# =============================================================================
# 6. WRITE MEMORY
# =============================================================================

safe_dir_create("model_memory")

results |>
  mutate(game_date = as.Date(GAME_DATE), pitcher = "Woods Richardson, Simeon",
         pitcher_id = WR_ID, park = PARK, obs_n = 11L,
         data_source = "career_file", confidence = "MODERATE") |>
  select(game_date, pitcher, pitcher_id, park, obs_n,
         data_source, confidence, player_name, hand, tier,
         PA, exp_wOBA, actual_wOBA, err_c, err_n, champ_wins, resid) |>
  write_csv("model_memory/hitter_obs_2026-04-10_wr.csv")

tibble(
  game_date        = as.Date(GAME_DATE), obs_n = 11L,
  pitcher          = "Woods Richardson, Simeon", pitcher_hand = "R",
  park = PARK, confidence = "MODERATE", data_source = "career_file",
  pitcher_line     = "4 IP 68P",
  result           = "TOR 10, MIN 4",
  n_hitters        = nrow(results),
  team_exp         = round(mean(results$exp_wOBA), 3),
  team_actual      = round(mean(results$actual_wOBA), 3),
  game_mae         = round(game_mae, 3),
  mae_champion     = round(mae_c, 3),
  mae_naive1       = round(mae_n, 3),
  bias             = round(bias, 3),
  spearman_rho     = round(rho, 3),
  top3_overlap     = length(intersect(pred_top3, act_top3)),
  h1_supported     = mae_c < mae_n,
  assumptions_held = sum(assumptions_scored$verdict == "held"),
  notable          = "Guerrero Edge validated (.636 actual). Slider to RHH failed (+13.7pp) as WR unraveled. Assumption failures correlate with pitcher distress — second instance this series."
) |>
  write_csv("model_memory/game_log_2026-04-10_wr.csv")

cat("\nMemory written. Running n: ~76 hitter-games, 11 observations.\n")

# =============================================================================
# END
# =============================================================================
# OBSERVATION 11 SUMMARY:
#   Game:             TOR 10, MIN 4
#   WR line:          4 IP 68P — knocked out in 4th
#   Champion MAE:     0.216  Naive-1: 0.228  H1 SUPPORTS
#   Game-level |err|: 0.115
#   Bias:             -0.115 (under-predicted — blowout)
#   Spearman rho:     +0.395
#   Top-3 overlap:    2/3
#   Assumptions:      3/4 held · Slider to RHH failed (+13.7pp)
#   Guerrero Edge:    actual .636 — call validated
#   Key finding:      Pitcher distress drives assumption failures.
#                     WR over-relied on Slider when getting hit.
#                     Same pattern as Yamamoto Apr 7.
#   Model action:     NONE — model is locked
# =============================================================================
