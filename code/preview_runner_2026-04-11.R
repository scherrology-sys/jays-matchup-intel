# =============================================================================
# Jays Matchup Intel
# April 11, 2026 · MIN @ TOR · Rogers Centre
# =============================================================================

library(tidyverse)
source("code/jays_matchup_helpers.R")

# =============================================================================
# 1. LOAD DATA
# =============================================================================

jr_career <- safe_read_csv("savant_data__40_.csv")   # Ryan career 2024-2026
jr_bvp    <- safe_read_csv("savant_data__39_.csv")   # Ryan BvP vs Jays (3 games)
jays_2026 <- safe_read_csv("savant_data__25_.csv")   # Jays 2026 batting through Apr 10
h24       <- safe_read_csv("savant_data__19_.csv")   # Jays 2024 batting
h25       <- safe_read_csv("savant_data__18_.csv")   # Jays 2025 batting

JR_ID     <- 657746L
GAME_DATE <- "2026-04-11"
PARK      <- "TOR"

cat(sprintf("Ryan career: %d pitches, %d starts\n",
    nrow(jr_career), n_distinct(jr_career$game_date)))
cat(sprintf("Ryan BvP vs Jays: %d pitches, %d games (%s)\n",
    nrow(jr_bvp), n_distinct(jr_bvp$game_date),
    paste(unique(jr_bvp$game_date), collapse=", ")))

# =============================================================================
# 2. PITCHER ROLLING MIX
# =============================================================================

mix_lhh <- rolling_mix(jr_career, GAME_DATE, batter_hand = "L")
mix_rhh <- rolling_mix(jr_career, GAME_DATE, batter_hand = "R")

eff_l <- mix_lhh$eff_sample
eff_r <- mix_rhh$eff_sample
conf  <- confidence_tier(eff_l, eff_r)

cat(sprintf("\nEff sample: L=%d  R=%d  Confidence: %s\n", eff_l, eff_r, conf))

cat("\nvs LHH:\n")
enframe(mix_lhh$mix, "pitch_name", "pct") |>
  mutate(pct = round(pct * 100, 1)) |> arrange(desc(pct)) |> print()

cat("\nvs RHH:\n")
enframe(mix_rhh$mix, "pitch_name", "pct") |>
  mutate(pct = round(pct * 100, 1)) |> arrange(desc(pct)) |> print()

cat("\nxwOBA by pitch (career file):\n")
jr_career |>
  filter(events %in% PA_EVENTS, !is.na(estimated_woba_using_speedangle)) |>
  group_by(pitch_name) |>
  summarise(n_PA  = n(),
            xwOBA = round(mean(estimated_woba_using_speedangle, na.rm=TRUE), 3),
            .groups="drop") |>
  filter(n_PA >= 20) |> arrange(xwOBA) |> print()

# =============================================================================
# 3. HITTER PRIORS
# =============================================================================

priors <- build_priors(bind_rows(h24, h25))

# Baseline C: 2026 actual
pa26 <- jays_2026 |>
  filter(events %in% PA_EVENTS) |>
  mutate(wv = coalesce(WOBA_WEIGHTS[events], 0),
         wd = as.integer(events != "hit_by_pitch"))
team_woba_2026 <- round(sum(pa26$wv) / sum(pa26$wd), 3)

# =============================================================================
# 4. CHAMPION MODEL
# =============================================================================

BATTER_POOL <- c(
  "Barger, Addison", "Clement, Ernie", "Giménez, Andrés",
  "Guerrero Jr., Vladimir", "Heineman, Tyler", "Kirk, Alejandro",
  "Lukes, Nathan", "Schneider, Davis", "Springer, George",
  "Straw, Myles", "Sánchez, Jesús", "Varsho, Daulton"
)

cat(sprintf("\n=== CHAMPION MODEL ===\n"))
cat(sprintf("Confidence: %s  Threshold: ±%.3f  Park: %s (%.2f)  K_adj: %d\n",
    conf, RANKING_TIER_THRESHOLDS[conf], PARK, PARK_FACTORS[PARK], adjusted_K(conf)))

champion_results <- map_dfr(BATTER_POOL, function(player) {
  hh <- priors$hand    |> filter(player_name == player)
  rp <- priors$overall |> filter(player_name == player)
  if (nrow(hh)==0 || nrow(rp)==0) return(tibble())
  hand    <- hh$hand[1]
  park_fb <- park_adjusted_baseline(hand, PARK)
  mx      <- if (hand == "L") mix_lhh$mix else mix_rhh$mix
  sp      <- priors$splits |> filter(player_name == player, PA_vs >= MIN_PA)
  ws <- 0; wm <- 0
  for (p in names(mx)) {
    r <- sp |> filter(pitch_name == p)
    if (nrow(r) > 0 && !is.na(r$pitch_wOBA[1])) {
      ws <- ws + mx[p] * r$pitch_wOBA[1]; wm <- wm + mx[p]
    }
  }
  exp <- round(ws + (1 - wm) * park_fb, 3)
  tibble(player_name=player, hand=hand, exp_wOBA=exp,
         prior_wOBA=rp$wOBA_overall[1])
}) |>
  filter(!is.na(exp_wOBA)) |>
  arrange(desc(exp_wOBA))

team_mean <- mean(champion_results$exp_wOBA, na.rm=TRUE)
champion_results <- compute_ranking_tiers(champion_results, conf, team_mean)

print(champion_results |>
  select(player_name, hand, tier, exp_wOBA, prior_wOBA) |>
  mutate(across(where(is.numeric), \(x) round(x, 3))), n=Inf)

cat(sprintf("\nTeam mean exp_wOBA: %.3f\n", team_mean))

# =============================================================================
# 5. BVP DIAGNOSTIC (context only)
# =============================================================================

cat("\n=== BVP DIAGNOSTIC (context only) ===\n")
cat("NOTE: Guerrero BvP .275 in 9 PA contradicts Edge placement.\n")
cat("Career pitch-type splits (300+ PA per pitch type) carry more weight than 9 at-bats.\n\n")

jr_bvp |>
  filter(events %in% PA_EVENTS) |>
  mutate(wv = coalesce(WOBA_WEIGHTS[events], 0),
         wd = as.integer(events != "hit_by_pitch")) |>
  group_by(player_name) |>
  summarise(PA=n(), wn=sum(wv,na.rm=T), wd=sum(wd,na.rm=T), .groups="drop") |>
  mutate(wOBA_BvP = round(wn / pmax(wd, 1), 3)) |>
  filter(PA >= 2) |> arrange(desc(PA)) |> print()

# =============================================================================
# 6. BASELINE TOURNAMENT
# =============================================================================

baseline_A <- champion_results |>
  left_join(priors$overall |> select(player_name, wOBA_overall), by="player_name") |>
  summarise(A = round(mean(wOBA_overall, na.rm=TRUE), 3)) |> pull(A)

baseline_B <- champion_results |>
  left_join(priors$hand |> select(player_name, hand), by="player_name") |>
  mutate(platoon = round(HAND_BASELINES[hand] * PARK_FACTORS[PARK], 3)) |>
  summarise(B = round(mean(platoon, na.rm=TRUE), 3)) |> pull(B)

baseline_C <- team_woba_2026

# Ryan 2025 xwOBA allowed from career file
JR_XWOBA_2025 <- jr_career |>
  filter(format(as.Date(game_date), "%Y") == "2025",
         !is.na(estimated_woba_using_speedangle)) |>
  summarise(xwOBA = round(mean(estimated_woba_using_speedangle), 3)) |>
  pull(xwOBA)
baseline_D <- round(JR_XWOBA_2025 * PARK_FACTORS[PARK], 3)

baseline_E <- round(mean(champion_results$exp_wOBA, na.rm=TRUE), 3)

tournament <- tribble(
  ~baseline, ~label,                                           ~value,
  "A",       "Hitter prior wOBA (2024-25)",                    baseline_A,
  "B",       "Platoon baseline, park-adjusted (TOR 0.99)",     baseline_B,
  "C",       "Team 2026 actual wOBA to date",                  baseline_C,
  "D",       "Pitcher proxy (Ryan 2025 xwOBA × 0.99)",        baseline_D,
  "E",       "Champion model",                                  baseline_E,
) |> mutate(champion_beats = value < baseline_E | baseline == "E")

cat("\n=== BASELINE TOURNAMENT ===\n")
print(tournament)
cat(sprintf("\nChampion: %.3f  vs A: %+.3f  vs B: %+.3f  vs C: %+.3f  vs D: %+.3f\n",
    baseline_E, baseline_E-baseline_A, baseline_E-baseline_B,
    baseline_E-baseline_C, baseline_E-baseline_D))

# =============================================================================
# 7. SAVE PRE-GAME PREDICTION
# =============================================================================

safe_dir_create("outputs/rankings")
champion_results |>
  arrange(desc(exp_wOBA)) |>
  mutate(rank_champion = row_number(), game_date = as.Date(GAME_DATE),
         pitcher = "Ryan, Joe", confidence = conf,
         team_mean_exp = round(team_mean, 3)) |>
  select(game_date, pitcher, confidence, team_mean_exp,
         player_name, hand, tier, exp_wOBA, rank_champion) |>
  write_csv("outputs/rankings/champion_rank_2026-04-11_ryan.csv")

cat("\nPre-game prediction saved. Study observation 12.\n")

# =============================================================================
# END
# =============================================================================
# CHAMPION SUMMARY — April 11, 2026:
#   Pitcher:        Joe Ryan RHP · MIN
#   Confidence:     MODERATE (eff_L=678, eff_R=667 · 57 starts · career file)
#   Team exp_wOBA:  0.339 (beats all 4 baselines)
#   Edge:           Guerrero (.402), Springer (.377)
#   Suppressed:     Straw (.307), Varsho (.305)
#   Best pitches:   Knuckle Curve (.173 xwOBA), Sweeper (.211)
#   Most hittable:  Sinker (.341), Slider (.376)
#   BvP flag:       Guerrero .275 in 9 PA contradicts Edge — career splits carry
#   Varsho flag:    Sweeper wOBA .147 in 45 PA drives Suppressed placement
#   Tier threshold: ±0.030 (MODERATE)
#   K_adj: 45
#   Park: Rogers Centre TOR (factor=0.99)
# =============================================================================
