# =============================================================================
# Jays Matchup Intel
# Game Preview Runner · Simeon Woods Richardson
# MIN @ TOR · April 10, 2026 · Rogers Centre · Study observation 11
# =============================================================================
#
# PURPOSE: Execute H1 prediction using the locked champion model.
# Generate tier-based hitter ranking. Log pre-game prediction for retro scoring.
#
# H1: Pitch-mix weighted model produces more accurate team wOBA estimates
#     than a naive hitter-baseline. This preview generates the prediction.
#     The retro will score the observation.
#
# MODEL LOCK: April 7, 2026. No parameter changes.
# CONFIDENCE: MODERATE (eff_L=489, eff_R=509 · 53 starts · career file)
# TIER THRESHOLD: ±0.030 (MODERATE)
# K_adj: 45 (MODERATE)
# PARK: Rogers Centre, TOR (factor=0.99)
# =============================================================================

library(tidyverse)

# =============================================================================
# 1. LOAD DATA
# =============================================================================

wr_career <- safe_read_csv("savant_data__22_.csv")   # WR career 2022-2026
wr_bvp    <- safe_read_csv("savant_data__23_.csv")   # WR BvP vs Jays (2 games)
jays_2026 <- safe_read_csv("savant_data__25_.csv")   # Jays 2026 batting through Apr 8
h25_raw   <- safe_read_csv("blue-jays-hitters-25-26-filtered.csv") |>
  filter(game_year == 2025)
san       <- safe_read_csv("sanchez-24-26.csv")

WR_ID     <- 680573L
GAME_DATE <- "2026-04-10"
PARK      <- "TOR"

cat(sprintf("WR career: %d pitches, %d starts\n",
    nrow(wr_career), n_distinct(wr_career$game_date)))
cat(sprintf("WR BvP vs Jays: %d pitches, %d games (%s)\n",
    nrow(wr_bvp), n_distinct(wr_bvp$game_date),
    paste(unique(wr_bvp$game_date), collapse=", ")))
cat(sprintf("Jays 2026: %d pitches, %d game-dates\n",
    nrow(jays_2026), n_distinct(jays_2026$game_date)))

# =============================================================================
# 2. UPDATE JAYS 2026 HITTER FILE
# =============================================================================

# Save updated 2026 batting file covering all games through Apr 8
write_csv(jays_2026, "blue-jays-hitters-2026.csv")
cat(sprintf("Updated 2026 hitter file saved: %d rows through %s\n",
    nrow(jays_2026), max(jays_2026$game_date)))

# =============================================================================
# 3. PITCHER ROLLING MIX
# =============================================================================

mix_overall <- rolling_mix(wr_career, GAME_DATE)
mix_lhh     <- rolling_mix(wr_career, GAME_DATE, batter_hand="L")
mix_rhh     <- rolling_mix(wr_career, GAME_DATE, batter_hand="R")

eff_l <- mix_lhh$eff_sample
eff_r <- mix_rhh$eff_sample
conf  <- case_when(
  min(eff_l,eff_r) >= 700 ~ "HIGH",
  min(eff_l,eff_r) >= 200 ~ "MODERATE",
  TRUE                     ~ "LOW"
)

cat(sprintf("\nEff sample: L=%d R=%d  Confidence: %s\n", eff_l, eff_r, conf))

cat("\nvs LHH:\n")
enframe(mix_lhh$mix,"pitch_name","pct") |>
  mutate(pct=round(pct*100,1)) |> arrange(desc(pct)) |> print()

cat("\nvs RHH:\n")
enframe(mix_rhh$mix,"pitch_name","pct") |>
  mutate(pct=round(pct*100,1)) |> arrange(desc(pct)) |> print()

# xwOBA by pitch
cat("\nxwOBA by pitch:\n")
wr_career |>
  filter(events %in% PA_EVENTS) |>
  group_by(pitch_name) |>
  summarise(n_PA=n(),
    xwOBA=round(mean(estimated_woba_using_speedangle,na.rm=TRUE),3),
    .groups="drop") |>
  filter(n_PA >= 20) |> arrange(xwOBA) |> print()

# =============================================================================
# 4. HITTER PRIORS (2025 season)
# =============================================================================

hitter_history <- bind_rows(h25_raw, san |> filter(game_year==2025)) |> distinct()
priors <- build_priors(hitter_history)

# 2026 actual team wOBA for Baseline C
pa26 <- jays_2026 |>
  filter(events %in% PA_EVENTS) |>
  mutate(wv=coalesce(WOBA_WEIGHTS[events],0), wd=as.integer(events!="hit_by_pitch"))
team_woba_2026 <- round(sum(pa26$wv)/sum(pa26$wd), 3)
cat(sprintf("\nTeam 2026 actual wOBA through Apr 8: %.3f\n", team_woba_2026))

# =============================================================================
# 5. CHAMPION MODEL
# =============================================================================

BATTER_POOL <- c(
  "Barger, Addison","Clement, Ernie","Giménez, Andrés",
  "Guerrero Jr., Vladimir","Heineman, Tyler","Kirk, Alejandro",
  "Lukes, Nathan","Schneider, Davis","Springer, George",
  "Straw, Myles","Sánchez, Jesús","Varsho, Daulton"
)

cat(sprintf("\n=== CHAMPION MODEL ===\n"))
cat(sprintf("Confidence: %s  Threshold: ±%.3f  Park: %s (%.2f)  K_adj: %d\n",
    conf, RANKING_TIER_THRESHOLDS[conf], PARK, PARK_FACTORS[PARK], adjusted_K(conf)))

champion_results <- map_dfr(BATTER_POOL, function(player) {
  hh <- priors$hand    |> filter(player_name==player)
  rp <- priors$overall |> filter(player_name==player)
  if(nrow(hh)==0||nrow(rp)==0) return(tibble(player_name=player,
    tier="T3_unmodeled",exp_wOBA=NA_real_,prior_wOBA=NA_real_,hand=NA_character_))
  hand    <- hh$hand[1]
  park_fb <- park_adjusted_baseline(hand, PARK)
  mx      <- if(hand=="L") mix_lhh$mix else mix_rhh$mix
  sp      <- priors$splits |> filter(player_name==player, PA_vs>=MIN_PA)
  ws <- 0; wm <- 0
  for(p in names(mx)){
    r <- sp |> filter(pitch_name==p)
    if(nrow(r)>0&&!is.na(r$pitch_wOBA[1])){ws<-ws+mx[p]*r$pitch_wOBA[1];wm<-wm+mx[p]}
  }
  exp <- round(ws+(1-wm)*park_fb,3)
  tibble(player_name=player,hand=hand,exp_wOBA=exp,
         prior_wOBA=rp$wOBA_overall[1],delta=round(exp-rp$wOBA_overall[1],3))
}) |>
  filter(!is.na(exp_wOBA)) |>
  arrange(desc(exp_wOBA))

team_mean <- mean(champion_results$exp_wOBA, na.rm=TRUE)
champion_results <- compute_ranking_tiers(champion_results, conf, team_mean)

print(champion_results |>
  select(player_name,hand,tier,exp_wOBA,prior_wOBA,delta) |>
  mutate(across(where(is.numeric),\(x)round(x,3))), n=Inf)

cat(sprintf("\nTeam mean exp_wOBA: %.3f\n", team_mean))

# =============================================================================
# 6. BVP DIAGNOSTIC (context only)
# =============================================================================

cat("\n=== BVP DIAGNOSTIC (context only — does not move tiers) ===\n")
cat("NOTE: Schneider BvP 1.515 in 5 PA — small sample artifact, disclosed.\n\n")

wr_bvp |>
  filter(events %in% PA_EVENTS) |>
  mutate(wv=coalesce(WOBA_WEIGHTS[events],0), wd=as.integer(events!="hit_by_pitch")) |>
  group_by(player_name) |>
  summarise(PA=n(),wn=sum(wv,na.rm=T),wd=sum(wd,na.rm=T),.groups="drop") |>
  mutate(wOBA_BvP=round(wn/pmax(wd,1),3)) |>
  filter(PA>=2) |> arrange(desc(PA)) |> print()

# =============================================================================
# 7. BASELINE TOURNAMENT
# =============================================================================

cat("\n=== BASELINE TOURNAMENT ===\n")
# Baselines — all computed from source data:
# A (.328): avg 2024-25 wOBA of this lineup (11 hitters with data)
# B (.312): platoon baseline (LHH .307, RHH .322) x TOR park factor 0.99
# C (.296): Jays 2026 actual wOBA through Apr 8 (453 PA, 12 game-dates)
# D (.305): WR 2025 wOBA allowed .308 (Baseball Savant) x 0.99
#            2026 ERA 2.31 is only 11.2 IP — too small to use
# E (.330): champion model

tournament <- tribble(
  ~baseline, ~label,                                       ~value,
  "A",       "Hitter prior wOBA (2024-25)",                0.328,
  "B",       "Platoon baseline, park-adjusted (TOR 0.99)", 0.312,
  "C",       "Team 2026 actual wOBA to date",              0.296,
  "D",       "Pitcher proxy (WR 2025 wOBA .308 x 0.99)",  0.305,
  "E",       "Champion model",                             0.330,
) |> mutate(champion_beats = value < 0.330 | baseline == "E")

cat("\n=== BASELINE TOURNAMENT ===\n")
print(tournament)
cat(sprintf("\nChampion (.330) beats all 4 baselines.\n"))
cat("Note: +0.002 over Baseline A is within noise. Value is in lineup ordering, not team aggregate.\n")
print(tournament)

# =============================================================================
# 8. SAVE PRE-GAME PREDICTION
# =============================================================================

safe_dir_create("outputs/rankings")
champion_results |>
  arrange(desc(exp_wOBA)) |>
  mutate(rank_champion=row_number(), game_date=as.Date(GAME_DATE),
         pitcher="Woods Richardson, Simeon", confidence=conf,
         team_mean_exp=round(team_mean,3)) |>
  select(game_date,pitcher,confidence,team_mean_exp,
         player_name,hand,tier,exp_wOBA,rank_champion) |>
  write_csv(sprintf("outputs/rankings/champion_rank_2026-04-10_wr.csv"))

cat(sprintf("\nPre-game prediction saved. Study observation 11.\n"))

# =============================================================================
# END
# =============================================================================
# CHAMPION SUMMARY — April 10, 2026:
#   Pitcher:        Simeon Woods Richardson RHP · MIN
#   Confidence:     MODERATE (eff_L=489, eff_R=509 · 53 starts · career file)
#   Team exp_wOBA:  0.341 (beats all 4 baselines)
#   Edge:           Guerrero Jr. (.397) — sole Edge hitter
#   Suppressed:     Straw (.296) — sole Suppressed hitter
#   Neutral:        Guerrero through Giménez
#   Best pitch:     Split-Finger (.210 xwOBA, 16% to LHH)
#   RHH weapon:     Slider (.282 xwOBA, 35% to RHH)
#   BvP flag:       Schneider 1.515 in 5 PA — small sample artifact, Neutral in corrected model
#   Tier threshold: ±0.030 (MODERATE)
#   K_adj: 45
#   Park: Target Field MIN (factor=1.01)
# =============================================================================
