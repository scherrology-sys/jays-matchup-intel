# =============================================================================
# Jays Matchup Intel
# Preview Runner · Shohei Ohtani
# LAD @ TOR · April 8, 2026 · 3:07 PM ET · Game 3 of 3
# Study observation 10
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
# CONFIDENCE: MODERATE (eff_sample=388, 15 starts, career file)
# TIER THRESHOLD: ±0.030 (MODERATE)
# K_adj: 45 (MODERATE)
# =============================================================================

library(tidyverse)

# =============================================================================
# 1. LOAD DATA
# =============================================================================

ohtani_raw <- safe_read_csv("savant_data__35_.csv")   # Career file 2025-26
bvp_raw    <- safe_read_csv("savant_data__36_.csv")   # WS BvP (context only)
h25_raw    <- safe_read_csv("blue-jays-hitters-25-26-filtered.csv") |>
  filter(game_year==2025)
san        <- safe_read_csv("sanchez-24-26.csv")

OHT_ID <- 660271L; GAME_DATE <- "2026-04-08"; PARK <- "TOR"

cat(sprintf("Ohtani career file: %d pitches, %d starts, %d seasons\n",
    nrow(ohtani_raw), n_distinct(ohtani_raw$game_date), n_distinct(ohtani_raw$game_year)))
cat(sprintf("BvP (WS 2025): %d pitches · context only · not in model\n", nrow(bvp_raw)))

# =============================================================================
# 2. PITCHER ROLLING MIX
# =============================================================================

mix_overall <- rolling_mix(ohtani_raw, GAME_DATE)
mix_lhh     <- rolling_mix(ohtani_raw, GAME_DATE, batter_hand="L")
mix_rhh     <- rolling_mix(ohtani_raw, GAME_DATE, batter_hand="R")

eff_t  <- mix_overall$eff_sample
conf   <- if(eff_t>=700)"HIGH" else if(eff_t>=200)"MODERATE" else "LOW"
k_adj  <- adjusted_K(conf)  # 45 at MODERATE

cat(sprintf("\nEff sample: %d  Confidence: %s  K_adj: %d\n", eff_t, conf, k_adj))
cat(sprintf("Tier threshold: ±%.3f\n", RANKING_TIER_THRESHOLDS[conf]))

cat("\nvs LHH weighted mix:\n")
enframe(mix_lhh$mix,"pitch_name","pct") |>
  mutate(pct=round(pct*100,1)) |> arrange(desc(pct)) |> print()

cat("\nvs RHH weighted mix:\n")
enframe(mix_rhh$mix,"pitch_name","pct") |>
  mutate(pct=round(pct*100,1)) |> arrange(desc(pct)) |> print()

# xwOBA by pitch
cat("\nxwOBA by pitch:\n")
ohtani_raw |> filter(events %in% PA_EVENTS) |>
  group_by(pitch_name) |>
  summarise(n_PA=n(),
    xwOBA=round(mean(estimated_woba_using_speedangle,na.rm=T),3),.groups="drop") |>
  filter(n_PA>=8) |> arrange(xwOBA) |> print()

# =============================================================================
# 3. HITTER PRIORS
# =============================================================================

hitter_history <- bind_rows(h25_raw, san |> filter(game_year==2025)) |> distinct()
priors <- build_priors(hitter_history)

# =============================================================================
# 4. CHAMPION MODEL
# =============================================================================

BATTER_POOL <- c(
  "Barger, Addison","Clement, Ernie","Giménez, Andrés",
  "Guerrero Jr., Vladimir","Heineman, Tyler","Kirk, Alejandro",
  "Lukes, Nathan","Schneider, Davis","Springer, George",
  "Straw, Myles","Sánchez, Jesús","Varsho, Daulton"
)

cat(sprintf("\n=== CHAMPION MODEL (locked) ===\n"))
cat(sprintf("Confidence: %s  Threshold: ±%.3f  K_adj: %d\n",
    conf, RANKING_TIER_THRESHOLDS[conf], k_adj))

champion_results <- map_dfr(BATTER_POOL, function(player) {
  hh <- priors$hand |> filter(player_name==player)
  rp <- priors$overall |> filter(player_name==player)
  if(nrow(hh)==0||nrow(rp)==0) return(tibble(player_name=player,tier="T3_unmodeled",
    exp_wOBA=NA_real_,prior_wOBA=NA_real_,hand=NA_character_))
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
}) |> filter(!is.na(exp_wOBA)) |> arrange(desc(exp_wOBA))

team_mean <- mean(champion_results$exp_wOBA,na.rm=T)
champion_results <- compute_ranking_tiers(champion_results, conf, team_mean)

cat(sprintf("\nTeam mean exp_wOBA: %.3f\n", team_mean))
print(champion_results |> select(player_name,hand,tier,exp_wOBA,prior_wOBA,delta), n=Inf)

# =============================================================================
# 5. BASELINE TOURNAMENT
# =============================================================================

# 2026 actual team wOBA through Apr 7
all_2026 <- map_dfr(list(
  safe_read_csv("savant_data__6_.csv"), safe_read_csv("Blue-Jays-3-31-26.csv"),
  safe_read_csv("4-1-26.csv"), safe_read_csv("savant_data__9_.csv"),
  safe_read_csv("savant_data__17_.csv"), safe_read_csv("savant_data__24_.csv"),
  safe_read_csv("savant_data__28_.csv"), safe_read_csv("savant_data__33_.csv")
), ~.x)
pa26 <- all_2026 |> filter(events %in% PA_EVENTS) |>
  mutate(wv=coalesce(WOBA_WEIGHTS[events],0), wd=as.integer(events!="hit_by_pitch"))
team_woba_2026 <- round(sum(pa26$wv)/sum(pa26$wd),3)

tournament <- run_baseline_tournament(
  champion_results, team_woba_2026, pitcher_era=2.5,
  park_code=PARK, confidence_tier=conf)
cat("\n=== BASELINE TOURNAMENT ===\n")
print(tournament)

# =============================================================================
# 6. BvP CONTEXT (not in model)
# =============================================================================

cat("\n=== BvP CONTEXT (WS 2025 — informational only) ===\n")
bvp_raw |> filter(events %in% PA_EVENTS) |>
  mutate(wv=coalesce(WOBA_WEIGHTS[events],0), wd=as.integer(events!="hit_by_pitch")) |>
  group_by(player_name) |>
  summarise(PA=n(),wn=sum(wv,na.rm=T),wd=sum(wd,na.rm=T),.groups="drop") |>
  mutate(wOBA_WS=round(wn/pmax(wd,1),3)) |>
  left_join(champion_results|>select(player_name,tier,exp_wOBA),by="player_name") |>
  select(player_name,PA,wOBA_WS,tier,exp_wOBA) |> arrange(desc(PA)) |> print()

# =============================================================================
# 7. SAVE PRE-GAME PREDICTION
# =============================================================================

safe_dir_create("outputs/rankings")
champion_results |>
  arrange(desc(exp_wOBA)) |>
  mutate(rank_champion=row_number(), game_date=as.Date(GAME_DATE),
         pitcher="Ohtani, Shohei", confidence=conf,
         team_mean_exp=round(team_mean,3)) |>
  select(game_date,pitcher,confidence,team_mean_exp,
         player_name,hand,tier,exp_wOBA,rank_champion) |>
  write_csv(sprintf("outputs/rankings/champion_rank_%s_ohtani.csv", GAME_DATE))

cat(sprintf("\nPre-game prediction saved for retro scoring.\n"))
cat(sprintf("Study observation 10. Running n will be ~70 after retro.\n"))

# =============================================================================
# END
# =============================================================================
# CHAMPION SUMMARY — April 8, 2026:
#   Pitcher:       Shohei Ohtani RHP · LAD
#   Confidence:    MODERATE (eff=388, 15 starts, career file)
#   Team exp_wOBA: 0.335 (beats all 4 baselines)
#   Edge:          Springer (.440) — WS BvP aligned
#   Suppressed:    Kirk (.305 IL), Clement (.275)
#   Neutral:       Guerrero through Sánchez
#   Key pitch:     Sweeper (.198 xwOBA, 37% to RHH)
#   Hittable:      Cutter (.505 xwOBA, 8% to LHH)
#   Tier threshold: ±0.030 (MODERATE)
#   K_adj: 45 (90% CI width 0.231)
# =============================================================================
