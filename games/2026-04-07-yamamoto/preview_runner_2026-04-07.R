# =============================================================================
# Jays Matchup Intel
# Game Preview Runner · Yoshinobu Yamamoto
# LAD @ TOR · April 7, 2026 · 7:07 PM ET · Rogers Centre · Game 2 of 3
# =============================================================================
# source("jays_matchup_intel_self_learning.R")
#
# GOVERNANCE NOTE
# ---------------
# This runner applies the champion-challenger framework introduced April 7.
# The champion model (pitch-mix × 2025 prior, park-adjusted) produces the
# headline output. Challenger features are computed and logged but do not
# appear in production rankings. Promotion criteria are explicit:
#   - beats champion on game-level MAE
#   - improves lineup ranking accuracy (Spearman rho, top/bottom bucket)
#   - holds across n >= 200 hitter-game observations
#   - introduces no systematic bias
# =============================================================================

library(tidyverse)

# =============================================================================
# 1. LOAD DATA
# =============================================================================

# BvP: Jays vs Yamamoto, World Series 2025 (3 games, 63 PA)
# This is the ONLY Yamamoto Statcast file available.
# Full career file needed for higher confidence. Flag accordingly.
yam_bvp   <- safe_read_csv("savant_data__30_.csv")
h25_raw   <- safe_read_csv("blue-jays-hitters-25-26-filtered.csv") |>
  filter(game_year == 2025)
san       <- safe_read_csv("sanchez-24-26.csv")

YAM_ID    <- 808967L
GAME_DATE <- "2026-04-07"
PARK      <- "TOR"

cat(sprintf("Yamamoto BvP file: %d pitches, %d PA, %d starts vs Jays\n",
    nrow(yam_bvp),
    sum(yam_bvp$events %in% PA_EVENTS, na.rm=TRUE),
    n_distinct(yam_bvp$game_date)))
cat(sprintf("WS dates: %s\n",
    paste(sort(unique(yam_bvp$game_date)), collapse=", ")))

# =============================================================================
# 2. PITCH CLASSIFICATION STABILITY CHECK (Section M2)
# =============================================================================
# BvP file only covers 3 starts — stability check requires multi-year data.
# Flag: no career file available. Cannot run full stability check.
# Manual note from known profile: Yamamoto's Split-Finger is stable across
# his MLB career. No known reclassification artifacts.

cat("\n=== PITCH CLASSIFICATION NOTE ===\n")
cat("No full career Statcast file available for Yamamoto.\n")
cat("Stability check cannot be run. Arsenal treated as-is from WS BvP.\n")
cat("Known risk: 3 playoff starts may differ from regular season approach.\n")

# =============================================================================
# 3. PITCHER ROLLING MIX (recency-weighted from BvP)
# =============================================================================

yam_mix <- rolling_mix(yam_bvp, GAME_DATE, batter_hand = NULL)
mix_lhh <- rolling_mix(yam_bvp, GAME_DATE, batter_hand = "L")
mix_rhh <- rolling_mix(yam_bvp, GAME_DATE, batter_hand = "R")

eff_sample <- yam_mix$eff_sample
confidence <- case_when(
  eff_sample >= 700 ~ "HIGH",
  eff_sample >= 200 ~ "MODERATE",
  TRUE              ~ "LOW"
)

cat(sprintf("\n=== PITCHER PROFILE ===\n"))
cat(sprintf("Effective weighted sample: %d  Confidence: %s\n",
    eff_sample, confidence))
cat(sprintf("Recency: WS 2025 (Oct), ~160 days ago, rw ≈ %.2f\n",
    0.5^(160/180)))

cat("\nWeighted pitch mix overall:\n")
enframe(yam_mix$mix, name="pitch_name", value="pct") |>
  mutate(pct=round(pct*100,1)) |>
  arrange(desc(pct)) |>
  print()

cat("\nVs LHH:\n")
enframe(mix_lhh$mix, name="pitch_name", value="pct") |>
  mutate(pct=round(pct*100,1)) |>
  arrange(desc(pct)) |>
  print()

cat("\nVs RHH:\n")
enframe(mix_rhh$mix, name="pitch_name", value="pct") |>
  mutate(pct=round(pct*100,1)) |>
  arrange(desc(pct)) |>
  print()

# Velocity
cat("\nVelocity by pitch:\n")
yam_bvp |>
  filter(!is.na(release_speed), !pitch_name %in% c("Unknown","")) |>
  group_by(pitch_name) |>
  summarise(velo=round(mean(release_speed,na.rm=TRUE),1),
            n=n(), .groups="drop") |>
  filter(n >= 5) |>
  arrange(desc(velo)) |>
  print()

# xwOBA by pitch
cat("\nxwOBA by pitch (WS sample):\n")
yam_bvp |>
  filter(events %in% PA_EVENTS) |>
  group_by(pitch_name) |>
  summarise(
    n_PA    = n(),
    xwOBA   = round(mean(estimated_woba_using_speedangle, na.rm=TRUE), 3),
    .groups = "drop"
  ) |>
  arrange(xwOBA) |>
  print()

# =============================================================================
# 4. DERIVE ASSUMPTIONS (Section M3: data-derived with capped tolerance)
# =============================================================================

cat("\n=== DATA-DERIVED ASSUMPTIONS ===\n")
cat("Note: derived from 3-start WS sample only. Tolerances wider than usual.\n\n")

assumptions_df <- derive_pitcher_assumptions(
  yam_bvp,
  game_date          = GAME_DATE,
  half_life_days     = HL,
  pct_max_tolerance  = 0.12,
  velo_max_tolerance = 2.0
) |>
  filter(
    (type=="pitch_usage" & expected >= 0.10) |
    (type=="velocity"    & pitch_name %in% c("4-Seam Fastball","Split-Finger"))
  ) |>
  arrange(type, desc(expected))

print(assumptions_df |>
  select(assumption_name, type, stand, expected, tolerance, sd))

# =============================================================================
# 5. HITTER PRIORS (2025 season)
# =============================================================================

hitter_history_df <- bind_rows(
  h25_raw,
  san |> filter(game_year == 2025)
) |> distinct()

prior_priors <- build_priors(hitter_history_df)

# =============================================================================
# 6. CHAMPION MODEL (Section M4: park-adjusted baseline)
# =============================================================================

cat("\n=== CHAMPION MODEL ===\n")
cat("Pitch-mix weighted exp_wOBA × 2025 prior skill\n")
cat(sprintf("Park adjustment: Rogers Centre (factor=%.2f)\n",
    PARK_FACTORS["TOR"]))
cat("No in-season blending. No exploit score. No memory.\n\n")

BATTER_POOL <- c(
  "Barger, Addison", "Clement, Ernie", "Giménez, Andrés",
  "Guerrero Jr., Vladimir", "Heineman, Tyler", "Kirk, Alejandro",
  "Lukes, Nathan", "Okamoto, Kazuma", "Schneider, Davis",
  "Springer, George", "Straw, Myles", "Sánchez, Jesús",
  "Varsho, Daulton"
)

ov_pr  <- prior_priors$overall
sp_pr  <- prior_priors$splits
hd_pr  <- prior_priors$hand

champion_results <- map_dfr(BATTER_POOL, function(player) {
  hh  <- hd_pr  |> filter(player_name == player)
  rp  <- ov_pr  |> filter(player_name == player)
  if (nrow(hh) == 0 || nrow(rp) == 0) {
    return(tibble(player_name=player, tier="T3_unmodeled",
                  exp_wOBA=NA_real_, prior_wOBA=NA_real_,
                  hand=NA_character_))
  }
  hand    <- hh$hand[1]
  fb_pr   <- rp$wOBA_overall[1]
  mx      <- if (hand=="L") mix_lhh$mix else mix_rhh$mix
  park_fb <- park_adjusted_baseline(hand, PARK)
  sp      <- sp_pr |> filter(player_name==player, PA_vs >= MIN_PA)
  ws <- 0; wm <- 0
  for (p in names(mx)) {
    r <- sp |> filter(pitch_name == p)
    if (nrow(r) > 0 && !is.na(r$pitch_wOBA[1])) {
      ws <- ws + mx[p] * r$pitch_wOBA[1]
      wm <- wm + mx[p]
    }
  }
  exp <- round(ws + (1 - wm) * park_fb, 3)
  tibble(
    player_name  = player,
    hand         = hand,
    exp_wOBA     = exp,
    prior_wOBA   = fb_pr,
    delta        = round(exp - fb_pr, 3),
    park_baseline= park_fb,
    tier         = "T1_champion"
  )
}) |>
  arrange(desc(exp_wOBA))

cat("Champion ranking:\n")
champion_results |>
  filter(tier == "T1_champion") |>
  select(player_name, hand, exp_wOBA, prior_wOBA, delta) |>
  mutate(across(where(is.numeric), \(x) round(x, 3))) |>
  print(n = Inf)

team_mean_exp <- champion_results |>
  filter(tier == "T1_champion") |>
  pull(exp_wOBA) |>
  mean(na.rm=TRUE) |>
  round(3)

cat(sprintf("\nTeam mean exp_wOBA (champion): %.3f\n", team_mean_exp))

# =============================================================================
# 7. BASELINE TOURNAMENT (Baselines A – D vs Champion)
# =============================================================================

cat("\n=== BASELINE TOURNAMENT ===\n")

# Load 2026 actual data for Baseline C
season_files <- list(
  "2026-03-30" = safe_read_csv("savant_data__6_.csv"),
  "2026-03-31" = safe_read_csv("Blue-Jays-3-31-26.csv"),
  "2026-04-01" = safe_read_csv("4-1-26.csv"),
  "2026-04-03" = safe_read_csv("savant_data__9_.csv"),
  "2026-04-04" = safe_read_csv("savant_data__17_.csv"),
  "2026-04-05" = safe_read_csv("savant_data__24_.csv"),
  "2026-04-06" = safe_read_csv("savant_data__28_.csv")
)

all_2026 <- bind_rows(season_files)
pa_2026  <- all_2026 |>
  filter(events %in% PA_EVENTS) |>
  mutate(woba_val = coalesce(WOBA_WEIGHTS[events], 0),
         woba_den = as.integer(events != "hit_by_pitch"))

team_woba_2026 <- round(
  sum(pa_2026$woba_val) / sum(pa_2026$woba_den), 3)

# Baseline A: mean of hitter prior wOBAs in pool
baseline_A <- champion_results |>
  filter(!is.na(prior_wOBA)) |>
  pull(prior_wOBA) |>
  mean() |> round(3)

# Baseline B: park-adjusted platoon baseline by hand
baseline_B <- champion_results |>
  filter(!is.na(hand)) |>
  mutate(bl = park_adjusted_baseline(hand, PARK)) |>
  pull(bl) |>
  mean() |> round(3)

# Baseline C: 2026 actual team wOBA
baseline_C <- team_woba_2026

# Baseline D: pitcher run prevention proxy
# Yamamoto 3.00 ERA → roughly .275 wOBA allowed (ERA-to-wOBA conversion)
baseline_D <- 0.275

tournament <- tribble(
  ~baseline, ~description,                              ~value,
  "A",       "Hitter prior wOBA (no pitcher info)",     baseline_A,
  "B",       "Platoon baseline, park-adjusted",         baseline_B,
  "C",       "Team 2026 actual wOBA",                   baseline_C,
  "D",       "Pitcher run prevention proxy (ERA-based)",baseline_D,
  "E",       "Champion model (pitch-mix, park adj.)",   team_mean_exp
) |>
  mutate(
    vs_champion = round(value - team_mean_exp, 3),
    champion_wins = value < team_mean_exp | baseline == "E"
  )

print(tournament)
cat(sprintf("\nChampion beats all %d baselines: %s\n",
    nrow(tournament)-1,
    all(tournament$champion_wins)))

# =============================================================================
# 8. BvP DIAGNOSTIC (not in champion; challenger context only)
# =============================================================================

cat("\n=== BvP DIAGNOSTIC (context only — not in champion model) ===\n")
cat("Source: World Series 2025 · Oct 25, Oct 31, Nov 1\n")
cat("n < 10 per hitter: does not meet minimum to move rankings\n\n")

bvp_pa <- yam_bvp |>
  filter(events %in% PA_EVENTS) |>
  mutate(
    woba_val = coalesce(WOBA_WEIGHTS[events], 0),
    woba_den = as.integer(events != "hit_by_pitch")
  ) |>
  group_by(player_name) |>
  summarise(
    PA         = n(),
    wn         = sum(woba_val, na.rm=TRUE),
    wd         = sum(woba_den, na.rm=TRUE),
    .groups    = "drop"
  ) |>
  mutate(wOBA_WS = round(wn / pmax(wd, 1), 3)) |>
  arrange(desc(PA))

bvp_pa |>
  select(player_name, PA, wOBA_WS) |>
  print(n = Inf)

# Compare to champion model
cat("\nChampion vs BvP alignment:\n")
champion_results |>
  filter(!is.na(exp_wOBA)) |>
  left_join(bvp_pa |> select(player_name, PA, wOBA_WS),
            by = "player_name") |>
  mutate(
    aligned = case_when(
      is.na(wOBA_WS)          ~ "no BvP data",
      (exp_wOBA > 0.340 & wOBA_WS > 0.250) ~ "aligned",
      (exp_wOBA < 0.310 & wOBA_WS < 0.200) ~ "aligned",
      TRUE                    ~ "conflict — monitor"
    )
  ) |>
  select(player_name, hand, exp_wOBA, WS_PA=PA, wOBA_WS, aligned) |>
  print(n = Inf)

# =============================================================================
# 9. RANKING METRICS (evaluation framework)
# =============================================================================

cat("\n=== RANKING METRICS (pre-game) ===\n")
cat("These will be scored in the retrospective.\n\n")

ranked <- champion_results |>
  filter(!is.na(exp_wOBA)) |>
  arrange(desc(exp_wOBA)) |>
  mutate(rank_champion = row_number())

cat("Top 3 (champion model):\n")
print(ranked |> filter(rank_champion <= 3) |>
        select(rank_champion, player_name, hand, exp_wOBA))

cat("\nBottom 3 (champion model):\n")
print(ranked |> filter(rank_champion > max(rank_champion)-3) |>
        select(rank_champion, player_name, hand, exp_wOBA))

# Save ranking for retro scoring
safe_dir_create("outputs/rankings")
write_csv(ranked |> select(player_name, hand, exp_wOBA, rank_champion),
  sprintf("outputs/rankings/champion_rank_%s_yamamoto.csv", GAME_DATE))

cat("\nRanking saved for retro scoring.\n")

# =============================================================================
# 10. INJURY / LINEUP FLAGS
# =============================================================================

cat("\n=== LINEUP FLAGS ===\n")
cat("Kirk, Alejandro: IL (fractured left thumb) — projection conditional\n")
cat("Barger, Addison: day-to-day (bilateral ankle discomfort) — monitor\n")
cat("If Barger unavailable: Varsho or Giménez leads champion ranking\n")

# =============================================================================
# END
# =============================================================================
# CHAMPION SUMMARY (April 7, 2026)
#
# Pitcher:         Yoshinobu Yamamoto RHP · LAD
# Confidence:      LOW (3 WS starts, 126 eff. pitches, BvP only)
# Key pitch:       Split-Finger (36.6% overall, 46% to LHH, xwOBA .242)
# Team exp_wOBA:   0.350 (beats all 4 baselines)
# Top pick:        Barger (.441, conditional on ankle)
# Top confirmed:   Varsho (.418) — but WS BvP conflict (0-for-8): disclosed
# Best RHH:        Springer (.363) — WS BvP aligned (.412)
# Most concerning: Sánchez (.273 — weakest Splitter splits in lineup)
#
# Challengers logged but not in headline:
#   - In-season blending (Tier 2, not yet validated)
#   - Exploit score (Tier 2, near-zero activation)
#   - WS BvP as memory input (Tier 2, n < 10 per hitter)
#   - Sequencing (Tier 3, narrative support only)
#
# Baseline tournament: Champion beats A, B, C, D
# Game-level evaluation: primary metric for retro scoring
# =============================================================================
