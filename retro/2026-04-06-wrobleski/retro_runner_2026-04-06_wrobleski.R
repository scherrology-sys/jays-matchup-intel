# =============================================================================
# Jays Matchup Intel
# Retrospective Runner · Justin Wrobleski
# LAD @ TOR · April 6, 2026 · Final: LAD 14, TOR 2
# =============================================================================
# source("jays_matchup_intel_self_learning.R")
# =============================================================================

library(tidyverse)

# =============================================================================
# 1. LOAD DATA
# =============================================================================

jays_bat  <- safe_read_csv("savant_data__28_.csv")   # Jays batting Apr 6
dodg_pit  <- safe_read_csv("savant_data__29_.csv")   # Dodgers pitching Apr 6
wrob_raw  <- safe_read_csv("savant_data__14_.csv")   # Wrobleski career

h25_raw   <- safe_read_csv("blue-jays-hitters-25-26-filtered.csv") |> filter(game_year==2025)
san       <- safe_read_csv("sanchez-24-26.csv")
h24       <- safe_read_csv("savant_data__16_.csv")

WROB_ID   <- 680736L
GAME_DATE <- "2026-04-06"

# =============================================================================
# 2. FILTER TO WROBLESKI ONLY
# =============================================================================

wrob_game <- jays_bat |> filter(pitcher == WROB_ID)

cat(sprintf("Wrobleski pitches tonight: %d\n", nrow(wrob_game)))
cat(sprintf("Innings pitched: %d\n", max(wrob_game$inning, na.rm=TRUE)))

# =============================================================================
# 3. ACTUAL PITCH MIX
# =============================================================================

cat("\n=== ACTUAL PITCH MIX (overall) ===\n")
wrob_game |>
  count(pitch_name) |>
  mutate(pct = round(n/sum(n)*100,1)) |>
  arrange(desc(pct)) |>
  print()

cat("\n=== vs LHH ===\n")
wrob_game |> filter(stand=="L") |>
  count(pitch_name) |>
  mutate(pct = round(n/sum(n)*100,1)) |>
  arrange(desc(pct)) |>
  print()

cat("\n=== vs RHH ===\n")
wrob_game |> filter(stand=="R") |>
  count(pitch_name) |>
  mutate(pct = round(n/sum(n)*100,1)) |>
  arrange(desc(pct)) |>
  print()

# =============================================================================
# 4. VELOCITY CHECK
# =============================================================================

cat("\n=== VELOCITY ===\n")
wrob_game |>
  filter(pitch_name %in% c("4-Seam Fastball","Slider","Sinker")) |>
  group_by(pitch_name) |>
  summarise(velo_actual = round(mean(release_speed, na.rm=TRUE),1),
            .groups="drop") |>
  mutate(velo_career = c(95.7, 94.9, 87.6)[match(pitch_name,
    c("4-Seam Fastball","Sinker","Slider"))],
    delta = round(velo_actual - velo_career, 1)) |>
  print()

# =============================================================================
# 5. HITTER ACTUALS vs WROBLESKI
# =============================================================================

cat("\n=== HITTER ACTUALS vs WROBLESKI (>=2 PA) ===\n")
actuals <- wrob_game |>
  filter(events %in% PA_EVENTS) |>
  mutate(
    woba_val = coalesce(WOBA_WEIGHTS[events], 0),
    woba_den = as.integer(events != "hit_by_pitch")
  ) |>
  group_by(player_name) |>
  summarise(PA=n(), wn=sum(woba_val), wd=sum(woba_den),
            outcomes=paste(events, collapse=", "), .groups="drop") |>
  mutate(actual_wOBA = round(wn/pmax(wd,1),3)) |>
  filter(PA >= 2) |>
  arrange(desc(actual_wOBA))

print(actuals |> select(player_name, PA, actual_wOBA, outcomes))

# =============================================================================
# 6. PREDICTIONS vs ACTUALS (Model A and B)
# =============================================================================

# Build priors
p25 <- build_priors(bind_rows(h25_raw, san |> filter(game_year==2025)) |> distinct())

# In-season splits: all games before Apr 6
season_games <- list(
  list(date="2026-03-30", data=safe_read_csv("savant_data__6_.csv")),
  list(date="2026-03-31", data=safe_read_csv("Blue-Jays-3-31-26.csv")),
  list(date="2026-04-01", data=safe_read_csv("4-1-26.csv")),
  list(date="2026-04-03", data=safe_read_csv("savant_data__9_.csv")),
  list(date="2026-04-04", data=safe_read_csv("savant_data__17_.csv")),
  list(date="2026-04-05", data=safe_read_csv("savant_data__24_.csv"))
)

season <- build_season_splits_bt(season_games, GAME_DATE)

# Rolling pitcher mix at game date
ml <- rolling_mix(wrob_raw, GAME_DATE, "L")$mix
mr <- rolling_mix(wrob_raw, GAME_DATE, "R")$mix

# Named predictions from preview
PREDICTIONS <- tribble(
  ~player_name,              ~hand, ~exp_blend,
  "Guerrero Jr., Vladimir",  "R",   0.416,
  "Heineman, Tyler",         "L",   0.413,
  "Straw, Myles",            "R",   0.385,
  "Springer, George",        "R",   0.370,
  "Kirk, Alejandro",         "R",   0.355,
  "Varsho, Daulton",         "L",   0.338,
  "Schneider, Davis",        "R",   0.301,
  "Sánchez, Jesús",          "L",   0.279,
  "Barger, Addison",         "L",   0.276,
  "Clement, Ernie",          "R",   0.277,
  "Lukes, Nathan",            "L",   0.262,
  "Giménez, Andrés",         "L",   0.275,
)

results <- PREDICTIONS |>
  left_join(actuals |> select(player_name, PA, actual_wOBA), by="player_name") |>
  filter(!is.na(actual_wOBA), PA >= 2) |>
  mutate(
    err        = round(abs(exp_blend - actual_wOBA), 3),
    residual   = round(exp_blend - actual_wOBA, 3),
    miss_type  = case_when(
      err < 0.100 ~ "within_variance",
      player_name == "Guerrero Jr., Vladimir" ~ "assumption_miss",
      player_name == "Heineman, Tyler"        ~ "assumption_miss",
      TRUE                                   ~ "variance_miss"
    )
  ) |>
  arrange(desc(err))

cat("\n=== PREDICTION vs ACTUAL ===\n")
print(results |> select(player_name, hand, exp_blend, actual_wOBA, err, residual, miss_type),
      n=Inf)

cat(sprintf("\nMAE:  %.3f\n", mean(results$err)))
cat(sprintf("Bias: %+.3f  (positive = model over-predicted)\n", mean(results$residual)))

# =============================================================================
# 7. ASSUMPTION SCORING
# =============================================================================

slider_lhh_actual <- wrob_game |>
  filter(stand=="L") |>
  summarise(pct = mean(pitch_name=="Slider")) |>
  pull(pct)

sinker_lhh_actual <- wrob_game |>
  filter(stand=="L") |>
  summarise(pct = mean(pitch_name=="Sinker")) |>
  pull(pct)

velo_actual <- wrob_game |>
  filter(pitch_name=="4-Seam Fastball") |>
  pull(release_speed) |>
  mean(na.rm=TRUE)

assumptions_scored <- tribble(
  ~assumption,                         ~expected, ~actual,     ~tolerance, ~verdict,
  "slider_to_lhh",                     0.339,     slider_lhh_actual, 0.06, "held",
  "fastball_velo",                     95.7,      velo_actual,       1.5,  "held",
  "sinker_to_lhh",                     0.330,     sinker_lhh_actual, 0.10, "failed"
) |>
  mutate(
    delta   = round(actual - expected, 3),
    verdict = if_else(abs(delta) <= tolerance, "held", "failed")
  )

cat("\n=== ASSUMPTION SCORING ===\n")
print(assumptions_scored)

# =============================================================================
# 8. WRITE MEMORY
# =============================================================================

safe_dir_create("model_memory")

# Hitter audit
hitter_audit <- results |>
  mutate(
    game_date    = as.Date(GAME_DATE),
    pitcher      = "Wrobleski, Justin",
    pitcher_hand = "L",
    pitcher_id   = WROB_ID
  ) |>
  select(game_date, pitcher, pitcher_hand, pitcher_id,
         player_name, hand, PA, exp_blend, actual_wOBA, err, residual, miss_type)

write_csv(hitter_audit,
  "model_memory/hitter_audit_2026-04-06_wrobleski.csv")

# Assumption audit
assumption_audit <- assumptions_scored |>
  mutate(
    game_date    = as.Date(GAME_DATE),
    pitcher      = "Wrobleski, Justin",
    pitcher_hand = "L",
    pitcher_archetype = "LHP_starter_lad"
  ) |>
  select(game_date, pitcher, pitcher_hand, pitcher_archetype,
         assumption, expected, actual, delta, tolerance, verdict)

write_csv(assumption_audit,
  "model_memory/assumption_audit_2026-04-06_wrobleski.csv")

cat("\nMemory files written.\n")

# =============================================================================
# 9. WHAT THE MEMORY SYSTEM LEARNS
# =============================================================================

cat("\n=== MEMORY SYSTEM UPDATES ===\n")
cat("1. Sinker assumption: FAILED (-22.5pp deviation)\n")
cat("   → Lower reliability weight for sinker_sequencing_lhh on LHP profiles\n")
cat("   → Widen CI on Sinker exposure component for next similar pitcher type\n\n")

cat("2. Fastball velo: HELD\n")
cat("   → Reliability weight unchanged\n\n")

cat("3. Slider to LHH: HELD (within tolerance)\n")
cat("   → Context note: Slider held but without the Sinker setup,\n")
cat("     the sequence the preview anticipated did not occur\n\n")

cat("4. Model bias this game: +0.161\n")
cat("   → Running 2026 bias now tracked as a metric alongside MAE\n")
cat("   → Root cause: 2025 prior too generous for 2026 Jays (-70pts mean delta)\n")
cat("   → Fix: in-season blend will correct automatically as data accumulates\n")
cat("   → Do NOT manually adjust prior or blending weights at n=7 games\n")

# =============================================================================
# 10. FULL GAME CONTEXT (non-Wrobleski innings)
# =============================================================================

cat("\n=== FULL GAME CONTEXT ===\n")
all_pa <- jays_bat |>
  filter(events %in% PA_EVENTS) |>
  mutate(woba_val = coalesce(WOBA_WEIGHTS[events], 0),
         woba_den = as.integer(events != "hit_by_pitch"))

cat(sprintf("Full game PA: %d\n", nrow(all_pa)))
cat(sprintf("Full game team wOBA: %.3f\n", sum(all_pa$woba_val)/sum(all_pa$woba_den)))
cat(sprintf("H: %d  K: %d  BB: %d\n",
  sum(all_pa$events %in% c("single","double","triple","home_run")),
  sum(all_pa$events=="strikeout"),
  sum(all_pa$events=="walk")))

cat("\nNote: The 14-2 score reflects Jays pitching, not Jays offense.\n")
cat("The retro and memory system score only the modeled component (Wrobleski).\n")

# =============================================================================
# END
# =============================================================================
# KEY FINDINGS:
#   Wrobleski: 5 IP, 78 pitches, effective
#   Sinker to LHH: 10.5% actual vs 33% expected — major deviation
#   4-Seam dominant to both sides at 50% (career 33%)
#   Model bias: +0.161 — prior too generous for 2026 Jays
#   In-season update (Guerrero top pick): first test = miss
#   Varsho: closest prediction of night (err=0.010)
#   Memory update: lower sinker sequencing reliability for LHP profiles
# =============================================================================
