# =============================================================================
# Jays Matchup Intel, Retrospective
# 2026-03-30 · TOR vs COL · Tomoyuki Sugano RHP
# @scherrology | Arm Chair Analyst
# =============================================================================
# Data required:
#   sugano_col_pitcher.csv          , Sugano 2025 full season pitch log (BAL)
#   blue-jays-3-30-26-results.xlsx  , Tonight's Statcast pitch log (Sugano PAs only)
#
# Scope: Starter PAs only. Relief pitchers excluded by design.
# Model evaluated against Bayesian posteriors from preview analysis.
# =============================================================================

library(tidyverse)
library(readxl)
library(scales)

# =============================================================================
# 0. CONFIGURATION
# =============================================================================

SUGANO_ID   <- 608372L
LEAGUE_WOBA <- 0.320

WOBA_WEIGHTS <- c(
  walk             = 0.696,
  hit_by_pitch     = 0.726,
  single           = 0.888,
  double           = 1.271,
  triple           = 1.616,
  home_run         = 2.101
)

PA_EVENTS <- c(
  "single", "double", "triple", "home_run",
  "field_out", "strikeout", "walk", "hit_by_pitch",
  "grounded_into_double_play", "force_out", "sac_fly",
  "fielders_choice", "fielders_choice_out", "double_play"
)

PITCH_COLORS <- c(
  "4-Seam Fastball" = "#E8002D",
  "Sinker"          = "#f4b942",
  "Sweeper"         = "#2dd4a0",
  "Split-Finger"    = "#7ab8f5",
  "Cutter"          = "#c084fc",
  "Curveball"       = "#fb923c"
)

# Bayesian posteriors from preview model
POSTERIORS <- tribble(
  ~player_name,              ~posterior,
  "Springer, George",        0.404,
  "Guerrero Jr., Vladimir",  0.382,
  "Schneider, Davis",        0.367,
  "Barger, Addison",         0.353,
  "Heineman, Tyler",         0.351,
  "Varsho, Daulton",         0.342,
  "Kirk, Alejandro",         0.341,
  "Sánchez, Jesús",          0.312,
  "Lukes, Nathan",           0.309,
  "Clement, Ernie",          0.305,
  "Straw, Myles",            0.297,
  "Giménez, Andrés",         0.289
)

# =============================================================================
# 1. LOAD DATA
# =============================================================================

sugano_2025 <- read_csv("sugano_col_pitcher.csv", show_col_types = FALSE)
tonight     <- read_excel("blue-jays-3-30-26-results.xlsx")

cat("Sugano 2025 pitches:", nrow(sugano_2025), "\n")
cat("Tonight pitches tracked:", nrow(tonight), "\n")
cat("Tonight hitters:", paste(unique(tonight$player_name), collapse = ", "), "\n")

# =============================================================================
# 2. LAYER 1: ARSENAL COMPARISON
# =============================================================================

# 2025 projected mix
proj_mix <- sugano_2025 |>
  filter(!is.na(pitch_name)) |>
  group_by(pitch_name) |>
  summarise(n_2025 = n(), .groups = "drop") |>
  mutate(pct_2025 = n_2025 / sum(n_2025) * 100)

# Tonight actual mix
act_mix <- tonight |>
  filter(!is.na(pitch_name)) |>
  group_by(pitch_name) |>
  summarise(n_tonight = n(), .groups = "drop") |>
  mutate(pct_tonight = n_tonight / sum(n_tonight) * 100)

# Velocity comparison
proj_velo <- sugano_2025 |>
  group_by(pitch_name) |>
  summarise(velo_2025 = mean(release_speed, na.rm = TRUE), .groups = "drop")

act_velo <- tonight |>
  group_by(pitch_name) |>
  summarise(velo_tonight = mean(release_speed, na.rm = TRUE), .groups = "drop")

arsenal_comparison <- proj_mix |>
  full_join(act_mix,   by = "pitch_name") |>
  left_join(proj_velo, by = "pitch_name") |>
  left_join(act_velo,  by = "pitch_name") |>
  replace_na(list(pct_2025 = 0, pct_tonight = 0)) |>
  mutate(
    pct_delta  = round(pct_tonight - pct_2025, 1),
    velo_delta = round(velo_tonight - velo_2025, 1),
    deviation  = case_when(
      abs(pct_delta) >= 8  ~ "significant",
      abs(pct_delta) >= 4  ~ "moderate",
      TRUE                 ~ "on script"
    )
  ) |>
  arrange(desc(pct_2025))

cat("\n=== ARSENAL COMPARISON ===\n")
arsenal_comparison |>
  select(pitch_name, pct_2025, pct_tonight, pct_delta, velo_2025, velo_tonight, velo_delta, deviation) |>
  mutate(across(where(is.numeric), \(x) round(x, 1))) |>
  print(n = Inf)

# =============================================================================
# 3. LAYER 1: ZONE COMPARISON BY HANDEDNESS
# =============================================================================

# In-zone zones only (1-9)
in_zones <- 1:9

zone_comp <- function(data, hand_label) {
  data |>
    filter(stand == hand_label, zone %in% in_zones) |>
    group_by(zone) |>
    summarise(n = n(), .groups = "drop") |>
    mutate(pct = n / sum(n) * 100, hand = hand_label)
}

zones_2025_rhh    <- zone_comp(sugano_2025, "R") |> mutate(source = "2025 Projected")
zones_tonight_rhh <- zone_comp(tonight,     "R") |> mutate(source = "Tonight Actual")
zones_2025_lhh    <- zone_comp(sugano_2025, "L") |> mutate(source = "2025 Projected")
zones_tonight_lhh <- zone_comp(tonight,     "L") |> mutate(source = "Tonight Actual")

zone_summary <- bind_rows(
  zones_2025_rhh, zones_tonight_rhh,
  zones_2025_lhh, zones_tonight_lhh
)

cat("\n=== ZONE COMPARISON RHH ===\n")
bind_rows(zones_2025_rhh, zones_tonight_rhh) |>
  select(source, zone, n, pct) |>
  mutate(pct = round(pct, 1)) |>
  print(n = Inf)

cat("\n=== ZONE COMPARISON LHH ===\n")
bind_rows(zones_2025_lhh, zones_tonight_lhh) |>
  select(source, zone, n, pct) |>
  mutate(pct = round(pct, 1)) |>
  print(n = Inf)

# =============================================================================
# 4. LAYER 2: MODEL SCORECARD
# =============================================================================

pa_df <- tonight |>
  filter(events %in% PA_EVENTS) |>
  mutate(
    woba_val  = coalesce(WOBA_WEIGHTS[events], 0),
    woba_den  = as.integer(events != "hit_by_pitch"),
    is_hit    = events %in% c("single", "double", "triple", "home_run"),
    is_k      = events == "strikeout",
    is_bb     = events == "walk"
  )

actual_results <- pa_df |>
  group_by(player_name, stand) |>
  summarise(
    PA       = n(),
    H        = sum(is_hit),
    BB       = sum(is_bb),
    K        = sum(is_k),
    HR       = sum(events == "home_run"),
    woba_num = sum(woba_val),
    woba_den = sum(woba_den),
    outcomes = paste(events, collapse = ", "),
    .groups  = "drop"
  ) |>
  mutate(actual_wOBA = woba_num / pmax(woba_den, 1))

scorecard <- actual_results |>
  left_join(POSTERIORS, by = "player_name") |>
  mutate(
    delta   = round(actual_wOBA - posterior, 3),
    verdict = case_when(
      is.na(posterior)            ~ "no model",
      abs(delta) <= 0.050         ~ "called",
      actual_wOBA > posterior     ~ "called",
      TRUE                        ~ "miss"
    )
  ) |>
  arrange(desc(posterior))

cat("\n=== MODEL SCORECARD ===\n")
scorecard |>
  select(player_name, stand, PA, H, BB, K, HR,
         actual_wOBA, posterior, delta, verdict, outcomes) |>
  mutate(across(where(is.numeric), \(x) round(x, 3))) |>
  print(n = Inf)

# Summary counts
cat("\nModel wins (called):", sum(scorecard$verdict == "called", na.rm = TRUE))
cat("\nMisses:             ", sum(scorecard$verdict == "miss",   na.rm = TRUE))
cat("\nNo model:           ", sum(scorecard$verdict == "no model", na.rm = TRUE), "\n")

# =============================================================================
# 5. LAYER 3: DEEP DIVE, pitch sequences for notable outcomes
# =============================================================================

# Full pitch sequence per hitter
pitch_seq <- tonight |>
  arrange(player_name, at_bat_number, pitch_number) |>
  select(player_name, at_bat_number, pitch_number, pitch_name,
         description, events, release_speed,
         plate_x, plate_z,
         launch_speed, launch_angle, hit_distance_sc,
         pfx_x, pfx_z)

# Vlad deep dive
cat("\n=== VLAD PITCH SEQUENCE ===\n")
pitch_seq |>
  filter(player_name == "Guerrero Jr., Vladimir") |>
  print(n = Inf)

# Springer deep dive
cat("\n=== SPRINGER PITCH SEQUENCE ===\n")
pitch_seq |>
  filter(player_name == "Springer, George") |>
  print(n = Inf)

# Sanchez deep dive
cat("\n=== SÁNCHEZ PITCH SEQUENCE ===\n")
pitch_seq |>
  filter(player_name == "Sánchez, Jesús") |>
  print(n = Inf)

# =============================================================================
# 6. ARSENAL STABILITY, game-to-game volatility from 2025
# =============================================================================
# Per-game pitch mix to compute coefficient of variation per pitch type.
# High CV flags pitches where Sugano's usage is unpredictable.
# This is the foundation for the Volatility Score in future previews.

game_mix <- sugano_2025 |>
  filter(!is.na(pitch_name)) |>
  group_by(game_pk, pitch_name) |>
  summarise(n = n(), .groups = "drop") |>
  group_by(game_pk) |>
  mutate(pct = n / sum(n) * 100) |>
  ungroup()

arsenal_stability <- game_mix |>
  group_by(pitch_name) |>
  summarise(
    mean_pct  = mean(pct),
    sd_pct    = sd(pct),
    cv        = sd_pct / mean_pct,
    min_pct   = min(pct),
    max_pct   = max(pct),
    n_games   = n(),
    .groups   = "drop"
  ) |>
  mutate(
    stability = case_when(
      cv < 0.25 ~ "stable",
      cv < 0.50 ~ "moderate",
      TRUE      ~ "volatile"
    )
  ) |>
  arrange(desc(cv))

cat("\n=== ARSENAL STABILITY (2025 game-by-game CV) ===\n")
arsenal_stability |>
  mutate(across(where(is.numeric), \(x) round(x, 3))) |>
  print(n = Inf)

cat("\nNote: Sweeper CV above threshold would have flagged volatile prior in tonight's preview.\n")

# =============================================================================
# 7. PLOTS
# =============================================================================

theme_jays <- function() {
  theme_minimal(base_family = "sans") +
    theme(
      plot.background  = element_rect(fill = "#002050", color = NA),
      panel.background = element_rect(fill = "#071e3d", color = NA),
      panel.grid.major = element_line(color = "rgba(255,255,255,0.06)", linewidth = 0.4),
      panel.grid.minor = element_blank(),
      text             = element_text(color = "#c8ddf0"),
      axis.text        = element_text(color = "#7a9cc0", size = 8),
      axis.title       = element_text(color = "#c8ddf0", size = 9),
      plot.title       = element_text(color = "white", size = 13, face = "bold"),
      plot.subtitle    = element_text(color = "#7a9cc0", size = 9),
      plot.caption     = element_text(color = "#4a7ca0", size = 7),
      legend.background = element_rect(fill = "#071e3d", color = NA),
      legend.text      = element_text(color = "#7a9cc0", size = 8),
      strip.text       = element_text(color = "#c8ddf0", size = 9)
    )
}

# 7a. Arsenal comparison, projected vs actual
p_arsenal_comp <- arsenal_comparison |>
  filter(pct_2025 > 0 | pct_tonight > 0) |>
  pivot_longer(cols = c(pct_2025, pct_tonight),
               names_to = "source", values_to = "pct") |>
  mutate(
    source     = if_else(source == "pct_2025", "2025 Projected", "Tonight Actual"),
    pitch_name = fct_reorder(pitch_name, pct, .fun = max)
  ) |>
  ggplot(aes(x = pct, y = pitch_name, fill = source)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  scale_fill_manual(values = c("2025 Projected" = "rgba(255,255,255,0.25)",
                                "Tonight Actual" = "#C4A24B")) +
  geom_vline(xintercept = 0, color = "white", alpha = 0.1) +
  labs(
    title    = "Sugano Arsenal, Projected vs Actual",
    subtitle = "Sweeper usage dropped 11.5 pts, 4-Seam spiked 11.7 pts",
    x        = "Usage %",
    y        = NULL,
    fill     = NULL,
    caption  = "Statcast via Baseball Savant, @scherrology"
  ) +
  theme_jays() +
  theme(legend.position = "bottom")

ggsave("retro_plot_01_arsenal_comp.png", p_arsenal_comp,
       width = 9, height = 5, dpi = 150, bg = "#002050")
cat("\nSaved: retro_plot_01_arsenal_comp.png\n")

# 7b. Model scorecard
p_scorecard <- scorecard |>
  filter(!is.na(posterior)) |>
  mutate(
    short_name  = str_replace(player_name, ",.*", ""),
    fill_color  = case_when(
      verdict == "called" ~ "#2dd4a0",
      verdict == "miss"   ~ "#ff4f6a",
      TRUE                ~ "#7a9cc0"
    )
  ) |>
  ggplot(aes(y = fct_reorder(short_name, posterior))) +
  geom_segment(aes(x = posterior, xend = actual_wOBA,
                   yend = fct_reorder(short_name, posterior)),
               color = "white", alpha = 0.15, linewidth = 1) +
  geom_point(aes(x = posterior), shape = 21, size = 3.5,
             fill = NA, color = "white", alpha = 0.4, stroke = 1.2) +
  geom_point(aes(x = actual_wOBA, color = fill_color), size = 5) +
  scale_color_identity() +
  geom_vline(xintercept = LEAGUE_WOBA, linetype = "dashed",
             color = "#E8002D", linewidth = 0.6) +
  geom_text(aes(x = pmax(actual_wOBA, posterior) + 0.02,
                label = sprintf("%.3f", actual_wOBA)),
            size = 2.8, color = "#c8ddf0", hjust = 0, family = "mono") +
  annotate("text", x = LEAGUE_WOBA + 0.005, y = 0.5,
           label = "Lg Avg", color = "#E8002D", size = 2.5, hjust = 0) +
  scale_x_continuous(limits = c(0, 1.15)) +
  labs(
    title    = "Retro 3-30-26, Model Scorecard vs Sugano",
    subtitle = "Open circle = posterior prediction, filled = actual wOBA tonight",
    x        = "wOBA",
    y        = NULL,
    caption  = "Statcast via Baseball Savant, Beta-Binomial model, @scherrology"
  ) +
  theme_jays()

ggsave("retro_plot_02_scorecard.png", p_scorecard,
       width = 9, height = 6, dpi = 150, bg = "#002050")
cat("Saved: retro_plot_02_scorecard.png\n")

# 7c. Arsenal stability (CV by pitch)
p_stability <- arsenal_stability |>
  mutate(
    pitch_name = fct_reorder(pitch_name, cv),
    bar_color  = case_when(
      stability == "volatile"  ~ "#ff4f6a",
      stability == "moderate"  ~ "#f4b942",
      TRUE                     ~ "#2dd4a0"
    )
  ) |>
  ggplot(aes(x = cv, y = pitch_name, fill = bar_color)) +
  geom_col(width = 0.65, show.legend = FALSE) +
  geom_vline(xintercept = 0.50, linetype = "dashed",
             color = "#ff4f6a", linewidth = 0.6, alpha = 0.7) +
  geom_vline(xintercept = 0.25, linetype = "dashed",
             color = "#f4b942", linewidth = 0.6, alpha = 0.7) +
  geom_text(aes(label = paste0(round(mean_pct, 1), "% avg")),
            hjust = -0.1, size = 2.8, color = "white", family = "mono") +
  scale_fill_identity() +
  scale_x_continuous(expand = expansion(mult = c(0, 0.2))) +
  annotate("text", x = 0.51, y = 0.6, label = "Volatile threshold",
           color = "#ff4f6a", size = 2.5, hjust = 0, family = "mono") +
  annotate("text", x = 0.26, y = 0.6, label = "Moderate threshold",
           color = "#f4b942", size = 2.5, hjust = 0, family = "mono") +
  labs(
    title    = "Sugano Arsenal Stability, 2025 Game-by-Game CV",
    subtitle = "High CV = volatile pitch mix, widens predictive prior for future previews",
    x        = "Coefficient of Variation (SD / Mean)",
    y        = NULL,
    caption  = "Statcast via Baseball Savant, @scherrology"
  ) +
  theme_jays()

ggsave("retro_plot_03_stability.png", p_stability,
       width = 9, height = 5, dpi = 150, bg = "#002050")
cat("Saved: retro_plot_03_stability.png\n")

# =============================================================================
# 8. EXPORT CLEAN TABLES
# =============================================================================

write_csv(arsenal_comparison, "retro_output_arsenal_comparison.csv")
write_csv(scorecard,          "retro_output_scorecard.csv")
write_csv(arsenal_stability,  "retro_output_stability.csv")
write_csv(zone_summary,       "retro_output_zones.csv")
write_csv(pitch_seq,          "retro_output_pitch_sequences.csv")

cat("\nAll retro outputs written.\n")
cat("Plots: retro_plot_01 through retro_plot_03\n")
cat("Tables: retro_output_arsenal_comparison, scorecard, stability, zones, pitch_sequences\n")

# =============================================================================
# END
# =============================================================================
