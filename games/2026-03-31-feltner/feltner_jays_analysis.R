# =============================================================================
# Jays Matchup Intel · Preview
# 2026-03-31 · TOR vs COL · Ryan Feltner RHP
# Model v2: Arsenal-Weighted Prior
# @scherrology | Arm Chair Analyst
# =============================================================================
# Data required:
#   feltner.csv                     , Feltner 2023-2025 pitch log (COL)
#   jays-vs-feltner.csv             , Blue Jays hitters vs Feltner (2024)
#   blue-jays-hitters-25-26.csv     , Blue Jays 2025-2026 pitch-level batter file
#   sanchez-hitter-approach.csv     , Jesús Sánchez approach file (MIA + TOR)
#
# Model v2 change:
#   Prior is no longer the hitter's flat overall 2025 wOBA.
#   Prior is now arsenal-weighted: each hitter's wOBA by pitch type is weighted
#   by Feltner's actual pitch mix vs that hitter's handedness.
#   No additional data export required — pitch_name is already in the batter file.
# =============================================================================

library(tidyverse)
library(readxl)
library(scales)
library(glue)

# =============================================================================
# 0. CONFIGURATION
# =============================================================================

K_PRIOR     <- 60
LEAGUE_WOBA <- 0.320
MIN_PA_SPLIT <- 10        # minimum PA vs a pitch type to use the split; else fallback

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
  "Changeup"        = "#60a5fa",
  "Slider"          = "#a78bfa",
  "Curveball"       = "#fb923c"
)

# Feltner 2025 pitch mix by handedness (from pitcher file)
# These are the weights applied to hitter pitch-type splits
FELTNER_LHH_MIX <- c(
  "4-Seam Fastball" = 0.416,
  "Changeup"        = 0.302,
  "Slider"          = 0.162,
  "Curveball"       = 0.100,
  "Sinker"          = 0.017,
  "Sweeper"         = 0.003
)

FELTNER_RHH_MIX <- c(
  "Sweeper"         = 0.273,
  "Sinker"          = 0.260,
  "4-Seam Fastball" = 0.229,
  "Slider"          = 0.182,
  "Changeup"        = 0.039,
  "Curveball"       = 0.017
)

# 2026 Blue Jays roster (hitters with 2026 data)
JAYS_2026 <- c(
  "Barger, Addison", "Clement, Ernie", "Giménez, Andrés",
  "Guerrero Jr., Vladimir", "Heineman, Tyler", "Kirk, Alejandro",
  "Lukes, Nathan", "Okamoto, Kazuma", "Schneider, Davis",
  "Springer, George", "Straw, Myles", "Sánchez, Jesús", "Varsho, Daulton"
)

# =============================================================================
# 1. LOAD DATA
# =============================================================================

feltner     <- read_csv("feltner.csv", show_col_types = FALSE)
jvf_raw     <- read_csv("jays-vs-feltner.csv", show_col_types = FALSE)
jays_full   <- read_csv("blue-jays-hitters-25-26.csv", show_col_types = FALSE)
sanchez_raw <- read_csv("sanchez-hitter-approach.csv", show_col_types = FALSE)

cat("Feltner pitches loaded:", nrow(feltner), "\n")
cat("Jays vs Feltner rows:", nrow(jvf_raw), "\n")
cat("Jays batter file rows:", nrow(jays_full), "\n")

# Filter to 2026 roster
jvf <- jvf_raw |> filter(player_name %in% JAYS_2026)

cat("\nJays vs Feltner (2026 roster only):", nrow(jvf), "pitches\n")
cat("Hitters with history:", paste(unique(jvf$player_name), collapse = ", "), "\n")
cat("Hitters without history:", paste(setdiff(JAYS_2026, unique(jvf$player_name)), collapse = ", "), "\n")

# =============================================================================
# 2. FELTNER ARSENAL ANALYSIS
# =============================================================================

f25 <- feltner |> filter(game_year == 2025)
f24 <- feltner |> filter(game_year == 2024)

cat("\nFeltner pitches by year:\n")
feltner |> count(game_year) |> print()

# 2025 arsenal
arsenal_25 <- f25 |>
  filter(!is.na(pitch_name)) |>
  group_by(pitch_name) |>
  summarise(
    n_25         = n(),
    velo_25      = mean(release_speed, na.rm = TRUE),
    whiff_n      = sum(description == "swinging_strike", na.rm = TRUE),
    swing_n      = sum(description %in% c("swinging_strike","foul","hit_into_play"), na.rm = TRUE),
    cs_n         = sum(description == "called_strike", na.rm = TRUE),
    xwoba_25     = mean(estimated_woba_using_speedangle, na.rm = TRUE),
    .groups      = "drop"
  ) |>
  mutate(
    pct_25       = n_25 / sum(n_25) * 100,
    whiff_rate   = whiff_n / pmax(swing_n, 1) * 100,
    csw_rate     = (whiff_n + cs_n) / n_25 * 100
  )

# 2024 arsenal for comparison
arsenal_24 <- f24 |>
  filter(!is.na(pitch_name)) |>
  group_by(pitch_name) |>
  summarise(
    n_24    = n(),
    velo_24 = mean(release_speed, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(pct_24 = n_24 / sum(n_24) * 100)

arsenal_comp <- arsenal_25 |>
  left_join(arsenal_24, by = "pitch_name") |>
  replace_na(list(pct_24 = 0, velo_24 = NA)) |>
  mutate(
    pct_delta  = round(pct_25 - pct_24, 1),
    evolved    = pct_delta >= 5  # flagged as meaningfully new
  ) |>
  arrange(desc(n_25))

cat("\n=== FELTNER ARSENAL (2025 vs 2024) ===\n")
arsenal_comp |>
  select(pitch_name, pct_25, pct_24, pct_delta, velo_25, whiff_rate, csw_rate, xwoba_25, evolved) |>
  mutate(across(where(is.numeric), \(x) round(x, 2))) |>
  print(n = Inf)

# Pitch mix by handedness (2025)
hand_mix <- f25 |>
  filter(!is.na(pitch_name), !is.na(stand)) |>
  group_by(stand, pitch_name) |>
  summarise(n = n(), .groups = "drop") |>
  group_by(stand) |>
  mutate(pct = n / sum(n) * 100) |>
  ungroup()

cat("\n=== PITCH MIX BY HANDEDNESS (2025) ===\n")
hand_mix |>
  select(stand, pitch_name, pct) |>
  mutate(pct = round(pct, 1)) |>
  pivot_wider(names_from = stand, values_from = pct, values_fill = 0) |>
  arrange(desc(R)) |>
  print(n = Inf)

# Arsenal stability (2024, primary season)
game_mix_24 <- f24 |>
  filter(!is.na(pitch_name)) |>
  group_by(game_pk, pitch_name) |>
  summarise(n = n(), .groups = "drop") |>
  group_by(game_pk) |>
  mutate(pct = n / sum(n) * 100) |>
  ungroup()

stability_24 <- game_mix_24 |>
  group_by(pitch_name) |>
  summarise(
    mean_pct  = mean(pct),
    sd_pct    = sd(pct),
    cv        = sd_pct / mean_pct,
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

cat("\n=== ARSENAL STABILITY 2024 (CV) ===\n")
stability_24 |> mutate(across(where(is.numeric), \(x) round(x, 3))) |> print(n = Inf)

# Zone frequencies (2025)
cat("\n=== ZONE FREQUENCIES (2025) ===\n")
cat("LHH:", deparse(as.list(table(f25[f25$stand=="L","zone"]))), "\n")
cat("RHH:", deparse(as.list(table(f25[f25$stand=="R","zone"]))), "\n")

# =============================================================================
# 3. MODEL V2: ARSENAL-WEIGHTED PRIOR
# =============================================================================

# 3a. Combine batter files for 2025 pitch-level data
jays_2025_all <- bind_rows(
  jays_full  |> filter(game_year == 2025),
  sanchez_raw |> filter(game_year == 2025)
)

# 3b. Hitter handedness
hitter_hand <- jays_2025_all |>
  group_by(player_name) |>
  summarise(hand = names(sort(table(stand), decreasing = TRUE))[1], .groups = "drop")

cat("\n=== HITTER HANDEDNESS ===\n")
print(hitter_hand)

# 3c. Per-hitter wOBA by pitch type (2025)
pa_2025 <- jays_2025_all |>
  filter(events %in% PA_EVENTS, !is.na(pitch_name)) |>
  mutate(
    woba_val = coalesce(WOBA_WEIGHTS[events], 0),
    woba_den = as.integer(events != "hit_by_pitch")
  )

pitch_type_splits <- pa_2025 |>
  group_by(player_name, pitch_name) |>
  summarise(
    PA_vs_pitch  = n(),
    woba_num     = sum(woba_val),
    woba_den     = sum(woba_den),
    pitch_wOBA   = woba_num / pmax(woba_den, 1),
    .groups      = "drop"
  )

cat("\n=== SPRINGER PITCH-TYPE SPLITS (2025) ===\n")
pitch_type_splits |>
  filter(player_name == "Springer, George") |>
  arrange(desc(PA_vs_pitch)) |>
  mutate(pitch_wOBA = round(pitch_wOBA, 3)) |>
  print(n = Inf)

# 3d. Overall 2025 wOBA per hitter (fallback)
overall_prior <- pa_2025 |>
  group_by(player_name) |>
  summarise(
    PA_2025      = n(),
    woba_num     = sum(woba_val),
    woba_den     = sum(woba_den),
    wOBA_overall = woba_num / pmax(woba_den, 1),
    .groups      = "drop"
  )

# 3e. Arsenal-weighted prior computation
compute_arsenal_prior <- function(player, hand_val, splits_df, fallback_woba) {
  mix <- if (hand_val == "L") FELTNER_LHH_MIX else FELTNER_RHH_MIX
  player_splits <- splits_df |>
    filter(player_name == player, PA_vs_pitch >= MIN_PA_SPLIT) |>
    select(pitch_name, pitch_wOBA)

  weighted_sum <- 0
  matched_wt   <- 0

  for (pitch in names(mix)) {
    wt <- mix[pitch]
    row <- player_splits |> filter(pitch_name == pitch)
    if (nrow(row) > 0 && !is.na(row$pitch_wOBA)) {
      weighted_sum <- weighted_sum + wt * row$pitch_wOBA
      matched_wt   <- matched_wt + wt
    }
  }

  # Fill unmatched pitches with overall wOBA
  unmatched_wt <- 1 - matched_wt
  weighted_sum + unmatched_wt * fallback_woba
}

# Compute for all hitters
priors_tbl <- overall_prior |>
  left_join(hitter_hand, by = "player_name") |>
  mutate(
    hand       = replace_na(hand, "R"),
    wOBA_arsenal = map2_dbl(player_name, hand,
      ~compute_arsenal_prior(.x, .y, pitch_type_splits, 
                              overall_prior$wOBA_overall[overall_prior$player_name == .x])),
    prior_delta  = round(wOBA_arsenal - wOBA_overall, 3)
  )

cat("\n=== ARSENAL-WEIGHTED PRIORS vs OVERALL ===\n")
priors_tbl |>
  select(player_name, hand, PA_2025, wOBA_overall, wOBA_arsenal, prior_delta) |>
  arrange(prior_delta) |>
  mutate(across(where(is.numeric), \(x) round(x, 3))) |>
  print(n = Inf)

# =============================================================================
# 4. MATCHUP DATA (2024 only)
# =============================================================================

pa_jvf <- jvf |>
  filter(events %in% PA_EVENTS) |>
  mutate(
    woba_val = coalesce(WOBA_WEIGHTS[events], 0),
    woba_den = as.integer(events != "hit_by_pitch")
  )

matchup_summary <- pa_jvf |>
  group_by(player_name) |>
  summarise(
    n_PA_felt      = n(),
    H              = sum(events %in% c("single","double","triple","home_run")),
    BB             = sum(events == "walk"),
    K              = sum(events == "strikeout"),
    HR             = sum(events == "home_run"),
    felt_woba_num  = sum(woba_val),
    felt_woba_den  = sum(woba_den),
    wOBA_vs_felt   = felt_woba_num / pmax(felt_woba_den, 1),
    outcomes       = paste(events, collapse = ", "),
    .groups        = "drop"
  )

cat("\n=== JAYS VS FELTNER (2024, 2026 roster) ===\n")
matchup_summary |>
  select(player_name, n_PA_felt, H, BB, K, HR, wOBA_vs_felt, outcomes) |>
  mutate(wOBA_vs_felt = round(wOBA_vs_felt, 3)) |>
  print(n = Inf)

# =============================================================================
# 5. BAYESIAN POSTERIORS (MODEL V2)
# =============================================================================

bayes_df <- priors_tbl |>
  left_join(matchup_summary |>
    select(player_name, n_PA_felt, felt_woba_num, felt_woba_den),
    by = "player_name") |>
  replace_na(list(n_PA_felt = 0, felt_woba_num = 0, felt_woba_den = 0)) |>
  mutate(
    # Beta prior parameters from arsenal-weighted prior
    alpha_0      = wOBA_arsenal * K_PRIOR,
    beta_0       = (1 - wOBA_arsenal) * K_PRIOR,
    # Posterior parameters
    alpha_post   = alpha_0 + felt_woba_num,
    beta_post    = beta_0  + (felt_woba_den - felt_woba_num),
    # Posterior summary
    wOBA_post    = alpha_post / (alpha_post + beta_post),
    CI_lo_90     = qbeta(0.05, alpha_post, beta_post),
    CI_hi_90     = qbeta(0.95, alpha_post, beta_post),
    CI_lo_95     = qbeta(0.025, alpha_post, beta_post),
    CI_hi_95     = qbeta(0.975, alpha_post, beta_post),
    has_matchup  = n_PA_felt >= 2,
    tier         = case_when(
      player_name == "Okamoto, Kazuma" ~ "T3",
      has_matchup                      ~ "T1",
      TRUE                             ~ "T2"
    )
  ) |>
  arrange(desc(wOBA_post))

cat("\n=== BAYESIAN POSTERIORS (MODEL V2) ===\n")
bayes_df |>
  select(player_name, hand, tier, PA_2025, wOBA_overall, wOBA_arsenal,
         prior_delta, n_PA_felt, wOBA_post, CI_lo_90, CI_hi_90) |>
  mutate(across(where(is.numeric), \(x) round(x, 3))) |>
  print(n = Inf)

# =============================================================================
# 6. PLOTS
# =============================================================================

theme_jays <- function() {
  theme_minimal(base_family = "sans") +
    theme(
      plot.background   = element_rect(fill = "#002050", color = NA),
      panel.background  = element_rect(fill = "#071e3d", color = NA),
      panel.grid.major  = element_line(color = "rgba(255,255,255,0.06)", linewidth = 0.4),
      panel.grid.minor  = element_blank(),
      text              = element_text(color = "#c8ddf0"),
      axis.text         = element_text(color = "#7a9cc0", size = 8),
      axis.title        = element_text(color = "#c8ddf0", size = 9),
      plot.title        = element_text(color = "white", size = 13, face = "bold"),
      plot.subtitle     = element_text(color = "#7a9cc0", size = 9),
      plot.caption      = element_text(color = "#4a7ca0", size = 7),
      legend.background = element_rect(fill = "#071e3d", color = NA),
      legend.text       = element_text(color = "#7a9cc0", size = 8),
      strip.text        = element_text(color = "#c8ddf0", size = 9)
    )
}

# 6a. Arsenal comparison 2025 vs 2024
p_arsenal <- arsenal_comp |>
  filter(pct_25 > 0 | pct_24 > 0) |>
  pivot_longer(cols = c(pct_25, pct_24), names_to = "year", values_to = "pct") |>
  mutate(
    year       = if_else(year == "pct_25", "2025 Actual", "2024 Reference"),
    pitch_name = fct_reorder(pitch_name, pct, .fun = max)
  ) |>
  ggplot(aes(x = pct, y = pitch_name, fill = year, alpha = year)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  scale_fill_manual(values  = c("2025 Actual" = "#C4A24B", "2024 Reference" = "white")) +
  scale_alpha_manual(values = c("2025 Actual" = 0.9,       "2024 Reference" = 0.25)) +
  annotate("text", x = 22, y = "Sweeper", label = "NEW WEAPON", color = "#2dd4a0",
           size = 2.5, hjust = 0, family = "mono") +
  labs(
    title    = "Feltner Arsenal, 2025 vs 2024",
    subtitle = "Sweeper jumped from 5% to 12.3% usage, primary RHH weapon",
    x        = "Usage %", y = NULL, fill = NULL, alpha = NULL,
    caption  = "Statcast via Baseball Savant, @scherrology"
  ) +
  theme_jays() +
  theme(legend.position = "bottom")

ggsave("feltner_plot_01_arsenal.png", p_arsenal, width = 9, height = 5, dpi = 150, bg = "#002050")
cat("\nSaved: feltner_plot_01_arsenal.png\n")

# 6b. Arsenal-weighted prior vs overall wOBA
p_prior_delta <- bayes_df |>
  filter(tier != "T3") |>
  mutate(
    short_name  = str_replace(player_name, ",.*", ""),
    delta_color = case_when(
      prior_delta >=  0.015 ~ "#2dd4a0",
      prior_delta <= -0.015 ~ "#ff4f6a",
      TRUE                  ~ "#7a9cc0"
    )
  ) |>
  ggplot(aes(y = fct_reorder(short_name, wOBA_arsenal))) +
  geom_segment(aes(x = wOBA_overall, xend = wOBA_arsenal,
                   yend = fct_reorder(short_name, wOBA_arsenal)),
               color = "white", alpha = 0.2, linewidth = 1.2,
               arrow = arrow(length = unit(0.08, "inches"), type = "closed")) +
  geom_point(aes(x = wOBA_overall), shape = 21, size = 3,
             fill = NA, color = "white", alpha = 0.4, stroke = 1) +
  geom_point(aes(x = wOBA_arsenal, color = delta_color), size = 4.5) +
  scale_color_identity() +
  geom_vline(xintercept = LEAGUE_WOBA, linetype = "dashed",
             color = "#E8002D", linewidth = 0.6) +
  geom_text(aes(x = wOBA_arsenal + 0.005,
                label = sprintf("%+.3f", prior_delta),
                color = delta_color),
            size = 2.8, hjust = 0, family = "mono") +
  annotate("text", x = LEAGUE_WOBA + 0.003, y = 0.5,
           label = "Lg Avg", color = "#E8002D", size = 2.5, hjust = 0) +
  scale_color_identity() +
  labs(
    title    = "Arsenal-Weighted Prior vs Flat Overall wOBA",
    subtitle = "Arrow shows direction of prior adjustment, label shows delta",
    x        = "wOBA", y = NULL,
    caption  = "Model v2, pitch-type splits weighted by Feltner's handedness mix, @scherrology"
  ) +
  theme_jays()

ggsave("feltner_plot_02_prior_delta.png", p_prior_delta,
       width = 9, height = 6, dpi = 150, bg = "#002050")
cat("Saved: feltner_plot_02_prior_delta.png\n")

# 6c. Bayesian posteriors
tier_colors <- c(T1 = "#2dd4a0", T2 = "#f4b942", T3 = "#ff4f6a")

p_bayes <- bayes_df |>
  filter(tier != "T3") |>
  mutate(short_name = str_replace(player_name, ",.*", "")) |>
  ggplot(aes(y = fct_reorder(short_name, wOBA_post))) +
  geom_segment(aes(x = CI_lo_90, xend = CI_hi_90,
                   yend = fct_reorder(short_name, wOBA_post)),
               color = "white", alpha = 0.2, linewidth = 1.5, lineend = "round") +
  geom_point(aes(x = wOBA_arsenal), shape = 21, size = 3,
             fill = NA, color = "white", alpha = 0.35, stroke = 1.2) +
  geom_point(aes(x = wOBA_post, color = tier), size = 4.5) +
  scale_color_manual(values = tier_colors,
                     labels = c(T1 = "T1 · Matchup updated", T2 = "T2 · Arsenal prior only")) +
  geom_vline(xintercept = LEAGUE_WOBA, linetype = "dashed",
             color = "#E8002D", linewidth = 0.6) +
  geom_text(aes(x = CI_hi_90 + 0.008,
                label = sprintf("%.3f", wOBA_post)),
            size = 2.8, color = "#c8ddf0", hjust = 0, family = "mono") +
  annotate("text", x = LEAGUE_WOBA + 0.005, y = 0.5,
           label = "Lg Avg (.320)", color = "#E8002D", size = 2.5, hjust = 0) +
  scale_x_continuous(limits = c(0.15, 0.57), breaks = seq(0.15, 0.55, 0.05)) +
  labs(
    title    = "Bayesian Posteriors, Jays vs Feltner",
    subtitle = "Model v2: Arsenal-weighted prior, open circle = prior, filled = posterior, bar = 90% CI",
    x        = "wOBA (posterior estimate)", y = NULL, color = NULL,
    caption  = "Statcast via Baseball Savant, Beta-Binomial Model v2, @scherrology"
  ) +
  theme_jays() +
  theme(legend.position = "bottom")

ggsave("feltner_plot_03_posteriors.png", p_bayes,
       width = 9, height = 7, dpi = 150, bg = "#002050")
cat("Saved: feltner_plot_03_posteriors.png\n")

# 6d. Arsenal stability
p_stability <- stability_24 |>
  filter(pitch_name != "Pitch Out") |>
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
  geom_vline(xintercept = 0.50, linetype = "dashed", color = "#ff4f6a", linewidth = 0.6) +
  geom_vline(xintercept = 0.25, linetype = "dashed", color = "#f4b942", linewidth = 0.6) +
  geom_text(aes(label = paste0(round(mean_pct, 1), "% avg")),
            hjust = -0.1, size = 2.8, color = "white", family = "mono") +
  scale_fill_identity() +
  scale_x_continuous(expand = expansion(mult = c(0, 0.2))) +
  labs(
    title    = "Feltner Arsenal Stability, 2024 Game-by-Game CV",
    subtitle = "High CV = volatile pitch mix, widens predictive prior for future previews",
    x        = "Coefficient of Variation (SD / Mean)", y = NULL,
    caption  = "Statcast via Baseball Savant, @scherrology"
  ) +
  theme_jays()

ggsave("feltner_plot_04_stability.png", p_stability,
       width = 9, height = 5, dpi = 150, bg = "#002050")
cat("Saved: feltner_plot_04_stability.png\n")

# =============================================================================
# 7. EXPORT CLEAN TABLES
# =============================================================================

write_csv(arsenal_comp,      "feltner_output_arsenal.csv")
write_csv(hand_mix,          "feltner_output_hand_mix.csv")
write_csv(stability_24,      "feltner_output_stability.csv")
write_csv(pitch_type_splits, "feltner_output_pitch_type_splits.csv")
write_csv(bayes_df,          "feltner_output_posteriors.csv")
write_csv(matchup_summary,   "feltner_output_matchup_2024.csv")

cat("\nAll outputs written.\n")
cat("Plots: feltner_plot_01 through feltner_plot_04\n")
cat("Tables: feltner_output_arsenal, hand_mix, stability, pitch_type_splits, posteriors, matchup_2024\n")

# =============================================================================
# END
# =============================================================================
