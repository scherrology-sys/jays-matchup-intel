# =============================================================================
# Blue Jays vs. Sugano, Matchup Intelligence
# Statcast Pitch-Level Analysis + Bayesian Hierarchical wOBA Model
# @scherrology-sys | March 30, 2026
# =============================================================================
# Data: Baseball Savant Statcast exports
#   sugano_col_pitcher.csv      , Sugano 2025 full season pitch log (BAL)
#   blue-jays-hitters-25-26.csv , Blue Jays hitters 2025 + 2026 pitch log
#   sanchez-hitter-approach.csv , Jesus Sanchez full approach file (MIA + TOR)
#
# Model: Beta-Binomial empirical Bayes
#   Prior, each hitter's 2025 full-season wOBA (k = 60 PA equivalent weight)
#   Likelihood, wOBA-weighted observations from 2025 vs-Sugano PAs
#   Posterior, updated wOBA estimate with 90% credible interval
# =============================================================================

library(tidyverse)
library(scales)

# =============================================================================
# 0. CONFIGURATION
# =============================================================================

SUGANO_ID   <- 608372L
K_PRIOR     <- 60        # prior equivalent sample size (PA)
LEAGUE_WOBA <- 0.320     # 2025 MLB average wOBA

# wOBA linear weights (2025 scale)
WOBA_WEIGHTS <- c(
  walk             = 0.696,
  hit_by_pitch     = 0.726,
  single           = 0.888,
  double           = 1.271,
  triple           = 1.616,
  home_run         = 2.101
)

# Events that constitute a plate appearance
PA_EVENTS <- c(
  "single", "double", "triple", "home_run",
  "field_out", "strikeout", "walk", "hit_by_pitch",
  "grounded_into_double_play", "force_out", "sac_fly",
  "fielders_choice", "fielders_choice_out", "double_play"
)

# Pitch color palette for plots
PITCH_COLORS <- c(
  "4-Seam Fastball" = "#E8002D",
  "Sinker"          = "#f4b942",
  "Sweeper"         = "#2dd4a0",
  "Split-Finger"    = "#7ab8f5",
  "Cutter"          = "#c084fc",
  "Curveball"       = "#fb923c"
)

# =============================================================================
# 1. LOAD DATA
# =============================================================================

sugano <- read_csv("sugano_col_pitcher.csv", show_col_types = FALSE)
jays   <- read_csv("blue-jays-hitters-25-26.csv", show_col_types = FALSE)
sanchez_raw <- read_csv("sanchez-hitter-approach.csv", show_col_types = FALSE)

cat("Sugano pitches loaded:", nrow(sugano), "\n")
cat("Jays pitch log loaded:", nrow(jays), "\n")
cat("Sanchez approach loaded:", nrow(sanchez_raw), "\n")

# =============================================================================
# 2. FILTER JAYS HITTERS TO THOSE WITH 2026 DATA
# =============================================================================

# Identify hitters with confirmed 2026 plate appearances
hitters_2026 <- jays |>
  filter(game_year == 2026) |>
  distinct(player_name) |>
  pull(player_name)

cat("\nHitters retained (have 2026 data):", length(hitters_2026), "\n")
cat(paste(" ", sort(hitters_2026), collapse = "\n"), "\n")

jays_filtered <- jays |>
  filter(player_name %in% hitters_2026)

cat("\nRows after filter:", nrow(jays_filtered), "\n")

# =============================================================================
# 3. HELPER: wOBA CALCULATION
# =============================================================================

#' Compute wOBA from a vector of event strings
#'
#' @param events character vector of Statcast event values
#' @return tibble with columns woba_num, woba_den, woba, n_pa
calc_woba <- function(events) {
  pa     <- events[events %in% PA_EVENTS]
  # HBP excluded from denominator per standard wOBA formula
  num    <- sum(WOBA_WEIGHTS[pa], na.rm = TRUE)
  den    <- sum(pa != "hit_by_pitch", na.rm = TRUE)
  tibble(
    n_pa     = length(pa),
    woba_num = num,
    woba_den = den,
    woba     = if_else(den > 0, num / den, NA_real_)
  )
}

# =============================================================================
# 4. SUGANO ARSENAL ANALYSIS
# =============================================================================

# 4a. Overall pitch mix
arsenal <- sugano |>
  filter(!is.na(pitch_name)) |>
  group_by(pitch_name) |>
  summarise(
    n         = n(),
    avg_velo  = mean(release_speed, na.rm = TRUE),
    avg_spin  = mean(release_spin_rate, na.rm = TRUE),
    avg_pfx_x = mean(pfx_x, na.rm = TRUE),
    avg_pfx_z = mean(pfx_z, na.rm = TRUE),
    whiff_n   = sum(description == "swinging_strike", na.rm = TRUE),
    swing_n   = sum(description %in% c("swinging_strike", "foul", "hit_into_play"), na.rm = TRUE),
    cs_n      = sum(description == "called_strike", na.rm = TRUE),
    xwoba_allowed = mean(estimated_woba_using_speedangle, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    pct        = n / sum(n) * 100,
    whiff_rate = whiff_n / pmax(swing_n, 1) * 100,
    csw_rate   = (whiff_n + cs_n) / n * 100
  ) |>
  arrange(desc(n))

cat("\n=== SUGANO ARSENAL ===\n")
arsenal |>
  select(pitch_name, pct, avg_velo, whiff_rate, csw_rate, xwoba_allowed) |>
  mutate(across(where(is.numeric), \(x) round(x, 2))) |>
  print(n = Inf)

# 4b. Pitch mix by batter handedness
arsenal_by_hand <- sugano |>
  filter(!is.na(pitch_name), !is.na(stand)) |>
  group_by(stand, pitch_name) |>
  summarise(n = n(), .groups = "drop") |>
  group_by(stand) |>
  mutate(pct = n / sum(n) * 100) |>
  ungroup() |>
  arrange(stand, desc(pct))

cat("\n=== PITCH MIX BY HANDEDNESS ===\n")
arsenal_by_hand |>
  select(stand, pitch_name, pct) |>
  mutate(pct = round(pct, 1)) |>
  pivot_wider(names_from = stand, values_from = pct, values_fill = 0) |>
  print(n = Inf)

# 4c. Zone frequency by handedness
zone_freq <- sugano |>
  filter(!is.na(zone), !is.na(stand)) |>
  group_by(stand, zone) |>
  summarise(n = n(), .groups = "drop") |>
  group_by(stand) |>
  mutate(pct = n / sum(n) * 100) |>
  ungroup()

cat("\n=== ZONE FREQUENCIES (in-zone only) ===\n")
zone_freq |>
  filter(zone %in% 1:9) |>
  select(stand, zone, n, pct) |>
  mutate(pct = round(pct, 1)) |>
  print(n = Inf)

# =============================================================================
# 5. JAYS vs SUGANO, 2025 HEAD-TO-HEAD
# =============================================================================

vs_sugano_raw <- jays_filtered |>
  filter(pitcher == SUGANO_ID, game_year == 2025)

cat("\n=== JAYS VS SUGANO 2025 ===\n")
cat("Total pitches:", nrow(vs_sugano_raw), "\n")
cat("Hitters who saw him:", paste(unique(vs_sugano_raw$player_name), collapse = ", "), "\n")

# PA-level matchup stats
vs_sugano_pa <- vs_sugano_raw |>
  filter(events %in% PA_EVENTS) |>
  mutate(
    is_hit    = events %in% c("single", "double", "triple", "home_run"),
    is_bb_hbp = events %in% c("walk", "hit_by_pitch"),
    is_k      = events == "strikeout",
    woba_val  = coalesce(WOBA_WEIGHTS[events], 0),
    woba_den  = as.integer(events != "hit_by_pitch")
  )

matchup_2025 <- vs_sugano_pa |>
  group_by(player_name) |>
  summarise(
    PA       = n(),
    H        = sum(is_hit),
    BB       = sum(events == "walk"),
    K        = sum(is_k),
    HR       = sum(events == "home_run"),
    woba_num = sum(woba_val),
    woba_den = sum(woba_den),
    .groups  = "drop"
  ) |>
  mutate(
    AVG     = round(H / PA, 3),
    K_pct   = round(K / PA * 100, 1),
    wOBA    = round(woba_num / pmax(woba_den, 1), 3)
  ) |>
  arrange(desc(wOBA))

cat("\n=== MATCHUP RESULTS ===\n")
matchup_2025 |>
  select(player_name, PA, H, BB, K, HR, K_pct, AVG, wOBA) |>
  print(n = Inf)

# =============================================================================
# 6. BAYESIAN MODEL, Beta-Binomial wOBA
# =============================================================================
# Prior: Beta(alpha_0, beta_0) anchored to each hitter's full 2025 wOBA
#   alpha_0 = wOBA_prior * K_PRIOR
#   beta_0  = (1 - wOBA_prior) * K_PRIOR
#
# Likelihood: wOBA-weighted PA observations vs Sugano
#   successes ~ woba_num_vs_sugano
#   failures  ~ woba_den_vs_sugano - woba_num_vs_sugano
#
# Posterior: Beta(alpha_0 + successes, beta_0 + failures)
#   posterior_mean = alpha_post / (alpha_post + beta_post)
#   90% CI from qbeta(0.05, ...) and qbeta(0.95, ...)
# =============================================================================

# 6a. Compute priors from 2025 full-season data (retained hitters)
jays_2025_pa <- jays_filtered |>
  filter(game_year == 2025, events %in% PA_EVENTS) |>
  mutate(
    woba_val = coalesce(WOBA_WEIGHTS[events], 0),
    woba_den = as.integer(events != "hit_by_pitch")
  )

priors_jays <- jays_2025_pa |>
  group_by(player_name) |>
  summarise(
    PA_2025   = n(),
    woba_num  = sum(woba_val),
    woba_den  = sum(woba_den),
    .groups   = "drop"
  ) |>
  mutate(wOBA_prior = woba_num / pmax(woba_den, 1))

# 6b. Sanchez prior from his own file (Miami 2025)
sanchez_2025_pa <- sanchez_raw |>
  filter(game_year == 2025, events %in% PA_EVENTS) |>
  mutate(
    woba_val = coalesce(WOBA_WEIGHTS[events], 0),
    woba_den = as.integer(events != "hit_by_pitch")
  )

sanchez_prior <- sanchez_2025_pa |>
  summarise(
    player_name = "Sánchez, Jesús",
    PA_2025     = n(),
    woba_num    = sum(woba_val),
    woba_den    = sum(woba_den)
  ) |>
  mutate(wOBA_prior = woba_num / pmax(woba_den, 1))

cat("\nSánchez 2025 wOBA (MIA):", round(sanchez_prior$wOBA_prior, 3),
    "over", sanchez_prior$PA_2025, "PA\n")

# Combine all priors
all_priors <- bind_rows(priors_jays, sanchez_prior)

# 6c. Sugano-specific observations per hitter
sug_obs <- vs_sugano_pa |>
  group_by(player_name) |>
  summarise(
    n_PA_sug     = n(),
    sug_woba_num = sum(woba_val),
    sug_woba_den = sum(woba_den),
    .groups      = "drop"
  )

# 6d. Join and compute posteriors
bayes_df <- all_priors |>
  left_join(sug_obs, by = "player_name") |>
  replace_na(list(n_PA_sug = 0, sug_woba_num = 0, sug_woba_den = 0)) |>
  mutate(
    # Beta prior parameters
    alpha_0    = wOBA_prior * K_PRIOR,
    beta_0     = (1 - wOBA_prior) * K_PRIOR,
    # Posterior parameters
    alpha_post = alpha_0 + sug_woba_num,
    beta_post  = beta_0 + (sug_woba_den - sug_woba_num),
    # Posterior summary
    wOBA_posterior = alpha_post / (alpha_post + beta_post),
    CI_lo_90       = qbeta(0.05, alpha_post, beta_post),
    CI_hi_90       = qbeta(0.95, alpha_post, beta_post),
    CI_lo_95       = qbeta(0.025, alpha_post, beta_post),
    CI_hi_95       = qbeta(0.975, alpha_post, beta_post),
    # Shift direction flag
    direction      = case_when(
      n_PA_sug == 0        ~ "prior only",
      wOBA_posterior > wOBA_prior + 0.005 ~ "upward update",
      wOBA_posterior < wOBA_prior - 0.005 ~ "downward update",
      TRUE                               ~ "minimal update"
    ),
    has_sugano_data = n_PA_sug >= 2
  ) |>
  arrange(desc(wOBA_posterior))

cat("\n=== BAYESIAN POSTERIORS ===\n")
bayes_df |>
  select(player_name, PA_2025, wOBA_prior, n_PA_sug,
         wOBA_posterior, CI_lo_90, CI_hi_90, direction) |>
  mutate(across(where(is.numeric), \(x) round(x, 3))) |>
  print(n = Inf)

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

# 7a. Arsenal usage bar chart
p_arsenal <- arsenal |>
  mutate(pitch_name = fct_reorder(pitch_name, pct)) |>
  ggplot(aes(x = pct, y = pitch_name, fill = pitch_name)) +
  geom_col(width = 0.65, show.legend = FALSE) +
  geom_text(aes(label = paste0(round(pct, 1), "%")),
            hjust = -0.15, size = 3, color = "white", family = "mono") +
  scale_fill_manual(values = PITCH_COLORS) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(
    title    = "Sugano Arsenal, 2025 Usage Mix",
    subtitle = "All batters, 2,573 pitches",
    x        = "Usage %",
    y        = NULL,
    caption  = "Statcast via Baseball Savant, @scherrology-sys"
  ) +
  theme_jays()

ggsave("plot_01_arsenal.png", p_arsenal, width = 8, height = 5, dpi = 150, bg = "#002050")
cat("\nSaved: plot_01_arsenal.png\n")

# 7b. Pitch mix by handedness (faceted)
p_hand <- arsenal_by_hand |>
  mutate(
    pitch_name = fct_reorder(pitch_name, pct),
    stand_label = if_else(stand == "L", "vs. LHH", "vs. RHH")
  ) |>
  ggplot(aes(x = pct, y = pitch_name, fill = pitch_name)) +
  geom_col(width = 0.65, show.legend = FALSE) +
  geom_text(aes(label = paste0(round(pct, 1), "%")),
            hjust = -0.15, size = 2.8, color = "white", family = "mono") +
  scale_fill_manual(values = PITCH_COLORS) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.2))) +
  facet_wrap(~stand_label) +
  labs(
    title    = "Sugano Pitch Mix, By Batter Handedness",
    subtitle = "RHH see heavy Sweeper + Sinker, LHH face Split-Finger as primary weapon",
    x        = "Usage %",
    y        = NULL,
    caption  = "Statcast via Baseball Savant, @scherrology-sys"
  ) +
  theme_jays()

ggsave("plot_02_handedness.png", p_hand, width = 10, height = 5, dpi = 150, bg = "#002050")
cat("Saved: plot_02_handedness.png\n")

# 7c. Head-to-head matchup bar chart
p_matchup <- matchup_2025 |>
  mutate(
    player_label = str_replace(player_name, ",.*", ""),
    fill_color   = case_when(
      wOBA >= 0.400 ~ "strong",
      wOBA >= 0.320 ~ "average",
      TRUE          ~ "weak"
    )
  ) |>
  ggplot(aes(x = wOBA, y = fct_reorder(player_label, wOBA), fill = fill_color)) +
  geom_col(width = 0.65, show.legend = FALSE) +
  geom_vline(xintercept = LEAGUE_WOBA, linetype = "dashed",
             color = "#E8002D", linewidth = 0.6) +
  geom_text(aes(label = paste0(wOBA, "  (", PA, " PA)")),
            hjust = -0.1, size = 2.8, color = "white", family = "mono") +
  scale_fill_manual(values = c("strong" = "#2dd4a0", "average" = "#f4b942", "weak" = "#ff4f6a")) +
  scale_x_continuous(limits = c(0, 0.65), expand = expansion(mult = c(0, 0.15))) +
  annotate("text", x = LEAGUE_WOBA + 0.01, y = 0.6, label = "Lg Avg",
           color = "#E8002D", size = 2.5, hjust = 0, family = "mono") +
  labs(
    title    = "Blue Jays vs. Sugano, 2025 Season",
    subtitle = "wOBA from actual plate appearance outcomes, sparse sample (2,7 PA per hitter)",
    x        = "wOBA vs Sugano",
    y        = NULL,
    caption  = "Statcast via Baseball Savant, @scherrology-sys"
  ) +
  theme_jays()

ggsave("plot_03_matchup.png", p_matchup, width = 9, height = 6, dpi = 150, bg = "#002050")
cat("Saved: plot_03_matchup.png\n")

# 7d. Bayesian posterior chart (lollipop with CI)
p_bayes <- bayes_df |>
  mutate(
    short_name  = str_replace(player_name, ",.*", ""),
    dot_color   = if_else(has_sugano_data, "#2dd4a0", "#f4b942"),
    update_direction = direction
  ) |>
  ggplot(aes(y = fct_reorder(short_name, wOBA_posterior))) +
  geom_segment(aes(x = CI_lo_90, xend = CI_hi_90, yend = fct_reorder(short_name, wOBA_posterior)),
               color = "white", alpha = 0.2, linewidth = 1.5, lineend = "round") +
  geom_point(aes(x = wOBA_prior), shape = 21, size = 3,
             fill = NA, color = "white", alpha = 0.35, stroke = 1) +
  geom_point(aes(x = wOBA_posterior, color = has_sugano_data), size = 4.5) +
  scale_color_manual(
    values = c("TRUE" = "#2dd4a0", "FALSE" = "#f4b942"),
    labels = c("TRUE" = "Posterior (with Sugano data)", "FALSE" = "Prior only (no Sugano history)")
  ) +
  geom_vline(xintercept = LEAGUE_WOBA, linetype = "dashed",
             color = "#E8002D", linewidth = 0.6) +
  geom_text(aes(x = CI_hi_90 + 0.008, label = sprintf("%.3f", wOBA_posterior)),
            size = 2.8, color = "#c8ddf0", hjust = 0, family = "mono") +
  scale_x_continuous(limits = c(0.15, 0.57), breaks = seq(0.15, 0.55, 0.05)) +
  annotate("text", x = LEAGUE_WOBA + 0.005, y = 0.5, label = "Lg Avg (.320)",
           color = "#E8002D", size = 2.5, hjust = 0, family = "mono") +
  labs(
    title    = "Bayesian Posterior wOBA, Blue Jays vs. Sugano",
    subtitle = paste0(
      "Beta-Binomial model, k=", K_PRIOR, " prior equivalent, ",
      "90% credible intervals shown\n",
      "Open circle = prior, filled = posterior, bar = 90% CI"
    ),
    x        = "wOBA (posterior estimate)",
    y        = NULL,
    color    = NULL,
    caption  = "Statcast via Baseball Savant, Beta-Binomial empirical Bayes, @scherrology-sys"
  ) +
  theme_jays() +
  theme(legend.position = "bottom")

ggsave("plot_04_bayes.png", p_bayes, width = 9, height = 7, dpi = 150, bg = "#002050")
cat("Saved: plot_04_bayes.png\n")

# 7e. Movement profile scatter
p_movement <- arsenal |>
  ggplot(aes(x = avg_pfx_x, y = avg_pfx_z, fill = pitch_name, size = pct)) +
  geom_hline(yintercept = 0, color = "white", alpha = 0.1) +
  geom_vline(xintercept = 0, color = "white", alpha = 0.1) +
  geom_point(shape = 21, color = "white", alpha = 0.9, stroke = 1) +
  geom_text(aes(label = pitch_name), vjust = -1.2, size = 2.5,
            color = "white", family = "mono") +
  scale_fill_manual(values = PITCH_COLORS, guide = "none") +
  scale_size_continuous(range = c(4, 14), guide = "none") +
  labs(
    title    = "Sugano Pitch Movement Profile, Pitcher POV",
    subtitle = "pfx_x = horizontal break (inches), pfx_z = vertical break relative to gravity\nCircle size proportional to usage %",
    x        = "Horizontal Break (in)",
    y        = "Induced Vertical Break (in)",
    caption  = "Statcast via Baseball Savant, @scherrology-sys"
  ) +
  theme_jays()

ggsave("plot_05_movement.png", p_movement, width = 8, height = 6, dpi = 150, bg = "#002050")
cat("Saved: plot_05_movement.png\n")

# =============================================================================
# 8. EXPORT CLEAN DATA TABLES
# =============================================================================

write_csv(arsenal,    "output_arsenal.csv")
write_csv(bayes_df,   "output_bayes_posteriors.csv")
write_csv(matchup_2025, "output_matchup_2025.csv")
write_csv(zone_freq,  "output_zone_frequencies.csv")

cat("\nAll outputs written successfully.\n")
cat("Plots: plot_01 through plot_05\n")
cat("Tables: output_arsenal, output_bayes_posteriors, output_matchup_2025, output_zone_frequencies\n")

# =============================================================================
# END
# =============================================================================
