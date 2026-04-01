# =============================================================================
# Jays Matchup Intel · Preview
# 2026-04-01 · TOR vs COL · Kyle Freeland LHP
# Model v2: Arsenal-Weighted Prior
# @scherrology | Arm Chair Analyst
# =============================================================================
# Data:
#   freeland-24-26.csv             , Freeland 2024-2026 pitcher file
#   blue-jays-vs-freeland-24-26.csv, Blue Jays hitters vs Freeland (2024)
#   blue-jays-hitters-25-26.csv    , Jays 2025-2026 batter file
#   sanchez-24-26.csv              , Sánchez approach file
#
# Data quality note:
#   All matchup data is 2024 only. Freeland is a fundamentally different
#   pitcher in 2025: Sinker dropped from 27% to 6%, Sweeper added from
#   scratch to 13%. Model uses 2025 pitch mix as the arsenal prior.
#   2024 matchup observations are included but flagged as stale.
# =============================================================================

library(tidyverse)
library(scales)

PA_EVENTS <- c(
  "single","double","triple","home_run","field_out","strikeout",
  "walk","hit_by_pitch","grounded_into_double_play","force_out",
  "sac_fly","fielders_choice","fielders_choice_out","double_play"
)
WOBA_WEIGHTS <- c(
  walk=0.696, hit_by_pitch=0.726, single=0.888,
  double=1.271, triple=1.616, home_run=2.101
)
K_PRIOR     <- 60
MIN_PA      <- 10
LEAGUE_WOBA <- 0.320

JAYS_2026 <- c(
  "Barger, Addison","Clement, Ernie","Giménez, Andrés",
  "Guerrero Jr., Vladimir","Heineman, Tyler","Kirk, Alejandro",
  "Lukes, Nathan","Okamoto, Kazuma","Schneider, Davis",
  "Springer, George","Straw, Myles","Sánchez, Jesús","Varsho, Daulton"
)

# Freeland 2025 pitch mix by handedness
FREELAND_LHH_MIX <- c(
  "Sweeper"         = 0.310,
  "4-Seam Fastball" = 0.294,
  "Knuckle Curve"   = 0.201,
  "Cutter"          = 0.136,
  "Sinker"          = 0.059
)
FREELAND_RHH_MIX <- c(
  "4-Seam Fastball" = 0.344,
  "Knuckle Curve"   = 0.276,
  "Cutter"          = 0.162,
  "Sweeper"         = 0.082,
  "Changeup"        = 0.074,
  "Sinker"          = 0.062
)

PITCH_COLORS <- c(
  "4-Seam Fastball" = "#E8002D",
  "Sinker"          = "#f4b942",
  "Sweeper"         = "#2dd4a0",
  "Knuckle Curve"   = "#60a5fa",
  "Cutter"          = "#c084fc",
  "Changeup"        = "#fb923c"
)

# =============================================================================
# 1. LOAD
# =============================================================================
freeland    <- read_csv("freeland-24-26.csv",              show_col_types=FALSE)
jvf_raw     <- read_csv("blue-jays-vs-freeland-24-26.csv", show_col_types=FALSE)
jays_full   <- read_csv("blue-jays-hitters-25-26.csv",     show_col_types=FALSE)
sanchez_raw <- read_csv("sanchez-24-26.csv",               show_col_types=FALSE)

f25 <- freeland |> filter(game_year == 2025)
f24 <- freeland |> filter(game_year == 2024)
jvf <- jvf_raw  |> filter(player_name %in% JAYS_2026)

cat("Freeland 2025 pitches:", nrow(f25), "\n")
cat("Freeland 2024 pitches:", nrow(f24), "\n")
cat("Jays vs Freeland (2026 roster, 2024 data):", nrow(jvf), "pitches\n")
cat("Hitters with history:", paste(unique(jvf$player_name), collapse=", "), "\n")
cat("Hitters without:", paste(setdiff(JAYS_2026, unique(jvf$player_name)), collapse=", "), "\n")

# =============================================================================
# 2. ARSENAL ANALYSIS
# =============================================================================
pitch_counts <- function(df) {
  df |>
    filter(!is.na(pitch_name)) |>
    count(pitch_name) |>
    mutate(pct = n / sum(n) * 100)
}

arsenal_25_raw <- f25 |>
  filter(!is.na(pitch_name)) |>
  group_by(pitch_name) |>
  summarise(
    n_25       = n(),
    velo_25    = mean(release_speed, na.rm=TRUE),
    spin_25    = mean(release_spin_rate, na.rm=TRUE),
    pfx_x_25   = mean(pfx_x, na.rm=TRUE),
    pfx_z_25   = mean(pfx_z, na.rm=TRUE),
    whiff_n    = sum(description == "swinging_strike", na.rm=TRUE),
    swing_n    = sum(description %in% c("swinging_strike","foul","hit_into_play"), na.rm=TRUE),
    cs_n       = sum(description == "called_strike", na.rm=TRUE),
    xwoba_25   = mean(estimated_woba_using_speedangle, na.rm=TRUE),
    .groups    = "drop"
  ) |>
  mutate(
    pct_25      = n_25 / sum(n_25) * 100,
    whiff_rate  = whiff_n / pmax(swing_n,1) * 100,
    csw_rate    = (whiff_n + cs_n) / n_25 * 100
  )

arsenal_24_raw <- f24 |>
  filter(!is.na(pitch_name)) |>
  group_by(pitch_name) |>
  summarise(n_24=n(), .groups="drop") |>
  mutate(pct_24 = n_24 / sum(n_24) * 100)

arsenal_comp <- arsenal_25_raw |>
  left_join(arsenal_24_raw, by="pitch_name") |>
  replace_na(list(pct_24=0)) |>
  mutate(pct_delta = round(pct_25 - pct_24, 1),
         evolved   = pct_24 == 0 & pct_25 > 5) |>
  arrange(desc(n_25))

cat("\n=== ARSENAL COMPARISON 2025 vs 2024 ===\n")
arsenal_comp |>
  select(pitch_name, pct_25, pct_24, pct_delta, velo_25,
         pfx_x_25, pfx_z_25, whiff_rate, csw_rate, xwoba_25, evolved) |>
  mutate(across(where(is.numeric), \(x) round(x,2))) |>
  print(n=Inf)

# Handedness splits
hand_mix <- f25 |>
  filter(!is.na(pitch_name), !is.na(stand)) |>
  group_by(stand, pitch_name) |>
  summarise(n=n(), .groups="drop") |>
  group_by(stand) |>
  mutate(pct = n/sum(n)*100) |>
  ungroup() |>
  arrange(stand, desc(pct))

cat("\n=== PITCH MIX BY HANDEDNESS 2025 ===\n")
hand_mix |>
  select(stand, pitch_name, pct) |>
  mutate(pct=round(pct,1)) |>
  pivot_wider(names_from=stand, values_from=pct, values_fill=0) |>
  print(n=Inf)

# Stability (game-to-game CV)
game_mix <- f25 |>
  filter(!is.na(pitch_name)) |>
  group_by(game_pk, pitch_name) |>
  summarise(n=n(), .groups="drop") |>
  group_by(game_pk) |>
  mutate(pct=n/sum(n)*100) |>
  ungroup()

stability <- game_mix |>
  group_by(pitch_name) |>
  summarise(
    mean_pct = mean(pct),
    sd_pct   = sd(pct),
    cv       = sd_pct/mean_pct,
    n_games  = n(),
    .groups  = "drop"
  ) |>
  mutate(flag = case_when(cv>=0.5~"volatile", cv>=0.25~"moderate", TRUE~"stable")) |>
  arrange(desc(cv))

cat("\n=== ARSENAL STABILITY ===\n")
stability |> mutate(across(where(is.numeric), \(x) round(x,3))) |> print(n=Inf)

# Zone frequencies
cat("\n=== ZONE FREQUENCIES 2025 ===\n")
cat("LHH:", deparse(as.list(sort(table(f25[f25$stand=="L","zone"])))),"\n")
cat("RHH:", deparse(as.list(sort(table(f25[f25$stand=="R","zone"])))),"\n")

# =============================================================================
# 3. MATCHUP DATA (2024 only, flagged stale)
# =============================================================================
pa_jvf <- jvf |>
  filter(events %in% PA_EVENTS) |>
  mutate(
    woba_val = coalesce(WOBA_WEIGHTS[events], 0),
    woba_den = as.integer(events != "hit_by_pitch")
  )

matchup_2024 <- pa_jvf |>
  group_by(player_name) |>
  summarise(
    PA           = n(),
    H            = sum(events %in% c("single","double","triple","home_run")),
    BB           = sum(events == "walk"),
    K            = sum(events == "strikeout"),
    HR           = sum(events == "home_run"),
    woba_num     = sum(woba_val),
    woba_den     = sum(woba_den),
    wOBA_2024    = woba_num / pmax(woba_den,1),
    outcomes     = paste(events, collapse=", "),
    data_note    = "2024 only · stale · Freeland rebuilt arsenal",
    .groups      = "drop"
  )

cat("\n=== JAYS VS FREELAND 2024 (2026 roster) ===\n")
matchup_2024 |>
  select(player_name, PA, H, BB, K, HR, wOBA_2024, data_note, outcomes) |>
  mutate(wOBA_2024=round(wOBA_2024,3)) |>
  print(n=Inf)

# =============================================================================
# 4. MODEL V2: ARSENAL-WEIGHTED PRIOR
# =============================================================================
jays_2025    <- jays_full  |> filter(game_year==2025)
san_2025     <- sanchez_raw|> filter(game_year==2025)
all_2025     <- bind_rows(jays_2025, san_2025)

pa_2025 <- all_2025 |>
  filter(events %in% PA_EVENTS) |>
  mutate(
    woba_val = coalesce(WOBA_WEIGHTS[events], 0),
    woba_den = as.integer(events != "hit_by_pitch")
  )

overall_prior <- pa_2025 |>
  group_by(player_name) |>
  summarise(
    PA_2025      = n(),
    op_wn        = sum(woba_val),
    op_wd        = sum(woba_den),
    wOBA_overall = op_wn / pmax(op_wd,1),
    .groups      = "drop"
  )

pitch_splits <- pa_2025 |>
  group_by(player_name, pitch_name) |>
  summarise(
    PA_vs      = n(),
    sp_wn      = sum(woba_val),
    sp_wd      = sum(woba_den),
    pitch_wOBA = sp_wn / pmax(sp_wd,1),
    .groups    = "drop"
  )

hitter_hand <- all_2025 |>
  group_by(player_name) |>
  summarise(hand = names(sort(table(stand),decreasing=TRUE))[1], .groups="drop")

compute_arsenal_prior <- function(player, hand_val, fallback) {
  mix <- if (hand_val=="L") FREELAND_LHH_MIX else FREELAND_RHH_MIX
  splits <- pitch_splits |>
    filter(player_name==player, PA_vs>=MIN_PA) |>
    select(pitch_name, pitch_wOBA)
  w_sum <- 0; w_matched <- 0
  for (pitch in names(mix)) {
    wt  <- mix[pitch]
    row <- filter(splits, pitch_name==pitch)
    if (nrow(row)>0 && !is.na(row$pitch_wOBA)) {
      w_sum    <- w_sum    + wt * row$pitch_wOBA
      w_matched <- w_matched + wt
    }
  }
  w_sum + (1-w_matched) * fallback
}

priors_tbl <- overall_prior |>
  left_join(hitter_hand, by="player_name") |>
  mutate(
    hand         = replace_na(hand,"R"),
    wOBA_arsenal = map2_dbl(player_name, hand,
      ~compute_arsenal_prior(.x,.y,
        overall_prior$wOBA_overall[overall_prior$player_name==.x])),
    prior_delta  = round(wOBA_arsenal - wOBA_overall, 3)
  )

cat("\n=== ARSENAL-WEIGHTED PRIORS ===\n")
priors_tbl |>
  select(player_name, hand, PA_2025, wOBA_overall, wOBA_arsenal, prior_delta) |>
  arrange(prior_delta) |>
  mutate(across(where(is.numeric), \(x) round(x,3))) |>
  print(n=Inf)

# Bayesian update
obs_tbl <- matchup_2024 |>
  select(player_name, n_PA=PA, m_wn=woba_num, m_wd=woba_den)

bayes_df <- priors_tbl |>
  left_join(obs_tbl, by="player_name") |>
  replace_na(list(n_PA=0, m_wn=0, m_wd=0)) |>
  mutate(
    alpha_0    = wOBA_arsenal * K_PRIOR,
    beta_0     = (1-wOBA_arsenal) * K_PRIOR,
    alpha_post = alpha_0 + m_wn,
    beta_post  = beta_0  + (m_wd - m_wn),
    wOBA_post  = alpha_post / (alpha_post + beta_post),
    CI_lo_90   = qbeta(0.05, alpha_post, beta_post),
    CI_hi_90   = qbeta(0.95, alpha_post, beta_post),
    tier       = case_when(
      player_name=="Okamoto, Kazuma" ~ "T3",
      n_PA >= 2                      ~ "T1",
      TRUE                           ~ "T2"
    ),
    data_note  = if_else(n_PA>0, "2024 matchup data · stale", "prior only")
  ) |>
  arrange(desc(wOBA_post))

cat("\n=== BAYESIAN POSTERIORS (MODEL V2) ===\n")
bayes_df |>
  select(player_name, hand, tier, PA_2025, wOBA_overall, wOBA_arsenal,
         prior_delta, n_PA, wOBA_post, CI_lo_90, CI_hi_90, data_note) |>
  mutate(across(where(is.numeric), \(x) round(x,3))) |>
  print(n=Inf)

# =============================================================================
# 5. PLOTS
# =============================================================================
theme_jays <- function() {
  theme_minimal(base_family="sans") +
    theme(
      plot.background  = element_rect(fill="#002050",color=NA),
      panel.background = element_rect(fill="#071e3d",color=NA),
      panel.grid.major = element_line(color="rgba(255,255,255,0.06)",linewidth=0.4),
      panel.grid.minor = element_blank(),
      text             = element_text(color="#c8ddf0"),
      axis.text        = element_text(color="#7a9cc0",size=8),
      axis.title       = element_text(color="#c8ddf0",size=9),
      plot.title       = element_text(color="white",size=13,face="bold"),
      plot.subtitle    = element_text(color="#7a9cc0",size=9),
      plot.caption     = element_text(color="#4a7ca0",size=7),
      legend.background= element_rect(fill="#071e3d",color=NA),
      legend.text      = element_text(color="#7a9cc0",size=8)
    )
}

# Arsenal comparison
p_arsenal <- arsenal_comp |>
  filter(pct_25>0|pct_24>0) |>
  pivot_longer(c(pct_25,pct_24), names_to="year", values_to="pct") |>
  mutate(
    year       = if_else(year=="pct_25","2025 Actual","2024 Reference"),
    pitch_name = fct_reorder(pitch_name, pct, .fun=max)
  ) |>
  ggplot(aes(x=pct, y=pitch_name, fill=year, alpha=year)) +
  geom_col(position=position_dodge(width=0.7), width=0.6) +
  scale_fill_manual(values=c("2025 Actual"="#C4A24B","2024 Reference"="white")) +
  scale_alpha_manual(values=c("2025 Actual"=0.9,"2024 Reference"=0.25)) +
  annotate("text", x=14, y="Sweeper", label="NEW IN 2025",
           color="#2dd4a0", size=2.5, hjust=0, family="mono") +
  annotate("text", x=7, y="Sinker", label="DOWN 20 pts",
           color="#ff4f6a", size=2.5, hjust=0, family="mono") +
  labs(title="Freeland Arsenal, 2025 vs 2024",
       subtitle="Sinker shelved, Sweeper added from scratch",
       x="Usage %", y=NULL, fill=NULL, alpha=NULL,
       caption="Statcast via Baseball Savant, @scherrology") +
  theme_jays() + theme(legend.position="bottom")

ggsave("freeland_plot_01_arsenal.png", p_arsenal, width=9, height=5, dpi=150, bg="#002050")
cat("\nSaved: freeland_plot_01_arsenal.png\n")

# Prior delta
p_delta <- bayes_df |>
  filter(tier!="T3") |>
  mutate(
    short_name  = str_replace(player_name,",.*",""),
    delta_color = case_when(
      prior_delta>=0.020 ~ "#2dd4a0",
      prior_delta<=-0.020~ "#ff4f6a",
      TRUE               ~ "#7a9cc0"
    )
  ) |>
  ggplot(aes(y=fct_reorder(short_name, wOBA_arsenal))) +
  geom_segment(aes(x=wOBA_overall, xend=wOBA_arsenal,
                   yend=fct_reorder(short_name,wOBA_arsenal)),
               color="white", alpha=0.2, linewidth=1.2,
               arrow=arrow(length=unit(0.08,"inches"),type="closed")) +
  geom_point(aes(x=wOBA_overall), shape=21, size=3,
             fill=NA, color="white", alpha=0.4, stroke=1) +
  geom_point(aes(x=wOBA_arsenal, color=delta_color), size=4.5) +
  scale_color_identity() +
  geom_vline(xintercept=LEAGUE_WOBA, linetype="dashed",
             color="#E8002D", linewidth=0.6) +
  geom_text(aes(x=wOBA_arsenal+0.005,
                label=sprintf("%+.3f", prior_delta),
                color=delta_color),
            size=2.8, hjust=0, family="mono") +
  labs(title="Arsenal-Weighted Prior vs Overall wOBA · Freeland",
       subtitle="Varsho drops most (-0.058) as LHH facing 31% Sweeper",
       x="wOBA", y=NULL,
       caption="Model v2, @scherrology") +
  theme_jays()

ggsave("freeland_plot_02_prior_delta.png", p_delta, width=9, height=6, dpi=150, bg="#002050")
cat("Saved: freeland_plot_02_prior_delta.png\n")

# Posteriors
tier_colors <- c(T1="#2dd4a0", T2="#f4b942", T3="#ff4f6a")

p_bayes <- bayes_df |>
  filter(tier!="T3") |>
  mutate(short_name=str_replace(player_name,",.*","")) |>
  ggplot(aes(y=fct_reorder(short_name, wOBA_post))) +
  geom_segment(aes(x=CI_lo_90, xend=CI_hi_90,
                   yend=fct_reorder(short_name,wOBA_post)),
               color="white", alpha=0.2, linewidth=1.5, lineend="round") +
  geom_point(aes(x=wOBA_arsenal), shape=21, size=3,
             fill=NA, color="white", alpha=0.35, stroke=1.2) +
  geom_point(aes(x=wOBA_post, color=tier), size=4.5) +
  scale_color_manual(values=tier_colors,
                     labels=c(T1="T1 · 2024 matchup (stale)",
                               T2="T2 · Arsenal prior only")) +
  geom_vline(xintercept=LEAGUE_WOBA, linetype="dashed",
             color="#E8002D", linewidth=0.6) +
  geom_text(aes(x=CI_hi_90+0.008, label=sprintf("%.3f",wOBA_post)),
            size=2.8, color="#c8ddf0", hjust=0, family="mono") +
  annotate("text", x=LEAGUE_WOBA+0.005, y=0.5,
           label="Lg Avg (.320)", color="#E8002D", size=2.5, hjust=0) +
  scale_x_continuous(limits=c(0.15,0.58), breaks=seq(0.15,0.55,0.05)) +
  labs(title="Bayesian Posteriors · Jays vs Freeland",
       subtitle="Model v2: Arsenal-weighted prior, 2024 matchup data flagged stale",
       x="wOBA (posterior estimate)", y=NULL, color=NULL,
       caption="Statcast via Baseball Savant, Beta-Binomial Model v2, @scherrology") +
  theme_jays() + theme(legend.position="bottom")

ggsave("freeland_plot_03_posteriors.png", p_bayes, width=9, height=7, dpi=150, bg="#002050")
cat("Saved: freeland_plot_03_posteriors.png\n")

# =============================================================================
# 6. EXPORT
# =============================================================================
write_csv(arsenal_comp,  "freeland_output_arsenal.csv")
write_csv(hand_mix,      "freeland_output_hand_mix.csv")
write_csv(stability,     "freeland_output_stability.csv")
write_csv(matchup_2024,  "freeland_output_matchup_2024.csv")
write_csv(bayes_df,      "freeland_output_posteriors.csv")

cat("\nAll outputs written.\n")
