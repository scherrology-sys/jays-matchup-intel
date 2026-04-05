# =============================================================================
# Jays Matchup Intel · Self-Learning Preview + Retro System
# @scherrology | Arm Chair Analyst
# =============================================================================
#
# Core loop
# -----------------------------------------------------------------------------
# Preview -> Observe -> Audit -> Update Memory -> Next Preview
#
# What changed in this version
# -----------------------------------------------------------------------------
# compute_weighted_pitcher_sample()  [NEW, section D]
#
#   The previous system read only the current season's pitches to determine
#   the pitch mix and confidence tier. A pitcher with 26 starts in 2025 and
#   1 start in 2026 was treated as a 92-pitch unknown. All prior-year signal
#   was thrown away.
#
#   This function takes the full multi-year Statcast file for a pitcher and
#   applies exponential recency decay (default half_life = 180 days). Each
#   pitch is weighted by how long ago it was thrown. The weighted mix and an
#   effective sample count are returned. A pitcher with 2,315 pitches in 2025
#   and 92 in 2026 produces an effective sample of ~981, not 92. Confidence
#   tiers and pitch mixes now reflect everything we know, weighted by recency.
#
#   The half-life of 180 days means a pitch thrown at 2025 mid-season carries
#   roughly 26% the weight of a pitch thrown today. A pitch from 2024 carries
#   about 9%. Old data informs but does not dominate.
#
#   build_preview() now takes pitcher_raw_df (the full file) instead of
#   pitcher_sample + pitcher_mix_lhh + pitcher_mix_rhh. The mix is derived
#   automatically. Manual overrides are still available if needed.
#
# File structure
# -----------------------------------------------------------------------------
# A. Shared constants and utilities
# B. Model memory helpers
# C. Pitch type mapping
# D. Multi-year weighted pitcher sample    [NEW]
# E. Data-derived hitter exposure weights
# F. Auto-derive assumption actuals
# G. Assumption sensitivity mapping
# H. Preview
# I. Computed execution flags
# J. Retro
# K. Example run blocks
# =============================================================================

library(tidyverse)
library(lubridate)

# =============================================================================
# A. SHARED CONSTANTS AND UTILITIES
# =============================================================================

PA_EVENTS <- c(
  "single","double","triple","home_run","field_out","strikeout",
  "walk","hit_by_pitch","grounded_into_double_play","force_out",
  "sac_fly","fielders_choice","fielders_choice_out","double_play"
)

WOBA_WEIGHTS <- c(
  walk=0.696, hit_by_pitch=0.726, single=0.888,
  double=1.271, triple=1.616, home_run=2.101
)

LEAGUE_WOBA <- 0.320
WOBA_SCALE  <- 1.20
K_PRIOR     <- 60
MIN_PA      <- 10

safe_dir_create <- function(path) {
  if (!dir.exists(path)) dir.create(path, recursive = TRUE)
  invisible(path)
}

safe_read_csv <- function(path, ...) {
  if (!file.exists(path)) stop("File not found: ", path)
  read_csv(path, show_col_types = FALSE, ...)
}

calc_exp_runs_per_pa <- function(exp_woba) {
  (exp_woba - LEAGUE_WOBA) / WOBA_SCALE
}

classify_confidence <- function(pitch_sample, historical_mae = NA_real_) {
  if (is.na(historical_mae)) {
    case_when(
      pitch_sample < 200  ~ "LOW",
      pitch_sample < 700  ~ "MODERATE",
      TRUE                ~ "HIGH"
    )
  } else {
    case_when(
      pitch_sample < 200                                                   ~ "LOW",
      pitch_sample < 300  & historical_mae > 0.080                        ~ "LOW",
      pitch_sample >= 300 & pitch_sample < 700 & historical_mae <= 0.060  ~ "MODERATE",
      pitch_sample >= 700 & historical_mae <= 0.040                       ~ "HIGH",
      TRUE                                                                 ~ "MODERATE"
    )
  }
}

label_assumption_result <- function(expected_value, actual_value, tolerance = 0.05) {
  if (is.na(expected_value) || is.na(actual_value) || is.na(tolerance)) return("not scored")
  diff <- actual_value - expected_value
  case_when(
    abs(diff) <= tolerance       ~ "held",
    abs(diff) <= tolerance * 2   ~ "partial",
    TRUE                         ~ "failed"
  )
}

# =============================================================================
# B. MODEL MEMORY HELPERS
# =============================================================================

init_model_memory <- function(memory_dir = "model_memory") {
  safe_dir_create(memory_dir)
  hf <- file.path(memory_dir, "model_memory_hitter_audit.csv")
  af <- file.path(memory_dir, "model_memory_assumption_audit.csv")
  if (!file.exists(hf)) {
    tibble(
      game_date=as.Date(character()), team=character(), opponent=character(),
      pitcher=character(), pitcher_id=integer(), pitcher_archetype=character(),
      hitter=character(), PA=integer(), exp_wOBA=numeric(),
      actual_wOBA=numeric(), residual=numeric(), abs_error=numeric(),
      audit_flag=character(), miss_type=character()
    ) |> write_csv(hf)
  }
  if (!file.exists(af)) {
    tibble(
      game_date=as.Date(character()), team=character(), opponent=character(),
      pitcher=character(), pitcher_id=integer(), pitcher_archetype=character(),
      assumption_name=character(), expected_value=numeric(),
      actual_value=numeric(), tolerance=numeric(),
      audit_result=character(), assumption_error=numeric(), derived=logical()
    ) |> write_csv(af)
  }
  invisible(memory_dir)
}

append_model_memory <- function(new_data, file_path) {
  if (!file.exists(file_path)) write_csv(new_data, file_path)
  else bind_rows(safe_read_csv(file_path), new_data) |> write_csv(file_path)
}

get_historical_mae <- function(
  memory_dir, pitcher_archetype = NULL,
  current_date = Sys.Date(), half_life_days = 30
) {
  file <- file.path(memory_dir, "model_memory_hitter_audit.csv")
  if (!file.exists(file)) return(NA_real_)
  mem <- safe_read_csv(file)
  if (!is.null(pitcher_archetype) && "pitcher_archetype" %in% names(mem))
    mem <- mem |> filter(pitcher_archetype == !!pitcher_archetype)
  if (nrow(mem) == 0 || !"abs_error" %in% names(mem)) return(NA_real_)
  mem |>
    mutate(
      game_date = as.Date(game_date),
      age_days  = pmax(as.numeric(as.Date(current_date) - game_date), 0),
      recency_w = 0.5^(age_days / half_life_days)
    ) |>
    with(weighted.mean(abs_error, recency_w, na.rm = TRUE))
}

get_assumption_reliability <- function(
  memory_dir, assumption_name,
  current_date = Sys.Date(), default_weight = 1.0, half_life_days = 30
) {
  file <- file.path(memory_dir, "model_memory_assumption_audit.csv")
  if (!file.exists(file)) return(default_weight)
  mem <- safe_read_csv(file)
  if (!all(c("assumption_name","audit_result","game_date") %in% names(mem)))
    return(default_weight)
  hist <- mem |>
    filter(assumption_name == !!assumption_name) |>
    mutate(
      game_date = as.Date(game_date),
      age_days  = pmax(as.numeric(as.Date(current_date) - game_date), 0),
      recency_w = 0.5^(age_days / half_life_days),
      fail_num  = case_when(
        audit_result == "failed"  ~ 1,
        audit_result == "partial" ~ 0.5,
        audit_result == "held"    ~ 0,
        TRUE                      ~ NA_real_
      )
    ) |>
    filter(!is.na(fail_num))
  if (nrow(hist) < 3) return(default_weight)
  max(0.35, 1 - weighted.mean(hist$fail_num, hist$recency_w, na.rm = TRUE))
}

summarise_model_memory <- function(memory_dir = "model_memory") {
  init_model_memory(memory_dir)
  hm <- safe_read_csv(file.path(memory_dir, "model_memory_hitter_audit.csv"))
  am <- safe_read_csv(file.path(memory_dir, "model_memory_assumption_audit.csv"))
  hitter_summary <- hm |>
    summarise(
      n_games              = n_distinct(game_date),
      n_hitter_games       = n(),
      mean_abs_error       = mean(abs_error, na.rm = TRUE),
      mean_bias            = mean(residual,  na.rm = TRUE),
      assumption_miss_rate = mean(miss_type == "assumption miss", na.rm = TRUE),
      execution_miss_rate  = mean(miss_type == "execution miss",  na.rm = TRUE),
      variance_miss_rate   = mean(miss_type == "variance miss",   na.rm = TRUE)
    )
  assumption_summary <- am |>
    mutate(fail_num = case_when(
      audit_result == "failed" ~ 1, audit_result == "partial" ~ 0.5,
      audit_result == "held"   ~ 0, TRUE ~ NA_real_)) |>
    group_by(assumption_name) |>
    summarise(
      n = n(), fail_rate = mean(audit_result == "failed", na.rm = TRUE),
      mean_fail_score = mean(fail_num, na.rm = TRUE),
      mean_abs_error  = mean(abs(assumption_error), na.rm = TRUE),
      .groups = "drop"
    ) |>
    arrange(desc(mean_fail_score))
  list(hitter_summary = hitter_summary, assumption_summary = assumption_summary)
}

# =============================================================================
# C. PITCH TYPE MAPPING
# =============================================================================

build_pitch_type_mapping <- function(
  sweeper_names   = c("Sweeper"),
  changeup_names  = c("Changeup"),
  fastball_names  = c("4-Seam Fastball", "Fastball", "Sinker")
) {
  list(
    changeup_to_lhh_pct = list(
      pitches = changeup_names, hand_filter = "L",
      denominator = "same_hand", derive_exposure = FALSE
    ),
    sweeper_usage_pct = list(
      pitches = sweeper_names, hand_filter = NULL,
      denominator = "all", derive_exposure = TRUE
    )
  )
}

# =============================================================================
# D. MULTI-YEAR WEIGHTED PITCHER SAMPLE
# =============================================================================
#
# Parameters:
#   pitcher_raw_df   Full multi-year Statcast file for the pitcher (all seasons).
#   game_date        Reference date for decay (typically today's game date).
#   half_life_days   Days for pitch weight to halve. Default 180.
#                    180 days = mid-2025 pitch carries ~26% weight vs today.
#                    360 days = mid-2025 pitch carries ~35% weight vs today.
#                    Flatten to 360 for pitchers with career < 500 pitches.
#
# Returns:
#   pitcher_mix_lhh   Named numeric: weighted pitch fractions vs left-handed hitters
#   pitcher_mix_rhh   Named numeric: weighted pitch fractions vs right-handed hitters
#   effective_sample  Integer: sum of recency weights, used for confidence tier
#   raw_by_year       Tibble: year / n_pitches / eff_mass, for transparency in output
#   n_years           Number of distinct seasons in the data
#
# The column game_date in Statcast exports is character "YYYY-MM-DD".
# The function handles both character and Date inputs.

compute_weighted_pitcher_sample <- function(
  pitcher_raw_df,
  game_date,
  half_life_days = 180
) {
  ref <- as.Date(game_date)

  df <- pitcher_raw_df |>
    filter(!is.na(pitch_name), !pitch_name %in% c("Unknown","")) |>
    mutate(
      pitch_date = as.Date(game_date),
      age_days   = pmax(as.numeric(ref - pitch_date), 0),
      recency_w  = 0.5^(age_days / half_life_days)
    )

  if (nrow(df) == 0) {
    return(list(
      pitcher_mix_lhh  = numeric(0),
      pitcher_mix_rhh  = numeric(0),
      effective_sample = 0L,
      raw_by_year      = tibble(yr = integer(), n_pitches = integer(), eff_mass = numeric()),
      n_years          = 0L
    ))
  }

  raw_by_year <- df |>
    group_by(yr = game_year) |>
    summarise(n_pitches = n(), eff_mass = round(sum(recency_w), 1), .groups = "drop") |>
    arrange(yr)

  effective_sample <- as.integer(round(sum(df$recency_w)))
  n_years          <- n_distinct(df$game_year)

  weighted_mix <- function(hand) {
    sub <- if (!is.null(hand)) filter(df, stand == hand) else df
    if (nrow(sub) == 0) return(named_numeric(0))
    sub |>
      group_by(pitch_name) |>
      summarise(w = sum(recency_w), .groups = "drop") |>
      mutate(frac = w / sum(w)) |>
      arrange(desc(frac)) |>
      with(setNames(round(frac, 4), pitch_name))
  }

  list(
    pitcher_mix_lhh  = weighted_mix("L"),
    pitcher_mix_rhh  = weighted_mix("R"),
    effective_sample = effective_sample,
    raw_by_year      = raw_by_year,
    n_years          = n_years
  )
}

# =============================================================================
# E. DATA-DERIVED HITTER EXPOSURE WEIGHTS
# =============================================================================

compute_hitter_exposure_weights <- function(hitter_history_df, pitch_type_mapping) {
  pa_hist <- hitter_history_df |>
    filter(!is.na(pitch_name), events %in% PA_EVENTS)
  map_dfr(names(pitch_type_mapping), function(asmp_name) {
    info <- pitch_type_mapping[[asmp_name]]
    if (!isTRUE(info$derive_exposure)) return(tibble())
    sub <- if (!is.null(info$hand_filter)) filter(pa_hist, stand == info$hand_filter) else pa_hist
    if (nrow(sub) == 0) return(tibble())
    exp <- sub |>
      group_by(player_name) |>
      summarise(total_pa = n(),
                type_pa  = sum(pitch_name %in% info$pitches, na.rm = TRUE),
                pitch_share = type_pa / pmax(total_pa, 1), .groups = "drop")
    max_share <- max(exp$pitch_share, na.rm = TRUE)
    exp |>
      mutate(assumption_name = asmp_name,
             exposure_weight = if_else(max_share > 0,
                                       round(0.50 + 0.50 * (pitch_share / max_share), 3),
                                       0.50)) |>
      select(player_name, assumption_name, exposure_weight)
  })
}

# =============================================================================
# F. AUTO-DERIVE ASSUMPTION ACTUALS FROM PITCH FILE
# =============================================================================

compute_assumption_actuals_from_pitchfile <- function(
  pitch_by_pitch_game_df, pitch_type_mapping
) {
  game <- pitch_by_pitch_game_df
  map_dfr(names(pitch_type_mapping), function(asmp_name) {
    info <- pitch_type_mapping[[asmp_name]]
    if (is.null(info$pitches))
      return(tibble(assumption_name = asmp_name, actual_value = NA_real_, derived = FALSE))
    sub <- if (!is.null(info$hand_filter)) filter(game, stand == info$hand_filter) else game
    if (nrow(sub) == 0)
      return(tibble(assumption_name = asmp_name, actual_value = NA_real_, derived = FALSE))
    tibble(assumption_name = asmp_name,
           actual_value    = round(mean(sub$pitch_name %in% info$pitches, na.rm = TRUE), 4),
           derived         = TRUE)
  })
}

# =============================================================================
# G. ASSUMPTION SENSITIVITY MAPPING
# =============================================================================

build_hitter_assumption_map <- function(
  preview_df, assumption_definitions_df, hitter_pitch_exposure_df = NULL
) {
  assumption_definitions_df <- assumption_definitions_df |>
    mutate(affected_hand = toupper(affected_hand))

  base_map <- tidyr::crossing(
    player_name     = preview_df$player_name,
    assumption_name = assumption_definitions_df$assumption_name
  ) |>
    left_join(preview_df |> select(player_name, hand, exp_wOBA_raw, wOBA_overall),
              by = "player_name") |>
    left_join(
      assumption_definitions_df |>
        select(assumption_name, affected_hand, impact_direction,
               default_shift, ci_widen_factor, importance_weight, description),
      by = "assumption_name"
    ) |>
    mutate(
      affected_by_hand   = case_when(
        affected_hand == "ALL" ~ TRUE, affected_hand == hand ~ TRUE, TRUE ~ FALSE),
      sensitivity_weight = if_else(affected_by_hand, 1, 0)
    )

  if (!is.null(hitter_pitch_exposure_df) && nrow(hitter_pitch_exposure_df) > 0) {
    base_map <- base_map |>
      left_join(hitter_pitch_exposure_df |>
                  select(player_name, assumption_name, exposure_weight),
                by = c("player_name","assumption_name")) |>
      mutate(sensitivity_weight = case_when(
        !is.na(exposure_weight) & affected_by_hand ~ exposure_weight,
        TRUE ~ sensitivity_weight)) |>
      select(-exposure_weight)
  }

  base_map |>
    mutate(sensitivity_weight = replace_na(sensitivity_weight, 0),
           default_shift      = replace_na(default_shift, 0),
           ci_widen_factor    = replace_na(ci_widen_factor, 1))
}

# =============================================================================
# H. PREVIEW
# =============================================================================

compute_arsenal_expectation <- function(
  player, hand_val, fallback, pitch_splits, pitcher_mix_lhh, pitcher_mix_rhh
) {
  mix    <- if (hand_val == "L") pitcher_mix_lhh else pitcher_mix_rhh
  splits <- pitch_splits |>
    filter(player_name == player, PA_vs >= MIN_PA) |>
    select(pitch_name, pitch_wOBA)
  w_sum <- 0; w_matched <- 0
  for (pitch in names(mix)) {
    wt  <- mix[pitch]
    row <- filter(splits, pitch_name == pitch)
    if (nrow(row) > 0 && !is.na(row$pitch_wOBA[1])) {
      w_sum <- w_sum + wt * row$pitch_wOBA[1]; w_matched <- w_matched + wt
    }
  }
  round(w_sum + (1 - w_matched) * fallback, 3)
}

apply_hitter_specific_adjustments <- function(
  preview_df, hitter_assumption_map, game_date, memory_dir
) {
  reliabilities <- hitter_assumption_map |>
    distinct(assumption_name) |>
    mutate(reliability_weight = map_dbl(
      assumption_name,
      ~get_assumption_reliability(memory_dir, .x, current_date = game_date)
    ))

  adj_map <- hitter_assumption_map |>
    left_join(reliabilities, by = "assumption_name") |>
    mutate(
      failure_weight    = 1 - reliability_weight,
      directional_shift = case_when(
        impact_direction == "down" ~
          -default_shift * sensitivity_weight * failure_weight * importance_weight,
        impact_direction == "up"   ~
          default_shift  * sensitivity_weight * failure_weight * importance_weight,
        TRUE ~ 0
      ),
      ci_widen_comp = case_when(
        impact_direction == "widen" ~
          1 + ((ci_widen_factor - 1) * sensitivity_weight * failure_weight),
        TRUE ~ 1
      )
    )

  hitter_adjustments <- adj_map |>
    group_by(player_name) |>
    summarise(
      total_directional_shift = sum(directional_shift, na.rm = TRUE),
      total_ci_widen_mult     = pmin(prod(ci_widen_comp, na.rm = TRUE), 2.0),
      fragility_score         = sum(sensitivity_weight * importance_weight, na.rm = TRUE),
      .groups = "drop"
    )

  preview_df |>
    left_join(hitter_adjustments, by = "player_name") |>
    mutate(
      total_directional_shift = replace_na(total_directional_shift, 0),
      total_ci_widen_mult     = replace_na(total_ci_widen_mult, 1),
      fragility_score         = replace_na(fragility_score, 0),
      exp_wOBA        = pmin(pmax(exp_wOBA_raw + total_directional_shift, 0.15), 0.65),
      prior_delta     = round(exp_wOBA - wOBA_overall, 3),
      alpha_0         = exp_wOBA * K_PRIOR,
      beta_0          = (1 - exp_wOBA) * K_PRIOR,
      CI_lo_90_base   = qbeta(0.05, alpha_0, beta_0),
      CI_hi_90_base   = qbeta(0.95, alpha_0, beta_0),
      ci_halfwidth    = ((CI_hi_90_base - CI_lo_90_base) / 2) * total_ci_widen_mult,
      CI_lo_90        = pmax(0, exp_wOBA - ci_halfwidth),
      CI_hi_90        = pmin(1, exp_wOBA + ci_halfwidth),
      CI_width        = round(CI_hi_90 - CI_lo_90, 3),
      exp_runs_per_pa = calc_exp_runs_per_pa(exp_wOBA)
    )
}

build_preview <- function(
  game_date,
  team, opponent, pitcher_name, pitcher_id, pitcher_hand,
  pitcher_raw_df,                    # full multi-year Statcast file for the pitcher
  batter_pool,
  hitter_history_df,
  assumptions_df,
  pitcher_mix_lhh          = NULL,   # optional manual override
  pitcher_mix_rhh          = NULL,   # optional manual override
  pitcher_archetype        = "unknown_archetype",
  pitch_type_mapping       = NULL,
  hitter_pitch_exposure_df = NULL,
  half_life_days           = 180,
  memory_dir               = "model_memory",
  output_dir               = "outputs/preview"
) {
  init_model_memory(memory_dir)
  safe_dir_create(output_dir)

  weighted <- compute_weighted_pitcher_sample(
    pitcher_raw_df = pitcher_raw_df,
    game_date      = game_date,
    half_life_days = half_life_days
  )

  # Use weighted mix unless caller provides manual overrides
  pitcher_mix_lhh  <- pitcher_mix_lhh  %||% weighted$pitcher_mix_lhh
  pitcher_mix_rhh  <- pitcher_mix_rhh  %||% weighted$pitcher_mix_rhh
  effective_sample <- weighted$effective_sample

  historical_mae   <- get_historical_mae(memory_dir, pitcher_archetype,
                                         current_date = as.Date(game_date))
  confidence_label <- classify_confidence(effective_sample, historical_mae)

  hf <- file.path(memory_dir, "model_memory_hitter_audit.csv")
  memory_game_count <- 0L
  if (file.exists(hf)) {
    m <- safe_read_csv(hf)
    if ("pitcher_archetype" %in% names(m))
      memory_game_count <- m |>
        filter(pitcher_archetype == !!pitcher_archetype) |>
        distinct(game_date) |> nrow()
  }

  pa_hist <- hitter_history_df |>
    filter(events %in% PA_EVENTS) |>
    mutate(woba_val = coalesce(WOBA_WEIGHTS[events], 0),
           woba_den = as.integer(events != "hit_by_pitch"))

  overall_prior <- pa_hist |>
    group_by(player_name) |>
    summarise(PA_hist = n(), op_wn = sum(woba_val, na.rm = TRUE),
              op_wd = sum(woba_den, na.rm = TRUE),
              wOBA_overall = op_wn / pmax(op_wd, 1), .groups = "drop")

  pitch_splits <- pa_hist |>
    group_by(player_name, pitch_name) |>
    summarise(PA_vs = n(), sp_wn = sum(woba_val, na.rm = TRUE),
              sp_wd = sum(woba_den, na.rm = TRUE),
              pitch_wOBA = sp_wn / pmax(sp_wd, 1), .groups = "drop")

  hitter_hand <- pa_hist |>
    group_by(player_name) |>
    summarise(hand = names(sort(table(stand), decreasing = TRUE))[1], .groups = "drop")

  preview_df <- overall_prior |>
    filter(player_name %in% batter_pool) |>
    left_join(hitter_hand, by = "player_name") |>
    mutate(
      hand         = replace_na(hand, "R"),
      exp_wOBA_raw = map2_dbl(player_name, hand, ~compute_arsenal_expectation(
        player = .x, hand_val = .y,
        fallback = overall_prior$wOBA_overall[overall_prior$player_name == .x],
        pitch_splits = pitch_splits,
        pitcher_mix_lhh = pitcher_mix_lhh, pitcher_mix_rhh = pitcher_mix_rhh
      ))
    )

  if (!is.null(pitch_type_mapping) && is.null(hitter_pitch_exposure_df)) {
    hitter_pitch_exposure_df <- compute_hitter_exposure_weights(
      hitter_history_df  = hitter_history_df,
      pitch_type_mapping = pitch_type_mapping
    )
    cat(sprintf("Exposure weights auto-derived: %d hitter-assumption pairs.\n",
                nrow(hitter_pitch_exposure_df)))
  }

  hitter_assumption_map <- build_hitter_assumption_map(
    preview_df               = preview_df,
    assumption_definitions_df = assumptions_df,
    hitter_pitch_exposure_df  = hitter_pitch_exposure_df
  )

  preview_df <- apply_hitter_specific_adjustments(
    preview_df = preview_df, hitter_assumption_map = hitter_assumption_map,
    game_date = game_date, memory_dir = memory_dir
  ) |>
    mutate(
      confidence = confidence_label, game_date = as.Date(game_date),
      team = team, opponent = opponent, pitcher = pitcher_name,
      pitcher_id = pitcher_id, pitcher_hand = pitcher_hand,
      pitcher_archetype = pitcher_archetype
    ) |>
    arrange(desc(exp_wOBA))

  lineup_summary <- preview_df |>
    slice_head(n = min(9, n())) |>
    summarise(
      mean_exp_wOBA     = mean(exp_wOBA, na.rm = TRUE),
      lineup_edge_vs_lg = mean_exp_wOBA - LEAGUE_WOBA,
      exp_runs_top9     = sum(exp_runs_per_pa, na.rm = TRUE)
    )

  preview_assumptions <- assumptions_df |>
    mutate(
      game_date = as.Date(game_date), team = team, opponent = opponent,
      pitcher = pitcher_name, pitcher_id = pitcher_id,
      pitcher_archetype = pitcher_archetype,
      reliability_weight = map_dbl(
        assumption_name,
        ~get_assumption_reliability(memory_dir, .x, current_date = game_date)
      )
    ) |>
    select(game_date, team, opponent, pitcher, pitcher_id, pitcher_archetype,
           assumption_name, expected_value, tolerance, importance_weight,
           reliability_weight, affected_hand, impact_direction, default_shift,
           ci_widen_factor, description)

  preview_meta <- tibble(
    game_date = as.Date(game_date), team = team, opponent = opponent,
    pitcher = pitcher_name, pitcher_id = pitcher_id, pitcher_hand = pitcher_hand,
    pitcher_archetype = pitcher_archetype,
    effective_sample  = effective_sample,
    n_years_in_data   = weighted$n_years,
    half_life_days    = half_life_days,
    confidence = confidence_label, historical_mae = historical_mae,
    memory_game_count = memory_game_count,
    mean_exp_wOBA_top9    = lineup_summary$mean_exp_wOBA,
    lineup_edge_vs_league = lineup_summary$lineup_edge_vs_lg,
    exp_runs_top9         = lineup_summary$exp_runs_top9
  )

  pfx <- paste0(game_date, "_", pitcher_id)
  write_csv(preview_df,            file.path(output_dir, paste0("expectations_",    pfx, ".csv")))
  write_csv(preview_assumptions,   file.path(output_dir, paste0("assumptions_",     pfx, ".csv")))
  write_csv(hitter_assumption_map, file.path(output_dir, paste0("hitter_map_",      pfx, ".csv")))
  write_csv(preview_meta,          file.path(output_dir, paste0("meta_",            pfx, ".csv")))
  write_csv(weighted$raw_by_year,  file.path(output_dir, paste0("sample_weights_",  pfx, ".csv")))

  cat("# =============================================================================\n")
  cat(sprintf("# Preview · %s @ %s · %s\n", team, opponent, pitcher_name))
  cat("# =============================================================================\n")
  cat(sprintf("Effective sample (recency-weighted): %d pitches\n", effective_sample))
  cat(sprintf("  half_life = %d days · %d seasons in data\n", half_life_days, weighted$n_years))
  print(weighted$raw_by_year)
  cat(sprintf("Confidence:           %s\n", confidence_label))
  cat(sprintf("Historical MAE:       %s\n",
              ifelse(is.na(historical_mae), "NA (no memory)", round(historical_mae, 3))))
  cat(sprintf("Memory games:         %d (archetype: %s)\n", memory_game_count, pitcher_archetype))
  cat(sprintf("Mean top-9 exp_wOBA:  %.3f\n", lineup_summary$mean_exp_wOBA))
  cat(sprintf("Lineup edge:          %+.3f vs league\n", lineup_summary$lineup_edge_vs_lg))
  cat("# Top 5\n")
  preview_df |> slice_head(n = 5) |>
    select(player_name, hand, exp_wOBA, CI_lo_90, CI_hi_90, CI_width,
           prior_delta, fragility_score) |>
    mutate(across(where(is.numeric), \(x) round(x, 3))) |>
    print(n = 5)
  cat("# =============================================================================\n")

  list(
    preview_df = preview_df, preview_assumptions = preview_assumptions,
    hitter_assumption_map = hitter_assumption_map, preview_meta = preview_meta,
    weighted_sample = weighted, pitch_type_mapping = pitch_type_mapping
  )
}

# =============================================================================
# I. COMPUTED EXECUTION FLAGS
# =============================================================================

compute_execution_flags <- function(
  pitch_by_pitch_game_df, pitcher_baseline_df = NULL,
  ff_pitch_names = c("4-Seam Fastball", "Fastball")
) {
  game  <- pitch_by_pitch_game_df
  ff_df <- game |> filter(pitch_name %in% ff_pitch_names)

  zone_rate  <- mean(ff_df$zone %in% 1:9, na.rm = TRUE)
  ball_rate  <- mean(game$description %in% c("ball","blocked_ball","pitchout"), na.rm = TRUE)
  bb_rate_pa <- mean((game |> filter(events %in% PA_EVENTS))$events == "walk", na.rm = TRUE)
  release_dev <- NA_real_; velo_delta <- NA_real_

  if (!is.null(pitcher_baseline_df) && nrow(pitcher_baseline_df) > 0) {
    req <- c("release_pos_x","release_pos_z","release_speed","pitch_name")
    if (all(req %in% names(pitcher_baseline_df))) {
      bf <- pitcher_baseline_df |> filter(pitch_name %in% ff_pitch_names)
      if (nrow(bf) > 0 && nrow(ff_df) > 0) {
        release_dev <- sqrt(
          (mean(ff_df$release_pos_x, na.rm=TRUE) - mean(bf$release_pos_x, na.rm=TRUE))^2 +
          (mean(ff_df$release_pos_z, na.rm=TRUE) - mean(bf$release_pos_z, na.rm=TRUE))^2
        )
        velo_delta <- mean(ff_df$release_speed, na.rm=TRUE) -
          mean(bf$release_speed, na.rm=TRUE)
      }
    }
  }

  tibble(
    flag_name    = c("fastball_zone_rate_low","ball_rate_high","walk_rate_high",
                     "release_point_drift","velo_drop"),
    metric_value = c(zone_rate, ball_rate, bb_rate_pa, release_dev, velo_delta),
    threshold    = c(0.45, 0.40, 0.12, 0.15, -1.0),
    flag_value   = c(
      !is.na(zone_rate)   & zone_rate   < 0.45,
      !is.na(ball_rate)   & ball_rate   > 0.40,
      !is.na(bb_rate_pa)  & bb_rate_pa  > 0.12,
      !is.na(release_dev) & release_dev > 0.15,
      !is.na(velo_delta)  & velo_delta  < -1.0
    )
  )
}

# =============================================================================
# J. RETRO
# =============================================================================
# miss_type priority:
#   1. within expected variance  — abs(residual) <= 0.050
#   2. insufficient sample       — PA < 2
#   3. no expectation            — T3 or unmodeled
#   4. assumption miss           — hitter exposed to a failed/partial assumption
#   5. execution miss            — no assumption failed; execution flag fired
#   6. variance miss             — residual > threshold, no structural explanation

build_retro <- function(
  game_date, team, opponent, pitcher_name, pitcher_id, pitcher_archetype,
  pitch_by_pitch_game_df,
  preview_expectations_df, preview_assumptions_df, hitter_assumption_map_df,
  actual_assumption_values_df = NULL,
  pitch_type_mapping          = NULL,
  pitcher_baseline_df         = NULL,
  memory_dir = "model_memory",
  output_dir = "outputs/retro"
) {
  init_model_memory(memory_dir)
  safe_dir_create(output_dir)

  if (is.null(actual_assumption_values_df) && !is.null(pitch_type_mapping)) {
    actual_assumption_values_df <- compute_assumption_actuals_from_pitchfile(
      pitch_by_pitch_game_df = pitch_by_pitch_game_df,
      pitch_type_mapping     = pitch_type_mapping
    )
    cat(sprintf("Actuals auto-derived for: %s\n",
                paste(filter(actual_assumption_values_df, derived)$assumption_name, collapse=", ")))
  }

  game_pa <- pitch_by_pitch_game_df |>
    filter(events %in% PA_EVENTS) |>
    mutate(woba_val = coalesce(WOBA_WEIGHTS[events], 0),
           woba_den = as.integer(events != "hit_by_pitch"))

  hitter_audit <- game_pa |>
    group_by(player_name) |>
    summarise(PA = n(), H = sum(events %in% c("single","double","triple","home_run")),
              BB = sum(events == "walk"), K = sum(events == "strikeout"),
              woba_num = sum(woba_val, na.rm=TRUE), woba_den = sum(woba_den, na.rm=TRUE),
              outcomes = paste(events, collapse=", "), .groups="drop") |>
    mutate(actual_wOBA = woba_num / pmax(woba_den, 1)) |>
    left_join(preview_expectations_df |>
                select(player_name, exp_wOBA, confidence, hand, fragility_score),
              by = "player_name") |>
    mutate(
      residual   = round(actual_wOBA - exp_wOBA, 3),
      abs_error  = abs(residual),
      audit_flag = case_when(
        is.na(exp_wOBA)        ~ "no expectation",
        PA < 2                 ~ "insufficient sample",
        abs(residual) <= 0.050 ~ "within expected variance",
        residual > 0.050       ~ "model underestimated damage",
        residual < -0.050      ~ "model overestimated damage"
      )
    )

  assumption_audit <- preview_assumptions_df |>
    left_join(actual_assumption_values_df |> select(assumption_name, actual_value),
              by = "assumption_name") |>
    mutate(
      audit_result     = pmap_chr(list(expected_value, actual_value, tolerance),
                                  ~label_assumption_result(..1, ..2, ..3)),
      assumption_error = actual_value - expected_value
    )

  execution_flags <- compute_execution_flags(pitch_by_pitch_game_df, pitcher_baseline_df)
  execution_issue <- any(execution_flags$flag_value, na.rm = TRUE)

  hitter_failed <- hitter_assumption_map_df |>
    left_join(assumption_audit |> select(assumption_name, audit_result), by = "assumption_name") |>
    filter(audit_result %in% c("failed","partial"), sensitivity_weight > 0) |>
    group_by(player_name) |>
    summarise(affected_assumption_count = n(),
              affected_assumptions = paste(unique(assumption_name), collapse=", "),
              .groups = "drop")

  hitter_audit <- hitter_audit |>
    left_join(hitter_failed, by = "player_name") |>
    mutate(
      affected_assumption_count = replace_na(affected_assumption_count, 0),
      affected_assumptions      = replace_na(affected_assumptions, ""),
      execution_issue_any       = execution_issue,
      miss_type = case_when(
        audit_flag == "within expected variance" ~ "within expected variance",
        audit_flag == "insufficient sample"      ~ "insufficient sample",
        audit_flag == "no expectation"           ~ "no expectation",
        affected_assumption_count > 0            ~ "assumption miss",
        execution_issue_any                      ~ "execution miss",
        TRUE                                     ~ "variance miss"
      ),
      game_date = as.Date(game_date), team = team, opponent = opponent,
      pitcher = pitcher_name, pitcher_id = pitcher_id,
      pitcher_archetype = pitcher_archetype, hitter = player_name
    ) |>
    select(game_date, team, opponent, pitcher, pitcher_id, pitcher_archetype,
           hitter, hand, PA, exp_wOBA, actual_wOBA, residual, abs_error,
           fragility_score, audit_flag, miss_type, affected_assumptions, outcomes)

  model_error <- hitter_audit |>
    filter(!is.na(exp_wOBA), PA >= 2) |>
    summarise(usable_hitters = n(),
              mae  = mean(abs_error, na.rm=TRUE), bias = mean(residual, na.rm=TRUE),
              rmse = sqrt(mean(residual^2, na.rm=TRUE)),
              assumption_miss_rate = mean(miss_type=="assumption miss", na.rm=TRUE),
              execution_miss_rate  = mean(miss_type=="execution miss",  na.rm=TRUE),
              variance_miss_rate   = mean(miss_type=="variance miss",   na.rm=TRUE))

  lineup_audit <- hitter_audit |>
    filter(!is.na(exp_wOBA), PA >= 2) |>
    summarise(mean_exp_wOBA = mean(exp_wOBA, na.rm=TRUE),
              mean_actual_wOBA = mean(actual_wOBA, na.rm=TRUE),
              lineup_residual  = mean_actual_wOBA - mean_exp_wOBA)

  learned <- assumption_audit |>
    mutate(lesson = case_when(
      audit_result == "failed"  ~
        paste0(assumption_name, " failed. Reduce reliability. Correct direction for exposed hitters next start."),
      audit_result == "partial" ~
        paste0(assumption_name, " partial. Widen CI for exposed hitters. Monitor."),
      audit_result == "held"    ~
        paste0(assumption_name, " held. Prior structure unchanged."),
      TRUE ~ paste0(assumption_name, " not scored.")
    )) |>
    select(assumption_name, audit_result, assumption_error, lesson)

  pfx <- paste0(game_date, "_", pitcher_id)
  write_csv(hitter_audit,     file.path(output_dir, paste0("hitter_audit_",    pfx, ".csv")))
  write_csv(assumption_audit, file.path(output_dir, paste0("assumption_audit_",pfx, ".csv")))
  write_csv(execution_flags,  file.path(output_dir, paste0("execution_flags_", pfx, ".csv")))
  bind_cols(model_error, lineup_audit) |>
    write_csv(file.path(output_dir, paste0("summary_", pfx, ".csv")))

  append_model_memory(
    hitter_audit |> select(game_date, team, opponent, pitcher, pitcher_id,
                            pitcher_archetype, hitter, PA, exp_wOBA, actual_wOBA,
                            residual, abs_error, audit_flag, miss_type),
    file.path(memory_dir, "model_memory_hitter_audit.csv")
  )
  append_model_memory(
    assumption_audit |>
      mutate(game_date = as.Date(game_date), team = team, opponent = opponent,
             pitcher = pitcher_name, pitcher_id = pitcher_id,
             pitcher_archetype = pitcher_archetype) |>
      select(game_date, team, opponent, pitcher, pitcher_id, pitcher_archetype,
             assumption_name, expected_value, actual_value, tolerance,
             audit_result, assumption_error),
    file.path(memory_dir, "model_memory_assumption_audit.csv")
  )

  cat("# =============================================================================\n")
  cat(sprintf("# Retro · %s @ %s · %s\n", team, opponent, pitcher_name))
  cat("# =============================================================================\n")
  cat(sprintf("MAE: %.3f  Bias: %.3f  RMSE: %.3f  Lineup residual: %+.3f\n",
              model_error$mae, model_error$bias, model_error$rmse, lineup_audit$lineup_residual))
  cat(sprintf("Miss: assumption %.0f%%  execution %.0f%%  variance %.0f%%\n",
              model_error$assumption_miss_rate*100, model_error$execution_miss_rate*100,
              model_error$variance_miss_rate*100))
  cat("# Learned\n"); print(learned, n = Inf)
  cat("# =============================================================================\n")

  list(hitter_audit = hitter_audit, assumption_audit = assumption_audit,
       execution_flags = execution_flags, model_error = model_error,
       lineup_audit = lineup_audit, learned = learned)
}

# =============================================================================
# K. EXAMPLE RUN BLOCKS — DAVIS MARTIN, APRIL 5 2026
# =============================================================================

# martin_raw  <- safe_read_csv("savant_data__19_.csv")   # all years, all pitches
# jays_raw    <- safe_read_csv("blue-jays-hitters-25-26.csv")
# sanchez_raw <- safe_read_csv("sanchez-24-26.csv")
#
# hitter_history_df <- bind_rows(
#   jays_raw    |> filter(game_year == 2025),
#   sanchez_raw |> filter(game_year == 2025)
# )
#
# ptm <- build_pitch_type_mapping()
#
# assumptions_df <- tibble(
#   assumption_name   = c("cutter_emergence","changeup_to_lhh_pct","fastball_command_risk"),
#   expected_value    = c(0.059, 0.323, NA_real_),
#   tolerance         = c(0.08,  0.06,  NA_real_),
#   importance_weight = c(1.00,  0.80,  0.90),
#   affected_hand     = c("all", "L",   "all"),
#   impact_direction  = c("widen","down","widen"),
#   default_shift     = c(0.000, 0.010, 0.000),
#   ci_widen_factor   = c(1.30,  1.15,  1.25),
#   description       = c(
#     "Cutter usage holds near weighted baseline (~6%); 2026 showed 20.7% in one start",
#     "Changeup remains primary vs LHH near 32% (weighted career baseline)",
#     "Fastball command stable; 2026 velo is 92.8, slightly below career 93.9"
#   )
# )
#
# martin_preview <- build_preview(
#   game_date         = "2026-04-05",
#   team              = "TOR", opponent = "CWS",
#   pitcher_name      = "Davis Martin", pitcher_id = 663436L, pitcher_hand = "R",
#   pitcher_raw_df    = martin_raw,     # full multi-year file
#   batter_pool = c(
#     "Barger, Addison","Clement, Ernie","Giménez, Andrés",
#     "Guerrero Jr., Vladimir","Heineman, Tyler","Kirk, Alejandro",
#     "Lukes, Nathan","Okamoto, Kazuma","Schneider, Davis",
#     "Springer, George","Straw, Myles","Sánchez, Jesús","Varsho, Daulton"
#   ),
#   hitter_history_df        = hitter_history_df,
#   assumptions_df           = assumptions_df,
#   pitch_type_mapping       = ptm,
#   hitter_pitch_exposure_df = NULL,
#   pitcher_archetype        = "RHP_bulk_cws_2026",
#   half_life_days           = 180,
#   memory_dir               = "model_memory",
#   output_dir               = "outputs/preview"
# )

# =============================================================================
# END
# =============================================================================
