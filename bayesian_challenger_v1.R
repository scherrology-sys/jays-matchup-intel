# =============================================================================
# Jays Matchup Intel
# Bayesian Hierarchical Challenger Model — Version 1
# =============================================================================
#
# PURPOSE
# -------
# This is a silent challenger model running alongside the champion (pitch-mix
# weighted frequentist exp_wOBA). It does not influence production output.
# Promotion criteria: beats champion on game-level MAE, improves ranking
# accuracy, holds across n >= 200, introduces no systematic bias.
#
# WHAT THIS IS
# ------------
# A silent challenger running alongside the frequentist champion.
# The value is in seeing where partial pooling behaves differently from the
# weighted average and building intuition for what the hierarchy is doing.
#
# MODEL STRUCTURE (v1)
# --------------------
# Outcome: observed hitter-game wOBA (continuous, ~Normal)
# Variance: heteroskedastic — tied to PA count (more PA = less noise)
#
# Fixed effects:
#   - pitcher_hand (L/R): structural platoon effect
#   - pitch_family_fastball_pct: % fastball-group pitches to this handedness
#   - pitch_family_breaking_pct: % breaking-group pitches
#   - pitch_family_offspeed_pct: % offspeed-group pitches
#   (reference: undefined group absorbed into intercept)
#
# Random effects (partial pooling):
#   - (1 | hitter): hitter-level intercept, pooled across all hitters
#   - (1 | pitcher): pitcher-level intercept, pooled across all pitchers
#   - (0 + pitch_family_fastball_pct + pitch_family_breaking_pct +
#      pitch_family_offspeed_pct | hitter): hitter × pitch-family interaction
#     Each hitter's deviation from the population slope for each pitch family
#     is partially pooled. A hitter with little data gets shrunk toward the
#     population average response to that pitch family.
#
# PITCH FAMILY GROUPINGS
# ----------------------
# Grouping is a partial-pooling scaffold chosen for stability of inference,
# not because all pitches within a family are assumed identical.
#
#   Fastball:  4-Seam Fastball, Sinker, Cutter
#              Note: Cutter is the shakiest member — behaves like a short
#              slider in practice. Kept here for v1 parsimony. Flag as
#              sensitivity-check candidate.
#
#   Breaking:  Slider, Curveball, Sweeper
#              Sweeper/Slider label instability handled by collapsing.
#
#   Offspeed:  Changeup, Splitter (Split-Finger)
#              Grouped because both occupy similar role in hitter timing
#              decision — velocity separation + fade/drop disruption.
#
#   Undefined: Knuckleball, Eephus, unknown, misc labels
#              Sink category. Not interpreted.
#
# LIKELIHOOD NOTE
# ---------------
# Normal on observed hitter-game wOBA is a convenience approximation.
# Acceptable for v1 challenger testing. See V2 POTENTIAL DIRECTION below.
#
# NOTATION
# --------
# wOBA_ig ~ Normal(mu_ig, sigma_ig)
# sigma_ig = sigma_base / sqrt(PA_ig)   [heteroskedastic: more PA = tighter]
# mu_ig = alpha + u_hitter[i] + u_pitcher[g]
#         + (beta_fast + v_fast[i]) * fastball_pct_g
#         + (beta_break + v_break[i]) * breaking_pct_g
#         + (beta_off + v_off[i]) * offspeed_pct_g
#         + gamma * pitcher_hand_g
#
# where u_hitter, u_pitcher are hitter/pitcher random intercepts
# and v_fast, v_break, v_off are hitter-level random slopes on pitch families
#
# =============================================================================

library(tidyverse)
library(brms)
library(tidybayes)

# =============================================================================
# 1. PITCH FAMILY MAPPING
# =============================================================================

PITCH_FAMILIES <- list(
  Fastball  = c("4-Seam Fastball", "Sinker", "Cutter"),
  Breaking  = c("Slider", "Curveball", "Sweeper"),
  Offspeed  = c("Changeup", "Splitter", "Split-Finger"),
  Undefined = c("Knuckleball", "Eephus", "Other", "Unknown", "")
)

assign_pitch_family <- function(pitch_name) {
  case_when(
    pitch_name %in% PITCH_FAMILIES$Fastball  ~ "Fastball",
    pitch_name %in% PITCH_FAMILIES$Breaking  ~ "Breaking",
    pitch_name %in% PITCH_FAMILIES$Offspeed  ~ "Offspeed",
    TRUE                                      ~ "Undefined"
  )
}

# =============================================================================
# 2. BUILD TRAINING DATA
# =============================================================================
#
# For each game in the backtest sample:
#   - Compute observed hitter-game wOBA (outcome)
#   - Compute pitcher pitch-family mix vs that hitter's handedness (features)
#   - Retain PA count for variance weighting
#   - Retain raw pitch labels in data (for future sensitivity checks)
#
# Input: same game files used by backtest_three_models.R
# =============================================================================

build_bayes_training_data <- function(
  game_list,          # list of (label, pitcher_df, game_df, game_date,
                      #          hitter_priors, pitcher_id, park, pitcher_hand)
  half_life_days = HL
) {
  map_dfr(game_list, function(g) {
    label        <- g$label
    pitcher_df   <- g$pitcher_df
    game_df      <- g$game_df
    game_date    <- g$game_date
    pid          <- g$pitcher_id
    park         <- g$park
    pitcher_hand <- g$pitcher_hand
    priors       <- g$priors

    if (!is.null(pid)) {
      pitcher_df <- pitcher_df |> filter(pitcher == pid)
      game_df    <- game_df    |> filter(pitcher == pid)
    }

    ref <- as.Date(game_date)
    hd  <- priors$hand

    # Pitcher pre-game pitch mix with recency weighting
    pitcher_pre <- pitcher_df |>
      filter(as.Date(game_date) < ref,
             !pitch_name %in% c("Unknown", "")) |>
      mutate(
        age_days     = as.numeric(ref - as.Date(game_date)),
        rw           = 0.5^(age_days / half_life_days),
        pitch_family = assign_pitch_family(pitch_name)
      )

    # Game PA outcomes
    pa <- game_df |>
      filter(events %in% PA_EVENTS) |>
      mutate(
        woba_val = coalesce(WOBA_WEIGHTS[events], 0),
        woba_den = as.integer(events != "hit_by_pitch")
      )

    acts <- pa |>
      group_by(player_name) |>
      summarise(
        PA     = n(),
        wn     = sum(woba_val, na.rm=TRUE),
        wd     = sum(woba_den, na.rm=TRUE),
        .groups = "drop"
      ) |>
      mutate(observed_woba = round(wn / pmax(wd, 1), 3)) |>
      filter(PA >= 2)

    # Compute pitch family mix by hitter handedness
    map_dfr(seq_len(nrow(acts)), function(i) {
      h    <- acts$player_name[i]
      hh   <- hd |> filter(player_name == h)
      if (nrow(hh) == 0) return(NULL)
      hand <- hh$hand[1]

      mix_raw <- pitcher_pre |>
        filter(stand == hand) |>
        group_by(pitch_family) |>
        summarise(rw_sum = sum(rw), .groups="drop") |>
        mutate(pct = rw_sum / sum(rw_sum))

      get_pct <- function(fam) {
        r <- mix_raw |> filter(pitch_family == fam)
        if (nrow(r) == 0) 0 else r$pct[1]
      }

      eff <- pitcher_pre |>
        filter(stand == hand) |>
        pull(rw) |>
        sum() |>
        as.integer()

      tibble(
        game              = label,
        game_date         = as.Date(game_date),
        hitter            = h,
        pitcher           = label,   # game-level pitcher ID for random effect
        hand              = hand,
        pitcher_hand      = pitcher_hand,
        PA                = acts$PA[i],
        observed_woba     = acts$observed_woba[i],
        fastball_pct      = round(get_pct("Fastball"),  3),
        breaking_pct      = round(get_pct("Breaking"),  3),
        offspeed_pct      = round(get_pct("Offspeed"),  3),
        undefined_pct     = round(get_pct("Undefined"), 3),
        eff_sample        = eff,
        confidence        = case_when(
          eff >= 700 ~ "HIGH",
          eff >= 200 ~ "MODERATE",
          TRUE       ~ "LOW"
        ),
        park              = park,
        # Retain raw pitch type distribution as JSON-like string for future
        # sensitivity checks (Cutter split, Splitter split, etc.)
        raw_mix_json      = {
          pitcher_pre |>
            filter(stand == hand) |>
            group_by(pitch_name) |>
            summarise(rw=sum(rw),.groups="drop") |>
            mutate(pct=round(rw/sum(rw)*100,1)) |>
            arrange(desc(pct)) |>
            summarise(s=paste(pitch_name,":",pct,"%",collapse=" | ")) |>
            pull(s)
        }
      )
    })
  })
}

# =============================================================================
# 3. FIT THE BAYESIAN HIERARCHICAL MODEL (brms v1)
# =============================================================================
#
# Model formula:
#   observed_woba | se(obs_se) ~
#     pitcher_hand +
#     fastball_pct + breaking_pct + offspeed_pct +
#     (1 | hitter) +
#     (1 | pitcher) +
#     (0 + fastball_pct + breaking_pct + offspeed_pct | hitter)
#
# Heteroskedastic variance: obs_se = sqrt(woba_var / PA)
# where woba_var ≈ 0.09 (approximate wOBA variance for a single PA)
#
# We use se() to pass the per-observation standard error directly to brms.
# This is cleaner than modeling sigma as a function of PA because brms
# handles the measurement-error structure correctly.
#
# Priors:
#   Intercept:     Normal(0.320, 0.05)  — centered at league average wOBA
#   pitcher_hand:  Normal(0, 0.03)      — small platoon effect
#   pitch family:  Normal(0, 0.05)      — moderate pitch-family effect
#   Random effect SD: HalfNormal(0, 0.05) — conservative pooling
#
# These are weakly informative. They encode:
#   - outcomes should be in the wOBA range (roughly 0.20 to 0.45)
#   - effects should be baseball-plausible, not unbounded
#   - partial pooling is meaningful: SDs should be non-trivial but not huge
# =============================================================================

fit_bayesian_challenger_v1 <- function(
  training_data,
  chains   = 4,
  iter     = 2000,
  warmup   = 1000,
  seed     = 2026,
  cores    = parallel::detectCores() - 1
) {
  # Compute per-observation SE from PA
  # wOBA variance per PA ≈ 0.09 (conservative estimate)
  WOBA_VAR_PER_PA <- 0.090

  model_data <- training_data |>
    mutate(
      obs_se        = sqrt(WOBA_VAR_PER_PA / PA),
      pitcher_hand_L = as.integer(pitcher_hand == "L")
    )

  # Formula: measurement error model via se()
  # Random slopes on pitch families allow each hitter to respond differently
  bf_formula <- bf(
    observed_woba | se(obs_se, sigma = TRUE) ~
      pitcher_hand_L +
      fastball_pct + breaking_pct + offspeed_pct +
      (1 | hitter) +
      (1 | pitcher) +
      (0 + fastball_pct + breaking_pct + offspeed_pct | hitter)
  )

  # Weakly informative priors
  model_priors <- c(
    prior(normal(0.320, 0.050), class = Intercept),
    prior(normal(0,     0.030), class = b, coef = pitcher_hand_L),
    prior(normal(0,     0.050), class = b, coef = fastball_pct),
    prior(normal(0,     0.050), class = b, coef = breaking_pct),
    prior(normal(0,     0.050), class = b, coef = offspeed_pct),
    prior(half_normal(0, 0.050), class = sd),
    prior(half_normal(0, 0.030), class = sigma)
  )

  brm(
    formula  = bf_formula,
    data     = model_data,
    family   = gaussian(),
    prior    = model_priors,
    chains   = chains,
    iter     = iter,
    warmup   = warmup,
    seed     = seed,
    cores    = cores,
    control  = list(adapt_delta = 0.95, max_treedepth = 12),
    file     = "outputs/bayes/challenger_v1"   # cache the fit
  )
}

# =============================================================================
# 4. GENERATE POSTERIOR PREDICTIONS FOR NEW GAME
# =============================================================================
#
# For a new game (preview context):
#   1. Compute the pitch-family mix for tonight's pitcher
#   2. Build a prediction data frame for each hitter
#   3. Extract posterior predictive distribution
#   4. Summarise: posterior mean, 90% credible interval, tier
#
# The posterior mean replaces the exp_wOBA point estimate.
# The credible interval is now a genuine posterior interval, not a
# Beta CI placed around a frequentist estimate.
# =============================================================================

predict_bayesian <- function(
  fitted_model,
  new_game_data,   # output of build_bayes_training_data for tonight
  ci_prob = 0.90
) {
  alpha <- (1 - ci_prob) / 2

  # Add obs_se for new observations (use median PA as baseline)
  WOBA_VAR_PER_PA <- 0.090
  pred_data <- new_game_data |>
    mutate(
      obs_se        = sqrt(WOBA_VAR_PER_PA / pmax(PA, 2)),
      pitcher_hand_L = as.integer(pitcher_hand == "L")
    )

  # Posterior predictive distribution
  preds <- posterior_epred(
    fitted_model,
    newdata   = pred_data,
    allow_new_levels = TRUE   # new hitters/pitchers get shrunk to population
  )

  # Summarise per hitter
  pred_data |>
    mutate(
      posterior_mean = round(colMeans(preds), 3),
      ci_lo          = round(apply(preds, 2, quantile, alpha),     3),
      ci_hi          = round(apply(preds, 2, quantile, 1 - alpha), 3),
      ci_width       = round(ci_hi - ci_lo, 3)
    ) |>
    select(game, hitter, hand, PA, pitcher_hand, confidence,
           fastball_pct, breaking_pct, offspeed_pct,
           posterior_mean, ci_lo, ci_hi, ci_width,
           eff_sample, raw_mix_json)
}

# =============================================================================
# 5. COMPARE CHAMPION VS CHALLENGER
# =============================================================================
#
# Score both models on the same hitter-game observations.
# Primary metric: game-level MAE (addresses independence assumption).
# Secondary: Spearman rank correlation within each game.
#
# The challenger earns promotion when:
#   - game-level MAE is lower than champion across >= 25 games
#   - Spearman rho is meaningfully positive
#   - no systematic bias introduced
#   - holds out-of-sample (first half to fit, second half to evaluate)
# =============================================================================

compare_champion_challenger <- function(
  champion_results,    # data frame with exp (champion), actual, game, hitter
  challenger_results   # data frame with posterior_mean, actual, game, hitter
) {
  combined <- champion_results |>
    select(game, hitter, exp_champion = exp, actual) |>
    left_join(
      challenger_results |> select(game, hitter, exp_challenger = posterior_mean),
      by = c("game","hitter")
    ) |>
    filter(!is.na(exp_challenger))

  cat("=== CHAMPION vs BAYESIAN CHALLENGER ===\n\n")

  # Hitter-level (diagnostic only — independence assumption applies)
  mae_champ <- mean(abs(combined$exp_champion - combined$actual), na.rm=TRUE)
  mae_chal  <- mean(abs(combined$exp_challenger - combined$actual), na.rm=TRUE)
  cat(sprintf("Hitter-level MAE  Champion: %.3f  Challenger: %.3f\n",
              mae_champ, mae_chal))

  # Game-level (primary metric)
  game_comp <- combined |>
    group_by(game) |>
    summarise(
      team_exp_champ = mean(exp_champion,  na.rm=TRUE),
      team_exp_chal  = mean(exp_challenger, na.rm=TRUE),
      team_actual    = mean(actual,          na.rm=TRUE),
      mae_champ      = abs(team_exp_champ - team_actual),
      mae_chal       = abs(team_exp_chal  - team_actual),
      .groups = "drop"
    )

  cat(sprintf("\nGame-level MAE    Champion: %.3f  Challenger: %.3f\n",
              mean(game_comp$mae_champ),
              mean(game_comp$mae_chal)))
  cat(sprintf("Champion wins: %d/%d games\n",
              sum(game_comp$mae_champ < game_comp$mae_chal),
              nrow(game_comp)))
  cat(sprintf("Challenger wins: %d/%d games\n\n",
              sum(game_comp$mae_chal < game_comp$mae_champ),
              nrow(game_comp)))

  # Ranking accuracy
  rank_comp <- combined |>
    group_by(game) |>
    summarise(
      spearman_champ = cor(exp_champion,  actual, method="spearman", use="complete.obs"),
      spearman_chal  = cor(exp_challenger, actual, method="spearman", use="complete.obs"),
      .groups = "drop"
    )

  cat(sprintf("Mean Spearman rho  Champion: %.3f  Challenger: %.3f\n",
              mean(rank_comp$spearman_champ, na.rm=TRUE),
              mean(rank_comp$spearman_chal,  na.rm=TRUE)))

  # Bias
  cat(sprintf("\nBias  Champion: %+.3f  Challenger: %+.3f\n",
              mean(combined$exp_champion  - combined$actual, na.rm=TRUE),
              mean(combined$exp_challenger - combined$actual, na.rm=TRUE)))

  invisible(list(combined=combined, game_comp=game_comp, rank_comp=rank_comp))
}

# =============================================================================
# 6. POSTERIOR PREDICTIVE CHECKS
# =============================================================================
#
# After fitting, check that the model is generating plausible data.
# Key checks:
#   - pp_check: does the posterior predictive distribution look like observed?
#   - check that wOBA values stay in plausible range (0.10 - 0.65)
#   - check for divergences and Rhat (should be < 1.01)
# =============================================================================

run_posterior_checks <- function(fitted_model) {
  cat("=== POSTERIOR PREDICTIVE CHECKS ===\n\n")

  # MCMC diagnostics
  diag <- summary(fitted_model)
  max_rhat <- max(diag$fixed$Rhat, na.rm=TRUE)
  cat(sprintf("Max Rhat: %.3f  (target < 1.01)\n", max_rhat))
  if (max_rhat > 1.01) {
    cat("WARNING: Rhat > 1.01 — chains have not converged. More iterations needed.\n")
  }

  n_divergent <- sum(nuts_params(fitted_model)$Value[
    nuts_params(fitted_model)$Parameter == "divergent__"])
  cat(sprintf("Divergent transitions: %d  (target: 0)\n", n_divergent))
  if (n_divergent > 0) {
    cat("WARNING: divergences present. Consider increasing adapt_delta.\n")
  }

  # Visual check
  pp_check(fitted_model, ndraws=50)
}

# =============================================================================
# 7. APPLY TO TONIGHT'S GAME (CHALLENGER PREDICTION)
# =============================================================================
#
# Call this after fitting to generate challenger predictions for any game.
# Returns a tibble that can be logged alongside the champion prediction.
# Does NOT feed into the HTML preview. Logged only.
# =============================================================================

log_challenger_prediction <- function(
  fitted_model,
  tonight_data,
  game_date,
  pitcher_name,
  output_dir = "outputs/bayes/predictions"
) {
  safe_dir_create(output_dir)

  preds <- predict_bayesian(fitted_model, tonight_data)

  preds <- preds |>
    mutate(
      game_date    = as.Date(game_date),
      pitcher      = pitcher_name,
      model        = "bayesian_challenger_v1",
      logged_at    = Sys.time()
    )

  fname <- sprintf("%s/challenger_%s_%s.csv",
                   output_dir,
                   format(as.Date(game_date), "%Y-%m-%d"),
                   str_to_lower(str_replace_all(pitcher_name, " ", "_")))

  write_csv(preds, fname)
  cat(sprintf("Challenger predictions logged: %s\n", fname))
  cat("NOTE: challenger predictions are silent — not in production output.\n")

  invisible(preds)
}

# =============================================================================
# END
# =============================================================================
#
# STATUS: v1 ready to fit once training data is assembled.
#
# To run:
#   1. Build training data: build_bayes_training_data(game_list)
#   2. Fit model: fit_bayesian_challenger_v1(training_data)
#   3. Run checks: run_posterior_checks(fitted_model)
#   4. Compare: compare_champion_challenger(champion_results, challenger_results)
#   5. Log tonight: log_challenger_prediction(fitted_model, tonight_data, ...)
#
# WHAT TO WATCH
# -----------------------------------------------------------------------
# The partial pooling is doing its job if:
#   - hitters with few observations get shrunk toward the population mean
#   - hitter × pitch-family slopes are small and vary sensibly across hitters
#   - the posterior for a LOW confidence game is noticeably wider than HIGH
#
# That last one is the key diagnostic. If the Bayesian posterior for Yamamoto
# (3 WS starts) is not materially wider than for Freeland (career file), the
# model is not propagating input uncertainty correctly.
#
# V2 POTENTIAL DIRECTION
# -----------------------------------------------------------------------
# Replace Normal likelihood on game-level wOBA with a PA-level model where
# each plate appearance outcome is modeled directly (multinomial or ordinal)
# and wOBA is derived downstream from posterior predictive simulation rather
# than modeled as the outcome variable directly. Whether this direction is
# worth pursuing depends on whether v1 shows meaningful lift over the champion.
# =============================================================================
