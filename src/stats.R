# ---------------------------------------------------------------
# Statistical engine: descriptive stats + the six inferential analyses
# ---------------------------------------------------------------

# Run-outs and retirements are not credited to the bowler.
NON_BOWLER_WKT <- c("run out", "retired hurt", "retired out", "obstructing the field")
is_bowler_wkt  <- function(w) !is.na(w) & !(w %in% NON_BOWLER_WKT)

# Group overs into Powerplay, Middle, and Death phases.
over_phase <- function(over) {
  factor(ifelse(over <= 6, "Powerplay", ifelse(over <= 15, "Middle", "Death")),
         levels = c("Powerplay", "Middle", "Death"))
}

fmt_p <- function(p) ifelse(is.na(p), "NA", ifelse(p < 0.001, "< 0.001", sprintf("%.3f", p)))

# ---------------- innings-level tables ----------------

# Create one row for each batter in each innings. Other summaries reuse this table.
batter_innings <- function(balls) {
  b <- balls %>%
    group_by(match_id, innings, season, venue, batting_team, bowling_team, player = striker) %>%
    summarise(runs = sum(runs_off_bat), balls = sum(wides == 0),
              fours = sum(runs_off_bat == 4), sixes = sum(runs_off_bat == 6),
              dots = sum(runs_off_bat == 0 & wides == 0), .groups = "drop")
  out <- balls %>%
    filter(!is.na(player_dismissed)) %>%
    distinct(match_id, innings, player = player_dismissed) %>%
    mutate(out = 1L)
  b %>%
    left_join(out, by = c("match_id", "innings", "player")) %>%
    mutate(out = coalesce(out, 0L), strike_rate = 100 * runs / pmax(balls, 1))
}

# Create one row per bowler per innings, including legal balls, runs, wickets, and dots.
bowler_innings <- function(balls) {
  balls %>%
    mutate(legal = wides == 0 & noballs == 0,
           conc  = runs_off_bat + wides + noballs,
           wkt   = is_bowler_wkt(wicket_type)) %>%
    group_by(match_id, innings, season, venue, batting_team, bowling_team, player = bowler) %>%
    summarise(balls = sum(legal), runs = sum(conc), wickets = sum(wkt),
              dots = sum(legal & conc == 0), .groups = "drop")
}

# Calculate each team innings total and attach the match result.
team_innings <- function(balls, matches) {
  balls %>%
    group_by(match_id, innings, season, venue, batting_team) %>%
    summarise(runs = sum(runs_off_bat + extras), wickets = sum(!is.na(player_dismissed)),
              .groups = "drop") %>%
    left_join(select(matches, match_id, winner, toss_winner, toss_decision), by = "match_id") %>%
    filter(!is.na(winner)) %>%
    mutate(won = as.integer(winner == batting_team))
}

# ---------------- summaries ----------------

# Summarise batting records, optionally grouped by player, season, or another field.
batting_summary <- function(bi, by = character(0)) {
  bi %>%
    group_by(across(all_of(by))) %>%
    summarise(innings = n(), hs = max(runs),
              fifties = sum(runs >= 50 & runs < 100), hundreds = sum(runs >= 100),
              outs = sum(out), runs = sum(runs), balls = sum(balls),
              fours = sum(fours), sixes = sum(sixes), dots = sum(dots), .groups = "drop") %>%
    mutate(avg = ifelse(outs > 0, runs / outs, NA_real_),
           sr = 100 * runs / pmax(balls, 1),
           runs_per_inn = runs / innings,
           boundary_pct = 100 * (fours + sixes) / pmax(balls, 1),
           dot_pct = 100 * dots / pmax(balls, 1))
}

# Summarise bowling records. Economy is runs per six legal balls (one over).
bowling_summary <- function(bw, by = character(0)) {
  bw %>%
    group_by(across(all_of(by))) %>%
    summarise(matches = n(), four_w = sum(wickets >= 4), balls = sum(balls),
              runs = sum(runs), wickets = sum(wickets), dots = sum(dots), .groups = "drop") %>%
    mutate(overs = balls %/% 6 + (balls %% 6) / 10,
           avg = ifelse(wickets > 0, runs / wickets, NA_real_),
           econ = 6 * runs / pmax(balls, 1),
           sr = ifelse(wickets > 0, balls / wickets, NA_real_),
           dot_pct = 100 * dots / pmax(balls, 1),
           wpm = wickets / matches)
}

# Calculate batting measures separately for Powerplay, Middle, and Death overs.
batting_phase_table <- function(b) {
  b %>% filter(wides == 0) %>%
    group_by(phase = over_phase(over)) %>%
    summarise(balls = n(), runs = sum(runs_off_bat),
              sr = 100 * runs / balls,
              boundary_pct = 100 * mean(runs_off_bat %in% c(4, 6)),
              dot_pct = 100 * mean(runs_off_bat == 0),
              outs = sum(!is.na(player_dismissed)), .groups = "drop") %>%
    mutate(avg = ifelse(outs > 0, runs / outs, NA_real_))
}

# Calculate bowling measures separately for the three phases.
bowling_phase_table <- function(b) {
  b %>%
    mutate(legal = wides == 0 & noballs == 0,
           conc = runs_off_bat + wides + noballs, wkt = is_bowler_wkt(wicket_type)) %>%
    group_by(phase = over_phase(over)) %>%
    summarise(balls = sum(legal), runs = sum(conc), wickets = sum(wkt),
              dots = sum(legal & conc == 0), .groups = "drop") %>%
    mutate(econ = 6 * runs / pmax(balls, 1), dot_pct = 100 * dots / pmax(balls, 1),
           sr = ifelse(wickets > 0, balls / wickets, NA_real_))
}

# ================= Q1: powerplay vs death-over strike rate =================
# Feature 1: compare a batter's strike rate in Powerplay and Death overs using a Welch t-test.
phase_test <- function(balls, player, alpha = 0.05, min_balls = 3) {
  d <- balls %>%
    filter(striker == player, wides == 0) %>%
    mutate(phase = over_phase(over)) %>%
    filter(phase %in% c("Powerplay", "Death")) %>%
    group_by(match_id, innings, phase) %>%
    summarise(runs = sum(runs_off_bat), balls = n(), .groups = "drop") %>%
    filter(balls >= min_balls) %>%
    mutate(sr = 100 * runs / balls, phase = droplevels(phase))
  pp <- d$sr[d$phase == "Powerplay"]; dd <- d$sr[d$phase == "Death"]
  if (length(pp) < 3 || length(dd) < 3)
    return(list(ok = FALSE, msg = paste0("Not enough innings in both phases (need >= 3 each; found ",
                                         length(pp), " powerplay / ", length(dd), " death).")))
  list(ok = TRUE, data = d, n_pp = length(pp), n_dd = length(dd),
       mean_pp = mean(pp), mean_dd = mean(dd),
       conf = 1 - alpha,
       t = t.test(dd, pp, conf.level = 1 - alpha))   # Welch two-sample t-test
}

# ================= Q2: does batting first matter? =================
# Feature 2: estimate how often the team batting first wins and compare results with toss winners.
bat_first_test <- function(ti, matches, alpha = 0.05) {
  d <- ti %>% filter(innings == 1)               # one row per match: did the side batting first win?
  n <- nrow(d); k <- sum(d$won)
  fit <- glm(won ~ 1, data = d, family = binomial)
  tm <- matches %>% filter(!is.na(winner), !is.na(toss_winner))
  kt <- sum(tm$winner == tm$toss_winner)
  list(n = n, k = k, rate = k / n, conf = 1 - alpha,
       binom = binom.test(k, n, conf.level = 1 - alpha),
       odds = unname(exp(coef(fit))),
       odds_ci = exp(confint.default(fit, level = 1 - alpha)[1, ]),
       by_season = d %>% group_by(season) %>%
         summarise(matches = n(), wins = sum(won), pct = 100 * mean(won), .groups = "drop"),
       toss = list(n = nrow(tm), k = kt, binom = binom.test(kt, nrow(tm), conf.level = 1 - alpha)))
}

# ================= Q3: consistency =================
# Feature 3: compare batter averages and variation. A smaller coefficient of variation means more consistency.
consistency_table <- function(bi, min_inn = 30, min_mean = 20, conf = 0.95) {
  bi %>%
    group_by(player) %>% filter(n() >= min_inn) %>%
    summarise(innings = n(), mean_runs = mean(runs), sd_runs = sd(runs),
              cv = sd_runs / mean_runs, se = sd_runs / sqrt(innings),
              ci_low = mean_runs - qt(1 - (1 - conf) / 2, innings - 1) * se,
              ci_high = mean_runs + qt(1 - (1 - conf) / 2, innings - 1) * se,
              .groups = "drop") %>%
    filter(mean_runs >= min_mean) %>%
    arrange(cv)
}

# ================= Q4: first-innings score -> win probability =================
# Feature 4: fit logistic regression to relate first-innings score to win/loss.
win_model <- function(ti) {
  d <- ti %>% filter(innings == 1) %>% transmute(match_id, score = runs, bat_first_won = won)
  fit  <- glm(bat_first_won ~ score, data = d, family = binomial)
  null <- glm(bat_first_won ~ 1, data = d, family = binomial)
  list(data = d, fit = fit,
       mcfadden = 1 - as.numeric(logLik(fit)) / as.numeric(logLik(null)),
       accuracy = mean((fitted(fit) > 0.5) == (d$bat_first_won == 1)),
       coefs = {
         s <- summary(fit)$coefficients; ci <- exp(confint.default(fit))
         tibble::tibble(Term = "First-innings score (per run)",
                        Estimate = s[2, 1], `Odds ratio` = exp(s[2, 1]),
                        `OR 2.5%` = ci[2, 1], `OR 97.5%` = ci[2, 2],
                        `p-value` = s[2, 4])
       })
}

# Predict a win probability for a chosen score, with a confidence range.
predict_win <- function(m, score) {
  p <- predict(m$fit, data.frame(score = score), type = "link", se.fit = TRUE)
  data.frame(score = score, p = plogis(p$fit),
             lo = plogis(p$fit - 1.96 * p$se.fit), hi = plogis(p$fit + 1.96 * p$se.fit))
}

# Cohen's h/d magnitude -> verdict text. d > 0 always means "better than the league".
# Convert an effect size and p-value into short, readable interpretation text.
dom_label <- function(d, p, alpha) {
  mag <- as.character(cut(abs(d), breaks = c(-Inf, .2, .5, .8, Inf), right = FALSE,
                          labels = c("negligible", "small", "medium", "large")))
  ifelse(is.na(p) | is.na(d) | p >= alpha, "No significant difference",
    ifelse(d > 0,
           ifelse(mag == "negligible", "Better, but negligible effect", paste0("Dominant (", mag, " effect)")),
           ifelse(mag == "negligible", "Worse, but negligible effect", paste0("Below league (", mag, " effect)"))))
}

# ================= Q5 / Q6: dot balls and boundaries =================
# Batting dot = no run off the bat and not a wide. Bowling dot = legal ball with 0 conceded.

# Build player-level batting counts used by the dot-ball and boundary features.
player_bat_tally <- function(balls) {
  balls %>% group_by(player = striker) %>%
    summarise(balls = sum(wides == 0), runs = sum(runs_off_bat),
              dots = sum(runs_off_bat == 0 & wides == 0),
              fours = sum(runs_off_bat == 4), sixes = sum(runs_off_bat == 6),
              innings = n_distinct(paste(match_id, innings)), .groups = "drop") %>%
    mutate(bdry = fours + sixes, dot_pct = 100 * dots / pmax(balls, 1),
           bdry_pct = 100 * bdry / pmax(balls, 1))
}

# Build player-level bowling counts used by the bowling-related features.
player_bowl_tally <- function(balls) {
  balls %>%
    mutate(legal = wides == 0 & noballs == 0, conc = runs_off_bat + wides + noballs) %>%
    group_by(player = bowler) %>%
    summarise(balls = sum(legal), dots = sum(legal & conc == 0),
              fours = sum(legal & runs_off_bat == 4), sixes = sum(legal & runs_off_bat == 6),
              .groups = "drop") %>%
    mutate(bdry = fours + sixes, dot_pct = 100 * dots / pmax(balls, 1),
           bdry_pct = 100 * bdry / pmax(balls, 1))
}

# One player's proportion (x successes in n balls) vs the rest of the league (totals X of N).
prop_dominance <- function(x, n, X, N, alpha = 0.05, higher_better = TRUE, min_n = 30) {
  if (is.na(n) || n < min_n)
    return(list(ok = FALSE, msg = paste0("Not enough balls for this player (need >= ", min_n, ").")))
  xr <- X - x; nr <- N - n
  pt <- prop.test(c(x, xr), c(n, nr), conf.level = 1 - alpha, correct = FALSE)   # two-proportion z-test
  p1 <- x / n; p2 <- xr / nr
  h <- (2 * asin(sqrt(p1)) - 2 * asin(sqrt(p2))) * if (higher_better) 1 else -1   # Cohen's h; + = better
  list(ok = TRUE, x = x, n = n, p1 = p1, p2 = p2, diff = p1 - p2, ci = as.numeric(pt$conf.int),
       p = pt$p.value, cohen = h, verdict = dom_label(h, pt$p.value, alpha), conf = 1 - alpha)
}

# Every player vs the rest of the league at once (same z-test, vectorised).
prop_board <- function(tot, higher_better = TRUE, min_n = 500, alpha = 0.05, xcol = "dots") {
  X <- sum(tot[[xcol]]); N <- sum(tot$balls)
  tot %>% filter(balls >= min_n) %>%
    mutate(x = .data[[xcol]], p1 = x / balls, nr = N - balls, p2 = (X - x) / nr,
           pp = X / N, z = (p1 - p2) / sqrt(pp * (1 - pp) * (1 / balls + 1 / nr)),
           p = 2 * pnorm(-abs(z)),
           cohen = (2 * asin(sqrt(p1)) - 2 * asin(sqrt(p2))) * if (higher_better) 1 else -1,
           verdict = dom_label(cohen, p, alpha))
}

# ---- team-level tables (one row per team innings) ----
# Count each team's fours and sixes and connect those totals to match results.
team_boundaries <- function(balls, matches) {
  balls %>% group_by(match_id, innings, batting_team) %>%
    summarise(fours = sum(runs_off_bat == 4), sixes = sum(runs_off_bat == 6),
              dots = sum(runs_off_bat == 0 & wides == 0), legal = sum(wides == 0), .groups = "drop") %>%
    mutate(bdry = fours + sixes, dot_pct = 100 * dots / pmax(legal, 1)) %>%
    left_join(select(matches, match_id, winner), by = "match_id") %>%
    filter(!is.na(winner)) %>%
    mutate(won = as.integer(winner == batting_team))
}

winner_loser_pairs <- function(tb) {
  tb %>% group_by(match_id) %>%
    filter(sum(won == 1) == 1, sum(won == 0) == 1) %>%
    summarise(f_w = fours[won == 1], f_l = fours[won == 0],
              s_w = sixes[won == 1], s_l = sixes[won == 0],
              b_w = bdry[won == 1],  b_l = bdry[won == 0], .groups = "drop")
}

# Calculate match-level boundary totals for trend and comparison charts.
match_boundaries <- function(balls) {
  balls %>% group_by(match_id, season) %>%
    summarise(fours = sum(runs_off_bat == 4), sixes = sum(runs_off_bat == 6), .groups = "drop")
}

# ---- Q7: does the team's dot-ball % predict winning? ----
dot_win_model <- function(tb, alpha = 0.05) {
  d <- tb %>% filter(legal > 0) %>% select(match_id, innings, dot_pct, won)
  fit  <- glm(won ~ dot_pct, data = d, family = binomial)
  null <- glm(won ~ 1, data = d, family = binomial)
  s <- summary(fit)$coefficients; ci <- exp(confint.default(fit, level = 1 - alpha))
  list(data = d, fit = fit, n = nrow(d), coef = s[2, 1], or = exp(s[2, 1]), or_ci = ci[2, ],
       p = s[2, 4], mcfadden = 1 - as.numeric(logLik(fit)) / as.numeric(logLik(null)),
       conf = 1 - alpha)
}

dot_curve <- function(m, x) {
  z <- qnorm((1 + m$conf) / 2)
  p <- predict(m$fit, data.frame(dot_pct = x), type = "link", se.fit = TRUE)
  data.frame(dot_pct = x, p = plogis(p$fit), lo = plogis(p$fit - z * p$se.fit), hi = plogis(p$fit + z * p$se.fit))
}

# ---- Q6: are boundaries increasing? ----
trend_row <- function(y, season, alpha) {
  fit <- lm(y ~ season); sm <- summary(fit)
  list(slope = sm$coefficients["season", 1], ci = as.numeric(confint(fit, "season", level = 1 - alpha)),
       p = sm$coefficients["season", 4], r2 = sm$r.squared)
}

boundary_trend <- function(mb, alpha = 0.05) {
  list(by_season = mb %>% group_by(season) %>%
         summarise(fours = mean(fours), sixes = mean(sixes), .groups = "drop"),
       fours = trend_row(mb$fours, mb$season, alpha),
       sixes = trend_row(mb$sixes, mb$season, alpha), conf = 1 - alpha)
}

# ---- Q6: do winners hit more boundaries? (paired t-test, one pair per match) ----
paired_test <- function(w, l, alpha = 0.05) {
  tt <- t.test(w, l, paired = TRUE, conf.level = 1 - alpha)
  list(mean_w = mean(w), mean_l = mean(l), diff = mean(w - l), ci = as.numeric(tt$conf.int),
       p = tt$p.value, pct = 100 * mean(w > l), n = length(w))
}
