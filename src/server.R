# Connect the visible controls to calculations and outputs. Shiny updates results when inputs change.
app_server <- function(input, output, session) {

  vb <- function(title, value, theme = "primary") value_box(title = title, value = value, theme = theme)
  dt <- function(df, round_cols = NULL, digits = 1, page = 10) {
    d <- datatable(df, rownames = FALSE, options = list(pageLength = page, dom = "tip", scrollX = TRUE))
    if (!is.null(round_cols)) d <- formatRound(d, round_cols, digits)
    d
  }
  f1 <- function(x) ifelse(is.na(x), "–", sprintf("%.1f", x))
  f2 <- function(x) ifelse(is.na(x), "–", sprintf("%.2f", x))
  interp <- function(txt, sig) div(class = paste("interp", if (!sig) "ok"), HTML(txt))

  # Fill player dropdowns using names found in the loaded data.
  default_bat  <- if ("V Kohli" %in% bat_players) "V Kohli" else bat_players[1]
  default_bowl <- if ("JJ Bumrah" %in% bowl_players) "JJ Bumrah" else bowl_players[1]
  for (id in c("bat_player", "q1_player"))
    updateSelectizeInput(session, id, choices = bat_players, selected = default_bat, server = TRUE)
  default_all <- if ("V Kohli" %in% all_players) "V Kohli" else all_players[1]
  for (id in c("dot_player", "bd_player"))
    updateSelectizeInput(session, id, choices = all_players, selected = default_all, server = TRUE)
  updateSelectizeInput(session, "bowl_player", choices = bowl_players, selected = default_bowl, server = TRUE)

  # Overview tab: headline totals, season trends, and top-player tables.
  output$ov_boxes <- renderUI({
    layout_columns(col_widths = c(3, 3, 3, 3),
      vb("Matches", format(n_distinct(matches$match_id), big.mark = ",")),
      vb("Seasons", paste(range(matches$season), collapse = "–"), "secondary"),
      vb("Runs scored", format(sum(bat_inn$runs), big.mark = ",")),
      vb("Sixes hit", format(sum(bat_inn$sixes), big.mark = ","), "success"))
  })
  output$ov_score <- renderPlot({
    d <- team_inn %>% filter(innings == 1) %>% group_by(season) %>% summarise(avg = mean(runs), .groups = "drop")
    ggplot(d, aes(season, avg)) +
      geom_line(colour = pal$navy, linewidth = 1.1) + geom_point(colour = pal$orange, size = 3) +
      scale_x_continuous(breaks = d$season) +
      labs(x = NULL, y = "Mean runs") + theme_ipl() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  output$ov_phase <- renderPlot({
    d <- balls %>% mutate(phase = over_phase(over)) %>%
      group_by(season, phase) %>%
      summarise(rpo = 6 * sum(runs_off_bat + wides + noballs) / sum(wides == 0 & noballs == 0), .groups = "drop")
    ggplot(d, aes(season, rpo, colour = phase)) + geom_line(linewidth = 1.1) + geom_point(size = 2) +
      scale_colour_manual(values = c(Powerplay = pal$teal, Middle = pal$navy, Death = pal$orange), name = NULL) +
      scale_x_continuous(breaks = unique(d$season)) +
      labs(x = NULL, y = "Runs per over") + theme_ipl() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  output$ov_bat <- renderDT({
    batting_summary(bat_inn, "player") %>% arrange(desc(runs)) %>% head(10) %>%
      select(Player = player, Inns = innings, Runs = runs, Avg = avg, SR = sr) %>% dt(c("Avg", "SR"), 1)
  })
  output$ov_bowl <- renderDT({
    bowling_summary(bowl_inn, "player") %>% arrange(desc(wickets)) %>% head(10) %>%
      select(Player = player, Matches = matches, Wkts = wickets, Avg = avg, Econ = econ) %>% dt(c("Avg", "Econ"), 2)
  })

  # Batting tab: update statistics when the selected player or seasons change.
  bat_sel <- reactive({
    req(input$bat_player, input$bat_seasons)
    list(bi = bat_inn %>% filter(player == input$bat_player, between(season, input$bat_seasons[1], input$bat_seasons[2])),
         b  = balls %>% filter(striker == input$bat_player, between(season, input$bat_seasons[1], input$bat_seasons[2])))
  })
  output$bat_boxes <- renderUI({
    bi <- bat_sel()$bi; validate(need(nrow(bi) > 0, "No innings for this selection."))
    s <- batting_summary(bi)
    layout_columns(col_widths = c(2, 2, 2, 2, 2, 2),
      vb("Innings", s$innings), vb("Runs", format(s$runs, big.mark = ",")),
      vb("Average", f1(s$avg), "secondary"), vb("Strike rate", f1(s$sr), "secondary"),
      vb("High score", s$hs, "success"), vb("50s / 100s", paste0(s$fifties, " / ", s$hundreds), "success"))
  })
  bat_tbl <- function(by, nm) {
    batting_summary(bat_sel()$bi, by) %>%
      select(!!nm := all_of(by), Inns = innings, Runs = runs, Avg = avg, SR = sr, HS = hs,
             `4s` = fours, `6s` = sixes, `Bdry %` = boundary_pct, `Dot %` = dot_pct)
  }
  output$bat_season <- renderPlot({
    d <- batting_summary(bat_sel()$bi, "season"); req(nrow(d) > 0)
    ggplot(d, aes(factor(season), runs)) + geom_col(fill = pal$navy, width = .7) +
      geom_text(aes(label = runs), vjust = -.4, colour = pal$grey, size = 3.6) +
      labs(x = NULL, y = "Runs") + theme_ipl() + theme(panel.grid.major.x = element_blank())
  })
  output$bat_season_tbl <- renderDT(dt(bat_tbl("season", "Season"), c("Avg", "SR", "Bdry %", "Dot %"), 1, 8))
  output$bat_hist <- renderPlot({
    bi <- bat_sel()$bi; req(nrow(bi) > 0)
    ggplot(bi, aes(runs)) + geom_histogram(binwidth = 10, boundary = 0, fill = pal$teal, colour = "white") +
      geom_vline(xintercept = mean(bi$runs), colour = pal$orange, linewidth = 1.1, linetype = 2) +
      labs(title = "Runs per innings", subtitle = sprintf("Mean %.1f · Median %.0f · SD %.1f",
                                                           mean(bi$runs), median(bi$runs), sd(bi$runs)),
           x = "Runs", y = "Innings") + theme_ipl()
  })
  bat_ph <- reactive(batting_phase_table(bat_sel()$b))
  output$bat_phase <- renderPlot({
    d <- bat_ph(); req(nrow(d) > 0)
    ggplot(d, aes(phase, sr, fill = phase)) + geom_col(width = .6) +
      geom_text(aes(label = sprintf("%.1f", sr)), vjust = -.4) +
      scale_fill_manual(values = c(Powerplay = pal$teal, Middle = pal$navy, Death = pal$orange)) +
      labs(x = NULL, y = "Strike rate") + theme_ipl() + theme(legend.position = "none")
  })
  output$bat_phase_tbl <- renderDT({
    bat_ph() %>% select(Phase = phase, Balls = balls, Runs = runs, SR = sr, `Bdry %` = boundary_pct,
                        `Dot %` = dot_pct, Outs = outs, Avg = avg) %>% dt(c("SR", "Bdry %", "Dot %", "Avg"), 1, 5)
  })
  output$bat_opp <- renderPlot({
    d <- batting_summary(bat_sel()$bi, "bowling_team") %>% filter(innings >= 3); req(nrow(d) > 0)
    ggplot(d, aes(reorder(bowling_team, runs_per_inn), runs_per_inn)) +
      geom_col(fill = pal$orange, width = .7) + coord_flip() +
      labs(x = NULL, y = "Runs per innings (min. 3 innings)") + theme_ipl()
  })
  output$bat_opp_tbl <- renderDT(dt(bat_tbl("bowling_team", "Opposition"), c("Avg", "SR", "Bdry %", "Dot %"), 1, 8))
  output$bat_venue_tbl <- renderDT(dt(bat_tbl("venue", "Venue"), c("Avg", "SR", "Bdry %", "Dot %"), 1, 10))
  output$bat_log <- renderDT({
    bat_sel()$bi %>% arrange(desc(season), desc(match_id)) %>%
      select(Season = season, Opposition = bowling_team, Venue = venue, Runs = runs, Balls = balls,
             `4s` = fours, `6s` = sixes, SR = strike_rate, Out = out) %>% dt("SR", 1, 10)
  })

  # Bowling tab: show the selected bowler's performance and phase breakdowns.
  bowl_sel <- reactive({
    req(input$bowl_player, input$bowl_seasons)
    list(bw = bowl_inn %>% filter(player == input$bowl_player, between(season, input$bowl_seasons[1], input$bowl_seasons[2])),
         b  = balls %>% filter(bowler == input$bowl_player, between(season, input$bowl_seasons[1], input$bowl_seasons[2])))
  })
  output$bowl_boxes <- renderUI({
    bw <- bowl_sel()$bw; validate(need(nrow(bw) > 0, "No bowling for this selection."))
    s <- bowling_summary(bw)
    layout_columns(col_widths = c(2, 2, 2, 2, 2, 2),
      vb("Matches", s$matches), vb("Wickets", s$wickets), vb("Average", f1(s$avg), "secondary"),
      vb("Economy", f2(s$econ), "secondary"), vb("Strike rate", f1(s$sr), "success"),
      vb("Dot ball %", f1(s$dot_pct), "success"))
  })
  bowl_tbl <- function(by, nm) {
    bowling_summary(bowl_sel()$bw, by) %>%
      select(!!nm := all_of(by), Matches = matches, Overs = overs, Runs = runs, Wkts = wickets,
             Avg = avg, Econ = econ, SR = sr, `Dot %` = dot_pct)
  }
  output$bowl_season <- renderPlot({
    d <- bowling_summary(bowl_sel()$bw, "season"); req(nrow(d) > 0)
    ggplot(d, aes(factor(season), wickets)) + geom_col(fill = pal$navy, width = .7) +
      geom_text(aes(label = wickets), vjust = -.4, colour = pal$grey) +
      labs(x = NULL, y = "Wickets") + theme_ipl()
  })
  output$bowl_season_tbl <- renderDT(dt(bowl_tbl("season", "Season"), c("Avg", "Econ", "SR", "Dot %"), 2, 8))
  bowl_ph <- reactive(bowling_phase_table(bowl_sel()$b))
  output$bowl_phase <- renderPlot({
    d <- bowl_ph(); req(nrow(d) > 0)
    ggplot(d, aes(phase, econ, fill = phase)) + geom_col(width = .6) +
      geom_text(aes(label = sprintf("%.2f", econ)), vjust = -.4) +
      scale_fill_manual(values = c(Powerplay = pal$teal, Middle = pal$navy, Death = pal$orange)) +
      labs(x = NULL, y = "Economy rate") + theme_ipl() + theme(legend.position = "none")
  })
  output$bowl_phase_tbl <- renderDT({
    bowl_ph() %>% select(Phase = phase, Balls = balls, Runs = runs, Wkts = wickets, Econ = econ,
                         `Dot %` = dot_pct, SR = sr) %>% dt(c("Econ", "Dot %", "SR"), 2, 5)
  })
  output$bowl_opp <- renderPlot({
    d <- bowling_summary(bowl_sel()$bw, "batting_team") %>% filter(matches >= 3); req(nrow(d) > 0)
    ggplot(d, aes(reorder(batting_team, -econ), econ)) + geom_col(fill = pal$orange, width = .7) +
      coord_flip() + labs(x = NULL, y = "Economy (min. 3 matches)") + theme_ipl()
  })
  output$bowl_opp_tbl <- renderDT(dt(bowl_tbl("batting_team", "Opposition"), c("Avg", "Econ", "SR", "Dot %"), 2, 8))
  output$bowl_venue_tbl <- renderDT(dt(bowl_tbl("venue", "Venue"), c("Avg", "Econ", "SR", "Dot %"), 2, 10))
  output$bowl_log <- renderDT({
    bowl_sel()$bw %>% arrange(desc(season), desc(match_id)) %>%
      mutate(Overs = balls %/% 6 + (balls %% 6) / 10, Econ = 6 * runs / pmax(balls, 1)) %>%
      select(Season = season, Opposition = batting_team, Venue = venue, Overs, Runs = runs,
             Wkts = wickets, Econ) %>% dt("Econ", 2, 10)
  })

  # ================= Statistical Lab =================
  # ---- Q1
  q1 <- reactive({ req(input$q1_player, input$q1_alpha); phase_test(balls, input$q1_player, input$q1_alpha) })
  output$q1_plot <- renderPlot({
    r <- q1(); validate(need(r$ok, r$msg))
    ggplot(r$data, aes(phase, sr, fill = phase)) +
      geom_boxplot(alpha = .55, outlier.shape = NA, width = .5) +
      geom_jitter(width = .12, alpha = .5, size = 1.8) +
      scale_fill_manual(values = c(Powerplay = pal$teal, Death = pal$orange)) +
      labs(x = NULL, y = "Strike rate (per innings-phase)") + theme_ipl() + theme(legend.position = "none")
  })
  output$q1_tbl <- renderTable({
    r <- q1(); req(r$ok); a <- input$q1_alpha; sig <- r$t$p.value < a
    data.frame(Statistic = c("Powerplay mean SR (n)", "Death mean SR (n)", "Difference (Death − PP)",
                             sprintf("%.0f%% CI of difference", 100 * (1 - a)),
                             sprintf("Decision at α = %.2f", a)),
               Value = c(sprintf("%.1f (%d)", r$mean_pp, r$n_pp), sprintf("%.1f (%d)", r$mean_dd, r$n_dd),
                         sprintf("%+.1f", r$mean_dd - r$mean_pp),
                         sprintf("[%.1f, %.1f]", r$t$conf.int[1], r$t$conf.int[2]),
                         if (sig) "Significant difference" else "No significant difference"))
  }, striped = TRUE, width = "100%")
  output$q1_text <- renderUI({
    r <- q1(); req(r$ok); a <- input$q1_alpha; sig <- r$t$p.value < a
    dir <- if (r$mean_dd > r$mean_pp) "higher in the death overs" else "higher in the powerplay"
    interp(sprintf("At α = %.2f the difference is <b>%s</b>. %s", a,
                   if (sig) "statistically significant" else "not statistically significant",
                   if (sig) paste0("Strike rate is ", dir, ", by about ", sprintf("%.1f", abs(r$mean_dd - r$mean_pp)), " runs per 100 balls.")
                   else "There is not enough evidence that this batter's tempo differs between the two phases."), sig)
  })

  # ---- Q2
  q2 <- reactive({ req(input$q2_alpha); bat_first_test(team_inn, matches, input$q2_alpha) })
  output$q2_boxes <- renderUI({
    r <- q2(); cl <- sprintf("%.0f%% CI", 100 * r$conf)
    layout_columns(col_widths = c(3, 3, 3, 3),
      vb("Bat-first win %", sprintf("%.1f%%", 100 * r$rate)),
      vb(cl, sprintf("%.1f–%.1f%%", 100 * r$binom$conf.int[1], 100 * r$binom$conf.int[2]), "secondary"),
      vb("Odds (bat first wins)", sprintf("%.2f", r$odds), "success"),
      vb("Toss winner wins %", sprintf("%.1f%%", 100 * r$toss$k / r$toss$n), "success"))
  })
  output$q2_plot <- renderPlot({
    d <- q2()$by_season
    ggplot(d, aes(factor(season), pct)) + geom_col(fill = pal$navy, width = .7) +
      geom_hline(yintercept = 50, colour = pal$orange, linetype = 2, linewidth = 1) +
      labs(x = NULL, y = "Bat-first side won (%)", subtitle = "Dashed line = 50% (no advantage)") + theme_ipl()
  })
  output$q2_tbl <- renderTable({
    r <- q2(); a <- input$q2_alpha; cl <- 100 * r$conf
    data.frame(Test = c("Batting first: exact binomial vs 50%",
                        sprintf("Odds ratio (bat-first win), %.0f%% CI", cl),
                        "Toss winner wins: exact binomial vs 50%"),
               Result = c(sprintf("%d of %d matches; p = %s", r$k, r$n, fmt_p(r$binom$p.value)),
                          sprintf("%.2f [%.2f, %.2f]", r$odds, r$odds_ci[1], r$odds_ci[2]),
                          sprintf("%d of %d matches; p = %s", r$toss$k, r$toss$n, fmt_p(r$toss$binom$p.value))),
               Decision = c(if (r$binom$p.value < a) "Significant" else "Not significant",
                            "–",
                            if (r$toss$binom$p.value < a) "Significant" else "Not significant"))
  }, striped = TRUE, width = "100%")
  output$q2_text <- renderUI({
    r <- q2(); a <- input$q2_alpha; sig <- r$binom$p.value < a
    interp(sprintf("Teams batting first win <b>%.1f%%</b> of matches. At α = %.2f this is <b>%s</b> from a coin-flip (p = %s). Winning the toss is <b>%s</b> associated with winning the match (p = %s).",
                   100 * r$rate, a, if (sig) "significantly different" else "not significantly different",
                   fmt_p(r$binom$p.value),
                   if (r$toss$binom$p.value < a) "significantly" else "not significantly", fmt_p(r$toss$binom$p.value)), sig)
  })

  # ---- Q3 (consistency)
  q4 <- reactive(consistency_table(bat_inn, input$q4_min, input$q4_mean))
  output$q4_plot <- renderPlot({
    d <- head(q4(), input$q4_top); validate(need(nrow(d) > 0, "No players meet these filters."))
    ggplot(d, aes(reorder(player, -cv), cv)) +
      geom_col(fill = pal$teal, width = .7) +
      geom_text(aes(label = sprintf("CV %.2f · mean %.1f", cv, mean_runs)), hjust = 1.05, colour = "white", size = 3.4) +
      coord_flip() + labs(x = NULL, y = "Coefficient of variation (lower = more consistent)") + theme_ipl()
  })
  output$q4_tbl <- renderDT({
    head(q4(), input$q4_top) %>%
      select(Player = player, Innings = innings, Mean = mean_runs, SD = sd_runs, CV = cv,
             `CI low` = ci_low, `CI high` = ci_high) %>% dt(c("Mean", "SD", "CV", "CI low", "CI high"), 2, 10)
  })

  # ---- Q4 (win prediction)
  q5 <- reactive(win_model(team_inn))
  output$q5_boxes <- renderUI({
    m <- q5(); p <- predict_win(m, input$q5_score)
    layout_columns(col_widths = c(3, 3, 3, 3),
      vb(paste("Win prob. at", input$q5_score), sprintf("%.0f%%", 100 * p$p)),
      vb("95% CI", sprintf("%.0f–%.0f%%", 100 * p$lo, 100 * p$hi), "secondary"),
      vb("McFadden R²", sprintf("%.3f", m$mcfadden), "success"),
      vb("Accuracy", sprintf("%.1f%%", 100 * m$accuracy), "success"))
  })
  output$q5_tbl <- renderTable(q5()$coefs, striped = TRUE, width = "100%", digits = 4)
  output$q5_text <- renderUI({
    m <- q5(); b <- coef(m$fit)[2]; p <- summary(m$fit)$coefficients[2, 4]
    interp(sprintf("Each extra run multiplies the odds of the batting-first side winning by <b>%.3f</b> (≈ %+.0f%% odds per 10 runs; p = %s). Even-money (50%%) is reached around <b>%.0f runs</b>. The model classifies %.1f%% of matches correctly — useful, but cricket outcomes also depend on pitch, dew, and the chase itself.",
                   exp(b), 100 * (exp(10 * b) - 1), fmt_p(p), -coef(m$fit)[1] / b, 100 * m$accuracy), p < 0.05)
  })

  # Shared number formatter for the remaining Statistical Lab analyses.
  fnum <- function(v, dig) ifelse(!is.finite(v), "–", formatC(v, format = "f", digits = dig, big.mark = ","))

  # ================= Q7 / Q8 shared helpers =================
  tally_row <- function(tab, p) tab[tab$player == p, ]
  tcol <- function(tab, col) if (nrow(tab)) tab[[col]][1] else NA_real_
  prop_tbl <- function(r, a, what, who) {
    data.frame(Statistic = c(paste0(what, " – ", who), paste0(what, " – rest of league"),
                             "Difference (percentage points)", sprintf("%.0f%% CI of difference", 100 * (1 - a)),
                             "p-value (two-proportion z-test)", "Effect size (Cohen's h; + = better)",
                             sprintf("Verdict at α = %.2f", a)),
               Value = c(sprintf("%.2f%%  (%s of %s balls)", 100 * r$p1, fnum(r$x, 0), fnum(r$n, 0)),
                         sprintf("%.2f%%", 100 * r$p2), sprintf("%+.2f", 100 * r$diff),
                         sprintf("[%.2f, %.2f]", 100 * r$ci[1], 100 * r$ci[2]), fmt_p(r$p),
                         sprintf("%+.2f", r$cohen), r$verdict))
  }
  prop_text <- function(r, a, who, what) {
    sig <- r$p < a; word <- if (r$diff > 0) "higher" else "lower"
    body <- if (sig)
      sprintf("%s's %s (%.2f%%) is significantly <b>%s</b> than the rest of the league (%.2f%%), a gap of %.2f percentage points (p = %s). Verdict: <b>%s</b>.",
              who, what, 100 * r$p1, word, 100 * r$p2, 100 * abs(r$diff), fmt_p(r$p), r$verdict)
    else
      sprintf("%s's %s (%.2f%%) is <b>not significantly different</b> from the rest of the league (%.2f%%); p = %s.",
              who, what, 100 * r$p1, 100 * r$p2, fmt_p(r$p))
    interp(sprintf("At α = %.2f: %s", a, body), sig)
  }
  phase_bars <- function(d, col, ylab) {
    ggplot(d, aes(phase, .data[[col]], fill = phase)) + geom_col(width = .6) +
      geom_text(aes(label = sprintf("%.1f%%", .data[[col]])), vjust = -.4) +
      scale_fill_manual(values = c(Powerplay = pal$teal, Middle = pal$navy, Death = pal$orange)) +
      labs(x = NULL, y = ylab) + theme_ipl() + theme(legend.position = "none")
  }

  # ================= Q7: dot balls =================
  dot_bat <- reactive({ req(input$dot_player, input$dot_alpha)
    r <- tally_row(bat_tally, input$dot_player)
    if (!nrow(r)) return(list(ok = FALSE, msg = "This player has not batted."))
    prop_dominance(r$dots, r$balls, sum(bat_tally$dots), sum(bat_tally$balls), input$dot_alpha, higher_better = FALSE)
  })
  dot_bowl <- reactive({ req(input$dot_player, input$dot_alpha)
    r <- tally_row(bowl_tally, input$dot_player)
    if (!nrow(r)) return(list(ok = FALSE, msg = "This player has not bowled."))
    prop_dominance(r$dots, r$balls, sum(bowl_tally$dots), sum(bowl_tally$balls), input$dot_alpha, higher_better = TRUE)
  })
  output$dot_boxes <- renderUI({
    req(input$dot_player); pb <- tally_row(bat_tally, input$dot_player); pw <- tally_row(bowl_tally, input$dot_player)
    layout_columns(col_widths = c(3, 3, 3, 3),
      vb("Dot balls faced", fnum(tcol(pb, "dots"), 0)),
      vb("Batting dot %", f1(tcol(pb, "dot_pct")), "secondary"),
      vb("Dot balls bowled", fnum(tcol(pw, "dots"), 0), "success"),
      vb("Bowling dot %", f1(tcol(pw, "dot_pct")), "success"))
  })
  output$dot_bat_plot <- renderPlot({
    req(input$dot_player); d <- batting_phase_table(balls %>% filter(striker == input$dot_player))
    validate(need(nrow(d) > 0, "This player has not batted."))
    phase_bars(d, "dot_pct", "Batting dot-ball % by phase")
  })
  output$dot_bat_tbl <- renderTable({
    r <- dot_bat(); req(r$ok); prop_tbl(r, input$dot_alpha, "Batting dot-ball %", input$dot_player)
  }, striped = TRUE, width = "100%")
  output$dot_bat_text <- renderUI({
    r <- dot_bat(); validate(need(r$ok, r$msg))
    prop_text(r, input$dot_alpha, input$dot_player, "batting dot-ball %")
  })
  output$dot_bowl_plot <- renderPlot({
    req(input$dot_player); d <- bowling_phase_table(balls %>% filter(bowler == input$dot_player))
    validate(need(nrow(d) > 0, "This player has not bowled."))
    phase_bars(d, "dot_pct", "Bowling dot-ball % by phase")
  })
  output$dot_bowl_tbl <- renderTable({
    r <- dot_bowl(); req(r$ok); prop_tbl(r, input$dot_alpha, "Bowling dot-ball %", input$dot_player)
  }, striped = TRUE, width = "100%")
  output$dot_bowl_text <- renderUI({
    r <- dot_bowl(); validate(need(r$ok, r$msg))
    prop_text(r, input$dot_alpha, input$dot_player, "bowling dot-ball %")
  })

  dot_board_df <- function(tot, higher_better, balls_lbl, dots_lbl) {
    req(input$dot_alpha, input$dot_min, input$dot_top)
    b <- prop_board(tot, higher_better, input$dot_min, input$dot_alpha, "dots")
    validate(need(nrow(b) > 0, "No players meet this minimum."))
    b <- if (input$dot_rank == "count") arrange(b, desc(dots))
         else if (higher_better) arrange(b, desc(p1)) else arrange(b, p1)
    out <- head(b, input$dot_top) %>%
      transmute(Player = player, B = balls, D = dots, `Dot %` = 100 * p1, `Cohen's h` = cohen,
                `p-value` = fmt_p(p), Verdict = verdict)
    names(out)[2:3] <- c(balls_lbl, dots_lbl)
    dt(out, c("Dot %", "Cohen's h"), 2, input$dot_top)
  }
  output$dot_board_bowl <- renderDT(dot_board_df(bowl_tally, TRUE, "Balls bowled", "Dot balls bowled"))
  output$dot_board_bat  <- renderDT(dot_board_df(bat_tally, FALSE, "Balls faced", "Dot balls faced"))

  dot_win <- reactive({ req(input$dot_alpha); dot_win_model(team_bd, input$dot_alpha) })
  output$dot_win_plot <- renderPlot({
    m <- dot_win(); sc <- seq(floor(min(m$data$dot_pct)), ceiling(max(m$data$dot_pct)), length.out = 200)
    cv <- dot_curve(m, sc)
    ggplot() +
      geom_jitter(data = m$data, aes(dot_pct, won), height = .04, alpha = .25, colour = pal$navy) +
      geom_ribbon(data = cv, aes(dot_pct, ymin = lo, ymax = hi), fill = pal$orange, alpha = .25) +
      geom_line(data = cv, aes(dot_pct, p), colour = pal$orange, linewidth = 1.3) +
      geom_hline(yintercept = .5, linetype = 2, colour = "grey60") +
      scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, .25)) +
      labs(x = "Team dot-ball % in the innings", y = "P(team wins)") + theme_ipl()
  })
  output$dot_win_tbl <- renderTable({
    m <- dot_win(); a <- input$dot_alpha
    data.frame(Statistic = c("Team innings analysed", "Odds ratio per +1 pp of dot balls",
                             sprintf("%.0f%% CI", 100 * (1 - a)), "Odds ratio per +5 pp",
                             "p-value (Wald test)", "McFadden R²", sprintf("Verdict at α = %.2f", a)),
               Value = c(format(m$n, big.mark = ","), sprintf("%.3f", m$or),
                         sprintf("[%.3f, %.3f]", m$or_ci[1], m$or_ci[2]), sprintf("%.3f", exp(5 * m$coef)),
                         fmt_p(m$p), sprintf("%.3f", m$mcfadden),
                         if (m$p >= a) "No significant effect"
                         else if (m$or < 1) "More dot balls significantly lower the win odds"
                         else "More dot balls significantly raise the win odds"))
  }, striped = TRUE, width = "100%")
  output$dot_win_text <- renderUI({
    m <- dot_win(); a <- input$dot_alpha; sig <- m$p < a
    interp(sprintf("At α = %.2f the dot-ball effect on winning is <b>%s</b> (p = %s). Every extra percentage point of dot balls multiplies a team's odds of winning by %.3f (about %+.0f%% per 5 points).",
                   a, if (sig) "statistically significant" else "not statistically significant", fmt_p(m$p),
                   m$or, 100 * (exp(5 * m$coef) - 1)), sig)
  })

  # ================= Q8: fours & sixes =================
  bd_stats <- function(v) {
    f <- v[["fours"]]; s6 <- v[["sixes"]]; b <- v[["balls"]]
    c(f, s6, f + s6, b, 100 * (f + s6) / b, b / f, b / s6, 100 * s6 / (f + s6))
  }
  bd_sum <- function(tab) c(fours = sum(tab$fours), sixes = sum(tab$sixes), balls = sum(tab$balls))
  bd_bat <- reactive({ req(input$bd_player, input$bd_alpha)
    r <- tally_row(bat_tally, input$bd_player)
    if (!nrow(r)) return(list(ok = FALSE, msg = "This player has not batted."))
    prop_dominance(r$bdry, r$balls, sum(bat_tally$bdry), sum(bat_tally$balls), input$bd_alpha, higher_better = TRUE)
  })
  bd_bowl <- reactive({ req(input$bd_player, input$bd_alpha)
    r <- tally_row(bowl_tally, input$bd_player)
    if (!nrow(r)) return(list(ok = FALSE, msg = "This player has not bowled."))
    prop_dominance(r$bdry, r$balls, sum(bowl_tally$bdry), sum(bowl_tally$balls), input$bd_alpha, higher_better = FALSE)
  })
  output$bd_boxes <- renderUI({
    req(input$bd_player); pb <- tally_row(bat_tally, input$bd_player); pw <- tally_row(bowl_tally, input$bd_player)
    layout_columns(col_widths = c(3, 3, 3, 3),
      vb("Fours hit", fnum(tcol(pb, "fours"), 0)),
      vb("Sixes hit", fnum(tcol(pb, "sixes"), 0), "secondary"),
      vb("Boundary % (hit)", f1(tcol(pb, "bdry_pct")), "success"),
      vb("Boundary % (conceded)", f1(tcol(pw, "bdry_pct")), "success"))
  })
  output$bd_bat_plot <- renderPlot({
    req(input$bd_player); d <- batting_phase_table(balls %>% filter(striker == input$bd_player))
    validate(need(nrow(d) > 0, "This player has not batted."))
    phase_bars(d, "boundary_pct", "Boundary % by phase")
  })
  output$bd_bat_prof <- renderTable({
    req(input$bd_player); r <- tally_row(bat_tally, input$bd_player); req(nrow(r) > 0)
    pv <- bd_sum(r); lv <- bd_sum(bat_tally) - pv; p <- bd_stats(pv); l <- bd_stats(lv)
    dg <- c(0, 0, 0, 0, 2, 1, 1, 1)
    d <- data.frame(Metric = c("Fours", "Sixes", "Boundaries (4s + 6s)", "Balls faced", "Boundary %",
                               "Balls per four", "Balls per six", "Sixes as % of boundaries"),
                    P = mapply(fnum, p, dg), L = mapply(fnum, l, dg))
    names(d)[2:3] <- c(input$bd_player, "Rest of league"); d
  }, striped = TRUE, width = "100%")
  output$bd_bat_tbl <- renderTable({
    r <- bd_bat(); req(r$ok); prop_tbl(r, input$bd_alpha, "Boundary %", input$bd_player)
  }, striped = TRUE, width = "100%")
  output$bd_bat_text <- renderUI({
    r <- bd_bat(); validate(need(r$ok, r$msg))
    prop_text(r, input$bd_alpha, input$bd_player, "boundary %")
  })
  output$bd_bowl_prof <- renderTable({
    req(input$bd_player); r <- tally_row(bowl_tally, input$bd_player)
    validate(need(nrow(r) > 0, "This player has not bowled."))
    pv <- bd_sum(r); lv <- bd_sum(bowl_tally) - pv; p <- bd_stats(pv)[1:5]; l <- bd_stats(lv)[1:5]
    dg <- c(0, 0, 0, 0, 2)
    d <- data.frame(Metric = c("Fours conceded", "Sixes conceded", "Boundaries conceded", "Legal balls bowled", "Boundary % conceded"),
                    P = mapply(fnum, p, dg), L = mapply(fnum, l, dg))
    names(d)[2:3] <- c(input$bd_player, "Rest of league"); d
  }, striped = TRUE, width = "100%")
  output$bd_bowl_tbl <- renderTable({
    r <- bd_bowl(); req(r$ok); prop_tbl(r, input$bd_alpha, "Boundary % conceded", input$bd_player)
  }, striped = TRUE, width = "100%")
  output$bd_bowl_text <- renderUI({
    r <- bd_bowl(); validate(need(r$ok, r$msg))
    prop_text(r, input$bd_alpha, input$bd_player, "boundary % conceded")
  })

  output$bd_board <- renderDT({
    req(input$bd_alpha, input$bd_min, input$bd_top, input$bd_rank)
    b <- prop_board(bat_tally, TRUE, input$bd_min, input$bd_alpha, "bdry")
    validate(need(nrow(b) > 0, "No batters meet this minimum."))
    b <- switch(input$bd_rank, fours = arrange(b, desc(fours)), sixes = arrange(b, desc(sixes)),
                bdry = arrange(b, desc(bdry)), pct = arrange(b, desc(p1)))
    out <- head(b, input$bd_top) %>%
      transmute(Batter = player, Inns = innings, Balls = balls, `4s` = fours, `6s` = sixes,
                Boundaries = bdry, `Boundary %` = 100 * p1, `Cohen's h` = cohen,
                `p-value` = fmt_p(p), Verdict = verdict)
    dt(out, c("Boundary %", "Cohen's h"), 2, input$bd_top)
  })

  bd_trend <- reactive({ req(input$bd_alpha); boundary_trend(match_bound, input$bd_alpha) })
  output$bd_trend_plot <- renderPlot({
    d <- tidyr::pivot_longer(bd_trend()$by_season, c(fours, sixes), names_to = "type", values_to = "n")
    ggplot(d, aes(season, n, colour = type)) + geom_line(linewidth = 1.1) + geom_point(size = 2.5) +
      geom_smooth(method = "lm", formula = y ~ x, se = FALSE, linetype = 2, linewidth = .7) +
      scale_colour_manual(values = c(fours = pal$navy, sixes = pal$orange), labels = c(fours = "Fours", sixes = "Sixes")) +
      scale_x_continuous(breaks = unique(d$season)) +
      labs(x = NULL, y = "Per match (both innings)", colour = NULL, subtitle = "Dashed = fitted linear trend") +
      theme_ipl() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  output$bd_trend_tbl <- renderTable({
    t <- bd_trend(); a <- input$bd_alpha
    row <- function(x) c(sprintf("%+.2f", x$slope), sprintf("[%.2f, %.2f]", x$ci[1], x$ci[2]),
                         sprintf("%.3f", x$r2), fmt_p(x$p),
                         if (x$p >= a) "No significant trend" else if (x$slope > 0) "Significant increase" else "Significant decrease")
    f <- row(t$fours); s6 <- row(t$sixes)
    d <- data.frame(Metric = c("Fours per match", "Sixes per match"), rbind(f, s6), row.names = NULL)
    names(d)[2:6] <- c("Slope per season", sprintf("%.0f%% CI", 100 * (1 - a)), "R²", "p-value", sprintf("Decision at α = %.2f", a))
    d
  }, striped = TRUE, width = "100%")
  output$bd_trend_text <- renderUI({
    t <- bd_trend(); a <- input$bd_alpha; sf <- t$fours$p < a; ss <- t$sixes$p < a
    desc <- function(x, sig, nm) if (sig) sprintf("%s per match are <b>%s significantly</b> by %.2f per season (p = %s)", nm,
                                                  if (x$slope > 0) "rising" else "falling", abs(x$slope), fmt_p(x$p))
                                 else sprintf("%s per match show <b>no significant trend</b> (p = %s)", nm, fmt_p(x$p))
    interp(sprintf("At α = %.2f: %s; %s.", a, desc(t$sixes, ss, "Sixes"), desc(t$fours, sf, "fours")), ss || sf)
  })

  bd_win <- reactive({ req(input$bd_alpha); a <- input$bd_alpha
    list(fours = paired_test(bd_pairs$f_w, bd_pairs$f_l, a), sixes = paired_test(bd_pairs$s_w, bd_pairs$s_l, a),
         bdry = paired_test(bd_pairs$b_w, bd_pairs$b_l, a)) })
  output$bd_win_plot <- renderPlot({
    n <- nrow(bd_pairs)
    d <- data.frame(group = factor(rep(c("Winning team", "Losing team"), each = n), levels = c("Winning team", "Losing team")),
                    v = c(bd_pairs$b_w, bd_pairs$b_l))
    ggplot(d, aes(group, v, fill = group)) + geom_boxplot(alpha = .55, width = .5) +
      stat_summary(fun = mean, geom = "point", shape = 23, size = 3.5, fill = "white") +
      scale_fill_manual(values = c(pal$teal, pal$orange)) +
      labs(x = NULL, y = "Boundaries (4s + 6s) in the innings", subtitle = "Diamond = mean") +
      theme_ipl() + theme(legend.position = "none")
  })
  output$bd_win_tbl <- renderTable({
    w <- bd_win(); a <- input$bd_alpha
    row <- function(x) c(sprintf("%.2f", x$mean_w), sprintf("%.2f", x$mean_l), sprintf("%+.2f", x$diff),
                         sprintf("[%.2f, %.2f]", x$ci[1], x$ci[2]), fmt_p(x$p), sprintf("%.1f%%", x$pct),
                         if (x$p >= a) "No significant difference" else if (x$diff > 0) "Winners hit more" else "Losers hit more")
    d <- data.frame(Metric = c("Fours", "Sixes", "Boundaries (4s + 6s)"),
                    rbind(row(w$fours), row(w$sixes), row(w$bdry)), row.names = NULL)
    names(d)[2:8] <- c("Winners (mean)", "Losers (mean)", "Difference", sprintf("%.0f%% CI", 100 * (1 - a)),
                       "p-value (paired t)", "Matches won by the bigger hitter", sprintf("Decision at α = %.2f", a))
    d
  }, striped = TRUE, width = "100%")
  output$bd_win_text <- renderUI({
    w <- bd_win(); a <- input$bd_alpha; x <- w$bdry; sig <- x$p < a
    interp(sprintf("At α = %.2f, across %s completed matches the winning side hit %.2f %s boundaries than the losing side (p = %s) — <b>%s</b>.",
                   a, format(x$n, big.mark = ","), abs(x$diff), if (x$diff > 0) "more" else "fewer", fmt_p(x$p),
                   if (sig) "statistically significant" else "not statistically significant"), sig)
  })

  # ================= Data =================
  output$matches_tbl <- renderDT({
    matches %>% arrange(desc(season), desc(match_id)) %>%
      select(Season = season, Team1 = team1, Team2 = team2, Venue = venue, `Toss winner` = toss_winner,
             Decision = toss_decision, Winner = winner, `Player of match` = player_of_match) %>% dt(NULL, 1, 15)
  })
  output$dl_matches <- downloadHandler(
    filename = function() "ipl_matches.csv",
    content = function(file) write.csv(matches, file, row.names = FALSE))
}
