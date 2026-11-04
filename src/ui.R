# CSS controls colours, spacing, cards, and other visual details.
app_css <- "
body { background:#f4f6fb; }
.navbar { background:#0f2a5c !important; border-bottom:4px solid #f97316; }
.navbar .navbar-brand, .navbar .nav-link { color:#fff !important; }
.navbar .nav-link.active { color:#f97316 !important; font-weight:600; }
.card { border:none; box-shadow:0 1px 6px rgba(15,42,92,.08); border-radius:14px; }
.card-header { background:#fff; font-weight:600; color:#0f2a5c; border-bottom:1px solid #e2e8f0; }
.bslib-value-box { border-radius:14px; }
.interp { background:#fff7ed; border-left:5px solid #f97316; padding:12px 16px;
          border-radius:8px; margin-top:12px; font-size:.95rem; }
.interp.ok { background:#ecfdf5; border-left-color:#14b8a6; }
.hero { background:linear-gradient(120deg,#0f2a5c,#1d4ed8); color:#fff; border-radius:16px;
        padding:22px 28px; margin-bottom:16px; }
.hero h2 { margin:0 0 4px 0; font-weight:700; } .hero p { margin:0; opacity:.9; }
.footer-note { color:#64748b; font-size:.8rem; text-align:center; padding:18px 0; }
table.table { font-size:.9rem; }
"

# Set the shared Bootstrap theme and significance-level slider.
app_theme <- bs_theme(
  version = 5, primary = "#0f2a5c", secondary = "#f97316", success = "#14b8a6",
  base_font = font_collection("system-ui", "-apple-system", "Segoe UI", "Roboto", "sans-serif")
)

lab_alpha <- function(id) sliderInput(id, "Significance level (α)", 0.01, 0.10, 0.05, step = 0.01)

# Define the visible tabs, controls, charts, and tables. server.R fills the outputs.
app_ui <- function(season_range) {
  page_navbar(
    title = "🏏 Cricket Performance Statistical Analyzer",
    theme = app_theme, fillable = FALSE,
    header = tags$head(tags$style(HTML(app_css))),
    footer = div(class = "footer-note",
                 "IPL ball-by-ball data © Cricsheet (cricsheet.org), CC BY-NC 4.0. ",
                 "Super overs excluded."),

    # ------------------------------------------------ Overview
    nav_panel("Overview",
      div(class = "hero",
          h2("IPL, measured properly"),
          p("Ball-by-ball analysis of every IPL match — descriptive stats, player profiles and formal hypothesis tests.")),
      uiOutput("ov_boxes"),
      layout_columns(col_widths = c(6, 6),
        card(card_header("Average first-innings score by season"), plotOutput("ov_score", height = 300)),
        card(card_header("Runs per over by phase and season"), plotOutput("ov_phase", height = 300))),
      layout_columns(col_widths = c(6, 6),
        card(card_header("Top 10 run scorers"), DTOutput("ov_bat")),
        card(card_header("Top 10 wicket takers"), DTOutput("ov_bowl")))
    ),

    # ------------------------------------------------ Batting
    nav_panel("Batting",
      layout_sidebar(
        sidebar = sidebar(width = 300,
          selectizeInput("bat_player", "Batter", choices = NULL),
          sliderInput("bat_seasons", "Seasons", season_range[1], season_range[2],
                      season_range, step = 1, sep = ""),
          helpText("Average = runs ÷ dismissals. Boundary % = share of balls faced hit for 4 or 6.")),
        uiOutput("bat_boxes"),
        navset_card_tab(
          nav_panel("By season", plotOutput("bat_season", height = 320), DTOutput("bat_season_tbl")),
          nav_panel("Innings distribution", plotOutput("bat_hist", height = 340)),
          nav_panel("Phases", plotOutput("bat_phase", height = 300), DTOutput("bat_phase_tbl")),
          nav_panel("Opposition", plotOutput("bat_opp", height = 380), DTOutput("bat_opp_tbl")),
          nav_panel("Venues", DTOutput("bat_venue_tbl")),
          nav_panel("Innings log", DTOutput("bat_log"))
        ))
    ),

    # ------------------------------------------------ Bowling
    nav_panel("Bowling",
      layout_sidebar(
        sidebar = sidebar(width = 300,
          selectizeInput("bowl_player", "Bowler", choices = NULL),
          sliderInput("bowl_seasons", "Seasons", season_range[1], season_range[2],
                      season_range, step = 1, sep = ""),
          helpText("Economy = runs per over. Wickets exclude run-outs and retirements.")),
        uiOutput("bowl_boxes"),
        navset_card_tab(
          nav_panel("By season", plotOutput("bowl_season", height = 320), DTOutput("bowl_season_tbl")),
          nav_panel("Phases", plotOutput("bowl_phase", height = 300), DTOutput("bowl_phase_tbl")),
          nav_panel("Opposition", plotOutput("bowl_opp", height = 380), DTOutput("bowl_opp_tbl")),
          nav_panel("Venues", DTOutput("bowl_venue_tbl")),
          nav_panel("Match log", DTOutput("bowl_log"))
        ))
    ),

    # ------------------------------------------------ Statistical lab
    nav_panel("Statistical Lab",
      div(class = "hero",
          h2("Statistical Lab"),
          p("Six research questions answered with hypothesis tests and regression models.")),
      navset_card_pill(
        nav_panel("1 · Powerplay vs Death",
          layout_columns(col_widths = c(4, 8),
            card(card_header("Question"),
                 p("Is a batter's strike rate significantly different between the powerplay (overs 1–6) and death overs (16–20)?"),
                 selectizeInput("q1_player", "Batter", choices = NULL), lab_alpha("q1_alpha"),
                 helpText("Unit of analysis: strike rate per innings-phase (≥ 3 balls). Welch two-sample t-test; the confidence interval and the decision use the chosen α.")),
            card(card_header("Result"), plotOutput("q1_plot", height = 300),
                 tableOutput("q1_tbl"), uiOutput("q1_text")))),
        nav_panel("2 · Batting first",
          layout_columns(col_widths = c(4, 8),
            card(card_header("Question"),
                 p("Does batting first change the probability of winning? Does winning the toss?"),
                 lab_alpha("q2_alpha"),
                 helpText("Intercept-only logistic regression on one row per match (outcome = side batting first won), and an exact binomial test against 50%. Confidence intervals and decisions use the chosen α.")),
            card(card_header("Result"), uiOutput("q2_boxes"), plotOutput("q2_plot", height = 280),
                 tableOutput("q2_tbl"), uiOutput("q2_text")))),
        nav_panel("3 · Consistency",
          layout_columns(col_widths = c(4, 8),
            card(card_header("Question"),
                 p("Who is the most consistent batter? Beyond the average, we use the coefficient of variation (CV = SD ÷ mean — lower is steadier) and a 95% CI for the mean."),
                 sliderInput("q4_min", "Min. innings", 10, 100, 40, step = 5),
                 sliderInput("q4_mean", "Min. mean runs per innings", 0, 40, 20, step = 1),
                 sliderInput("q4_top", "Show top N", 5, 30, 15, step = 1)),
            card(card_header("Result"), plotOutput("q4_plot", height = 420), DTOutput("q4_tbl")))),
        nav_panel("4 · Predict a win",
          layout_columns(col_widths = c(4, 8),
            card(card_header("Question"),
                 p("Can the first-innings score predict whether the side batting first wins?"),
                 numericInput("q5_score", "First-innings score", 170, 80, 300, step = 5),
                 helpText("Logistic regression: logit P(bat-first side wins) = β₀ + β₁·score.")),
            card(card_header("Result"), uiOutput("q5_boxes"),
                 tableOutput("q5_tbl"), uiOutput("q5_text")))),

        nav_panel("5 · Dot balls",
          layout_columns(col_widths = c(4, 8),
            card(card_header("Question"),
                 p("Search any player to see their dot-ball record. Is a batter's or bowler's dot-ball percentage significantly different from the rest of the league? Do dot balls decide matches?"),
                 selectizeInput("dot_player", "Search player", choices = NULL), lab_alpha("dot_alpha"),
                 radioButtons("dot_rank", "Leaderboard: rank by", c("Most dot balls" = "count", "Best dot-ball %" = "pct")),
                 sliderInput("dot_min", "Leaderboard: min. balls", 100, 2000, 500, step = 100),
                 sliderInput("dot_top", "Leaderboard: show top N", 5, 30, 10, step = 1),
                 helpText("Batting dot = no run off the bat (wides excluded). Bowling dot = legal ball with no runs conceded. Each player is compared with all other players' balls using a two-proportion z-test; the interval and verdict use the chosen α. Cohen's h (positive = better: fewer dots for a batter, more for a bowler): < 0.2 negligible, 0.2–0.5 small, 0.5–0.8 medium, ≥ 0.8 large.")),
            card(card_header("Player record"), uiOutput("dot_boxes"),
                 navset_tab(
                   nav_panel("As batter", plotOutput("dot_bat_plot", height = 260),
                             tableOutput("dot_bat_tbl"), uiOutput("dot_bat_text")),
                   nav_panel("As bowler", plotOutput("dot_bowl_plot", height = 260),
                             tableOutput("dot_bowl_tbl"), uiOutput("dot_bowl_text"))))),
          card(card_header("Dot-ball leaderboards"),
               navset_tab(nav_panel("Bowlers", DTOutput("dot_board_bowl")),
                          nav_panel("Batters", DTOutput("dot_board_bat"))),
               helpText("Every player is tested against the rest of the league, so a few false positives are expected at any α.")),
          card(card_header("Do dot balls decide matches?"),
               layout_columns(col_widths = c(7, 5),
                              plotOutput("dot_win_plot", height = 320),
                              div(tableOutput("dot_win_tbl"), uiOutput("dot_win_text"))),
               helpText("Logistic regression: win ~ team dot-ball % in its innings (one row per team innings, both innings of every completed match)."))),

        nav_panel("6 · Fours & sixes",
          layout_columns(col_widths = c(4, 8),
            card(card_header("Question"),
                 p("Search any player to see how many fours and sixes they hit (or conceded). Is their boundary rate significantly different from the rest of the league? Are boundaries increasing, and do winning teams hit more of them?"),
                 selectizeInput("bd_player", "Search player", choices = NULL), lab_alpha("bd_alpha"),
                 radioButtons("bd_rank", "Leaderboard: rank by",
                              c("Most fours" = "fours", "Most sixes" = "sixes", "Most boundaries" = "bdry", "Best boundary %" = "pct")),
                 sliderInput("bd_min", "Leaderboard: min. balls faced", 100, 2000, 500, step = 100),
                 sliderInput("bd_top", "Leaderboard: show top N", 5, 30, 10, step = 1),
                 helpText("Boundary % = (4s + 6s) ÷ balls faced. A batter's boundary % is compared with all other batters' balls (two-proportion z-test); for bowlers the same test is run on boundaries conceded per legal ball. The interval and verdict use the chosen α. Cohen's h: positive = better (more boundaries for a batter, fewer conceded for a bowler).")),
            card(card_header("Player record"), uiOutput("bd_boxes"),
                 navset_tab(
                   nav_panel("As batter", plotOutput("bd_bat_plot", height = 260), tableOutput("bd_bat_prof"),
                             tableOutput("bd_bat_tbl"), uiOutput("bd_bat_text")),
                   nav_panel("As bowler (conceded)", tableOutput("bd_bowl_prof"),
                             tableOutput("bd_bowl_tbl"), uiOutput("bd_bowl_text"))))),
          card(card_header("Boundary leaderboard (batters)"), DTOutput("bd_board"),
               helpText("Ranked by the option chosen on the left; the verdict always tests boundary %. Every batter is tested against the rest of the league, so a few false positives are expected at any α.")),
          layout_columns(col_widths = c(6, 6),
            card(card_header("Are boundaries increasing?"), plotOutput("bd_trend_plot", height = 280),
                 tableOutput("bd_trend_tbl"), uiOutput("bd_trend_text")),
            card(card_header("Do winning teams hit more boundaries?"), plotOutput("bd_win_plot", height = 280),
                 tableOutput("bd_win_tbl"), uiOutput("bd_win_text"))))
      )
    ),

    # ------------------------------------------------ Data
    nav_panel("Data",
      card(card_header("Matches"),
           downloadButton("dl_matches", "Download matches (CSV)", class = "btn-sm mb-2"),
           DTOutput("matches_tbl")),
      card(card_header("About this data"),
           p("Source: Cricsheet ball-by-ball CSV for all IPL seasons. Franchise renames are unified ",
             "(e.g. Delhi Daredevils → Delhi Capitals, Kings XI Punjab → Punjab Kings), venue names have the city suffix removed, ",
             "and a season is the calendar year in which it started. Matches without a result are excluded from win analyses."))
    )
  )
}
