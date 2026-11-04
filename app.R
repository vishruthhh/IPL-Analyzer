# Cricket Performance Statistical Analyzer — run with: shiny::runApp()
# Check required packages first and report any that need installing.
required <- c("shiny", "bslib", "dplyr", "tidyr", "ggplot2", "DT", "readr", "scales", "tibble")
missing_pkgs <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_pkgs)) stop("Install missing packages first: install.packages(c(",
                               paste0('"', missing_pkgs, '"', collapse = ", "), "))")
suppressPackageStartupMessages({
  library(shiny); library(bslib); library(dplyr); library(tidyr); library(ggplot2); library(DT)
})

# Load the project modules from src/.
for (f in c("data", "stats", "plots", "ui", "server")) source(file.path("src", paste0(f, ".R")), local = TRUE)

# Load clean datasets; the first run downloads and prepares them if needed.
ipl     <- load_ipl_data("data")
balls   <- ipl$balls
matches <- ipl$matches

# Prepare commonly used tables once at startup so dashboards can reuse them.
bat_inn   <- batter_innings(balls)
bowl_inn  <- bowler_innings(balls)
team_inn  <- team_innings(balls, matches)
bat_tally   <- player_bat_tally(balls)        # Q5/Q6: per-player ball tallies
bowl_tally  <- player_bowl_tally(balls)
all_players <- sort(unique(c(bat_tally$player, bowl_tally$player)))
team_bd     <- team_boundaries(balls, matches)
bd_pairs    <- winner_loser_pairs(team_bd)
match_bound <- match_boundaries(balls)
bat_players  <- bat_inn  %>% group_by(player) %>% summarise(r = sum(runs)) %>% arrange(desc(r)) %>% pull(player)
bowl_players <- bowl_inn %>% group_by(player) %>% summarise(w = sum(wickets)) %>% arrange(desc(w)) %>% pull(player)

shinyApp(app_ui(range(balls$season)), app_server)
