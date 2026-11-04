# 🏏 Cricket Performance Statistical Analyzer
An R/Shiny app for statistical evaluation of IPL players and teams, built on
[Cricsheet](https://cricsheet.org) ball-by-ball data (IPL only).

## Run it
```r
install.packages(c("shiny","bslib","dplyr","tidyr","ggplot2","DT","readr","scales","tibble"))
shiny::runApp()          # from the project folder
```
First launch downloads `ipl_csv2.zip` from Cricsheet (needs internet, ~1 min) and caches
clean data in `data/*.rds`. Offline? Download the zip manually, then:
```r
source("src/data.R"); build_ipl_data(zip_path = "ipl_csv2.zip")
```
Requires R ≥ 4.1 and bslib ≥ 0.7.

## Structure
| File | Purpose |
|---|---|
| `app.R` | Launcher: loads data, precomputes tables, starts the app |
| `src/data.R` | Download, parse and clean Cricsheet data (team/venue name unification) |
| `src/stats.R` | Descriptive stats + the six statistical analyses |
| `src/plots.R` | ggplot theme and palette |
| `src/ui.R`, `src/server.R` | Shiny frontend (bslib) |
| `data-raw/prepare_data.R` | Rebuild the cached data |

## Statistical Lab
1. **Powerplay vs death strike rate** – Welch t-test on per-innings-phase SR; CI and decision follow the chosen α
2. **Batting first** – intercept-only logistic regression / exact binomial test, toss effect; CIs follow the chosen α
3. **Consistency** – mean, SD, coefficient of variation, 95% CI
4. **Win prediction** – logistic regression of bat-first win on first-innings score (+ calculator)
5. **Dot balls** – search any player: batting/bowling dot-ball % vs the league (two-proportion z-test, Cohen's h), leaderboards, and logistic regression of winning on team dot-ball %
6. **Fours & sixes** – search any player: boundary % hit/conceded vs the league, boundary leaderboard, season trend (regression) and winners-vs-losers (paired t-test)

## Definitions
Balls faced exclude wides. Bowler runs = bat runs + wides + no-balls. Bowler wickets exclude
run-outs/retirements. Super overs excluded. Season = calendar year the season started.
Matches without a result are excluded from win analyses.

Data © Cricsheet, CC BY-NC 4.0 — keep attribution, non-commercial use.
