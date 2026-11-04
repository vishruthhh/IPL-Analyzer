# IPL Statistical Analyzer — Beginner's Code Guide

This guide explains how the project fits together. The app's features and calculations have been kept in place; comments were added to make the existing code easier to follow.

## How the app runs

1. **`app.R`** checks that the needed R packages are installed.
2. It loads the other R files from `src/`.
3. **`src/data.R`** loads the saved IPL data. If the saved data is missing, it downloads and prepares the Cricsheet data.
4. `app.R` prepares common tables such as batter innings, bowler innings, and team innings.
5. **`src/ui.R`** describes what the user sees: tabs, controls, charts, and tables.
6. **`src/server.R`** connects those controls to calculations and fills the charts and tables.
7. **`src/stats.R`** contains reusable calculations and statistical tests.
8. **`src/plots.R`** keeps chart colours and styling consistent.

## What each file is for

- **`app.R`** — starts the app and prepares the data used by the rest of the project.
- **`src/data.R`** — reads and cleans raw match data, standardises team/venue names, and saves clean data.
- **`src/stats.R`** — calculates batting and bowling statistics and runs the six Statistical Lab analyses.
- **`src/plots.R`** — defines shared chart colours and a shared chart theme.
- **`src/ui.R`** — defines the page layout and output placeholders.
- **`src/server.R`** — responds to user selections and creates the displayed outputs.
- **`data-raw/prepare_data.R`** — entry point for preparing the data manually.

## The six Statistical Lab features

1. **Powerplay vs Death** — compares a batter's strike rate in the two phases.
2. **Batting First** — measures how often the team batting first wins and compares outcomes with toss winners.
3. **Consistency** — compares batters' average scores and variation in their innings.
4. **Predict a Win** — uses logistic regression to estimate the probability of winning from a first-innings score.
5. **Dot Balls** — studies dot-ball performance and its relationship with winning.
6. **Fours & Sixes** — studies boundary rates, seasonal changes, and differences between winners and losers.

## A few R/Shiny terms in plain English

- **Function**: a named set of steps that performs a task.
- **Data frame / tibble**: a table of rows and columns.
- **`filter()`**: keeps rows that match a condition.
- **`mutate()`**: adds or changes a column.
- **`group_by()` + `summarise()`**: groups rows and calculates one or more totals for each group.
- **`renderPlot()` / `renderDT()`**: tells Shiny how to display a plot or interactive table.
- **`reactive()`**: recalculates a result when one of the inputs it uses changes.
- **p-value**: a measure used in a statistical test to assess how compatible the observed result is with the test's null hypothesis. It is not the probability that the null hypothesis is true.
- **Confidence interval**: a range produced by a method intended to capture the parameter at a stated long-run rate, under the method's assumptions.

## Run the project

Open R or RStudio, set the working directory to the `ipl-analyzer` folder, then run:

```r
shiny::runApp()
```

The first run may need an internet connection to download the source data. The app requires the R packages listed at the top of `app.R`.

## Note about this beginner-friendly version

The changes in this version focus on clearer comments and explanations while keeping the original calculations, feature logic, output IDs, and UI structure intact. This is a readability pass, not a rewrite of the statistical methods. The project should still be run and checked in R/RStudio after any future code edits.
