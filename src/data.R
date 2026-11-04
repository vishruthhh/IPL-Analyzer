# ---------------------------------------------------------------
# Data layer: download + clean Cricsheet IPL ball-by-ball data
# Source: https://cricsheet.org/downloads/ipl_csv2.zip
# ---------------------------------------------------------------

IPL_URL <- "https://cricsheet.org/downloads/ipl_csv2.zip"

# Team names changed over the years. Convert old names to one common name so a team is not counted twice.
TEAM_MAP <- c(
  "Delhi Daredevils"            = "Delhi Capitals",
  "Kings XI Punjab"             = "Punjab Kings",
  "Royal Challengers Bangalore" = "Royal Challengers Bengaluru",
  "Rising Pune Supergiants"     = "Rising Pune Supergiant",
  "Pune Warriors"               = "Pune Warriors India"
)

# Standardise venue names so small naming differences do not split one stadium into multiple entries.
VENUE_MAP <- c(
  "M.Chinnaswamy Stadium"                       = "M Chinnaswamy Stadium",
  "Feroz Shah Kotla"                            = "Arun Jaitley Stadium",
  "Punjab Cricket Association IS Bindra Stadium" = "Punjab Cricket Association Stadium",
  "Sardar Patel Stadium"                        = "Narendra Modi Stadium",
  "Rajiv Gandhi International Stadium"          = "Rajiv Gandhi International Stadium"
)

BALL_COLS <- c("match_id", "start_date", "venue", "innings", "ball",
               "batting_team", "bowling_team", "striker", "non_striker", "bowler",
               "runs_off_bat", "extras", "wides", "noballs", "byes", "legbyes",
               "penalty", "wicket_type", "player_dismissed")

# Replace values found in a lookup table. Leave values not in the table unchanged.
recode_map <- function(x, map) {
  i <- match(x, names(map))
  ifelse(is.na(i), x, unname(map)[i])
}

# Read one match file. Read columns as text first so files can be combined consistently.
read_balls_file <- function(f) {
  x <- readr::read_csv(f, col_types = readr::cols(.default = readr::col_character()),
                       progress = FALSE)
  for (n in BALL_COLS) if (!n %in% names(x)) x[[n]] <- NA_character_
  x$match_id <- sub("\\.csv$", "", basename(f))
  x[, BALL_COLS]
}

# Extract match details such as the winner and toss winner from the info CSV.
read_info_file <- function(f) {
  l <- readLines(f, warn = FALSE)
  l <- sub("^info,", "", l[startsWith(l, "info,")])
  key <- sub(",.*$", "", l)
  val <- gsub('^"|"$', "", sub("^[^,]*,", "", l))
  get <- function(k) { v <- val[key == k]; if (length(v)) v[1] else NA_character_ }
  tibble::tibble(
    match_id        = sub("_info\\.csv$", "", basename(f)),
    city            = get("city"),
    toss_winner     = get("toss_winner"),
    toss_decision   = get("toss_decision"),
    winner          = get("winner"),
    player_of_match = get("player_of_match")
  )
}

# Clean the delivery data: convert numbers, add season/over, standardise names, and remove super overs.
clean_balls <- function(b) {
  num <- c("innings", "runs_off_bat", "extras", "wides", "noballs",
           "byes", "legbyes", "penalty")
  b[num] <- lapply(b[num], function(v) {
    v <- suppressWarnings(as.numeric(v)); v[is.na(v)] <- 0; v
  })
  b$over   <- floor(suppressWarnings(as.numeric(b$ball))) + 1
  b$season <- as.integer(substr(b$start_date, 1, 4))
  b$venue  <- recode_map(sub(",.*$", "", b$venue), VENUE_MAP)
  b$batting_team <- recode_map(b$batting_team, TEAM_MAP)
  b$bowling_team <- recode_map(b$bowling_team, TEAM_MAP)
  b$wicket_type[!is.na(b$wicket_type) & b$wicket_type == ""] <- NA
  b <- b[b$innings %in% c(1, 2), ]            # drop super overs
  b[, c("match_id", "season", "venue", "innings", "over", "batting_team",
        "bowling_team", "striker", "non_striker", "bowler", "runs_off_bat",
        "extras", "wides", "noballs", "byes", "legbyes", "penalty",
        "wicket_type", "player_dismissed")]
}

# Download or read the source data, clean it, join match details, and save clean RDS files.
build_ipl_data <- function(zip_path = NULL, dir = "data", url = IPL_URL) {
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  if (is.null(zip_path)) {
    zip_path <- tempfile(fileext = ".zip")
    message("Downloading Cricsheet IPL data ...")
    utils::download.file(url, zip_path, mode = "wb", quiet = TRUE)
  }
  exdir <- tempfile("ipl_")
  utils::unzip(zip_path, exdir = exdir)
  files      <- list.files(exdir, pattern = "\\.csv$", full.names = TRUE, recursive = TRUE)
  info_files <- files[grepl("_info\\.csv$", files)]
  ball_files <- files[grepl("^[0-9]+\\.csv$", basename(files))]
  message("Reading ", length(ball_files), " matches ...")

  balls <- clean_balls(dplyr::bind_rows(lapply(ball_files, read_balls_file)))

  info <- dplyr::bind_rows(lapply(info_files, read_info_file))
  info$winner      <- recode_map(info$winner, TEAM_MAP)
  info$toss_winner <- recode_map(info$toss_winner, TEAM_MAP)

  meta <- balls |>
    dplyr::group_by(match_id) |>
    dplyr::summarise(season = dplyr::first(season), venue = dplyr::first(venue),
                     team1 = dplyr::first(batting_team[innings == 1]),
                     team2 = dplyr::first(batting_team[innings == 2]),
                     .groups = "drop")
  matches <- dplyr::left_join(meta, info, by = "match_id")

  saveRDS(balls,   file.path(dir, "ipl_balls.rds"))
  saveRDS(matches, file.path(dir, "ipl_matches.rds"))
  message("Saved ", nrow(balls), " deliveries from ", nrow(matches), " matches.")
  invisible(list(balls = balls, matches = matches))
}

# Use saved clean data when possible. If it is missing, build it first.
load_ipl_data <- function(dir = "data") {
  fb <- file.path(dir, "ipl_balls.rds"); fm <- file.path(dir, "ipl_matches.rds")
  if (!file.exists(fb) || !file.exists(fm)) return(build_ipl_data(dir = dir))
  list(balls = readRDS(fb), matches = readRDS(fm))
}
