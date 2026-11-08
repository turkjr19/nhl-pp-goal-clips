#!/usr/bin/env Rscript
# nhl_highlight_clips.R
#
# Finds NHL POWER PLAY goals that have a highlight clip, and gives you the clip link.
#
# How it works:
#   1. Gets the day's schedule and keeps games that have started.
#   2. For each game, reads /gamecenter/{id}/landing -> summary.scoring[].goals[]
#   3. Keeps goals where strength == "pp" (power play).
#   4. Keeps those that include a `highlightClip` (the clip ID) and builds a link:
#        - the nhl.com share link if the API provides one (highlightClipSharingUrl),
#        - otherwise the NHL's Brightcove player link built from the clip ID.
#
# This script only FINDS the clips and links to them. It does not download video.
#
# Clips usually appear a few minutes AFTER the goal, so a goal with no clip yet
# is normal. Use --watch to keep checking and print clips as they show up.
#
# Setup:
#   install.packages(c("httr2", "jsonlite"))
#
# Usage:
#   Rscript nhl_highlight_clips.R                   # YESTERDAY's power play goals
#   Rscript nhl_highlight_clips.R 2026-11-07        # a specific date
#   Rscript nhl_highlight_clips.R --all             # every goal, not just power play
#   Rscript nhl_highlight_clips.R 2026-11-08 --watch  # keep polling, print new clips
#   (--pp-only is still accepted, but power play only is now the default)

suppressPackageStartupMessages({
  library(httr2)
  library(jsonlite)
})

BASE         <- "https://api-web.nhle.com/v1"
OUT_FILE     <- "pp_goal_clips.csv"   # clips found are appended here (deduplicated)
POLL_SECONDS <- 60

# Player link for a clip ID. Taken from the open-source goal-bot project's code,
# not verified by me against the live site - click one to confirm it plays.
BRIGHTCOVE_PLAYER <- paste0("https://players.brightcove.net/6415718365001/",
                            "EXtG1xJ7H_default/index.html?videoId=")

# ---- helpers ----------------------------------------------------------------

# Safely turn an API value into a single string.
# Handles NULL, {"default": "..."} objects, and large numbers (no 1e+12 notation).
as_text <- function(x) {
  if (is.null(x) || length(x) == 0) return(NA_character_)
  if (is.list(x)) {
    if (!is.null(x$default)) x <- x$default else return(NA_character_)
  }
  if (is.numeric(x)) return(format(x[1], scientific = FALSE, trim = TRUE))
  as.character(x[1])
}

nhl_get <- function(path) {
  request(paste0(BASE, path)) |>
    req_user_agent("nhl-highlight-clips-r/0.1") |>
    req_retry(max_tries = 3) |>
    req_perform() |>
    resp_body_json(simplifyVector = FALSE)
}

# IDs of games on `date` that have started (live or finished).
get_started_game_ids <- function(date) {
  sched <- nhl_get(paste0("/schedule/", date))
  day <- Filter(function(d) identical(d$date, date), sched$gameWeek)
  if (length(day) == 0) return(character())
  games <- day[[1]]$games
  if (length(games) == 0) return(character())
  ids    <- vapply(games, function(g) as_text(g$id), character(1))
  states <- vapply(games, function(g) as_text(g$gameState), character(1))
  ids[!states %in% c("FUT", "PRE")]
}

goal_row <- function(g, game_id, matchup, period_label, game_date) {
  scorer <- as_text(g$name)
  if (is.na(scorer)) {
    scorer <- trimws(paste(na.omit(c(as_text(g$firstName), as_text(g$lastName))),
                           collapse = " "))
  }
  clip    <- as_text(g$highlightClip)               # NA until the clip exists
  sharing <- as_text(g$highlightClipSharingUrl)     # nhl.com link, if provided
  player  <- if (is.na(clip)) NA_character_ else paste0(BRIGHTCOVE_PLAYER, clip)
  
  data.frame(
    game_date      = game_date,
    game_id        = game_id,
    matchup        = matchup,
    period         = period_label,
    time_in_period = as_text(g$timeInPeriod),
    team           = as_text(g$teamAbbrev),
    scorer         = scorer,
    strength       = as_text(g$strength),            # "ev", "pp" or "sh"
    away_score     = as_text(g$awayScore),
    home_score     = as_text(g$homeScore),
    highlight_clip = clip,
    sharing_url    = sharing,
    player_url     = player,
    clip_url       = if (!is.na(sharing)) sharing else player,   # the one we print
    stringsAsFactors = FALSE
  )
}

# All goals for one game (with or without a clip yet).
get_goals_for_game <- function(game_id, game_date) {
  land <- nhl_get(paste0("/gamecenter/", game_id, "/landing"))
  scoring <- land$summary$scoring
  if (is.null(scoring) || length(scoring) == 0) return(NULL)
  
  matchup <- paste0(as_text(land$awayTeam$abbrev), " @ ", as_text(land$homeTeam$abbrev))
  
  rows <- list()
  for (p in scoring) {
    pd <- p$periodDescriptor
    period_label <- paste0(as_text(pd$periodType), as_text(pd$number))
    for (g in p$goals) {
      rows[[length(rows) + 1]] <- goal_row(g, game_id, matchup, period_label, game_date)
    }
  }
  if (length(rows) == 0) return(NULL)
  do.call(rbind, rows)
}

# All goals for the date, one row per goal.
get_goals <- function(date) {
  ids <- get_started_game_ids(date)
  out <- lapply(ids, function(id) {
    tryCatch(get_goals_for_game(id, date), error = function(e) {
      message(sprintf("  game %s failed: %s", id, conditionMessage(e)))
      NULL
    })
  })
  out <- Filter(Negate(is.null), out)
  if (length(out) == 0) return(NULL)
  do.call(rbind, out)
}

# Columns written to the CSV (internal columns such as the clip ID are left out).
CSV_COLS <- c("game_date", "game_id", "matchup", "period", "time_in_period", "team",
              "scorer", "strength", "away_score", "home_score", "clip_url")

# A goal is identified by game + period + time + team.
goal_key <- function(df) paste(df$game_id, df$period, df$time_in_period, df$team)

# Append goals not already in OUT_FILE.
save_new <- function(clips) {
  if (nrow(clips) == 0) return(invisible())
  out <- clips[, CSV_COLS, drop = FALSE]
  if (file.exists(OUT_FILE)) {
    old <- read.csv(OUT_FILE, colClasses = "character")
    if (!identical(names(old), CSV_COLS)) {
      # Older file with different columns: keep it under a new name, start a fresh one.
      backup <- paste0("pp_goal_clips_old_", format(Sys.time(), "%Y%m%d%H%M%S"), ".csv")
      file.rename(OUT_FILE, backup)
      message("Existing ", OUT_FILE, " had different columns; renamed to ", backup)
    } else {
      out <- out[!goal_key(out) %in% goal_key(old), , drop = FALSE]
    }
  }
  if (nrow(out) == 0) return(invisible())
  write.table(out, OUT_FILE, sep = ",", row.names = FALSE, qmethod = "double",
              col.names = !file.exists(OUT_FILE), append = file.exists(OUT_FILE))
}

show_clips <- function(clips) {
  for (i in seq_len(nrow(clips))) {
    with(clips[i, ], cat(sprintf(
      "[%s] %s %s %s | %s | %s %s-%s | %s\n",
      matchup, period, time_in_period, toupper(strength), team,
      scorer, away_score, home_score, clip_url)))
  }
}

# ---- main -------------------------------------------------------------------

args     <- commandArgs(trailingOnly = TRUE)
watch    <- "--watch" %in% args
pp_only  <- !("--all" %in% args)          # power play only unless --all is given
date_arg <- args[grepl("^\\d{4}-\\d{2}-\\d{2}$", args)]

# Default: yesterday's date in Toronto time (pass a YYYY-MM-DD argument to override).
today_toronto <- as.Date(format(Sys.time(), "%Y-%m-%d", tz = "America/Toronto"))
date <- if (length(date_arg)) date_arg[1] else format(today_toronto - 1, "%Y-%m-%d")

seen_keys <- character()

repeat {
  goals <- tryCatch(get_goals(date), error = function(e) {
    message("Request failed: ", conditionMessage(e))
    NULL
  })
  
  if (is.null(goals)) {
    message(sprintf("%s: no goals found yet.", date))
  } else {
    total <- nrow(goals)
    is_pp <- !is.na(goals$strength) & goals$strength == "pp"
    if (pp_only) goals <- goals[is_pp, , drop = FALSE]
    
    clips <- goals[!is.na(goals$highlight_clip), , drop = FALSE]
    
    message(sprintf("%s: %d goal(s) total%s; %d clip(s) ready, %d waiting for a clip.",
                    date, total,
                    if (pp_only) sprintf(", %d power play", nrow(goals)) else "",
                    nrow(clips), nrow(goals) - nrow(clips)))
    
    keys <- paste(clips$game_id, clips$highlight_clip)
    to_show <- if (watch) clips[!keys %in% seen_keys, , drop = FALSE] else clips
    show_clips(to_show)
    save_new(to_show)
    seen_keys <- union(seen_keys, keys)
  }
  
  if (!watch) break
  Sys.sleep(POLL_SECONDS)
}
