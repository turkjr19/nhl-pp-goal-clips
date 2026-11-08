
#!/usr/bin/env Rscript

# ============================================================
# nhl_highlight_clips.R
#
# Finds NHL POWER PLAY goals that have a highlight clip.
#
# CSV OUTPUT:
#   pp_goal_clips.csv
#
# Includes:
#   player_url = Brightcove player URL
#   clip_url   = NHL sharing URL, if available,
#                otherwise Brightcove player URL
#
# HOW IT WORKS:
#   1. Gets the day's schedule and keeps games that have started.
#   2. Reads gamecenter/{id}/landing.
#   3. Extracts scoring[].goals[].
#   4. Keeps power play goals by default.
#   5. Finds goals with highlight clips.
#   6. Builds Brightcove player URLs.
#   7. Saves new goals to pp_goal_clips.csv.
#
# USAGE:
#   Rscript nhl_highlight_clips.R
#   Rscript nhl_highlight_clips.R 2026-11-07
#   Rscript nhl_highlight_clips.R --all
#   Rscript nhl_highlight_clips.R 2026-11-08 --watch
#
#   --all      Include all goal strengths.
#   --watch    Continue checking for new clips.
#
# ============================================================


# ------------------------------------------------------------
# LIBRARIES
# ------------------------------------------------------------

suppressPackageStartupMessages({

  library(httr2)
  library(jsonlite)

})


# ------------------------------------------------------------
# SETTINGS
# ------------------------------------------------------------

BASE <- "https://api-web.nhle.com/v1"

OUT_FILE <- "pp_goal_clips.csv"

POLL_SECONDS <- 60


# Brightcove player URL
BRIGHTCOVE_PLAYER <- paste0(
  "https://players.brightcove.net/6415718365001/",
  "EXtG1xJ7H_default/index.html?videoId="
)


# ------------------------------------------------------------
# CSV COLUMNS
# ------------------------------------------------------------

CSV_COLS <- c(
  "game_date",
  "game_id",
  "matchup",
  "period",
  "time_in_period",
  "team",
  "scorer",
  "strength",
  "away_score",
  "home_score",
  "player_url",
  "clip_url"
)


# ------------------------------------------------------------
# HELPERS
# ------------------------------------------------------------

# Safely convert API values into strings.

as_text <- function(x) {

  if (is.null(x) || length(x) == 0) {
    return(NA_character_)
  }

  if (is.list(x)) {

    if (!is.null(x$default)) {
      x <- x$default
    } else {
      return(NA_character_)
    }

  }

  if (is.numeric(x)) {

    return(
      format(
        x[1],
        scientific = FALSE,
        trim = TRUE
      )
    )

  }

  as.character(x[1])

}


# ------------------------------------------------------------
# NHL API REQUEST
# ------------------------------------------------------------

nhl_get <- function(path) {

  request(paste0(BASE, path)) |>
    req_user_agent("nhl-highlight-clips-r/0.1") |>
    req_retry(max_tries = 3) |>
    req_perform() |>
    resp_body_json(simplifyVector = FALSE)

}


# ------------------------------------------------------------
# GET STARTED GAMES
# ------------------------------------------------------------

get_started_game_ids <- function(date) {

  sched <- nhl_get(
    paste0("/schedule/", date)
  )

  day <- Filter(
    function(d) identical(d$date, date),
    sched$gameWeek
  )

  if (length(day) == 0) {
    return(character())
  }

  games <- day[[1]]$games

  if (length(games) == 0) {
    return(character())
  }

  ids <- vapply(
    games,
    function(g) as_text(g$id),
    character(1)
  )

  states <- vapply(
    games,
    function(g) as_text(g$gameState),
    character(1)
  )

  ids[!states %in% c("FUT", "PRE")]

}


# ------------------------------------------------------------
# CREATE GOAL ROW
# ------------------------------------------------------------

goal_row <- function(
  g,
  game_id,
  matchup,
  period_label,
  game_date
) {

  scorer <- as_text(g$name)

  if (is.na(scorer)) {

    scorer <- trimws(
      paste(
        na.omit(
          c(
            as_text(g$firstName),
            as_text(g$lastName)
          )
        ),
        collapse = " "
      )
    )

  }

  # NHL highlight clip ID
  clip <- as_text(g$highlightClip)

  # NHL sharing URL, if provided
  sharing <- as_text(g$highlightClipSharingUrl)

  # Brightcove player URL
  player <- if (is.na(clip)) {
    NA_character_
  } else {
    paste0(BRIGHTCOVE_PLAYER, clip)
  }


  data.frame(

    game_date = game_date,

    game_id = game_id,

    matchup = matchup,

    period = period_label,

    time_in_period = as_text(g$timeInPeriod),

    team = as_text(g$teamAbbrev),

    scorer = scorer,

    strength = as_text(g$strength),

    away_score = as_text(g$awayScore),

    home_score = as_text(g$homeScore),

    highlight_clip = clip,

    sharing_url = sharing,

    # Brightcove URL used by the video downloader
    player_url = player,

    # NHL sharing URL or Brightcove fallback
    clip_url = if (!is.na(sharing)) {
      sharing
    } else {
      player
    },

    stringsAsFactors = FALSE

  )

}


# ------------------------------------------------------------
# GET GOALS FOR ONE GAME
# ------------------------------------------------------------

get_goals_for_game <- function(game_id, game_date) {

  land <- nhl_get(
    paste0("/gamecenter/", game_id, "/landing")
  )

  scoring <- land$summary$scoring

  if (is.null(scoring) || length(scoring) == 0) {
    return(NULL)
  }

  matchup <- paste0(
    as_text(land$awayTeam$abbrev),
    " @ ",
    as_text(land$homeTeam$abbrev)
  )

  rows <- list()

  for (p in scoring) {

    pd <- p$periodDescriptor

    period_label <- paste0(
      as_text(pd$periodType),
      as_text(pd$number)
    )

    for (g in p$goals) {

      rows[[length(rows) + 1]] <- goal_row(
        g,
        game_id,
        matchup,
        period_label,
        game_date
      )

    }

  }

  if (length(rows) == 0) {
    return(NULL)
  }

  do.call(rbind, rows)

}


# ------------------------------------------------------------
# GET ALL GOALS FOR A DATE
# ------------------------------------------------------------

get_goals <- function(date) {

  ids <- get_started_game_ids(date)

  out <- lapply(
    ids,
    function(id) {

      tryCatch(

        get_goals_for_game(id, date),

        error = function(e) {

          message(
            sprintf(
              "Game %s failed: %s",
              id,
              conditionMessage(e)
            )
          )

          NULL

        }

      )

    }
  )

  out <- Filter(Negate(is.null), out)

  if (length(out) == 0) {
    return(NULL)
  }

  do.call(rbind, out)

}


# ------------------------------------------------------------
# GOAL IDENTIFICATION
# ------------------------------------------------------------

goal_key <- function(df) {

  paste(
    df$game_id,
    df$period,
    df$time_in_period,
    df$team
  )

}


# ------------------------------------------------------------
# SAVE NEW GOALS
#
# Automatically migrates older CSV files to include player_url.
# Existing records are preserved.
# ------------------------------------------------------------

save_new <- function(clips) {

  if (is.null(clips) || nrow(clips) == 0) {
    return(invisible())
  }

  out <- clips[, CSV_COLS, drop = FALSE]


  # ----------------------------------------------------------
  # EXISTING CSV
  # ----------------------------------------------------------

  if (file.exists(OUT_FILE)) {

    old <- read.csv(
      OUT_FILE,
      colClasses = "character",
      stringsAsFactors = FALSE
    )


    # Add player_url to older CSV files
    if (!"player_url" %in% names(old)) {

      old$player_url <- NA_character_

      # Recover Brightcove URL from clip_url
      if ("clip_url" %in% names(old)) {

        is_brightcove <- !is.na(old$clip_url) &
          grepl(
            "players\\.brightcove\\.net",
            old$clip_url
          )

        old$player_url[is_brightcove] <-
          old$clip_url[is_brightcove]

      }

      message(
        "Migrated existing CSV to include player_url."
      )

    }


    # Add any other missing columns
    for (col in CSV_COLS) {

      if (!col %in% names(old)) {
        old[[col]] <- NA_character_
      }

    }


    # Keep correct column order
    old <- old[, CSV_COLS, drop = FALSE]


    # Remove duplicate goals
    out <- out[
      !goal_key(out) %in% goal_key(old),
      ,
      drop = FALSE
    ]


    # Combine old and new records
    combined <- rbind(old, out)


    # Rewrite CSV
    write.csv(
      combined,
      OUT_FILE,
      row.names = FALSE,
      na = ""
    )

  } else {

    # Create new CSV
    write.csv(
      out,
      OUT_FILE,
      row.names = FALSE,
      na = ""
    )

  }


  message(
    "CSV updated: ",
    OUT_FILE
  )

}


# ------------------------------------------------------------
# DISPLAY CLIPS
# ------------------------------------------------------------

show_clips <- function(clips) {

  for (i in seq_len(nrow(clips))) {

    with(
      clips[i, ],
      cat(
        sprintf(
          "[%s] %s %s %s | %s | %s %s-%s | %s\n",
          matchup,
          period,
          time_in_period,
          toupper(strength),
          team,
          scorer,
          away_score,
          home_score,
          player_url
        )
      )
    )

  }

}


# ============================================================
# MAIN
# ============================================================

args <- commandArgs(trailingOnly = TRUE)

watch <- "--watch" %in% args

# Power play only unless --all is specified
pp_only <- !("--all" %in% args)

date_arg <- args[
  grepl("^\\d{4}-\\d{2}-\\d{2}$", args)
]


# Default to yesterday in Toronto time
today_toronto <- as.Date(
  format(
    Sys.time(),
    "%Y-%m-%d",
    tz = "America/Toronto"
  )
)

date <- if (length(date_arg)) {
  date_arg[1]
} else {
  format(today_toronto - 1, "%Y-%m-%d")
}


seen_keys <- character()


# ============================================================
# RUN
# ============================================================

repeat {

  goals <- tryCatch(

    get_goals(date),

    error = function(e) {

      message(
        "Request failed: ",
        conditionMessage(e)
      )

      NULL

    }

  )


  if (is.null(goals)) {

    message(
      sprintf(
        "%s: no goals found yet.",
        date
      )
    )

  } else {

    total <- nrow(goals)

    is_pp <- !is.na(goals$strength) &
      goals$strength == "pp"

    if (pp_only) {
      goals <- goals[is_pp, , drop = FALSE]
    }


    # Only goals with an available highlight clip
    clips <- goals[
      !is.na(goals$highlight_clip),
      ,
      drop = FALSE
    ]


    message(
      sprintf(
        "%s: %d goal(s) total%s; %d clip(s) ready, %d waiting for a clip.",
        date,
        total,
        if (pp_only) {
          sprintf(", %d power play", nrow(goals))
        } else {
          ""
        },
        nrow(clips),
        nrow(goals) - nrow(clips)
      )
    )


    keys <- paste(
      clips$game_id,
      clips$highlight_clip
    )

    to_show <- if (watch) {
      clips[!keys %in% seen_keys, , drop = FALSE]
    } else {
      clips
    }


    show_clips(to_show)

    save_new(to_show)

    seen_keys <- union(
      seen_keys,
      keys
    )

  }


  if (!watch) {
    break
  }

  Sys.sleep(POLL_SECONDS)

}


# ============================================================
# COMPLETE
# ============================================================

message("")
message("========================================")
message("NHL HIGHLIGHT CLIP SCRIPT COMPLETE")
message("Date: ", date)
message("CSV: ", OUT_FILE)
message("========================================")
