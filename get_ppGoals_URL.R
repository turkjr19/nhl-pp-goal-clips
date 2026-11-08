
#!/usr/bin/env Rscript
# ============================================================
# NHL Power Play Goal Clips
#
# Finds NHL power-play goals with highlight clips.
# Saves goal information and video links to pp_goal_clips.csv.
#
# CSV includes:
#   game_date, game_id, matchup, period, time_in_period,
#   team, scorer, strength, away_score, home_score,
#   player_url, clip_url
#
# Default: yesterday's games, Toronto time.
# ============================================================

suppressPackageStartupMessages({
  library(httr2)
  library(jsonlite)
})

# ------------------------------------------------------------
# SETTINGS
# ------------------------------------------------------------

BASE         <- "https://api-web.nhle.com/v1"
OUT_FILE     <- "pp_goal_clips.csv"
POLL_SECONDS <- 60

BRIGHTCOVE_PLAYER <- paste0(
  "https://players.brightcove.net/6415718365001/",
  "EXtG1xJ7H_default/index.html?videoId="
)

# Optional GitHub API settings
GH_REPO   <- Sys.getenv("GH_REPO")
GH_TOKEN  <- Sys.getenv("GH_TOKEN", unset = Sys.getenv("GITHUB_PAT"))
GH_PATH   <- Sys.getenv("GH_PATH", unset = "pp_goal_clips.csv")
GH_BRANCH <- Sys.getenv("GH_BRANCH")
GH_API    <- Sys.getenv("GH_API", unset = "https://api.github.com")

# ------------------------------------------------------------
# HELPERS
# ------------------------------------------------------------

as_text <- function(x) {
  if (is.null(x) || length(x) == 0) return(NA_character_)

  if (is.list(x)) {
    if (!is.null(x$default)) {
      x <- x$default
    } else {
      return(NA_character_)
    }
  }

  if (is.numeric(x)) {
    return(format(x[1], scientific = FALSE, trim = TRUE))
  }

  as.character(x[1])
}

nhl_get <- function(path) {
  request(paste0(BASE, path)) |>
    req_user_agent("nhl-highlight-clips-r/0.1") |>
    req_retry(max_tries = 3) |>
    req_perform() |>
    resp_body_json(simplifyVector = FALSE)
}

# ------------------------------------------------------------
# GAME SCHEDULE
# ------------------------------------------------------------

get_started_game_ids <- function(date) {

  sched <- nhl_get(paste0("/schedule/", date))

  day <- Filter(
    function(d) identical(d$date, date),
    sched$gameWeek
  )

  if (length(day) == 0) return(character())

  games <- day[[1]]$games

  if (length(games) == 0) return(character())

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

goal_row <- function(g, game_id, matchup, period_label, game_date) {

  scorer <- as_text(g$name)

  if (is.na(scorer)) {
    scorer <- trimws(
      paste(
        na.omit(c(
          as_text(g$firstName),
          as_text(g$lastName)
        )),
        collapse = " "
      )
    )
  }

  clip <- as_text(g$highlightClip)

  sharing <- as_text(g$highlightClipSharingUrl)

  player <- if (is.na(clip)) {
    NA_character_
  } else {
    paste0(BRIGHTCOVE_PLAYER, clip)
  }

  data.frame(
    game_date      = game_date,
    game_id        = game_id,
    matchup        = matchup,
    period         = period_label,
    time_in_period = as_text(g$timeInPeriod),
    team           = as_text(g$teamAbbrev),
    scorer         = scorer,
    strength       = as_text(g$strength),
    away_score     = as_text(g$awayScore),
    home_score     = as_text(g$homeScore),
    highlight_clip = clip,
    sharing_url    = sharing,
    player_url     = player,
    clip_url       = if (!is.na(sharing)) sharing else player,
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

  if (length(rows) == 0) return(NULL)

  do.call(rbind, rows)
}

# ------------------------------------------------------------
# GET GOALS FOR A DATE
# ------------------------------------------------------------

get_goals <- function(date) {

  ids <- get_started_game_ids(date)

  out <- lapply(ids, function(id) {

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
  })

  out <- Filter(Negate(is.null), out)

  if (length(out) == 0) return(NULL)

  do.call(rbind, out)
}

# ------------------------------------------------------------
# CSV SETTINGS
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

# Internal columns used for processing, not published.
INTERNAL_COLS <- c(
  "highlight_clip",
  "sharing_url"
)

# Unique goal identifier.
goal_key <- function(df) {
  paste(
    df$game_id,
    df$period,
    df$time_in_period,
    df$team
  )
}

# ------------------------------------------------------------
# UPGRADE EXISTING CSV
#
# Adds player_url to an older CSV without losing history.
# ------------------------------------------------------------

upgrade_existing_csv <- function() {

  if (!file.exists(OUT_FILE)) return(invisible())

  old <- read.csv(
    OUT_FILE,
    colClasses = "character",
    stringsAsFactors = FALSE
  )

  changed <- FALSE

  for (col in CSV_COLS) {

    if (!col %in% names(old)) {

      old[[col]] <- NA_character_
      changed <- TRUE

      message("Added missing CSV column: ", col)
    }
  }

  if (!identical(names(old), CSV_COLS)) {

    old <- old[, CSV_COLS, drop = FALSE]
    changed <- TRUE
  }

  if (changed) {

    write.csv(
      old,
      OUT_FILE,
      row.names = FALSE,
      na = ""
    )

    message("Existing CSV successfully upgraded.")
  }
}

# Upgrade the existing CSV immediately when the script starts.
upgrade_existing_csv()

# ------------------------------------------------------------
# SAVE LOCAL CSV
# ------------------------------------------------------------

save_csv <- function(out) {

  if (nrow(out) == 0) return(invisible())

  if (file.exists(OUT_FILE)) {

    old <- read.csv(
      OUT_FILE,
      colClasses = "character",
      stringsAsFactors = FALSE
    )

    for (col in CSV_COLS) {

      if (!col %in% names(old)) {
        old[[col]] <- NA_character_
      }
    }

    old <- old[, CSV_COLS, drop = FALSE]

    # Remove goals already in the CSV.
    out <- out[
      !goal_key(out) %in% goal_key(old),
      ,
      drop = FALSE
    ]

    if (nrow(out) == 0) return(invisible())

    combined <- rbind(old, out)

  } else {

    combined <- out[, CSV_COLS, drop = FALSE]
  }

  write.csv(
    combined,
    OUT_FILE,
    row.names = FALSE,
    na = ""
  )

  message(
    "CSV updated: ",
    nrow(combined),
    " total records."
  )
}

# ------------------------------------------------------------
# OPTIONAL GITHUB API FUNCTIONS
# ------------------------------------------------------------

gh_req <- function(url, accept = "application/vnd.github+json") {

  request(url) |>
    req_auth_bearer_token(GH_TOKEN) |>
    req_headers(
      Accept = accept,
      `X-GitHub-Api-Version` = "2022-11-28"
    ) |>
    req_user_agent("nhl-highlight-clips-r/0.1") |>
    req_error(is_error = \(r) FALSE)
}

gh_message <- function(resp) {

  tryCatch(
    as.character(resp_body_json(resp)$message),
    error = function(e) ""
  )
}

csv_bytes <- function(df) {

  tmp <- tempfile(fileext = ".csv")

  on.exit(unlink(tmp))

  write.csv(
    df,
    tmp,
    row.names = FALSE,
    na = "",
    fileEncoding = "UTF-8"
  )

  readBin(tmp, "raw", file.size(tmp))
}

# ------------------------------------------------------------
# SYNC CSV WITH GITHUB API
# ------------------------------------------------------------

github_sync_once <- function(out) {

  url <- paste0(
    GH_API,
    "/repos/",
    GH_REPO,
    "/contents/",
    GH_PATH
  )

  with_ref <- function(req) {
    if (nzchar(GH_BRANCH)) {
      req_url_query(req, ref = GH_BRANCH)
    } else {
      req
    }
  }

  meta <- with_ref(gh_req(url)) |>
    req_perform()

  sha <- NULL
  combined <- out

  if (resp_status(meta) == 200) {

    sha <- resp_body_json(meta)$sha

    raw <- with_ref(
      gh_req(url, "application/vnd.github.raw+json")
    ) |>
      req_perform()

    if (resp_status(raw) != 200) {
      stop(
        "Could not read ",
        GH_PATH,
        " (HTTP ",
        resp_status(raw),
        ")"
      )
    }

    csv_text <- resp_body_string(
      raw,
      encoding = "UTF-8"
    )

    if (nzchar(trimws(csv_text))) {

      old <- read.csv(
        text = csv_text,
        colClasses = "character",
        encoding = "UTF-8",
        stringsAsFactors = FALSE
      )

      # Upgrade older GitHub CSV files too.
      for (col in CSV_COLS) {

        if (!col %in% names(old)) {
          old[[col]] <- NA_character_
        }
      }

      old <- old[, CSV_COLS, drop = FALSE]

      out <- out[
        !goal_key(out) %in% goal_key(old),
        ,
        drop = FALSE
      ]

      if (nrow(out) == 0) return(0L)

      combined <- rbind(old, out)
    }

  } else if (resp_status(meta) != 404) {

    stop(
      "HTTP ",
      resp_status(meta),
      " ",
      gh_message(meta)
    )
  }

  body <- list(
    message = sprintf(
      "Add %d power play goal(s): %s",
      nrow(out),
      paste(unique(out$game_date), collapse = ", ")
    ),
    content = jsonlite::base64_enc(
      csv_bytes(combined)
    )
  )

  if (!is.null(sha)) {
    body$sha <- sha
  }

  if (nzchar(GH_BRANCH)) {
    body$branch <- GH_BRANCH
  }

  put <- gh_req(url) |>
    req_method("PUT") |>
    req_body_json(body) |>
    req_perform()

  status <- resp_status(put)

  if (status %in% c(200, 201)) {
    return(nrow(out))
  }

  if (status %in% c(409, 422)) {
    return(NA_integer_)
  }

  hint <- if (status == 404) {
    " - check GH_REPO and token permissions"
  } else {
    ""
  }

  stop(
    "HTTP ",
    status,
    " ",
    gh_message(put),
    hint
  )
}

save_github <- function(out) {

  if (GH_REPO == "" || nrow(out) == 0) {
    return(invisible())
  }

  tryCatch({

    if (GH_TOKEN == "") {
      stop("GH_TOKEN is not set")
    }

    n <- github_sync_once(out)

    if (is.na(n)) {
      n <- github_sync_once(out)
    }

    if (is.na(n)) {
      stop("The file kept changing; will retry next pass")
    }

    if (n > 0) {
      message(
        sprintf(
          "Added %d rows to GitHub: %s/%s",
          n,
          GH_REPO,
          GH_PATH
        )
      )
    }

  }, error = function(e) {

    message(
      "GitHub update failed: ",
      conditionMessage(e)
    )
  })
}

# ------------------------------------------------------------
# SAVE NEW CLIPS
# ------------------------------------------------------------

save_new <- function(clips) {

  if (nrow(clips) == 0) return(invisible())

  out <- clips[, CSV_COLS, drop = FALSE]

  save_csv(out)

  save_github(out)
}

# ------------------------------------------------------------
# DISPLAY CLIPS
# ------------------------------------------------------------

show_clips <- function(clips) {

  for (i in seq_len(nrow(clips))) {

    with(clips[i, ], {

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
          clip_url
        )
      )
    })
  }
}

# ------------------------------------------------------------
# MAIN PROGRAM
# ------------------------------------------------------------

args <- commandArgs(trailingOnly = TRUE)

watch <- "--watch" %in% args

pp_only <- !("--all" %in% args)

date_arg <- args[
  grepl("^\\d{4}-\\d{2}-\\d{2}$", args)
]

# Default date: yesterday in Toronto time.
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

    # Only save goals that have a highlight clip.
    clips <- goals[
      !is.na(goals$highlight_clip),
      ,
      drop = FALSE
    ]

    message(
      sprintf(
        "%s: %d goals total%s; %d clips ready, %d waiting for a clip.",
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

    if (nrow(clips) > 0) {

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

      save_new(clips)

      seen_keys <- union(seen_keys, keys)
    }
  }

  if (!watch) break

  Sys.sleep(POLL_SECONDS)
}

message("NHL power-play clip collection finished.")
