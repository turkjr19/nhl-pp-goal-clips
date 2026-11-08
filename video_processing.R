
# ============================================================
# Hockey Video Processing
# Reads player_url from pp_goal_clips.csv
# Downloads each unique Brightcove video
# Trims to the final 100 seconds
# Saves as videos/VIDEO_ID.mp4
# ============================================================

# ------------------------------------------------------------
# SETTINGS
# ------------------------------------------------------------

CSV_FILE <- "pp_goal_clips.csv"
OUTPUT_FOLDER <- "videos"

MAX_DURATION <- 100
MAXIMUM_SIZE <- 100 * 1000 * 1000

YT_DLP <- "yt-dlp"
FFMPEG <- "ffmpeg"
FFPROBE <- "ffprobe"

dir.create(OUTPUT_FOLDER, showWarnings = FALSE)

# ------------------------------------------------------------
# EXTRACT BRIGHTCOVE VIDEO ID
# ------------------------------------------------------------

get_video_id <- function(url) {

  match <- regmatches(
    url,
    regexpr("[?&]videoId=[^&]+", url)
  )

  if (length(match) == 0 || !nzchar(match)) {
    return(NA_character_)
  }

  sub("^[?&]videoId=", "", match)
}

# ------------------------------------------------------------
# GET VIDEO DURATION
# ------------------------------------------------------------

get_duration <- function(file) {

  result <- system2(
    FFPROBE,
    args = c(
      "-v", "error",
      "-show_entries", "format=duration",
      "-of", "default=noprint_wrappers=1:nokey=1",
      file
    ),
    stdout = TRUE,
    stderr = TRUE
  )

  as.numeric(result[1])
}

# ------------------------------------------------------------
# PROCESS ONE VIDEO
# ------------------------------------------------------------

process_video <- function(url, video_id) {

  output_file <- file.path(
    OUTPUT_FOLDER,
    paste0(video_id, ".mp4")
  )

  # Skip existing videos
  if (file.exists(output_file)) {
    message("Already downloaded: ", video_id)
    return(invisible(NULL))
  }

  message("----------------------------------------")
  message("Processing video: ", video_id)
  message("URL: ", url)

  temp_folder <- tempfile("hockey_video_")
  dir.create(temp_folder)

  on.exit(
    unlink(temp_folder, recursive = TRUE),
    add = TRUE
  )

  download_template <- file.path(
    temp_folder,
    "source.%(ext)s"
  )

  # ----------------------------------------------------------
  # DOWNLOAD
  # ----------------------------------------------------------

  message("Downloading video...")

  download_result <- system2(
    YT_DLP,
    args = c(
      "--quiet",
      "--no-warnings",
      "--no-playlist",
      "--retries", "5",
      "--fragment-retries", "5",
      "--force-overwrites",
      "--merge-output-format", "mp4",
      "--remux-video", "mp4",
      "-o", download_template,
      url
    ),
    stdout = TRUE,
    stderr = TRUE
  )

  source_files <- list.files(
    temp_folder,
    pattern = "^source\\.",
    full.names = TRUE
  )

  if (length(source_files) == 0) {
    stop("Download failed for video: ", video_id)
  }

  source_file <- source_files[1]

  # ----------------------------------------------------------
  # CHECK DURATION
  # ----------------------------------------------------------

  duration <- get_duration(source_file)

  if (is.na(duration) || duration <= 0) {
    stop("Could not determine video duration: ", video_id)
  }

  message("Original duration: ", round(duration, 1), " seconds")

  # ----------------------------------------------------------
  # TRIM TO LAST 100 SECONDS
  # ----------------------------------------------------------

  start_time <- max(0, duration - MAX_DURATION)

  message("Trimming video...")

  trim_result <- system2(
    FFMPEG,
    args = c(
      "-y",
      "-ss", as.character(start_time),
      "-i", source_file,
      "-t", as.character(MAX_DURATION),
      "-c:v", "libx264",
      "-preset", "fast",
      "-crf", "23",
      "-c:a", "aac",
      "-movflags", "+faststart",
      output_file
    ),
    stdout = TRUE,
    stderr = TRUE
  )

  if (!file.exists(output_file)) {
    stop("FFmpeg failed to create video: ", video_id)
  }

  # ----------------------------------------------------------
  # CHECK FILE SIZE
  # ----------------------------------------------------------

  file_size <- file.info(output_file)$size

  message(
    "Processed size: ",
    round(file_size / 1000000, 2),
    " MB"
  )

  if (file_size > MAXIMUM_SIZE) {
    unlink(output_file)

    stop(
      "Video exceeds GitHub's 100 MB file limit: ",
      video_id
    )
  }

  message("SUCCESS: ", output_file)

  invisible(output_file)
}

# ============================================================
# MAIN
# ============================================================

message("Reading CSV: ", CSV_FILE)

clips <- read.csv(
  CSV_FILE,
  stringsAsFactors = FALSE
)

if (!"player_url" %in% names(clips)) {
  stop("CSV does not contain a player_url column")
}

# Remove empty URLs and duplicates
urls <- unique(
  clips$player_url[
    !is.na(clips$player_url) &
      nzchar(trimws(clips$player_url))
  ]
)

message("Unique video URLs found: ", length(urls))

# Extract video IDs
video_ids <- vapply(
  urls,
  get_video_id,
  character(1)
)

# Remove URLs without a recognizable video ID
valid <- !is.na(video_ids)

urls <- urls[valid]
video_ids <- video_ids[valid]

if (length(urls) == 0) {
  stop("No valid Brightcove video URLs found")
}

# ------------------------------------------------------------
# PROCESS ALL VIDEOS
# ------------------------------------------------------------

success <- 0
failed <- 0

for (i in seq_along(urls)) {

  message("")
  message("VIDEO ", i, " OF ", length(urls))

  tryCatch({

    process_video(
      url = urls[i],
      video_id = video_ids[i]
    )

    success <- success + 1

  }, error = function(e) {

    failed <<- failed + 1

    message(
      "FAILED: ",
      video_ids[i],
      " - ",
      conditionMessage(e)
    )

  })
}

# ------------------------------------------------------------
# SUMMARY
# ------------------------------------------------------------

message("")
message("========================================")
message("VIDEO PROCESSING COMPLETE")
message("Successful or already present: ", success)
message("Failed: ", failed)
message("========================================")

if (failed > 0) {
  stop("One or more videos failed. See log above.")
}
