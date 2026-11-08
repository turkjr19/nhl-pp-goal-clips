
# ============================================================
# NHL GOAL BOT - GITHUB VIDEO PROCESSING
# ============================================================

MAXIMUM_DURATION <- 100
MAXIMUM_SIZE <- 100000000

DOWNLOAD_RETRY_DELAY_SECONDS <- 10

DELETE_RETRY_ATTEMPTS <- 5
DELETE_RETRY_DELAY_SECONDS <- 0.5

YT_DLP <- Sys.which("yt-dlp")
FFMPEG <- Sys.which("ffmpeg")
FFPROBE <- Sys.which("ffprobe")

video_url <- Sys.getenv("VIDEO_URL")
video_id <- Sys.getenv("VIDEO_ID")

if (video_url == "") {
  stop("VIDEO_URL is missing")
}

if (video_id == "") {
  stop("VIDEO_ID is missing")
}

# Create videos folder if necessary
dir.create("videos", showWarnings = FALSE)

# Save using Brightcove video ID
filename <- file.path(
  "videos",
  paste0(video_id, ".mp4")
)

message("Output file: ", filename)

if (YT_DLP == "") stop("yt-dlp not found")
if (FFMPEG == "") stop("FFmpeg not found")
if (FFPROBE == "") stop("FFprobe not found")
if (video_url == "") stop("VIDEO_URL is missing")


# ------------------------------------------------------------
# DOWNLOAD
# ------------------------------------------------------------

download_file <- function(url, filename) {

  args <- c(
    "--quiet",
    "--no-warnings",
    "--no-playlist",
    "--retries", "3",
    "--fragment-retries", "3",
    "--force-overwrites",
    "--merge-output-format", "mp4",
    "--remux-video", "mp4",
    "-o", filename,
    url
  )

  result <- tryCatch({

    output <- system2(
      YT_DLP,
      args = shQuote(args),
      stdout = TRUE,
      stderr = TRUE
    )

    status <- attr(output, "status")
    if (is.null(status)) status <- 0

    status == 0 &&
      file.exists(filename) &&
      file.info(filename)$size > 0

  }, error = function(e) FALSE)

  result
}


download <- function(url, filename) {

  for (attempt in 1:5) {

    message("Download attempt ", attempt, " of 5")

    if (download_file(url, filename)) {
      message("Download successful")
      return(TRUE)
    }

    if (attempt < 5) {
      Sys.sleep(DOWNLOAD_RETRY_DELAY_SECONDS)
    }
  }

  stop("Download failed after five attempts")
}


# ------------------------------------------------------------
# VIDEO DURATION
# ------------------------------------------------------------

get_duration <- function(file) {

  result <- system2(
    FFPROBE,
    args = shQuote(c(
      "-v", "error",
      "-show_entries", "format=duration",
      "-of", "csv=p=0",
      file
    )),
    stdout = TRUE,
    stderr = FALSE
  )

  as.numeric(result[1])
}


# ------------------------------------------------------------
# VIDEO SIZE
# ------------------------------------------------------------

get_size <- function(file) {

  as.numeric(file.info(file)$size)
}


# ------------------------------------------------------------
# TRIM VIDEO
# ------------------------------------------------------------

trim <- function(file, start, end) {

  output_file <- sub(
    "\\.mp4$",
    "_trimmed.mp4",
    file,
    ignore.case = TRUE
  )

  args <- c(
    "-y",
    "-ss", as.character(start),
    "-i", file,
    "-t", as.character(end - start),
    "-map", "0",
    "-c", "copy",
    "-avoid_negative_ts", "make_zero",
    output_file
  )

  status <- system2(
    FFMPEG,
    args = shQuote(args),
    stdout = FALSE,
    stderr = FALSE
  )

  if (status != 0 || !file.exists(output_file)) {
    stop("Video trimming failed")
  }

  output_file
}


# ------------------------------------------------------------
# NORMALIZE VIDEO
# ------------------------------------------------------------

normalize_video <- function(file) {

  duration <- get_duration(file)

  if (duration > MAXIMUM_DURATION) {

    message("Trimming video to last 100 seconds")

    trimmed <- trim(
      file,
      duration - MAXIMUM_DURATION,
      duration
    )

    if (!file.copy(trimmed, file, overwrite = TRUE)) {
      stop("Unable to replace original video")
    }

    unlink(trimmed)
  }

  size <- get_size(file)

  if (size > MAXIMUM_SIZE) {

    message(
      "WARNING: Video exceeds 100 MB: ",
      size
    )
  }

  invisible(file)
}


# ------------------------------------------------------------
# READ VIDEO
# ------------------------------------------------------------

read_video <- function(file) {

  if (!file.exists(file)) return(NULL)

  con <- file(file, "rb")
  on.exit(close(con))

  readBin(
    con,
    what = "raw",
    n = file.info(file)$size
  )
}


# ------------------------------------------------------------
# DELETE VIDEO
# ------------------------------------------------------------

remove_video <- function(file) {

  for (attempt in 1:DELETE_RETRY_ATTEMPTS) {

    if (!file.exists(file)) return(invisible(NULL))

    if (unlink(file) == 0) {
      return(invisible(NULL))
    }

    if (attempt < DELETE_RETRY_ATTEMPTS) {
      Sys.sleep(DELETE_RETRY_DELAY_SECONDS)
    }
  }

  message("Unable to remove: ", file)

  invisible(NULL)
}


# ============================================================
# EXECUTION
# ============================================================

message("Starting NHL video download")

download(video_url, filename)

message("Original duration: ", get_duration(filename))

normalize_video(filename)

message("--------------------------------")
message("VIDEO PROCESSING COMPLETE")
message("--------------------------------")

message("File: ", filename)
message("Duration: ", get_duration(filename), " seconds")
message("Size: ", round(get_size(filename) / 1000000, 2), " MB")

message("Ready for upload to GitHub artifact")
