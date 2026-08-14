################################################################################
#' @description Pull data from multiple ODK tools, save with date suffix,
#'record metadata about each pull (JSON sidecar + running log), archive in shared data folder
#' @return 
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
library(dplyr)
library(httr2)
library(jsonlite)
library(digest)   # for sha256
################################################################################

# For scheduled task ------------------------------------------------------

# In order to run scheduled task, need to hard code path (don't rely on relative path)
mypath <- "C:/Users/HEilerts/Institute of International Programs Dropbox/Hallie Eilerts-Spinelli/MNHeval-HealthFacility/facility-mnh-nigeria/data/"

# Explicitly load Renviron variables
readRenviron("C:/Users/HEilerts/Institute of International Programs Dropbox/Hallie Eilerts-Spinelli/MNHeval-HealthFacility/facility-mnh-nigeria/.Renviron")

# Configure ---------------------------------------------------------------

if (!dir.exists(mypath)) dir.create(mypath, recursive = TRUE)
log_path <- file.path(mypath, "pull_log.jsonl")

pull_date      <- format(Sys.Date(), "%Y-%m-%d")   # for metadata fields
pull_date_file <- format(Sys.Date(), "%Y%m%d")     # for filenames, matches OneDrive convention

# Short tool name
# Environment variable name for the URL
# name needed to get from the base .csv URL to the "final" .csv.zip URL
tools <- tibble::tribble(
  ~tool,     ~env_var,             ~form_slug,
  "ToolA",   "ODK_URL_ToolA",      "hfe-tool-a",
  "ToolB",   "ODK_URL_ToolB",      "hfe-tool-b",
  "ToolC",   "ODK_URL_ToolC",      "hfe-tool-c",
  "ToolD",   "ODK_URL_ToolD",      "hfe-tool-d",
  "ToolR1",  "ODK_URL_ToolR1",     "hfe-tool-r1",
  "ToolR2",  "ODK_URL_ToolR2",     "hfe-rapid-r2"
)


# Function to pull, save with date suffix, return metadata -----

fn_pull_tool <- function(tool, env_var, form_slug, mypath, pull_date, pull_date_file) {
  
  base_url <- Sys.getenv(env_var)
  if (identical(base_url, "")) {
    warning(sprintf("Env var %s is empty — skipping %s", env_var, tool))
    return(NULL)
  }
  
  zip_url <- base_url %>%
    sub(sprintf("forms/%s/", form_slug), sprintf("forms/%s-final/", form_slug), .) %>%
    sub("\\.csv$", ".csv.zip", .)
  
  req_time <- Sys.time()
  
  resp <- tryCatch(
    request(zip_url) %>%
      req_url_query(groupPaths = "false", splitSelectMultiples = "false") %>%
      req_auth_basic(Sys.getenv("ODK_USER"), Sys.getenv("ODK_PASSWORD")) %>%
      req_perform(),
    error = function(e) e
  )
  
  if (inherits(resp, "error")) {
    clean_msg <- gsub("\033\\[[0-9;]*m", "", conditionMessage(resp))
    warning(sprintf("Failed to pull %s: %s", tool, clean_msg))
    return(list(
      tool = tool,
      pulled_at = format(req_time, "%Y-%m-%dT%H:%M:%S%z"),
      source_url = zip_url,
      status = "error",
      error_message = clean_msg
    ))
  }
  
  # Figure out if the form is a zip file or regular
  content_type <- resp_header(resp, "content-type") %||% NA_character_
  is_zip <- grepl("zip", content_type, ignore.case = TRUE) ||
    grepl("\\.zip($|\\?)", zip_url)
  
  ext <- if (is_zip) "csv.zip" else "csv"
  
  # File stem matches OneDrive convention: {form_slug}-final_{YYYYMMDD}
  file_stem <- sprintf("%s-final_%s", form_slug, pull_date_file)
  out_file  <- file.path(mypath, sprintf("%s.%s", file_stem, ext))
  
  writeBin(resp_body_raw(resp), out_file)
  
  meta <- list(
    tool          = tool,
    form_slug     = form_slug,
    pulled_at     = format(req_time, "%Y-%m-%dT%H:%M:%S%z"),
    source_url    = zip_url,
    status        = "success",
    http_status   = resp_status(resp),
    is_zip        = is_zip,
    output_file   = out_file,
    file_size_bytes = file.info(out_file)$size,
    sha256        = digest(out_file, algo = "sha256", file = TRUE),
    content_type  = content_type,
    etag          = resp_header(resp, "etag") %||% NA_character_,
    last_modified = resp_header(resp, "last-modified") %||% NA_character_,
    r_version     = R.version.string,
    httr2_version = as.character(packageVersion("httr2"))
  )
  
  # sidecar metadata file next to the data — same stem, .meta.json
  sidecar_file <- file.path(mypath, sprintf("%s.meta.json", file_stem))
  write_json(meta, sidecar_file, auto_unbox = TRUE, pretty = TRUE)
  
  meta
}


# Function to archive old local copies before today's pull --------------------

fn_archive_old_local <- function(form_slug, mypath, pull_date_file) {
  archive_dir <- file.path(mypath, "archive")
  if (!dir.exists(archive_dir)) dir.create(archive_dir, recursive = TRUE)
  
  # data + sidecar files for this tool sitting directly in mypath
  existing <- list.files(mypath, pattern = paste0("^", form_slug, "-final_"), full.names = TRUE)
  
  # don't archive anything from today's pull if this is somehow re-run
  to_move <- existing[!grepl(pull_date_file, basename(existing), fixed = TRUE)]
  
  if (length(to_move) > 0) {
    file.rename(to_move, file.path(archive_dir, basename(to_move)))
    message(sprintf("Archived %d old local file(s) for %s", length(to_move), form_slug))
  }
}

# Pull to local and archive, append each result to JSON log --------------------------

results <- purrr::pmap(tools, function(tool, env_var, form_slug) {
  
  message(sprintf("Pulling %s...", tool))
  
  fn_archive_old_local(form_slug, mypath, pull_date_file)
  
  meta <- fn_pull_tool(tool, env_var, form_slug, mypath, pull_date, pull_date_file)
  
  if (!is.null(meta)) {
    cat(toJSON(meta, auto_unbox = TRUE), "\n", file = log_path, append = TRUE)
    
    if (meta$status == "success") {
      message(sprintf("  ✓ %s: success", tool))
    } else {
      message(sprintf("  ✗ %s: error — %s", tool, meta$error_message))
    }
  } else {
    message(sprintf("  ✗ %s: skipped (missing env var)", tool))
  }
  
  meta
})

names(results) <- tools$tool


# View data pull log ------------------------------------------------------

# # safe load
# if (file.exists(log_path) && file.info(log_path)$size > 0) {
#   df_pull_log <- stream_in(file(log_path))
# } else {
#   message("No pull log yet — skipping log review.")
# }
# # df_pull_log <- stream_in(file(log_path))
# 
# # View errors
# df_pull_log %>%
#   filter(status == "error")   
# 
# # View today's pulls
# df_pull_log %>%
#   filter(pulled_at >= pull_date)


# Ad-hoc edit to pull log -------------------------------------------------

# # Read every line as a raw string first
# # so we can filter without needing rectangular data frame (in case columns differ across rows)
# log_lines <- readLines(log_path)
# log_list  <- purrr::map(log_lines, jsonlite::fromJSON)
# 
# # Keep only pulls at/after a cutoff time (after my first real final run)
# cutoff <- "2026-08-12T09:41:00-0400"
# keep <- purrr::map_lgl(log_list, ~ .x$pulled_at >= cutoff)
# 
# log_lines_clean <- log_lines[keep]
# 
# writeLines(log_lines_clean, log_path)

