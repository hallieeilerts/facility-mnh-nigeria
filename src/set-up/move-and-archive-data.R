################################################################################
#' @description Move today's pulled data and metadata from local data folder
#' into the shared OneDrive Raw data folders, archiving whatever was previously there.
#' @return 
################################################################################
rm(list = ls())
library(dplyr)
################################################################################

# Configure ---------------------------------------------------------------

# In order to run scheduled task, need to hard code path (don't rely on relative path)
local_path <- "C:/Users/HEilerts/Institute of International Programs Dropbox/Hallie Eilerts-Spinelli/MNHeval-HealthFacility/facility-mnh-nigeria/data/"

onedrive_base <- file.path(
  "C:/Users/HEilerts/OneDrive - Johns Hopkins",
  "WEST AFRICA MNH EVALUATIONS - Documents",
  "Facility MNH Innovations - Nigeria",
  "_IRB Binders - Shared Access",
  "Baseline and POM IRB Binder - Shared Access",
  "13_Data"
)

tools <- tibble::tribble(
  ~tool,     ~form_slug,          ~onedrive_folder,
  "ToolA",   "hfe-tool-a",        "Tool A",
  "ToolB",   "hfe-tool-b",        "Tool B",
  "ToolC",   "hfe-tool-c",        "Tool C",
  "ToolD",   "hfe-tool-d",        "Tool D",
  "ToolR1",  "hfe-tool-r1",       "RAPID R1",
  "ToolR2",  "hfe-rapid-r2",      "RAPID R2"
)

# Function: transfer one tool's files to OneDrive, archiving what's there ----
fn_transfer_tool <- function(tool, form_slug, onedrive_folder, local_path, onedrive_base) {
  
  # 1. Check for today's local file
  #    Must match form_slug and today's date
  #    This ensures the pull-data.R did run successfully, and we want to move/archive to OneDrive
  today_str <- format(Sys.Date(), "%Y%m%d")
  local_files <- list.files(local_path, pattern = paste0("^", form_slug, "-final_", today_str), full.names = TRUE)
  local_files <- local_files[!file.info(local_files)$isdir]
  
  if (length(local_files) == 0) {
    warning(sprintf("[%s] No file from today found (%s-final_%s*) — skipping entirely (OneDrive untouched)", tool, form_slug, today_str))
    return(invisible(NULL))
  }
  
  # 2. Set up paths for OneDrive folders
  raw_dir     <- file.path(onedrive_base, onedrive_folder, "Raw data")
  archive_dir <- file.path(raw_dir, "Archive")
  
  if (!dir.exists(raw_dir))     dir.create(raw_dir, recursive = TRUE)
  if (!dir.exists(archive_dir)) dir.create(archive_dir, recursive = TRUE)
  
  # 3. Archive whatever is currently sitting in OneDrive Raw data, except today's own file
  existing <- list.files(raw_dir, full.names = TRUE)
  existing <- existing[!file.info(existing)$isdir]
  
  to_archive <- existing[!grepl(today_str, basename(existing), fixed = TRUE)]
  
  if (length(to_archive) > 0) {
    ok <- file.copy(to_archive, file.path(archive_dir, basename(to_archive)), overwrite = TRUE)
    if (all(ok)) {
      file.remove(to_archive)
      message(sprintf("[%s] Archived %d existing file(s)", tool, length(to_archive)))
    } else {
      warning(sprintf("[%s] Failed to archive: %s", tool,
                      paste(basename(to_archive)[!ok], collapse = ", ")))
    }
  }
  
  # 4. Copy new file(s) + sidecar json into Raw data
  ok <- file.copy(local_files, file.path(raw_dir, basename(local_files)), overwrite = TRUE)
  
  if (all(ok)) {
    message(sprintf("[%s] Transferred %d file(s) to %s", tool, length(local_files), raw_dir))
  } else {
    warning(sprintf("[%s] Failed to transfer: %s", tool,
                    paste(basename(local_files)[!ok], collapse = ", ")))
  }
  
  invisible(NULL)
}

# Run for all tools ---------------------------------------------------------
purrr::pwalk(tools, function(tool, form_slug, onedrive_folder) {
  fn_transfer_tool(tool, form_slug, onedrive_folder, local_path, onedrive_base)
})
