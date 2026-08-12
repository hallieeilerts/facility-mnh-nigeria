################################################################################
#' @description Move today's pulled data and metadata from local data folder
#' into the shared OneDrive Raw data folders, archiving whatever was previously there.
#' @return 
################################################################################
rm(list = ls())
library(dplyr)
################################################################################

# Configure ---------------------------------------------------------------

# location of data stored locally
local_path <- "./data/"

# location of one drive folder
onedrive_base <- file.path(
  "C:/Users/HEilerts/OneDrive - Johns Hopkins",
  "WEST AFRICA MNH EVALUATIONS - Documents",
  "Facility MNH Innovations - Nigeria",
  "_IRB Binders - Shared Access",
  "Baseline and POM IRB Binder - Shared Access",
  "13_Data"
)

# form_slug must match what the pull script uses for local filenames
# onedrive_folder must match the literal folder name on OneDrive (has spaces)
# tool is short name for use in R
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
  
  raw_dir     <- file.path(onedrive_base, onedrive_folder, "Raw data")
  archive_dir <- file.path(raw_dir, "Archive")
  
  if (!dir.exists(raw_dir))     dir.create(raw_dir, recursive = TRUE)
  if (!dir.exists(archive_dir)) dir.create(archive_dir, recursive = TRUE)
  
  # archive whatever is currently sitting in Raw data, except today's own file
  existing <- list.files(raw_dir, full.names = TRUE)
  existing <- existing[!file.info(existing)$isdir]   # exclude the Archive folder itself
  
  today_str <- format(Sys.Date(), "%Y%m%d")
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
  
  # find today's local data + metadata file for this tool
  local_files <- list.files(local_path, pattern = paste0("^", form_slug, "-final_"), full.names = TRUE)
  local_files <- local_files[!file.info(local_files)$isdir]
  
  if (length(local_files) == 0) {
    warning(sprintf("[%s] No local file found matching %s-final_* — skipping transfer", tool, form_slug))
    return(invisible(NULL))
  }
  
  # copy new file(s) + sidecar json into Raw data
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
