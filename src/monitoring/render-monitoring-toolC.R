################################################################################
#' @description Render the Tool C monitoring report RMD file to gen/monitoring/, 
#' (Adds a date suffix and archives the previous version first)
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
library(rmarkdown)
################################################################################

# Configure ---------------------------------------------------------------

rmd_path    <- "./src/monitoring/monitoring-toolC.Rmd"
out_dir     <- "./gen/monitoring/"
archive_dir <- file.path(out_dir, "archive")

if (!dir.exists(out_dir))     dir.create(out_dir, recursive = TRUE)
if (!dir.exists(archive_dir)) dir.create(archive_dir, recursive = TRUE)

report_date <- format(Sys.Date(), "%Y%m%d")
out_file    <- sprintf("monitoring-toolC_%s.docx", report_date)

# Archive any existing report(s) in gen/monitoring before rendering -------

existing <- list.files(out_dir, pattern = "^monitoring-toolC_.*\\.docx$", full.names = TRUE)
existing <- existing[!file.info(existing)$isdir]  # exclude archive/ itself

# Don't archive a report from today's date if this is a same-day re-render
to_archive <- existing[!grepl(report_date, basename(existing), fixed = TRUE)]

if (length(to_archive) > 0) {
  file.rename(to_archive, file.path(archive_dir, basename(to_archive)))
  message(sprintf("Archived %d previous report(s)", length(to_archive)))
}

# Render --------------------------------------------------------------------

rmarkdown::render(
  input       = rmd_path,
  output_file = out_file,
  output_dir  = out_dir
)

message(sprintf("Report rendered to %s", file.path(out_dir, out_file)))

