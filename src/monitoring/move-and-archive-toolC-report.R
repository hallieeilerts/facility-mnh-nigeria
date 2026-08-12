################################################################################
#' @description Transfer RMD report to OneDrive folder, archiving previous version
################################################################################
#' Clear environment
rm(list = ls())
################################################################################

# Configure ---------------------------------------------------------------
out_dir     <- "./gen/monitoring/"
report_date <- format(Sys.Date(), "%Y%m%d")
out_file    <- sprintf("monitoring-toolC_%s.docx", report_date)
rendered_path <- file.path(out_dir, out_file)

onedrive_dqr_dir <- file.path(
  "C:/Users/HEilerts/OneDrive - Johns Hopkins",
  "WEST AFRICA MNH EVALUATIONS - Documents",
  "Facility MNH Innovations - Nigeria",
  "_IRB Binders - Shared Access",
  "Baseline and POM IRB Binder - Shared Access",
  "13_Data",
  "Tool C",
  "DQ reports"
)
onedrive_archive_dir <- file.path(onedrive_dqr_dir, "Archive")

# Fail if today's report hasn't been rendered yet ---------------------

if (!file.exists(rendered_path)) {
  stop("No rendered report found at ", rendered_path, 
       " — run the render script first.")
}

if (!dir.exists(onedrive_dqr_dir))     dir.create(onedrive_dqr_dir, recursive = TRUE)
if (!dir.exists(onedrive_archive_dir)) dir.create(onedrive_archive_dir, recursive = TRUE)

# Archive whatever's currently in DQ reports, except today's own file -------

existing_dqr <- list.files(onedrive_dqr_dir, full.names = TRUE)
existing_dqr <- existing_dqr[!file.info(existing_dqr)$isdir]  # exclude Archive/ itself
to_archive_dqr <- existing_dqr[!grepl(report_date, basename(existing_dqr), fixed = TRUE)]

if (length(to_archive_dqr) > 0) {
  ok <- file.copy(to_archive_dqr, file.path(onedrive_archive_dir, basename(to_archive_dqr)), overwrite = TRUE)
  if (all(ok)) {
    file.remove(to_archive_dqr)
    message(sprintf("Archived %d previous DQ report(s) on OneDrive", length(to_archive_dqr)))
  } else {
    warning(sprintf("Failed to archive: %s", paste(basename(to_archive_dqr)[!ok], collapse = ", ")))
  }
}

# Copy today's rendered report, renamed to the OneDrive convention ----------

onedrive_name <- sprintf("Tool C_DQR_%s.docx", report_date)
onedrive_path <- file.path(onedrive_dqr_dir, onedrive_name)

ok <- file.copy(rendered_path, onedrive_path, overwrite = TRUE)
if (ok) {
  message(sprintf("Report transferred to %s", onedrive_path))
} else {
  warning("Failed to transfer report to OneDrive")
}
