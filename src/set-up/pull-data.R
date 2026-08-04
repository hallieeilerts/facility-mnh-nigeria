################################################################################
#' @description Pull data from API
#' @return 
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
library(dplyr)
library(tidyr)
library(readr)
library(httr2) 
################################################################################

# set path
mypath <- "./data/"

# Build the submissions.csv.zip URL and download
zip_url <- Sys.getenv("ODK_URL_ToolC") %>%
  sub("forms/hfe-tool-c/", "forms/hfe-tool-c-final/", .) %>%
  sub("\\.csv$", ".csv.zip", .)

resp <- request(zip_url) %>%
  req_url_query(groupPaths = "false", splitSelectMultiples = "false") %>%
  req_auth_basic(Sys.getenv("ODK_USER"), Sys.getenv("ODK_PASSWORD")) %>%
  req_perform()

# Unzip and read every CSV into a named list of tibbles
exdir <- tempfile(); dir.create(exdir)
tmp <- tempfile(fileext = ".zip")
writeBin(resp_body_raw(resp), tmp)
csv_files <- unzip(tmp, exdir = exdir) %>% grep("\\.csv$", ., ignore.case = TRUE, value = TRUE)

tables <- setNames(
  lapply(csv_files, readr::read_csv, show_col_types = FALSE),
  tools::file_path_sans_ext(basename(csv_files))
)
str(tables, max.level = 1)

saveRDS(tables, paste0(mypath, "facilityMNCH-toolC.rds"))

