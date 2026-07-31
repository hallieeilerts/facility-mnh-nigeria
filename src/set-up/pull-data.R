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
#' Inputs
################################################################################

csv_resp <- request(Sys.getenv("ODK_URL_ToolA")) %>%
  req_url_query(groupPaths = "false", splitSelectMultiples = "false") %>%
  req_auth_basic(Sys.getenv("ODK_USER"), Sys.getenv("ODK_PASSWORD")) %>%
  req_perform()
dat <- read_csv(resp_body_string(csv_resp), show_col_types = FALSE)
write.csv(dat, "./data/facilityMNCH-toolA.csv", row.names = FALSE)
saveRDS(dat, "./data/facilityMNCH-toolA.rds")


base_url <- Sys.getenv("ODK_URL_ToolC")
# insert /v1 right after the domain
zip_url <- sub("(https://[^/]+)(/.*)", "\\1/v1\\2", base_url)
zip_url <- paste0(sub("\\.zip$", "", zip_url), ".zip")
zip_url
#zip_url <- paste0(sub("\\.zip$", "", Sys.getenv("ODK_URL_ToolC")), ".zip")
resp <- request(zip_url) %>%
  req_url_query(groupPaths = "false", splitSelectMultiples = "false") %>%
  req_auth_basic(Sys.getenv("ODK_USER"), Sys.getenv("ODK_PASSWORD")) %>%
  req_perform()
tmp <- tempfile(fileext = ".zip")
writeBin(resp_body_raw(resp), tmp)
exdir <- tempfile(); dir.create(exdir)
files <- unzip(tmp, exdir = exdir)
basename(files) 
csv_files <- files[grepl("\\.csv$", files, ignore.case = TRUE)]
tables <- setNames(
  lapply(csv_files, readr::read_csv, show_col_types = FALSE),
  tools::file_path_sans_ext(basename(csv_files))
)
str(tables, max.level = 1)
saveRDS(tables, "./data/facilityMNCH-toolC.rds")

