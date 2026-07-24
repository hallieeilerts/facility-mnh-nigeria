################################################################################
#' @description Pull data from API
#' @return 
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
#' Inputs
################################################################################

csv_resp <- request(Sys.getenv("ODK_URL_ToolA")) %>%
  req_url_query(groupPaths = "false", splitSelectMultiples = "false") %>%
  req_auth_basic(Sys.getenv("ODK_USER"), Sys.getenv("ODK_PASSWORD")) %>%
  req_perform()
dat <- read_csv(resp_body_string(csv_resp), show_col_types = FALSE)

write.csv(dat, "./data/facilityMNCH-toolA.csv", row.names = FALSE)
saveRDS(dat, "./data/facilityMNCH-toolA.rds")