################################################################################
#' @description Clean tool C - MaternPeriDeathRepeat
#' Review/audit repeat group (MaternPeriDeathRepeat)
#' Lives in a separate ODK repeat table, joined back to main data via PARENT_KEY
#' @return 
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
library(dplyr)
library(tidyr)
library(here)
library(kableExtra)
library(stringr)
library(purrr)
#' Inputs
toolc_tables <- readRDS("./data/facilityMNCH-toolC.rds")
dat <- toolc_tables$`hfe-tool-c`
dat_q27 <- toolc_tables$`hfe-tool-c-MaternPeriDeathRepeat`
source(here("src", "monitoring", "helper-functions.R"))
################################################################################


dat_outcomes <- toolc_tables$`hfe-tool-c-MaternPeriDeathRepeat` %>%
  left_join(dat %>% select(KEY, g03), by = c("PARENT_KEY" = "KEY"))

# Missing monthly counts (every row = a selected outcome, so all 12 months
cond_m1 <- is.na(dat_outcomes$m1)
cond_m2 <- is.na(dat_outcomes$m2)
cond_m3 <- is.na(dat_outcomes$m3)
cond_m4 <- is.na(dat_outcomes$m4)
cond_m5 <- is.na(dat_outcomes$m5)
cond_m6 <- is.na(dat_outcomes$m6)
cond_m7 <- is.na(dat_outcomes$m7)
cond_m8 <- is.na(dat_outcomes$m8)
cond_m9 <- is.na(dat_outcomes$m9)
cond_m10 <- is.na(dat_outcomes$m10)
cond_m11 <- is.na(dat_outcomes$m11)
cond_m12 <- is.na(dat_outcomes$m12)

check_defs_outcomes <- tribble(
  ~cond_name, ~question, ~label,              ~issue,
  "cond_m1",  "m1",      "Jul-25 count",      "Jul-25 count is blank (m1)",
  "cond_m2",  "m2",      "Aug-25 count",      "Aug-25 count is blank (m2)",
  "cond_m3",  "m3",      "Sep-25 count",      "Sep-25 count is blank (m3)",
  "cond_m4",  "m4",      "Oct-25 count",      "Oct-25 count is blank (m4)",
  "cond_m5",  "m5",      "Nov-25 count",      "Nov-25 count is blank (m5)",
  "cond_m6",  "m6",      "Dec-25 count",      "Dec-25 count is blank (m6)",
  "cond_m7",  "m7",      "Jan-26 count",      "Jan-26 count is blank (m7)",
  "cond_m8",  "m8",      "Feb-26 count",      "Feb-26 count is blank (m8)",
  "cond_m9",  "m9",      "Mar-26 count",      "Mar-26 count is blank (m9)",
  "cond_m10", "m10",     "Apr-26 count",      "Apr-26 count is blank (m10)",
  "cond_m11", "m11",     "May-26 count",      "May-26 count is blank (m11)",
  "cond_m12", "m12",     "Jun-26 count",      "Jun-26 count is blank (m12)"
)

conditions_outcomes <- mget(check_defs_outcomes$cond_name)

section_outcomes_results <- check_defs_outcomes %>%
  pmap_dfr(function(cond_name, question, label, issue) {
    flagged <- which(conditions_outcomes[[cond_name]])
    if (length(flagged) == 0) return(NULL)
    tibble(
      KEY = dat_outcomes$KEY[flagged],
      g03 = dat_outcomes$g03[flagged],
      get_outcome = dat_outcomes$get_outcome[flagged],
      question    = question,
      label       = label,
      issue       = issue
    )
  })
