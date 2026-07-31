################################################################################
#' @description Clean tool C - q27
#' Review/audit repeat group (dth_review_repeat)
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
# dat <- toolc_tables$pom_tool_c
# dat_q210 <- toolc_tables$`pom_tool_c-dth_review_repeat`
# dat_q237 <- toolc_tables$`pom_tool_c-q237a_repeat`
# dat_q249 <- toolc_tables$`pom_tool_c-MaternPeriDeathRepeat`
# dat_q254 <- toolc_tables$`pom_tool_c-q254oth_repeat`
dat <- toolc_tables$`hfe-tool-c`
dat_q27 <- toolc_tables$`hfe-tool-c-dth_review_repeat`
source(here("src", "monitoring", "helper-functions.R"))
################################################################################

# force character typing and join facility identifier
dat_q27 <- toolc_tables$`hfe-tool-c-dth_review_repeat` %>%
  mutate(across(everything(), as.character)) %>%
  left_join(dat %>% select(KEY, g03, q25a), by = c("PARENT_KEY" = "KEY"))

# Map get_death1 labels to q25a codes
death_code_map <- c(
  "Maternal deaths" = "1",
  "Stillbirths"     = "2",
  "Neonatal deaths" = "3",
  "Child deaths"    = "4"
)

dat_q27 <- dat_q27 %>%
  mutate(
    death_code = death_code_map[get_death1],
    reported_in_q25a = !is.na(q25a) & !is.na(death_code) &
      mapply(function(codes, code) {
        grepl(paste0("(^|\\s)", code, "(\\s|$)"), codes)
      }, q25a, death_code)
  )

# Conditions

# Who participates in reviews
cond_q27a <- (is.na(dat_q27$q27a) | trimws(dat_q27$q27a) == "") & dat_q27$reported_in_q25a
cond_q27ango <- !is.na(dat_q27$q27a) & grepl("(^|\\s)17(\\s|$)", dat_q27$q27a) & (is.na(dat_q27$q27ango) | trimws(dat_q27$q27ango) == "") & dat_q27$reported_in_q25a
cond_q27anun <- !is.na(dat_q27$q27a) & grepl("(^|\\s)18(\\s|$)", dat_q27$q27a) & (is.na(dat_q27$q27anun) | trimws(dat_q27$q27anun) == "") & dat_q27$reported_in_q25a
cond_q27anlmoh <- !is.na(dat_q27$q27a) & grepl("(^|\\s)19(\\s|$)", dat_q27$q27a) & (is.na(dat_q27$q27anlmoh) | trimws(dat_q27$q27anlmoh) == "") & dat_q27$reported_in_q25a
cond_q27ansmoh <- !is.na(dat_q27$q27a) & grepl("(^|\\s)20(\\s|$)", dat_q27$q27a) & (is.na(dat_q27$q27ansmoh) | trimws(dat_q27$q27ansmoh) == "") & dat_q27$reported_in_q25a
cond_q27annmoh <- !is.na(dat_q27$q27a) & grepl("(^|\\s)21(\\s|$)", dat_q27$q27a) & (is.na(dat_q27$q27annmoh) | trimws(dat_q27$q27annmoh) == "") & dat_q27$reported_in_q25a
cond_q27anoth <- !is.na(dat_q27$q27a) & grepl("(^|\\s)96(\\s|$)", dat_q27$q27a) & (is.na(dat_q27$q27anoth) | trimws(dat_q27$q27anoth) == "") & dat_q27$reported_in_q25a

# How frequently reviewed
cond_q27b <- (is.na(dat_q27$q27b) | trimws(dat_q27$q27b) == "") & dat_q27$reported_in_q25a
cond_q27both <- !is.na(dat_q27$q27b) & dat_q27$q27b == "96" & (is.na(dat_q27$q27both) | trimws(dat_q27$q27both) == "") & dat_q27$reported_in_q25a

# How many reviewed
cond_q27c <- (is.na(dat_q27$q27c) | trimws(dat_q27$q27c) == "") & dat_q27$reported_in_q25a
cond_q27csample <- !is.na(dat_q27$q27c) & dat_q27$q27c == "2" & (is.na(dat_q27$q27csample) | trimws(dat_q27$q27csample) == "") & dat_q27$reported_in_q25a
cond_q27coth <- !is.na(dat_q27$q27c) & dat_q27$q27c == "96" & (is.na(dat_q27$q27coth) | trimws(dat_q27$q27coth) == "") & dat_q27$reported_in_q25a

# Sources used to collect information
cond_q27d <- (is.na(dat_q27$q27d) | trimws(dat_q27$q27d) == "") & dat_q27$reported_in_q25a
cond_q27doth <- !is.na(dat_q27$q27d) & grepl("(^|\\s)96(\\s|$)", dat_q27$q27d) & (is.na(dat_q27$q27doth) | trimws(dat_q27$q27doth) == "") & dat_q27$reported_in_q25a

# Who compiles/collects information for review
# FLAG: Excel sheet's relevance column for q27engo-q27enoth references q27a (e.g. selected(${q27a},'17')), 
# but these fields sit under q27e ("who is responsible for collecting/compiling information")
# This looks like a copy-paste error in the form
# Code below is written checking q27e (but excel tool needs to be fixed)
# If validation results look off, revisit against q27a.
cond_q27e <- (is.na(dat_q27$q27e) | trimws(dat_q27$q27e) == "") & dat_q27$reported_in_q25a
cond_q27engo <- !is.na(dat_q27$q27e) & grepl("(^|\\s)17(\\s|$)", dat_q27$q27e) & (is.na(dat_q27$q27engo) | trimws(dat_q27$q27engo) == "") & dat_q27$reported_in_q25a
cond_q27enun <- !is.na(dat_q27$q27e) & grepl("(^|\\s)18(\\s|$)", dat_q27$q27e) & (is.na(dat_q27$q27enun) | trimws(dat_q27$q27enun) == "") & dat_q27$reported_in_q25a
cond_q27enlmoh <- !is.na(dat_q27$q27e) & grepl("(^|\\s)19(\\s|$)", dat_q27$q27e) & (is.na(dat_q27$q27enlmoh) | trimws(dat_q27$q27enlmoh) == "") & dat_q27$reported_in_q25a
cond_q27ensmoh <- !is.na(dat_q27$q27e) & grepl("(^|\\s)20(\\s|$)", dat_q27$q27e) & (is.na(dat_q27$q27ensmoh) | trimws(dat_q27$q27ensmoh) == "") & dat_q27$reported_in_q25a
cond_q27ennmoh <- !is.na(dat_q27$q27e) & grepl("(^|\\s)21(\\s|$)", dat_q27$q27e) & (is.na(dat_q27$q27ennmoh) | trimws(dat_q27$q27ennmoh) == "") & dat_q27$reported_in_q25a
cond_q27enoth <- !is.na(dat_q27$q27e) & grepl("(^|\\s)96(\\s|$)", dat_q27$q27e) & (is.na(dat_q27$q27enoth) | trimws(dat_q27$q27enoth) == "") & dat_q27$reported_in_q25a

# When last reviewed
cond_q27f <- (is.na(dat_q27$q27f) | trimws(dat_q27$q27f) == "") & dat_q27$reported_in_q25a

# Death type not reported in q25a, but review/audit data exists anyway
cond_neg_q27_notreported <- !dat_q27$reported_in_q25a & 
  (!is.na(dat_q27$q27a) | !is.na(dat_q27$q27b) | !is.na(dat_q27$q27c) | 
     !is.na(dat_q27$q27d) | !is.na(dat_q27$q27e) | !is.na(dat_q27$q27f))


check_defs_q27 <- tribble(
  ~cond_name,          ~question,           ~label,                              ~issue,
  "cond_q27a",         "q27a",              "Who participates in review",       "Who participates in review is blank (q27a)",
  "cond_q27ango",      "q27a / q27ango",    "NGO rep in review",                 "NGO/Managing Authority Rep selected (q27a=17) and q27ango is blank",
  "cond_q27anun",      "q27a / q27anun",    "UN rep in review",                  "UN Agency Rep selected (q27a=18) and q27anun is blank",
  "cond_q27anlmoh",    "q27a / q27anlmoh",  "LGA MOH rep in review",             "LGA MoH Rep selected (q27a=19) and q27anlmoh is blank",
  "cond_q27ansmoh",    "q27a / q27ansmoh",  "State MOH rep in review",           "State MoH Rep selected (q27a=20) and q27ansmoh is blank",
  "cond_q27annmoh",    "q27a / q27annmoh",  "National MOH rep in review",       "National MoH Rep selected (q27a=21) and q27annmoh is blank",
  "cond_q27anoth",     "q27a / q27anoth",   "Other rep in review",               "Other selected in review (q27a=96) and q27anoth is blank",
  
  "cond_q27b",         "q27b",              "How frequently reviewed",          "How frequently reviewed is blank (q27b)",
  "cond_q27both",      "q27b / q27both",    "How frequently reviewed other",    "Other selected for review frequency (q27b=96) and q27both is blank",
  
  "cond_q27c",         "q27c",              "How many reviewed",                "How many reviewed is blank (q27c)",
  "cond_q27csample",   "q27c / q27csample", "Sample size reviewed",             "Sample of deaths selected (q27c=2) and q27csample is blank",
  "cond_q27coth",      "q27c / q27coth",    "How many reviewed other",          "Other selected for how many reviewed (q27c=96) and q27coth is blank",
  
  "cond_q27d",         "q27d",              "Sources used to collect info",     "Sources used to collect info is blank (q27d)",
  "cond_q27doth",      "q27d / q27doth",    "Sources used to collect info other","Other selected for sources (q27d=96) and q27doth is blank",
  
  "cond_q27e",         "q27e",              "Who collects/compiles info",       "Who collects/compiles info is blank (q27e)",
  "cond_q27engo",      "q27e / q27engo",    "NGO rep collects info",             "NGO/Managing Authority Rep selected (q27e=17) and q27engo is blank",
  "cond_q27enun",      "q27e / q27enun",    "UN rep collects info",              "UN Agency Rep selected (q27e=18) and q27enun is blank",
  "cond_q27enlmoh",    "q27e / q27enlmoh",  "LGA MOH rep collects info",         "LGA MoH Rep selected (q27e=19) and q27enlmoh is blank",
  "cond_q27ensmoh",    "q27e / q27ensmoh",  "State MOH rep collects info",       "State MoH Rep selected (q27e=20) and q27ensmoh is blank",
  "cond_q27ennmoh",    "q27e / q27ennmoh",  "National MOH rep collects info",   "National MoH Rep selected (q27e=21) and q27ennmoh is blank",
  "cond_q27enoth",     "q27e / q27enoth",   "Other rep collects info",           "Other selected for collecting info (q27e=96) and q27enoth is blank",
  
  "cond_q27f",         "q27f",              "When last reviewed",               "When last reviewed is blank (q27f)",
  
  "cond_neg_q27_notreported", "q25a / q27a-f", "Review data for unreported death type", "Death type not selected in q25a but review/audit data exists for this repeat instance"
)

conditions_q27 <- mget(check_defs_q27$cond_name)

section27_results <- check_defs_q27 %>%
  pmap_dfr(function(cond_name, question, label, issue) {
    flagged <- which(conditions_q27[[cond_name]])
    if (length(flagged) == 0) return(NULL)
    tibble(
      KEY = dat_q27$KEY[flagged],
      g03 = dat_q27$g03[flagged],
      get_death1 = dat_q27$get_death1[flagged],
      question       = question,
      label          = label,
      issue          = issue
    )
  })

