################################################################################
#' @description Clean tool C
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
library(openxlsx)
#' Inputs
toolc_tables <- readRDS("./data/facilityMNCH-toolC.rds")
dat <- toolc_tables$`hfe-tool-c`
source(here("src", "monitoring", "helper-functions.R"))
################################################################################

# overall summary table
dat %>%
  select(t_level1, level2, level3, t_f01, recruit, consent) %>%
  arrange(t_level1, level2) %>%
  compact_kable(col.names = c("State", "LGA", "Ward",
                              "Facility name",
                              "recruit", "consent"))

# Conditions --------------------------------------------------------------

# Maternal death capture
cond_q13a <- (is.na(dat$q13a) | trimws(dat$q13a) == "")
cond_neg_q13awhy <- !is.na(dat$q13a) & dat$q13a == "0" & (is.na(dat$q13awhy) | trimws(dat$q13awhy) == "")
# note: adding "ca" to denote "condition a". this is a way of distinguishing conditions that apply to the same question without making it unclear what the variable name is
cond_q13a1_ca <- !is.na(dat$q13a) & dat$q13a == "1" & (is.na(dat$q13a1) | trimws(dat$q13a1) == "")
cond_q13a1_cb <- !is.na(dat$q13a) & dat$q13a == "1" & (!is.na(dat$q13a1) & (as.numeric(dat$q13a1) < 1990 | (as.numeric(dat$q13a1) > 2026 & as.numeric(dat$q13a1) != 9999) ))
cond_q13a2 <- !is.na(dat$q13a) & dat$q13a == "1" & (is.na(dat$q13a2) | trimws(dat$q13a2) == "")
cond_q14a <- !is.na(dat$q13a) & dat$q13a == "1" & (is.na(dat$q14a) | trimws(dat$q14a) == "")

# Stillbirth capture
cond_q13b <- (is.na(dat$q13b) | trimws(dat$q13b) == "")
cond_neg_q13bwhy <- !is.na(dat$q13b) & dat$q13b == "0" & (is.na(dat$q13bwhy) | trimws(dat$q13bwhy) == "")
cond_q13b1_ca <- !is.na(dat$q13b) & dat$q13b == "1" & (is.na(dat$q13b1) | trimws(dat$q13b1) == "")
cond_q13b1_cb <- !is.na(dat$q13b) & dat$q13b == "1" & (!is.na(dat$q13b1) & (as.numeric(dat$q13b1) < 1990 | (as.numeric(dat$q13b1) > 2026 & as.numeric(dat$q13b1) != 9999) ))
cond_q13b2 <- !is.na(dat$q13b) & dat$q13b == "1" & (is.na(dat$q13b2) | trimws(dat$q13b2) == "")
cond_q14b <- !is.na(dat$q13b) & dat$q13b == "1" & (is.na(dat$q14b) | trimws(dat$q14b) == "")

# Neonatal deaths
cond_q13c <- (is.na(dat$q13c) | trimws(dat$q13c) == "")
cond_neg_q13cwhy <- !is.na(dat$q13c) & dat$q13c == "0" & (is.na(dat$q13cwhy) | trimws(dat$q13cwhy) == "")
cond_q13c1_ca <- !is.na(dat$q13c) & dat$q13c == "1" & (is.na(dat$q13c1) | trimws(dat$q13c1) == "")
cond_q13c1_cb <- !is.na(dat$q13c) & dat$q13c == "1" & (!is.na(dat$q13c1) & (as.numeric(dat$q13c1) < 1990 | (as.numeric(dat$q13c1) > 2026 & as.numeric(dat$q13c1) != 9999) ))
cond_q13c2 <- !is.na(dat$q13c) & dat$q13c == "1" & (is.na(dat$q13c2) | trimws(dat$q13c2) == "")
cond_q14c <- !is.na(dat$q13c) & dat$q13c == "1" & (is.na(dat$q14c) | trimws(dat$q14c) == "")

# Child deaths
cond_q13d <- (is.na(dat$q13d) | trimws(dat$q13d) == "")
cond_neg_q13dwhy <- !is.na(dat$q13d) & dat$q13d == "0" & (is.na(dat$q13dwhy) | trimws(dat$q13dwhy) == "")
cond_q13d1_ca <- !is.na(dat$q13d) & dat$q13d == "1" & (is.na(dat$q13d1) | trimws(dat$q13d1) == "")
cond_q13d1_cb <- !is.na(dat$q13d) & dat$q13d == "1" & (!is.na(dat$q13d1) & (as.numeric(dat$q13d1) < 1990 | (as.numeric(dat$q13d1) > 2026 & as.numeric(dat$q13d1) != 9999) ))
cond_q13d2 <- !is.na(dat$q13d) & dat$q13d == "1" & (is.na(dat$q13d2) | trimws(dat$q13d2) == "")
cond_q14d <- !is.na(dat$q13d) & dat$q13d == "1" & (is.na(dat$q14d) | trimws(dat$q14d) == "")

# MPCDSR training, mentoring, supervision, electronic or paper-based
cond_q15 <- (is.na(dat$q15) | trimws(dat$q15) == "")
cond_neg_q15a <- !is.na(dat$q15) & dat$q15 == "0" & !is.na(dat$q15a)
cond_q15a <- !is.na(dat$q15) & dat$q15 != "0" & (is.na(dat$q15a) | trimws(dat$q15a) == "")
cond_q16 <- (is.na(dat$q16) | trimws(dat$q16) == "")
cond_q17 <- (is.na(dat$q17) | trimws(dat$q17) == "")
cond_neg_q17a <- !is.na(dat$q17) & dat$q17 == "0" & !is.na(dat$q17a)
cond_q17a <- !is.na(dat$q17) & dat$q17 != "0" & (is.na(dat$q17a) | trimws(dat$q17a) == "")
cond_q17aoth <- !is.na(dat$q17a) & grepl("(^|\\s)96(\\s|$)", dat$q17a) & (is.na(dat$q17aoth) | trimws(dat$q17aoth) == "")
cond_q18 <- (is.na(dat$q18) | trimws(dat$q18) == "")
cond_q19 <- !is.na(dat$q18) & (dat$q18 == "2" | dat$q18 == "3") & (is.na(dat$q19) | trimws(dat$q19) == "")
cond_q19oth <- !is.na(dat$q19) & grepl("(^|\\s)96(\\s|$)", dat$q19) & (is.na(dat$q19oth) | trimws(dat$q19oth) == "")

# How identified
cond_q21m <- (is.na(dat$q21m) | trimws(dat$q21m) == "")
cond_q21oth <- !is.na(dat$q21m) & grepl("(^|\\s)96(\\s|$)", dat$q21m) & # row has "96" as a standalone code
  (is.na(dat$q21oth) | trimws(dat$q21oth) == "")

# How soon notified facility deaths
cond_q22a <- !is.na(dat$q13a2) & grepl("(^|\\s)1(\\s|$)", dat$q13a2) & (is.na(dat$q22a) | trimws(dat$q22a) == "")
cond_q22aoth <- !is.na(dat$q22a) & grepl("(^|\\s)96(\\s|$)", dat$q22a) &  # row has "96" as a standalone code
  (is.na(dat$q22aoth) | trimws(dat$q22aoth) == "")
cond_q22b <- !is.na(dat$q13b2) & grepl("(^|\\s)1(\\s|$)", dat$q13b2) & (is.na(dat$q22b) | trimws(dat$q22b) == "")
cond_q22both <- !is.na(dat$q22b) & grepl("(^|\\s)96(\\s|$)", dat$q22b) &  # row has "96" as a standalone code
  (is.na(dat$q22both) | trimws(dat$q22both) == "")
cond_q22c <- !is.na(dat$q13c2) & grepl("(^|\\s)1(\\s|$)", dat$q13c2) & (is.na(dat$q22c) | trimws(dat$q22c) == "")
cond_q22coth <- !is.na(dat$q22c) & grepl("(^|\\s)96(\\s|$)", dat$q22c) &  # row has "96" as a standalone code
  (is.na(dat$q22coth) | trimws(dat$q22coth) == "")
cond_q22d <- !is.na(dat$q13d2) & grepl("(^|\\s)1(\\s|$)", dat$q13d2) & (is.na(dat$q22d) | trimws(dat$q22d) == "")
cond_q22doth <- !is.na(dat$q22d) & grepl("(^|\\s)96(\\s|$)", dat$q22d) &  # row has "96" as a standalone code
  (is.na(dat$q22doth) | trimws(dat$q22doth) == "")

# How soon notified community deaths
cond_q23a <- !is.na(dat$q13a2) & grepl("(^|\\s)2(\\s|$)", dat$q13a2) & (is.na(dat$q23a) | trimws(dat$q23a) == "")
cond_q23aoth <- !is.na(dat$q23a) & grepl("(^|\\s)96(\\s|$)", dat$q23a) &  # row has "96" as a standalone code
  (is.na(dat$q23aoth) | trimws(dat$q23aoth) == "")
cond_q23b <- !is.na(dat$q13b2) & grepl("(^|\\s)2(\\s|$)", dat$q13b2) & (is.na(dat$q23b) | trimws(dat$q23b) == "")
cond_q23both <- !is.na(dat$q23b) & grepl("(^|\\s)96(\\s|$)", dat$q23b) &  # row has "96" as a standalone code
  (is.na(dat$q23both) | trimws(dat$q23both) == "")
cond_q23c <- !is.na(dat$q13c2) & grepl("(^|\\s)2(\\s|$)", dat$q13c2) & (is.na(dat$q23c) | trimws(dat$q23c) == "")
cond_q23coth <- !is.na(dat$q23c) & grepl("(^|\\s)96(\\s|$)", dat$q23c) &  # row has "96" as a standalone code
  (is.na(dat$q23coth) | trimws(dat$q23coth) == "")
cond_q23d <- !is.na(dat$q13d2) & grepl("(^|\\s)2(\\s|$)", dat$q13d2) & (is.na(dat$q23d) | trimws(dat$q23d) == "")
cond_q23doth <- !is.na(dat$q23d) & grepl("(^|\\s)96(\\s|$)", dat$q23d) &  # row has "96" as a standalone code
  (is.na(dat$q23doth) | trimws(dat$q23doth) == "")

# Who is notified, audits, committees
cond_q24m <- (is.na(dat$q24m) | trimws(dat$q24m) == "")
cond_q24oth <- !is.na(dat$q24m) & grepl("(^|\\s)96(\\s|$)", dat$q24m) &
  (is.na(dat$q24oth) | trimws(dat$q24oth) == "")
cond_q25 <- (is.na(dat$q25) | trimws(dat$q25) == "")
cond_q25a <- !is.na(dat$q25) & dat$q25 == "1" & (is.na(dat$q25a) | trimws(dat$q25a) == "")
cond_q26m <- (is.na(dat$q26m) | trimws(dat$q26m) == "")
cond_q26moth <- !is.na(dat$q26m) & grepl("(^|\\s)96(\\s|$)", dat$q26m) & 
  (is.na(dat$q26moth) | trimws(dat$q26moth) == "")
cond_q26b <- (is.na(dat$q26b) | trimws(dat$q26b) == "")
cond_q26both <- !is.na(dat$q26b) & grepl("(^|\\s)96(\\s|$)", dat$q26b) & 
  (is.na(dat$q26both) | trimws(dat$q26both) == "")

# Code of conduct
cond_q211 <- (is.na(dat$q211) | trimws(dat$q211) == "")
cond_q211oth <- !is.na(dat$q211) & dat$q211 == "2" & (is.na(dat$q211oth) | trimws(dat$q211oth) == "")
#### !!!!! FLAG: SHOULD Q212 AND Q213 ALSO BE ASKED IF Q211=1 (YES, REPORTED NOT SEEN)?
cond_neg_q212 <- !is.na(dat$q211) & dat$q211 != "2" & !is.na(dat$q212)
cond_q212 <- !is.na(dat$q211) & dat$q211 == "2" & (is.na(dat$q212) | trimws(dat$q212) == "")
cond_neg_q213 <- !is.na(dat$q211) & dat$q211 != "2" & !is.na(dat$q213)
cond_q213 <- !is.na(dat$q211) & dat$q211 == "2" & (is.na(dat$q213) | trimws(dat$q213) == "")
cond_neg_q213a <- !is.na(dat$q213) & dat$q213 != "1" & !is.na(dat$q213a)
cond_q213a <- !is.na(dat$q213) & dat$q213 == "1" & (is.na(dat$q213a) | trimws(dat$q213a) == "")
cond_neg_q213b <- !is.na(dat$q213) & dat$q213 != "1" & !is.na(dat$q213b)
cond_q213b <- !is.na(dat$q213) & dat$q213 == "1" & (is.na(dat$q213b) | trimws(dat$q213b) == "")

# Community-based deaths
cond_q214 <- (is.na(dat$q214) | trimws(dat$q214) == "")
cond_neg_q214a <- !is.na(dat$q214) & dat$q214 != "1" & !is.na(dat$q214a)
cond_q214a <- !is.na(dat$q214) & dat$q214 == "1" & (is.na(dat$q214a) | trimws(dat$q214a) == "")

# Community-based maternal deaths
cond_neg_q215a <- !is.na(dat$q214a) & !grepl("(^|\\s)1(\\s|$)", dat$q214a) & !is.na(dat$q215a)
cond_q215a <-  !is.na(dat$q214a) & grepl("(^|\\s)1(\\s|$)", dat$q214a) &  (is.na(dat$q215a) | trimws(dat$q215a) == "")
cond_q215aoth <-  !is.na(dat$q215a) & grepl("(^|\\s)96(\\s|$)", dat$q215a) & (is.na(dat$q215aoth) | trimws(dat$q215aoth) == "")
cond_neg_q215b <- !is.na(dat$q214a) & !grepl("(^|\\s)1(\\s|$)", dat$q214a) & !is.na(dat$q215b)
cond_q215b <-  !is.na(dat$q214a) & grepl("(^|\\s)1(\\s|$)", dat$q214a) & (is.na(dat$q215b) | trimws(dat$q215b) == "")
cond_q215cngo <- !is.na(dat$q215b)   & grepl("(^|\\s)15(\\s|$)", dat$q215b) & (is.na(dat$q215cngo) | trimws(dat$q215cngo) == "")
cond_q215cnun <- !is.na(dat$q215b)   & grepl("(^|\\s)16(\\s|$)", dat$q215b) & (is.na(dat$q215cnun) | trimws(dat$q215cnun) == "")
cond_q215cnlmoh <- !is.na(dat$q215b) & grepl("(^|\\s)17(\\s|$)", dat$q215b) & (is.na(dat$q215cnlmoh) | trimws(dat$q215cnlmoh) == "")
cond_q215cnsmoh <- !is.na(dat$q215b) & grepl("(^|\\s)18(\\s|$)", dat$q215b) & (is.na(dat$q215cnsmoh) | trimws(dat$q215cnsmoh) == "")
cond_q215cnnmoh <- !is.na(dat$q215b) & grepl("(^|\\s)19(\\s|$)", dat$q215b) & (is.na(dat$q215cnnmoh) | trimws(dat$q215cnnmoh) == "")
cond_q215cnoth <- !is.na(dat$q215b)  & grepl("(^|\\s)96(\\s|$)", dat$q215b) & (is.na(dat$q215cnoth) | trimws(dat$q215cnoth) == "")

# Community-based stillbirths (q214a code = 2)
cond_neg_q216a <- !is.na(dat$q214a) & !grepl("(^|\\s)2(\\s|$)", dat$q214a) & !is.na(dat$q216a)
cond_q216a <-  !is.na(dat$q214a) & grepl("(^|\\s)2(\\s|$)", dat$q214a) &  (is.na(dat$q216a) | trimws(dat$q216a) == "")
cond_q216both <-  !is.na(dat$q216a) & grepl("(^|\\s)96(\\s|$)", dat$q216a) & (is.na(dat$q216both) | trimws(dat$q216both) == "")
cond_neg_q216b <- !is.na(dat$q214a) & !grepl("(^|\\s)2(\\s|$)", dat$q214a) & !is.na(dat$q216b)
cond_q216b <-  !is.na(dat$q214a) & grepl("(^|\\s)2(\\s|$)", dat$q214a) & (is.na(dat$q216b) | trimws(dat$q216b) == "")
cond_q216bngo <- !is.na(dat$q216b) & grepl("(^|\\s)15(\\s|$)", dat$q216b) & (is.na(dat$q216bngo) | trimws(dat$q216bngo) == "")
cond_q216bnun <- !is.na(dat$q216b) & grepl("(^|\\s)16(\\s|$)", dat$q216b) & (is.na(dat$q216bnun) | trimws(dat$q216bnun) == "")
cond_q216bnlmoh <- !is.na(dat$q216b) & grepl("(^|\\s)17(\\s|$)", dat$q216b) & (is.na(dat$q216bnlmoh) | trimws(dat$q216bnlmoh) == "")
cond_q216bnsmoh <- !is.na(dat$q216b) & grepl("(^|\\s)18(\\s|$)", dat$q216b) & (is.na(dat$q216bnsmoh) | trimws(dat$q216bnsmoh) == "")
cond_q216bnnmoh <- !is.na(dat$q216b) & grepl("(^|\\s)19(\\s|$)", dat$q216b) & (is.na(dat$q216bnnmoh) | trimws(dat$q216bnnmoh) == "")
cond_q216bnoth <- !is.na(dat$q216b) & grepl("(^|\\s)96(\\s|$)", dat$q216b) & (is.na(dat$q216bnoth) | trimws(dat$q216bnoth) == "")

# Community-based neonatal deaths (q214a code = 3)
cond_neg_q217a <- !is.na(dat$q214a) & !grepl("(^|\\s)3(\\s|$)", dat$q214a) & !is.na(dat$q217a)
cond_q217a <-  !is.na(dat$q214a) & grepl("(^|\\s)3(\\s|$)", dat$q214a) &  (is.na(dat$q217a) | trimws(dat$q217a) == "")
cond_q217both <-  !is.na(dat$q217a) & grepl("(^|\\s)96(\\s|$)", dat$q217a) & (is.na(dat$q217both) | trimws(dat$q217both) == "")
cond_neg_q217b <- !is.na(dat$q214a) & !grepl("(^|\\s)3(\\s|$)", dat$q214a) & !is.na(dat$q217b)
cond_q217b <-  !is.na(dat$q214a) & grepl("(^|\\s)3(\\s|$)", dat$q214a) & (is.na(dat$q217b) | trimws(dat$q217b) == "")
cond_q217bngo <- !is.na(dat$q217b) & grepl("(^|\\s)15(\\s|$)", dat$q217b) & (is.na(dat$q217bngo) | trimws(dat$q217bngo) == "")
cond_q217bnun <- !is.na(dat$q217b) & grepl("(^|\\s)16(\\s|$)", dat$q217b) & (is.na(dat$q217bnun) | trimws(dat$q217bnun) == "")
cond_q217bnlmoh <- !is.na(dat$q217b) & grepl("(^|\\s)17(\\s|$)", dat$q217b) & (is.na(dat$q217bnlmoh) | trimws(dat$q217bnlmoh) == "")
cond_q217bnsmoh <- !is.na(dat$q217b) & grepl("(^|\\s)18(\\s|$)", dat$q217b) & (is.na(dat$q217bnsmoh) | trimws(dat$q217bnsmoh) == "")
cond_q217bnnmoh <- !is.na(dat$q217b) & grepl("(^|\\s)19(\\s|$)", dat$q217b) & (is.na(dat$q217bnnmoh) | trimws(dat$q217bnnmoh) == "")
cond_q217bnoth <- !is.na(dat$q217b) & grepl("(^|\\s)96(\\s|$)", dat$q217b) & (is.na(dat$q217bnoth) | trimws(dat$q217bnoth) == "")

# Community-based child deaths (q214a code = 4)
cond_neg_q218a <- !is.na(dat$q214a) & !grepl("(^|\\s)4(\\s|$)", dat$q214a) & !is.na(dat$q218a)
cond_q218a <-  !is.na(dat$q214a) & grepl("(^|\\s)4(\\s|$)", dat$q214a) & (is.na(dat$q218a) | trimws(dat$q218a) == "")
cond_q218both <-  !is.na(dat$q218a) & grepl("(^|\\s)96(\\s|$)", dat$q218a) & (is.na(dat$q218both) | trimws(dat$q218both) == "")
cond_neg_q218b <- !is.na(dat$q214a) & !grepl("(^|\\s)4(\\s|$)", dat$q214a) & !is.na(dat$q218b)
cond_q218b <-  !is.na(dat$q214a) & grepl("(^|\\s)4(\\s|$)", dat$q214a) & (is.na(dat$q218b) | trimws(dat$q218b) == "")
cond_q218bngo <- !is.na(dat$q218b) & grepl("(^|\\s)15(\\s|$)", dat$q218b) & (is.na(dat$q218bngo) | trimws(dat$q218bngo) == "")
cond_q218bnun <- !is.na(dat$q218b) & grepl("(^|\\s)16(\\s|$)", dat$q218b) & (is.na(dat$q218bnun) | trimws(dat$q218bnun) == "")
cond_q218bnlmoh <- !is.na(dat$q218b) & grepl("(^|\\s)17(\\s|$)", dat$q218b) & (is.na(dat$q218bnlmoh) | trimws(dat$q218bnlmoh) == "")
cond_q218bnsmoh <- !is.na(dat$q218b) & grepl("(^|\\s)18(\\s|$)", dat$q218b) & (is.na(dat$q218bnsmoh) | trimws(dat$q218bnsmoh) == "")
cond_q218bnnmoh <- !is.na(dat$q218b) & grepl("(^|\\s)19(\\s|$)", dat$q218b) & (is.na(dat$q218bnnmoh) | trimws(dat$q218bnnmoh) == "")
cond_q218bnoth <- !is.na(dat$q218b) & grepl("(^|\\s)96(\\s|$)", dat$q218b) & (is.na(dat$q218bnoth) | trimws(dat$q218bnoth) == "")

# Financial support for VASA
cond_q219 <- !is.na(dat$q214) & dat$q214 == "1" & (is.na(dat$q219) | trimws(dat$q219) == "")
cond_neg_q219a <- !is.na(dat$q219) & dat$q219 != "1" & !is.na(dat$q219a)
cond_q219a <- !is.na(dat$q219) & dat$q219 == "1" & (is.na(dat$q219a) | trimws(dat$q219a) == "")
cond_neg_q219b <- !is.na(dat$q219) & dat$q219 != "1" & !is.na(dat$q219b)
cond_q219b <- !is.na(dat$q219) & dat$q219 == "1" & (is.na(dat$q219b) | trimws(dat$q219b) == "")

## Section: Analysis and recommendations

# Classification system for cause of death
cond_q220 <- (is.na(dat$q220) | trimws(dat$q220) == "")
cond_q220oth <- !is.na(dat$q220) & grepl("(^|\\s)96(\\s|$)", dat$q220) & (is.na(dat$q220oth) | trimws(dat$q220oth) == "")

# Components addressed during death reviews
cond_q221 <- !is.na(dat$q25) & dat$q25 == "1" & (is.na(dat$q221) | trimws(dat$q221) == "")
cond_neg_q221 <- !is.na(dat$q25) & dat$q25 != "1" & !is.na(dat$q221)
cond_q221oth <- !is.na(dat$q221) & grepl("(^|\\s)96(\\s|$)", dat$q221) & (is.na(dat$q221oth) | trimws(dat$q221oth) == "")

# Death review documentation
# Maternal
cond_neg_q222a <- !is.na(dat$q25a) & !grepl("(^|\\s)1(\\s|$)", dat$q25a) & !is.na(dat$q222a)
cond_q222a <- !is.na(dat$q25a) & grepl("(^|\\s)1(\\s|$)", dat$q25a) & (is.na(dat$q222a) | trimws(dat$q222a) == "")
cond_q222aoth <- !is.na(dat$q222a) & dat$q222a == "96" & (is.na(dat$q222aoth) | trimws(dat$q222aoth) == "")
# FLAG: q222a1 instructs to take a picture. not entirely sure what this response will look like
cond_q222a1 <- !is.na(dat$q222a) & dat$q222a != "96" & (is.na(dat$q222a1) | trimws(dat$q222a1) == "")
# Stillbirth
cond_neg_q222b <- !is.na(dat$q25a) & !grepl("(^|\\s)2(\\s|$)", dat$q25a) & !is.na(dat$q222b)
cond_q222b <- !is.na(dat$q25a) & grepl("(^|\\s)2(\\s|$)", dat$q25a) & (is.na(dat$q222b) | trimws(dat$q222b) == "")
cond_q222both <- !is.na(dat$q222b) & dat$q222b == "96" & (is.na(dat$q222both) | trimws(dat$q222both) == "")
# FLAG: q222b1 instructs to take a picture. not entirely sure what this response will look like
# Note: Excel form's label for q222b1 literally reads "Take a picture of the maternal mortality form/report". This is a copy-paste error from q222a1. 
cond_q222b1 <- !is.na(dat$q222b) & dat$q222b != "96" & (is.na(dat$q222b1) | trimws(dat$q222b1) == "")
# Neonatal
cond_neg_q222c <- !is.na(dat$q25a) & !grepl("(^|\\s)3(\\s|$)", dat$q25a) & !is.na(dat$q222c)
cond_q222c <- !is.na(dat$q25a) & grepl("(^|\\s)3(\\s|$)", dat$q25a) & (is.na(dat$q222c) | trimws(dat$q222c) == "")
cond_q222coth <- !is.na(dat$q222c) & dat$q222c == "96" & (is.na(dat$q222coth) | trimws(dat$q222coth) == "")
# FLAG: q222c1 instructs to take a picture. not entirely sure what this response will look like
# Note: Excel form's label for q222c1 literally reads "Take a picture of the maternal mortality form/report". This is a copy-paste error from q222a1. 
cond_q222c1 <- !is.na(dat$q222c) & dat$q222c != "96" & (is.na(dat$q222c1) | trimws(dat$q222c1) == "")
# Child deaths
cond_neg_q222d <- !is.na(dat$q25a) & !grepl("(^|\\s)4(\\s|$)", dat$q25a) & !is.na(dat$q222d)
cond_q222d <- !is.na(dat$q25a) & grepl("(^|\\s)4(\\s|$)", dat$q25a) & (is.na(dat$q222d) | trimws(dat$q222d) == "")
cond_q222doth <- !is.na(dat$q222d) & dat$q222d == "96" & (is.na(dat$q222doth) | trimws(dat$q222doth) == "")
# FLAG: q222d1 instructs to take a picture. not entirely sure what this response will look like
# Note: Excel form's label for q222d1 literally reads "Take a picture of the maternal mortality form/report". This is a copy-paste error from q222a1. 
cond_q222d1 <- !is.na(dat$q222d) & dat$q222d != "96" & (is.na(dat$q222d1) | trimws(dat$q222d1) == "")

# Death case report preparation and dissemination
cond_q223 <- (is.na(dat$q223) | trimws(dat$q223) == "")
cond_neg_q224a <- !is.na(dat$q223) & dat$q223 != "1" & !is.na(dat$q224a)
cond_q224a <- !is.na(dat$q223) & dat$q223 == "1" & (is.na(dat$q224a) | trimws(dat$q224a) == "")
cond_neg_q224b <- !is.na(dat$q223) & dat$q223 != "1" & !is.na(dat$q224b)
cond_q224b <- !is.na(dat$q223) & dat$q223 == "1" & (is.na(dat$q224b) | trimws(dat$q224b) == "")
cond_q224bngo <- !is.na(dat$q224b) & grepl("(^|\\s)15(\\s|$)", dat$q224b) & (is.na(dat$q224bngo) | trimws(dat$q224bngo) == "")
cond_q224bnun <- !is.na(dat$q224b) & grepl("(^|\\s)16(\\s|$)", dat$q224b) & (is.na(dat$q224bnun) | trimws(dat$q224bnun) == "")
cond_q224bnlmoh <- !is.na(dat$q224b) & grepl("(^|\\s)17(\\s|$)", dat$q224b) & (is.na(dat$q224bnlmoh) | trimws(dat$q224bnlmoh) == "")
cond_q224bnsmoh <- !is.na(dat$q224b) & grepl("(^|\\s)18(\\s|$)", dat$q224b) & (is.na(dat$q224bnsmoh) | trimws(dat$q224bnsmoh) == "")
cond_q224bnnmoh <- !is.na(dat$q224b) & grepl("(^|\\s)19(\\s|$)", dat$q224b) & (is.na(dat$q224bnnmoh) | trimws(dat$q224bnnmoh) == "")
cond_q224bnoth <- !is.na(dat$q224b) & grepl("(^|\\s)96(\\s|$)", dat$q224b) & (is.na(dat$q224bnoth) | trimws(dat$q224bnoth) == "")
cond_neg_q225 <- !is.na(dat$q223) & dat$q223 != "1" & !is.na(dat$q225)
cond_q225 <- !is.na(dat$q223) & dat$q223 == "1" & (is.na(dat$q225) | trimws(dat$q225) == "")
cond_q225oth <- !is.na(dat$q225) & grepl("(^|\\s)96(\\s|$)", dat$q225) & (is.na(dat$q225oth) | trimws(dat$q225oth) == "")

# Documentation of VASA
# FLAG: the excel logic references ${q14a} (a free-text clinical definition field ), not ${q214a} (the multi-select "which deaths investigated with VASA" field, which has codes 1,2,3,4 for maternal, sb, nnd, child).
# Code below is written for q214a
# Maternal deaths
cond_neg_q226a <- !is.na(dat$q214a) & !grepl("(^|\\s)1(\\s|$)", dat$q214a) & !is.na(dat$q226a)
cond_q226a <- !is.na(dat$q214a) & grepl("(^|\\s)1(\\s|$)", dat$q214a) & (is.na(dat$q226a) | trimws(dat$q226a) == "")
cond_q226aoth <- !is.na(dat$q226a) & dat$q226a == "96" & (is.na(dat$q226aoth) | trimws(dat$q226aoth) == "")
# FLAG: excel form says "take a picture" for 226ali while pdf says "did you obtain a copy". Not sure what will actually happen
cond_q226a1i <- !is.na(dat$q226a) & dat$q226a != "96" & (is.na(dat$q226a1i) | trimws(dat$q226a1i) == "")
# Stillbirths
cond_neg_q226b <- !is.na(dat$q214a) & !grepl("(^|\\s)2(\\s|$)", dat$q214a) & !is.na(dat$q226b)
cond_q226b <- !is.na(dat$q214a) & grepl("(^|\\s)2(\\s|$)", dat$q214a) & (is.na(dat$q226b) | trimws(dat$q226b) == "")
cond_q226both <- !is.na(dat$q226b) & dat$q226b == "96" & (is.na(dat$q226both) | trimws(dat$q226both) == "")
# FLAG: excel form says "take a picture" for 226bli while pdf says "did you obtain a copy". Not sure what will actually happen
cond_q226b1i <- !is.na(dat$q226b) & dat$q226b != "96" & (is.na(dat$q226b1i) | trimws(dat$q226b1i) == "")
# Neonatal deaths
cond_neg_q226c <- !is.na(dat$q214a) & !grepl("(^|\\s)3(\\s|$)", dat$q214a) & !is.na(dat$q226c)
cond_q226c <- !is.na(dat$q214a) & grepl("(^|\\s)3(\\s|$)", dat$q214a) & (is.na(dat$q226c) | trimws(dat$q226c) == "")
cond_q226coth <- !is.na(dat$q226c) & dat$q226c == "96" & (is.na(dat$q226coth) | trimws(dat$q226coth) == "")
# FLAG: excel form says "take a picture" for 226cli while pdf says "did you obtain a copy". Not sure what will actually happen
cond_q226c1i <- !is.na(dat$q226c) & dat$q226c != "96" & (is.na(dat$q226c1i) | trimws(dat$q226c1i) == "")
# Child deaths
cond_neg_q226d <- !is.na(dat$q214a) & !grepl("(^|\\s)4(\\s|$)", dat$q214a) & !is.na(dat$q226d)
cond_q226d <- !is.na(dat$q214a) & grepl("(^|\\s)4(\\s|$)", dat$q214a) & (is.na(dat$q226d) | trimws(dat$q226d) == "")
cond_q226doth <- !is.na(dat$q226d) & dat$q226d == "96" & (is.na(dat$q226doth) | trimws(dat$q226doth) == "")
# FLAG: excel form says "take a picture" for 226dli while pdf says "did you obtain a copy". Not sure what will actually happen
cond_q226d1i <- !is.na(dat$q226d) & dat$q226d != "96" & (is.na(dat$q226d1i) | trimws(dat$q226d1i) == "")

# Components and reporting of VASA
cond_neg_q227 <- !is.na(dat$q214) & dat$q214 != "1" & !is.na(dat$q227)
cond_q227 <- !is.na(dat$q214) & dat$q214 == "1" & (is.na(dat$q227) | trimws(dat$q227) == "")
cond_q227oth <- !is.na(dat$q227) & grepl("(^|\\s)96(\\s|$)", dat$q227) & (is.na(dat$q227oth) | trimws(dat$q227oth) == "")
cond_neg_q228 <- !is.na(dat$q214) & dat$q214 != "1" & !is.na(dat$q228)
cond_q228 <- !is.na(dat$q214) & dat$q214 == "1" & (is.na(dat$q228) | trimws(dat$q228) == "")
cond_neg_q229 <- !is.na(dat$q214) & dat$q214 != "1" & !is.na(dat$q229)
cond_q229 <- !is.na(dat$q214) & dat$q214 == "1" & (is.na(dat$q229) | trimws(dat$q229) == "")
cond_q229oth <- !is.na(dat$q229) & grepl("(^|\\s)96(\\s|$)", dat$q229) & (is.na(dat$q229oth) | trimws(dat$q229oth) == "")

# Follow-up on death reviews
cond_neg_q230 <- !(!is.na(dat$q25) & dat$q25 == "1" & !is.na(dat$q214) & dat$q214 == "1") & !is.na(dat$q230)
cond_q230 <- !is.na(dat$q25) & dat$q25 == "1" & !is.na(dat$q214) & dat$q214 == "1" & (is.na(dat$q230) | trimws(dat$q230) == "")
cond_neg_q230a <- !is.na(dat$q230) & dat$q230 != "1" & !is.na(dat$q230a)
cond_q230a <- !is.na(dat$q230) & dat$q230 == "1" & (is.na(dat$q230a) | trimws(dat$q230a) == "")
# FLAG: q231 is typed as "text" in the Excel form despite being phrased as a yes/no question ("Is there a systematic process...")
# Code is written below as select_one yn
cond_neg_q231 <- !(!is.na(dat$q25) & dat$q25 == "1" & !is.na(dat$q214) & dat$q214 == "1") & !is.na(dat$q231)
cond_q231 <- !is.na(dat$q25) & dat$q25 == "1" & !is.na(dat$q214) & dat$q214 == "1" & (is.na(dat$q231) | trimws(dat$q231) == "")
cond_neg_q232 <- !(!is.na(dat$q25) & dat$q25 == "1" & !is.na(dat$q214) & dat$q214 == "1") & !is.na(dat$q232)
cond_q232 <- !is.na(dat$q25) & dat$q25 == "1" & !is.na(dat$q214) & dat$q214 == "1" & (is.na(dat$q232) | trimws(dat$q232) == "")
cond_neg_q232a <- !is.na(dat$q232) & dat$q232 != "1" & !is.na(dat$q232a)
cond_q232a <- !is.na(dat$q232) & dat$q232 == "1" & (is.na(dat$q232a) | trimws(dat$q232a) == "")
cond_neg_q233 <- !(!is.na(dat$q25) & dat$q25 == "1" & !is.na(dat$q214) & dat$q214 == "1") & !is.na(dat$q233)
cond_q233 <- !is.na(dat$q25) & dat$q25 == "1" & !is.na(dat$q214) & dat$q214 == "1" & (is.na(dat$q233) | trimws(dat$q233) == "")
cond_neg_q233a <- !is.na(dat$q233) & dat$q233 != "1" & !is.na(dat$q233a)
cond_q233a <- !is.na(dat$q233) & dat$q233 == "1" & (is.na(dat$q233a) | trimws(dat$q233a) == "")
cond_neg_q233b <- !is.na(dat$q233) & dat$q233 != "1" & !is.na(dat$q233b)
cond_q233b <- !is.na(dat$q233) & dat$q233 == "1" & (is.na(dat$q233b) | trimws(dat$q233b) == "")

# Reporting completeness and timeliness
cond_neg_q234 <- !(!is.na(dat$q25) & dat$q25 == "1" & !is.na(dat$q214) & dat$q214 == "1") & !is.na(dat$q234)
cond_q234 <- !is.na(dat$q25) & dat$q25 == "1" & !is.na(dat$q214) & dat$q214 == "1" & (is.na(dat$q234) | trimws(dat$q234) == "")

# Aggregate reporting
cond_neg_q235 <- !is.na(dat$q234) & dat$q234 != "1" & !is.na(dat$q235)
cond_q235 <- !is.na(dat$q234) & dat$q234 == "1" & (is.na(dat$q235) | trimws(dat$q235) == "")
cond_q235ngo <- !is.na(dat$q235) & grepl("(^|\\s)15(\\s|$)", dat$q235) & (is.na(dat$q235ngo) | trimws(dat$q235ngo) == "")
cond_q235nun <- !is.na(dat$q235) & grepl("(^|\\s)16(\\s|$)", dat$q235) & (is.na(dat$q235nun) | trimws(dat$q235nun) == "")
cond_q235nlmoh <- !is.na(dat$q235) & grepl("(^|\\s)17(\\s|$)", dat$q235) & (is.na(dat$q235nlmoh) | trimws(dat$q235nlmoh) == "")
cond_q235nsmoh <- !is.na(dat$q235) & grepl("(^|\\s)18(\\s|$)", dat$q235) & (is.na(dat$q235nsmoh) | trimws(dat$q235nsmoh) == "")
cond_q235nnmoh <- !is.na(dat$q235) & grepl("(^|\\s)19(\\s|$)", dat$q235) & (is.na(dat$q235nnmoh) | trimws(dat$q235nnmoh) == "")
cond_q235noth <- !is.na(dat$q235) & grepl("(^|\\s)96(\\s|$)", dat$q235) & (is.na(dat$q235noth) | trimws(dat$q235noth) == "")
cond_neg_q236 <- !is.na(dat$q234) & dat$q234 != "1" & !is.na(dat$q236)
cond_q236 <- !is.na(dat$q234) & dat$q234 == "1" & (is.na(dat$q236) | trimws(dat$q236) == "")
cond_neg_q237a <- !is.na(dat$q234) & dat$q234 != "1" & !is.na(dat$q237a)
cond_q237a <- !is.na(dat$q234) & dat$q234 == "1" & (is.na(dat$q237a) | trimws(dat$q237a) == "")
cond_q237aoth <- !is.na(dat$q237a) & dat$q237a == "96" & (is.na(dat$q237aoth) | trimws(dat$q237aoth) == "")
cond_neg_q238 <- !is.na(dat$q234) & dat$q234 != "1" & !is.na(dat$q238)
cond_q238 <- !is.na(dat$q234) & dat$q234 == "1" & (is.na(dat$q238) | trimws(dat$q238) == "")
cond_q238oth <- !is.na(dat$q238) & grepl("(^|\\s)96(\\s|$)", dat$q238) & (is.na(dat$q238oth) | trimws(dat$q238oth) == "")

## Section: MPCDSR Data

# MPCDSR data points collected/reported
cond_neg_q239 <- !is.na(dat$q25) & dat$q25 != "1" & !is.na(dat$q239)
cond_q239 <- !is.na(dat$q25) & dat$q25 == "1" & (is.na(dat$q239) | trimws(dat$q239) == "")
cond_neg_q240 <- !is.na(dat$q214) & dat$q214 != "1" & !is.na(dat$q240)
cond_q240 <- !is.na(dat$q214) & dat$q214 == "1" & (is.na(dat$q240) | trimws(dat$q240) == "")
cond_neg_q241 <- !is.na(dat$q234) & dat$q234 != "1" & !is.na(dat$q241)
cond_q241 <- !is.na(dat$q234) & dat$q234 == "1" & (is.na(dat$q241) | trimws(dat$q241) == "")

# After MaternPeriDeathRepeat

# Helper: flags NA, non-numeric, or negative values in a column
flag_invalid_numeric <- function(x) {
  num_x <- suppressWarnings(as.numeric(x))
  is.na(x) | trimws(x) == "" | (is.na(num_x) & !is.na(x)) | (!is.na(num_x) & num_x < 0)
}

# Facility-based maternal death recommendations (past 12 months)
cond_q243a1i <- flag_invalid_numeric(dat$q243a1i)
cond_q243b1i <- flag_invalid_numeric(dat$q243b1i)
cond_q243c1i <- flag_invalid_numeric(dat$q243c1i)
cond_q243d1i <- flag_invalid_numeric(dat$q243d1i)
cond_q243e1i <- flag_invalid_numeric(dat$q243e1i)

# Facility-based neonatal death recommendations (past 12 months)
cond_q244a1i <- flag_invalid_numeric(dat$q244a1i)
cond_q244b1i <- flag_invalid_numeric(dat$q244b1i)
cond_q244c1i <- flag_invalid_numeric(dat$q244c1i)
cond_q244d1i <- flag_invalid_numeric(dat$q244d1i)
cond_q244e1i <- flag_invalid_numeric(dat$q244e1i)

# Facility-based stillbirth recommendations (past 12 months)
cond_q245a1i <- flag_invalid_numeric(dat$q245a1i)
cond_q245b1i <- flag_invalid_numeric(dat$q245b1i)
cond_q245c1i <- flag_invalid_numeric(dat$q245c1i)
cond_q245d1i <- flag_invalid_numeric(dat$q245d1i)
cond_q245e1i <- flag_invalid_numeric(dat$q245e1i)

## Section: Integration with Quality Improvement and other surveillance systems

# Quality improvement and MPCDSR system integration
cond_q246 <- (is.na(dat$q246) | trimws(dat$q246) == "")
cond_q247 <- (is.na(dat$q247) | trimws(dat$q247) == "")
cond_q248 <- (is.na(dat$q248) | trimws(dat$q248) == "")
cond_q249 <- (is.na(dat$q249) | trimws(dat$q249) == "")
cond_q250 <- (is.na(dat$q250) | trimws(dat$q250) == "")
cond_q250oth1 <- !is.na(dat$q250) & grepl("(^|\\s)961(\\s|$)", dat$q250) & (is.na(dat$q250oth1) | trimws(dat$q250oth1) == "")
cond_q250oth2 <- !is.na(dat$q250) & grepl("(^|\\s)962(\\s|$)", dat$q250) & (is.na(dat$q250oth2) | trimws(dat$q250oth2) == "")
cond_neg_q251a <- !is.na(dat$q250) & !grepl("(^|\\s)1(\\s|$)", dat$q250) & !is.na(dat$q251a)
cond_q251a <- !is.na(dat$q250) & grepl("(^|\\s)1(\\s|$)", dat$q250) & (is.na(dat$q251a) | trimws(dat$q251a) == "")
cond_neg_q251b <- !is.na(dat$q250) & !grepl("(^|\\s)2(\\s|$)", dat$q250) & !is.na(dat$q251b)
cond_q251b <- !is.na(dat$q250) & grepl("(^|\\s)2(\\s|$)", dat$q250) & (is.na(dat$q251b) | trimws(dat$q251b) == "")
cond_neg_q251c <- !is.na(dat$q250) & !grepl("(^|\\s)961(\\s|$)", dat$q250) & !is.na(dat$q251c)
cond_q251c <- !is.na(dat$q250) & grepl("(^|\\s)961(\\s|$)", dat$q250) & (is.na(dat$q251c) | trimws(dat$q251c) == "")
cond_neg_q251d <- !is.na(dat$q250) & !grepl("(^|\\s)962(\\s|$)", dat$q250) & !is.na(dat$q251d)
cond_q251d <- !is.na(dat$q250) & grepl("(^|\\s)962(\\s|$)", dat$q250) & (is.na(dat$q251d) | trimws(dat$q251d) == "")

## Section: Data quality and use

# Data quality control and consistency checks
cond_q252 <- (is.na(dat$q252) | trimws(dat$q252) == "")
cond_neg_q253a <- !is.na(dat$q252) & !grepl("(^|\\s)1(\\s|$)", dat$q252) & !is.na(dat$q253a)
cond_q253a <- !is.na(dat$q252) & grepl("(^|\\s)1(\\s|$)", dat$q252) & (is.na(dat$q253a) | trimws(dat$q253a) == "")
cond_q253aoth <- !is.na(dat$q253a) & dat$q253a == "96" & (is.na(dat$q253aoth) | trimws(dat$q253aoth) == "")
cond_neg_q253b <- !is.na(dat$q252) & !grepl("(^|\\s)2(\\s|$)", dat$q252) & !is.na(dat$q253b)
cond_q253b <- !is.na(dat$q252) & grepl("(^|\\s)2(\\s|$)", dat$q252) & (is.na(dat$q253b) | trimws(dat$q253b) == "")
cond_q253both <- !is.na(dat$q253b) & dat$q253b == "96" & (is.na(dat$q253both) | trimws(dat$q253both) == "")
cond_neg_q253c <- !is.na(dat$q252) & !grepl("(^|\\s)3(\\s|$)", dat$q252) & !is.na(dat$q253c)
cond_q253c <- !is.na(dat$q252) & grepl("(^|\\s)3(\\s|$)", dat$q252) & (is.na(dat$q253c) | trimws(dat$q253c) == "")
cond_q253coth <- !is.na(dat$q253c) & dat$q253c == "96" & (is.na(dat$q253coth) | trimws(dat$q253coth) == "")
cond_q254 <- (is.na(dat$q254) | trimws(dat$q254) == "")
cond_q254oth <- !is.na(dat$q254) & grepl("(^|\\s)96(\\s|$)", dat$q254) & (is.na(dat$q254oth) | trimws(dat$q254oth) == "")
cond_q255 <- (is.na(dat$q255) | trimws(dat$q255) == "")
cond_q255oth <- !is.na(dat$q255) & grepl("(^|\\s)96(\\s|$)", dat$q255) & (is.na(dat$q255oth) | trimws(dat$q255oth) == "")
cond_q256 <- (is.na(dat$q256) | trimws(dat$q256) == "")
cond_q257 <- (is.na(dat$q257) | trimws(dat$q257) == "")

# Condition definitions ---------------------------------------------------

check_defs <- tribble(
  ~cond_name,            ~question,           ~label,                     ~issue,
  # Maternal death capture
  "cond_q13a",          "q13a",            "Maternal death captured",         "Maternal deaths captured is blank (q13a)",
  "cond_neg_q13awhy",   "q13a / q13awhy",  "Maternal death captured why",     "Maternal deaths not captured (q13a=0) and reason why is blank (q13awhy)",
  "cond_q13a1_ca",      "q13a / q13a1",    "Maternal death captured since year", "Maternal deaths captured (q13a=1) and year that capture began is blank (q13a1)",
  "cond_q13a1_cb",      "q13a / q13a1",    "Maternal death captured since year", "Maternal deaths captured (q13a=1) and year that capture began is < 1990 or > 2026 and does not equal the designated missing value of 9999 (q13a1)",
  "cond_q13a2",         "q13a / q13a2",    "Maternal death capture level",   "Maternal deaths captured (q13a=1) and level of capture is blank (q13a2)",
  "cond_q14a",          "q13a / q14a",     "Maternal death definition",      "Maternal deaths captured (q13a=1) and definition is blank (q14a)",
  
  # Stillbirth capture
  "cond_q13b",          "q13b",            "Stillbirth captured",            "Stillbirths captured is blank (q13b)",
  "cond_neg_q13bwhy",   "q13b / q13bwhy",  "Stillbirth captured why",        "Stillbirths not captured (q13b=0) and reason why is blank (q13bwhy)",
  "cond_q13b1_ca",      "q13b / q13b1",    "Stillbirth captured since year", "Stillbirths captured (q13b=1) and year that capture began is blank (q13b1)",
  "cond_q13b1_cb",      "q13b / q13b1",    "Stillbirth captured since year", "Stillbirths captured (q13b=1) and year that capture began is < 1990 or > 2026 and does not equal the designated missing value of 9999 (q13b1)",
  "cond_q13b2",         "q13b / q13b2",    "Stillbirth capture level",       "Stillbirths captured (q13b=1) and level of capture is blank (q13b2)",
  "cond_q14b",          "q13b / q14b",     "Stillbirth definition",           "Stillbirths captured (q13b=1) and definition is blank (q14b)",
  
  # Neonatal death capture
  "cond_q13c",          "q13c",            "Neonatal death captured",        "Neonatal deaths captured is blank (q13c)",
  "cond_neg_q13cwhy",   "q13c / q13cwhy",  "Neonatal death captured why",    "Neonatal deaths not captured (q13c=0) and reason why is blank (q13cwhy)",
  "cond_q13c1_ca",      "q13c / q13c1",    "Neonatal death since year",      "Neonatal deaths captured (q13c=1) and year that capture began is blank (q13c1)",
  "cond_q13c1_cb",      "q13c / q13c1",    "Neonatal death since year",      "Neonatal deaths captured (q13c=1) and year that capture began is < 1990 or > 2026 and does not equal the designated missing value of 9999 (q13c1)",
  "cond_q13c2",         "q13c / q13c2",    "Neonatal death capture level",   "Neonatal deaths captured (q13c=1) and level of capture is blank (q13c2)",
  "cond_q14c",          "q13c / q14c",     "Neonatal death definition",      "Neonatal deaths captured (q13c=1) and definition is blank (q14c)",
  
  # Child death capture
  "cond_q13d",          "q13d",            "Child death captured",           "Child deaths captured is blank (q13d)",
  "cond_neg_q13dwhy",   "q13d / q13dwhy",  "Child death captured why",       "Child deaths not captured (q13d=0) and reason why is blank (q13dwhy)",
  "cond_q13d1_ca",      "q13d / q13d1",    "Child death since year",         "Child deaths captured (q13d=1) and year that capture began is blank (q13d1)",
  "cond_q13d1_cb",      "q13d / q13d1",    "Child death since year",         "Child deaths captured (q13d=1) and year that capture began is < 1990 or > 2026 and does not equal the designated missing value of 9999 (q13d1)",
  "cond_q13d2",         "q13d / q13d2",    "Child death capture level",      "Child deaths captured (q13d=1) and level of capture is blank (q13d2)",
  "cond_q14d",          "q13d / q14d",     "Child death definition",         "Child deaths captured (q13d=1) and definition is blank (q14d)",
  
  # MPCDSR training, mentoring, supervision, electronic or paper-based
  "cond_q15",      "q15",           "MPCDSR training provided",        "MPCDSR training provided is blank (q15)",
  "cond_neg_q15a", "q15 / q15a",    "Number trained in MPCDSR",        "MPCDSR training not provided (q15=0) and number trained is not blank (q15a)",
  "cond_q15a",     "q15 / q15a",    "Number trained in MPCDSR",        "MPCDSR training provided (q15!=0) and number trained is blank (q15a)",
  "cond_q16",      "q16",           "Mentoring/coaching past 12m",     "Mentoring/coaching in past 12m is blank (q16)",
  "cond_q17",      "q17",           "Supervision visits past 12m",     "Supervision visits in past 12m is blank (q17)",
  "cond_neg_q17a", "q17 / q17a",    "Components of supervision",       "No supervision visits (q17=0) and components is not blank (q17a)",
  "cond_q17a",     "q17 / q17a",    "Components of supervision",       "Supervision visits occurred (q17!=0) and components is blank (q17a)",
  "cond_q17aoth",  "q17a / q17aoth","Components of supervision other", "Other selected for supervision components (q17a=96) but q17aoth is blank",
  "cond_q18",      "q18",           "Electronic or paper-based",       "Electronic or paper-based is blank (q18)",
  "cond_q19",      "q18 / q19",     "Which electronic database",       "System is electronic (q18=2|3) and database is blank (q19)",
  "cond_q19oth",   "q19 / q19oth",  "Which electronic database other", "Other selected for database (q19=96) but q19oth is blank",
  
  # How identified
  "cond_q21m",          "q21m",             "How are deaths identified",       "How are deaths identified is blank (q21m)",
  "cond_q21oth",        "q21m / q21oth",    "How are deaths identified other", "Other selected for how deaths identified (q21m=96) but q21oth is blank",  
  
  # How soon notified facility deaths
  "cond_q22a",          "q13a2 / q22a",    "How soon facility maternal deaths",       "Facility maternal deaths captured (q13a2=1) and how soon notified is blank (q22a)",
  "cond_q22aoth",       "q22a / q22aoth",  "How soon facility maternal deaths other", "Other selected for how soon maternal deaths notified (q22a=96) but q22aoth is blank",
  "cond_q22b",          "q13b2 / q22b",    "How soon facility stillbirths",            "Facility stillbirths captured (q13b2=1) and how soon notified is blank (q22b)",
  "cond_q22both",       "q22b / q22both",  "How soon facility stillbirths other",     "Other selected for how soon stillbirths notified (q22b=96) but q22both is blank",
  "cond_q22c",          "q13c2 / q22c",    "How soon facility neonatal deaths",       "Facility neonatal deaths captured (q13c2=1) and how soon notified is blank (q22c)",
  "cond_q22coth",       "q22c / q22coth",  "How soon facility neonatal deaths other", "Other selected for how soon neonatal deaths notified (q22c=96) but q22coth is blank",
  "cond_q22d",          "q13d2 / q22d",    "How soon facility child deaths",          "Facility child deaths captured (q13d2=1) and how soon notified is blank (q22d)",
  "cond_q22doth",       "q22d / q22doth",  "How soon facility child deaths other",    "Other selected for how soon child deaths notified (q22d=96) but q22doth is blank",
  
  # How soon notified community deaths
  "cond_q23a",          "q13a2 / q23a",    "How soon community maternal deaths",       "Community maternal deaths captured (q13a2=2) and how soon notified is blank (q23a)",
  "cond_q23aoth",       "q23a / q23aoth",  "How soon community maternal deaths other", "Other selected for how soon maternal deaths notified (q23a=96) but q23aoth is blank",
  "cond_q23b",          "q13b2 / q23b",    "How soon community stillbiths",            "Community stillbirths captured (q13b2=2) and how soon notified is blank (q23b)",
  "cond_q23both",       "q23b / q23both",  "How soon community stillbirths other",     "Other selected for how soon stillbirths notified (q23b=96) but q23both is blank",
  "cond_q23c",          "q13c2 / q23c",    "How soon community neonatal deaths",       "Community neonatal deaths captured (q13c2=2) and how soon notified is blank (q23c)",
  "cond_q23coth",       "q23c / q23coth",  "How soon community neonatal deaths other", "Other selected for how soon neonatal deaths notified (q23c=96) but q23coth is blank",
  "cond_q23d",          "q13d2 / q23d",    "How soon community child deaths",          "Community child deaths captured (q13d2=2) and how soon notified is blank (q23d)",
  "cond_q23doth",       "q23d / q23doth",  "How soon community child deaths other",    "Other selected for how soon child deaths notified (q23d=96) but q23doth is blank",
  
  # Who is notified, audits, committees
  "cond_q24m",          "q24m",            "Who notified of deaths",        "Who notified of deaths is blank (q24m)",
  "cond_q24oth",        "q24 / q24oth",    "Who notified of deaths other",  "Other selected for who notified of deaths (q24m=96) but q24oth is blank",
  "cond_q25",           "q25",             "Deaths reviewed or audited",    "Deaths reviewed or audited is blank (q25)",
  "cond_q25a",          "q25 / q25a",      "Which deaths reviewed or audited", "Deaths reviewed or audited (q25=1) and q25a is blank",
  "cond_q26m",          "q26m",            "Which review committees",          "Which review committees is blank (q26m)",
  "cond_q26moth",       "q26m / q26moth",  "Which review committees other",  "Other selected for committees to review (q26m=96) but q26moth is blank",
  "cond_q26b",          "q26b",            "Established committees at higher level", "Established committees at higher level is blank (q26b)",
  "cond_q26both",       "q26b / q26both",  "Established committees at higher level other",  "Other selected for established committees at higher level (q26b=96) but q26both is blank",
  
  # Code of conduct
  "cond_q211",          "q211",            "Code of conduct",            "Code of conduct is blank (q211)",
  "cond_q211oth",       "q211 / q211oth",  "Code of conduct photo",      "Code of conduct observed (q211=2) and photo is blank (q211oth)",
  "cond_neg_q212",      "q211 / q212",     "Meeting minutes",            "Code of conduct is not observed (q211!=2) and q212 is not blank.",
  "cond_q212",          "q211 / q212",     "Meeting minutes",            "Code of conduct is observed (q211=2) and q212 is blank.",
  "cond_neg_q213",      "q211 / q213",     "Financial support",          "Code of conduct is not observed (q211!=2) and q213 is not blank.",
  "cond_q213",          "q211 / q213",     "Financial support",          "Code of conduct is observed (q211=2) and q213 is blank.",  
  "cond_neg_q213a",     "q213 / q213a",    "Financial support source",   "Financial support is not provided (q213!=1) and q213a is not blank.",  
  "cond_q213a",         "q213 / q213a",    "Financial support source",   "Financial support is provided (q213=1) and q213a is blank.",   
  "cond_neg_q213b",     "q213 / q213b",    "Financial support use",      "Financial support is not provided (q213!=1) and q213b is not blank.",  
  "cond_q213b",         "q213 / q213b",    "Financial support use",      "Financial support is provided (q213=1) and q213b is blank.",    

  # Community-based deaths
  "cond_q214",          "q214",            "Investigation of community deaths",   "Investigation of community deaths is blank (q214)",
  "cond_neg_q214a",     "q214 / q214a",    "Which community deaths investigated", "Community deaths not investigated (q214!=1) and q214a is not blank",
  "cond_q214a",         "q214 / q214a",    "Which community deaths investigated", "Community deaths are investigated (q214=1) and q214a is blank",
  
  # Community-based maternal deaths
  "cond_neg_q215a",     "q214a / q215a",    "How many community maternal deaths investigated", "Maternal deaths not investigated (q214a!=1) and q215a is not blank",
  "cond_q215a",         "q214a / q215a",    "How many community maternal deaths investigated", "Maternal deaths investigated (q214a=1) and q215a is blank",
  "cond_q215aoth",      "q215a / q215aoth", "How many community maternal deaths investigated other", "Other selected for how maternal deaths investigated (q215a=96) but q215aoth is blank",
  "cond_neg_q215b",    "q214a / q215b",     "Who does VASA for community maternal deaths", "Maternal deaths not investigated (q214a!=1) and q215b is not blank",
  "cond_q215b",        "q214a / q215b",     "Who does VASA for community maternal deaths", "Maternal deaths investigated (q214a=1) and q215b is blank",
  "cond_q215cngo",     "q215b / q215cngo",  "NGO rep maternal death VASA",        "Other NGO conducts VASA for maternal deaths (q215b=15) and q215cngo is blank",
  "cond_q215cnun",     "q215b / q215cnun",  "UN rep maternal death VASA",         "Other UN conducts VASA for maternal deaths (q215b=16) and q215cnun is blank",
  "cond_q215cnlmoh",   "q215b / q215cnlmoh","LGA MOH rep maternal death VASA",    "Other LGA MOH conducts VASA for maternal deaths (q215b=17) and q215cnlmoh is blank",
  "cond_q215cnsmoh",   "q215b / q215cnsmoh","State MOH maternal death VASA",      "Other state MOH conducts VASA for maternal deaths (q215b=18) and q215cnsmoh is blank",
  "cond_q215cnnmoh",   "q215b / q215cnnmoh","National MOH maternal death VASA",   "National MOH conducts VASA for maternal deaths (q215b=19) and q215cnnmoh is blank",
  "cond_q215cnoth",    "q215b / q215cnoth", "Other maternal death VASA",          "Other source conducts VASA for maternal deaths (q215b=96) and q215cnoth is blank",
  
  # Community-based stillbirths
  "cond_neg_q216a", "q214a / q216a", "How community stillbirths investigated", "Stillbirths not investigated (q214a!=2) and q216a is not blank",
  "cond_q216a",     "q214a / q216a", "How community stillbirths investigated", "Stillbirths investigated (q214a=2) and q216a is blank",
  "cond_q216both",  "q216a / q216both", "How community stillbirths investigated other", "Other selected for how stillbirths investigated (q216a=96) but q216both is blank",
  "cond_neg_q216b", "q214a / q216b", "VASA for community stillbirths", "Stillbirths not investigated (q214a!=2) and q216b is not blank",
  "cond_q216b",     "q214a / q216b", "VASA for community stillbirths", "Stillbirths investigated (q214a=2) and q216b is blank",
  "cond_q216bngo",  "q216b / q216bngo", "NGO rep stillbirth VASA", "Other NGO conducts VASA for stillbirths (q216b=15) and q216bngo is blank",
  "cond_q216bnun",  "q216b / q216bnun", "UN rep stillbirth VASA", "Other UN conducts VASA for stillbirths (q216b=16) and q216bnun is blank",
  "cond_q216bnlmoh","q216b / q216bnlmoh", "LGA MOH rep stillbirth VASA", "Other LGA MOH conducts VASA for stillbirths (q216b=17) and q216bnlmoh is blank",
  "cond_q216bnsmoh","q216b / q216bnsmoh", "State MOH stillbirth VASA", "Other state MOH conducts VASA for stillbirths (q216b=18) and q216bnsmoh is blank",
  "cond_q216bnnmoh","q216b / q216bnnmoh", "National MOH stillbirth VASA", "National MOH conducts VASA for stillbirths (q216b=19) and q216bnnmoh is blank",
  "cond_q216bnoth", "q216b / q216bnoth", "Other stillbirth VASA", "Other source conducts VASA for stillbirths (q216b=96) and q216bnoth is blank",
  
  # Community-based neonatal deaths
  "cond_neg_q217a", "q214a / q217a", "How community neonatal deaths investigated", "Neonatal deaths not investigated (q214a!=3) and q217a is not blank",
  "cond_q217a",     "q214a / q217a", "How community neonatal deaths investigated", "Neonatal deaths investigated (q214a=3) and q217a is blank",
  "cond_q217both",  "q217a / q217both", "How community neonatal deaths investigated other", "Other selected for how neonatal deaths investigated (q217a=96) but q217both is blank",
  "cond_neg_q217b", "q214a / q217b", "VASA for community neonatal deaths", "Neonatal deaths not investigated (q214a!=3) and q217b is not blank",
  "cond_q217b",     "q214a / q217b", "VASA for community neonatal deaths", "Neonatal deaths investigated (q214a=3) and q217b is blank",
  "cond_q217bngo",  "q217b / q217bngo", "NGO rep neonatal death VASA", "Other NGO conducts VASA for neonatal deaths (q217b=15) and q217bngo is blank",
  "cond_q217bnun",  "q217b / q217bnun", "UN rep neonatal death VASA", "Other UN conducts VASA for neonatal deaths (q217b=16) and q217bnun is blank",
  "cond_q217bnlmoh","q217b / q217bnlmoh", "LGA MOH rep neonatal death VASA", "Other LGA MOH conducts VASA for neonatal deaths (q217b=17) and q217bnlmoh is blank",
  "cond_q217bnsmoh","q217b / q217bnsmoh", "State MOH neonatal death VASA", "Other state MOH conducts VASA for neonatal deaths (q217b=18) and q217bnsmoh is blank",
  "cond_q217bnnmoh","q217b / q217bnnmoh", "National MOH neonatal death VASA", "National MOH conducts VASA for neonatal deaths (q217b=19) and q217bnnmoh is blank",
  "cond_q217bnoth", "q217b / q217bnoth", "Other neonatal death VASA", "Other source conducts VASA for neonatal deaths (q217b=96) and q217bnoth is blank",
  
  # Community-based child deaths
  "cond_neg_q218a", "q214a / q218a", "How community child deaths investigated", "Child deaths not investigated (q214a!=4) and q218a is not blank",
  "cond_q218a",     "q214a / q218a", "How community child deaths investigated", "Child deaths investigated (q214a=4) and q218a is blank",
  "cond_q218both",  "q218a / q218both", "How community child deaths investigated other", "Other selected for how child deaths investigated (q218a=96) but q218both is blank",
  "cond_neg_q218b", "q214a / q218b", "VASA for community child deaths", "Child deaths not investigated (q214a!=4) and q218b is not blank",
  "cond_q218b",     "q214a / q218b", "VASA for community child deaths", "Child deaths investigated (q214a=4) and q218b is blank",
  "cond_q218bngo",  "q218b / q218bngo", "NGO rep child death VASA", "Other NGO conducts VASA for child deaths (q218b=15) and q218bngo is blank",
  "cond_q218bnun",  "q218b / q218bnun", "UN rep child death VASA", "Other UN conducts VASA for child deaths (q218b=16) and q218bnun is blank",
  "cond_q218bnlmoh","q218b / q218bnlmoh", "LGA MOH rep child death VASA", "Other LGA MOH conducts VASA for child deaths (q218b=17) and q218bnlmoh is blank",
  "cond_q218bnsmoh","q218b / q218bnsmoh", "State MOH child death VASA", "Other state MOH conducts VASA for child deaths (q218b=18) and q218bnsmoh is blank",
  "cond_q218bnnmoh","q218b / q218bnnmoh", "National MOH child death VASA", "National MOH conducts VASA for child deaths (q218b=19) and q218bnnmoh is blank",
  "cond_q218bnoth", "q218b / q218bnoth", "Other child death VASA", "Other source conducts VASA for child deaths (q218b=96) and q218bnoth is blank",

  # Financial support for VASA
  "cond_q219",      "q214 / q219",  "Financial support for VASA",       "Community deaths investigated (q214=1) and financial support is blank (q219)",
  "cond_neg_q219a", "q219 / q219a", "Financial support source for VASA","Financial support not provided (q219!=1) and q219a is not blank.",
  "cond_q219a",     "q219 / q219a", "Financial support source for VASA","Financial support is provided (q219=1) and q219a is blank.",
  "cond_neg_q219b", "q219 / q219b", "Financial support use for VASA",   "Financial support not provided (q219!=1) and q219b is not blank.",
  "cond_q219b",     "q219 / q219b", "Financial support use for VASA",   "Financial support is provided (q219=1) and q219b is blank.",
  
  ## Section: Analysis and recommendations
  
  # Classification system for cause of death
  "cond_q220",      "q220",           "Classification system for cause of death",       "Classification system is blank (q220)",
  "cond_q220oth",   "q220 / q220oth", "Classification system for cause of death other", "Other selected for classification system (q220=96) but q220oth is blank",
  
  # Components addressed during death reviews
  "cond_neg_q221",  "q25 / q221",     "Components addressed in death reviews",          "Deaths not reviewed/audited (q25!=1) and q221 is not blank",
  "cond_q221",      "q25 / q221",     "Components addressed in death reviews",          "Deaths reviewed/audited (q25=1) and q221 is blank",
  "cond_q221oth",   "q221 / q221oth", "Components addressed in death reviews other",    "Other selected for components addressed (q221=96) but q221oth is blank",
   
  # Death review documentation
  # Maternal deaths
  "cond_neg_q222a", "q25a / q222a", "How maternal death reviews documented", "Maternal deaths not reviewed (q25a!=1) and q222a is not blank",
  "cond_q222a",     "q25a / q222a", "How maternal death reviews documented", "Maternal deaths reviewed (q25a=1) and q222a is blank",
  "cond_q222aoth",  "q222a / q222aoth", "How maternal death reviews documented other", "Other selected for how documented (q222a=96) but q222aoth is blank",
  "cond_q222a1",    "q222a / q222a1", "Photo of maternal mortality form", "Standard method selected (q222a!=96) but photo is blank (q222a1)",
  # Stillbirths
  "cond_neg_q222b", "q25a / q222b", "How stillbirth reviews documented", "Stillbirths not reviewed (q25a!=2) and q222b is not blank",
  "cond_q222b",     "q25a / q222b", "How stillbirth reviews documented", "Stillbirths reviewed (q25a=2) and q222b is blank",
  "cond_q222both",  "q222b / q222both", "How stillbirth reviews documented other", "Other selected for how documented (q222b=96) but q222both is blank",
  "cond_q222b1",    "q222b / q222b1", "Photo of stillbirth form", "Standard method selected (q222b!=96) but photo is blank (q222b1)",
  # Neonatal deaths
  "cond_neg_q222c", "q25a / q222c", "How neonatal death reviews documented", "Neonatal deaths not reviewed (q25a!=3) and q222c is not blank",
  "cond_q222c",     "q25a / q222c", "How neonatal death reviews documented", "Neonatal deaths reviewed (q25a=3) and q222c is blank",
  "cond_q222coth",  "q222c / q222coth", "How neonatal death reviews documented other", "Other selected for how documented (q222c=96) but q222coth is blank",
  "cond_q222c1",    "q222c / q222c1", "Photo of neonatal death form", "Standard method selected (q222c!=96) but photo is blank (q222c1)",
  # Child deaths
  "cond_neg_q222d", "q25a / q222d", "How child death reviews documented", "Child deaths not reviewed (q25a!=4) and q222d is not blank",
  "cond_q222d",     "q25a / q222d", "How child death reviews documented", "Child deaths reviewed (q25a=4) and q222d is blank",
  "cond_q222doth",  "q222d / q222doth", "How child death reviews documented other", "Other selected for how documented (q222d=96) but q222doth is blank",
  "cond_q222d1",    "q222d / q222d1", "Photo of child death form", "Standard method selected (q222d!=96) but photo is blank (q222d1)",
  
  # Death case report preparation and dissemination
  "cond_q223",         "q223",              "Death case report prepared",         "Death case report prepared is blank (q223)",
  "cond_neg_q224a",    "q223 / q224a",      "When report prepared",               "Report not prepared (q223!=1) and q224a is not blank",
  "cond_q224a",        "q223 / q224a",      "When report prepared",               "Report prepared (q223=1) and q224a is blank",
  "cond_neg_q224b",    "q223 / q224b",      "Who prepares report",                "Report not prepared (q223!=1) and q224b is not blank",
  "cond_q224b",        "q223 / q224b",      "Who prepares report",                "Report prepared (q223=1) and q224b is blank",
  "cond_q224bngo",     "q224b / q224bngo",  "NGO rep prepares report",             "NGO/Managing Authority Rep selected (q224b=15) and q224bngo is blank",
  "cond_q224bnun",     "q224b / q224bnun",  "UN rep prepares report",              "UN Agency Rep selected (q224b=16) and q224bnun is blank",
  "cond_q224bnlmoh",   "q224b / q224bnlmoh","LGA MOH rep prepares report",         "LGA MoH Rep selected (q224b=17) and q224bnlmoh is blank",
  "cond_q224bnsmoh",   "q224b / q224bnsmoh","State MOH rep prepares report",       "State MoH Rep selected (q224b=18) and q224bnsmoh is blank",
  "cond_q224bnnmoh",   "q224b / q224bnnmoh","National MOH rep prepares report",    "National MoH Rep selected (q224b=19) and q224bnnmoh is blank",
  "cond_q224bnoth",    "q224b / q224bnoth", "Other rep prepares report",           "Other selected for who prepares report (q224b=96) and q224bnoth is blank",
  "cond_neg_q225",     "q223 / q225",       "Who results shared with",            "Report not prepared (q223!=1) and q225 is not blank",
  "cond_q225",         "q223 / q225",       "Who results shared with",            "Report prepared (q223=1) and q225 is blank",
  "cond_q225oth",      "q225 / q225oth",    "Who results shared with other",       "Other selected for who results shared with (q225=96) and q225oth is blank",
  
  # Documentation of VASA
  # Maternal
  "cond_neg_q226a", "q214a / q226a", "How maternal VASA documented", "Maternal deaths not investigated (q214a!=1) and q226a is not blank",
  "cond_q226a",     "q214a / q226a", "How maternal VASA documented", "Maternal deaths investigated (q214a=1) and q226a is blank",
  "cond_q226aoth",  "q226a / q226aoth", "How maternal VASA documented other", "Other selected for how documented (q226a=96) but q226aoth is blank",
  "cond_q226a1i",   "q226a / q226a1i", "Photo of maternal VASA form", "Standard method selected (q226a!=96) but photo is blank (q226a1i)",
  # Stillbirth
  "cond_neg_q226b", "q214a / q226b", "How stillbirth VASA documented", "Stillbirths not investigated (q214a!=2) and q226b is not blank",
  "cond_q226b",     "q214a / q226b", "How stillbirth VASA documented", "Stillbirths investigated (q214a=2) and q226b is blank",
  "cond_q226both",  "q226b / q226both", "How stillbirth VASA documented other", "Other selected for how documented (q226b=96) but q226both is blank",
  "cond_q226b1i",   "q226b / q226b1i", "Photo of stillbirth VASA form", "Standard method selected (q226b!=96) but photo is blank (q226b1i)",
  # Neonatal
  "cond_neg_q226c", "q214a / q226c", "How neonatal VASA documented", "Neonatal deaths not investigated (q214a!=3) and q226c is not blank",
  "cond_q226c",     "q214a / q226c", "How neonatal VASA documented", "Neonatal deaths investigated (q214a=3) and q226c is blank",
  "cond_q226coth",  "q226c / q226coth", "How neonatal VASA documented other", "Other selected for how documented (q226c=96) but q226coth is blank",
  "cond_q226c1i",   "q226c / q226c1i", "Photo of neonatal VASA form", "Standard method selected (q226c!=96) but photo is blank (q226c1i)",
  # Child
  "cond_neg_q226d", "q214a / q226d", "How child VASA documented", "Child deaths not investigated (q214a!=4) and q226d is not blank",
  "cond_q226d",     "q214a / q226d", "How child VASA documented", "Child deaths investigated (q214a=4) and q226d is blank",
  "cond_q226doth",  "q226d / q226doth", "How child VASA documented other", "Other selected for how documented (q226d=96) but q226doth is blank",
  "cond_q226d1i",   "q226d / q226d1i", "Photo of child VASA form", "Standard method selected (q226d!=96) but photo is blank (q226d1i)",
  
  # Components and reporting of VASA
  "cond_neg_q227",  "q214 / q227",   "Components addressed in VASA",        "Community deaths not investigated (q214!=1) and q227 is not blank",
  "cond_q227",      "q214 / q227",   "Components addressed in VASA",        "Community deaths investigated (q214=1) and q227 is blank",
  "cond_q227oth",   "q227 / q227oth","Components addressed in VASA other",  "Other selected for components (q227=96) but q227oth is blank",
  "cond_neg_q228",  "q214 / q228",   "VASA report prepared",                "Community deaths not investigated (q214!=1) and q228 is not blank",
  "cond_q228",      "q214 / q228",   "VASA report prepared",                "Community deaths investigated (q214=1) and q228 is blank",
  "cond_neg_q229",  "q214 / q229",   "Who VASA results shared with",        "Community deaths not investigated (q214!=1) and q229 is not blank",
  "cond_q229",      "q214 / q229",   "Who VASA results shared with",        "Community deaths investigated (q214=1) and q229 is blank",
  "cond_q229oth",   "q229 / q229oth","Who VASA results shared with other",  "Other selected for who results shared with (q229=96) and q229oth is blank",
  
  # Follow-up on death reviews
  "cond_neg_q230",  "q25 / q214 / q230", "Individuals assigned to follow up",   "Deaths not reviewed and/or community deaths not investigated (q25!=1 or q214!=1) and q230 is not blank",
  "cond_q230",      "q25 / q214 / q230", "Individuals assigned to follow up",   "Deaths reviewed and community deaths investigated (q25=1 and q214=1) and q230 is blank",
  "cond_neg_q230a", "q230 / q230a",      "Person's role for follow-up",         "No individual assigned (q230!=1) and q230a is not blank",
  "cond_q230a",     "q230 / q230a",       "Person's role for follow-up",         "Individual assigned (q230=1) and q230a is blank",
  "cond_neg_q231",  "q25 / q214 / q231", "Systematic reporting-back process",   "Deaths not reviewed and/or community deaths not investigated (q25!=1 or q214!=1) and q231 is not blank",
  "cond_q231",      "q25 / q214 / q231", "Systematic reporting-back process",   "Deaths reviewed and community deaths investigated (q25=1 and q214=1) and q231 is blank",
  "cond_neg_q232",  "q25 / q214 / q232", "Tracking system for recommendations", "Deaths not reviewed and/or community deaths not investigated (q25!=1 or q214!=1) and q232 is not blank",
  "cond_q232",      "q25 / q214 / q232", "Tracking system for recommendations", "Deaths reviewed and community deaths investigated (q25=1 and q214=1) and q232 is blank",
  "cond_neg_q232a", "q232 / q232a",      "How tracking system works",           "No tracking system (q232!=1) and q232a is not blank",
  "cond_q232a",     "q232 / q232a",      "How tracking system works",           "Tracking system exists (q232=1) and q232a is blank",
  "cond_neg_q233",  "q25 / q214 / q233", "Financial support for recommendations", "Deaths not reviewed and/or community deaths not investigated (q25!=1 or q214!=1) and q233 is not blank",
  "cond_q233",      "q25 / q214 / q233", "Financial support for recommendations", "Deaths reviewed and community deaths investigated (q25=1 and q214=1) and q233 is blank",
  "cond_neg_q233a", "q233 / q233a",  "Funding source for recommendations",  "Financial support not provided (q233!=1) and q233a is not blank",
  "cond_q233a",     "q233 / q233a",  "Funding source for recommendations",  "Financial support provided (q233=1) and q233a is blank",
  "cond_neg_q233b", "q233 / q233b",  "Funding use for recommendations",     "Financial support not provided (q233!=1) and q233b is not blank",
  "cond_q233b",     "q233 / q233b",  "Funding use for recommendations",     "Financial support provided (q233=1) and q233b is blank",
  
  # Reporting completeness and timeliness
  "cond_neg_q234",  "q25 / q214 / q234", "Submits data to higher level",     "Deaths not reviewed and/or community deaths not investigated (q25!=1 or q214!=1) and q234 is not blank",
  "cond_q234",      "q25 / q214 / q234", "Submits data to higher level",     "Deaths reviewed and community deaths investigated (q25=1 and q214=1) and q234 is blank",
  
  # Aggregate reporting (submit_agg group)
  "cond_neg_q235",   "q234 / q235",   "Who prepares aggregate report",       "Facility does not submit data (q234!=1) and q235 is not blank",
  "cond_q235",       "q234 / q235",   "Who prepares aggregate report",       "Facility submits data (q234=1) and q235 is blank",
  "cond_q235ngo",    "q235 / q235ngo","NGO rep prepares aggregate report",   "NGO/Managing Authority Rep selected (q235=15) and q235ngo is blank",
  "cond_q235nun",    "q235 / q235nun","UN rep prepares aggregate report",    "UN Agency Rep selected (q235=16) and q235nun is blank",
  "cond_q235nlmoh",  "q235 / q235nlmoh","LGA MOH rep prepares aggregate report", "LGA MoH Rep selected (q235=17) and q235nlmoh is blank",
  "cond_q235nsmoh",  "q235 / q235nsmoh","State MOH rep prepares aggregate report", "State MoH Rep selected (q235=18) and q235nsmoh is blank",
  "cond_q235nnmoh",  "q235 / q235nnmoh","National MOH rep prepares aggregate report", "National MoH Rep selected (q235=19) and q235nnmoh is blank",
  "cond_q235noth",   "q235 / q235noth","Other rep prepares aggregate report", "Other selected for who prepares report (q235=96) and q235noth is blank",
  "cond_neg_q236",   "q234 / q236",   "Where facility reports aggregate data", "Facility does not submit data (q234!=1) and q236 is not blank",
  "cond_q236",       "q234 / q236",   "Where facility reports aggregate data", "Facility submits data (q234=1) and q236 is blank",
  "cond_neg_q237a",  "q234 / q237a",  "How often facility submits data",     "Facility does not submit data (q234!=1) and q237a is not blank",
  "cond_q237a",      "q234 / q237a",  "How often facility submits data",     "Facility submits data (q234=1) and q237a is blank",
  "cond_q237aoth",   "q237a / q237aoth","How often facility submits data other", "Other selected for submission frequency (q237a=96) but q237aoth is blank",
  "cond_neg_q238",   "q234 / q238",   "How data privacy maintained",         "Facility does not submit data (q234!=1) and q238 is not blank",
  "cond_q238",       "q234 / q238",   "How data privacy maintained",         "Facility submits data (q234=1) and q238 is blank",
  "cond_q238oth",    "q238 / q238oth","How data privacy maintained other",   "Other selected for data privacy method (q238=96) and q238oth is blank",
  
  ## Section: MPCDSR Data
  
  # MPCDSR data points collected/reported
  "cond_neg_q239",  "q25 / q239",   "Data points in death review form",   "Deaths not reviewed/audited (q25!=1) and q239 is not blank",
  "cond_q239",      "q25 / q239",   "Data points in death review form",   "Deaths reviewed/audited (q25=1) and q239 is blank",
  "cond_neg_q240",  "q214 / q240",  "Data points in VASA form",           "Community deaths not investigated (q214!=1) and q240 is not blank",
  "cond_q240",      "q214 / q240",  "Data points in VASA form",           "Community deaths investigated (q214=1) and q240 is blank",
  "cond_neg_q241",  "q234 / q241",  "Data points submitted to higher level", "Facility does not submit data (q234!=1) and q241 is not blank",
  "cond_q241",      "q234 / q241",  "Data points submitted to higher level", "Facility submits data (q234=1) and q241 is blank",
  
  ## After MaternPeriDeathRepeat?
  # I'm a little unclear on the structure here
  
  # Facility-based maternal death recommendations (past 12 months)
  "cond_q243a1i", "q243a1i", "Recommendations specified (maternal)",         "Recommendations specified is blank, non-numeric, or negative (q243a1i)",
  "cond_q243b1i", "q243b1i", "Recommendations implemented (maternal)",       "Recommendations implemented is blank, non-numeric, or negative (q243b1i)",
  "cond_q243c1i", "q243c1i", "Implemented <3 months (maternal)",             "Implemented in <3 months is blank, non-numeric, or negative (q243c1i)",
  "cond_q243d1i", "q243d1i", "Implemented 3-6 months (maternal)",            "Implemented in 3-6 months is blank, non-numeric, or negative (q243d1i)",
  "cond_q243e1i", "q243e1i", "Implemented >6 months (maternal)",             "Implemented in >6 months is blank, non-numeric, or negative (q243e1i)",
  
  # Facility-based neonatal death recommendations (past 12 months)
  "cond_q244a1i", "q244a1i", "Recommendations specified (neonatal)",         "Recommendations specified is blank, non-numeric, or negative (q244a1i)",
  "cond_q244b1i", "q244b1i", "Recommendations implemented (neonatal)",       "Recommendations implemented is blank, non-numeric, or negative (q244b1i)",
  "cond_q244c1i", "q244c1i", "Implemented <3 months (neonatal)",             "Implemented in <3 months is blank, non-numeric, or negative (q244c1i)",
  "cond_q244d1i", "q244d1i", "Implemented 3-6 months (neonatal)",            "Implemented in 3-6 months is blank, non-numeric, or negative (q244d1i)",
  "cond_q244e1i", "q244e1i", "Implemented >6 months (neonatal)",             "Implemented in >6 months is blank, non-numeric, or negative (q244e1i)",
  
  # Facility-based stillbirth recommendations (past 12 months)
  "cond_q245a1i", "q245a1i", "Recommendations specified (stillbirth)",       "Recommendations specified is blank, non-numeric, or negative (q245a1i)",
  "cond_q245b1i", "q245b1i", "Recommendations implemented (stillbirth)",     "Recommendations implemented is blank, non-numeric, or negative (q245b1i)",
  "cond_q245c1i", "q245c1i", "Implemented <3 months (stillbirth)",           "Implemented in <3 months is blank, non-numeric, or negative (q245c1i)",
  "cond_q245d1i", "q245d1i", "Implemented 3-6 months (stillbirth)",          "Implemented in 3-6 months is blank, non-numeric, or negative (q245d1i)",
  "cond_q245e1i", "q245e1i", "Implemented >6 months (stillbirth)",           "Implemented in >6 months is blank, non-numeric, or negative (q245e1i)",
  
  ## Section: Integration with Quality Improvement and other surveillance systems
  
  # Quality improvement and MPCDSR system integration
  "cond_q246",      "q246",           "QI strategy/program exists",       "QI strategy/program is blank (q246)",
  "cond_q247",      "q247",           "QI committee exists",              "QI committee is blank (q247)",
  "cond_q248",      "q248",           "QI committee shares members with MPCDSR", "Shares members with MPCDSR is blank (q248)",
  "cond_q249",      "q249",           "Single staff position for QI",     "Single staff position is blank (q249)",
  
  "cond_q250",      "q250",           "MPCDSR linked to other systems",   "MPCDSR linked to other systems is blank (q250)",
  "cond_q250oth1",  "q250 / q250oth1","Other system 1 specify",           "Other system 1 selected (q250=961) but q250oth1 is blank",
  "cond_q250oth2",  "q250 / q250oth2","Other system 2 specify",           "Other system 2 selected (q250=962) but q250oth2 is blank",
  
  "cond_neg_q251a", "q250 / q251a",   "How linked to CRVS",                "CRVS not selected (q250!=1) and q251a is not blank",
  "cond_q251a",     "q250 / q251a",   "How linked to CRVS",                "CRVS selected (q250=1) and q251a is blank",
  "cond_neg_q251b", "q250 / q251b",   "How linked to IDSR",                "IDSR not selected (q250!=2) and q251b is not blank",
  "cond_q251b",     "q250 / q251b",   "How linked to IDSR",                "IDSR selected (q250=2) and q251b is blank",
  "cond_neg_q251c", "q250 / q251c",   "How linked to other system 1",      "Other system 1 not selected (q250!=961) and q251c is not blank",
  "cond_q251c",     "q250 / q251c",   "How linked to other system 1",      "Other system 1 selected (q250=961) and q251c is blank",
  "cond_neg_q251d", "q250 / q251d",   "How linked to other system 2",      "Other system 2 not selected (q250!=962) and q251d is not blank",
  "cond_q251d",     "q250 / q251d",   "How linked to other system 2",      "Other system 2 selected (q250=962) and q251d is blank",
  
  ## Section: Data quality and use
  
  # Data quality control and consistency checks
  "cond_q252",      "q252",           "Data quality control steps conducted", "Data quality control steps is blank (q252)",
  "cond_neg_q253a", "q252 / q253a",   "When cause of death verified",          "Cause of death step not selected (q252!=1) and q253a is not blank",
  "cond_q253a",     "q252 / q253a",   "When cause of death verified",          "Cause of death step selected (q252=1) and q253a is blank",
  "cond_q253aoth",  "q253a / q253aoth","When cause of death verified other",   "Other selected (q253a=96) but q253aoth is blank",
  "cond_neg_q253b", "q252 / q253b",   "When death review data verified",       "Death review data step not selected (q252!=2) and q253b is not blank",
  "cond_q253b",     "q252 / q253b",   "When death review data verified",       "Death review data step selected (q252=2) and q253b is blank",
  "cond_q253both",  "q253b / q253both","When death review data verified other", "Other selected (q253b=96) but q253both is blank",
  "cond_neg_q253c", "q252 / q253c",   "When VASA data verified",               "VASA data step not selected (q252!=3) and q253c is not blank",
  "cond_q253c",     "q252 / q253c",   "When VASA data verified",               "VASA data step selected (q252=3) and q253c is blank",
  "cond_q253coth",  "q253c / q253coth","When VASA data verified other",        "Other selected (q253c=96) but q253coth is blank",
  "cond_q254",      "q254",           "Plausibility checks (indicators)",       "Plausibility checks on indicators is blank (q254)",
  "cond_q254oth",   "q254 / q254oth", "Plausibility checks (indicators) other", "Other selected for indicator checks (q254=96) and q254oth is blank",
  "cond_q255",      "q255",           "Plausibility checks (cause of death)",   "Plausibility checks on cause of death is blank (q255)",
  "cond_q255oth",   "q255 / q255oth", "Plausibility checks (cause of death) other", "Other selected for cause of death checks (q255=96) and q255oth is blank",
  "cond_q256",      "q256",           "Data quality assessments past 12m",      "Data quality assessments is blank (q256)",
  "cond_q257",      "q257",           "Data visuals available on-site",         "Data visuals available is blank (q257)"
  
)

conditions <- mget(check_defs$cond_name)

# format individual-level results
tool_results <- check_defs %>%
  pmap_dfr(function(cond_name, question, label, issue) {
    flagged <- which(conditions[[cond_name]])
    if (length(flagged) == 0) return(NULL)
    tibble(
      KEY = dat$KEY[flagged],
      g03 = dat$g03[flagged],
      question       = question,
      label          = label,
      issue          = issue
    )
  })

# summary table
tool_results %>%
  select(question, label, issue) %>%
  group_by(question, label, issue) %>%
  summarise(n = n()) %>%
  compact_kable(caption = "Query summary table for section 1 ")


# Save excel file ---------------------------------------------------------

# save individual level results as excel file
var_key <- tibble::tribble(
  ~variable,      ~description,
  "key",       "KEY to identify observation",
  "g03",  "interviewer id",
  "question",   "question number in tool",
  "label",    "question label",
  "issue",       "description of issue"
)

wb <- createWorkbook()
addWorksheet(wb, "toolC-main")
writeData(wb, "toolC-main", tool_results)

addWorksheet(wb, "variable-key")
writeData(wb, "variable-key", var_key)
setColWidths(wb, "variable-key", cols = 1:2, widths = c(18, 100))

fn_save_dated_workbook(wb, "data-queries-toolC-main")
