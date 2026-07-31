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
#' Inputs
toolc_tables <- readRDS("./data/facilityMNCH-toolC.rds")
# dat <- toolc_tables$pom_tool_c
# dat_q210 <- toolc_tables$`pom_tool_c-dth_review_repeat`
# dat_q237 <- toolc_tables$`pom_tool_c-q237a_repeat`
# dat_q249 <- toolc_tables$`pom_tool_c-MaternPeriDeathRepeat`
# dat_q254 <- toolc_tables$`pom_tool_c-q254oth_repeat`
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

# MPCDSR focal point, training, supervision, electronic or paper-based
cond_q15_ca <- (is.na(dat$q15) | trimws(dat$q15) == "")
cond_q15_cb <- !is.na(dat$q15) & 
  grepl("(^|\\s)0(\\s|$)", dat$q15) &  # row has "0" as a standalone code
  grepl("\\s", dat$q15)                # row has more than one code (contains a space)
cond_q16 <- (is.na(dat$q16) | trimws(dat$q16) == "")
cond_neg_q16a <- !is.na(dat$q16) & dat$q16 == "0" & !is.na(dat$q16a)
cond_q17 <- (is.na(dat$q17) | trimws(dat$q17) == "")
cond_q18 <- (is.na(dat$q18) | trimws(dat$q18) == "")
cond_q19    <- !is.na(dat$q18) & dat$q18 == "1" & (is.na(dat$q19) | trimws(dat$q19) == "")
cond_q19oth <- !is.na(dat$q19) & grepl("(^|\\s)96(\\s|$)", dat$q19) & # row has "96" as a standalone code
  (is.na(dat$q19oth) | trimws(dat$q19oth) == "")
cond_q110 <- (is.na(dat$q110) | trimws(dat$q110) == "")
cond_neg_q111_ca <- !is.na(dat$q110) & dat$q110 == "1" & !is.na(dat$q111)
cond_neg_q111_cb <- !is.na(dat$q110) & dat$q110 == "4" & !is.na(dat$q111)
cond_q111 <- !is.na(dat$q110) & dat$q110 %in% c(2,3) & (is.na(dat$q111) | trimws(dat$q111) == "")
cond_q111oth <- !is.na(dat$q111) & grepl("(^|\\s)96(\\s|$)", dat$q111) & # row has "96" as a standalone code
  (is.na(dat$q111oth) | trimws(dat$q111oth) == "")

# How identified
cond_neg_q21 <- !is.na(dat$q110) & dat$q110 == "4" & !is.na(dat$q21)
cond_q21 <- (is.na(dat$q21) | trimws(dat$q21) == "")
cond_q21oth <- !is.na(dat$q21) & grepl("(^|\\s)96(\\s|$)", dat$q21) & # row has "96" as a standalone code
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
cond_q24 <- (is.na(dat$q24) | trimws(dat$q24) == "")
cond_q24oth <- !is.na(dat$q24) & grepl("(^|\\s)96(\\s|$)", dat$q24) &  # row has "96" as a standalone code
  (is.na(dat$q24oth) | trimws(dat$q24oth) == "")
cond_q25 <- (is.na(dat$q25) | trimws(dat$q25) == "")
cond_q25a <- !is.na(dat$q25) & dat$q25 == "1" & (is.na(dat$q25a) | trimws(dat$q25a) == "")
cond_q26 <- (is.na(dat$q26) | trimws(dat$q26) == "")
cond_q26oth <- !is.na(dat$q26) & grepl("(^|\\s)96(\\s|$)", dat$q26) &  # row has "96" as a standalone code
  (is.na(dat$q26oth) | trimws(dat$q26oth) == "")

# Code of conduct
cond_q211 <- (is.na(dat$q211) | trimws(dat$q211) == "")
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
#### !!!!! FLAG: SHOULD q215a, q215b, q215c BE ASKED IF q214a=1? rather than q214=1 as it says in the excel file
cond_neg_q215a <- !is.na(dat$q214a) & !grepl("(^|\\s)1(\\s|$)", dat$q214a) & !is.na(dat$q215a)
cond_q215a <-  !is.na(dat$q214a) & grepl("(^|\\s)1(\\s|$)", dat$q214a) &  (is.na(dat$q215a) | trimws(dat$q215a) == "")
cond_q215aoth <-  !is.na(dat$q215a) & grepl("(^|\\s)96(\\s|$)", dat$q215a) & (is.na(dat$q215aoth) | trimws(dat$q215aoth) == "")
cond_neg_q215b <- !is.na(dat$q214a) & !grepl("(^|\\s)1(\\s|$)", dat$q214a) & !is.na(dat$q215b)
cond_q215b <-  !is.na(dat$q214a) & grepl("(^|\\s)1(\\s|$)", dat$q214a) & (is.na(dat$q215b) | trimws(dat$q215b) == "")
cond_q215both <-  !is.na(dat$q215b) & grepl("(^|\\s)96(\\s|$)", dat$q215b) & (is.na(dat$q215both) | trimws(dat$q215both) == "")
cond_neg_q215c <- !is.na(dat$q214a)  & !grepl("(^|\\s)1(\\s|$)", dat$q214a) & !is.na(dat$q215c)
cond_q215c <-  !is.na(dat$q214a) & grepl("(^|\\s)1(\\s|$)", dat$q214a) & (is.na(dat$q215c) | trimws(dat$q215c) == "")
cond_q215cngo <- !is.na(dat$q215c)   & grepl("(^|\\s)15(\\s|$)", dat$q215c) & (is.na(dat$q215cngo) | trimws(dat$q215cngo) == "")
cond_q215cnun <- !is.na(dat$q215c)   & grepl("(^|\\s)16(\\s|$)", dat$q215c) & (is.na(dat$q215cnun) | trimws(dat$q215cnun) == "")
cond_q215cnlmoh <- !is.na(dat$q215c) & grepl("(^|\\s)17(\\s|$)", dat$q215c) & (is.na(dat$q215cnlmoh) | trimws(dat$q215cnlmoh) == "")
cond_q215cnsmoh <- !is.na(dat$q215c) & grepl("(^|\\s)18(\\s|$)", dat$q215c) & (is.na(dat$q215cnsmoh) | trimws(dat$q215cnsmoh) == "")
cond_q215cnnmoh <- !is.na(dat$q215c) & grepl("(^|\\s)19(\\s|$)", dat$q215c) & (is.na(dat$q215cnnmoh) | trimws(dat$q215cnnmoh) == "")
cond_q215cnoth <- !is.na(dat$q215c)  & grepl("(^|\\s)96(\\s|$)", dat$q215c) & (is.na(dat$q215cnoth) | trimws(dat$q215cnoth) == "")

# Community-based stillbirths (q214a code = 2)
cond_neg_q216a <- !is.na(dat$q214a) & !grepl("(^|\\s)2(\\s|$)", dat$q214a) & !is.na(dat$q216a)
cond_q216a <-  !is.na(dat$q214a) & grepl("(^|\\s)2(\\s|$)", dat$q214a) &  (is.na(dat$q216a) | trimws(dat$q216a) == "")
cond_q216aoth <-  !is.na(dat$q216a) & grepl("(^|\\s)96(\\s|$)", dat$q216a) & (is.na(dat$q216aoth) | trimws(dat$q216aoth) == "")
cond_neg_q216b <- !is.na(dat$q214a) & !grepl("(^|\\s)2(\\s|$)", dat$q214a) & !is.na(dat$q216b)
cond_q216b <-  !is.na(dat$q214a) & grepl("(^|\\s)2(\\s|$)", dat$q214a) & (is.na(dat$q216b) | trimws(dat$q216b) == "")
cond_q216both <-  !is.na(dat$q216b) & grepl("(^|\\s)96(\\s|$)", dat$q216b) & (is.na(dat$q216both) | trimws(dat$q216both) == "")
cond_neg_q216c <- !is.na(dat$q214a) & !grepl("(^|\\s)2(\\s|$)", dat$q214a) & !is.na(dat$q216c)
cond_q216c <-  !is.na(dat$q214a)  & grepl("(^|\\s)2(\\s|$)", dat$q214a) & (is.na(dat$q216c) | trimws(dat$q216c) == "")
cond_q216cngo <- !is.na(dat$q216c) & grepl("(^|\\s)15(\\s|$)", dat$q216c) & (is.na(dat$q216cngo) | trimws(dat$q216cngo) == "")
cond_q216cnun <- !is.na(dat$q216c)  & grepl("(^|\\s)16(\\s|$)", dat$q216c) & (is.na(dat$q216cnun) | trimws(dat$q216cnun) == "")
cond_q216cnlmoh <- !is.na(dat$q216c)  & grepl("(^|\\s)17(\\s|$)", dat$q216c) & (is.na(dat$q216cnlmoh) | trimws(dat$q216cnlmoh) == "")
cond_q216cnsmoh <- !is.na(dat$q216c)  & grepl("(^|\\s)18(\\s|$)", dat$q216c) & (is.na(dat$q216cnsmoh) | trimws(dat$q216cnsmoh) == "")
cond_q216cnnmoh <- !is.na(dat$q216c)  & grepl("(^|\\s)19(\\s|$)", dat$q216c) & (is.na(dat$q216cnnmoh) | trimws(dat$q216cnnmoh) == "")
cond_q216cnoth <- !is.na(dat$q216c)  & grepl("(^|\\s)96(\\s|$)", dat$q216c) & (is.na(dat$q216cnoth) | trimws(dat$q216cnoth) == "")

# Community-based neonatal deaths (q214a code = 3)
cond_neg_q217a <- !is.na(dat$q214a) & !grepl("(^|\\s)3(\\s|$)", dat$q214a) & !is.na(dat$q217a)
cond_q217a <-  !is.na(dat$q214a) & grepl("(^|\\s)3(\\s|$)", dat$q214a) &  (is.na(dat$q217a) | trimws(dat$q217a) == "")
cond_q217aoth <-  !is.na(dat$q217a) & grepl("(^|\\s)96(\\s|$)", dat$q217a) &(is.na(dat$q217aoth) | trimws(dat$q217aoth) == "")
cond_neg_q217b <- !is.na(dat$q214a) & !grepl("(^|\\s)3(\\s|$)", dat$q214a) & !is.na(dat$q217b)
cond_q217b <-  !is.na(dat$q214a) & grepl("(^|\\s)3(\\s|$)", dat$q214a) & (is.na(dat$q217b) | trimws(dat$q217b) == "")
cond_q217both <-  !is.na(dat$q217b) & grepl("(^|\\s)96(\\s|$)", dat$q217b) & (is.na(dat$q217both) | trimws(dat$q217both) == "")
cond_neg_q217c <- !is.na(dat$q214a) & !grepl("(^|\\s)3(\\s|$)", dat$q214a) & !is.na(dat$q217c)
cond_q217c <-  !is.na(dat$q214a)  & grepl("(^|\\s)3(\\s|$)", dat$q214a) & (is.na(dat$q217c) | trimws(dat$q217c) == "")
cond_q217cngo <- !is.na(dat$q217c) & grepl("(^|\\s)15(\\s|$)", dat$q217c) & (is.na(dat$q217cngo) | trimws(dat$q217cngo) == "")
cond_q217cnun <- !is.na(dat$q217c)  & grepl("(^|\\s)16(\\s|$)", dat$q217c) & (is.na(dat$q217cnun) | trimws(dat$q217cnun) == "")
cond_q217cnlmoh <- !is.na(dat$q217c)  & grepl("(^|\\s)17(\\s|$)", dat$q217c) & (is.na(dat$q217cnlmoh) | trimws(dat$q217cnlmoh) == "")
cond_q217cnsmoh <- !is.na(dat$q217c)  & grepl("(^|\\s)18(\\s|$)", dat$q217c) & (is.na(dat$q217cnsmoh) | trimws(dat$q217cnsmoh) == "")
cond_q217cnnmoh <- !is.na(dat$q217c)  & grepl("(^|\\s)19(\\s|$)", dat$q217c) &  (is.na(dat$q217cnnmoh) | trimws(dat$q217cnnmoh) == "")
cond_q217cnoth <- !is.na(dat$q217c)  & grepl("(^|\\s)96(\\s|$)", dat$q217c) & (is.na(dat$q217cnoth) | trimws(dat$q217cnoth) == "")

# Community-based child deaths (q214a code = 4)
cond_neg_q218a <- !is.na(dat$q214a) & !grepl("(^|\\s)4(\\s|$)", dat$q214a) & !is.na(dat$q218a)
cond_q218a <-  !is.na(dat$q214a) & grepl("(^|\\s)4(\\s|$)", dat$q214a) & (is.na(dat$q218a) | trimws(dat$q218a) == "")
cond_q218aoth <-  !is.na(dat$q218a) & grepl("(^|\\s)96(\\s|$)", dat$q218a) & (is.na(dat$q218aoth) | trimws(dat$q218aoth) == "")
cond_neg_q218b <- !is.na(dat$q214a) & !grepl("(^|\\s)4(\\s|$)", dat$q214a) & !is.na(dat$q218b)
cond_q218b <-  !is.na(dat$q214a) & grepl("(^|\\s)4(\\s|$)", dat$q214a) & (is.na(dat$q218b) | trimws(dat$q218b) == "")
cond_q218both <-  !is.na(dat$q218b) & grepl("(^|\\s)96(\\s|$)", dat$q218b) & (is.na(dat$q218both) | trimws(dat$q218both) == "")
cond_neg_q218c <- !is.na(dat$q214a) & !grepl("(^|\\s)4(\\s|$)", dat$q214a) & !is.na(dat$q218c)
cond_q218c <-  !is.na(dat$q214a)  & grepl("(^|\\s)4(\\s|$)", dat$q214a) & (is.na(dat$q218c) | trimws(dat$q218c) == "")
cond_q218cngo <- !is.na(dat$q218c) & grepl("(^|\\s)15(\\s|$)", dat$q218c) & (is.na(dat$q218cngo) | trimws(dat$q218cngo) == "")
cond_q218cnun <- !is.na(dat$q218c)  & grepl("(^|\\s)16(\\s|$)", dat$q218c) & (is.na(dat$q218cnun) | trimws(dat$q218cnun) == "")
cond_q218cnlmoh <- !is.na(dat$q218c)  & grepl("(^|\\s)17(\\s|$)", dat$q218c) & (is.na(dat$q218cnlmoh) | trimws(dat$q218cnlmoh) == "")
cond_q218cnsmoh <- !is.na(dat$q218c)  & grepl("(^|\\s)18(\\s|$)", dat$q218c) & (is.na(dat$q218cnsmoh) | trimws(dat$q218cnsmoh) == "")
cond_q218cnnmoh <- !is.na(dat$q218c)  & grepl("(^|\\s)19(\\s|$)", dat$q218c) & (is.na(dat$q218cnnmoh) | trimws(dat$q218cnnmoh) == "")
cond_q218cnoth <- !is.na(dat$q218c)  & grepl("(^|\\s)96(\\s|$)", dat$q218c) & (is.na(dat$q218cnoth) | trimws(dat$q218cnoth) == "")



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
  
  # MPCDSR focal point, training, supervision, electronic or paper-based
  "cond_q15_ca",        "q15",             "MPCDSR focal point at facility", "MPCDSR focal point is blank (q15)",
  "cond_q15_cb",        "q15",             "MPCDSR focal point at facility", "No MPCDSR focal point (q15=0) but multiple responses selected [length(q15) > 1]",
  "cond_q16",           "q16",             "Staff trained in MPCDSR",        "Staff trained in MPCDSR is blank (q16)",
  "cond_neg_q16a",      "q16 / q16a",      "Staff trained in MPCDSR how many",  "No training provided in MPCDSR (q16=0) but q16a is not blank",
  "cond_q17",           "q17",             "Staff mentoring in MPCDSR in past 12m", "Staff mentoring in MPCDSR in past 12m is blank (q17)",
  "cond_q18",           "q18",             "Supervision visits in past 12m",      "Supervision visits in past 12m (q18)",
  "cond_q19",           "q18 / q19",       "Components of supervision",     "There has been MPCDSR supervision (q18=1) but q19 is blank",
  "cond_q19oth",        "q19 / q19oth",    "Components of supervision other","Other selected for components of supervision (q19=96) but q19oth is blank",  
  "cond_q110",          "q110",            "MPCDSR electronic or paper-based", "MPCDSR electronic of paper-based is blank (q110)",
  "cond_neg_q111_ca",   "q110 / q111",     "Which electronic database system",  "MPCDSR system is paper-based (q110=1) and q111 is not blank",
  "cond_neg_q111_cb",   "q110 / q111",     "Which electronic database system",  "No MPCDSR system (q110=4) and q111 is not blank",
  "cond_q111",          "q110 / q111",     "Which electronic database system",  "MPCDSR is electronic (q110=2|3) and q111 is blank",
  "cond_q111oth",       "q111 / q111oth",  "Electronic database system other","Other selected for electronic database system (q111=96) but q111oth is blank",
  
  # How identified
  "cond_neg_q21",       "q110 / q21",      "How are deaths identified",       "No MPCDSR system (q110=4) and q21 is not blank",
  "cond_q21",           "q21",             "How are deaths identified",       "How are deaths identified is blank (q21)",
  "cond_q21oth",        "q21 / q21oth",    "How are deaths identified other", "Other selected for how deaths identified (q21=96) but q21oth is blank",  
  
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
  "cond_q24",           "q24",             "Who notified of deaths",        "Who notified of deaths is blank (q24)",
  "cond_q24oth",        "q24 / q24oth",    "Who notified of deaths other",  "Other selected for who notified of deaths (q24=96) but q24oth is blank",
  "cond_q25",           "q25",             "Deaths reviewed or audited",    "Deaths reviewed or audited is blank (q25)",
  "cond_q25a",          "q25 / q25a",      "Which deaths reviewed or audited", "Deaths reviewed or audited (q25=1) and q25a is blank",
  "cond_q26",           "q26",             "Established committees to review", "Established committees to review is blank (q26)",
  "cond_q26oth",        "q26 / q26oth",    "Established committees to review other",  "Other selected established committees to review (q26=96) but q26oth is blank",
  
  # Code of conduct
  "cond_q211",          "q211",            "Code of conduct",            "Code of conduct is blank (q211)",
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
  "cond_neg_q215a",     "q214a / q215a",    "How community maternal deaths investigated", "Maternal deaths not investigated (q214a!=1) and q215a is not blank",
  "cond_q215a",         "q214a / q215a",    "How community maternal deaths investigated", "Maternal deaths investigated (q214a=1) and q215a is blank",
  "cond_q215aoth",      "q215a / q215aoth", "How community maternal deaths investigated other", "Other selected for how maternal deaths investigated (q215a=96) but q215aoth is blank",
  "cond_neg_q215b",     "q214a / q215b",   "How many community maternal deaths", "Maternal deaths not investigated (q214a!=1) and q215b is not blank",
  "cond_q215b",         "q214a / q215b",   "How many community maternal deaths", "Maternal deaths investigated (q214a=1) and q215b is blank",
  "cond_q215both",      "q215b / q215both","How many community maternal deaths other", "Other selected for how many maternal deaths investigated (q215b=96) and q215both is blank",
  "cond_neg_q215c",    "q214a / q215c",     "VASA for community maternal deaths", "Maternal deaths not investigated (q214a!=1) and q215c is not blank",
  "cond_q215c",        "q214a / q215c",     "VASA for community maternal deaths", "Maternal deaths investigated (q214a=1) and q215c is blank",
  "cond_q215cngo",     "q215c / q215cngo",  "NGO rep maternal death VASA",        "Other NGO conducts VASA for maternal deaths (q215c=15) and q215cngo is blank",
  "cond_q215cnun",     "q215c / q215cnun",  "UN rep maternal death VASA",         "Other UN conducts VASA for maternal deaths (q215c=16) and q215cnun is blank",
  "cond_q215cnlmoh",   "q215c / q215cnlmoh","LGA MOH rep maternal death VASA",    "Other LGA MOH conducts VASA for maternal deaths (q215c=17) and q215cnlmoh is blank",
  "cond_q215cnsmoh",   "q215c / q215cnsmoh","State MOH maternal death VASA",      "Other state MOH conducts VASA for maternal deaths (q215c=18) and q215cnsmoh is blank",
  "cond_q215cnnmoh",   "q215c / q215cnnmoh","National MOH maternal death VASA",   "National MOH conducts VASA for maternal deaths (q215c=19) and q215cnnmoh is blank",
  "cond_q215cnoth",    "q215c / q215cnoth", "Other maternal death VASA",          "Other source conducts VASA for maternal deaths (q215c=96) and q215cnoth is blank",
  
  # Community-based stillbirths
  "cond_neg_q216a", "q214a / q216a", "How community stillbirths investigated", "Stillbirths not investigated (q214a!=2) and q216a is not blank",
  "cond_q216a",     "q214a / q216a", "How community stillbirths investigated", "Stillbirths investigated (q214a=2) and q216a is blank",
  "cond_q216aoth",  "q216a / q216aoth", "How community stillbirths investigated other", "Other selected for how stillbirths investigated (q216a=96) but q216aoth is blank",
  "cond_neg_q216b", "q214a / q216b", "How many community stillbirths", "Stillbirths not investigated (q214a!=2) and q216b is not blank",
  "cond_q216b",     "q214a / q216b", "How many community stillbirths", "Stillbirths investigated (q214a=2) and q216b is blank",
  "cond_q216both",  "q216b / q216both", "How many community stillbirths other", "Other selected for how many stillbirths investigated (q216b=96) and q216both is blank",
  "cond_neg_q216c", "q214a / q216c", "VASA for community stillbirths", "Stillbirths not investigated (q214a!=2) and q216c is not blank",
  "cond_q216c",     "q214a / q216c", "VASA for community stillbirths", "Stillbirths investigated (q214a=2) and q216c is blank",
  "cond_q216cngo",  "q216c / q216cngo", "NGO rep stillbirth VASA", "Other NGO conducts VASA for stillbirths (q216c=15) and q216cngo is blank",
  "cond_q216cnun",  "q216c / q216cnun", "UN rep stillbirth VASA", "Other UN conducts VASA for stillbirths (q216c=16) and q216cnun is blank",
  "cond_q216cnlmoh","q216c / q216cnlmoh", "LGA MOH rep stillbirth VASA", "Other LGA MOH conducts VASA for stillbirths (q216c=17) and q216cnlmoh is blank",
  "cond_q216cnsmoh","q216c / q216cnsmoh", "State MOH stillbirth VASA", "Other state MOH conducts VASA for stillbirths (q216c=18) and q216cnsmoh is blank",
  "cond_q216cnnmoh","q216c / q216cnnmoh", "National MOH stillbirth VASA", "National MOH conducts VASA for stillbirths (q216c=19) and q216cnnmoh is blank",
  "cond_q216cnoth", "q216c / q216cnoth", "Other stillbirth VASA", "Other source conducts VASA for stillbirths (q216c=96) and q216cnoth is blank",
  
  # Community-based neonatal deaths
  "cond_neg_q217a", "q214a / q217a", "How community neonatal deaths investigated", "Neonatal deaths not investigated (q214a!=3) and q217a is not blank",
  "cond_q217a",     "q214a / q217a", "How community neonatal deaths investigated", "Neonatal deaths investigated (q214a=3) and q217a is blank",
  "cond_q217aoth",  "q217a / q217aoth", "How community neonatal deaths investigated other", "Other selected for how neonatal deaths investigated (q217a=96) but q217aoth is blank",
  "cond_neg_q217b", "q214a / q217b", "How many community neonatal deaths", "Neonatal deaths not investigated (q214a!=3) and q217b is not blank",
  "cond_q217b",     "q214a / q217b", "How many community neonatal deaths", "Neonatal deaths investigated (q214a=3) and q217b is blank",
  "cond_q217both",  "q217b / q217both", "How many community neonatal deaths other", "Other selected for how many neonatal deaths investigated (q217b=96) and q217both is blank",
  "cond_neg_q217c", "q214a / q217c", "VASA for community neonatal deaths", "Neonatal deaths not investigated (q214a!=3) and q217c is not blank",
  "cond_q217c",     "q214a / q217c", "VASA for community neonatal deaths", "Neonatal deaths investigated (q214a=3) and q217c is blank",
  "cond_q217cngo",  "q217c / q217cngo", "NGO rep neonatal death VASA", "Other NGO conducts VASA for neonatal deaths (q217c=15) and q217cngo is blank",
  "cond_q217cnun",  "q217c / q217cnun", "UN rep neonatal death VASA", "Other UN conducts VASA for neonatal deaths (q217c=16) and q217cnun is blank",
  "cond_q217cnlmoh","q217c / q217cnlmoh", "LGA MOH rep neonatal death VASA", "Other LGA MOH conducts VASA for neonatal deaths (q217c=17) and q217cnlmoh is blank",
  "cond_q217cnsmoh","q217c / q217cnsmoh", "State MOH neonatal death VASA", "Other state MOH conducts VASA for neonatal deaths (q217c=18) and q217cnsmoh is blank",
  "cond_q217cnnmoh","q217c / q217cnnmoh", "National MOH neonatal death VASA", "National MOH conducts VASA for neonatal deaths (q217c=19) and q217cnnmoh is blank",
  "cond_q217cnoth", "q217c / q217cnoth", "Other neonatal death VASA", "Other source conducts VASA for neonatal deaths (q217c=96) and q217cnoth is blank",
  
  # Community-based child deaths
  "cond_neg_q218a", "q214a / q218a", "How community child deaths investigated", "Child deaths not investigated (q214a!=4) and q218a is not blank",
  "cond_q218a",     "q214a / q218a", "How community child deaths investigated", "Child deaths investigated (q214a=4) and q218a is blank",
  "cond_q218aoth",  "q218a / q218aoth", "How community child deaths investigated other", "Other selected for how child deaths investigated (q218a=96) but q218aoth is blank",
  "cond_neg_q218b", "q214a / q218b", "How many community child deaths", "Child deaths not investigated (q214a!=4) and q218b is not blank",
  "cond_q218b",     "q214a / q218b", "How many community child deaths", "Child deaths investigated (q214a=4) and q218b is blank",
  "cond_q218both",  "q218b / q218both", "How many community child deaths other", "Other selected for how many child deaths investigated (q218b=96) and q218both is blank",
  "cond_neg_q218c", "q214a / q218c", "VASA for community child deaths", "Child deaths not investigated (q214a!=4) and q218c is not blank",
  "cond_q218c",     "q214a / q218c", "VASA for community child deaths", "Child deaths investigated (q214a=4) and q218c is blank",
  "cond_q218cngo",  "q218c / q218cngo", "NGO rep child death VASA", "Other NGO conducts VASA for child deaths (q218c=15) and q218cngo is blank",
  "cond_q218cnun",  "q218c / q218cnun", "UN rep child death VASA", "Other UN conducts VASA for child deaths (q218c=16) and q218cnun is blank",
  "cond_q218cnlmoh","q218c / q218cnlmoh", "LGA MOH rep child death VASA", "Other LGA MOH conducts VASA for child deaths (q218c=17) and q218cnlmoh is blank",
  "cond_q218cnsmoh","q218c / q218cnsmoh", "State MOH child death VASA", "Other state MOH conducts VASA for child deaths (q218c=18) and q218cnsmoh is blank",
  "cond_q218cnnmoh","q218c / q218cnnmoh", "National MOH child death VASA", "National MOH conducts VASA for child deaths (q218c=19) and q218cnnmoh is blank",
  "cond_q218cnoth", "q218c / q218cnoth", "Other child death VASA", "Other source conducts VASA for child deaths (q218c=96) and q218cnoth is blank"
  
  
)


conditions <- list(
  # Maternal death capture
  cond_q13a = cond_q13a,
  cond_neg_q13awhy = cond_neg_q13awhy,
  cond_q13a1_ca = cond_q13a1_ca,
  cond_q13a1_cb = cond_q13a1_cb,
  cond_q13a2 = cond_q13a2,
  cond_q14a = cond_q14a,
  
  # Stillbirth capture
  cond_q13b = cond_q13b,
  cond_neg_q13bwhy = cond_neg_q13bwhy,
  cond_q13b1_ca = cond_q13b1_ca,
  cond_q13b1_cb = cond_q13b1_cb,
  cond_q13b2 = cond_q13b2,
  cond_q14b = cond_q14b,
  
  # Neonatal death capture
  cond_q13c = cond_q13c,
  cond_neg_q13cwhy = cond_neg_q13cwhy,
  cond_q13c1_ca = cond_q13c1_ca,
  cond_q13c1_cb = cond_q13c1_cb,
  cond_q13c2 = cond_q13c2,
  cond_q14c = cond_q14c,
  
  # Child death capture
  cond_q13d = cond_q13d,
  cond_neg_q13dwhy = cond_neg_q13dwhy,
  cond_q13d1_ca = cond_q13d1_ca,
  cond_q13d1_cb = cond_q13d1_cb,
  cond_q13d2 = cond_q13d2,
  cond_q14d = cond_q14d,
  
  # MPCDSR focal point, training, supervision, electronic or paper-based
  cond_q15_ca = cond_q15_ca,
  cond_q15_cb = cond_q15_cb,
  cond_q16 = cond_q16,
  cond_neg_q16a = cond_neg_q16a,
  cond_q17 = cond_q17,
  cond_q18 = cond_q18,
  cond_q19 = cond_q19,
  cond_q19oth = cond_q19oth,
  cond_q110 = cond_q110,
  cond_neg_q111_ca = cond_neg_q111_ca,
  cond_neg_q111_cb = cond_neg_q111_cb,
  cond_q111 = cond_q111,
  cond_q111oth = cond_q111oth,
  
  # How identified
  cond_neg_q21 = cond_neg_q21,
  cond_q21 = cond_q21,
  cond_q21oth = cond_q21oth,
  
  # How soon notified facility deaths
  cond_q22a = cond_q22a,
  cond_q22aoth = cond_q22aoth,
  cond_q22b = cond_q22b,
  cond_q22both = cond_q22both,
  cond_q22c = cond_q22c,
  cond_q22coth = cond_q22coth,
  cond_q22d = cond_q22d,
  cond_q22doth = cond_q22doth,
  
  # How soon notified community deaths
  cond_q23a = cond_q23a,
  cond_q23aoth = cond_q23aoth,
  cond_q23b = cond_q23b,
  cond_q23both = cond_q23both,
  cond_q23c = cond_q23c,
  cond_q23coth = cond_q23coth,
  cond_q23d = cond_q23d,
  cond_q23doth = cond_q23doth,
  
  # Who is notified, audits, committees
  cond_q24 = cond_q24,
  cond_q24oth = cond_q24oth,
  cond_q25 = cond_q25,
  cond_q25a = cond_q25a,
  cond_q26 = cond_q26,
  cond_q26oth = cond_q26oth,
  
  # Code of conduct
  cond_q211 = cond_q211,
  cond_neg_q212 = cond_neg_q212,
  cond_q212 = cond_q212,
  cond_neg_q213 = cond_neg_q213,
  cond_q213 = cond_q213,
  cond_neg_q213a = cond_neg_q213a,
  cond_q213a = cond_q213a,
  cond_neg_q213b = cond_neg_q213b,
  cond_q213b = cond_q213b,
  
  # Community-based deaths
  cond_q214 = cond_q214,
  cond_neg_q214a = cond_neg_q214a,
  cond_q214a = cond_q214a,
  
  # Community-based maternal deaths
  cond_neg_q215a = cond_neg_q215a,
  cond_q215a = cond_q215a,
  cond_q215aoth = cond_q215aoth,
  cond_neg_q215b = cond_neg_q215b,
  cond_q215b = cond_q215b,
  cond_q215both = cond_q215both,
  cond_neg_q215c = cond_neg_q215c,
  cond_q215c = cond_q215c,
  cond_q215cngo = cond_q215cngo,
  cond_q215cnun = cond_q215cnun,
  cond_q215cnlmoh = cond_q215cnlmoh,
  cond_q215cnsmoh = cond_q215cnsmoh,
  cond_q215cnnmoh = cond_q215cnnmoh,
  cond_q215cnoth = cond_q215cnoth,
  
  # Community-based stillbirths
  cond_neg_q216a = cond_neg_q216a, cond_q216a = cond_q216a, cond_q216aoth = cond_q216aoth,
  cond_neg_q216b = cond_neg_q216b, cond_q216b = cond_q216b, cond_q216both = cond_q216both,
  cond_neg_q216c = cond_neg_q216c, cond_q216c = cond_q216c,
  cond_q216cngo = cond_q216cngo, cond_q216cnun = cond_q216cnun,
  cond_q216cnlmoh = cond_q216cnlmoh, cond_q216cnsmoh = cond_q216cnsmoh,
  cond_q216cnnmoh = cond_q216cnnmoh, cond_q216cnoth = cond_q216cnoth,
  
  # Community-based neonatal deaths
  cond_neg_q217a = cond_neg_q217a, cond_q217a = cond_q217a, cond_q217aoth = cond_q217aoth,
  cond_neg_q217b = cond_neg_q217b, cond_q217b = cond_q217b, cond_q217both = cond_q217both,
  cond_neg_q217c = cond_neg_q217c, cond_q217c = cond_q217c,
  cond_q217cngo = cond_q217cngo, cond_q217cnun = cond_q217cnun,
  cond_q217cnlmoh = cond_q217cnlmoh, cond_q217cnsmoh = cond_q217cnsmoh,
  cond_q217cnnmoh = cond_q217cnnmoh, cond_q217cnoth = cond_q217cnoth,
  
  # Community-based child deaths
  cond_neg_q218a = cond_neg_q218a, cond_q218a = cond_q218a, cond_q218aoth = cond_q218aoth,
  cond_neg_q218b = cond_neg_q218b, cond_q218b = cond_q218b, cond_q218both = cond_q218both,
  cond_neg_q218c = cond_neg_q218c, cond_q218c = cond_q218c,
  cond_q218cngo = cond_q218cngo, cond_q218cnun = cond_q218cnun,
  cond_q218cnlmoh = cond_q218cnlmoh, cond_q218cnsmoh = cond_q218cnsmoh,
  cond_q218cnnmoh = cond_q218cnnmoh, cond_q218cnoth = cond_q218cnoth
)

section1_results <- check_defs %>%
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

# q210 --------------------------------------------------------------------


