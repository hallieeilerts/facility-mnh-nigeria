
################################################
# Health Facility MNH Innovations - Nigeria
################################################


# Set up ------------------------------------------------------------------

source("./src/set-up/pull-data.R", local = new.env())

# Monitoring --------------------------------------------------------------

#source("./src/monitoring/monitoring-report-toolA.Rmd", local = new.env())
#source("./src/monitoring/monitoring-table-toolA.Rmd", local = new.env())

source("./src/monitoring/clean-toolC.R", local = new.env())
rmarkdown::render("./src/monitoring/monitoring-table-toolC.Rmd")

source("./src/monitoring/clean-toolC-q27.R", local = new.env())
source("./src/monitoring/clean-toolC-MaternPeriDeathRepeat.R", local = new.env())
# need to add these to report

source("./src/monitoring/monitoring-report-data-collection.Rmd", local = new.env())



# key off of f03 instead of the KEY
# real server
# https://odkc.akenahealth.org
# Email: mmary1@jhu.edu
# Password: marypassword

