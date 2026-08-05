
################################################
# Health Facility MNH Innovations - Nigeria
################################################


# Set up ------------------------------------------------------------------

source("./src/set-up/pull-data.R", local = new.env())

# Monitoring --------------------------------------------------------------

source("./src/monitoring/clean-toolC.R", local = new.env())
rmarkdown::render("./src/monitoring/monitoring-table-toolC.Rmd")
rmarkdown::render("./src/monitoring/monitoring-toolC.Rmd") # this one combines, pull-data, clean-toolC, and making the report

source("./src/monitoring/clean-toolC-q27.R", local = new.env())
source("./src/monitoring/clean-toolC-MaternPeriDeathRepeat.R", local = new.env())
# need to add these to report

source("./src/monitoring/monitoring-report-data-collection.Rmd", local = new.env())




