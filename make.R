
################################################
# Health Facility MNH Innovations - Nigeria
################################################


# Set up ------------------------------------------------------------------

source("./src/set-up/pull-data.R", local = new.env())

# Monitoring --------------------------------------------------------------

source("./src/monitoring/monitoring-report-toolA.Rmd", local = new.env())
source("./src/monitoring/monitoring-table-toolA.Rmd", local = new.env())

# source("./src/monitoring/monitoring-report-toolB.Rmd", local = new.env())
# source("./src/monitoring/monitoring-table-toolB.Rmd", local = new.env())
# 
# source("./src/monitoring/monitoring-report-toolC.Rmd", local = new.env())
# source("./src/monitoring/monitoring-table-toolC.Rmd", local = new.env())
# 
# source("./src/monitoring/monitoring-report-toolD.Rmd", local = new.env())
# source("./src/monitoring/monitoring-table-toolD.Rmd", local = new.env())

source("./src/monitoring/monitoring-report-data-collection.Rmd", local = new.env())

# Analysis -------------------------------------------------



