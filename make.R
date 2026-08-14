
################################################
# Health Facility MNH Innovations - Nigeria
################################################

# Pull and archive --------------------------------------------------------

#source("./src/set-up/schedule-tasks.R", local = new.env())
source("./src/set-up/pull-data.R", local = new.env())
source("./src/set-up/move-and-archive-data.R", local = new.env())

# Monitoring --------------------------------------------------------------

source("./src/monitoring/render-monitoring-toolC.R", local = new.env())
source("./src/monitoring/move-and-archive-toolC-report.R", local = new.env())


# For tomorrow...
# get schedule-tasks to run
# make sure render-monitoring-toolC can run from make file. currently have to open the script to do. or maybe it works...



