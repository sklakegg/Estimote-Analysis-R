library("lubridate")
library("anytime")

# Directories
base_dir <- "/Users/researcher/"
data_dir <- paste0(base_dir,"Google Drive/Documents/Forskning/2017 - Context-Awareness for Elderly Care/Data/Study/Nearables - Study B")
scripts_dir <- paste0(base_dir,"Desktop")
output_dir <- paste0(base_dir,"Desktop")
models_dir <- paste0(base_dir,"Desktop")

# Set variables
patients_all <- c("P1", "P2", "P3", "P4", "P5", "P6", "P7", "P8", "P9", "P10", "P11", "P12", "P13", "P14", "P15")
nurses_all <- c("N1", "N2", "N3", "N4", "N5", "N6")

nearables <- c(P1 = "<Insert UUID>", P2 = "<Insert UUID>",
               N1 = "<Insert UUID>", N2 = "<Insert UUID>",
               B1 = "<Insert UUID>", B2="<Insert UUID>", B3 = "<Insert UUID>")

# Needs to match with features used in geofence prediction model
automation_units <- c(RM1 = "<Insert UUID>", RM1 = "<Insert UUID>",
                 RM2 = "<Insert UUID>", RM2 = "<Insert UUID>",
                 RM3 = "<Insert UUID>", RM3 = "<Insert UUID>")

bed_sensors <- unique(grep('B+', names(nearables), value=TRUE))
human_sensors <- unique(grep('P|N+', names(nearables), value=TRUE))

# Load list of .Rdata
setwd(data_dir)

# Iterate through each day of .Rdata
temp = list.files(pattern="*.Rdata")
for(i in 1:length(temp)) {
  setwd(data_dir)
  load(temp[i])
  night_time <- df_combined$timestamp[1]
  second(night_time) <- 59
  minute(night_time) <- 29
  hour(night_time) <- 20
  day_start_timestamp <- df_combined$timestamp[1]
  
  # Remove duplicate values for each individual device_id based on timestamp and estimote_id.
  nearable_data_loc <- df_combined[duplicated(df_combined[c("timestamp","estimote_id", "device_id")]) == FALSE,]
  
  # Remove data after 20.30. Patient is going to sleep then. Want to track location only during day for better %.
  # Don't know if patient is wearing sensor after that time.
  nearable_data_loc <- nearable_data_loc[which(nearable_data_loc$timestamp < night_time),]
  
  # Split data for location (duplicate from different device_id included).
  nearable_data_location <- split(nearable_data_loc , f = nearable_data_loc$estimote_id)
  nearable_data_location <- nearable_data_location[which(names(nearable_data_location) %in% human_sensors)]
  rm(nearable_data_loc)
  gc()
  
  # Remove duplicate values based on timestamp and estimote_id.
  nearable_data_unique <- df_combined[duplicated(df_combined[c("timestamp","estimote_id")]) == FALSE,]
  rm(df_combined)
  gc()
  
  # Split data for sleep and activity and sleep (duplicates from different device_id NOT included).
  nearable_data_split <- split(nearable_data_unique , f = nearable_data_unique$estimote_id)
  rm(nearable_data_unique)
  gc()
  nearable_data_bed <- nearable_data_split[which(names(nearable_data_split) %in% bed_sensors)]
  nearable_data_human_sensors <- nearable_data_split[which(names(nearable_data_split) %in% human_sensors)]
  
  # Remove data after 20.30 as patient is going to sleep then. Don't know if patient is wearing sensor after that time.
  for(i in 1:length(nearable_data_human_sensors)) {
    nearable_data_human_sensors[[i]] <- nearable_data_human_sensors[[i]][which(nearable_data_human_sensors[[i]]$timestamp < night_time),]
  }
  
  # Run activity script
  setwd(scripts_dir)
  source("calc_activity.R")
  run_activity_script(nearable_data_human_sensors)
  
  # Load geofence model for proximity script
  setwd(models_dir)
  load("rf_model.rda")
  # Run proximity script
  setwd(scripts_dir)
  source("calc_proximity.R")
  run_proximity_script(nearable_data_location, day_start_timestamp, night_time)
  
  # # Build bed models
  # setwd(scripts_dir)
  # source("build_bedmodels.R")
  # run_build_bed_models_script(nearable_data_bed)
  # Run sleep script
  setwd(scripts_dir)
  source("calc_sleep.R")
  run_sleep_script(nearable_data_bed, bed_sensors)
}