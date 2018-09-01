library("anytime")

run_sleep_script <- function(sleep_data, bed_sensors) {

  existing_sensors <- bed_sensors[which(bed_sensors %in% names(sleep_data))]
  sleep_data <- sleep_data[existing_sensors]
  current_date <- format(sleep_data[[1]]$timestamp[1], format = "%Y-%m-%d")

  #assign k-means clusters
  #adapted from: https://stats.stackexchange.com/questions/78322/is-there-a-function-in-r-that-takes-the-centers-of-clusters-that-were-found-and
  clusters <- function(x, centers) {
    if (is.vector(x)) x <- as.matrix(x)
    # compute squared euclidean distance from each sample to each cluster center
    tmp <- sapply(seq_len(nrow(x)),
                  function(i) apply(centers, 1,
                                    function(v) sum((x[i, ]-v)^2)))
    max.col(-t(tmp))  # find index of min distance
  } 
  
  # Calculate features
  calculate_features <- function(data, interval){
    sens <- data$estimote_id[1]
    feature_data <- data.frame(timestamp=as.POSIXct(character()), 
                               estimote_id=character(), 
                               mean_x_acc=double(),
                               mean_y_acc=double(),
                               stringsAsFactors=FALSE)
    min_timestamp <- min(data$timestamp)
    max_timestamp <- max(data$timestamp)-interval
    
    if(min_timestamp < max_timestamp) {
      intervals <- seq(min_timestamp, max_timestamp, by=interval)
      
      for (d in intervals) {
        interval_subset <- data[data$timestamp > d & data$timestamp <= (d + interval), ]
        len <- dim(interval_subset)[1]
        
        #only consider if there's enough data 
        if (len>=30) {
          mean_magnitude <- mean(interval_subset$total_acc)
          interval_activity <- list(as.POSIXct(d + interval, origin="1970-01-01"), 
                                    sens,
                                    mean(interval_subset$x_acceleration), 
                                    mean(interval_subset$y_acceleration))
          
          feature_data[nrow(feature_data)+1,] <- interval_activity
        }
      }
    }
    return(feature_data)
  }
  
  #Calculate sleep duration for all sensors
  interval <- 300 #seconds = 5 minutes
  
  sleep_values <- vector("list")
  bed_index <- 1 
  
  for(i in 1:length(sleep_data)) {
    sens_name <- names(sleep_data[i])
    sens_data <- sleep_data[[i]]
    sens_data$timestamp <- as.POSIXct(sens_data$timestamp, origin="1970-01-01")
    sens_data$total_acc <- sqrt(sens_data$x_acceleration^2 + sens_data$y_acceleration^2 + sens_data$z_acceleration^2)
    sens_features <- calculate_features(sens_data, interval)
    
    if(nrow(sens_features) > 1) {
      #cluster the data based on the sensor's bed model
      setwd(models_dir)
      load(file=paste("bed_model_", sens_name, ".RData", sep=""))
      new_clusters <- clusters(sens_features[c(-1,-2)], fit$centers)
      
      #modify cluster labels to correspond to 0=off, 1=on the bed 
      new_clusters[which(new_clusters==1)] <- as.integer(dimnames(fit$centers)[[1]][1])
      new_clusters[which(new_clusters==2)] <- as.integer(dimnames(fit$centers)[[1]][2])
      
      total_time_off_bed <- sum(new_clusters==0)*interval/60
      total_time_on_bed <- 24*60-total_time_off_bed
      sleep_values[[bed_index]] <- total_time_on_bed
      names(sleep_values)[bed_index] <- sens_name
      bed_index <- bed_index+1
      
    } else {
      sleep_values[[bed_index]] <- 0
      names(sleep_values)[bed_index] <- sens_name
      bed_index <- bed_index+1
    }
  }
  sleep_values_vector <- data.frame(t(unlist(sleep_values)))
  sleep_values <- data.frame(date = current_date, sleep_values_vector)
  
  # Save .Rdata
  setwd(output_dir)
  save(sleep_values, file = paste(current_date, "sleep_values.Rdata", sep = " - "))
}
