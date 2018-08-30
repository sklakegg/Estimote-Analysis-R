run_activity_script <- function(activity_data) {
  
  # Calculate activity features for one user
  calculate_activity_features <- function(data, interval){
    
    sens <- data$estimote_id[1]
    feature_data <- data.frame(timestamp=as.POSIXct(character()), 
                               estimote_id=character(), 
                               mad=double(),
                               stringsAsFactors=FALSE)

    min_int <- min(data$timestamp)
    max_int <- max(data$timestamp)-interval
    if(min_int < max_int) {
      intervals <- seq(min_int, max_int, by=interval)
      
      for (d in intervals) {
        interval_subset <- data[data$timestamp > d & data$timestamp <= (d + interval), ]
        len <- dim(interval_subset)[1]
        
        # Only consider if there's data from at least 1/3 of the interval length
        if (len>=interval/3) {
          mean_magnitude <- mean(interval_subset$total_acc)
          interval_activity <- list(as.POSIXct(d + interval, origin="1970-01-01"), 
                                    sens, 
                                    1/len*sum( abs(interval_subset$total_acc-mean_magnitude)))
          feature_data[nrow(feature_data)+1,] <- interval_activity
        } 
      }
    }
    return(feature_data)
  }
  
  # Calculate amount of activity for all users
  interval <- 120 # seconds = 2 minutes
  
  all_users <- c("P1", "P2", "...")
  num_users <- length(all_users)
  activity_sum_intervals <- vector("list", num_users)
  names(activity_sum_intervals) <- all_users
  activity_nintervals <- activity_sum_intervals
  current_date <- format(activity_data[[1]]$timestamp[1], format = "%Y-%m-%d")
  
  for(i in 1:length(activity_data)) {
    sens_data <- activity_data[[i]]
    sens_name <- names(activity_data[i])
    
    sens_data$timestamp <- as.POSIXct(sens_data$timestamp, origin="1970-01-01")
    sens_data$total_acc <- sqrt(sens_data$x_acceleration^2 + sens_data$y_acceleration^2 + sens_data$z_acceleration^2)
    sens_features <- calculate_activity_features(sens_data, interval)
    
    # If zero activity intervals assign zero vector 
    if(nrow(sens_features) == 0) { 
      activity_sum_intervals[[sens_name]] <- numeric(14)
      activity_nintervals[[sens_name]] <- numeric(14)
    } else {
      # Bind activity per hour.
      time_threshold <- as.POSIXct(format(sens_features$timestamp[1], format = "%Y-%m-%d %06:30"))
      mad_intervals <- list()
      for(j in 1:14) {
        bin_rows <- which(sens_features$timestamp >= time_threshold & sens_features$timestamp < time_threshold + 3600)
        mad_intervals[[j]] <- sens_features[bin_rows,]
        time_threshold <- time_threshold + 3600
      }
      
      # Store in list.
      sum_madintervals <- lapply(mad_intervals, function(x){
        sum(x$mad)
      })
      sum_madintervals <- unlist(sum_madintervals)
      sum_madintervals[which(is.nan(sum_madintervals))] <- 0
      num_madintervals <- unlist(lapply(mad_intervals, nrow))
      
      activity_sum_intervals[[sens_name]] <- sum_madintervals
      activity_nintervals[[sens_name]] <- num_madintervals
    }
  }
  null_listelements <- lapply(activity_nintervals, function(x){
    is.null(x)
  })   
  null_listelements <- unlist(null_listelements)
  null_listelements <- names(which(null_listelements))
  if(length(null_listelements) > 0){
    for(k in 1:length(null_listelements)) {
      activity_sum_intervals[[null_listelements[k]]] <- numeric(14)
      activity_nintervals[[null_listelements[k]]] <- numeric(14)
    }
  }
  
  # Save .Rdata
  setwd("/Path")
  save(activity_sum_intervals, file = paste(current_date, "activity_sum_intervals.Rdata", sep = " - "))
  
  setwd("/Path")
  save(activity_nintervals, file = paste(current_date, "activity_nintervals.Rdata", sep = " - "))
}