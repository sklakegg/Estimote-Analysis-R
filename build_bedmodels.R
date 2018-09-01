library("RMySQL")

run_build_bed_models_script <- function(nearable_data_bed) {
  
  test_bimodality <- function(variable1, variable2){
    require(modes)
    fit1 <- kmeans(variable1, 2)
    ash1 <- Ashmans_D(mean(variable1[which(fit1$cluster==1)]),mean(variable1[which(fit1$cluster==2)]),sd(variable1[which(fit1$cluster==1)]),sd(variable1[which(fit1$cluster==2)]))
    fit2 <- kmeans(variable2, 2)
    ash2 <- Ashmans_D(mean(variable2[which(fit2$cluster==1)]),mean(variable2[which(fit2$cluster==2)]),sd(variable2[which(fit2$cluster==1)]),sd(variable2[which(fit2$cluster==2)]))
    if(ash1>ash2) {
      return(deparse(substitute(variable1)))
    } else {
      return(deparse(substitute(variable2)))
    }
  }
  
  # Calculate features
  calculate_features <- function(data, interval){
    sens <- data$estimote_id[1]
    feature_data <- data.frame(timestamp=as.POSIXct(character()), 
                               estimote_id=character(), 
                               mean_x_acc=double(),
                               mean_y_acc=double(),
                               stringsAsFactors=FALSE)
    intervals <- seq(min(data$timestamp), max(data$timestamp)-interval, by=interval)
    
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
    return(feature_data)
  }
  
  #Build sleep models for all beds (except for B10 and B14 which have an engine to blow warm air into the mattress)
  interval <- 300 #seconds = 5 minutes
  for(i in 1:length(nearable_data_bed)) {
    
    sens_name <- names(nearable_data_bed[i])
    sens_data <- nearable_data_bed[[i]]
    sens_data$timestamp <- as.POSIXct(sens_data$timestamp, origin="1970-01-01")
    sens_data$total_acc <- sqrt(sens_data$x_acceleration^2 + sens_data$y_acceleration^2 + sens_data$z_acceleration^2)
    sens_features <- calculate_features(sens_data, interval)
    
    #fit two k-means clusters for on/off the bed
    fit <- kmeans(sens_features[c(-1,-2)], 2)
    
    #which axis has more bimodality, x or y (on/off bed)?
    dominant_axis <- test_bimodality(sens_features$mean_x_acc, sens_features$mean_y_acc)
    
    #check which cluster corresponds to "person is on the bed" (data should have more variation compared with empty bed)
    var1 <- eval(parse(text=paste("var(", dominant_axis, "[which(fit$cluster==1)])", sep="")))
    var2 <- eval(parse(text=paste("var(", dominant_axis, "[which(fit$cluster==2)])", sep="")))
    
    #modify cluster center names to correspond to 0=off, 1=on the bed
    if(var1>var2) {
      dimnames(fit$centers)[[1]][2] <- 0 
    } else {
      dimnames(fit$centers)[[1]][1] <- 0
      dimnames(fit$centers)[[1]][2] <- 1
    }
    
    #testing
    if(var1>var2) {
      fit$cluster[which(fit$cluster==2)] <- 0
    } else {
      fit$cluster[which(fit$cluster==1)] <- 0
      fit$cluster[which(fit$cluster==2)] <- 1
    }
    
    # #plot x-acceleration
    # plot(sens_features$timestamp, sens_features$mean_x_acc, col=as.factor(fit$cluster), type = "p", xaxt = "n", main = paste(sens_name, ", x-acceleration", sep=""))
    # r <- as.POSIXct(round(range(sens_features$timestamp), "days"))
    # axis.POSIXct(1, at = seq(r[1], r[2], by = "day"), format = "%d", las=2)
    # legend("topright", inset=.01, title="On the bed", as.character(unique(fit$cluster)), fill=as.factor(unique(fit$cluster)), horiz=TRUE)
    # dev.off()
    # jpeg(paste("sleep_", sens_name, "_x_acc_Nov17.jpg", sep=""))
    
    # #plot y-acceleration
    # plot(sens_features$timestamp, sens_features$mean_y_acc, col=as.factor(fit$cluster), type = "p", xaxt = "n", main = paste(sens_name, ", y-acceleration", sep=""))
    # r <- as.POSIXct(round(range(sens_features$timestamp), "days"))
    # axis.POSIXct(1, at = seq(r[1], r[2], by = "day"), format = "%d", las=2)
    # legend("topright", inset=.01, title="On the bed", as.character(unique(fit$cluster)), fill=as.factor(unique(fit$cluster)), horiz=TRUE)
    # dev.off()
    # jpeg(paste("sleep_", sens_name, "_y_acc_Nov17.jpg", sep=""))
    
    save(fit, file = paste("bed_model_", sens_name, ".RData", sep=""))
    rm(fit)
  }
}