library("caret")
library("randomForest")
library("dplyr")
library("RMySQL")
library("gtools")
library("tibble")
library("tidyr")

run_proximity_script <- function(location_data_df, day_start_timestamp, day_end_timestamp) {

    create_rssi_frame <- function(start, stop, section_data) {
  
      # Create df with timestamps from start to stop with 1s intervals
      df_timeseq <- data.frame(DateTime = seq(start, stop, by=(1)))
      
      if (nrow(section_data) == 0) {return <- NULL
      } else {
        section_data <- data.frame(section_data$timestamp, section_data$device_id, as.numeric(section_data$rssi))
        names(section_data) <- c("timestamp", "device_id", "rssi")
        section_data_split <- split(section_data , f = section_data$device_id)
        df_rssi_list <- list()
        
        # Remove duplicates and add 0 rssi where there is missing data entries
        for(i in 1:length(section_data_split)) {
          section_data_split[[i]] <- section_data_split[[i]][duplicated(section_data_split[[i]][,c("timestamp")]) == FALSE,]
          df_rssi_values <- data.frame("timestamp" = section_data_split[[i]]$timestamp, "rssi" = section_data_split[[i]]$rssi)
          df_no_values <- data.frame("timestamp" = df_timeseq[[1]], "rssi" = 0)
          df_rssi_values$timestamp <- format.POSIXct(df_rssi_values$timestamp,'%m-%d-%y %H:%M:%S')
          df_no_values$timestamp <- format.POSIXct(df_no_values$timestamp,'%m-%d-%y %H:%M:%S')
          df_no_values$rssi[match(df_rssi_values$timestamp, df_no_values$timestamp)] <- df_rssi_values$rssi
          df_rssi_list[[i]] <- df_no_values
          names(df_rssi_list)[i] <- names(section_data_split)[i]
        }
        
        # Add df of 0 for automation_units not present
        missing_automation_units <- unique(names(automation_units))[!unique(names(automation_units)) %in% unique(names(df_rssi_list))]
        if(length(missing_automation_units) > 0 ) {
          for(i in 1:length(missing_automation_units)) {
            df_missing <- data.frame("timestamp" = df_timeseq[[1]], "rssi" = 0)
            pos <- length(df_rssi_list)+1
            df_rssi_list[[pos]] <- df_missing
            names(df_rssi_list)[pos] <- missing_automation_units[i]
          }
        }
        
        # Retrieve rssi column from all dataframes and store in summary df
        rssi_summary <- data.frame(init = numeric(length(df_rssi_list[[1]][[1]])))
        for(i in 1:length(unique(names(automation_units)))) {
          rssi_summary[i] <- df_rssi_list[[unique(names(automation_units))[i]]][[2]]
        }
        names(rssi_summary) <- paste("RSSI", unique(names(automation_units)))
        
        return <- rssi_summary
      }
    }
    
    # Create location df
    df_loc_preds <- data.frame(matrix(ncol = 0, nrow = 50400))
    for(i in 1:length(location_data_df)) {
      nearable_name <- names(location_data_df[i])
      rssi_df <- create_rssi_frame(day_start_timestamp, day_end_timestamp, location_data_df[[i]])
      
      # Identify and track rownames of empty (containing only 0s) and non empty rows
      # Each row represent 1 second
      # Empty rows are removed before prediction and then binded back in to maintain structure
      zero_vector <- integer(13)
      row_sub = apply(rssi_df, 1, function(x) all.equal(as.numeric(x), zero_vector))
      removed_rows <- which(row_sub==TRUE)
      saved_rows <- which(!row_sub==TRUE)
      
      # Predicts and assigns correct rownames
      rssi_predict_df <- rssi_df[saved_rows,]
      loc_predicts <- predict(rf_model, newdata=rssi_predict_df)
      loc_predicts <- as.data.frame(as.character(loc_predicts), stringsAsFactors=FALSE)
      colnames(loc_predicts) <- nearable_name
      row.names(loc_predicts) <- saved_rows
      
      # Df with rows that should be 0
      zeros_df <- data.frame(integer(length(removed_rows)))
      colnames(zeros_df) <- nearable_name
      row.names(zeros_df) <- removed_rows
      
      # Combine two dfs
      loc_predicts <- rbind(loc_predicts, zeros_df)
      loc_predicts <- data.frame(loc_predicts[order(as.numeric(row.names(loc_predicts))), ])
      names(loc_predicts) <- nearable_name
      
      df_loc_preds[i] <- loc_predicts
    }
    
    # Total time of a day in seconds
    day_in_s <- 50400
    
    # Total time of the day with sensors data
    A <- function(x) {
      table_x <- table(x)
      if (length(which(names(table_x) == 0))) {
        table_x <- table_x[-which(names(table_x) == 0)]
      }
      sum(table_x) }
    tot_time_sens_data <- apply(df_loc_preds, 2, A)
    # In percentage
    tot_time_sens_data_percentage <- ((tot_time_sens_data / 50400) * 100)
    
    # Time around others (i.e., time around patient or nurse)
    B <- function(x) {
      table_x <- table(unlist(x))
      # Find sections which has a freq > 1 in table (i.e., sections with two people or more)
      social_sections <- names(which(table_x > 1))
      # Removes section zero
      if (length(social_sections) > 0) { social_sections <- social_sections[-which(social_sections == 0)] }
      # Social sections are replaced by 1, rest 0
      replace_sections <- which(x %in% social_sections)
      if(length(replace_sections) > 0) {
        x[replace_sections] <- 1
        x[-replace_sections] <- 0
      } else {
        x[1:length(x)] <- 0
      }
      return(x)
    }
    time_others_df <- df_loc_preds
    time_others_df <- time_others_df %>% mutate_all(as.character)
    time_others <- apply(time_others_df, 1, B)
    time_others <- data.frame(t(time_others), stringsAsFactors = FALSE)
    time_others <- time_others %>% mutate_all(as.numeric)
    # Sum
    time_others_sum <- apply(time_others, 2, sum)
    # In percentage
    time_others_p <- (time_others_sum/tot_time_sens_data)*100
    time_others_p[which(is.na(time_others_p))] <- 0
    # Per hour
    sum_hours <- function(x) {
      start <- 0
      end <- 0
      j <- 1
      hour_base <- numeric(14)
      while (end < 50400) { 
        start <- end + 1
        end <- start + 3599
        hour_base[j] <- sum(x[start:end])
        j <- j + 1
      }
      return(hour_base)
    }
    time_others_perhour <- apply(time_others, 2, sum_hours)
    
    nrow_hours <- function(x) {
      start <- 0
      end <- 0
      j <- 1
      nrow_base <- numeric(14)
      while (end < 50400) { 
        start <- end + 1
        end <- start + 3599
        nrow_table <- table(x[start:end])
        nrow_table <- sum(nrow_table[which(names(nrow_table) != "0")])
        nrow_base[j] <- nrow_table
        j <- j + 1
      }
      return(nrow_base)
    }
    time_others_nrow <- apply(time_others_df, 2, nrow_hours)
    
    # Time around each patient per day
    patients <- unique(grep('P+', names(time_others_df), value=TRUE))
    time_each_patient <- time_others_df[,patients]
    list_time_eachpatient <- list()
    for(i in 1:ncol(time_each_patient)) {
      temp_eachpatient_df <- time_each_patient[which(time_each_patient[,i] != "0"),]
      patient_ref <- temp_eachpatient_df[,i]
      temp_eachpatient_df <- temp_eachpatient_df[,-i]
      time_temp <- apply(temp_eachpatient_df, 2, function(x) {
        x <- x == patient_ref
        x <- sum(x)
      })
      list_time_eachpatient[[i]] <- time_temp
      names(list_time_eachpatient)[i] <- names(time_each_patient)[i]
    }
    
    adjency_list <- merge(patients_all, patients_all)
    A <- as.character(adjency_list$x)
    B <- as.character(adjency_list$y)
    adjency_list <- cbind(A, B)
    each_pat_nrow <- apply(adjency_list, 1, function(x) {
      cond1 <- unlist(x[[1]])
      cond2 <- unlist(x[[2]])
      cond_vec <- c(cond1, cond2)
      if(sum(cond_vec %in% names(time_each_patient)) == 2) {
        temp_eachpatientnrow_df <- time_each_patient[, cond_vec] 
        temp_eachpatientnrow_df <- temp_eachpatientnrow_df[which(temp_eachpatientnrow_df[,1] != 0),]
        temp_eachpatientnrow_df <- temp_eachpatientnrow_df[which(temp_eachpatientnrow_df[,2] != 0),]
        return(nrow(temp_eachpatientnrow_df))
      }
      else{return(0)}
    })
    each_pat_nrow <- cbind(adjency_list, each_pat_nrow)
    
    # Time in each zone per hour
    sum_time_section <- function(x) {
      start <- 0
      end <- 0
      j <- 1
      sum_df_time <- data.frame(matrix(ncol = 2, nrow = 0))
      colnames(sum_df_time) <- c("sum_time_public", "sum_time_private")
      while (end < 50400) { 
        start <- end + 1
        end <- start + 3599
        person_zone <- table(x[start:end])
        sum_time_public <- sum(person_zone[which(names(person_zone) %in% public_zones)])
        sum_time_private <- sum(person_zone[which(names(person_zone) %in% private_rooms)])  
        sum_df_time <- rbind(sum_df_time, data.frame(sum_time_public, sum_time_private))
        j <- j + 1
      }
      return(sum_df_time)
    }
    time_zone_perhour <- apply(time_others_df, 2, sum_time_section)
    
    # Time around nurses.
    time_nurses_df <- df_loc_preds
    time_nurses_df <- time_nurses_df %>% mutate_all(as.character)
    nurses <- unique(grep('N+', names(time_nurses_df), value=TRUE))
    
    if(length(nurses) < 1) { time_nurses_p <- NULL
    } else {
      # Remove rows with no nurse data
      empty_rows_f <- function(x) {
        zero_vector_n <- as.character(integer(length(x)))
        all(x == zero_vector_n)
      }
      
      if(length (nurses) == 1) {
        temp_nurse <- data.frame(time_nurses_df[,which(colnames(time_nurses_df) %in% nurses)])
        colnames(temp_nurse) <- nurses
        empty_rows <- apply(temp_nurse, 1, empty_rows_f)
        time_nurses_df <- time_nurses_df[which(empty_rows==FALSE),]
      } else {
        empty_rows <- apply(time_nurses_df[,which(colnames(time_nurses_df) %in% nurses)], 1, empty_rows_f)
        time_nurses_df <- time_nurses_df[which(empty_rows==FALSE),]
      }
  
      
      # Calculate time around nurses for each patient
      if(length (nurses) == 1) {
        nurse_df <- data.frame(time_nurses_df[,which(colnames(time_nurses_df) %in% nurses)])
        colnames(nurse_df) <- nurses
      } else{
        nurse_df <- time_nurses_df[,which(colnames(time_nurses_df) %in% nurses)]
      }
      
      time_nurses <- data.frame(matrix(ncol = 0, nrow = 1))
      tot_sensor_data <- data.frame(matrix(ncol = 0, nrow = 1))
      time_nurse_vectorlist_hours <- list()
      time_nurse_hours_nrow <- list()
      for(i in 1:length(patients)) {
        temp_df <- cbind(nurse_df, time_nurses_df[,patients[i]], stringsAsFactors = FALSE)
        names(temp_df)[ncol(temp_df)] <- patients[i]
        temp_df <- temp_df[temp_df[,patients[i]]!="0",]
        
        if(nrow(temp_df) == 0) {
          # If there is no patient and nurse data at the same time
          # This division will cause NaN
          time_nurses[patients[i]] <- 0
          tot_sensor_data[patients[i]] <- 0
        } else {
          check_section <- apply(temp_df, 1, function(x) {
            x[length(x)] %in% x[1:length(x)-1]
          })
          time_tog <- sum(check_section==TRUE)
          time_nurses[patients[i]] <- time_tog
          tot_sensor_data[patients[i]] <- nrow(temp_df)
          time_nurse_vectorlist_hours[[i]] <- check_section
          names(time_nurse_vectorlist_hours)[i] <- patients[i]
          
          # Per hour nrow
          sum_nurse_data <- function(x) {
            start <- 0
            end <- 0
            j <- 1
            nurse_nrow <- numeric(14)
            while (end < 50400) { 
              start <- end + 1
              end <- start + 3599
              nurse_nrow[j] <- sum(between(as.numeric(rownames(temp_df)), start, end))
              j <- j + 1
            }
            return(nurse_nrow)
          }
          nurse_hours_nrow <- sum_nurse_data(temp_df[,1])
          time_nurse_hours_nrow[[i]] <- nurse_hours_nrow
          names(time_nurse_hours_nrow)[i] <- patients[i]
        }
      }
      # Sum
      time_nurses_sum <- apply(time_nurses, 2, sum)
      # In percentage
      # Set NaNs to 0
      time_nurses_p <- (time_nurses/tot_sensor_data)*100
      time_nurses_p[which(is.na(time_nurses_p))] <- 0
      time_nurses_p <- unlist(time_nurses_p)
      # Per hour
      df_time_nurse_perhour <- data.frame(matrix(0, ncol = length(names(time_nurse_vectorlist_hours)), nrow = 50400))
      for(i in 1:length(names(time_nurse_vectorlist_hours))) {
        names(df_time_nurse_perhour)[i] <- names(time_nurse_vectorlist_hours)[i]
        temp_vec <- time_nurse_vectorlist_hours[[i]]
        df_time_nurse_perhour[as.numeric(names(temp_vec[which(temp_vec)])), i] <- 1
    }
      time_nurses_perhour <- apply(df_time_nurse_perhour, 2, sum_hours)
  }
    
    current_date <- as.Date(day_start_timestamp)
    
    # Time around others per hour
    time_others_perhour <- as.data.frame(time_others_perhour)
    mis_pat <- patients_all[which(!patients_all %in% names(time_others_df))]
    time_others_perhour[mis_pat] <- 0
    mis_nurse <- nurses_all[which(!nurses_all %in% names(time_others_df))]
    time_others_perhour[mis_nurse] <- 0
    time_others_perhour <- time_others_perhour[c(nurses_all, patients_all)]
    setwd(output_dir)
    save(time_others_perhour, file = paste(current_date, "time_others.Rdata", sep = " - "))

    # Time around others per hour nrow
    time_others_nrow <- as.data.frame(time_others_nrow)
    mis_pat <- patients_all[which(!patients_all %in% names(time_others_df))]
    time_others_nrow[mis_pat] <- 0
    mis_nurse <- nurses_all[which(!nurses_all %in% names(time_others_df))]
    time_others_nrow[mis_nurse] <- 0
    time_others_nrow <- time_others_nrow[c(nurses_all, patients_all)]
    setwd(output_dir)
    save(time_others_nrow, file = paste(current_date, "time_others_nrow.Rdata", sep = " - "))
    
    # Social proximity dataframe
    prox_df <- data.frame(matrix(ncol = 15, nrow = 15, 0))
    colnames(prox_df) <- patients_all
    rownames(prox_df) <- patients_all
    for(i in 1:length(patients_all)) {
      temp_vec <- list_time_eachpatient[[patients_all[i]]]
      if (length(temp_vec) > 0) {
        mis_pat <- patients_all[which(!patients_all %in% names(temp_vec))]
        temp_vec[mis_pat] <- 0
        temp_vec <- temp_vec[patients_all]
        prox_df[i,] <- temp_vec
      }
    }
    setwd(output_dir)
    save(prox_df, file = paste(current_date, "social_proximity.Rdata", sep = " - "))
    
    # Social proximity dataframe nrow
    prox_df_nrow <- as.data.frame(each_pat_nrow)
    prox_df_nrow <- spread(prox_df_nrow, A, each_pat_nrow)
    rownames_pdn <- prox_df_nrow$B
    prox_df_nrow <- prox_df_nrow[,-1]
    prox_df_nrow <- mutate_all(prox_df_nrow, as.character) %>% mutate_all(as.numeric)
    rownames(prox_df_nrow) <- rownames_pdn
    prox_df_nrow <- prox_df_nrow[patients_all,patients_all]
    prox_df_nrow <- as.matrix(prox_df_nrow)
    diag(prox_df_nrow) <- 0
    prox_df_nrow <- as.data.frame(prox_df_nrow)
    setwd(output_dir)
    save(prox_df_nrow, file = paste(current_date, "social_proximity_nrow.Rdata", sep = " - "))
    
    # Time per zone per hour
    sum_df_time <- data.frame(matrix(ncol = 2, nrow = 14, 0))
    colnames(sum_df_time) <- c("sum_time_public", "sum_time_private")
    nurspat_vec <- c(nurses_all, patients_all)
    miss_nurspat <- nurspat_vec[which(!nurspat_vec %in% names(time_zone_perhour))]
    if (length(miss_nurspat) > 0) {
      for(i in 1:length(miss_nurspat)) {
        time_zone_perhour[[miss_nurspat[i]]] <- sum_df_time
      }
    }
    time_zone_perhour <- time_zone_perhour[nurspat_vec]
    setwd(output_dir)
    save(time_zone_perhour, file = paste(current_date, "time_per_zone.Rdata", sep = " - "))
    
    # Time around nurses per hour
    time_nurses_perhour <- as.data.frame(time_nurses_perhour)
    mis_pat <- patients_all[which(!patients_all %in% names(time_nurses_perhour))]
    time_nurses_perhour[mis_pat] <- 0
    time_nurses_perhour <- time_nurses_perhour[, patients_all]
    setwd(output_dir)
    save(time_nurses_perhour, file = paste(current_date, "time_nurses.Rdata", sep = " - "))
    
    # Time around nurses per hour nrow
    time_nurse_hours_nrow <- as.data.frame(time_nurse_hours_nrow)
    mis_pat <- patients_all[which(!patients_all %in% names(time_nurse_hours_nrow))]
    time_nurse_hours_nrow[mis_pat] <- 0
    time_nurse_hours_nrow <- time_nurse_hours_nrow[, patients_all]
    setwd(output_dir)
    save(time_nurse_hours_nrow, file = paste(current_date, "time_nurses_nrow.Rdata", sep = " - "))
    }