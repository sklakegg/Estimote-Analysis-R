library(RColorBrewer)

current_dir <- "#"

# Load data
setwd(paste(current_dir, "/activity", sep = ""))
temp = list.files(pattern="*.Rdata")

activity_sum <- 0
for(i in 1:length(temp)) {
  load(temp[i])
  activity_sum <- activity_sum + data.frame(activity_sum_intervals)
}

# Load nrow data
setwd(paste(current_dir, "/activity_nrow", sep = ""))
temp = list.files(pattern="*.Rdata")

activity_nrow <- 0
for(i in 1:length(temp)) {
  load(temp[i])
  activity_nrow <- activity_nrow + data.frame(activity_nintervals)
}

# User activity per hour
mean_timehour <- apply(activity_sum/activity_nrow, 1, mean)
time_interval <- factor(c("06:30\n07:30", "07:30\n08:30", "08:30\n09:30", "09:30\n10:30", "10:30\n11:30", "11:30\n12:30",
                          "12:30\n13:30", "13:30\n14:30", "14:30\n15:30", "15:30\n16:30", "16:30\n17:30", "17:30\n18:30",
                          "18:30\n19:30", "19:30\n20:30"))
df_timehour <- data.frame(mean_timehour, time_interval)

# Activity per user
mean_activity_user <- data.frame(as.list((apply(activity_sum/activity_nrow, 2, mean))))
mean_activity_user$N1 <- mean(unlist(mean_activity_user[c("N1", "N2", "N3", "N4", "N5", "N6")]))
mean_activity_user <- mean_activity_user[1:16]
names(mean_activity_user)[16] <- "N_AVG"
mean_activity_user <- melt(mean_activity_user)
mean_activity_user$group <- factor(c(rep("Patient", 15), rep("Nurse", 1)), levels = c("Patient", "Nurse"))

# Nurse activity by weekday
setwd(paste(current_dir, "/activity", sep = ""))
temp = list.files(pattern="*.Rdata")
weekday_sum <- data.frame(matrix(nrow = 1, ncol = 7, 0))
colnames(weekday_sum) <- c(0, 1, 2, 3, 4, 5, 6)
for(i in 1:length(temp)) {
  wkday <- as.character(as.POSIXlt(strsplit(temp[i], " ")[[1]][1])$wday)
  load(temp[i])
  weekday_sum[,wkday] <- weekday_sum[,wkday] + sum(data.frame(activity_sum_intervals)[,16:21])
}

setwd(paste(current_dir, "/activity_nrow", sep = ""))
temp = list.files(pattern="*.Rdata")
weekday_nrow <- data.frame(matrix(nrow = 1, ncol = 7, 0))
colnames(weekday_nrow) <- c(0, 1, 2, 3, 4, 5, 6)
for(i in 1:length(temp)) {
  wkday <- as.character(as.POSIXlt(strsplit(temp[i], " ")[[1]][1])$wday)
  load(temp[i])
  weekday_nrow[,wkday] <- weekday_nrow[,wkday] + sum(data.frame(activity_nintervals)[,16:21])
}

weekday_sum_perc <- weekday_sum/weekday_nrow
colnames(weekday_sum_perc) <- c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")
weekday_sum_perc <- weekday_sum_perc[,c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")]
weekday_sum_perc <- melt(weekday_sum_perc)

# Plots
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
col_gra <- c("#e6194b", "#3cb44b", "#ffe119", "#0082c8", "#f58231", "#911eb4", "#46f0f0", "#f032e6", "#d2f53c", "#fabebe", 
             "#008080", "#e6beff", "#aa6e28", "#800000", "#aaffc3", "#808000", "#ffd8b1", "#000080", "#808080", "#808080", "#000000")

plot_theme <- theme_bw() + theme(strip.background = element_blank()) + theme(strip.background = element_blank())+ theme(panel.margin = unit(1.5, "lines")
) + theme(text = element_text(size = 20, face = "bold")) + theme( axis.title.x = element_text(size=30), axis.title.y = element_text(size=30),
                                                                  axis.text.x=element_text(colour="black")) + theme(axis.text.y=element_text(colour="black")) 

# activity_hour_plot
activityhour_plot <- ggplot(df_timehour, aes(x = time_interval, y = mean_timehour, fill = time_interval)) + geom_bar(stat = "identity") + scale_fill_manual(
  values = col_gra) + xlab("Time interval") + ylab("Activity (MAD)") + plot_theme + guides(fill=FALSE) 
setwd("/Desktop")
ggsave("activity_hour_plot.png", width = 36, height = 17, units = "cm", dpi = 300)

# activity_user_plot
activityuser_plot <- ggplot(mean_activity_user, aes(x = variable, y = value, fill = variable)) + geom_bar(stat = "identity") + scale_fill_manual(
  values = col_vector) + xlab("User") + ylab("Activity (MAD)") + plot_theme + guides(fill=FALSE) + facet_grid(. ~ group, scales = "free_x", space = "free"
  ) + theme(strip.text.x = element_text(size = 25, colour = "orange"))
setwd("/Desktop")
ggsave("activity_user_plot.png", width = 44, height = 17, units = "cm", dpi = 300)

# activityweekday_plot
activityweekday_plot <- ggplot(weekday_sum_perc, aes(x = variable, y = value, fill = variable)) + geom_bar(stat = "identity") + scale_fill_manual(
  values = col_vector) + xlab("Weekday") + ylab("Nurse activity (MAD)") + plot_theme + guides(fill=FALSE)
# GG save.
setwd("/Desktop")
ggsave("activity_weekday.png", width = 36, height = 17, units = "cm", dpi = 300)