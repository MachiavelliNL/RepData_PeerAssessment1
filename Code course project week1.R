
## read the data
##setwd("Data Science Specialization/Reproducable Research/RepData_PeerAssessment1")



##What is mean total number of steps taken per day?
DF_date <- summarize(group_by(DF,date),total = sum(steps,na.rm = T))
hist(DF_date$total)
Steps_mean <- mean(DF_date$total,na.rm = T) ## 9354.23
Steps_median <- median(DF_date$total,na.rm = T) ## 10395


##What is the average daily activity pattern?
DF_interval <- summarize(group_by(DF,interval),mean = mean(steps,na.rm = T))
plot(DF_interval$interval,DF_interval$mean,type="l")
Max_steps_interval <- as.character(DF_interval[which.max(DF_interval$mean),1])

##Imputing missing values
sum(is.na(DF$steps))
DF_new <- merge(DF,DF_interval,by="interval")
DF_new$steps <- ifelse(is.na(DF_new$steps),DF_new$mean,DF_new$steps)
DF_new <- DF_new[,1:3]


DF_new_date <- summarize(group_by(DF_new,date),total = sum(steps,na.rm = T))
hist(DF_new_date$total)
Steps_new_mean <- mean(DF_new_date$total) ## 10766.19
Steps_new_median <- median(DF_new_date$total) ## 10766.19


weekdays <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')
DF_new$DayOfTheWeek <- factor((weekdays(DF_new$date) %in% weekdays), 
                   levels=c(FALSE, TRUE), labels=c('weekend', 'weekday'))
 

DF_new_interval <- summarize(group_by(DF_new,interval,DayOfTheWeek),mean = mean(steps,na.rm = T))


ggplot(DF_new,aes(steps,interval))+geom_line()+facet_grid(. ~ DayOfTheWeek)
