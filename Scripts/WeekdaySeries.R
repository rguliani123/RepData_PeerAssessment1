# Script Name: WeekdaySeries.R
#
## set working directory
# setwd("/Users/rguliani/Documents/RajeevDocs/Coursera/RepResearch/RepData_PeerAssessment1")

library(plyr)

## Step #1: read interval level activity data from the current dorectory
ActData <- read.csv("./activity.csv", sep = ","  , header = TRUE)
classes <- sapply(ActData, class)

# Filter NA values 
FilterActData<- ActData[ which(ActData$steps!='NA' ),]

# Create and assign weekday or weekend based on date using weekdays() function
FilterActData$day <- weekdays(as.Date(FilterActData$date))

categorize_day <- function(day) 
         {     day <- 
                 if (day %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))    
                   return("weekday") 
                 else if (day %in% c("Saturday", "Sunday"))        
                     return("weekend") 
                 else stop("Error in date conversion") 
}
FilterActData$dayType <- sapply(FilterActData$day, FUN = categorize_day)


# Aggregate steps data by interval and day type (weekend or weekday)
AggCatIntData <- ddply(FilterActData, .(interval,dayType), summarise, totsteps = mean(steps))

# Plot interval level data by interval and day type
ggplot(AggCatIntData, aes(interval, totsteps)) + geom_line() + facet_grid(dayType ~ .) +     
  xlab("Interval") + ylab("# steps")


## Saving to file
dev.copy(png, file="plot1.png", height=480, width=480)
dev.off()
