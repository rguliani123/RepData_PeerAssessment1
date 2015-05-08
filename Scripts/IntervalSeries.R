# Script Name: IntervalSeries.R
#
## set working directory
# setwd("/Users/rguliani/Documents/RajeevDocs/Coursera/RepResearch/RepData_PeerAssessment1")

library(plyr)

## Step #1: read interval level activity data from the current dorectory
ActData <- read.csv("./activity.csv", sep = ","  , header = TRUE)
classes <- sapply(ActData, class)

# Filter NA values 
FilterActData<- ActData[ which(ActData$steps!='NA' ),]


# Aggregate steps data by interval
AggIntData <- ddply(FilterActData, .(interval), summarise, totsteps = mean(steps))

# Plot the aggregate emissions data and format
plot(AggIntData$totsteps~AggIntData$interval,type="l", ylab="# Steps", xlab="Interval", col.lab="blue",
     main="# Steps by Interval", col = "red", ylim=c(0, 250), xlim=c(0, 2355), lwd=5)

## Saving to file
dev.copy(png, file="plot1.png", height=480, width=480)
dev.off()
