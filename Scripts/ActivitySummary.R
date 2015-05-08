# Script Name: ActivitySumamry.pl
#
## set working directory
# setwd("/Users/rguliani/Documents/RajeevDocs/Coursera/RepResearch/RepData_PeerAssessment1")

library(plyr)

## Step #1: read interval level activity data from the current dorectory
ActData <- read.csv("./activity.csv", sep = ","  , header = TRUE)
classes <- sapply(ActData, class)

# Filter NA values 
FilterActData<- ActData[ which(ActData$steps!='NA' ),]


# Aggregate steps data by date
AggData <- ddply(FilterActData, .(date), summarise, totsteps = sum(steps))

# Display total steps by day
print(AggData[,1:2],right = TRUE, row.names = FALSE)


## Plot a histogram of total steps by day
hist(AggData$totsteps, main="Distribution of Total Steps by Day", col="Red",
     ylab="Days", xlab="# of Steps")

## Saving to file
dev.copy(png, file="plot1.png", height=480, width=480)
dev.off()

# calculate mean and median
meanSteps <- mean(AggData$totsteps, na.rm = TRUE)
meanSteps <- round(meanSteps, digits = 2)
print(c("Mean: ", meanSteps),right = TRUE, row.names = FALSE)
medianSteps <- median(AggData$totsteps, na.rm = TRUE)
medianSteps <- round(medianSteps, digits = 2)
print(c("Median Steps: ",medianSteps),right = TRUE, row.names = FALSE)


