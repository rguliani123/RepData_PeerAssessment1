# Script Name: ActivityMissingSumamry.pl
#
## set working directory
# setwd("/Users/rguliani/Documents/RajeevDocs/Coursera/RepResearch/RepData_PeerAssessment1")

library(plyr)
library(gam)

## Step #1: read interval level activity data from the current directory
ActData <- read.csv("./activity.csv", sep = ","  , header = TRUE)
classes <- sapply(ActData, class)

# Fill missing values values 
# FilterActData<- ActData[ which(ActData$steps!='NA' ),]
FilledActData <- na.gam.replace (ActData)


# Aggregate steps data by date
AggData <- ddply(FilledActData, .(date), summarise, totsteps = sum(steps))

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
print(c("Mean: ", meanSteps),right = TRUE, row.names = FALSE)
medianSteps <- median(AggData$totsteps, na.rm = TRUE)
print(c("Median Steps: ",medianSteps),right = TRUE, row.names = FALSE)
