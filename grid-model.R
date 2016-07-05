####################################
#
#	Grid Test for Merged Zones
#	Dan Gillis & Brandon Edwards
#	July 2016
#
####################################

####################################
#	Clear Memory
####################################

remove(list=ls())

####################################
#	Require Libraries
####################################


####################################
#	Constants
####################################

AnalysisYear<-2015

#setwd(paste("C:/Users/Brandon/Dropbox/", AnalysisYear+1, " TAC Analysis/", sep=""))

####################################
#	Load Data
####################################	

HER<-read.csv(paste("C:/Users/Brandon/Documents/GitHub/grid-model/", AnalysisYear, "_Harvest_Data_All.csv", sep=""))

HER.MERGE<-HER[ ! HER$ZONE %in% c("6-1", "6-3", "5-1", "5-2", "5-3", "5-4", "5-5", "5-6", "5-7", "5-8", "5-9"), ]
HER.MERGE<-HER.MERGE[order(-HER.MERGE$GRID),]

####################################
# Analysis of Stepped Grids
####################################

#Get unique grid numbers
grid <- unique(HER.MERGE$GRID)

for (i in 5:length(grid))
{
  HER.ANALYZE<-HER.MERGE[HER.MERGE$GRID %in% c(grid[i], grid[i-1], grid[i-2], grid[i-3], grid[i-4]), ]
}
