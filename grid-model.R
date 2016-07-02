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

HER<-read.csv(paste("C:/Users/Brandon/SkyDrive/URA/grid-model/", AnalysisYear, "_Harvest_Data_All.csv", sep=""))

#HER.MERGE<-HER[HER$ZONE=="4-1", "4-2", "4-3", "4-7", "4-4", "4-5", ]
