####################################
#
#	Grid Test for Merged Zones
#
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

#install.packages("rjags")
#install.packages("coda")		
library(rjags)
library(coda)

####################################
#	Constants
####################################

AnalysisYear <- 2015
step.size <- 10

####################################
# File IO
####################################

mainDir <- paste(getwd(), "/", sep = "")
subDir <- paste("Step Size ", step.size, "/", sep = "")
dir.create(file.path(mainDir, subDir))

####################################
#	Load Data
####################################	

HER <- read.csv(paste(mainDir, AnalysisYear, "_Harvest_Data_All.csv", sep=""))

####################################
# Merge Zones and Get Grid Rows
####################################

#Main Basin Zones
HER.MERGED <- HER[HER$ZONE %in% c("4-5", "4-4", "4-7", "4-3", "4-2", "4-1"), ]
HER.MERGED <- HER.MERGED[order(-HER.MERGED$GRID),]
HER.MERGED$GRID <- (HER.MERGED$GRID - (HER.MERGED$GRID %% 100)) / 100

####################################
# Parameter Estimates of Aggregated
# Grids
####################################

grid.rows <- unique(HER.MERGED$GRID)

for (i in step.size:length(grid.rows))
{
  numRegions <- 1
  numYears <- 32
  region <- i - (step.size - 1)
  
  #This loop gets the rows to be analyzed
  HER.ANALYZE <- NULL
  for (j in (i - (step.size - 1)):i)
  {
    temp.data <- HER.MERGED[HER.MERGED$GRID %in% c(grid.rows[j]), ]
    HER.ANALYZE <- rbind(HER.ANALYZE, temp.data)
  }
  
  #	harvest in kg, effort in km
  harvest<-aggregate(HER.ANALYZE$HVSWT_KG, by=list(HER.ANALYZE$YEAR), sum)
  effort<-aggregate(HER.ANALYZE$GRLEN5/1000, by=list(HER.ANALYZE$YEAR), sum)
  
  #	Use this to pull 1 zone at a time
  
  harvest<-harvest[harvest[, 1]>(AnalysisYear-numYears), ]
  effort<-effort[effort[, 1]>(AnalysisYear-numYears), ]
  
  yearsneeded<-seq(AnalysisYear-numYears+1, AnalysisYear)
  missingYears<-data.frame(Group.1=yearsneeded)
  
  harvest<-merge(harvest, missingYears, by=intersect(names(harvest), names(missingYears)), all=TRUE)
  
  harvest$x[is.na(harvest$x)]<-0.1
  
  effort<-merge(effort, missingYears, by=intersect(names(effort), names(missingYears)), all=TRUE)
  
  effort$x[is.na(effort$x)]<-0.1
  
  harvest<-harvest[, 2]
  effort<-effort[, 2]
  
  ####################################
  #	Run JAGS Model
  ####################################	
  
  model.file<-paste(mainDir, "SP_Model_", AnalysisYear, ".txt", sep="")
  
  jags<-jags.model(model.file, 
                   data = list('C'=harvest, 
                               'I'=effort, 
                               'numPeriods'=numYears, 
                               'numRegions'=numRegions),
                   inits = list('P'=rep(0.5, numYears*numRegions), 'r'=rep(0.3, numRegions), 'KK'=rep(2000000, numRegions), iq=rep(10000, numRegions), tau=c(1), sigma=c(4), Pinitial=rep(0.5, numRegions)),
                   n.chains=3,
                   n.adapt=10000)
  
  #	Estimate number of iterations required for convergence
  
  #	ZONE1	25000 to 50000
  #	ZONE2	25000 to 50000
  #	ZONE3	25000 to 50000
  #	4-5		25000 to 50000
  
  param.list<-c('KK', 'r', 'q', 'MSY')
  samples<-coda.samples(jags, param.list, 25000)
  
  eval(parse(text=paste("gelman.", gsub("-", ".", region), "<-gelman.diag(samples)", sep="")))
  
  pdf(paste(mainDir, subDir, "Gelman_Plots_", region, ".pdf", sep=""), paper="a4")
  gelman.plot(samples)
  dev.off()
  
  #	Most Gelman plots look good at iter=25000, 
  #	Opt to up them to 50000
  
  param.list<-c('KK', 'r', 'q', 'MSY', 'Bpred')
  samples<-coda.samples(jags, param.list, 40000)
  
  results.mcmc<-summary(window(samples, start=35000))
  
  results.1<-data.frame(results.mcmc[[1]])
  results.2<-data.frame(results.mcmc[[2]])
  results.1<-data.frame(names=row.names(results.1), results.1, results.2)
  write.csv(results.1, paste(mainDir, subDir, region, " parameter estimates.csv", sep=""))
  
}