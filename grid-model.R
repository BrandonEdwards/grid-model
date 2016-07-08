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

#setwd(paste("C:/Users/Brandon/Dropbox/", AnalysisYear+1, " TAC Analysis/", sep=""))

####################################
#	Load Data
####################################	

HER <- read.csv(paste("C:/Users/Brandon/Documents/GitHub/grid-model/", AnalysisYear, "_Harvest_Data_All.csv", sep=""))

HER.MERGED <- HER[ ! HER$ZONE %in% c("6-1", "6-3", "5-1", "5-2", "5-3", "5-4", "5-5", "5-6", "5-7", "5-8", "5-9"), ]
HER.MERGED <- HER.MERGED[order(-HER.MERGED$GRID),]
HER.MERGED$GRID <- (HER.MERGED$GRID - (HER.MERGED$GRID %% 100)) / 100

####################################
# Analysis of Unique Stepped Grids
####################################

grid.rows <- unique(HER.MERGED$GRID)

for (i in 7:length(grid.rows))
{
  numRegions <- 1
  numYears <- 32
  region <- i - 4
  
  HER.ANALYZE<-HER.MERGED[HER.MERGED$GRID %in% c(grid.rows[i], grid.rows[i-1], grid.rows[i-2], grid.rows[i-3], grid.rows[i-4]), ]
  
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
  
  model.file<-paste("C:/Users/Brandon/Documents/GitHub/grid-model/SP_Model_", AnalysisYear, ".txt", sep="")
  
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
  
  pdf(paste("C:/Users/Brandon/Documents/GitHub/grid-model/Plots/Gelman_Plots_", region, ".pdf", sep=""), paper="a4")
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
  write.csv(results.1, paste("C:/Users/Brandon/Documents/GitHub/grid-model/Plots/", region, " parameter estimates.csv", sep=""))
  
}