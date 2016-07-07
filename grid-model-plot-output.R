####################################
#
#	Grid Test for Merged Zones - Plots
# By Row
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

#install.packages("ggplot2)
library(ggplot2)

####################################
#	Constants
####################################

AnalysisYear <- 2015

#setwd(paste("C:/Users/Brandon/Dropbox/", AnalysisYear+1, " TAC Analysis/", sep=""))

####################################
# Create Plots
####################################

all.results <- NULL

for (regions in 1:32)
{
  temp.data<-read.csv(paste("C:/Users/Brandon/Documents/GitHub/grid-model/Plots/", regions, " parameter estimates.csv", sep=""))
  temp.data<-data.frame(zone=regions, temp.data)
    
  all.results<-rbind(all.results, temp.data)
}

param.list<-c('KK', 'r', 'q', 'MSY')
param.name<-c("Carrying Capacity (K)", "Growth Rate (r)", "Catchability Coefficient (q)", "Maximum Sustainable Yield (MSY)")
for (param in param.list)
{
  temp.data<-all.results[all.results$names==param, ]
  
  p1<-ggplot(temp.data, aes(x=zone, y=Mean, colour=zone)) 
  p1<-p1 + geom_line(size=2)
  #p1<-p1 + geom_line(data=temp.data, aes(x=max.year, y=X97.5., group=zone, colour=zone), size=0.5, linetype=2)
  p1<-p1 + geom_ribbon(data=temp.data, aes(ymin=X2.5., ymax=X97.5., group=zone, colour=NULL, fill=zone, alpha=0.2), alpha=0.2)
  p1<-p1 + xlab("Zone")
  p1<-p1 + ylab(param.name[match(param, param.list)])
  p1<-p1 + ylim(0, max(temp.data$X97.5))
  #p1<-p1 + xlim(AnalysisYear-9, AnalysisYear+1)
  p1<-p1 + ggtitle(paste("Estimated ", param.name[match(param, param.list)], "\nby Zone", sep=""))
  p1<-p1 + theme(text=element_text(size=30), legend.position="bottom", axis.text.x=element_text(angle=90))
  #p1<-p1 + scale_x_continuous(breaks=seq(AnalysisYear-9, AnalysisYear+1, 1))
  p1
  
  # output to file in the report directory
  png(paste("C:/Users/Brandon/Documents/GitHub/grid-model/Plots/output/", param, "_by_Zone.png", sep=""), width=960, height=960)
  print(p1)
  dev.off()
} 
