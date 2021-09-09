
args=commandArgs(trailingOnly=TRUE)
i=as.numeric(args[1])

scale=1.2
filename=paste("Simulation_base",as.character(i),".R",sep="")
source(filename)