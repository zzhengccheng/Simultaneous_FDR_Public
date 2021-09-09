
args=commandArgs(trailingOnly=TRUE)
i=as.numeric(args[1])

scale=2
filename=paste("Simulation_base_binary",as.character(i),".R",sep="")
source(filename)