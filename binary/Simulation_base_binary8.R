
#####baseline setting
n1=1000
n2=1000
p=200
s0=40
s1=40
s2=40
q=0.2
rho1=rho2=0.5
sigma1=1
sigma2=-1
samesig=1
iniseed=1111

#####do parallel setting on s0 with 6 core
library(doParallel)
registerDoParallel(cores=10)
M=1000

for (rho1 in c(0.25,0.3,0.35)){
rho2=1-rho1
result=foreach (m=1:M, .combine=cbind)%dopar%{
	source("datagen_Binary.R")
	source("compKnock_Binary.R")
	source("compKnockStack_Binary.R")
	source("compKnockInt_Binary.R")
	set.seed(iniseed+m)
	data=datagen(n1,n2,p,s0,s1,s2,rho1,rho2,sigma1,sigma2,samesig,scale)
	myselect=myest(data,q,method="Diff")
	myselect_stack=myest_stack(data,q,method="Diff")
	myselect_int=myest_int(data,q,method="Diff")
	####evaluate FDR and power
    TP=FDP=TP_stack=FDP_stack=TP_int=FDP_int=NA
	TP=length(which(myselect%in%c(1:s0)))
	FDP=(length(myselect)-TP)/max(1,length(myselect))
	TP_stack=length(which(myselect_stack%in%c(1:s0)))
	FDP_stack=(length(myselect_stack)-TP_stack)/max(1,length(myselect_stack))
	TP_int=length(which(myselect_int%in%c(1:s0)))
	FDP_int=(length(myselect_int)-TP_int)/max(1,length(myselect_int))
    c(FDP,TP,FDP_stack,TP_stack,FDP_int,TP_int)
}

FDP=result[1,]
TP=result[2,]
FDP_stack=result[3,]
TP_stack=result[4,]
FDP_int=result[5,]
TP_int=result[6,]

FDR=mean(FDP,na.rm=TRUE)
power=mean(TP,na.rm=TRUE)/s0
FDR_stack=mean(FDP_stack,na.rm=TRUE)
power_stack=mean(TP_stack,na.rm=TRUE)/s0
FDR_int=mean(FDP_int,na.rm=TRUE)
power_int=mean(TP_int,na.rm=TRUE)/s0
result=c(FDR,power,FDR_stack,power_stack,FDR_int,power_int)
write.csv(result,paste("Binary_result_n1=",as.character(n1),"n2=",as.character(n2),"p=",as.character(p),"s0=",as.character(s0),"s1=",as.character(s1),"s2=",as.character(s2),"rho1=",as.character(rho1),"rho2=",as.character(rho2),"sigma1=",as.character(sigma1),"sigma2=",as.character(sigma2),"samesig=",as.character(samesig),"q=",as.character(q),"scale=",as.character(scale),"iniseed=",as.character(iniseed),".csv",sep=""))
}
