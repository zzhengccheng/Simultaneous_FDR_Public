data<-read.delim("communities.data",na.strings="?",sep=",",header=FALSE)
data$C=as.numeric(data[,8]>median(data[,8]))+1
data$Y=data[,128]
data=data[,c(129,130,6:8,10:127)]

####use non-missing features
data=data[,which(apply(is.na(data),2,mean)==0)]

####remove race variable
data=data[,-c(5:7)]

source("compKnock.R")
source("compKnockStack.R")
source("compKnockInt.R")
source("repfdr.R")

q=0.1
p=ncol(data)-2
set.seed(8888)
myselect=myest(data,q,method="Diff")
myselect_stack=myest_stack(data,q,method="Diff")
myselect_int=myest_int(data,q,method="Diff")
myselect_repfdr=myest_repfdr(data,q)
myselect
myselect_stack
myselect_int
myselect_repfdr


####add fake feature from permutated data
fdata=data[sample(1:nrow(data),nrow(data),replace=TRUE),3:ncol(data)]
names(fdata)=paste(names(fdata),"F",sep="")
ffdata=cbind(data,fdata)


q=0.1
p=ncol(data)-2
set.seed(8888)
myselect=myest(ffdata,q,method="Diff")
myselect_stack=myest_stack(ffdata,q,method="Diff")
myselect_int=myest_int(ffdata,q,method="Diff")
myselect_repfdr=myest_repfdr(ffdata,q)
myselect
myselect_stack
myselect_int
myselect_repfdr




####add noise feature
idata=matrix(data=rnorm(100*nrow(data)),nrow=nrow(data),ncol=100)
idata=data.frame(idata)
names(idata)=paste("I",as.character(c(1:100)),sep="")

fidata=cbind(ffdata,idata)


q=0.1
p=ncol(data)-2
set.seed(8888)
myselect=myest(fidata,q,method="Diff")
myselect_stack=myest_stack(fidata,q,method="Diff")
myselect_int=myest_int(fidata,q,method="Diff")
myselect_repfdr=myest_repfdr(fidata,q)
myselect
myselect_stack
myselect_int
myselect_repfdr





