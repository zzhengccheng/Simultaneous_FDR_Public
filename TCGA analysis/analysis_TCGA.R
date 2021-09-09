data<-read.csv("GBM_exp_clean.csv",header=TRUE)
data=data[which(!is.na(data$DAYSTODEATH)),]
seed=8888

####GENDER

X=data[,c(8:130)]
Y=log(data[,3])
C=1+as.numeric(data$GENDER=="female")
XY=cbind(C,Y,X)
names(XY)[1:2]=c("C","Y")
XY=na.omit(XY)

source("compKnock_hd.R")
source("compKnockStack_hd.R")
source("compKnockInt_hd.R")
source("repfdr.R")

q=0.1
p=ncol(XY)-2
set.seed(seed)
myselect=myest(XY,q,method="Diff")
myselect_stack=myest_stack(XY,q,method="Diff")
myselect_int=myest_int(XY,q,method="Diff")
myselect_repfdr=myest_repfdr(XY,q)


####GENDER SEPARATE
###female
X=data[which(data$GENDER=="female"),c(8:130)]
Y=log(data[which(data$GENDER=="female"),3])
C=sample(c(1,2),length(Y),replace=TRUE)
XY=cbind(C,Y,X)
names(XY)[1:2]=c("C","Y")
XY=na.omit(XY)

source("compKnock_hd.R")
source("compKnockStack_hd.R")
source("compKnockInt_hd.R")
source("repfdr.R")

q=0.1
p=ncol(XY)-2
set.seed(seed)
myselect_female=myest_stack(XY,q,method="Diff")



###male
X=data[which(data$GENDER=="male"),c(8:130)]
Y=log(data[which(data$GENDER=="male"),3])
C=sample(c(1,2),length(Y),replace=TRUE)
XY=cbind(C,Y,X)
names(XY)[1:2]=c("C","Y")
XY=na.omit(XY)

source("compKnock_hd.R")
source("compKnockStack_hd.R")
source("compKnockInt_hd.R")
source("repfdr.R")

q=0.1
p=ncol(XY)-2
set.seed(seed)
myselect_male=myest_stack(XY,q,method="Diff")



myselect
myselect_stack
myselect_int
myselect_repfdr
myselect_male
myselect_female








