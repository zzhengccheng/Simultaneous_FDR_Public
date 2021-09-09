####fitting
library(knockoff)
library(glmnet)
myest<-function(data,q,method="Max"){
	##method Max: sign max
	##method Diff: difference
	data1<-data[which(data$C==1),]
	data2<-data[which(data$C==2),]
	X1<-as.matrix(data1[,-c(1:2)])
	X2<-as.matrix(data2[,-c(1:2)])
	X1c<-X1%*%diag(1/sqrt(colSums(X1^2)))
	X2c<-X2%*%diag(1/sqrt(colSums(X2^2)))
	y1<-data1[,2]
	y2<-data2[,2]
	X1Knock<-create.second_order(X1c)
	X2Knock<-create.second_order(X2c)
	fit1<-cv.glmnet(as.matrix(cbind(X1c,X1Knock)),c(y1),family="binomial",standardize=FALSE,intercept=TRUE)
	fit2<-cv.glmnet(as.matrix(cbind(X2c,X2Knock)),c(y2),family="binomial",standardize=FALSE,intercept=TRUE)
	Z1=abs(coef(fit1)[1+(1:p)])
	Z1tilde=abs(coef(fit1)[1+p+(1:p)])
	Z2=abs(coef(fit2)[1+(1:p)])
	Z2tilde=abs(coef(fit2)[1+p+(1:p)])
	Z=Z1*Z2+Z1tilde*Z2tilde
	Ztilde=Z1*Z2tilde+Z2*Z1tilde
	if (method=="Max"){
		W=pmax(Z,Ztilde)*(-1)^(Z<=Ztilde)
	}
	if (method=="Diff"){
		W=Z-Ztilde
	}
	mythred=knockoff.threshold(W,fdr=q,offset=1)
	myselect=which(W>=mythred)
	return(myselect)
}