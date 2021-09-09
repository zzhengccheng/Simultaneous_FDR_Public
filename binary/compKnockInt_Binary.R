####fitting
library(knockoff)
library(glmnet)
myest_int<-function(data,q,method="Max"){
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
	if (method=="Max"){
		W1=pmax(Z1,Z1tilde)*(-1)^(Z1<=Z1tilde)
		W2=pmax(Z2,Z2tilde)*(-1)^(Z2<=Z2tilde)
	}
	if (method=="Diff"){
		W1=Z1-Z1tilde
		W2=Z2-Z2tilde
	}
	mythred1=knockoff.threshold(W1,fdr=q,offset=1)
	myselect1=which(W1>=mythred1)
	mythred2=knockoff.threshold(W2,fdr=q,offset=1)
	myselect2=which(W2>=mythred2)
	myselect=intersect(myselect1,myselect2)
	return(myselect)
}