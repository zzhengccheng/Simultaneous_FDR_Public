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
	X1Knock<-create.fixed(X1c)$Xk
	X2Knock<-create.fixed(X2c)$Xk
	fit1<-cv.glmnet(as.matrix(cbind(X1c,X1Knock)),c(y1),family="gaussian",standardize=FALSE,intercept=FALSE)
	fit2<-cv.glmnet(as.matrix(cbind(X2c,X2Knock)),c(y2),family="gaussian",standardize=FALSE,intercept=FALSE)
	#fit1<-lm(y1~cbind(X1c,X1Knock))
	#fit2<-lm(y2~cbind(X2c,X2Knock))
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
	#list(myselect=myselect,Z=Z,Ztilde=Ztilde,Z1=Z1,Z1tilde=Z1tilde,Z2=Z2,Z2tilde=Z2tilde)
}