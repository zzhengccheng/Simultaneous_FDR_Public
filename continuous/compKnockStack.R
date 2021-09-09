####fitting
myest_stack<-function(data,q,method="Max"){
	##method Max: sign max
	##method Diff: difference
	X<-as.matrix(data[,-c(1:2)])
	Xc<-X%*%diag(1/sqrt(colSums(X^2)))
	y<-data[,2]
	XKnock<-create.fixed(Xc)$Xk
	fit<-cv.glmnet(as.matrix(cbind(Xc,XKnock)),c(y),family="gaussian",standardize=FALSE,intercept=FALSE)
	Z=abs(coef(fit)[1+(1:p)])
	Ztilde=abs(coef(fit)[1+p+(1:p)])
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