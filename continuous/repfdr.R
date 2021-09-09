####fitting using repfdr
library(repfdr)
myest_repfdr<-function(data,q){
	##method Max: sign max
	##method Diff: difference
	X<-as.matrix(data[,-c(1:2)])
	Xc<-X%*%diag(1/sqrt(colSums(X^2)))
	y<-data[,2]
	fit1<-glm(y[which(data$C==1)]~Xc[which(data$C==1),]-1,family="gaussian")
	Z1=summary(fit1)$coef[,3]
	fit2<-glm(y[which(data$C==2)]~Xc[which(data$C==2),]-1,family="gaussian")
	Z2=summary(fit2)$coef[,3]
	Zmat=cbind(Z1,Z2)
	ztobins_res = ztobins(Zmat,n.association.status = 2,plot.diagnostics = FALSE,n.bin= 100)
	repfdr_res = repfdr(ztobins_res$pdf.binned.z,
                    ztobins_res$binned.z.mat,
                    non.null = 'meta-analysis')
	
	myselect=as.numeric(which(repfdr_res$mat[,2] <= q))
	return(myselect)
}