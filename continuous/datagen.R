library(MASS)
Sigma.AR<-function(rho,p){
	rho^abs(outer(1:p,1:p,"-"))
}

datagen<-function(n1,n2,p,s0,s1,s2,rho1,rho2,sigma1,sigma2,samesig,scale=1.2){
	###n1 sample size in population 1
	###n2 sample size in population 2
	###p number of predictors
	###s0 number of true signals
	###s1 number of signals shown in population 1 only
	###s2 number of signals shown in population 2 only
	###rho1 correlation parameter for design matrix in population 1
	###rho2 correlation parameter for design matrix in population 2
	###sigma1 noise parameter for population 1
	###sigma2 noise parameter for population 2
	###samesig=1 true signals magnitude same for both data
	###samesig=0 true signals magnitude different for both data
	###effect scale multiplier
	Sigma1<-Sigma.AR(rho1,p)
	Sigma2<-Sigma.AR(rho2,p)
	X1<-mvrnorm(n=n1,mu=rep(0,p),Sigma=Sigma1)
	X2<-mvrnorm(n=n2,mu=rep(0,p),Sigma=Sigma2)
	b0.1<-runif(s0)
      b0.2<-runif(s0)
      if (samesig){b0.2<-b0.1}
	b1<-runif(s1)
	b2<-runif(s2)
	ss0<-sample(c(-1,1),s0,prob=c(0.5,0.5),replace=TRUE)
	ss1<-sample(c(-1,1),s1,prob=c(0.5,0.5),replace=TRUE)
	ss2<-sample(c(-1,1),s2,prob=c(0.5,0.5),replace=TRUE)
	beta1<-c(b0.1*ss0,b1*ss1,rep(0,p-s0-s1))*scale
 	beta2<-c(b0.2*ss0,rep(0,s1),b2*ss2,rep(0,p-s0-s1-s2))*scale
	y1<-X1%*%beta1+rnorm(n1)*sigma1
	y2<-X2%*%beta2+rnorm(n2)*sigma2
	data1<-cbind(1,y1,X1)
	data2<-cbind(2,y2,X2)
	data<-data.frame(rbind(data1,data2))
	names(data)=c("C","Y",paste("X",1:p,sep=""))
	data
}