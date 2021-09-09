source("FDR_power_plot.R")

####summarize binary results



####figure 2 change s0
n1=1000
n2=1000
p=200
s0=40
s1=0
s2=0
q=0.2
rho1=rho2=0.5
sigma1=1
sigma2=-1
samesig=1
iniseed=1111

scale=2
vary_list=c(10,20,30,40,50,60)
cols=c("black","red","blue")
pchs=c(15,16,17)
xlab="s0"
names=c("Simultaneous","Pooling","Intersection")
FDRmat=powermat=matrix(data=NA,nrow=length(vary_list),ncol=length(names))
for (i in 1:length(vary_list)){
	s0=vary_list[i]
	filename=paste("Binary_result_n1=",as.character(n1),"n2=",as.character(n2),"p=",as.character(p),"s0=",as.character(s0),"s1=",as.character(s1),"s2=",as.character(s2),"rho1=",as.character(rho1),"rho2=",as.character(rho2),"sigma1=",as.character(sigma1),"sigma2=",as.character(sigma2),"samesig=",as.character(samesig),"q=",as.character(q),"scale=",as.character(scale),"iniseed=",as.character(iniseed),".csv",sep="")
	tmp=c(read.csv(filename)[,2])
	FDRmat[i,]=tmp[c(1,3,5)]
	powermat[i,]=tmp[c(2,4,6)]
}

pdf("Binary_s0.pdf")
FDR_and_power_plot(FDRmat=FDRmat, powermat=powermat, names=names, cols=cols, pchs=pchs, alpha=q, vary_list,xlab=xlab, title='',leg_coords=c(min(vary_list),0.5),show_legend=TRUE,labelsize=1.5,legsize=1.5,cexsize=1.8)
dev.off()

####figure 3 change s1=s2
n1=1000
n2=1000
p=200
s0=40
s1=0
s2=0
q=0.2
rho1=rho2=0.5
sigma1=1
sigma2=-1
samesig=1
iniseed=1111

scale=2
vary_list=c(10,20,30,40,50,60)
cols=c("black","red","blue")
pchs=c(15,16,17)
xlab="s1=s2"
names=c("Simultaneous","Pooling","Intersection")
FDRmat=powermat=matrix(data=NA,nrow=length(vary_list),ncol=length(names))
for (i in 1:length(vary_list)){
	s2=s1=vary_list[i]
	filename=paste("Binary_result_n1=",as.character(n1),"n2=",as.character(n2),"p=",as.character(p),"s0=",as.character(s0),"s1=",as.character(s1),"s2=",as.character(s2),"rho1=",as.character(rho1),"rho2=",as.character(rho2),"sigma1=",as.character(sigma1),"sigma2=",as.character(sigma2),"samesig=",as.character(samesig),"q=",as.character(q),"scale=",as.character(scale),"iniseed=",as.character(iniseed),".csv",sep="")
	tmp=c(read.csv(filename)[,2])
	FDRmat[i,]=tmp[c(1,3,5)]
	powermat[i,]=tmp[c(2,4,6)]
}

pdf("Binary_s1_s2.pdf")
FDR_and_power_plot(FDRmat=FDRmat, powermat=powermat, names=names, cols=cols, pchs=pchs, alpha=q, vary_list,xlab=xlab, title='',leg_coords=c(min(vary_list),0.5),show_legend=TRUE,labelsize=1.5,legsize=1.5,cexsize=1.8)
dev.off()


####figure 4 change s2
n1=1000
n2=1000
p=200
s0=40
s1=0
s2=0
q=0.2
rho1=rho2=0.5
sigma1=1
sigma2=-1
samesig=1
iniseed=1111


scale=2
vary_list=c(10,20,30,40,50,60)
cols=c("black","red","blue")
pchs=c(15,16,17)
xlab="s2"
names=c("Simultaneous","Pooling","Intersection")
FDRmat=powermat=matrix(data=NA,nrow=length(vary_list),ncol=length(names))
for (i in 1:length(vary_list)){
	s2=vary_list[i]
	filename=paste("Binary_result_n1=",as.character(n1),"n2=",as.character(n2),"p=",as.character(p),"s0=",as.character(s0),"s1=",as.character(s1),"s2=",as.character(s2),"rho1=",as.character(rho1),"rho2=",as.character(rho2),"sigma1=",as.character(sigma1),"sigma2=",as.character(sigma2),"samesig=",as.character(samesig),"q=",as.character(q),"scale=",as.character(scale),"iniseed=",as.character(iniseed),".csv",sep="")
	tmp=c(read.csv(filename)[,2])
	FDRmat[i,]=tmp[c(1,3,5)]
	powermat[i,]=tmp[c(2,4,6)]
}

pdf("Binary_s2.pdf")
FDR_and_power_plot(FDRmat=FDRmat, powermat=powermat, names=names, cols=cols, pchs=pchs, alpha=q, vary_list,xlab=xlab, title='',leg_coords=c(min(vary_list),0.5),show_legend=TRUE,labelsize=1.5,legsize=1.5,cexsize=1.8)
dev.off()


####figure 5 change rho1 s1=0, s2=0
n1=1000
n2=1000
p=200
s0=40
s1=0
s2=0
q=0.2
rho1=rho2=0.5
sigma1=1
sigma2=-1
samesig=1
iniseed=1111


scale=2
vary_list=c(0.25,0.3,0.35,0.4,0.5)
cols=c("black","red","blue")
pchs=c(15,16,17)
xlab=expression(rho[1])
names=c("Simultaneous","Pooling","Intersection")
FDRmat=powermat=matrix(data=NA,nrow=length(vary_list),ncol=length(names))
for (i in 1:length(vary_list)){
	rho1=vary_list[i]
	rho2=1-rho1
	filename=paste("Binary_result_n1=",as.character(n1),"n2=",as.character(n2),"p=",as.character(p),"s0=",as.character(s0),"s1=",as.character(s1),"s2=",as.character(s2),"rho1=",as.character(rho1),"rho2=",as.character(rho2),"sigma1=",as.character(sigma1),"sigma2=",as.character(sigma2),"samesig=",as.character(samesig),"q=",as.character(q),"scale=",as.character(scale),"iniseed=",as.character(iniseed),".csv",sep="")
	tmp=c(read.csv(filename)[,2])
	FDRmat[i,]=tmp[c(1,3,5)]
	powermat[i,]=tmp[c(2,4,6)]
}

pdf("Binary_rho_s1=0s2=0.pdf")
FDR_and_power_plot(FDRmat=FDRmat, powermat=powermat, names=names, cols=cols, pchs=pchs, alpha=q, vary_list,xlab=xlab, title='',leg_coords=c(min(vary_list),0.5),show_legend=TRUE,labelsize=1.5,legsize=1.5,cexsize=1.8)
dev.off()


####figure 6 change rho1 s1=40, s2=40
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


scale=2
vary_list=c(0.25,0.3,0.35,0.4,0.5)
cols=c("black","red","blue")
pchs=c(15,16,17)
xlab=expression(rho[1])
names=c("Simultaneous","Pooling","Intersection")
FDRmat=powermat=matrix(data=NA,nrow=length(vary_list),ncol=length(names))
for (i in 1:length(vary_list)){
	rho1=vary_list[i]
	rho2=1-rho1
	filename=paste("Binary_result_n1=",as.character(n1),"n2=",as.character(n2),"p=",as.character(p),"s0=",as.character(s0),"s1=",as.character(s1),"s2=",as.character(s2),"rho1=",as.character(rho1),"rho2=",as.character(rho2),"sigma1=",as.character(sigma1),"sigma2=",as.character(sigma2),"samesig=",as.character(samesig),"q=",as.character(q),"scale=",as.character(scale),"iniseed=",as.character(iniseed),".csv",sep="")
	tmp=c(read.csv(filename)[,2])
	FDRmat[i,]=tmp[c(1,3,5)]
	powermat[i,]=tmp[c(2,4,6)]
}

pdf("Binary_rho_s1=40s2=40.pdf")
FDR_and_power_plot(FDRmat=FDRmat, powermat=powermat, names=names, cols=cols, pchs=pchs, alpha=q, vary_list,xlab=xlab, title='',leg_coords=c(min(vary_list),0.5),show_legend=TRUE,labelsize=1.5,legsize=1.5,cexsize=1.8)
dev.off()


####figure 7 change rho1 s1=0, s2=40
n1=1000
n2=1000
p=200
s0=40
s1=0
s2=40
q=0.2
rho1=rho2=0.5
sigma1=1
sigma2=-1
samesig=1
iniseed=1111


scale=2
vary_list=c(0.25,0.3,0.35,0.4,0.5)
cols=c("black","red","blue")
pchs=c(15,16,17)
xlab=expression(rho[1])
names=c("Simultaneous","Pooling","Intersection")
FDRmat=powermat=matrix(data=NA,nrow=length(vary_list),ncol=length(names))
for (i in 1:length(vary_list)){
	rho1=vary_list[i]
	rho2=1-rho1
	filename=paste("Binary_result_n1=",as.character(n1),"n2=",as.character(n2),"p=",as.character(p),"s0=",as.character(s0),"s1=",as.character(s1),"s2=",as.character(s2),"rho1=",as.character(rho1),"rho2=",as.character(rho2),"sigma1=",as.character(sigma1),"sigma2=",as.character(sigma2),"samesig=",as.character(samesig),"q=",as.character(q),"scale=",as.character(scale),"iniseed=",as.character(iniseed),".csv",sep="")
	tmp=c(read.csv(filename)[,2])
	FDRmat[i,]=tmp[c(1,3,5)]
	powermat[i,]=tmp[c(2,4,6)]
}

pdf("Binary_rho_s1=0s2=40.pdf")
FDR_and_power_plot(FDRmat=FDRmat, powermat=powermat, names=names, cols=cols, pchs=pchs, alpha=q, vary_list,xlab=xlab, title='',leg_coords=c(min(vary_list),0.5),show_legend=TRUE,labelsize=1.5,legsize=1.5,cexsize=1.8)
dev.off()






####figure 8 change sigma1 s1=0, s2=0
n1=1000
n2=1000
p=200
s0=40
s1=0
s2=0
q=0.2
rho1=rho2=0.5
sigma1=1
sigma2=-1
samesig=1
iniseed=1111


scale=2
vary_list=c(0,0.5,1,1.5,2,2.5)
cols=c("black","red","blue")
pchs=c(15,16,17)
xlab=expression(alpha[1])
names=c("Simultaneous","Pooling","Intersection")
FDRmat=powermat=matrix(data=NA,nrow=length(vary_list),ncol=length(names))
for (i in 1:length(vary_list)){
	sigma1=vary_list[i]
	sigma2=-sigma1
	filename=paste("Binary_result_n1=",as.character(n1),"n2=",as.character(n2),"p=",as.character(p),"s0=",as.character(s0),"s1=",as.character(s1),"s2=",as.character(s2),"rho1=",as.character(rho1),"rho2=",as.character(rho2),"sigma1=",as.character(sigma1),"sigma2=",as.character(sigma2),"samesig=",as.character(samesig),"q=",as.character(q),"scale=",as.character(scale),"iniseed=",as.character(iniseed),".csv",sep="")
	tmp=c(read.csv(filename)[,2])
	FDRmat[i,]=tmp[c(1,3,5)]
	powermat[i,]=tmp[c(2,4,6)]
}

pdf("Binary_sigma_s1=0s2=0.pdf")
FDR_and_power_plot(FDRmat=FDRmat, powermat=powermat, names=names, cols=cols, pchs=pchs, alpha=q, vary_list,xlab=xlab, title='',leg_coords=c(min(vary_list),0.5),show_legend=TRUE,labelsize=1.5,legsize=1.5,cexsize=1.8)
dev.off()



####figure 9 change sigma1 s1=40, s2=40
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


scale=2
vary_list=c(0,0.5,1,1.5,2,2.5)
cols=c("black","red","blue")
pchs=c(15,16,17)
xlab=expression(alpha[1])
names=c("Simultaneous","Pooling","Intersection")
FDRmat=powermat=matrix(data=NA,nrow=length(vary_list),ncol=length(names))
for (i in 1:length(vary_list)){
	sigma1=vary_list[i]
	sigma2=-sigma1
	filename=paste("Binary_result_n1=",as.character(n1),"n2=",as.character(n2),"p=",as.character(p),"s0=",as.character(s0),"s1=",as.character(s1),"s2=",as.character(s2),"rho1=",as.character(rho1),"rho2=",as.character(rho2),"sigma1=",as.character(sigma1),"sigma2=",as.character(sigma2),"samesig=",as.character(samesig),"q=",as.character(q),"scale=",as.character(scale),"iniseed=",as.character(iniseed),".csv",sep="")
	tmp=c(read.csv(filename)[,2])
	FDRmat[i,]=tmp[c(1,3,5)]
	powermat[i,]=tmp[c(2,4,6)]
}

pdf("Binary_sigma_s1=40s2=40.pdf")
FDR_and_power_plot(FDRmat=FDRmat, powermat=powermat, names=names, cols=cols, pchs=pchs, alpha=q, vary_list,xlab=xlab, title='',leg_coords=c(min(vary_list),0.5),show_legend=TRUE,labelsize=1.5,legsize=1.5,cexsize=1.8)
dev.off()


####figure 10 change sigma1 s1=0, s2=40
n1=1000
n2=1000
p=200
s0=40
s1=0
s2=40
q=0.2
rho1=rho2=0.5
sigma1=1
sigma2=-1
samesig=1
iniseed=1111


scale=2
vary_list=c(0,0.5,1,1.5,2,2.5)
cols=c("black","red","blue")
pchs=c(15,16,17)
xlab=expression(alpha[1])
names=c("Simultaneous","Pooling","Intersection")
FDRmat=powermat=matrix(data=NA,nrow=length(vary_list),ncol=length(names))
for (i in 1:length(vary_list)){
	sigma1=vary_list[i]
	sigma2=-sigma1
	filename=paste("Binary_result_n1=",as.character(n1),"n2=",as.character(n2),"p=",as.character(p),"s0=",as.character(s0),"s1=",as.character(s1),"s2=",as.character(s2),"rho1=",as.character(rho1),"rho2=",as.character(rho2),"sigma1=",as.character(sigma1),"sigma2=",as.character(sigma2),"samesig=",as.character(samesig),"q=",as.character(q),"scale=",as.character(scale),"iniseed=",as.character(iniseed),".csv",sep="")
	tmp=c(read.csv(filename)[,2])
	FDRmat[i,]=tmp[c(1,3,5)]
	powermat[i,]=tmp[c(2,4,6)]
}

pdf("Binary_sigma_s1=0s2=40.pdf")
FDR_and_power_plot(FDRmat=FDRmat, powermat=powermat, names=names, cols=cols, pchs=pchs, alpha=q, vary_list,xlab=xlab, title='',leg_coords=c(min(vary_list),0.5),show_legend=TRUE,labelsize=1.5,legsize=1.5,cexsize=1.8)
dev.off()



####samesig=0


####figure 2 change s0
n1=1000
n2=1000
p=200
s0=40
s1=0
s2=0
q=0.2
rho1=rho2=0.5
sigma1=1
sigma2=-1
samesig=0
iniseed=1111

scale=2
vary_list=c(10,20,30,40,50,60)
cols=c("black","red","blue")
pchs=c(15,16,17)
xlab="s0"
names=c("Simultaneous","Pooling","Intersection")
FDRmat=powermat=matrix(data=NA,nrow=length(vary_list),ncol=length(names))
for (i in 1:length(vary_list)){
	s0=vary_list[i]
	filename=paste("Binary_result_n1=",as.character(n1),"n2=",as.character(n2),"p=",as.character(p),"s0=",as.character(s0),"s1=",as.character(s1),"s2=",as.character(s2),"rho1=",as.character(rho1),"rho2=",as.character(rho2),"sigma1=",as.character(sigma1),"sigma2=",as.character(sigma2),"samesig=",as.character(samesig),"q=",as.character(q),"scale=",as.character(scale),"iniseed=",as.character(iniseed),".csv",sep="")
	tmp=c(read.csv(filename)[,2])
	FDRmat[i,]=tmp[c(1,3,5)]
	powermat[i,]=tmp[c(2,4,6)]
}

pdf("Binary_s0samesig=0.pdf")
FDR_and_power_plot(FDRmat=FDRmat, powermat=powermat, names=names, cols=cols, pchs=pchs, alpha=q, vary_list,xlab=xlab, title='',leg_coords=c(min(vary_list),0.5),show_legend=TRUE,labelsize=1.5,legsize=1.5,cexsize=1.8)
dev.off()

####figure 3 change s1=s2
n1=1000
n2=1000
p=200
s0=40
s1=0
s2=0
q=0.2
rho1=rho2=0.5
sigma1=1
sigma2=-1
samesig=0
iniseed=1111

scale=2
vary_list=c(10,20,30,40,50,60)
cols=c("black","red","blue")
pchs=c(15,16,17)
xlab="s1=s2"
names=c("Simultaneous","Pooling","Intersection")
FDRmat=powermat=matrix(data=NA,nrow=length(vary_list),ncol=length(names))
for (i in 1:length(vary_list)){
	s2=s1=vary_list[i]
	filename=paste("Binary_result_n1=",as.character(n1),"n2=",as.character(n2),"p=",as.character(p),"s0=",as.character(s0),"s1=",as.character(s1),"s2=",as.character(s2),"rho1=",as.character(rho1),"rho2=",as.character(rho2),"sigma1=",as.character(sigma1),"sigma2=",as.character(sigma2),"samesig=",as.character(samesig),"q=",as.character(q),"scale=",as.character(scale),"iniseed=",as.character(iniseed),".csv",sep="")
	tmp=c(read.csv(filename)[,2])
	FDRmat[i,]=tmp[c(1,3,5)]
	powermat[i,]=tmp[c(2,4,6)]
}

pdf("Binary_s1_s2samesig=0.pdf")
FDR_and_power_plot(FDRmat=FDRmat, powermat=powermat, names=names, cols=cols, pchs=pchs, alpha=q, vary_list,xlab=xlab, title='',leg_coords=c(min(vary_list),0.5),show_legend=TRUE,labelsize=1.5,legsize=1.5,cexsize=1.8)
dev.off()


####figure 4 change s2
n1=1000
n2=1000
p=200
s0=40
s1=0
s2=0
q=0.2
rho1=rho2=0.5
sigma1=1
sigma2=-1
samesig=0
iniseed=1111


scale=2
vary_list=c(10,20,30,40,50,60)
cols=c("black","red","blue")
pchs=c(15,16,17)
xlab="s2"
names=c("Simultaneous","Pooling","Intersection")
FDRmat=powermat=matrix(data=NA,nrow=length(vary_list),ncol=length(names))
for (i in 1:length(vary_list)){
	s2=vary_list[i]
	filename=paste("Binary_result_n1=",as.character(n1),"n2=",as.character(n2),"p=",as.character(p),"s0=",as.character(s0),"s1=",as.character(s1),"s2=",as.character(s2),"rho1=",as.character(rho1),"rho2=",as.character(rho2),"sigma1=",as.character(sigma1),"sigma2=",as.character(sigma2),"samesig=",as.character(samesig),"q=",as.character(q),"scale=",as.character(scale),"iniseed=",as.character(iniseed),".csv",sep="")
	tmp=c(read.csv(filename)[,2])
	FDRmat[i,]=tmp[c(1,3,5)]
	powermat[i,]=tmp[c(2,4,6)]
}

pdf("Binary_s2samesig=0.pdf")
FDR_and_power_plot(FDRmat=FDRmat, powermat=powermat, names=names, cols=cols, pchs=pchs, alpha=q, vary_list,xlab=xlab, title='',leg_coords=c(min(vary_list),0.5),show_legend=TRUE,labelsize=1.5,legsize=1.5,cexsize=1.8)
dev.off()


####figure 5 change rho1 s1=0, s2=0
n1=1000
n2=1000
p=200
s0=40
s1=0
s2=0
q=0.2
rho1=rho2=0.5
sigma1=1
sigma2=-1
samesig=0
iniseed=1111


scale=2
vary_list=c(0.25,0.3,0.35,0.4,0.5)
cols=c("black","red","blue")
pchs=c(15,16,17)
xlab=expression(rho[1])
names=c("Simultaneous","Pooling","Intersection")
FDRmat=powermat=matrix(data=NA,nrow=length(vary_list),ncol=length(names))
for (i in 1:length(vary_list)){
	rho1=vary_list[i]
	rho2=1-rho1
	filename=paste("Binary_result_n1=",as.character(n1),"n2=",as.character(n2),"p=",as.character(p),"s0=",as.character(s0),"s1=",as.character(s1),"s2=",as.character(s2),"rho1=",as.character(rho1),"rho2=",as.character(rho2),"sigma1=",as.character(sigma1),"sigma2=",as.character(sigma2),"samesig=",as.character(samesig),"q=",as.character(q),"scale=",as.character(scale),"iniseed=",as.character(iniseed),".csv",sep="")
	tmp=c(read.csv(filename)[,2])
	FDRmat[i,]=tmp[c(1,3,5)]
	powermat[i,]=tmp[c(2,4,6)]
}

pdf("Binary_rho_s1=0s2=0samesig=0.pdf")
FDR_and_power_plot(FDRmat=FDRmat, powermat=powermat, names=names, cols=cols, pchs=pchs, alpha=q, vary_list,xlab=xlab, title='',leg_coords=c(min(vary_list),0.5),show_legend=TRUE,labelsize=1.5,legsize=1.5,cexsize=1.8)
dev.off()


####figure 6 change rho1 s1=40, s2=40
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
samesig=0
iniseed=1111


scale=2
vary_list=c(0.25,0.3,0.35,0.4,0.5)
cols=c("black","red","blue")
pchs=c(15,16,17)
xlab=expression(rho[1])
names=c("Simultaneous","Pooling","Intersection")
FDRmat=powermat=matrix(data=NA,nrow=length(vary_list),ncol=length(names))
for (i in 1:length(vary_list)){
	rho1=vary_list[i]
	rho2=1-rho1
	filename=paste("Binary_result_n1=",as.character(n1),"n2=",as.character(n2),"p=",as.character(p),"s0=",as.character(s0),"s1=",as.character(s1),"s2=",as.character(s2),"rho1=",as.character(rho1),"rho2=",as.character(rho2),"sigma1=",as.character(sigma1),"sigma2=",as.character(sigma2),"samesig=",as.character(samesig),"q=",as.character(q),"scale=",as.character(scale),"iniseed=",as.character(iniseed),".csv",sep="")
	tmp=c(read.csv(filename)[,2])
	FDRmat[i,]=tmp[c(1,3,5)]
	powermat[i,]=tmp[c(2,4,6)]
}

pdf("Binary_rho_s1=40s2=40samesig=0.pdf")
FDR_and_power_plot(FDRmat=FDRmat, powermat=powermat, names=names, cols=cols, pchs=pchs, alpha=q, vary_list,xlab=xlab, title='',leg_coords=c(min(vary_list),0.5),show_legend=TRUE,labelsize=1.5,legsize=1.5,cexsize=1.8)
dev.off()


####figure 7 change rho1 s1=0, s2=40
n1=1000
n2=1000
p=200
s0=40
s1=0
s2=40
q=0.2
rho1=rho2=0.5
sigma1=1
sigma2=-1
samesig=0
iniseed=1111


scale=2
vary_list=c(0.25,0.3,0.35,0.4,0.5)
cols=c("black","red","blue")
pchs=c(15,16,17)
xlab=expression(rho[1])
names=c("Simultaneous","Pooling","Intersection")
FDRmat=powermat=matrix(data=NA,nrow=length(vary_list),ncol=length(names))
for (i in 1:length(vary_list)){
	rho1=vary_list[i]
	rho2=1-rho1
	filename=paste("Binary_result_n1=",as.character(n1),"n2=",as.character(n2),"p=",as.character(p),"s0=",as.character(s0),"s1=",as.character(s1),"s2=",as.character(s2),"rho1=",as.character(rho1),"rho2=",as.character(rho2),"sigma1=",as.character(sigma1),"sigma2=",as.character(sigma2),"samesig=",as.character(samesig),"q=",as.character(q),"scale=",as.character(scale),"iniseed=",as.character(iniseed),".csv",sep="")
	tmp=c(read.csv(filename)[,2])
	FDRmat[i,]=tmp[c(1,3,5)]
	powermat[i,]=tmp[c(2,4,6)]
}

pdf("Binary_rho_s1=0s2=40samesig=0.pdf")
FDR_and_power_plot(FDRmat=FDRmat, powermat=powermat, names=names, cols=cols, pchs=pchs, alpha=q, vary_list,xlab=xlab, title='',leg_coords=c(min(vary_list),0.5),show_legend=TRUE,labelsize=1.5,legsize=1.5,cexsize=1.8)
dev.off()






####figure 8 change sigma1 s1=0, s2=0
n1=1000
n2=1000
p=200
s0=40
s1=0
s2=0
q=0.2
rho1=rho2=0.5
sigma1=1
sigma2=-1
samesig=0
iniseed=1111


scale=2
vary_list=c(0,0.5,1,1.5,2,2.5)
cols=c("black","red","blue")
pchs=c(15,16,17)
xlab=expression(alpha[1])
names=c("Simultaneous","Pooling","Intersection")
FDRmat=powermat=matrix(data=NA,nrow=length(vary_list),ncol=length(names))
for (i in 1:length(vary_list)){
	sigma1=vary_list[i]
	sigma2=-sigma1
	filename=paste("Binary_result_n1=",as.character(n1),"n2=",as.character(n2),"p=",as.character(p),"s0=",as.character(s0),"s1=",as.character(s1),"s2=",as.character(s2),"rho1=",as.character(rho1),"rho2=",as.character(rho2),"sigma1=",as.character(sigma1),"sigma2=",as.character(sigma2),"samesig=",as.character(samesig),"q=",as.character(q),"scale=",as.character(scale),"iniseed=",as.character(iniseed),".csv",sep="")
	tmp=c(read.csv(filename)[,2])
	FDRmat[i,]=tmp[c(1,3,5)]
	powermat[i,]=tmp[c(2,4,6)]
}

pdf("Binary_sigma_s1=0s2=0samesig=0.pdf")
FDR_and_power_plot(FDRmat=FDRmat, powermat=powermat, names=names, cols=cols, pchs=pchs, alpha=q, vary_list,xlab=xlab, title='',leg_coords=c(min(vary_list),0.5),show_legend=TRUE,labelsize=1.5,legsize=1.5,cexsize=1.8)
dev.off()



####figure 9 change sigma1 s1=40, s2=40
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
samesig=0
iniseed=1111


scale=2
vary_list=c(0,0.5,1,1.5,2,2.5)
cols=c("black","red","blue")
pchs=c(15,16,17)
xlab=expression(alpha[1])
names=c("Simultaneous","Pooling","Intersection")
FDRmat=powermat=matrix(data=NA,nrow=length(vary_list),ncol=length(names))
for (i in 1:length(vary_list)){
	sigma1=vary_list[i]
	sigma2=-sigma1
	filename=paste("Binary_result_n1=",as.character(n1),"n2=",as.character(n2),"p=",as.character(p),"s0=",as.character(s0),"s1=",as.character(s1),"s2=",as.character(s2),"rho1=",as.character(rho1),"rho2=",as.character(rho2),"sigma1=",as.character(sigma1),"sigma2=",as.character(sigma2),"samesig=",as.character(samesig),"q=",as.character(q),"scale=",as.character(scale),"iniseed=",as.character(iniseed),".csv",sep="")
	tmp=c(read.csv(filename)[,2])
	FDRmat[i,]=tmp[c(1,3,5)]
	powermat[i,]=tmp[c(2,4,6)]
}

pdf("Binary_sigma_s1=40s2=40samesig=0.pdf")
FDR_and_power_plot(FDRmat=FDRmat, powermat=powermat, names=names, cols=cols, pchs=pchs, alpha=q, vary_list,xlab=xlab, title='',leg_coords=c(min(vary_list),0.5),show_legend=TRUE,labelsize=1.5,legsize=1.5,cexsize=1.8)
dev.off()


####figure 10 change sigma1 s1=0, s2=40
n1=1000
n2=1000
p=200
s0=40
s1=0
s2=40
q=0.2
rho1=rho2=0.5
sigma1=1
sigma2=-1
samesig=0
iniseed=1111


scale=2
vary_list=c(0,0.5,1,1.5,2,2.5)
cols=c("black","red","blue")
pchs=c(15,16,17)
xlab=expression(alpha[1])
names=c("Simultaneous","Pooling","Intersection")
FDRmat=powermat=matrix(data=NA,nrow=length(vary_list),ncol=length(names))
for (i in 1:length(vary_list)){
	sigma1=vary_list[i]
	sigma2=-sigma1
	filename=paste("Binary_result_n1=",as.character(n1),"n2=",as.character(n2),"p=",as.character(p),"s0=",as.character(s0),"s1=",as.character(s1),"s2=",as.character(s2),"rho1=",as.character(rho1),"rho2=",as.character(rho2),"sigma1=",as.character(sigma1),"sigma2=",as.character(sigma2),"samesig=",as.character(samesig),"q=",as.character(q),"scale=",as.character(scale),"iniseed=",as.character(iniseed),".csv",sep="")
	tmp=c(read.csv(filename)[,2])
	FDRmat[i,]=tmp[c(1,3,5)]
	powermat[i,]=tmp[c(2,4,6)]
}

pdf("Binary_sigma_s1=0s2=40samesig=0.pdf")
FDR_and_power_plot(FDRmat=FDRmat, powermat=powermat, names=names, cols=cols, pchs=pchs, alpha=q, vary_list,xlab=xlab, title='',leg_coords=c(min(vary_list),0.5),show_legend=TRUE,labelsize=1.5,legsize=1.5,cexsize=1.8)
dev.off()












