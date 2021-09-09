FDR_and_power_plot <<- function(FDRmat, powermat, names, cols, pchs, alpha, vary_list,xlab='', title='',leg_coords=integer(0),show_legend=FALSE,labelsize=1,legsize=1,cexsize=1.2){
if(length(leg_coords)==0){leg_x=min(vary_list);leg_y=1}else{leg_x=leg_coords[1];leg_y=leg_coords[2]}
xzero=min(vary_list)-.05*(max(vary_list)-min(vary_list))
xzero1=min(vary_list)-.17*(max(vary_list)-min(vary_list))
xzero2=min(vary_list)-.07*(max(vary_list)-min(vary_list))
xzero3=min(vary_list)-.1*(max(vary_list)-min(vary_list))
plot(0:1,0:1,type='n',xlab=xlab,ylab='',xlim=c(xzero1,max(vary_list)),ylim=c(-.75,1.05),axes=FALSE,main=title,cex.main=2,cex.lab=1*labelsize)
segments(xzero2,0,max(vary_list),0)
axis(side=1,at=vary_list,cex.axis=0.8*cexsize)
segments(xzero,-.75,xzero,-.05)
segments(xzero,.05,xzero,1.05)
for(i in (0:3)*2){
segments(xzero,-.75+i/10,xzero2,-.75+i/10)
text(xzero3,-.75+i/10,i/10,cex=0.8*cexsize)
}
for(i in (0:5)*2){
segments(xzero,.05+i/10,xzero2,0.05+i/10)
text(xzero3,.05+i/10,i/10,cex=0.8*cexsize)
}
text(xzero1,-.3,expression(FDR),srt=90,cex=1*labelsize)
text(xzero1,.75,'Power',srt=90,cex=1*labelsize)
for(i in 1:length(names)){
points(vary_list,FDRmat[,i]-.75,type='l',col=cols[i])
points(vary_list,powermat[,i]+.05,type='l',col=cols[i])
points(vary_list,FDRmat[,i]-.75,pch=pchs[i],col=cols[i],cex=1)
points(vary_list,powermat[,i]+.05,pch=pchs[i],col=cols[i],cex=1)
}
points(vary_list,(alpha-.75)*rep(1,length(vary_list)),type='l',lty='dotted',col='gray50',lwd=2)
if(show_legend){
legend(leg_x,leg_y,legend=names,col=cols,pch=pchs,lty='solid',cex=0.8*legsize)
}
}
