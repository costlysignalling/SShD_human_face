# w<-10
# tiff("differences1.tif",width=w,height=w,units="cm",res=600,compression="lzw")

dat.m<-data.frame(difH=difH,mean.dist=mean.dist)

dat.m.s<-scale(dat.m)

means<-attr(dat.m.s,"scaled:center")
sds<-attr(dat.m.s,"scaled:scale")

dat.m.s<-as.data.frame(dat.m.s)

dat.m.s[,1]*sds[1]+means[1]

m <- quap(
  alist(
    mean.dist ~ dnorm( mu , sigma ) ,
    mu <- a + b*difH,
    a ~ dnorm(0,1) ,
    b ~ dnorm(0,1),
    sigma ~ dexp( 1 )
  ),data=dat.m.s)

precis(m)
mu.exp <- link( m, data=data.frame(difH=seq(-3,3,0.1) ) )

difseq<-seq(-2.8,2.8,0.01) 

par(mar=c(3,3,2,2),mgp=c(1.8,0.6,0),font.lab=2)
plot( dat.m.s$difH , dat.m.s$mean.dist , pch=1 , col="black" ,
      xlab="Difference in mean body height" , ylab="Distance between sex means",axes=F,xlim=c(-2,2.5),ylim=c(-2,2.5),type="n")

axis(1,at=seq(-2,2,1),labels=round(seq(-2,2,1)*sds[1]+means[1],1))
axis(2,at=seq(-2,2,1),labels=round(seq(-2,2,1)*sds[2]+means[2],3))

mu <- link( m, data=data.frame(difH=difseq) )
mu_mean <- apply( mu , 2 , mean )
mu_ci <- apply( mu , 2 , PI , prob=0.89 )
lines( difseq , mu_mean , lwd=1.2 )
shade( mu_ci , difseq )

points(dat.m.s$difH,dat.m.s$mean.dist,col=cols,pch=16)
text(dat.m.s$difH,dat.m.s$mean.dist,labs[-1],col=cols,cex=1,pos=c(1,4,4,3,1,4,3,3))

#dev.off()
