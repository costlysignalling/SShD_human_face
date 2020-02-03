# w<-20
# tiff("urbanization1.tif",width=w,height=2*(w/7),units="cm",res=600,compression="lzw")

layout(matrix(rep(1:2,c(1,1)),nrow=1))


dat.m<-data.frame(disp.m=mor.var$disp.m,disp.f=mor.var$disp.f)

par(mar=c(3,3,2,2),mgp=c(1.8,0.6,0),font.lab=2)
plot( dat.m$disp.m , dat.m$disp.f , pch=1 , col="black" ,
      xlab="Morphological disparity men" , ylab="Morphological disparity women",axes=T,xlim=c(0.0025,0.0052),ylim=c(0.0025,0.0052),type="n",bty="n")

lines(c(-100,100),c(-100,100),lwd=1.5,lty=2)
sq<-(par("usr")[2]-par("usr")[1])/5
cent<-0.005
polygon(c(cent-(sqrt(2)/2)*sq,cent,cent+(sqrt(2)/2)*sq,cent),c(cent,cent-(sqrt(2)/2)*sq,cent,cent+(sqrt(2)/2)*sq),border=NA,col="white")
text(cent,cent,"x = y",srt=45)

points(dat.m$disp.m,dat.m$disp.f,col=cols[reord],pch=16)
text(dat.m$disp.m,dat.m$disp.f,rownames(mor.var),col=cols[reord],cex=1,pos=c(4,3,4,3,3,4,2,4))
title("A",adj=0)

dat.m<-data.frame(disp.pop=mor.var$disp.pop,dif=dif.var$dif)

dat.m.s<-scale(dat.m)

means<-attr(dat.m.s,"scaled:center")
sds<-attr(dat.m.s,"scaled:scale")

dat.m.s<-as.data.frame(dat.m.s)

dat.m.s[,1]*sds[1]+means[1]

m <- quap(
  alist(
    dif ~ dnorm( mu , sigma ) ,
    mu <- a + b*disp.pop,
    a ~ dnorm(0,1) ,
    b ~ dnorm(0,1),
    sigma ~ dexp( 1 )
  ),data=dat.m.s)

precis(m)
mu.exp <- link( m, data=data.frame(disp.pop=seq(-3,3,0.1) ) )

difseq<-seq(-2.8,2.8,0.01) 

par(mar=c(3,3,2,2),mgp=c(1.8,0.6,0),font.lab=2)
plot( dat.m.s$disp.pop , dat.m.s$dif , pch=1 , col="black" ,
      xlab="Morphological disparity" , ylab="SShD distance between sexes",axes=F,xlim=c(-2,2.5),ylim=c(-2,2.5),type="n")

axis(1,at=seq(-2,2,1),labels=c("-2SD","-1SD","MEAN","+1SD","+2SD"))
axis(2,at=seq(-2,2,1),labels=c("-2SD","-1SD","MEAN","+1SD","+2SD"))

mu <- link( m, data=data.frame(disp.pop=difseq) )
mu_mean <- apply( mu , 2 , mean )
mu_ci <- apply( mu , 2 , PI , prob=0.89 )
lines( difseq , mu_mean , lwd=1.2 )
shade( mu_ci , difseq )

points(dat.m.s$disp.pop,dat.m.s$dif,col=cols[reord],pch=16)
text(dat.m.s$disp.pop,dat.m.s$dif,rownames(mor.var),col=cols[reord],cex=1,pos=c(1,1,3,1,3,1,1,3))
title("B",adj=0)

# dev.off()
