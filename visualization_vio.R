
#w<-11
#tiff("vio1.tiff",width=w,height=w*1.3,units="cm",res=600,compression="lzw")

bgcol<-c("#DDEEFF","#FFDDEE","#DDEEDD")
colgrid<-"#FFFFFF88"

rat<-c(3,16,16,16,21,3)

layout(matrix(c(rep(1,rat[1]),rep(2,rat[2]),rep(3,rat[3]),rep(4,rat[4]),rep(5,rat[5]),rep(6,rat[6]))))
par(mar=c(0.2,3,0.2,1),mgp=c(1.8,0.6,0),font.axis=1,font.lab=2)

plot(NULL,axes=F,bty="n",xlim=c(1,9),ylim=c(0,10),xlab="",ylab="")
rect(-100,-100,100,100,col=1)
text(c(2,5,8),5,c("Africa","Europe","South America"),col="white",cex=1,font=2)
text(c(2,5,8),5,c("Africa","Europe","South America"),col=bgcol,cex=1,font=2)
abline(v=c(3,7),lwd=2,col="white")

#Total
fnat.plot<-factor(gtf$nat[gtf$sex=="F"],levels(gtf$nat[gtf$sex=="F"])[reord])
mnat.plot<-factor(gtf$nat[gtf$sex=="M"],levels(gtf$nat[gtf$sex=="M"])[reord])
nat.plot<-factor(gtf$nat,levels(gtf$nat)[reord])

vioplot(fsexscores1~fnat.plot, range = 1.5, horizontal = F, plotCentre = "point",ylim=c(-0.004,0.004),xlab="Country",ylab="Total SShD",yaxt="n")
axis(2,at=c(-0.002,0,0.002))
rect(-10,-10,2.5,10,col=bgcol[1],border=NA)
rect(2.5,-10,6.5,10,col=bgcol[2],border=NA)
rect(6.5,-10,10,10,col=bgcol[3],border=NA)
abline(v=1:8,lwd=1,col=colgrid)
vioplot(fsexscores1~fnat.plot, range = 1.5, horizontal = F, col = "#FF770080", plotCentre = "point",add=T,xlab="",ylab="")
vioplot(msexscores1~mnat.plot, range = 1.5, horizontal = F, col = "#3366FF80", plotCentre = "point",add=T,xlab="",ylab="")
vioplot(sexscores1~nat.plot, range = 1.5, horizontal = F, col = "#FFFFFF00", lineCol = NA,add=T,xlab="",ylab="",border="#FF0000",lwd=1.5,rectCol=NA,pchMed=NA)

fnat.plot2<-factor(gtf2$nat[gtf2$sex=="F"],levels(gtf2$nat[gtf2$sex=="F"])[reord])
mnat.plot2<-factor(gtf2$nat[gtf2$sex=="M"],levels(gtf2$nat[gtf2$sex=="M"])[reord])
nat.plot2<-factor(gtf2$nat,levels(gtf2$nat)[reord])

#Allometric
vioplot(fsexscoresA~fnat.plot2, range = 1.5, horizontal = F, col = "#FF770080", plotCentre = "point", ylim=c(-0.004, 0.004), xlab="Country",ylab="Allometric SShD",yaxt="n")
axis(2,at=c(-0.002,0,0.002))
rect(-10,-10,2.5,10,col=bgcol[1],border=NA)
rect(2.5,-10,6.5,10,col=bgcol[2],border=NA)
rect(6.5,-10,10,10,col=bgcol[3],border=NA)
abline(v=1:8,lwd=1,col=colgrid)
vioplot(fsexscoresA~fnat.plot2, range = 1.5, horizontal = F, col = "#FF770080", plotCentre = "point",add=T, xlab="",ylab="")
vioplot(msexscoresA~mnat.plot2, range = 1.5, horizontal = F, col = "#3366FF80", plotCentre = "point",add=T,xlab="",ylab="")
vioplot(sexscoresA~nat.plot2, range = 1.5, horizontal = F, col = "#FFFFFF00", lineCol = NA,add=T,xlab="",ylab="",border="#FF0000",lwd=1.5,rectCol=NA,pchMed=NA)


vioplot(fsexscoresN~fnat.plot2, range = 1.5, horizontal = F, col = "#FF770080", plotCentre = "point", ylim=c(-0.004, 0.004), xlab="Country",ylab="Nonallometric SShD",yaxt="n")
axis(2,at=c(-0.002,0,0.002))
rect(-10,-10,2.5,10,col=bgcol[1],border=NA)
rect(2.5,-10,6.5,10,col=bgcol[2],border=NA)
rect(6.5,-10,10,10,col=bgcol[3],border=NA)
abline(v=1:8,lwd=1,col=colgrid)
vioplot(fsexscoresN~fnat.plot2, range = 1.5, horizontal = F, col = "#FF770080", plotCentre = "point",add=T, xlab="",ylab="")
vioplot(msexscoresN~mnat.plot2, range = 1.5, horizontal = F, col = "#3366FF80", plotCentre = "point",add=T,xlab="",ylab="")
vioplot(sexscoresN~nat.plot2, range = 1.5, horizontal = F, col = "#FFFFFF00", lineCol = NA,add=T,xlab="",ylab="",border="#FF0000",lwd=1.5,rectCol=NA,pchMed=NA)


# HEIGHT PLOTS

par(mar=c(3,3,0.2,1),mgp=c(1.8,0.6,0))

vioplot(gtf2$height[gtf2$sex=="F"]~fnat.plot2, range = 1.5, horizontal = F, col = "#FF770080", plotCentre = "point", ylim = c(142, 202), xlab="Country",ylab="Height")
rect(-10,-100,2.5,1000,col=bgcol[1],border=NA)
rect(2.5,-100,6.5,1000,col=bgcol[2],border=NA)
rect(6.5,-100,10,1000,col=bgcol[3],border=NA)
abline(v=1:8,lwd=1,col=colgrid)
vioplot(gtf2$height[gtf2$sex=="F"]~fnat.plot2, range = 1.5, horizontal = F, col = "#FF770080", plotCentre = "point", add=T, xlab="",ylab="")
vioplot(gtf2$height[gtf2$sex=="M"]~mnat.plot2, range = 1.5, horizontal = F, col = "#3366FF80", plotCentre = "point",add=T,xlab="",ylab="")
vioplot(gtf2$height~nat.plot2, range = 1.5, horizontal = F, col = "#FFFFFF00", lineCol = NA,add=T,xlab="",ylab="",border="darkgreen",lwd=1.5, rectCol=NA,pchMed=NA)

par(mar=c(0,3,0,1),mgp=c(1.5,0.5,0))

plot(NULL,axes=F,bty="n",xlim=c(0,9),ylim=c(0,10),xlab="",ylab="")
legend("center",pch=15,col=c("#3366FF80","#FF770080"),legend=c("Men","Women"),horiz = T,bty="n",cex=1.2)

#dev.off()



