
#tiff("morphs_all.tiff",width=20,height=15,units="cm",res=600,compression="lzw")

par (mar = c(0,0,0,0), oma = c(0,2.5,2.5,0))
(gp1<-layout(matrix(c(1:15), 3, 5, byrow = T), widths=c(1,1,1), heights=c(1,1,1), respect = F)) 

cex1<-1.1
cex2<-1.1

plotRefToTarget(MEAN, WOMEN, method = "TPS", mag = 3, links = links, gridPars = GP1)
mtext("Total SShD", 2, cex = cex2)
mtext("3x women", cex = cex1)
plotRefToTarget(MEAN, WOMEN, method = "TPS", mag = 1, links = links, gridPars = GP1)
mtext("women mean", cex = cex1)
plotRefToTarget(MEAN, MEAN, method = "TPS", mag = 1, links = links, gridPars = GP1)
mtext ("mean", cex = cex1)
plotRefToTarget(MEAN, MEN, method = "TPS", mag = 1, links = links, gridPars = GP1)
mtext ("men mean", cex = cex1)
plotRefToTarget(MEAN, MEN, method = "TPS", mag = 3, links = links, gridPars = GP1)
mtext ("3x men", cex = cex1)


plotRefToTarget(aMEAN, aWOMEN, method = "TPS", mag = 3, links = links, gridPars = GP1)
mtext("Allometric SShD", 2, cex = cex2)
#title ("3x women", cex = 2)
plotRefToTarget(aMEAN, aWOMEN, method = "TPS", mag = 1, links = links, gridPars = GP1)
#title("women mean", cex = 2)
plotRefToTarget(aMEAN, aMEAN, method = "TPS", mag = 1, links = links, gridPars = GP1)
#title ("mean", cex = 2)
plotRefToTarget(aMEAN, aMEN, method = "TPS", mag = 1, links = links, gridPars = GP1)
#title ("men mean", cex = 2)
plotRefToTarget(aMEAN, aMEN, method = "TPS", mag = 3, links = links, gridPars = GP1)
#title ("3x men", cex = 2)

plotRefToTarget(nMEAN, nWOMEN, method = "TPS", mag = 3, links = links, gridPars = GP1)
mtext("Non-allometric SShD", 2, cex = cex2)
#title ("3x women", cex = 2)
plotRefToTarget(nMEAN, nWOMEN, method = "TPS", mag = 1, links = links, gridPars = GP1)
#title("women mean", cex = 2)
plotRefToTarget(nMEAN, nMEAN, method = "TPS", mag = 1, links = links, gridPars = GP1)
#title ("mean", cex = 2)
plotRefToTarget(nMEAN, nMEN, method = "TPS", mag = 1, links = links, gridPars = GP1)
#title ("men mean", cex = 2)
plotRefToTarget(nMEAN, nMEN, method = "TPS", mag = 3, links = links, gridPars = GP1)
#title ("3x men", cex = 2)

#dev.off()
