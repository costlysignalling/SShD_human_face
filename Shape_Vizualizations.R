# first run "Dimorphism_X_combine_Allometry.R"

# VISUALIZATION OF OVERAL SShD FOR EACH CULTURE

BRAZ.F <- mshape (onecoords$BRAZ [1:72, 1:2, which(dat$sex[dat$nat=="BRAZ"]=="F")])
BRAZ.M <- mshape (onecoords$BRAZ [1:72, 1:2, which(dat$sex[dat$nat=="BRAZ"]=="M")])
BRAZ.MEAN <- mshape (onecoords$BRAZ)   

CMR.F <- mshape (onecoords$CMR [1:72, 1:2, which(dat$sex[dat$nat=="CMR"]=="F")])
CMR.M <- mshape (onecoords$CMR [1:72, 1:2, which(dat$sex[dat$nat=="CMR"]=="M")])
CMR.MEAN <- mshape (onecoords$CMR)

COL.F <- mshape (onecoords$COL [1:72, 1:2, which(dat$sex[dat$nat=="COL"]=="F")])
COL.M <- mshape (onecoords$COL [1:72, 1:2, which(dat$sex[dat$nat=="COL"]=="M")])
COL.MEAN <- mshape (onecoords$COL)   

CZ.F <- mshape (onecoords$CZ [1:72, 1:2, which(dat$sex[dat$nat=="CZ"]=="F")])
CZ.M <- mshape (onecoords$CZ [1:72, 1:2, which(dat$sex[dat$nat=="CZ"]=="M")])
CZ.MEAN <- mshape (onecoords$CZ)   

NAM.F <- mshape (onecoords$NAM [1:72, 1:2, which(dat$sex[dat$nat=="NAM"]=="F")])
NAM.M <- mshape (onecoords$NAM [1:72, 1:2, which(dat$sex[dat$nat=="NAM"]=="M")])
NAM.MEAN <- mshape (onecoords$NAM)   

RO.F <- mshape (onecoords$RO [1:72, 1:2, which(dat$sex[dat$nat=="RO"]=="F")])
RO.M <- mshape (onecoords$RO [1:72, 1:2, which(dat$sex[dat$nat=="RO"]=="M")])
RO.MEAN <- mshape (onecoords$RO)   

TUR.F <- mshape (onecoords$TR [1:72, 1:2, which(dat$sex[dat$nat=="TR"]=="F")])
TUR.M <- mshape (onecoords$TR [1:72, 1:2, which(dat$sex[dat$nat=="TR"]=="M")])
TUR.MEAN <- mshape (onecoords$TR)

UK.F <- mshape (onecoords$UK [1:72, 1:2, which(dat$sex[dat$nat=="UK"]=="F")])
UK.M <- mshape (onecoords$UK [1:72, 1:2, which(dat$sex[dat$nat=="UK"]=="M")])
UK.MEAN <- mshape (onecoords$UK)   


# compound image

tiff("morphs_national.tiff",width=16,height=28,units="cm",res=600,compression="lzw")

par (mar = c(0,0,0,0), oma = c(0,3,3,0))
gp1<-layout(matrix(c(1:40), 8, 5, byrow = T), widths=c(1,1,1), heights=c(1,1,1), respect = F) 
layout.show(gp1)

cex1<-1.2
cex2<-1.2

font2<-2
font1<-2

# namibia
plotRefToTarget(NAM.MEAN, NAM.F, method = "TPS", mag = 3, links = links, gridPars = GP1)
mtext("Namibia", 2, cex = cex2, font = font2)
mtext("3x women", cex = cex1, font = font1)
plotRefToTarget(NAM.MEAN, NAM.F, method = "TPS", mag = 1, links = links, gridPars = GP1)
mtext("women mean", cex = cex1, font = font1)
plotRefToTarget(NAM.MEAN, NAM.MEAN, method = "TPS", mag = 1, links = links, gridPars = GP1)
mtext("mean", cex = cex1, font = font1)
plotRefToTarget(NAM.MEAN, NAM.M, method = "TPS", mag = 1, links = links, gridPars = GP1)
mtext("men mean", cex = cex1, font = font1)
plotRefToTarget(NAM.MEAN, NAM.M, method = "TPS", mag = 3, links = links, gridPars = GP1)
mtext("3x men", cex = cex1, font = font1)

# cameroon
plotRefToTarget(CMR.MEAN, CMR.F, method = "TPS", mag = 3, links = links, gridPars = GP1)
mtext("Cameroon", 2, cex = cex2, font = font2)
plotRefToTarget(CMR.MEAN, CMR.F, method = "TPS", mag = 1, links = links, gridPars = GP1)
plotRefToTarget(CMR.MEAN, CMR.MEAN, method = "TPS", mag = 1, links = links, gridPars = GP1)
plotRefToTarget(CMR.MEAN, CMR.M, method = "TPS", mag = 1, links = links, gridPars = GP1)
plotRefToTarget(CMR.MEAN, CMR.M, method = "TPS", mag = 3, links = links, gridPars = GP1)

# turkey
plotRefToTarget(TUR.MEAN, TUR.F, method = "TPS", mag = 3, links = links, gridPars = GP1)
mtext("Turkey", 2, cex = cex2, font = font2)
plotRefToTarget(TUR.MEAN, TUR.F, method = "TPS", mag = 1, links = links, gridPars = GP1)
plotRefToTarget(TUR.MEAN, TUR.MEAN, method = "TPS", mag = 1, links = links, gridPars = GP1)
plotRefToTarget(TUR.MEAN, TUR.M, method = "TPS", mag = 1, links = links, gridPars = GP1)
plotRefToTarget(TUR.MEAN, TUR.M, method = "TPS", mag = 3, links = links, gridPars = GP1)

# romania
plotRefToTarget(RO.MEAN, RO.F, method = "TPS", mag = 3, links = links, gridPars = GP1)
mtext("Romania", 2, cex = cex2, font = font2)
plotRefToTarget(RO.MEAN, RO.F, method = "TPS", mag = 1, links = links, gridPars = GP1)
plotRefToTarget(RO.MEAN, RO.MEAN, method = "TPS", mag = 1, links = links, gridPars = GP1)
plotRefToTarget(RO.MEAN, RO.M, method = "TPS", mag = 1, links = links, gridPars = GP1)
plotRefToTarget(RO.MEAN, RO.M, method = "TPS", mag = 3, links = links, gridPars = GP1)

# czechia
plotRefToTarget(CZ.MEAN, CZ.F, method = "TPS", mag = 3, links = links, gridPars = GP1)
mtext("Czechia", 2, cex = cex2, font = font2)
plotRefToTarget(CZ.MEAN, CZ.F, method = "TPS", mag = 1, links = links, gridPars = GP1)
plotRefToTarget(CZ.MEAN, CZ.MEAN, method = "TPS", mag = 1, links = links, gridPars = GP1)
plotRefToTarget(CZ.MEAN, CZ.M, method = "TPS", mag = 1, links = links, gridPars = GP1)
plotRefToTarget(CZ.MEAN, CZ.M, method = "TPS", mag = 3, links = links, gridPars = GP1)

# UK
plotRefToTarget(UK.MEAN, UK.F, method = "TPS", mag = 3, links = links, gridPars = GP1)
mtext("UK", 2, cex = cex2, font = font2)
plotRefToTarget(UK.MEAN, UK.F, method = "TPS", mag = 1, links = links, gridPars = GP1)
plotRefToTarget(UK.MEAN, UK.MEAN, method = "TPS", mag = 1, links = links, gridPars = GP1)
plotRefToTarget(UK.MEAN, UK.M, method = "TPS", mag = 1, links = links, gridPars = GP1)
plotRefToTarget(UK.MEAN, UK.M, method = "TPS", mag = 3, links = links, gridPars = GP1)

# brazil
plotRefToTarget(BRAZ.MEAN, BRAZ.F, method = "TPS", mag = 3, links = links, gridPars = GP1)
mtext("Brazil", 2, cex = cex2, font = font2)
plotRefToTarget(BRAZ.MEAN, BRAZ.F, method = "TPS", mag = 1, links = links, gridPars = GP1)
plotRefToTarget(BRAZ.MEAN, BRAZ.MEAN, method = "TPS", mag = 1, links = links, gridPars = GP1)
plotRefToTarget(BRAZ.MEAN, BRAZ.M, method = "TPS", mag = 1, links = links, gridPars = GP1)
plotRefToTarget(BRAZ.MEAN, BRAZ.M, method = "TPS", mag = 3, links = links, gridPars = GP1)

# colombia
plotRefToTarget(COL.MEAN, COL.F, method = "TPS", mag = 3, links = links, gridPars = GP1)
mtext("Colombia", 2, cex = cex2, font = font2)
plotRefToTarget(COL.MEAN, COL.F, method = "TPS", mag = 1, links = links, gridPars = GP1)
plotRefToTarget(COL.MEAN, COL.MEAN, method = "TPS", mag = 1, links = links, gridPars = GP1)
plotRefToTarget(COL.MEAN, COL.M, method = "TPS", mag = 1, links = links, gridPars = GP1)
plotRefToTarget(COL.MEAN, COL.M, method = "TPS", mag = 3, links = links, gridPars = GP1)


dev.off()

# VISUALIZATION OF ALLOMETRIC SShD DECOMPOSITION

# BRAZ
# allometric sshd
reg1 <- procD.lm(coords ~ height, iter = 999, data = brazgtf, weights = NULL); summary(reg1)

regbraz <- procD.lm(reg1$fitted ~ sex, iter = 999, data = brazgtf, weights = NULL); summary(regbraz)
coefficients<-coef(regbraz, test = F)

braz.a.FEMALE <-mshape(arrayspecs (reg1$fitted, 72, 2) [1:72, 1:2, which(dat$sex[dat$nat=="BRAZ"]=="F")])
braz.a.MALE  <- mshape(arrayspecs (reg1$fitted, 72, 2) [1:72, 1:2, which(dat$sex[dat$nat=="BRAZ"]=="M")])
braz.a.MEAN <- mshape(arrayspecs(reg1$fitted, 72, 2))

plotRefToTarget(braz.a.MEAN, braz.a.MALE, method = "TPS", mag = 4, links = links, gridPars = GP1)
plotRefToTarget(braz.a.MEAN, braz.a.MEAN, method = "TPS", mag = 4, links = links, gridPars = GP1)
plotRefToTarget(braz.a.MEAN, braz.a.FEMALE, method = "TPS", mag = 4, links = links, gridPars = GP1)

# non-allometric sshd

regbraz <- procD.lm(reg1$residuals ~ sex, iter = 999, data = brazgtf, weights = NULL); summary(regbraz)

allocoord<-arrayspecs(reg1$residuals + predict(lm(two.d.array(brazgtf$coords)~1)),72,2)

braz.n.FEMALE <-mshape(allocoord [1:72, 1:2, which(dat$sex[dat$nat=="BRAZ"]=="F")])
braz.n.MALE  <- mshape(allocoord [1:72, 1:2, which(dat$sex[dat$nat=="BRAZ"]=="M")])
braz.n.MEAN <- mshape(allocoord)

plotRefToTarget(braz.n.MEAN, braz.n.MALE, method = "TPS", mag = 4, links = links, gridPars = GP1)
plotRefToTarget(braz.n.MEAN, braz.n.MEAN, method = "TPS", mag = 4, links = links, gridPars = GP1)
plotRefToTarget(braz.n.MEAN, braz.n.FEMALE, method = "TPS", mag = 4, links = links, gridPars = GP1)


# CMR
# allometric sshd
reg1 <- procD.lm(coords ~ height, iter = 999, data = cmrgtf, weights = NULL); summary(reg1)
regcmr <-procD.lm(reg1$fitted ~ sex, iter = 999, data = cmrgtf, weights = NULL); summary(regcmr)

cmr.a.FEMALE <-mshape(arrayspecs (reg1$fitted, 72, 2) [1:72, 1:2, which(dat$sex[dat$nat=="CMR"]=="F")])
cmr.a.MALE  <- mshape(arrayspecs (reg1$fitted, 72, 2) [1:72, 1:2, which(dat$sex[dat$nat=="CMR"]=="M")])
cmr.a.MEAN <- mshape(arrayspecs(reg1$fitted, 72, 2))

plotRefToTarget(cmr.a.MEAN, cmr.a.MALE, method = "TPS", mag = 4, links = links, gridPars = GP1)
plotRefToTarget(cmr.a.MEAN, cmr.a.MEAN, method = "TPS", mag = 4, links = links, gridPars = GP1)
plotRefToTarget(cmr.a.MEAN, cmr.a.FEMALE, method = "TPS", mag = 4, links = links, gridPars = GP1)

# non-allometric sshd
regcmr <- procD.lm(reg1$residuals ~ sex, iter = 999, data = cmrgtf, weights = NULL); summary(regcmr)

allocoord<-arrayspecs(reg1$residuals + predict(lm(two.d.array(cmrgtf$coords)~1)),72,2)

cmr.n.FEMALE <-mshape(allocoord [1:72, 1:2, which(dat$sex[dat$nat=="CMR"]=="F")])
cmr.n.MALE  <- mshape(allocoord [1:72, 1:2, which(dat$sex[dat$nat=="CMR"]=="M")])
cmr.n.MEAN <- mshape(allocoord)

plotRefToTarget(cmr.n.MEAN, cmr.n.MALE, method = "TPS", mag = 4, links = links, gridPars = GP1)
plotRefToTarget(cmr.n.MEAN, cmr.n.MEAN, method = "TPS", mag = 4, links = links, gridPars = GP1)
plotRefToTarget(cmr.n.MEAN, cmr.n.FEMALE, method = "TPS", mag = 4, links = links, gridPars = GP1)


# COL
# allometric sshd
reg1 <- procD.lm(coords ~ height, iter = 999, data = colgtf, weights = NULL); summary(reg1)
regcol <-procD.lm(reg1$fitted ~ sex, iter = 999, data = colgtf, weights = NULL); summary(regcol)

col.a.FEMALE <-mshape(arrayspecs (reg1$fitted, 72, 2) [1:72, 1:2, which(dat$sex[dat$nat=="COL"]=="F")])
col.a.MALE  <- mshape(arrayspecs (reg1$fitted, 72, 2) [1:72, 1:2, which(dat$sex[dat$nat=="COL"]=="M")])
col.a.MEAN <- mshape(arrayspecs(reg1$fitted, 72, 2))

plotRefToTarget(col.a.MEAN, col.a.MALE, method = "TPS", mag = 4, links = links, gridPars = GP1)
plotRefToTarget(col.a.MEAN, col.a.MEAN, method = "TPS", mag = 4, links = links, gridPars = GP1)
plotRefToTarget(col.a.MEAN, col.a.FEMALE, method = "TPS", mag = 4, links = links, gridPars = GP1)

# non-allometric sshd
regcol <- procD.lm(reg1$residuals ~ sex, iter = 999, data = colgtf, weights = NULL); summary(regcol)

allocoord<-arrayspecs(reg1$residuals + predict(lm(two.d.array(colgtf$coords)~1)),72,2)

col.n.FEMALE <-mshape(allocoord [1:72, 1:2, which(dat$sex[dat$nat=="COL"]=="F")])
col.n.MALE  <- mshape(allocoord [1:72, 1:2, which(dat$sex[dat$nat=="COL"]=="M")])
col.n.MEAN <- mshape(allocoord)

plotRefToTarget(col.n.MEAN, col.n.MALE, method = "TPS", mag = 4, links = links, gridPars = GP1)
plotRefToTarget(col.n.MEAN, col.n.MEAN, method = "TPS", mag = 4, links = links, gridPars = GP1)
plotRefToTarget(col.n.MEAN, col.n.FEMALE, method = "TPS", mag = 4, links = links, gridPars = GP1)

# CZ
# allometric sshd
reg1 <- procD.lm(coords ~ height, iter = 999, data = czgtf, weights = NULL); summary(reg1)
regcz <-procD.lm(reg1$fitted ~ sex, iter = 999, data = czgtf, weights = NULL); summary(regcz)

cz.a.FEMALE <-mshape(arrayspecs (reg1$fitted, 72, 2) [1:72, 1:2, which(dat$sex[dat$nat=="CZ"]=="F")])
cz.a.MALE  <- mshape(arrayspecs (reg1$fitted, 72, 2) [1:72, 1:2, which(dat$sex[dat$nat=="CZ"]=="M")])
cz.a.MEAN <- mshape(arrayspecs(reg1$fitted, 72, 2))

plotRefToTarget(cz.a.MEAN, cz.a.MALE, method = "TPS", mag = 4, links = links, gridPars = GP1)
plotRefToTarget(cz.a.MEAN, cz.a.MEAN, method = "TPS", mag = 4, links = links, gridPars = GP1)
plotRefToTarget(cz.a.MEAN, cz.a.FEMALE, method = "TPS", mag = 4, links = links, gridPars = GP1)

# non-allometric sshd
regcz <- procD.lm(reg1$residuals ~ sex, iter = 999, data = czgtf, weights = NULL); summary(regcz)

allocoord<-arrayspecs(reg1$residuals + predict(lm(two.d.array(czgtf$coords)~1)),72,2)

cz.n.FEMALE <-mshape(allocoord [1:72, 1:2, which(dat$sex[dat$nat=="CZ"]=="F")])
cz.n.MALE  <- mshape(allocoord [1:72, 1:2, which(dat$sex[dat$nat=="CZ"]=="M")])
cz.n.MEAN <- mshape(allocoord)

plotRefToTarget(cz.n.MEAN, cz.n.MALE, method = "TPS", mag = 4, links = links, gridPars = GP1)
plotRefToTarget(cz.n.MEAN, cz.n.MEAN, method = "TPS", mag = 4, links = links, gridPars = GP1)
plotRefToTarget(cz.n.MEAN, cz.n.FEMALE, method = "TPS", mag = 4, links = links, gridPars = GP1)


# NAM
# allometric sshd
reg1 <- procD.lm(coords ~ height, iter = 999, data = namgtf, weights = NULL); summary(reg1)
regnam <-procD.lm(reg1$fitted ~ sex, iter = 999, data = namgtf, weights = NULL); summary(regnam)

nam.a.FEMALE <-mshape(arrayspecs (reg1$fitted, 72, 2) [1:72, 1:2, which(dat$sex[dat$nat=="NAM"]=="F")])
nam.a.MALE  <- mshape(arrayspecs (reg1$fitted, 72, 2) [1:72, 1:2, which(dat$sex[dat$nat=="NAM"]=="M")])
nam.a.MEAN <- mshape(arrayspecs(reg1$fitted, 72, 2))

plotRefToTarget(nam.a.MEAN, nam.a.MALE, method = "TPS", mag = 4, links = links, gridPars = GP1)
plotRefToTarget(nam.a.MEAN, nam.a.MEAN, method = "TPS", mag = 4, links = links, gridPars = GP1)
plotRefToTarget(nam.a.MEAN, nam.a.FEMALE, method = "TPS", mag = 4, links = links, gridPars = GP1)

# non-allometric sshd
regnam <- procD.lm(reg1$residuals ~ sex, iter = 999, data = namgtf, weights = NULL); summary(regnam)

allocoord<-arrayspecs(reg1$residuals + predict(lm(two.d.array(namgtf$coords)~1)),72,2)

nam.n.FEMALE <-mshape(allocoord [1:72, 1:2, which(dat$sex[dat$nat=="NAM"]=="F")])
nam.n.MALE  <- mshape(allocoord [1:72, 1:2, which(dat$sex[dat$nat=="NAM"]=="M")])
nam.n.MEAN <- mshape(allocoord)

plotRefToTarget(nam.n.MEAN, nam.n.MALE, method = "TPS", mag = 4, links = links, gridPars = GP1)
plotRefToTarget(nam.n.MEAN, nam.n.MEAN, method = "TPS", mag = 4, links = links, gridPars = GP1)
plotRefToTarget(nam.n.MEAN, nam.n.FEMALE, method = "TPS", mag = 4, links = links, gridPars = GP1)


# TUR
# allometric sshd
reg1 <- procD.lm(coords ~ height, iter = 999, data = trgtf, weights = NULL); summary(reg1)
regtur <-procD.lm(reg1$fitted ~ sex, iter = 999, data = trgtf, weights = NULL); summary(regtur)

tur.a.FEMALE <-mshape(arrayspecs (reg1$fitted, 72, 2) [1:72, 1:2, which(dat$sex[dat$nat=="TR"]=="F")])
tur.a.MALE  <- mshape(arrayspecs (reg1$fitted, 72, 2) [1:72, 1:2, which(dat$sex[dat$nat=="TR"]=="M")])
tur.a.MEAN <- mshape(arrayspecs(reg1$fitted, 72, 2))

plotRefToTarget(tur.a.MEAN, tur.a.MALE, method = "TPS", mag = 4, links = links, gridPars = GP1)
plotRefToTarget(tur.a.MEAN, tur.a.MEAN, method = "TPS", mag = 4, links = links, gridPars = GP1)
plotRefToTarget(tur.a.MEAN, tur.a.FEMALE, method = "TPS", mag = 4, links = links, gridPars = GP1)

# non-allometric sshd
regtur <- procD.lm(reg1$residuals ~ sex, iter = 999, data = trgtf, weights = NULL); summary(regtur)

allocoord<-arrayspecs(reg1$residuals + predict(lm(two.d.array(trgtf$coords)~1)),72,2)

tur.n.FEMALE <-mshape(allocoord [1:72, 1:2, which(dat$sex[dat$nat=="TR"]=="F")])
tur.n.MALE  <- mshape(allocoord [1:72, 1:2, which(dat$sex[dat$nat=="TR"]=="M")])
tur.n.MEAN <- mshape(allocoord)

plotRefToTarget(tur.n.MEAN, tur.n.MALE, method = "TPS", mag = 4, links = links, gridPars = GP1)
plotRefToTarget(tur.n.MEAN, tur.n.MEAN, method = "TPS", mag = 4, links = links, gridPars = GP1)
plotRefToTarget(tur.n.MEAN, tur.n.FEMALE, method = "TPS", mag = 4, links = links, gridPars = GP1)


# RO
# allometric sshd
reg1 <- procD.lm(coords ~ height, iter = 999, data = rogtf, weights = NULL); summary(reg1)
regro <-procD.lm(reg1$fitted ~ sex, iter = 999, data = rogtf, weights = NULL); summary(regro)

ro.a.FEMALE <-mshape(arrayspecs (reg1$fitted, 72, 2) [1:72, 1:2, which(dat$sex[dat$nat=="RO"]=="F")])
ro.a.MALE  <- mshape(arrayspecs (reg1$fitted, 72, 2) [1:72, 1:2, which(dat$sex[dat$nat=="RO"]=="M")])
ro.a.MEAN <- mshape(arrayspecs (reg1$fitted, 72, 2))

plotRefToTarget(ro.a.MEAN, ro.a.MALE, method = "TPS", mag = 4, links = links, gridPars = GP1)
plotRefToTarget(ro.a.MEAN, ro.a.MEAN, method = "TPS", mag = 4, links = links, gridPars = GP1)
plotRefToTarget(ro.a.MEAN, ro.a.FEMALE, method = "TPS", mag = 4, links = links, gridPars = GP1)

# non-allometric sshd
regro <- procD.lm(reg1$residuals ~ sex, iter = 999, data = rogtf, weights = NULL); summary(regro)

allocoord<-arrayspecs(reg1$residuals + predict(lm(two.d.array(rogtf$coords)~1)),72,2)

ro.n.FEMALE <-mshape(allocoord [1:72, 1:2, which(dat$sex[dat$nat=="RO"]=="F")])
ro.n.MALE  <- mshape(allocoord [1:72, 1:2, which(dat$sex[dat$nat=="RO"]=="M")])
ro.n.MEAN <- mshape(allocoord)

plotRefToTarget(ro.n.MEAN, ro.n.MALE, method = "TPS", mag = 4, links = links, gridPars = GP1)
plotRefToTarget(ro.n.MEAN, ro.n.MEAN, method = "TPS", mag = 4, links = links, gridPars = GP1)
plotRefToTarget(ro.n.MEAN, ro.n.FEMALE, method = "TPS", mag = 4, links = links, gridPars = GP1)


# UK

# allometric sshd
reg1 <- procD.lm(coords ~ height, iter = 999, data = ukgtf, weights = NULL); summary(reg1)
reguk <-procD.lm(reg1$fitted ~ sex, iter = 999, data = ukgtf, weights = NULL); summary(reguk)

uk.a.FEMALE <-mshape(arrayspecs (reg1$fitted, 72, 2) [1:72, 1:2, which(dat$sex[dat$nat=="UK"]=="F")])
uk.a.MALE  <- mshape(arrayspecs (reg1$fitted, 72, 2) [1:72, 1:2, which(dat$sex[dat$nat=="UK"]=="M")])
uk.a.MEAN <- mshape(arrayspecs (reg1$fitted, 72, 2))

plotRefToTarget(uk.a.MEAN, uk.a.MALE, method = "TPS", mag = 4, links = links, gridPars = GP1)
plotRefToTarget(uk.a.MEAN, uk.a.MEAN, method = "TPS", mag = 4, links = links, gridPars = GP1)
plotRefToTarget(uk.a.MEAN, uk.a.FEMALE, method = "TPS", mag = 4, links = links, gridPars = GP1)

# non-allometric sshd
reguk <- procD.lm(reg1$residuals ~ sex, iter = 999, data = ukgtf, weights = NULL); summary(reguk)

allocoord<-arrayspecs(reg1$residuals + predict(lm(two.d.array(ukgtf$coords)~1)),72,2)

uk.n.FEMALE <-mshape(allocoord [1:72, 1:2, which(dat$sex[dat$nat=="UK"]=="F")])
uk.n.MALE  <- mshape(allocoord [1:72, 1:2, which(dat$sex[dat$nat=="UK"]=="M")])
uk.n.MEAN <- mshape(allocoord)

plotRefToTarget(uk.n.MEAN, uk.n.MALE, method = "TPS", mag = 4, links = links, gridPars = GP1)
plotRefToTarget(uk.n.MEAN, uk.n.MEAN, method = "TPS", mag = 4, links = links, gridPars = GP1)
plotRefToTarget(uk.n.MEAN, uk.n.FEMALE, method = "TPS", mag = 4, links = links, gridPars = GP1)


# compound images
# Allometric

tiff("morphs_national_allo.tiff",width=16,height=28,units="cm",res=600,compression="lzw")

par (mar = c(0,0,0,0), oma = c(0,3,3,0))
gp1<-layout(matrix(c(1:40), 8, 5, byrow = T), widths=c(1,1,1), heights=c(1,1,1), respect = F) 
layout.show(gp1)

cex1<-1.2
cex2<-1.2

font2<-2
font1<-2

# namibia
plotRefToTarget(nam.a.MEAN, nam.a.FEMALE, method = "TPS", mag = 3, links = links, gridPars = GP1)
mtext("Namibia", 2, cex = cex2, font = font2)
mtext("3x women", cex = cex1, font = font1)
plotRefToTarget(nam.a.MEAN, nam.a.FEMALE, method = "TPS", mag = 1, links = links, gridPars = GP1)
mtext("women mean", cex = cex1, font = font1)
plotRefToTarget(nam.a.MEAN, nam.a.MEAN, method = "TPS", mag = 1, links = links, gridPars = GP1)
mtext("mean", cex = cex1, font = font1)
plotRefToTarget(nam.a.MEAN, nam.a.MALE, method = "TPS", mag = 1, links = links, gridPars = GP1)
mtext("men mean", cex = cex1, font = font1)
plotRefToTarget(nam.a.MEAN, nam.a.MALE, method = "TPS", mag = 3, links = links, gridPars = GP1)
mtext("3x men", cex = cex1, font = font1)

# cameroon
plotRefToTarget(cmr.a.MEAN, cmr.a.FEMALE, method = "TPS", mag = 3, links = links, gridPars = GP1)
mtext("Cameroon", 2, cex = cex2, font = font2)
plotRefToTarget(cmr.a.MEAN, cmr.a.FEMALE, method = "TPS", mag = 1, links = links, gridPars = GP1)
plotRefToTarget(cmr.a.MEAN, cmr.a.MEAN, method = "TPS", mag = 1, links = links, gridPars = GP1)
plotRefToTarget(cmr.a.MEAN, cmr.a.MALE, method = "TPS", mag = 1, links = links, gridPars = GP1)
plotRefToTarget(cmr.a.MEAN, cmr.a.MALE, method = "TPS", mag = 3, links = links, gridPars = GP1)

# turkey
plotRefToTarget(tur.a.MEAN, tur.a.FEMALE, method = "TPS", mag = 3, links = links, gridPars = GP1)
mtext("Turkey", 2, cex = cex2, font = font2)
plotRefToTarget(tur.a.MEAN, tur.a.FEMALE, method = "TPS", mag = 1, links = links, gridPars = GP1)
plotRefToTarget(tur.a.MEAN, tur.a.MEAN, method = "TPS", mag = 1, links = links, gridPars = GP1)
plotRefToTarget(tur.a.MEAN, tur.a.MALE, method = "TPS", mag = 1, links = links, gridPars = GP1)
plotRefToTarget(tur.a.MEAN, tur.a.MALE, method = "TPS", mag = 3, links = links, gridPars = GP1)

# romania
plotRefToTarget(ro.a.MEAN, ro.a.FEMALE, method = "TPS", mag = 3, links = links, gridPars = GP1)
mtext("Romania", 2, cex = cex2, font = font2)
plotRefToTarget(ro.a.MEAN, ro.a.FEMALE, method = "TPS", mag = 1, links = links, gridPars = GP1)
plotRefToTarget(ro.a.MEAN, ro.a.MEAN, method = "TPS", mag = 1, links = links, gridPars = GP1)
plotRefToTarget(ro.a.MEAN, ro.a.MALE, method = "TPS", mag = 1, links = links, gridPars = GP1)
plotRefToTarget(ro.a.MEAN, ro.a.MALE, method = "TPS", mag = 3, links = links, gridPars = GP1)

# czechia
plotRefToTarget(cz.a.MEAN, cz.a.FEMALE, method = "TPS", mag = 3, links = links, gridPars = GP1)
mtext("Czechia", 2, cex = cex2, font = font2)
plotRefToTarget(cz.a.MEAN, cz.a.FEMALE, method = "TPS", mag = 1, links = links, gridPars = GP1)
plotRefToTarget(cz.a.MEAN, cz.a.MEAN, method = "TPS", mag = 1, links = links, gridPars = GP1)
plotRefToTarget(cz.a.MEAN, cz.a.MALE, method = "TPS", mag = 1, links = links, gridPars = GP1)
plotRefToTarget(cz.a.MEAN, cz.a.MALE, method = "TPS", mag = 3, links = links, gridPars = GP1)

# uk
plotRefToTarget(uk.a.MEAN, uk.a.FEMALE, method = "TPS", mag = 3, links = links, gridPars = GP1)
mtext("UK", 2, cex = cex2, font = font2)
plotRefToTarget(uk.a.MEAN, uk.a.FEMALE, method = "TPS", mag = 1, links = links, gridPars = GP1)
plotRefToTarget(uk.a.MEAN, uk.a.MEAN, method = "TPS", mag = 1, links = links, gridPars = GP1)
plotRefToTarget(uk.a.MEAN, uk.a.MALE, method = "TPS", mag = 1, links = links, gridPars = GP1)
plotRefToTarget(uk.a.MEAN, uk.a.MALE, method = "TPS", mag = 3, links = links, gridPars = GP1)

# brazil
plotRefToTarget(braz.a.MEAN, braz.a.FEMALE, method = "TPS", mag = 3, links = links, gridPars = GP1)
mtext("Brazil", 2, cex = cex2, font = font2)
plotRefToTarget(braz.a.MEAN, braz.a.FEMALE, method = "TPS", mag = 1, links = links, gridPars = GP1)
plotRefToTarget(braz.a.MEAN, braz.a.MEAN, method = "TPS", mag = 1, links = links, gridPars = GP1)
plotRefToTarget(braz.a.MEAN, braz.a.MALE, method = "TPS", mag = 1, links = links, gridPars = GP1)
plotRefToTarget(braz.a.MEAN, braz.a.MALE, method = "TPS", mag = 3, links = links, gridPars = GP1)

# colombia
plotRefToTarget(col.a.MEAN, col.a.FEMALE, method = "TPS", mag = 3, links = links, gridPars = GP1)
mtext("Colombia", 2, cex = cex2, font = font2)
plotRefToTarget(col.a.MEAN, col.a.FEMALE, method = "TPS", mag = 1, links = links, gridPars = GP1)
plotRefToTarget(col.a.MEAN, col.a.MEAN, method = "TPS", mag = 1, links = links, gridPars = GP1)
plotRefToTarget(col.a.MEAN, col.a.MALE, method = "TPS", mag = 1, links = links, gridPars = GP1)
plotRefToTarget(col.a.MEAN, col.a.MALE, method = "TPS", mag = 3, links = links, gridPars = GP1)

dev.off()

# NON-allometric

tiff("morphs_national_nonallo.tiff",width=16,height=28,units="cm",res=600,compression="lzw")

par (mar = c(0,0,0,0), oma = c(0,3,3,0))
gp1<-layout(matrix(c(1:40), 8, 5, byrow = T), widths=c(1,1,1), heights=c(1,1,1), respect = F) 
layout.show(gp1)

cex1<-1.2
cex2<-1.2

font2<-2
font1<-2

# namibia
plotRefToTarget(nam.n.MEAN, nam.n.FEMALE, method = "TPS", mag = 3, links = links, gridPars = GP1)
mtext("Namibia", 2, cex = cex2, font = font2)
mtext("3x women", cex = cex1, font = font1)
plotRefToTarget(nam.n.MEAN, nam.n.FEMALE, method = "TPS", mag = 1, links = links, gridPars = GP1)
mtext("women mean", cex = cex1, font = font1)
plotRefToTarget(nam.n.MEAN, nam.n.MEAN, method = "TPS", mag = 1, links = links, gridPars = GP1)
mtext("mean", cex = cex1, font = font1)
plotRefToTarget(nam.n.MEAN, nam.n.MALE, method = "TPS", mag = 1, links = links, gridPars = GP1)
mtext("men mean", cex = cex1, font = font1)
plotRefToTarget(nam.n.MEAN, nam.n.MALE, method = "TPS", mag = 3, links = links, gridPars = GP1)
mtext("3x men", cex = cex1, font = font1)

# cameroon
plotRefToTarget(cmr.n.MEAN, cmr.n.FEMALE, method = "TPS", mag = 3, links = links, gridPars = GP1)
mtext("Cameroon", 2, cex = cex2, font = font2)
plotRefToTarget(cmr.n.MEAN, cmr.n.FEMALE, method = "TPS", mag = 1, links = links, gridPars = GP1)
plotRefToTarget(cmr.n.MEAN, cmr.n.MEAN, method = "TPS", mag = 1, links = links, gridPars = GP1)
plotRefToTarget(cmr.n.MEAN, cmr.n.MALE, method = "TPS", mag = 1, links = links, gridPars = GP1)
plotRefToTarget(cmr.n.MEAN, cmr.n.MALE, method = "TPS", mag = 3, links = links, gridPars = GP1)

# turkey
plotRefToTarget(tur.n.MEAN, tur.n.FEMALE, method = "TPS", mag = 3, links = links, gridPars = GP1)
mtext("Turkey", 2, cex = cex2, font = font2)
plotRefToTarget(tur.n.MEAN, tur.n.FEMALE, method = "TPS", mag = 1, links = links, gridPars = GP1)
plotRefToTarget(tur.n.MEAN, tur.n.MEAN, method = "TPS", mag = 1, links = links, gridPars = GP1)
plotRefToTarget(tur.n.MEAN, tur.n.MALE, method = "TPS", mag = 1, links = links, gridPars = GP1)
plotRefToTarget(tur.n.MEAN, tur.n.MALE, method = "TPS", mag = 3, links = links, gridPars = GP1)

# romania
plotRefToTarget(ro.n.MEAN, ro.n.FEMALE, method = "TPS", mag = 3, links = links, gridPars = GP1)
mtext("Romania", 2, cex = cex2, font = font2)
plotRefToTarget(ro.n.MEAN, ro.n.FEMALE, method = "TPS", mag = 1, links = links, gridPars = GP1)
plotRefToTarget(ro.n.MEAN, ro.n.MEAN, method = "TPS", mag = 1, links = links, gridPars = GP1)
plotRefToTarget(ro.n.MEAN, ro.n.MALE, method = "TPS", mag = 1, links = links, gridPars = GP1)
plotRefToTarget(ro.n.MEAN, ro.n.MALE, method = "TPS", mag = 3, links = links, gridPars = GP1)

# czechia
plotRefToTarget(cz.n.MEAN, cz.n.FEMALE, method = "TPS", mag = 3, links = links, gridPars = GP1)
mtext("Czechia", 2, cex = cex2, font = font2)
plotRefToTarget(cz.n.MEAN, cz.n.FEMALE, method = "TPS", mag = 1, links = links, gridPars = GP1)
plotRefToTarget(cz.n.MEAN, cz.n.MEAN, method = "TPS", mag = 1, links = links, gridPars = GP1)
plotRefToTarget(cz.n.MEAN, cz.n.MALE, method = "TPS", mag = 1, links = links, gridPars = GP1)
plotRefToTarget(cz.n.MEAN, cz.n.MALE, method = "TPS", mag = 3, links = links, gridPars = GP1)

# uk
plotRefToTarget(uk.n.MEAN, uk.n.FEMALE, method = "TPS", mag = 3, links = links, gridPars = GP1)
mtext("UK", 2, cex = cex2, font = font2)
plotRefToTarget(uk.n.MEAN, uk.n.FEMALE, method = "TPS", mag = 1, links = links, gridPars = GP1)
plotRefToTarget(uk.n.MEAN, uk.n.MEAN, method = "TPS", mag = 1, links = links, gridPars = GP1)
plotRefToTarget(uk.n.MEAN, uk.n.MALE, method = "TPS", mag = 1, links = links, gridPars = GP1)
plotRefToTarget(uk.n.MEAN, uk.n.MALE, method = "TPS", mag = 3, links = links, gridPars = GP1)

# brazil
plotRefToTarget(braz.n.MEAN, braz.n.FEMALE, method = "TPS", mag = 3, links = links, gridPars = GP1)
mtext("Brazil", 2, cex = cex2, font = font2)
plotRefToTarget(braz.n.MEAN, braz.n.FEMALE, method = "TPS", mag = 1, links = links, gridPars = GP1)
plotRefToTarget(braz.n.MEAN, braz.n.MEAN, method = "TPS", mag = 1, links = links, gridPars = GP1)
plotRefToTarget(braz.n.MEAN, braz.n.MALE, method = "TPS", mag = 1, links = links, gridPars = GP1)
plotRefToTarget(braz.n.MEAN, braz.n.MALE, method = "TPS", mag = 3, links = links, gridPars = GP1)

# colombia
plotRefToTarget(col.n.MEAN, col.n.FEMALE, method = "TPS", mag = 3, links = links, gridPars = GP1)
mtext("Colombia", 2, cex = cex2, font = font2)
plotRefToTarget(col.n.MEAN, col.n.FEMALE, method = "TPS", mag = 1, links = links, gridPars = GP1)
plotRefToTarget(col.n.MEAN, col.n.MEAN, method = "TPS", mag = 1, links = links, gridPars = GP1)
plotRefToTarget(col.n.MEAN, col.n.MALE, method = "TPS", mag = 1, links = links, gridPars = GP1)
plotRefToTarget(col.n.MEAN, col.n.MALE, method = "TPS", mag = 3, links = links, gridPars = GP1)


dev.off()



# Comparison od allometric and non-allometric shape changes


tiff("morphs_compare.tiff",width=15,height=15,units="cm",res=600,compression="lzw")

colf<-"#FF7700A0"
colm<-"#3366FFA0"

afcol<-"#DDEEFF"
eucol<-"#FFDDEE"
amcol<-"#DDEEDD"


GP2<-gridPar(pt.bg = colm, pt.size = 0.6, link.col = colm, link.lwd = 1.8, link.lty = 1, out.col = colm, out.cex = 0.1, tar.pt.bg = colf, tar.pt.size = 0.6, tar.link.col = colf, tar.link.lwd = 1.8, tar.link.lty = 1, tar.out.col = colf, tar.out.cex = 0.1, n.col.cell = 30, grid.col = "grey55", grid.lwd = 2, grid.lty = 1, txt.adj = 0.5, txt.pos = 1, txt.cex = 0.8, txt.col = "black")

par (mar = c(0,0,0,0), oma = c(0,3,0,0))

cex1<-1
cex2<-1

font1<-2
font2<-2

mline<-0.5

mag1<-5

rat<-4

lmat<-matrix(rep(1:16+8,each=rat),ncol=4)
lmat<-rbind(c(1,2,3,4),lmat[1:(2*rat),],rep(25,4),c(5,6,7,8),lmat[(2*rat+1):(nrow(lmat)),])

gp1<-layout(lmat, widths=c(1,1,1), heights=c(1,1,1), respect = F) 

layout.show(gp1)

plot(1:10,1:10,axes=F)
rect(-1000,-1000,1000,1000,col=afcol,xpd=T)
plot(1:10,1:10,axes=F)
rect(-1000,-1000,1000,1000,col=afcol,xpd=T)
plot(1:10,1:10,axes=F)
rect(-1000,-1000,1000,1000,col=amcol,xpd=T)
plot(1:10,1:10,axes=F)
rect(-1000,-1000,1000,1000,col=amcol,xpd=T)

plot(1:10,1:10,axes=F)
rect(-1000,-1000,1000,1000,col=eucol,xpd=T)
plot(1:10,1:10,axes=F)
rect(-1000,-1000,1000,1000,col=eucol,xpd=T)
plot(1:10,1:10,axes=F)
rect(-1000,-1000,1000,1000,col=eucol,xpd=T)
plot(1:10,1:10,axes=F)
rect(-1000,-1000,1000,1000,col=eucol,xpd=T)

par(pch=16)
plotRefToTarget2(nam.a.MALE, nam.a.FEMALE, method = "points", mag = mag1, links = links, gridPars = GP2)
mtext("Allometric", 2, cex = cex1, font = font1)
mtext ("Namibia", cex = cex2,line=mline,font=font2)
plotRefToTarget2(nam.n.MALE, nam.n.FEMALE, method = "points", mag = mag1, links = links, gridPars = GP2)
mtext("Nonallometric", 2, cex = cex1, font = font1)

plotRefToTarget2(tur.a.MALE, tur.a.FEMALE, method = "points", mag = mag1, links = links, gridPars = GP2)
mtext("Allometric", 2, cex = cex1, font = font1)
mtext ("Turkey", cex = cex2,line=mline,font=font2)
plotRefToTarget2(tur.n.MALE, tur.n.FEMALE, method = "points", mag = mag1, links = links, gridPars = GP2)
mtext("Nonallometric", 2, cex = cex1, font = font1)

plotRefToTarget2(cmr.a.MALE, cmr.a.FEMALE, method = "points", mag = mag1, links = links, gridPars = GP2)
mtext ("Cameroon", cex = cex2,line=mline,font=font2)
plotRefToTarget2(cmr.n.MALE, cmr.n.FEMALE, method = "points", mag = mag1, links = links, gridPars = GP2)

plotRefToTarget2(ro.a.MALE, ro.a.FEMALE, method = "points", mag = mag1, links = links, gridPars = GP2)
mtext ("Romania", cex = cex2,line=mline,font=font2)
plotRefToTarget2(ro.n.MALE, ro.n.FEMALE, method = "points", mag = mag1, links = links, gridPars = GP2)

plotRefToTarget2(col.a.MALE, col.a.FEMALE, method = "points", mag = mag1, links = links, gridPars = GP2)
mtext ("Colombia", cex = cex2,line=mline,font=font2)
plotRefToTarget2(col.n.MALE, col.n.FEMALE, method = "points", mag = mag1, links = links, gridPars = GP2)

plotRefToTarget2(cz.a.MALE, cz.a.FEMALE, method = "points", mag = mag1, links = links, gridPars = GP2)
mtext ("Czechia", cex = cex2,line=mline,font=font2)
plotRefToTarget2(cz.n.MALE, cz.n.FEMALE, method = "points", mag = mag1, links = links, gridPars = GP2)

plotRefToTarget2(braz.a.MALE, braz.a.FEMALE, method = "points", mag = mag1, links = links, gridPars = GP2)
mtext ("Brazil", cex = cex2,line=mline,font=font2)
plotRefToTarget2(braz.n.MALE, braz.n.FEMALE, method = "points", mag = mag1, links = links, gridPars = GP2)

plotRefToTarget2(uk.a.MALE, uk.a.FEMALE, method = "points", mag = mag1, links = links, gridPars = GP2)
mtext ("UK", cex = cex2,line=mline,font=font2)
plotRefToTarget2(uk.n.MALE, uk.n.FEMALE, method = "points", mag = mag1, links = links, gridPars = GP2)

dev.off()


# TPS aproach

tiff("morphs_compare_TPS.tiff",width=15,height=15,units="cm",res=600,compression="lzw")

colf<-"#FF7700A0"
colm<-"#3366FFA0"

afcol<-"#DDEEFF"
eucol<-"#FFDDEE"
amcol<-"#DDEEDD"

GP2<-gridPar(pt.bg = "cyan4", pt.size = 0.5, link.col = "cyan4", link.lwd = 1, link.lty = 1, out.col = "cyan4", out.cex = 0.1, tar.pt.bg = "black", tar.pt.size = 0.5, tar.link.col = "black", tar.link.lwd = 1, tar.link.lty = 1, tar.out.col = "black", tar.out.cex = 0.1, n.col.cell = 30, grid.col = "grey55", grid.lwd = 0.8, grid.lty = 1, txt.adj = 0.5, txt.pos = 1, txt.cex = 0.8, txt.col = "black")

par (mar = c(0,0,0,0), oma = c(0,3,0,0))

cex1<-1
cex2<-1

font1<-2
font2<-2

mline<-0.5

mag1<-5

rat<-4

lmat<-matrix(rep(1:16+8,each=rat),ncol=4)
lmat<-rbind(c(1,2,3,4),lmat[1:(2*rat),],rep(25,4),c(5,6,7,8),lmat[(2*rat+1):(nrow(lmat)),])

gp1<-layout(lmat, widths=c(1,1,1), heights=c(1,1,1), respect = F) 

layout.show(gp1)

plot(1:10,1:10,axes=F)
rect(-1000,-1000,1000,1000,col=afcol,xpd=T)
plot(1:10,1:10,axes=F)
rect(-1000,-1000,1000,1000,col=afcol,xpd=T)
plot(1:10,1:10,axes=F)
rect(-1000,-1000,1000,1000,col=amcol,xpd=T)
plot(1:10,1:10,axes=F)
rect(-1000,-1000,1000,1000,col=amcol,xpd=T)

plot(1:10,1:10,axes=F)
rect(-1000,-1000,1000,1000,col=eucol,xpd=T)
plot(1:10,1:10,axes=F)
rect(-1000,-1000,1000,1000,col=eucol,xpd=T)
plot(1:10,1:10,axes=F)
rect(-1000,-1000,1000,1000,col=eucol,xpd=T)
plot(1:10,1:10,axes=F)
rect(-1000,-1000,1000,1000,col=eucol,xpd=T)

par(pch=16)

plotRefToTarget(nam.a.MALE, nam.a.FEMALE, method = "TPS", mag = mag1, links = links, gridPars = GP2)
mtext("Allometric", 2, cex = cex1, font = font1)
mtext ("Namibia", cex = cex2,line=mline,font=font2)
plotRefToTarget(nam.n.MALE, nam.n.FEMALE, method = "TPS", mag = mag1, links = links, gridPars = GP2)
mtext("Nonallometric", 2, cex = cex1, font = font1)

plotRefToTarget(tur.a.MALE, tur.a.FEMALE, method = "TPS", mag = mag1, links = links, gridPars = GP2)
mtext("Allometric", 2, cex = cex1, font = font1)
mtext ("Turkey", cex = cex2,line=mline,font=font2)
plotRefToTarget(tur.n.MALE, tur.n.FEMALE, method = "TPS", mag = mag1, links = links, gridPars = GP2)
mtext("Nonallometric", 2, cex = cex1, font = font1)

plotRefToTarget(cmr.a.MALE, cmr.a.FEMALE, method = "TPS", mag = mag1, links = links, gridPars = GP2)
mtext ("Cameroon", cex = cex2,line=mline,font=font2)
plotRefToTarget(cmr.n.MALE, cmr.n.FEMALE, method = "TPS", mag = mag1, links = links, gridPars = GP2)

plotRefToTarget(ro.a.MALE, ro.a.FEMALE, method = "TPS", mag = mag1, links = links, gridPars = GP2)
mtext ("Romania", cex = cex2,line=mline,font=font2)
plotRefToTarget(ro.n.MALE, ro.n.FEMALE, method = "TPS", mag = mag1, links = links, gridPars = GP2)

plotRefToTarget(col.a.MALE, col.a.FEMALE, method = "TPS", mag = mag1, links = links, gridPars = GP2)
mtext ("Colombia", cex = cex2,line=mline,font=font2)
plotRefToTarget(col.n.MALE, col.n.FEMALE, method = "TPS", mag = mag1, links = links, gridPars = GP2)

plotRefToTarget(cz.a.MALE, cz.a.FEMALE, method = "TPS", mag = mag1, links = links, gridPars = GP2)
mtext ("Czechia", cex = cex2,line=mline,font=font2)
plotRefToTarget(cz.n.MALE, cz.n.FEMALE, method = "TPS", mag = mag1, links = links, gridPars = GP2)

plotRefToTarget(braz.a.MALE, braz.a.FEMALE, method = "TPS", mag = mag1, links = links, gridPars = GP2)
mtext ("Brazil", cex = cex2,line=mline,font=font2)
plotRefToTarget(braz.n.MALE, braz.n.FEMALE, method = "TPS", mag = mag1, links = links, gridPars = GP2)

plotRefToTarget(uk.a.MALE, uk.a.FEMALE, method = "TPS", mag = mag1, links = links, gridPars = GP2)
mtext ("UK", cex = cex2,line=mline,font=font2)
plotRefToTarget(uk.n.MALE, uk.n.FEMALE, method = "TPS", mag = mag1, links = links, gridPars = GP2)

dev.off()

