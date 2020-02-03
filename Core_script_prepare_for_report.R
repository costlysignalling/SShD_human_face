
# Load packages (R studio should suggest installation of those, that are not present on your computer)
library(Morpho)
library(geomorph)
library(abind)
library(RRPP)
library(ggpubr)
library(standardize)
library(lmerTest)
library(vioplot)
library(randomcoloR)
library(ggplot2)
library(ppcor)
library(ggfortify)
library(tidyverse)
library(cluster)
library(factoextra)
library(dendextend)
library(corrplot)
library(randomcoloR)
library(mapplots)

# Set some defaults to avoid extensive report messages
formals(procD.lm)$print.progress<-FALSE
formals(morphol.disparity)$print.progress<-FALSE

# Load data
load("Symmetrized_coordinates_height.RData")  # Loads facial coordinates into object gpone.
#Here, only coordinates of faces for which the information about body height is available (N=1114). If you want to run parts of the analysis, where this information is not necessary on the unrestricted sample (N=1317) load  data file "Symmetrized_coordinates_all.RData" The rest of the script shall be identical, some parts of the analysis (like the decomposition to the allometric/nonallometric components) will, however, not work.

#load indications of links for the visualizations of facial morphology
links<-read.table("linksR.txt")

dimdat<-read.delim("dimorphism_data.txt") # Loads other variables
dimdat<-dimdat[!is.na(dimdat$height),] # Use only the lines, where body height is reliably assessed 

# Divide the data to two sets according to the sex
fdat <- subset(dimdat, sex=="F")
mdat <- subset(dimdat, sex=="M")

# Connect these data frames to the final data to ensure there are no iindividuals with unsure sex
dat<- rbind(fdat, mdat)

#Inspect the coordinates
plot(gpagen(gpone, PrinAxes = F))

# Create geomorph data frame with varibales corresponding to several variables from dimdat
onegdf <- geomorph.data.frame(coords = gpone, sex = dimdat$sex, set = dimdat$set, nat = dimdat$nat, age = dimdat$age, weight = dimdat$weight, height = dimdat$height, attractiveness = dimdat$Attractiveness)

# Subset landmark coordinates according to sex 
shapesex<- coords.subset(onegdf$coords, onegdf$sex)

# Create array ordered by sex from the shapesex data frame
shape <- abind (shapesex$F, shapesex$M)

# Generalized procrustes analysis of the resulting array
gpsh<-gpagen(shape,  ProcD = F, curves = NULL, PrinAxes = F, Proj = TRUE)
plot(gpsh)

# Create geomorph data frame with varibales corresponding to several vectors from dat which has the same order of lines as the set of shapes in gpsh - females then males
gtf<-geomorph.data.frame(gpsh, sex = dat$sex, set =  dat$set, nat = dat$nat, age = dat$age, weight = dat$weight, height = dat$height, attractiveness = dat$Attractiveness)

# Investigate the outliers
invisible(capture.output(
  plotOutliers(gtf$coords, groups = gtf$sex, inspect.outliers = FALSE)
))
# Some points are indicated as potential outliers, but all the data points create a smooth curve, so we decide to retain the whole data

plotTangentSpace(gtf$coords, axis1=1, axis2=2, warpgrids=F, groups = gtf$sex, legend = T)
plotTangentSpace(gtf$coords, axis1=2, axis2=3, warpgrids=F, groups = gtf$sex, legend = T)
plotTangentSpace(gtf$coords, axis1=1, axis2=2, warpgrids=F, groups = gtf$nat, legend = T)
plotTangentSpace(gtf$coords, axis1=2, axis2=3, warpgrids=F, groups = gtf$nat, legend = T)
# Scatter plots do not indicate any problems of extreme outliers

# Analysis of variance in the face shape explained by sex
reg1<-procD.lm(coords ~ sex, iter = 999, data = gtf, weights = NULL)
summary(reg1)

# Extract the centers of the sex-specific clouds of points in the multidemensional space. Intercept corresponds to the female mean, sexM slope to the difference between male and female mean.
coefficients<-coef(reg1, test = F)
sex.vecs<-coefficients[2,]

#Projection of facial shapes on the vector connecting female and male mean, the resulting score is saved as sexscores variable
scores <- two.d.array (gpsh$coords) %*% t(coefficients)
sexscores <- scores [,2]
hist(sexscores)

#Save these sexscores for a future reference to an object with "original" sexscores
orig_sexscores<-sexscores

# Restart graphics window
dev.off()

# Preparing the maleness-femaleness scores for each face within each country and maleness-femaless vectors for each country
onecoords<-coords.subset(onegdf$coords, onegdf$nat)

# We repeat the same procedure for each nationality separately
# Brazil
brazgdf <- geomorph.data.frame(coords = onecoords$BRAZ, sex = dat$sex[dat$nat=="BRAZ"])
reg1<-procD.lm(coords ~ sex, iter = 999, data = brazgdf); summary(reg1)

coefficients<-coef(reg1, test = F)
sex.vecs<-cbind(sex.vecs,coefficients[2,])

scores <- two.d.array (onecoords$BRAZ) %*% t(coefficients)
brazsc <- scores [,2]

fbrazsc <- brazsc [which(dat$sex[dat$nat=="BRAZ"]=="F")]
mbrazsc <- brazsc [which(dat$sex[dat$nat=="BRAZ"]=="M")]

brazcoef<-t(coefficients)[,2]

# cmr
cmrgdf <- geomorph.data.frame(coords = onecoords$CMR, sex = dat$sex[dat$nat=="CMR"])
reg1<-procD.lm(coords ~ sex, iter = 999, data = cmrgdf); summary(reg1)

coefficients<-coef(reg1, test = F)
sex.vecs<-cbind(sex.vecs,coefficients[2,])

scores <- two.d.array (onecoords$CMR) %*% t(coefficients)
cmrsc <- scores [,2]

fcmrsc <- cmrsc [which(dat$sex[dat$nat=="CMR"]=="F")]
mcmrsc <- cmrsc [which(dat$sex[dat$nat=="CMR"]=="M")]

cmrcoef<-t(coefficients)[,2]

# col
colgdf <- geomorph.data.frame(coords = onecoords$COL, sex = dat$sex[dat$nat=="COL"])
reg1<-procD.lm(coords ~ sex, iter = 999, data = colgdf); summary(reg1)

coefficients<-coef(reg1, test = F)
sex.vecs<-cbind(sex.vecs,coefficients[2,])

scores <- two.d.array (onecoords$COL) %*% t(coefficients)
colsc <- scores [,2]

fcolsc <- colsc [which(dat$sex[dat$nat=="COL"]=="F")]
mcolsc <- colsc [which(dat$sex[dat$nat=="COL"]=="M")]

colcoef<-t(coefficients)[,2]

# cz
czgdf <- geomorph.data.frame(coords = onecoords$CZ, sex = dat$sex[dat$nat=="CZ"])
reg1<-procD.lm(coords ~ sex, iter = 999, data = czgdf); summary(reg1)

coefficients<-coef(reg1, test = F)
sex.vecs<-cbind(sex.vecs,coefficients[2,])

scores <- two.d.array (onecoords$CZ) %*% t(coefficients)
czsc <- scores [,2]

fczsc <- czsc [which(dat$sex[dat$nat=="CZ"]=="F")]
mczsc <- czsc [which(dat$sex[dat$nat=="CZ"]=="M")]

czcoef<-t(coefficients)[,2]

# nam
namgdf <- geomorph.data.frame(coords = onecoords$NAM, sex = dat$sex[dat$nat=="NAM"])
reg1<-procD.lm(coords ~ sex, iter = 999, data = namgdf); summary(reg1)

coefficients<-coef(reg1, test = F)
sex.vecs<-cbind(sex.vecs,coefficients[2,])

scores <- two.d.array (onecoords$NAM) %*% t(coefficients)
namsc <- scores [,2]

fnamsc <- namsc [which(dat$sex[dat$nat=="NAM"]=="F")]
mnamsc <- namsc [which(dat$sex[dat$nat=="NAM"]=="M")]

namcoef<-t(coefficients)[,2]

# ro
rogdf <- geomorph.data.frame(coords = onecoords$RO, sex = dat$sex[dat$nat=="RO"])
reg1<-procD.lm(coords ~ sex, iter = 999, data = rogdf); summary(reg1)

coefficients<-coef(reg1, test = F)
sex.vecs<-cbind(sex.vecs,coefficients[2,])

scores <- two.d.array (onecoords$RO) %*% t(coefficients)
rosc <- scores [,2]

frosc <- rosc [which(dat$sex[dat$nat=="RO"]=="F")]
mrosc <- rosc [which(dat$sex[dat$nat=="RO"]=="M")]

rocoef<-t(coefficients)[,2]

# tr
trgdf <- geomorph.data.frame(coords = onecoords$TR, sex = dat$sex[dat$nat=="TR"])
reg1<-procD.lm(coords ~ sex, iter = 999, data = trgdf); summary(reg1)

coefficients<-coef(reg1, test = F)
sex.vecs<-cbind(sex.vecs,coefficients[2,])

scores <- two.d.array (onecoords$TR) %*% t(coefficients)
trsc <- scores [,2]

ftrsc <- trsc [which(dat$sex[dat$nat=="TR"]=="F")]
mtrsc <- trsc [which(dat$sex[dat$nat=="TR"]=="M")]

trcoef<-t(coefficients)[,2]

# uk
ukgdf <- geomorph.data.frame(coords = onecoords$UK, sex = dat$sex[dat$nat=="UK"])
reg1<-procD.lm(coords ~ sex, iter = 999, data = ukgdf); summary(reg1)

coefficients<-coef(reg1, test = F)
sex.vecs<-cbind(sex.vecs,coefficients[2,])

scores <- two.d.array (onecoords$UK) %*% t(coefficients)
uksc <- scores [,2]

fuksc <- uksc [which(dat$sex[dat$nat=="UK"]=="F")]
muksc <- uksc [which(dat$sex[dat$nat=="UK"]=="M")]

ukcoef<-t(coefficients)[,2]

#We connect the nation specific scores into vectors of all sex scores - equivalents of previous approach
fsexscores1 <- c(fcmrsc, fczsc, ftrsc, fnamsc, frosc, fuksc, fcolsc, fbrazsc)
msexscores1 <- c(mcmrsc, mczsc, mtrsc, mnamsc, mrosc, muksc, mcolsc, mbrazsc)
sexscores1<-c(fsexscores1,msexscores1)

vioplot(sexscores1~onegdf$sex, range = 1.5, horizontal = F, col = "gold", plotCentre = "point")
vioplot(sexscores1~onegdf$nat, range = 1.5, horizontal = F, col = "gold", plotCentre = "point")

#Visualization of both sexes malenes-femaleness distributions
vioplot(fsexscores1~gtf$nat[gtf$sex=="F"], range = 1.5, horizontal = F, col = "#FF770080", plotCentre = "point", ylim=c(-0.004, 0.004), xlab="Country",ylab="SShD")
vioplot(msexscores1~gtf$nat[gtf$sex=="M"], range = 1.5, horizontal = F, col = "#3366FF80", plotCentre = "point",add=T,xlab="",ylab="")
vioplot(sexscores1~gtf$nat, range = 1.5, horizontal = F, col = "#FFFFFF00", lineCol = NA,add=T,xlab="",ylab="",border="#FF0066",lwd=1.8,rectCol=NA,pchMed=NA)
title (main = "Sexual Shape Dimorphism")


# ********************************************
# Correlation beween SShD scores and ratings
# ********************************************

FATRA <- scale_by(Attractiveness ~ nat, data = fdat, scale = 1)  # scale attractiveness within each level of a factor: "nationality"
MATRA <- scale_by(Attractiveness ~ nat, data = mdat, scale = 1)
SATRA <- c(FATRA, MATRA)

fdat$Attractiveness <- FATRA
mdat$Attractiveness <- MATRA
dat$Attractiveness <- SATRA

brazdat <- subset(dat, nat == "BRAZ")
fbrazdat <- subset (brazdat, sex == "F")
mbrazdat <- subset (brazdat, sex == "M")

cmrdat <- subset(dat, nat == "CMR")
fcmrdat <- subset (cmrdat, sex == "F")
mcmrdat <- subset (cmrdat, sex == "M")

coldat <- subset (dat, nat == "COL")
fcoldat <- subset (coldat, sex == "F")
mcoldat <- subset (coldat, sex == "M")

czdat <- subset (dat, nat == "CZ")
fczdat <- subset (czdat, sex == "F")
mczdat <- subset (czdat, sex == "M")

namdat <- subset (dat, nat == "NAM")
fnamdat <- subset (namdat, sex == "F")
mnamdat <- subset (namdat, sex == "M")

rodat <- subset (dat, nat == "RO")
frodat <- subset (rodat, sex == "F")
mrodat <- subset (rodat, sex == "M")

trdat <- subset (dat, nat == "TR")
ftrdat <- subset (trdat, sex == "F")
mtrdat <- subset (trdat, sex == "M")

ukdat <- subset (dat, nat == "UK")
fukdat <- subset (ukdat, sex == "F")
mukdat <- subset (ukdat, sex == "M")


# We investigate the correlation between maleness-femaleness and attractivenes in each subset. We reverse the maleness-femaleness scale in females to expect positive correlation with the "sex-typicality" in both, males and females. Partial correaltion controlling for age of participants was conducted as well. We create a new variable called Sextypicality for each nationality

# BRAZ
(fp<-with(fbrazdat, pcor.test(Attractiveness, -1*fbrazsc, age)))
(fc<-with(fbrazdat, cor.test(Attractiveness, -1*fbrazsc)))

(mp<-with(mbrazdat, pcor.test(Attractiveness, mbrazsc, age)))
(mc<-with(mbrazdat, cor.test(Attractiveness, mbrazsc)))

Sextypicality <- c(-1*fbrazsc, mbrazsc)
brazdat1 <- cbind (brazdat, Sextypicality)

#We construct a linear model of the interaction between sex and sextypicality. We plot the regression lines together with 95% confidence intervals.

fontsize<-14

model<-lm(Attractiveness~sex*Sextypicality, data=brazdat1)
summary(model)

DF.pred<-expand.grid(Sextypicality=seq(min(Sextypicality),max(Sextypicality),0.0001),sex=c("F","M"))
PRED<-predict.lm(model,newdata = DF.pred, type="response",interval="confidence")
gg.predict<-ggplot(data.frame(DF.pred,PRED),aes(y = fit,x=Sextypicality,group=sex))+
  geom_line(data=data.frame(DF.pred,PRED),aes(y = fit,x=Sextypicality,group=sex,color=sex))+
  geom_ribbon(data=data.frame(DF.pred,PRED),aes(ymin=lwr, ymax=upr, x=Sextypicality, fill = sex,group=sex), alpha = 0.2)+ geom_point(data=brazdat1,aes(x=Sextypicality,y=Attractiveness,colour=sex, group=sex), alpha=0.5)+
  theme_bw(base_size = fontsize)

gg.predict <- gg.predict + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())

BrazPlot<- gg.predict + scale_color_discrete(labels=c("Women", "Men"))+
  scale_fill_discrete(labels=c("Women", "Men"))+
  scale_y_continuous(name="Attractiveness")

BrazPlot


# CMR

(fp<-with(fcmrdat, pcor.test(Attractiveness, -1*fcmrsc, age)))
(fc<-with(fcmrdat, cor.test(Attractiveness, -1*fcmrsc)))

(mp<-with(mcmrdat, pcor.test(Attractiveness, mcmrsc, age)))
(mc<-with(mcmrdat, cor.test(Attractiveness, mcmrsc)))

Sextypicality <- c(-1*fcmrsc, mcmrsc)
cmrdat1 <- cbind (cmrdat, Sextypicality)

model<-lm(Attractiveness~sex*Sextypicality, data=cmrdat1)
summary(model)

DF.pred<-expand.grid(Sextypicality=seq(min(Sextypicality),max(Sextypicality),0.0001),sex=c("F","M"))
PRED<-predict.lm(model,newdata = DF.pred, type="response",interval="confidence")
gg.predict<-ggplot(data.frame(DF.pred,PRED),aes(y = fit,x=Sextypicality,group=sex))+
  geom_line(data=data.frame(DF.pred,PRED),aes(y = fit,x=Sextypicality,group=sex,color=sex))+
  geom_ribbon(data=data.frame(DF.pred,PRED),aes(ymin=lwr, ymax=upr, x=Sextypicality, fill = sex,group=sex), alpha = 0.2)+ geom_point(data=cmrdat1,aes(x=Sextypicality,y=Attractiveness,colour=sex, group=sex),alpha=0.5)+
  theme_bw(base_size = fontsize)

gg.predict <- gg.predict + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())

CmrPlot<-gg.predict+ scale_color_discrete(labels=c("Women", "Men"))+
  scale_fill_discrete(labels=c("Women", "Men"))+
  scale_y_continuous(name="Attractiveness")

CmrPlot

# COL
(fp<-with(fcoldat, pcor.test(Attractiveness, -1*fcolsc, age)))
(fc<-with(fcoldat, cor.test(Attractiveness, -1*fcolsc)))

(mp<-with(mcoldat, pcor.test(Attractiveness, mcolsc, age)))
(mc<-with(mcoldat, cor.test(Attractiveness, mcolsc)))

Sextypicality <- c(-1*fcolsc, mcolsc)
coldat1 <- cbind (coldat, Sextypicality)

model<-lm(Attractiveness~sex*Sextypicality, data=coldat1)
summary(model)

DF.pred<-expand.grid(Sextypicality=seq(min(Sextypicality),max(Sextypicality),0.0001),sex=c("F","M"))
PRED<-predict.lm(model,newdata = DF.pred, type="response",interval="confidence")
gg.predict<-ggplot(data.frame(DF.pred,PRED),aes(y = fit,x=Sextypicality,group=sex))+
  geom_line(data=data.frame(DF.pred,PRED),aes(y = fit,x=Sextypicality,group=sex,color=sex))+
  geom_ribbon(data=data.frame(DF.pred,PRED),aes(ymin=lwr, ymax=upr, x=Sextypicality, fill = sex,group=sex), alpha = 0.2)+ geom_point(data=coldat1,aes(x=Sextypicality,y=Attractiveness,colour=sex, group=sex),alpha=0.5)+
  theme_bw(base_size = fontsize)

gg.predict <- gg.predict + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())

ColPlot<-gg.predict+ scale_color_discrete(labels=c("Women", "Men"))+
  scale_fill_discrete(labels=c("Women", "Men"))+
  scale_y_continuous(name="Attractiveness")

ColPlot

# CZ
(fp<-with(fczdat, pcor.test(Attractiveness, -1*fczsc, age)))
(fc<-with(fczdat, cor.test(Attractiveness, -1*fczsc)))

(mp<-with(mczdat, pcor.test(Attractiveness, mczsc, age)))
(mc<-with(mczdat, cor.test(Attractiveness, mczsc)))

Sextypicality <- c(-1*fczsc, mczsc)

czdat1 <- cbind (czdat, Sextypicality)

model<-lm(Attractiveness~sex*Sextypicality, data=czdat1)
summary(model)

DF.pred<-expand.grid(Sextypicality=seq(min(Sextypicality),max(Sextypicality),0.0001),sex=c("F","M"))
PRED<-predict.lm(model,newdata = DF.pred, type="response",interval="confidence")
gg.predict<-ggplot(data.frame(DF.pred,PRED),aes(y = fit,x=Sextypicality,group=sex))+
  geom_line(data=data.frame(DF.pred,PRED),aes(y = fit,x=Sextypicality,group=sex,color=sex))+
  geom_ribbon(data=data.frame(DF.pred,PRED),aes(ymin=lwr, ymax=upr, x=Sextypicality, fill = sex,group=sex), alpha = 0.2)+ geom_point(data=czdat1,aes(x=Sextypicality,y=Attractiveness,colour=sex, group=sex),alpha=0.5)+
  theme_bw(base_size = fontsize)

gg.predict <- gg.predict + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())

CzPlot<-gg.predict+ scale_color_discrete(labels=c("Women", "Men"))+
  scale_fill_discrete(labels=c("Women", "Men"))+
  scale_y_continuous(name="Attractiveness")

CzPlot

# NAM

fnamdat.comp<-fnamdat[!is.na(fnamdat$age),]
fnamsc.comp<-fnamsc[!is.na(fnamdat$age)]

mnamdat.comp<-mnamdat[!is.na(mnamdat$age),]
mnamsc.comp<-mnamsc[!is.na(mnamdat$age)]

(fp<-with(fnamdat.comp, pcor.test(Attractiveness, -1*fnamsc.comp, age)))
(fc<-with(fnamdat, cor.test(Attractiveness, -1*fnamsc)))

(mp<-with(mnamdat.comp, pcor.test(Attractiveness, mnamsc.comp, age)))
(mc<-with(mnamdat, cor.test(Attractiveness, mnamsc)))

Sextypicality <- c(-1*fnamsc, mnamsc)
namdat1 <- cbind (namdat, Sextypicality)

model<-lm(Attractiveness~sex*Sextypicality, data=namdat1)
summary(model)

DF.pred<-expand.grid(Sextypicality=seq(min(Sextypicality),max(Sextypicality),0.0001),sex=c("F","M"))
PRED<-predict.lm(model,newdata = DF.pred, type="response",interval="confidence")
gg.predict<-ggplot(data.frame(DF.pred,PRED),aes(y = fit,x=Sextypicality,group=sex))+
  geom_line(data=data.frame(DF.pred,PRED),aes(y = fit,x=Sextypicality,group=sex,color=sex))+
  geom_ribbon(data=data.frame(DF.pred,PRED),aes(ymin=lwr, ymax=upr, x=Sextypicality, fill = sex,group=sex), alpha = 0.2)+ geom_point(data=namdat1,aes(x=Sextypicality,y=Attractiveness,colour=sex, group=sex),alpha=0.5)+
  theme_bw(base_size = fontsize)

gg.predict <- gg.predict + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())

NamPlot<-gg.predict+ scale_color_discrete(labels=c("Women", "Men"))+
  scale_fill_discrete(labels=c("Women", "Men"))+
  scale_y_continuous(name="Attractiveness")

NamPlot

# RO
(fp<-with(frodat, pcor.test(Attractiveness, -1*frosc, age)))
(fc<-with(frodat, cor.test(Attractiveness, -1*frosc)))

(mp<-with(mrodat, pcor.test(Attractiveness, mrosc, age)))
(mc<-with(mrodat, cor.test(Attractiveness, mrosc)))

Sextypicality <- c(-1*frosc, mrosc)
rodat1 <- cbind (rodat, Sextypicality)

model<-lm(Attractiveness~sex*Sextypicality, data=rodat1)
summary(model)

DF.pred<-expand.grid(Sextypicality=seq(min(Sextypicality),max(Sextypicality),0.0001),sex=c("F","M"))
PRED<-predict.lm(model,newdata = DF.pred, type="response",interval="confidence")
gg.predict<-ggplot(data.frame(DF.pred,PRED),aes(y = fit,x=Sextypicality,group=sex))+
  geom_line(data=data.frame(DF.pred,PRED),aes(y = fit,x=Sextypicality,group=sex,color=sex))+
  geom_ribbon(data=data.frame(DF.pred,PRED),aes(ymin=lwr, ymax=upr, x=Sextypicality, fill = sex,group=sex), alpha = 0.2)+ geom_point(data=rodat1,aes(x=Sextypicality,y=Attractiveness,colour=sex, group=sex),alpha=0.5)+
  theme_bw(base_size = fontsize)

gg.predict <- gg.predict + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())

RoPlot<-gg.predict+ scale_color_discrete(labels=c("Women", "Men"))+
  scale_fill_discrete(labels=c("Women", "Men"))+
  scale_y_continuous(name="Attractiveness")

RoPlot

# TR
(fp<-with(ftrdat, pcor.test(Attractiveness, -1*ftrsc, age)))
(fc<-with(ftrdat, cor.test(Attractiveness, -1*ftrsc)))

(mp<-with(mtrdat, pcor.test(Attractiveness, mtrsc, age)))
(mc<-with(mtrdat, cor.test(Attractiveness, mtrsc)))

Sextypicality <- c(-1*ftrsc, mtrsc)
trdat1 <- cbind (trdat, Sextypicality)

model<-lm(Attractiveness~sex*Sextypicality, data=trdat1)
summary(model)

DF.pred<-expand.grid(Sextypicality=seq(min(Sextypicality),max(Sextypicality),0.0001),sex=c("F","M"))
PRED<-predict.lm(model,newdata = DF.pred, type="response",interval="confidence")
gg.predict<-ggplot(data.frame(DF.pred,PRED),aes(y = fit,x=Sextypicality,group=sex))+
  geom_line(data=data.frame(DF.pred,PRED),aes(y = fit,x=Sextypicality,group=sex,color=sex))+
  geom_ribbon(data=data.frame(DF.pred,PRED),aes(ymin=lwr, ymax=upr, x=Sextypicality, fill = sex,group=sex), alpha = 0.2)+ geom_point(data=trdat1,aes(x=Sextypicality,y=Attractiveness,colour=sex, group=sex),alpha=0.5)+
  theme_bw(base_size = fontsize)

gg.predict <- gg.predict + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())

TrPlot<-gg.predict+ scale_color_discrete(labels=c("Women", "Men"))+
  scale_fill_discrete(labels=c("Women", "Men"))+
  scale_y_continuous(name="Attractiveness")

TrPlot

# UK

(fp<-with(fukdat, pcor.test(Attractiveness, -1*fuksc, age)))
(fc<-with(fukdat, cor.test(Attractiveness, -1*fuksc)))

(mp<-with(mukdat, pcor.test(Attractiveness, muksc, age)))
(mc<-with(mukdat, cor.test(Attractiveness, muksc)))

Sextypicality <- c(-1*fuksc, muksc)
ukdat1 <- cbind (ukdat, Sextypicality)

model<-lm(Attractiveness~sex*Sextypicality, data=ukdat1)
summary(model)

DF.pred<-expand.grid(Sextypicality=seq(min(Sextypicality),max(Sextypicality),0.0001),sex=c("F","M"))
PRED<-predict.lm(model,newdata = DF.pred, type="response",interval="confidence")
gg.predict<-ggplot(data.frame(DF.pred,PRED),aes(y = fit,x=Sextypicality,group=sex))+
  geom_line(data=data.frame(DF.pred,PRED),aes(y = fit,x=Sextypicality,group=sex,color=sex))+
  geom_ribbon(data=data.frame(DF.pred,PRED),aes(ymin=lwr, ymax=upr, x=Sextypicality, fill = sex,group=sex), alpha = 0.2)+ geom_point(data=ukdat1,aes(x=Sextypicality,y=Attractiveness,colour=sex, group=sex),alpha=0.5)+
  theme_bw(base_size = fontsize)

gg.predict <- gg.predict + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())

UkPlot<-gg.predict+ scale_color_discrete(labels=c("Women", "Men"))+
  scale_fill_discrete(labels=c("Women", "Men"))+
  scale_y_continuous(name="Attractiveness")

UkPlot

# ALLTOGETHER

# We retrive maleness-femaleness scores from the original sexscores
fscores<-orig_sexscores[which(dat$sex=="F")] 
mscores<-orig_sexscores[which(dat$sex=="M")]

fdat.comp<-fdat[!is.na(fdat$age),]
fscores.comp<-fscores[!is.na(fdat$age)]

mdat.comp<-mdat[!is.na(mdat$age),]
mscores.comp<-mscores[!is.na(mdat$age)]

(fp<-with(fdat.comp, pcor.test(Attractiveness, -1*fscores.comp, age)))
(fc<-with(fdat, cor.test(Attractiveness, -1*fscores)))

(mp<-with(mdat.comp, pcor.test(Attractiveness, mscores.comp, age)))
(mc<-with(mdat, cor.test(Attractiveness, mscores)))

# And conduct similar tests on the full sample - This is ineresting
(fall.cor.at<-cor.test(FATRA, -1*fscores))
(fall.cor.h<-cor.test (fdat$height, fscores))

(mall.cor.at<-cor.test(MATRA, mscores))
(mall.cor.h<-cor.test (mdat$height, mscores))

Sextypicality <- c(-1*fscores, mscores)
dat1 <- cbind (dat, Sextypicality, SATRA)

model<-lm(SATRA~sex*Sextypicality, data=dat1)
summary(model)

DF.pred<-expand.grid(Sextypicality=seq(min(Sextypicality), max(Sextypicality),0.0001),sex=c("F","M"))
PRED<-predict.lm(model,newdata = DF.pred, type="response",interval="confidence")
gg.predict<-ggplot(data.frame(DF.pred,PRED),aes(y = fit,x=Sextypicality,group=sex))+
  geom_line(data=data.frame(DF.pred,PRED),aes(y = fit,x=Sextypicality,group=sex,color=sex))+
  geom_ribbon(data=data.frame(DF.pred,PRED),aes(ymin=lwr, ymax=upr, x=Sextypicality, fill = sex,group=sex), alpha = 0.2)+ geom_point(data=dat1,aes(x=Sextypicality,y=SATRA,colour=sex, group=sex),alpha=0.05)+
  theme_bw(base_size = fontsize)

gg.predict <- gg.predict + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())

AllPlot<-gg.predict+ scale_color_discrete(labels=c("Women", "Men"))+
  scale_fill_discrete(labels=c("Women", "Men"))+
  scale_y_continuous(name="Attractiveness")

AllPlot


#We measure the angle between the vector of maleness-femaleness and within-sex attractiveness

fgdf <- geomorph.data.frame(coords = gtf$coords[,,gtf$sex=="F"], attractiveness = gtf$attractiveness[gtf$sex=="F"], nat = gtf$nat[gtf$sex=="F"])

mgdf <- geomorph.data.frame(coords = gtf$coords[,,gtf$sex=="M"], attractiveness = gtf$attractiveness[gtf$sex=="M"], nat = gtf$nat[gtf$sex=="M"])

# We need to define several important functions
# We create a function that calculates the angle between two vectors in multidimensional space

# Functions to convert radians to degrees and vice versa
rad2deg<-function(r){r*180/pi}
deg2rad<-function(d){d*pi/180}

# Function tha calculates the angle between 2 vectors in n dimensional space
angle <- function(x,y,deg=T){
  dot.prod <- x%*%y
  norm.x <- norm(x,type="2")
  norm.y <- norm(y,type="2")
  theta <- acos(dot.prod / (norm.x * norm.y))
  res<-as.numeric(theta)
  if(deg==T){
    res<-rad2deg(res)
  }
  return(res)
}

# Function that converts any angle to the smallest possible angle that ratins the angles but not the directions of vectors  
sangle<-function(d){min(c(d%%360,abs(d%%360-180),abs(abs(d%%360-180)-180)))}

# We find the vectors as coefficients in multidimensional regression
regsex<-procD.lm(coords ~ sex, iter = 999, data = gtf, weights = NULL); summary(regsex)
coefficients<-coef(regsex, test = F)
vsex<-coefficients[2,]

frega<-procD.lm(coords ~ attractiveness, iter = 999, data = fgdf); summary(frega)
coefficients<-coef(frega, test = F)
vfatr<-coefficients[2,]

mrega<-procD.lm(coords ~ attractiveness, iter = 999, data = mgdf); summary(mrega)
coefficients<-coef(mrega, test = F)
vmatr<-coefficients[2,]

# We calculate the angles

(fang.atr<-angle(-1*vsex, vfatr))
sangle(fang.atr)

(mang.atr<-angle(vsex, vmatr))
sangle(mang.atr)


# ***** TPS vizualization of SShD *****
shapesex<- coords.subset(onegdf$coords, onegdf$sex)

MEN <- mshape(shapesex$M)
WOMEN <- mshape(shapesex$F)
MEAN <- mshape(onegdf$coords)

par (mar = c(0,0,0,0), oma = c(0,3,0,0))
(gp1<-layout(matrix(c(1:5), 1, 5, byrow = T), widths=c(1,1,1), heights=c(1,1,1), respect = F)) 


# Set plotting parameters for the grid 
GP1<-gridPar(pt.bg = "red", pt.size = 1, link.col = "blue", link.lwd = 3, link.lty = 1, out.col = "gray", out.cex = 0.1, tar.pt.bg = "black", tar.pt.size = 0.5, tar.link.col = "black", tar.link.lwd = 3, tar.link.lty = 1, tar.out.col = "black", tar.out.cex = 0.1, n.col.cell = 30, grid.col = "grey65", grid.lwd = 1.5, grid.lty = 1, txt.adj = 0.5, txt.pos = 1, txt.cex = 0.8, txt.col = "black")


par (mar = c(0,0,1.5,0), oma = c(0,3,0,0))
(gp1<-layout(matrix(c(1:5), 1, 5, byrow = T), widths=c(1,1,1), heights=c(1,1,1), respect = F))

plotRefToTarget(MEAN, WOMEN, method = "TPS", mag = 3, links = links, gridPars = GP1)
mtext("SShD", 2, cex = 1.2, font = NA)
title ("3x women", cex = 2)
plotRefToTarget(MEAN, WOMEN, method = "TPS", mag = 1, links = links, gridPars = GP1)
title("women mean", cex = 2)
plotRefToTarget(MEAN, MEAN, method = "TPS", mag = 1, links = links, gridPars = GP1)
title ("mean", cex = 2)
plotRefToTarget(MEAN, MEN, method = "TPS", mag = 1, links = links, gridPars = GP1)
title ("men mean", cex = 2)
plotRefToTarget(MEAN, MEN, method = "TPS", mag = 3, links = links, gridPars = GP1)
title ("3x men", cex = 2)


# *********************
# MIXED EFFECT MODELS
# *********************
# We can reach similar conclusions with mixed effect models

# Create a new variable with reversed sex scores for females - it is a sextypicality variable
Sextypicality <- scale(c(-1*fscores, mscores))

# Create a dataset with this variable 
dat2 <- cbind (dat, Sextypicality, SATRA)

# Complete model testing (among other things) how does the slope for males differ form the slope for females

mmodel1<- lmer(SATRA ~ Sextypicality*sex + (Sextypicality*sex|nat), data = dat2)
summary(mmodel1)

# Model restricted to the interaction term. Tests, how does the attractiveness depend on sextypicality in males and females separately.
mmodel2<- lmer(SATRA ~ 0 + Sextypicality:sex + (0 + Sextypicality:sex|nat), data = dat2)
summary(mmodel2)

# Calculate morphological variation (disparity): Morphological disparity is estimated as the Procrustes variance, overall or for groups, using residuals of a linear model fit 

mgdf <- geomorph.data.frame(coords = shapesex$M, nat = mdat$nat)
fgdf <- geomorph.data.frame(coords = shapesex$F, nat = fdat$nat)

(mdisp.m<-morphol.disparity(coords ~ 1, groups = ~nat, iter = 999, seed = NULL, data = mgdf))
(mdisp.f<-morphol.disparity(coords ~ 1, groups = ~nat, iter = 999, seed = NULL, data = fgdf))
(mdisp.1<-morphol.disparity(coords ~ 1, groups = ~nat, iter = 999, seed = NULL, data = gtf))
(mdisp.1.sex<-morphol.disparity(coords ~ sex, groups = ~nat, iter = 999, seed = NULL, data = gtf)) # residualizing sex
(mdisp.sex<-morphol.disparity(coords ~ 1, groups = ~sex, iter = 999, seed = NULL, data = gtf)) # men are more variable in facial shape
(mdisp.sex.nat<-morphol.disparity(coords ~ nat, groups = ~sex, iter = 999, seed = NULL, data = gtf)) # men are more variable in facial shape even when controlling for nationality


# We will decompose the total SShD to allometric and nonallometric components and conduct similar analyses on them
# *************************************
#        Allometric SShD
# *************************************
hdat<-dat
gtf2<-gtf

gtf2$Attractiveness<-SATRA

brazdat <- subset(hdat, nat == "BRAZ")
fbrazdat <- subset (brazdat, sex == "F")
mbrazdat <- subset (brazdat, sex == "M")

cmrdat <- subset(hdat, nat == "CMR")
fcmrdat <- subset (cmrdat, sex == "F")
mcmrdat <- subset (cmrdat, sex == "M")

coldat <- subset (hdat, nat == "COL")
fcoldat <- subset (coldat, sex == "F")
mcoldat <- subset (coldat, sex == "M")

czdat <- subset (hdat, nat == "CZ")
fczdat <- subset (czdat, sex == "F")
mczdat <- subset (czdat, sex == "M")

namdat <- subset (hdat, nat == "NAM")
fnamdat <- subset (namdat, sex == "F")
mnamdat <- subset (namdat, sex == "M")

rodat <- subset (hdat, nat == "RO")
frodat <- subset (rodat, sex == "F")
mrodat <- subset (rodat, sex == "M")

trdat <- subset (hdat, nat == "TR")
ftrdat <- subset (trdat, sex == "F")
mtrdat <- subset (trdat, sex == "M")

ukdat <- subset (hdat, nat == "UK")
fukdat <- subset (ukdat, sex == "F")
mukdat <- subset (ukdat, sex == "M")


# ALL
reg1 <- procD.lm(coords ~ height, iter = 999, data = gtf2); summary(reg1)
height.vecs<-coef(reg1, test = F)[2,]

regall <- procD.lm(reg1$fitted ~ sex, iter = 999, data = gtf2, weights = NULL); summary(regall)
coefficients<-coef(regall, test = F)
Asex.vecs<-coefficients[2,]

sexscores <- two.d.array (gtf2$coords) %*% t(coefficients)
Asc <- sexscores [,2]

fAsc <- Asc [which(gtf2$sex=="F")]
mAsc <- Asc [which(gtf2$sex=="M")]


# BRAZ
brazgtf <- geomorph.data.frame(coords = gtf2$coords[,,gtf2$nat=="BRAZ"], sex = gtf2$sex[gtf2$nat=="BRAZ"], height = gtf2$height[gtf2$nat=="BRAZ"])

reg1 <- procD.lm(coords ~ height, iter = 999, data = brazgtf, weights = NULL); summary(reg1)
height.vecs<-cbind(height.vecs,coef(reg1, test = F)[2,])

regbraz <- procD.lm(reg1$fitted ~ sex, iter = 999, data = brazgtf, weights = NULL); summary(regbraz)
coefficients<-coef(regbraz, test = F)
Asex.vecs<-cbind(Asex.vecs,coefficients[2,])

sexscores <- two.d.array (brazgtf$coords) %*% t(coefficients)
brazAsc <- sexscores [,2]

length(brazAsc)
fbrazAsc <- brazAsc [brazgtf$sex=="F"]
mbrazAsc <- brazAsc [brazgtf$sex=="M"]


# CMR
cmrgtf <- geomorph.data.frame(coords = gtf2$coords[,,gtf2$nat=="CMR"], sex = gtf2$sex[gtf2$nat=="CMR"], height = gtf2$height[gtf2$nat=="CMR"])

reg1 <- procD.lm(coords ~ height, iter = 999, data = cmrgtf, weights = NULL); summary(reg1)
height.vecs<-cbind(height.vecs,coef(reg1, test = F)[2,])

regcmr <- procD.lm(reg1$fitted ~ sex, iter = 999, data = cmrgtf, weights = NULL); summary(regcmr)
coefficients<-coef(regcmr, test = F)
Asex.vecs<-cbind(Asex.vecs,coefficients[2,])

sexscores <- two.d.array (cmrgtf$coords) %*% t(coefficients)
cmrAsc <- sexscores [,2]

length(cmrAsc)
fcmrAsc <- cmrAsc [cmrgtf$sex=="F"]
mcmrAsc <- cmrAsc [cmrgtf$sex=="M"]


# COL
colgtf <- geomorph.data.frame(coords = gtf2$coords[,,gtf2$nat=="COL"], sex = gtf2$sex[gtf2$nat=="COL"], height = gtf2$height[gtf2$nat=="COL"])

reg1 <- procD.lm(coords ~ height, iter = 999, data = colgtf, weights = NULL); summary(reg1)
height.vecs<-cbind(height.vecs,coef(reg1, test = F)[2,])

regcol <- procD.lm(reg1$fitted ~ sex, iter = 999, data = colgtf, weights = NULL); summary(regcol)
coefficients<-coef(regcol, test = F)
Asex.vecs<-cbind(Asex.vecs,coefficients[2,])

sexscores <- two.d.array (colgtf$coords) %*% t(coefficients)
colAsc <- sexscores [,2]

length(colAsc)
fcolAsc <- colAsc [colgtf$sex=="F"]
mcolAsc <- colAsc [colgtf$sex=="M"]


# CZ
czgtf <- geomorph.data.frame(coords = gtf2$coords[,,gtf2$nat=="CZ"], sex = gtf2$sex[gtf2$nat=="CZ"], height = gtf2$height[gtf2$nat=="CZ"])

reg1 <- procD.lm(coords ~ height, iter = 999, data = czgtf, weights = NULL); summary(reg1)
height.vecs<-cbind(height.vecs,coef(reg1, test = F)[2,])

regcz <- procD.lm(reg1$fitted ~ sex, iter = 999, data = czgtf, weights = NULL); summary(regcz)
coefficients<-coef(regcz, test = F)
Asex.vecs<-cbind(Asex.vecs,coefficients[2,])

sexscores <- two.d.array (czgtf$coords) %*% t(coefficients)
czAsc <- sexscores [,2]

length(czAsc)
fczAsc <- czAsc [czgtf$sex=="F"]
mczAsc <- czAsc [czgtf$sex=="M"]

# NAM
namgtf <- geomorph.data.frame(coords = gtf2$coords[,,gtf2$nat=="NAM"], sex = gtf2$sex[gtf2$nat=="NAM"], height = gtf2$height[gtf2$nat=="NAM"])

reg1 <- procD.lm(coords ~ height, iter = 999, data = namgtf, weights = NULL); summary(reg1)
height.vecs<-cbind(height.vecs,coef(reg1, test = F)[2,])

regnam <- procD.lm(reg1$fitted ~ sex, iter = 999, data = namgtf, weights = NULL); summary(regnam)
coefficients<-coef(regnam, test = F)
Asex.vecs<-cbind(Asex.vecs,coefficients[2,])

sexscores <- two.d.array (namgtf$coords) %*% t(coefficients)
namAsc <- sexscores [,2]

length(namAsc)
fnamAsc <- namAsc [namgtf$sex=="F"]
mnamAsc <- namAsc [namgtf$sex=="M"]

# RO
rogtf <- geomorph.data.frame(coords = gtf2$coords[,,gtf2$nat=="RO"], sex = gtf2$sex[gtf2$nat=="RO"], height = gtf2$height[gtf2$nat=="RO"])

reg1 <- procD.lm(coords ~ height, iter = 999, data = rogtf, weights = NULL); summary(reg1)
height.vecs<-cbind(height.vecs,coef(reg1, test = F)[2,])

regro <- procD.lm(reg1$fitted ~ sex, iter = 999, data = rogtf, weights = NULL); summary(regro)
coefficients<-coef(regro, test = F)
Asex.vecs<-cbind(Asex.vecs,coefficients[2,])

sexscores <- two.d.array (rogtf$coords) %*% t(coefficients)
roAsc <- sexscores [,2]

length(roAsc)
froAsc <- roAsc [rogtf$sex=="F"]
mroAsc <- roAsc [rogtf$sex=="M"]

# TR
trgtf <- geomorph.data.frame(coords = gtf2$coords[,,gtf2$nat=="TR"], sex = gtf2$sex[gtf2$nat=="TR"], height = gtf2$height[gtf2$nat=="TR"])

reg1 <- procD.lm(coords ~ height, iter = 999, data = trgtf, weights = NULL); summary(reg1)
height.vecs<-cbind(height.vecs,coef(reg1, test = F)[2,])

regtr <- procD.lm(reg1$fitted ~ sex, iter = 999, data = trgtf, weights = NULL); summary(regtr)
coefficients<-coef(regtr, test = F)
Asex.vecs<-cbind(Asex.vecs,coefficients[2,])

sexscores <- two.d.array (trgtf$coords) %*% t(coefficients)
trAsc <- sexscores [,2]

length(trAsc)
ftrAsc <- trAsc [trgtf$sex=="F"]
mtrAsc <- trAsc [trgtf$sex=="M"]


# UK
ukgtf <- geomorph.data.frame(coords = gtf2$coords[,,gtf2$nat=="UK"], sex = gtf2$sex[gtf2$nat=="UK"], height = gtf2$height[gtf2$nat=="UK"])

reg1 <- procD.lm(coords ~ height, iter = 999, data = ukgtf, weights = NULL); summary(reg1)
height.vecs<-cbind(height.vecs,coef(reg1, test = F)[2,])

reguk <- procD.lm(reg1$fitted ~ sex, iter = 999, data = ukgtf, weights = NULL); summary(reguk)
coefficients<-coef(reguk, test = F)
Asex.vecs<-cbind(Asex.vecs,coefficients[2,])

sexscores <- two.d.array (ukgtf$coords) %*% t(coefficients)
ukAsc <- sexscores [,2]

length(ukAsc)
fukAsc <- ukAsc [ukgtf$sex=="F"]
mukAsc <- ukAsc [ukgtf$sex=="M"]



# *************************************
#      Non-Allometric SShD
# *************************************

# ALL
reg1 <- procD.lm(coords ~ height, iter = 999, data = gtf2); summary(reg1)

regall <- procD.lm(reg1$residuals ~ sex, iter = 999, data = gtf2, weights = NULL); summary(regall)
coefficients<-coef(regall, test = F)
Nsex.vecs<-coefficients[2,]

sexscores <- two.d.array (gtf2$coords) %*% t(coefficients)
Nsc <- sexscores [,2]

fNsc <- Nsc [which(gtf2$sex=="F")]
mNsc <- Nsc [which(gtf2$sex=="M")]


# BRAZ
reg1 <- procD.lm(coords ~ height, iter = 999, data = brazgtf, weights = NULL); summary(reg1)

regbraz <- procD.lm(reg1$residuals ~ sex, iter = 999, data = brazgtf, weights = NULL); summary(regbraz)
coefficients<-coef(regbraz, test = F)
Nsex.vecs<-cbind(Nsex.vecs,coefficients[2,])

sexscores <- two.d.array (brazgtf$coords) %*% t(coefficients)
brazNsc <- sexscores [,2]

length(brazNsc)
fbrazNsc <- brazNsc [brazgtf$sex=="F"]
mbrazNsc <- brazNsc [brazgtf$sex=="M"]


# CMR
reg1 <- procD.lm(coords ~ height, iter = 999, data = cmrgtf, weights = NULL); summary(reg1)

regcmr <- procD.lm(reg1$residuals ~ sex, iter = 999, data = cmrgtf, weights = NULL); summary(regcmr)
coefficients<-coef(regcmr, test = F)
Nsex.vecs<-cbind(Nsex.vecs,coefficients[2,])

sexscores <- two.d.array (cmrgtf$coords) %*% t(coefficients)
cmrNsc <- sexscores [,2]

length(cmrNsc)
fcmrNsc <- cmrNsc [cmrgtf$sex=="F"]
mcmrNsc <- cmrNsc [cmrgtf$sex=="M"]


# COL
reg1 <- procD.lm(coords ~ height, iter = 999, data = colgtf, weights = NULL); summary(reg1)

regcol <- procD.lm(reg1$residuals ~ sex, iter = 999, data = colgtf, weights = NULL); summary(regcol)
coefficients<-coef(regcol, test = F)
Nsex.vecs<-cbind(Nsex.vecs,coefficients[2,])

sexscores <- two.d.array (colgtf$coords) %*% t(coefficients)
colNsc <- sexscores [,2]

length(colNsc)
fcolNsc <- colNsc [colgtf$sex=="F"]
mcolNsc <- colNsc [colgtf$sex=="M"]


# CZ
reg1 <- procD.lm(coords ~ height, iter = 999, data = czgtf, weights = NULL); summary(reg1)

regcz <- procD.lm(reg1$residuals ~ sex, iter = 999, data = czgtf, weights = NULL); summary(regcz)
coefficients<-coef(regcz, test = F)
Nsex.vecs<-cbind(Nsex.vecs,coefficients[2,])

sexscores <- two.d.array (czgtf$coords) %*% t(coefficients)
czNsc <- sexscores [,2]

length(czNsc)
fczNsc <- czNsc [czgtf$sex=="F"]
mczNsc <- czNsc [czgtf$sex=="M"]

# NAM
reg1 <- procD.lm(coords ~ height, iter = 999, data = namgtf, weights = NULL); summary(reg1)

regnam <- procD.lm(reg1$residuals ~ sex, iter = 999, data = namgtf, weights = NULL); summary(regnam)
coefficients<-coef(regnam, test = F)
Nsex.vecs<-cbind(Nsex.vecs,coefficients[2,])

sexscores <- two.d.array (namgtf$coords) %*% t(coefficients)
namNsc <- sexscores [,2]

length(namNsc)
fnamNsc <- namNsc [namgtf$sex=="F"]
mnamNsc <- namNsc [namgtf$sex=="M"]

# RO
reg1 <- procD.lm(coords ~ height, iter = 999, data = rogtf, weights = NULL); summary(reg1)

regro <- procD.lm(reg1$residuals ~ sex, iter = 999, data = rogtf, weights = NULL); summary(regro)
coefficients<-coef(regro, test = F)
Nsex.vecs<-cbind(Nsex.vecs,coefficients[2,])

sexscores <- two.d.array (rogtf$coords) %*% t(coefficients)
roNsc <- sexscores [,2]

length(roNsc)
froNsc <- roNsc [rogtf$sex=="F"]
mroNsc <- roNsc [rogtf$sex=="M"]

# TR
reg1 <- procD.lm(coords ~ height, iter = 999, data = trgtf, weights = NULL); summary(reg1)

regtr <- procD.lm(reg1$residuals ~ sex, iter = 999, data = trgtf, weights = NULL); summary(regtr)
coefficients<-coef(regtr, test = F)
Nsex.vecs<-cbind(Nsex.vecs,coefficients[2,])

sexscores <- two.d.array (trgtf$coords) %*% t(coefficients)
trNsc <- sexscores [,2]

length(trNsc)
ftrNsc <- trNsc [trgtf$sex=="F"]
mtrNsc <- trNsc [trgtf$sex=="M"]


# UK
reg1 <- procD.lm(coords ~ height, iter = 999, data = ukgtf, weights = NULL); summary(reg1)

reguk <- procD.lm(reg1$residuals ~ sex, iter = 999, data = ukgtf, weights = NULL); summary(reguk)
coefficients<-coef(reguk, test = F)
Nsex.vecs<-cbind(Nsex.vecs,coefficients[2,])

sexscores <- two.d.array (ukgtf$coords) %*% t(coefficients)
ukNsc <- sexscores [,2]

length(ukNsc)
fukNsc <- ukNsc [ukgtf$sex=="F"]
mukNsc <- ukNsc [ukgtf$sex=="M"]

# **************
# VioPLOTS
# **************

par (mar = c(5.1,4.1,3.1,1.1), oma = c(0,0,0,0))
(gp1<-layout(matrix(c(1), 1, 1, byrow = T), widths=c(1,1,1), heights=c(1,1,1), respect = F))
# ALLOMETRIC

# shows real range in SShD
fsexscoresA <- c(fcmrAsc, fczAsc, ftrAsc, fnamAsc, froAsc, fukAsc, fcolAsc, fbrazAsc)
msexscoresA <- c(mcmrAsc, mczAsc, mtrAsc, mnamAsc, mroAsc, mukAsc, mcolAsc, mbrazAsc)
sexscoresA<- c(fsexscoresA, msexscoresA)

vioplot(fsexscoresA~gtf2$nat[gtf2$sex=="F"], range = 1.5, horizontal = F, col = "#FF770080", plotCentre = "point", ylim=c(-0.002, 0.0024), xlab="Country",ylab="SShD")
vioplot(msexscoresA~gtf2$nat[gtf2$sex=="M"], range = 1.5, horizontal = F, col = "#3366FF80", plotCentre = "point",add=T,xlab="",ylab="")
vioplot(sexscoresA~gtf2$nat, range = 1.5, horizontal = F, col = "#FFFFFF00", lineCol = NA,add=T,xlab="",ylab="",border="#FF0066",lwd=1.8,rectCol=NA,pchMed=NA)
title (main = "Allometric Sexual Shape Dimorphism")


vioplot(sexscoresA~gtf2$sex, range = 1.5, horizontal = F, col = "gold", plotCentre = "point")


# NON-ALLOMETRIC

fsexscoresN <- c(fcmrNsc, fczNsc, ftrNsc, fnamNsc, froNsc, fukNsc, fcolNsc, fbrazNsc)
msexscoresN <- c(mcmrNsc, mczNsc, mtrNsc, mnamNsc, mroNsc, mukNsc, mcolNsc, mbrazNsc)
sexscoresN <- c(fsexscoresN, msexscoresN)

vioplot(fsexscoresN~gtf2$nat[gtf2$sex=="F"], range = 1.5, horizontal = F, col = "#FF770080", plotCentre = "point", ylim=c(-0.0018, 0.0023), xlab="Country",ylab="SShD")
vioplot(msexscoresN~gtf2$nat[gtf2$sex=="M"], range = 1.5, horizontal = F, col = "#3366FF80", plotCentre = "point",add=T,xlab="",ylab="")
vioplot(sexscoresN~gtf2$nat, range = 1.5, horizontal = F, col = "#FFFFFF00", lineCol = NA,add=T,xlab="",ylab="",border="#FF0066",lwd=1.8,rectCol=NA,pchMed=NA)
title (main = "Nonallometric Sexual Shape Dimorphism", outer = F)

vioplot(sexscoresN~gtf2$sex, range = 1.5, horizontal = F, col = "gold", plotCentre = "point")

# HEIGHT PLOTS

vioplot(gtf2$height[gtf2$sex=="F"]~gtf2$nat[gtf2$sex=="F"], range = 1.5, horizontal = F, col = "#FF770080", plotCentre = "point", ylim = c(142, 202), xlab="Country",ylab="Height")
vioplot(gtf2$height[gtf2$sex=="M"]~gtf2$nat[gtf2$sex=="M"], range = 1.5, horizontal = F, col = "#3366FF80", plotCentre = "point",add=T,xlab="",ylab="")
vioplot(gtf2$height~gtf2$nat, range = 1.5, horizontal = F, col = "#FFFFFF00", lineCol = NA,add=T,xlab="",ylab="",border="darkgreen",lwd=1.8, rectCol=NA,pchMed=NA)
title (main = "Height")

# Attractiveness plots
vioplot(gtf2$Attractiveness[gtf2$sex=="F"]~gtf2$nat[gtf2$sex=="F"], range = 1.5, horizontal = F, col = "#FF770080", plotCentre = "point",ylim = c(-3, 4), xlab="Country",ylab="Attractiveness")
vioplot(gtf2$Attractiveness[gtf2$sex=="M"]~gtf2$nat[gtf2$sex=="M"], range = 1.5, horizontal = F, col = "#3366FF80", plotCentre = "point",add=T,xlab="",ylab="")
vioplot(gtf2$Attractiveness~gtf2$nat, range = 1.5, horizontal = F, col = "#FFFFFF00", lineCol = NA,add=T,xlab="",ylab="",border="darkgreen",lwd=1.8, rectCol=NA,pchMed=NA)
title (main = "Attractiveness")



# ********************************************
# Correlation beween SShD scores and ratings
# ********************************************

# ALLOMETRIC SShD
# We investigate the correlation between Allometric maleness-femaleness and attractivenes in each subset in the same waz as we did in the previous generic maleness-femaleness case.

# BRAZ
(fp<-with(fbrazdat, pcor.test(Attractiveness, -1*fbrazAsc, age)))
(fc<-with(fbrazdat, cor.test(Attractiveness, -1*fbrazAsc)))

(mp<-with(mbrazdat, pcor.test(Attractiveness, mbrazAsc, age)))
(mc<-with(mbrazdat, cor.test(Attractiveness, mbrazAsc)))

Allometric_SShD <- c(-1*fbrazAsc, mbrazAsc)
brazdat1 <- cbind (brazdat, Allometric_SShD)

brazdat1$Attractiveness <-scale(brazdat1$Attractiveness)
model<-lm(Attractiveness~sex*Allometric_SShD, data=brazdat1); summary(model)

DF.pred<-expand.grid(Allometric_SShD=seq(min(Allometric_SShD),max(Allometric_SShD),0.0001),sex=c("F","M"))
PRED<-predict.lm(model,newdata = DF.pred, type="response",interval="confidence")
gg.predict<-ggplot(data.frame(DF.pred,PRED),aes(y = fit,x=Allometric_SShD,group=sex))+
  geom_line(data=data.frame(DF.pred,PRED),aes(y = fit,x=Allometric_SShD,group=sex,color=sex))+
  geom_ribbon(data=data.frame(DF.pred,PRED),aes(ymin=lwr, ymax=upr, x=Allometric_SShD, fill = sex,group=sex), alpha = 0.2)+ geom_point(data=brazdat1,aes(x=Allometric_SShD,y=Attractiveness,colour=sex, group=sex),alpha=0.5)+
  theme_bw(base_size = fontsize)

gg.predict <- gg.predict + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())

brazA<-gg.predict+ scale_color_discrete(labels=c("Women", "Men"))+
  scale_fill_discrete(labels=c("Women", "Men"))+
  scale_y_continuous(name="Attractiveness")+
  scale_x_continuous(name="Allometric Sextypicality")

brazA


# CMR
(fp<-with(fcmrdat, pcor.test(Attractiveness, -1*fcmrAsc, age)))
(fc<-with(fcmrdat, cor.test(Attractiveness, -1*fcmrAsc)))

(mp<-with(mcmrdat, pcor.test(Attractiveness, mcmrAsc, age)))
(mc<-with(mcmrdat, cor.test(Attractiveness, mcmrAsc)))

Allometric_SShD <- c(-1*fcmrAsc, mcmrAsc)
cmrdat1 <- cbind (cmrdat, Allometric_SShD)

cmrdat1$Attractiveness <-scale(cmrdat1$Attractiveness)

model<-lm(Attractiveness~sex*Allometric_SShD, data=cmrdat1); summary(model)

DF.pred<-expand.grid(Allometric_SShD=seq(min(Allometric_SShD),max(Allometric_SShD),0.0001),sex=c("F","M"))
PRED<-predict.lm(model,newdata = DF.pred, type="response",interval="confidence")
gg.predict<-ggplot(data.frame(DF.pred,PRED),aes(y = fit,x=Allometric_SShD,group=sex))+
  geom_line(data=data.frame(DF.pred,PRED),aes(y = fit,x=Allometric_SShD,group=sex,color=sex))+
  geom_ribbon(data=data.frame(DF.pred,PRED),aes(ymin=lwr, ymax=upr, x=Allometric_SShD, fill = sex,group=sex), alpha = 0.2)+ geom_point(data=cmrdat1,aes(x=Allometric_SShD,y=Attractiveness,colour=sex, group=sex),alpha=0.5)+
  theme_bw(base_size = fontsize)

gg.predict <- gg.predict + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())

cmrA<-gg.predict + scale_color_discrete(labels=c("Women", "Men"))+
  scale_fill_discrete(labels=c("Women", "Men"))+
  scale_y_continuous(name="Attractiveness")+
  scale_x_continuous(name="Allometric Sextypicality")

cmrA


# COL
(fp<-with(fcoldat, pcor.test(Attractiveness, -1*fcolAsc, age)))
(fc<-with(fcoldat, cor.test(Attractiveness, -1*fcolAsc)))

(mp<-with(mcoldat, pcor.test(Attractiveness, mcolAsc, age)))
(mc<-with(mcoldat, cor.test(Attractiveness, mcolAsc)))

Allometric_SShD <- c(-1*fcolAsc, mcolAsc)
coldat1 <- cbind (coldat, Allometric_SShD)

coldat1$Attractiveness <-scale(coldat1$Attractiveness)
model<-lm(Attractiveness~sex*Allometric_SShD, data=coldat1); summary(model)

DF.pred<-expand.grid(Allometric_SShD=seq(min(Allometric_SShD),max(Allometric_SShD),0.0001),sex=c("F","M"))
PRED<-predict.lm(model,newdata = DF.pred, type="response",interval="confidence")
gg.predict<-ggplot(data.frame(DF.pred,PRED),aes(y = fit,x=Allometric_SShD,group=sex))+
  geom_line(data=data.frame(DF.pred,PRED),aes(y = fit,x=Allometric_SShD,group=sex,color=sex))+
  geom_ribbon(data=data.frame(DF.pred,PRED),aes(ymin=lwr, ymax=upr, x=Allometric_SShD, fill = sex,group=sex), alpha = 0.2)+ geom_point(data=coldat1,aes(x=Allometric_SShD,y=Attractiveness,colour=sex, group=sex),alpha=0.5)+
  theme_bw(base_size = fontsize)

gg.predict <- gg.predict + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())

colA<-gg.predict+ scale_color_discrete(labels=c("Women", "Men"))+
  scale_fill_discrete(labels=c("Women", "Men"))+
  scale_y_continuous(name="Attractiveness")+
  scale_x_continuous(name="Allometric Sextypicality")

colA

# CZ
(fp<-with(fczdat, pcor.test(Attractiveness, -1*fczAsc, age)))
(fc<-with(fczdat, cor.test(Attractiveness, -1*fczAsc)))

(mp<-with(mczdat, pcor.test(Attractiveness, mczAsc, age)))
(mc<-with(mczdat, cor.test(Attractiveness, mczAsc)))

Allometric_SShD <- c(-1*fczAsc, mczAsc)
czdat1 <- cbind (czdat, Allometric_SShD)

czdat1$Attractiveness <-scale(czdat1$Attractiveness)

model<-lm(Attractiveness~sex*Allometric_SShD, data=czdat1); summary(model)

DF.pred<-expand.grid(Allometric_SShD=seq(min(Allometric_SShD),max(Allometric_SShD),0.0001),sex=c("F","M"))
PRED<-predict.lm(model,newdata = DF.pred, type="response",interval="confidence")
gg.predict<-ggplot(data.frame(DF.pred,PRED),aes(y = fit,x=Allometric_SShD,group=sex))+
  geom_line(data=data.frame(DF.pred,PRED),aes(y = fit,x=Allometric_SShD,group=sex,color=sex))+
  geom_ribbon(data=data.frame(DF.pred,PRED),aes(ymin=lwr, ymax=upr, x=Allometric_SShD, fill = sex,group=sex), alpha = 0.2)+ geom_point(data=czdat1,aes(x=Allometric_SShD,y=Attractiveness,colour=sex, group=sex),alpha=0.5)+
  theme_bw(base_size = fontsize)

gg.predict <- gg.predict + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())

czA<-gg.predict+ scale_color_discrete(labels=c("Women", "Men"))+
  scale_fill_discrete(labels=c("Women", "Men"))+
  scale_y_continuous(name="Attractiveness")+
  scale_x_continuous(name="Allometric Sextypicality")

czA


# NAM
(fp<-with(fnamdat, pcor.test(Attractiveness, -1*fnamAsc, age)))
(fc<-with(fnamdat, cor.test(Attractiveness, -1*fnamAsc)))

(mp<-with(mnamdat, pcor.test(Attractiveness, mnamAsc, age)))
(mc<-with(mnamdat, cor.test(Attractiveness, mnamAsc)))

Allometric_SShD <- c(-1*fnamAsc, mnamAsc)
namdat1 <- cbind (namdat, Allometric_SShD)

namdat1$Attractiveness <-scale(namdat1$Attractiveness)
model<-lm(Attractiveness~sex*Allometric_SShD, data=namdat1); summary(model)

DF.pred<-expand.grid(Allometric_SShD=seq(min(Allometric_SShD),max(Allometric_SShD),0.0001),sex=c("F","M"))
PRED<-predict.lm(model,newdata = DF.pred, type="response",interval="confidence")
gg.predict<-ggplot(data.frame(DF.pred,PRED),aes(y = fit,x=Allometric_SShD,group=sex))+
  geom_line(data=data.frame(DF.pred,PRED),aes(y = fit,x=Allometric_SShD,group=sex,color=sex))+
  geom_ribbon(data=data.frame(DF.pred,PRED),aes(ymin=lwr, ymax=upr, x=Allometric_SShD, fill = sex,group=sex), alpha = 0.2)+ geom_point(data=namdat1,aes(x=Allometric_SShD,y=Attractiveness,colour=sex, group=sex),alpha=0.5)+
  theme_bw(base_size = fontsize)

gg.predict <- gg.predict + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())

namA<-gg.predict+ scale_color_discrete(labels=c("Women", "Men"))+
  scale_fill_discrete(labels=c("Women", "Men"))+
  scale_y_continuous(name="Attractiveness")+
  scale_x_continuous(name="Allometric Sextypicality")

namA



# RO
(fp<-with(frodat, pcor.test(Attractiveness, -1*froAsc, age)))
(fc<-with(frodat, cor.test(Attractiveness, -1*froAsc)))

(mp<-with(mrodat, pcor.test(Attractiveness, mroAsc, age)))
(mc<-with(mrodat, cor.test(Attractiveness, mroAsc)))

Allometric_SShD <- c(-1*froAsc, mroAsc)
rodat1 <- cbind (rodat, Allometric_SShD)

rodat1$Attractiveness <-scale(rodat1$Attractiveness)
model<-lm(Attractiveness~sex*Allometric_SShD, data=rodat1); summary(model)

DF.pred<-expand.grid(Allometric_SShD=seq(min(Allometric_SShD),max(Allometric_SShD),0.0001),sex=c("F","M"))
PRED<-predict.lm(model,newdata = DF.pred, type="response",interval="confidence")
gg.predict<-ggplot(data.frame(DF.pred,PRED),aes(y = fit,x=Allometric_SShD,group=sex))+
  geom_line(data=data.frame(DF.pred,PRED),aes(y = fit,x=Allometric_SShD,group=sex,color=sex))+
  geom_ribbon(data=data.frame(DF.pred,PRED),aes(ymin=lwr, ymax=upr, x=Allometric_SShD, fill = sex,group=sex), alpha = 0.2)+ geom_point(data=rodat1,aes(x=Allometric_SShD,y=Attractiveness,colour=sex, group=sex),alpha=0.5)+
  theme_bw(base_size = fontsize)

gg.predict <- gg.predict + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())

roA<-gg.predict+ scale_color_discrete(labels=c("Women", "Men"))+
  scale_fill_discrete(labels=c("Women", "Men"))+
  scale_y_continuous(name="Attractiveness")+
  scale_x_continuous(name="Allometric Sextypicality")

roA


# TR
(fp<-with(ftrdat, pcor.test(Attractiveness, -1*ftrAsc, age)))
(fc<-with(ftrdat, cor.test(Attractiveness, -1*ftrAsc)))

(mp<-with(mtrdat, pcor.test(Attractiveness, mtrAsc, age)))
(mc<-with(mtrdat, cor.test(Attractiveness, mtrAsc)))

Allometric_SShD <- c(-1*ftrAsc, mtrAsc)
trdat1 <- cbind (trdat, Allometric_SShD)

trdat1$Attractiveness <-scale(trdat1$Attractiveness)

model<-lm(Attractiveness~sex*Allometric_SShD, data=trdat1); summary(model)

DF.pred<-expand.grid(Allometric_SShD=seq(min(Allometric_SShD),max(Allometric_SShD),0.00001),sex=c("F","M"))
PRED<-predict.lm(model,newdata = DF.pred, type="response",interval="confidence")
gg.predict<-ggplot(data.frame(DF.pred,PRED),aes(y = fit,x=Allometric_SShD,group=sex))+
  geom_line(data=data.frame(DF.pred,PRED),aes(y = fit,x=Allometric_SShD,group=sex,color=sex))+
  geom_ribbon(data=data.frame(DF.pred,PRED),aes(ymin=lwr, ymax=upr, x=Allometric_SShD, fill = sex,group=sex), alpha = 0.2)+ geom_point(data=trdat1,aes(x=Allometric_SShD,y=Attractiveness,colour=sex, group=sex),alpha=0.5)+
  theme_bw(base_size = fontsize)

gg.predict <- gg.predict + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())

trA<-gg.predict+ scale_color_discrete(labels=c("Women", "Men"))+
  scale_fill_discrete(labels=c("Women", "Men"))+
  scale_y_continuous(name="Attractiveness")+
  scale_x_continuous(name="Allometric Sextypicality")

trA


#UK
(fp<-with(fukdat, pcor.test(Attractiveness, -1*fukAsc, age)))
(fc<-with(fukdat, cor.test(Attractiveness, -1*fukAsc)))

(mp<-with(mukdat, pcor.test(Attractiveness, mukAsc, age)))
(mc<-with(mukdat, cor.test(Attractiveness, mukAsc)))

Allometric_SShD <- c(-1*fukAsc, mukAsc)
ukdat1 <- cbind (ukdat, Allometric_SShD)

ukdat1$Attractiveness <-scale(ukdat1$Attractiveness)

model<-lm(Attractiveness~sex*Allometric_SShD, data=ukdat1); summary(model)

DF.pred<-expand.grid(Allometric_SShD=seq(min(Allometric_SShD),max(Allometric_SShD),0.00001),sex=c("F","M"))
PRED<-predict.lm(model,newdata = DF.pred, type="response",interval="confidence")
gg.predict<-ggplot(data.frame(DF.pred,PRED),aes(y = fit,x=Allometric_SShD,group=sex))+
  geom_line(data=data.frame(DF.pred,PRED),aes(y = fit,x=Allometric_SShD,group=sex,color=sex))+
  geom_ribbon(data=data.frame(DF.pred,PRED),aes(ymin=lwr, ymax=upr, x=Allometric_SShD, fill = sex,group=sex), alpha = 0.2)+ geom_point(data=ukdat1,aes(x=Allometric_SShD,y=Attractiveness,colour=sex, group=sex),alpha=0.5)+
  theme_bw(base_size = fontsize)

gg.predict <- gg.predict + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())

ukA<-gg.predict+ scale_color_discrete(labels=c("Women", "Men"))+
  scale_fill_discrete(labels=c("Women", "Men"))+
  scale_y_continuous(name="Attractiveness")+
  scale_x_continuous(name="Allometric Sextypicality")

ukA


# *******************************************
# NON-ALLOMETRIC SShD

# BRAZ
(fp<-with(fbrazdat, pcor.test(Attractiveness, -1*fbrazNsc, age)))
(fc<-with(fbrazdat, cor.test(Attractiveness, -1*fbrazNsc)))

(mp<-with(mbrazdat, pcor.test(Attractiveness, mbrazNsc, age)))
(mc<-with(mbrazdat, cor.test(Attractiveness, mbrazNsc)))

Nonallometric_SShD <- c(-1*fbrazNsc, mbrazNsc)
brazdat1 <- cbind (brazdat, Nonallometric_SShD)

brazdat1$Attractiveness <-scale(brazdat1$Attractiveness)

model<-lm(Attractiveness~sex*Nonallometric_SShD, data=brazdat1); summary(model)

DF.pred<-expand.grid(Nonallometric_SShD=seq(min(Nonallometric_SShD),max(Nonallometric_SShD),0.0001),sex=c("F","M"))
PRED<-predict.lm(model,newdata = DF.pred, type="response",interval="confidence")
gg.predict<-ggplot(data.frame(DF.pred,PRED),aes(y = fit,x=SShD,group=sex))+
  geom_line(data=data.frame(DF.pred,PRED),aes(y = fit,x=Nonallometric_SShD,group=sex,color=sex))+
  geom_ribbon(data=data.frame(DF.pred,PRED),aes(ymin=lwr, ymax=upr, x=Nonallometric_SShD, fill = sex,group=sex), alpha = 0.2)+ geom_point(data=brazdat1,aes(x=Nonallometric_SShD,y=Attractiveness,colour=sex, group=sex),alpha=0.5)+
  theme_bw(base_size = fontsize)

gg.predict <- gg.predict + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())

brazN<-gg.predict+ scale_color_discrete(labels=c("Women", "Men"))+
  scale_fill_discrete(labels=c("Women", "Men"))+
  scale_y_continuous(name="Attractiveness")+
  scale_x_continuous(name="Nonallometric Sextypicality")

brazN



# CMR
(fp<-with(fcmrdat, pcor.test(Attractiveness, -1*fcmrNsc, age)))
(fc<-with(fcmrdat, cor.test(Attractiveness, -1*fcmrNsc)))

(mp<-with(mcmrdat, pcor.test(Attractiveness, mcmrNsc, age)))
(mc<-with(mcmrdat, cor.test(Attractiveness, mcmrNsc)))

Nonallometric_SShD <- c(-1*fcmrNsc, mcmrNsc)
cmrdat1 <- cbind (cmrdat, Nonallometric_SShD)

cmrdat1$Attractiveness <-scale(cmrdat1$Attractiveness)

model<-lm(Attractiveness~sex*Nonallometric_SShD, data=cmrdat1); summary(model)

DF.pred<-expand.grid(Nonallometric_SShD=seq(min(Nonallometric_SShD),max(Nonallometric_SShD),0.00001),sex=c("F","M"))
PRED<-predict.lm(model,newdata = DF.pred, type="response",interval="confidence")
gg.predict<-ggplot(data.frame(DF.pred,PRED),aes(y = fit,x=Nonallometric_SShD,group=sex))+
  geom_line(data=data.frame(DF.pred,PRED),aes(y = fit,x=Nonallometric_SShD,group=sex,color=sex))+
  geom_ribbon(data=data.frame(DF.pred,PRED),aes(ymin=lwr, ymax=upr, x=Nonallometric_SShD, fill = sex,group=sex), alpha = 0.2)+ geom_point(data=cmrdat1,aes(x=Nonallometric_SShD,y=Attractiveness,colour=sex, group=sex),alpha=0.5)+
  theme_bw(base_size = fontsize)

gg.predict <- gg.predict + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())

cmrN<-gg.predict+ scale_color_discrete(labels=c("Women", "Men"))+
  scale_fill_discrete(labels=c("Women", "Men"))+
  scale_y_continuous(name="Attractiveness")+
  scale_x_continuous(name="Nonallometric Sextypicality")

cmrN


# COL

(fp<-with(fcoldat, pcor.test(Attractiveness, -1*fcolNsc, age)))
(fc<-with(fcoldat, cor.test(Attractiveness, -1*fcolNsc)))

(mp<-with(mcoldat, pcor.test(Attractiveness, mcolNsc, age)))
(mc<-with(mcoldat, cor.test(Attractiveness, mcolNsc)))

Nonallometric_SShD <- c(-1*fcolNsc, mcolNsc)
coldat1 <- cbind (coldat, Nonallometric_SShD)

coldat1$Attractiveness <-scale(coldat1$Attractiveness)

model<-lm(Attractiveness~sex*Nonallometric_SShD, data=coldat1); summary(model)

DF.pred<-expand.grid(Nonallometric_SShD=seq(min(Nonallometric_SShD),max(Nonallometric_SShD),0.0001),sex=c("F","M"))
PRED<-predict.lm(model,newdata = DF.pred, type="response",interval="confidence")
gg.predict<-ggplot(data.frame(DF.pred,PRED),aes(y = fit,x=Nonallometric_SShD,group=sex))+
  geom_line(data=data.frame(DF.pred,PRED),aes(y = fit,x=Nonallometric_SShD,group=sex,color=sex))+
  geom_ribbon(data=data.frame(DF.pred,PRED),aes(ymin=lwr, ymax=upr, x=Nonallometric_SShD, fill = sex,group=sex), alpha = 0.2)+ geom_point(data=coldat1,aes(x=Nonallometric_SShD,y=Attractiveness,colour=sex, group=sex),alpha=0.5)+
  theme_bw(base_size = fontsize)

gg.predict <- gg.predict + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())

colN<-gg.predict+ scale_color_discrete(labels=c("Women", "Men"))+
  scale_fill_discrete(labels=c("Women", "Men"))+
  scale_y_continuous(name="Attractiveness")+
  scale_x_continuous(name="Nonallometric Sextypicality")

colN


# CZ
(fp<-with(fczdat, pcor.test(Attractiveness, -1*fczNsc, age)))
(fc<-with(fczdat, cor.test(Attractiveness, -1*fczNsc)))

(mp<-with(mczdat, pcor.test(Attractiveness, mczNsc, age)))
(mc<-with(mczdat, cor.test(Attractiveness, mczNsc)))

Nonallometric_SShD <- c(-1*fczNsc, mczNsc)
czdat1 <- cbind (czdat, Nonallometric_SShD)

czdat1$Attractiveness <-scale(czdat1$Attractiveness)

model<-lm(Attractiveness~sex*Nonallometric_SShD, data=czdat1); summary(model)

DF.pred<-expand.grid(Nonallometric_SShD=seq(min(Nonallometric_SShD),max(Nonallometric_SShD),0.0001),sex=c("F","M"))
PRED<-predict.lm(model,newdata = DF.pred, type="response",interval="confidence")
gg.predict<-ggplot(data.frame(DF.pred,PRED),aes(y = fit,x=Nonallometric_SShD,group=sex))+
  geom_line(data=data.frame(DF.pred,PRED),aes(y = fit,x=Nonallometric_SShD,group=sex,color=sex))+
  geom_ribbon(data=data.frame(DF.pred,PRED),aes(ymin=lwr, ymax=upr, x=Nonallometric_SShD, fill = sex,group=sex), alpha = 0.2)+ geom_point(data=czdat1,aes(x=Nonallometric_SShD,y=Attractiveness,colour=sex, group=sex),alpha=0.5)+
  theme_bw(base_size = fontsize)

gg.predict <- gg.predict + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())

czN<-gg.predict+ scale_color_discrete(labels=c("Women", "Men"))+
  scale_fill_discrete(labels=c("Women", "Men"))+
  scale_y_continuous(name="Attractiveness")+
  scale_x_continuous(name="Nonallometric Sextypicality")

czN

# NAM
(fp<-with(fnamdat, pcor.test(Attractiveness, -1*fnamNsc, age)))
(fc<-with(fnamdat, cor.test(Attractiveness, -1*fnamNsc)))

(mp<-with(mnamdat, pcor.test(Attractiveness, mnamNsc, age)))
(mc<-with(mnamdat, cor.test(Attractiveness, mnamNsc)))

Nonallometric_SShD <- c(-1*fnamNsc, mnamNsc)
namdat1 <- cbind (namdat, Nonallometric_SShD)

namdat1$Attractiveness <-scale(namdat1$Attractiveness)

model<-lm(Attractiveness~sex*Nonallometric_SShD, data=namdat1); summary(model)

DF.pred<-expand.grid(Nonallometric_SShD=seq(min(Nonallometric_SShD),max(Nonallometric_SShD), 0.00001), sex=c("F","M"))
PRED<-predict.lm(model,newdata = DF.pred, type="response",interval="confidence")
gg.predict<-ggplot(data.frame(DF.pred,PRED),aes(y = fit,x=Nonallometric_SShD,group=sex))+
  geom_line(data=data.frame(DF.pred,PRED),aes(y = fit,x=Nonallometric_SShD,group=sex,color=sex))+
  geom_ribbon(data=data.frame(DF.pred,PRED),aes(ymin=lwr, ymax=upr, x=Nonallometric_SShD, fill = sex,group=sex), alpha = 0.2)+ geom_point(data=namdat1,aes(x=Nonallometric_SShD,y=Attractiveness,colour=sex, group=sex),alpha=0.5)+
  theme_bw(base_size = fontsize)

gg.predict <- gg.predict + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())

namN<-gg.predict+ scale_color_discrete(labels=c("Women", "Men"))+
  scale_fill_discrete(labels=c("Women", "Men"))+
  scale_y_continuous(name="Attractiveness")+
  scale_x_continuous(name="Nonallometric Sextypicality")

namN


# RO
(fp<-with(frodat, pcor.test(Attractiveness, -1*froNsc, age)))
(fc<-with(frodat, cor.test(Attractiveness, -1*froNsc)))

(mp<-with(mrodat, pcor.test(Attractiveness, mroNsc, age)))
(mc<-with(mrodat, cor.test(Attractiveness, mroNsc)))

Nonallometric_SShD <- c(-1*froNsc, mroNsc)
rodat1 <- cbind (rodat, Nonallometric_SShD)

rodat1$Attractiveness <-scale(rodat1$Attractiveness)

model<-lm(Attractiveness~sex*Nonallometric_SShD, data=rodat1); summary(model)

DF.pred<-expand.grid(Nonallometric_SShD=seq(min(Nonallometric_SShD),max(Nonallometric_SShD), 0.00001), sex=c("F","M"))
PRED<-predict.lm(model,newdata = DF.pred, type="response",interval="confidence")
gg.predict<-ggplot(data.frame(DF.pred,PRED),aes(y = fit,x=Nonallometric_SShD,group=sex))+
  geom_line(data=data.frame(DF.pred,PRED),aes(y = fit,x=Nonallometric_SShD,group=sex,color=sex))+
  geom_ribbon(data=data.frame(DF.pred,PRED),aes(ymin=lwr, ymax=upr, x=Nonallometric_SShD, fill = sex,group=sex), alpha = 0.2)+ geom_point(data=rodat1,aes(x=Nonallometric_SShD,y=Attractiveness,colour=sex, group=sex),alpha=0.5)+
  theme_bw(base_size = fontsize)

gg.predict <- gg.predict + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())

roN<-gg.predict+ scale_color_discrete(labels=c("Women", "Men"))+
  scale_fill_discrete(labels=c("Women", "Men"))+
  scale_y_continuous(name="Attractiveness")+
  scale_x_continuous(name="Nonallometric Sextypicality")

roN


# TR
(fp<-with(ftrdat, pcor.test(Attractiveness, -1*ftrNsc, age)))
(fc<-with(ftrdat, cor.test(Attractiveness, -1*ftrNsc)))

(mp<-with(mtrdat, pcor.test(Attractiveness, mtrNsc, age)))
(mc<-with(mtrdat, cor.test(Attractiveness, mtrNsc)))

Nonallometric_SShD <- c(-1*ftrNsc, mtrNsc)
trdat1 <- cbind (trdat, Nonallometric_SShD)

trdat1$Attractiveness <-scale(trdat1$Attractiveness)
model<-lm(Attractiveness~sex*Nonallometric_SShD, data=trdat1); summary(model)

DF.pred<-expand.grid(Nonallometric_SShD=seq(min(Nonallometric_SShD),max(Nonallometric_SShD),0.0001),sex=c("F","M"))
PRED<-predict.lm(model,newdata = DF.pred, type="response",interval="confidence")
gg.predict<-ggplot(data.frame(DF.pred,PRED),aes(y = fit,x=Nonallometric_SShD,group=sex))+
  geom_line(data=data.frame(DF.pred,PRED),aes(y = fit,x=Nonallometric_SShD,group=sex,color=sex))+
  geom_ribbon(data=data.frame(DF.pred,PRED),aes(ymin=lwr, ymax=upr, x=Nonallometric_SShD, fill = sex,group=sex), alpha = 0.2)+ geom_point(data=trdat1,aes(x=Nonallometric_SShD,y=Attractiveness,colour=sex, group=sex),alpha=0.5)+
  theme_bw(base_size = fontsize)

gg.predict <- gg.predict + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())

trN<-gg.predict+ scale_color_discrete(labels=c("Women", "Men"))+
  scale_fill_discrete(labels=c("Women", "Men"))+
  scale_y_continuous(name="Attractiveness")+
  scale_x_continuous(name="Nonallometric Sextypicality")

trN


# UK
(fp<-with(fukdat, pcor.test(Attractiveness, -1*fukNsc, age)))
(fc<-with(fukdat, cor.test(Attractiveness, -1*fukNsc)))

(mp<-with(mukdat, pcor.test(Attractiveness, mukNsc, age)))
(mc<-with(mukdat, cor.test(Attractiveness, mukNsc)))

Nonallometric_SShD <- c(-1*fukNsc, mukNsc)
ukdat1 <- cbind (ukdat, Nonallometric_SShD)

ukdat1$Attractiveness <-scale(ukdat1$Attractiveness)

model<-lm(Attractiveness~sex*Nonallometric_SShD, data=ukdat1); summary(model)

DF.pred<-expand.grid(Nonallometric_SShD=seq(min(Nonallometric_SShD),max(Nonallometric_SShD),0.0001),sex=c("F","M"))
PRED<-predict.lm(model,newdata = DF.pred, type="response",interval="confidence")
gg.predict<-ggplot(data.frame(DF.pred,PRED),aes(y = fit,x=SShD,group=sex))+
  geom_line(data=data.frame(DF.pred,PRED),aes(y = fit,x=Nonallometric_SShD,group=sex,color=sex))+
  geom_ribbon(data=data.frame(DF.pred,PRED),aes(ymin=lwr, ymax=upr, x=Nonallometric_SShD, fill = sex,group=sex), alpha = 0.2)+ geom_point(data=ukdat1,aes(x=Nonallometric_SShD,y=Attractiveness,colour=sex, group=sex),alpha=0.5)+
  theme_bw(base_size = fontsize)

gg.predict <- gg.predict + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())

ukN<-gg.predict+ scale_color_discrete(labels=c("Women", "Men"))+
  scale_fill_discrete(labels=c("Women", "Men"))+
  scale_y_continuous(name="Attractiveness")+
  scale_x_continuous(name="Nonallometric Sextypicality")

ukN



# -----ALTOGETHER------
# ALLO

(fp<-with(fdat, pcor.test(Attractiveness, -1*fAsc, age)))
(fc<-with(fdat, cor.test(Attractiveness, -1*fAsc)))

(mp<-with(mdat, pcor.test(Attractiveness, -1*mAsc, age)))
(mc<-with(mdat, cor.test(Attractiveness, -1*mAsc)))

Allometric_SShD <- c(-1*fAsc, mAsc)
dat1 <- cbind (hdat, Allometric_SShD)

model<-lm(Attractiveness~sex*Allometric_SShD, data=dat1); summary(model)

DF.pred<-expand.grid(Allometric_SShD=seq(min(Allometric_SShD),max(Allometric_SShD),0.00001),sex=c("F","M"))
PRED<-predict.lm(model,newdata = DF.pred, type="response",interval="confidence")
gg.predict<-ggplot(data.frame(DF.pred,PRED),aes(y = fit, x = Allometric_SShD, group=sex))+
  geom_line(data=data.frame(DF.pred,PRED),aes(y = fit,x=Allometric_SShD,group=sex,color=sex))+
  geom_ribbon(data=data.frame(DF.pred,PRED),aes(ymin=lwr, ymax=upr, x=Allometric_SShD, fill = sex,group=sex), alpha = 0.2)+ geom_point(data=dat1,aes(x=Allometric_SShD,y=Attractiveness,colour=sex, group=sex),alpha=0.05)+
  theme_bw(base_size = fontsize)

gg.predict <- gg.predict + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(),panel.border = element_blank())

allA<-gg.predict + scale_color_discrete(labels=c("Women", "Men"))+
  scale_fill_discrete(labels=c("Women", "Men"))+
  scale_y_continuous(name="Attractiveness")+
  scale_x_continuous(name="Allometric Sextypicality")

allA


# NON_ALLO

(fp<-with(fdat, pcor.test(Attractiveness, -1*fNsc, age)))
(fc<-with(fdat, cor.test(Attractiveness, -1*fNsc)))

(mp<-with(mdat, pcor.test(Attractiveness, -1*mNsc, age)))
(mc<-with(mdat, cor.test(Attractiveness, -1*mNsc)))

Nonallometric_SShD <- c(-1*fNsc, mNsc)
dat1 <- cbind (hdat, Nonallometric_SShD)

model<-lm(Attractiveness~sex*Nonallometric_SShD, data=dat1); summary(model)

DF.pred<-expand.grid(Nonallometric_SShD=seq(min(Nonallometric_SShD),max(Nonallometric_SShD),0.00001),sex=c("F","M"))
PRED<-predict.lm(model,newdata = DF.pred, type="response",interval="confidence")
gg.predict<-ggplot(data.frame(DF.pred,PRED),aes(y = fit, x = Nonallometric_SShD, group=sex))+
  geom_line(data=data.frame(DF.pred,PRED),aes(y = fit,x=Nonallometric_SShD,group=sex,color=sex))+
  geom_ribbon(data=data.frame(DF.pred,PRED),aes(ymin=lwr, ymax=upr, x=Nonallometric_SShD, fill = sex,group=sex), alpha = 0.2)+ geom_point(data=dat1,aes(x=Nonallometric_SShD,y=Attractiveness,colour=sex, group=sex),alpha=0.05)+
  theme_bw(base_size = fontsize)

gg.predict <- gg.predict + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), panel.border = element_blank())

allN<-gg.predict + scale_color_discrete(labels=c("Women", "Men"))+
  scale_fill_discrete(labels=c("Women", "Men"))+
  scale_y_continuous(name="Attractiveness")+
  scale_x_continuous(name="Nonallometric Sextypicality")

allN



# ********************
# MIXED EFFECT MODELS
# ********************

alldat<-cbind (hdat, Allometric_SShD=scale(Allometric_SShD), Nonallometric_SShD=scale(Nonallometric_SShD))
fdat<-subset(alldat, sex == "F")
mdat<-subset(alldat, sex == "M")

# ALLO
mmodelA1<- lmer(Attractiveness ~ Allometric_SShD*sex + (Allometric_SShD*sex|nat), data = alldat)
summary(mmodelA1)

# NONALLO
mmodelN1<- lmer(Attractiveness ~ Nonallometric_SShD*sex + (Nonallometric_SShD*sex|nat), data = alldat)
summary(mmodelN1)


# *******************************
# VIZUALITION OF SHAPE CHANGES
# *******************************

# ***** TPS vizualization of Allometric SShD ********

aloreg1 <- procD.lm(coords ~ height, iter = 999, data = gtf2, weights = NULL); summary(aloreg1)

alowfit <- arrayspecs(aloreg1$fitted, 72, 2) [,,which(gtf2$sex=="F")]
alomfit <- arrayspecs(aloreg1$fitted, 72, 2) [,,which(gtf2$sex=="M")]

aMEN <- mshape(alomfit)
aWOMEN <- mshape(alowfit)
aMEAN <- mshape(gtf2$coords)


# ***** TPS vizualization of NON-Allometric SShD *****
shapesex<- coords.subset(gtf2$coords, gtf2$sex)

nreg1 <- procD.lm(coords ~ height, iter = 999, data = gtf2, weights = NULL); summary(nreg1)

nwfit <- arrayspecs(nreg1$residuals, 72, 2) [,,which(gtf2$sex=="F")] +  arrayspecs(predict(lm(two.d.array(shapesex$F)~1)), 72,2)
nmfit <- arrayspecs(nreg1$residuals, 72, 2) [,,which(gtf2$sex=="M")] + arrayspecs(predict(lm(two.d.array(shapesex$M)~1)), 72,2)

nMEN <- mshape(nmfit)
nWOMEN <- mshape(nwfit)
nMEAN <- mshape(gtf2$coords)


# combined TPS plot ALLO + NONALLO

par (mar = c(0,0,1.5,0), oma = c(0,3,0,0))
(gp1<-layout(matrix(c(1:10), 2, 5, byrow = T), widths=c(1,1,1), heights=c(1,1,1), respect = F)) 

plotRefToTarget(aMEAN, aWOMEN, method = "TPS", mag = 3, links = links, gridPars = GP1)
mtext("Allometric SShD", 2, cex = 1.2, font = NA)
title ("3x women", cex = 2)
plotRefToTarget(aMEAN, aWOMEN, method = "TPS", mag = 1, links = links, gridPars = GP1)
title("women mean", cex = 2)
plotRefToTarget(aMEAN, aMEAN, method = "TPS", mag = 1, links = links, gridPars = GP1)
title ("mean", cex = 2)
plotRefToTarget(aMEAN, aMEN, method = "TPS", mag = 1, links = links, gridPars = GP1)
title ("men mean", cex = 2)
plotRefToTarget(aMEAN, aMEN, method = "TPS", mag = 3, links = links, gridPars = GP1)
title ("3x men", cex = 2)

plotRefToTarget(nMEAN, nWOMEN, method = "TPS", mag = 3, links = links, gridPars = GP1)
mtext("Non-allometric SShD", 2, cex = 1.2, font = NA)
title ("3x women", cex = 2)
plotRefToTarget(nMEAN, nWOMEN, method = "TPS", mag = 1, links = links, gridPars = GP1)
title("women mean", cex = 2)
plotRefToTarget(nMEAN, nMEAN, method = "TPS", mag = 1, links = links, gridPars = GP1)
title ("mean", cex = 2)
plotRefToTarget(nMEAN, nMEN, method = "TPS", mag = 1, links = links, gridPars = GP1)
title ("men mean", cex = 2)
plotRefToTarget(nMEAN, nMEN, method = "TPS", mag = 3, links = links, gridPars = GP1)
title ("3x men", cex = 2)


