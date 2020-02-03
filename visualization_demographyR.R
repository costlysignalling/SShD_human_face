
#w<-16

#tiff("demography.tiff",width=w,height=w/5,units="cm",res=600,compression="lzw")

dem<-read.table("demography.csv",sep="\t",header=T)
demnam<-dem[,1]

dem<-dem[,-1]
row.names(dem)<-demnam
round(dem,2)

reord<-c(5,2,7,6,4,8,3,1)

cols<-c("#00AA22","#0055EE","#44CC00","#FF2277","#004488","#880022","#CC9900","#EE0000")
coldem<-c("#555555","#FF6600","#0066DD","#008822")

pdem<-dem[,1:4]
pdem<-pdem/rep(apply(pdem,2,max),each=8)*0.6

x<-rep(3:10)
y<-rep(0,8)

par(mar=c(0,0,0,0))
plot(1:10,1:10,bty="n",axes=F,xlab="",ylab="",xlim=c(0,10.2),ylim=c(-0.2,1.2),type="n")
text(x,y,rownames(dem),col=cols[reord],xpd=T)


offx<-0.1
offy<--0.2
demlwd<-2

for(i in 1:8){
  lines(rep(x[i]-offx/2-offx,2),c(y[i]-offy,y[i]-offy+pdem[i,1]),lwd=demlwd,col=coldem[1],xpd=T)
  lines(rep(x[i]-offx/2,2),c(y[i]-offy,y[i]-offy+pdem[i,2]),lwd=demlwd,col=coldem[2],xpd=T)
  lines(rep(x[i]+offx/2,2),c(y[i]-offy,y[i]-offy+pdem[i,3]),lwd=demlwd,col=coldem[3],xpd=T)
  lines(rep(x[i]+offx/2+offx,2),c(y[i]-offy,y[i]-offy+pdem[i,4]),lwd=demlwd,col=coldem[4],xpd=T)
    }

text(rep(-0.5,4),c(3,2,1,0)*0.25,c("Urbanization","Population density","Population size","GDP per capita"),xpd=T,col=coldem,pos=4)

#dev.off()