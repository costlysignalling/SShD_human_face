
anglabsA<-paste(round(Aang,1),"° ",anglabs,sep="")
anglabsN<-paste(round(Nang,1),"° ",anglabs,sep="")
anglabsAN<-paste(round(ANang,1),"° ",anglabs,sep="")

visangle<-function(x,anglab,col,plline=T,pltext=T,tlength=0.1){
  if(plline==T){
    lines(c(cos(deg2rad(x))*(1+tlength/2),cos(deg2rad(x))*(1-tlength/2)),c(sin(deg2rad(x))*(1+tlength/2),sin(deg2rad(x))*(1-tlength/2)),lwd=2,col=col)
  }
  if(pltext==T){
    text(cos(deg2rad(x))*(1+tlength/2),sin(deg2rad(x))*(1+tlength/2),anglab,pos=4,srt=x,font=2,col=col)
  }
}

visangles<-function(ang,anglabs,col,plline=T,pltext=T,add=F,lab0="0° Pure allometry",lab90="90° No allometry"){
  if(add==F){
    xseq<-seq(0,1,0.001)
    yseq<-sqrt(1-xseq^2)
    
    par(mar=c(0.4,0.4,0.4,0.4))
    plot(NULL,xlim=c(0,1.85),ylim=c(0,1.85),xlab="",ylab="",axes = F)
    polygon(c(0,xseq),c(0,yseq),col="#EEEEEE")
    text(1,0,lab0,pos=4)
    text(-0.05,1.05,lab90,pos=4,srt=90)
  }
  
  for(i in 1:length(ang)){
    visangle(ang[i],anglabs[i],cols[i],plline=plline,pltext=pltext)
  }
}

#w<-20
#tiff("angles.tiff",width=w,height=w/2,units="cm",res=600,compression="lzw")
 
par(mfrow=c(1,2))

spread<-(30-(max(Aang)-min(Aang)))/2
visangles(Aang,anglabsA,cols,pltext=F)
visangles(seq(min(Aang)-spread,max(Aang)+spread,l=length(Aang))[rank(Aang)],anglabsA,cols,plline=F,add=T)
text(-0.1,1.8,"SShD and allometric SShD",pos=4,font=1,cex=1.1)
#text(-0.1,1.8,expression(paste(alpha, ". SShD and allometric SShD", sep="")),pos=4,font=1,cex=1.1)


spread<-(30-(max(Nang)-min(Nang)))/2
visangles(Nang,anglabsN,cols,pltext=F,lab0="0° Pure SShD",lab90="90° No SShD")
visangles(seq(min(Nang)-spread,max(Nang)+spread,l=length(Nang))[rank(Nang)],anglabsN,cols,plline=F,add=T)
text(-0.1,1.8,"SShD and nonallometric SShD",pos=4,font=1,cex=1.1)
#text(-0.1,1.8,expression(paste(nu, ". SShD and nonallometric SShD", sep="")),pos=4,font=1,cex=1.1)


#dev.off()
