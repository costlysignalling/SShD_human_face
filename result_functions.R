library(stats)

#Function returning nice correlation tables
cortab<-function(x,rounding=2){
  lx<-ncol(x)
  res<-matrix(NA,ncol=lx,nrow=lx)
  for(i in 1:lx){
    for(j in 1:lx){
      ct<-cor.test(x[,i],x[,j])
      res[i,j]<-paste(round(ct[[4]],rounding),ifelse(ct[[3]]<0.05,"*",ifelse(ct[[3]]<0.01,".","")),sep="")
    }
  }
  rownames(res)<-names(x)
  colnames(res)<-names(x)
  return(res)
}

#Function returning angles
angles<-function(x,y){
  lx<-ncol(x)
  res<-rep(NA,lx)
  for(i in 1:lx){
    res[i]<-angle(x[,i],y[,i])
  }
  names(res)<-colnames(x)
  return(res)
}

disp.tab.sig<-function(mdisp){
  min(mdisp[[2]][mdisp[[3]]<0.05][mdisp[[2]][mdisp[[3]]<0.05]>max(mdisp[[2]][mdisp[[3]]>0.05])])
}


tanglegram2<-function(dend1,dend2){
  dend<-untangle(dendlist(dend1,dend2))
  tanglegram(dend[[1]],dend[[2]],
             highlight_distinct_edges = FALSE, # Turn-off dashed lines
             common_subtrees_color_branches = TRUE, # Color common branches 
             main = paste("entanglement =", round(entanglement(dend), 2))
  )
  return(entanglement(dend))
}


rowize<-function(x){
  byr<-c(x[,,1])
  for(i in 2:dim(x)[3]){
    byr<-rbind(byr,c(x[,,i])) 
  }
  return(byr)
}


#This function gives an alternative visulization for plotRefToTarget "points" method with pch=16
plotRefToTarget2<-function (M1, M2, mesh = NULL, outline = NULL, method = c("points"), mag = 1, 
                            links = NULL, label = FALSE, axes = FALSE, gridPars = NULL, 
                            useRefPts = FALSE, ...) 
{
  method <- match.arg(method)
  if (any(is.na(M1))) {
    stop("Data contains missing values. Estimate these first (see 'estimate.missing').")
  }
  if (any(is.na(M2))) {
    stop("Data contains missing values. Estimate these first (see 'estimate.missing').")
  }
  if (is.null(gridPars)) 
    gP <- gridPar()
  else gP <- gridPars
  k <- dim(M1)[2]
  mag <- (mag - 1)
  M2 <- M2 + (M2 - M1) * mag
  limits <- function(x, s) {
    r <- range(x)
    rc <- scale(r, scale = FALSE)
    l <- mean(r) + s * rc
  }
  if (k == 2) {
    if (method == "points") {
      plot.new()
      if (axes) {
        plot.window(limits(M1[, 1], 1.25), limits(M1[, 
                                                     2], 1.25), xlab = "x", ylab = "y", 
                    asp = 1)
      }
      if (!axes) {
        plot.window(limits(M1[, 1], 1.25), limits(M1[, 
                                                     2], 1.25), xlab = "", ylab = "", 
                    asp = 1, xaxt = "n", yaxt = "n")
      }
      if (label) {
        text(M1, label = paste(1:dim(M1)[1]), adj = gP$txt.adj, 
             pos = gP$txt.pos, cex = gP$txt.cex, col = gP$txt.col)
      }
      if (!is.null(outline)) {
        curve.warp <- tps2d(outline, M1, M2)
        plot.xy(xy.coords(outline), type = "p", 
                pch = 16, cex = gP$out.cex, col = gP$out.col)
        plot.xy(xy.coords(curve.warp), type = "p", 
                pch = 16, cex = gP$tar.out.cex, col = gP$tar.out.col)
      }
      if (!is.null(links)) {
        linkcol <- rep(gP$link.col, nrow(links))[1:nrow(links)]
        linklwd <- rep(gP$link.lwd, nrow(links))[1:nrow(links)]
        linklty <- rep(gP$link.lty, nrow(links))[1:nrow(links)]
        tarlinkcol <- rep(gP$tar.link.col, nrow(links))[1:nrow(links)]
        tarlinklwd <- rep(gP$tar.link.lwd, nrow(links))[1:nrow(links)]
        tarlinklty <- rep(gP$tar.link.lty, nrow(links))[1:nrow(links)]
        for (i in 1:nrow(links)) {
          segments(M1[links[i, 1], 1], M1[links[i, 1], 
                                          2], M1[links[i, 2], 1], M1[links[i, 2], 2], 
                   col = linkcol[i], lty = linklty[i], lwd = linklwd[i])
          segments(M2[links[i, 1], 1], M2[links[i, 1], 
                                          2], M2[links[i, 2], 1], M2[links[i, 2], 2], 
                   col = tarlinkcol[i], lty = tarlinklty[i], 
                   lwd = tarlinklwd[i])
        }
      }
      plot.xy(xy.coords(M2), type = "p", pch = 16,
              bg = gP$tar.pt.bg, cex = gP$tar.pt.size,col=gP$tar.pt.bg)
      plot.xy(xy.coords(M1), type = "p", pch = 16,
              bg = gP$pt.bg, cex = gP$pt.size,col=gP$pt.bg)
    }
  }
}