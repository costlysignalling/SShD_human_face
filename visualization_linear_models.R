
# ****************
#  COMBINED PLOTS
# ****************

# modify plots margin (top, right, bottom, left)
top<-1
bottom<-0.5

NamPlot<-NamPlot+ theme(plot.margin = unit(c(top,0,bottom,0), "cm"))
CmrPlot<-CmrPlot+ theme(plot.margin = unit(c(top,0,bottom,0), "cm"))
TrPlot<-TrPlot+ theme(plot.margin = unit(c(top,0,bottom,0), "cm"))
RoPlot<-RoPlot+ theme(plot.margin = unit(c(top,0,bottom,0), "cm"))
CzPlot<-CzPlot+ theme(plot.margin = unit(c(top,0,bottom,0), "cm"))
UkPlot<-UkPlot+ theme(plot.margin = unit(c(top,0,bottom,0), "cm"))
ColPlot<-ColPlot+ theme(plot.margin = unit(c(top,0,bottom,0), "cm"))
BrazPlot<-BrazPlot+ theme(plot.margin = unit(c(top,0,bottom,0), "cm"))
AllPlot<-AllPlot + theme(plot.margin = unit(c(top,0,bottom,0), "cm"))

AllPlot2<-AllPlot

NamPlot <- NamPlot + theme (axis.title.x = element_blank (), panel.border = element_blank())
CmrPlot <- CmrPlot + theme (axis.title.x = element_blank(), axis.title.y = element_blank (), panel.border = element_blank())
TrPlot <- TrPlot + theme (axis.title.x = element_blank(), axis.title.y = element_blank (), panel.border = element_blank())

RoPlot <- RoPlot + theme (axis.title.x = element_blank(), panel.border = element_blank())
CzPlot <- CzPlot + theme (axis.title.x = element_blank (), axis.title.y = element_blank (), panel.border = element_blank())
UkPlot <- UkPlot + theme (axis.title.x = element_blank (), axis.title.y = element_blank (), panel.border = element_blank())

ColPlot <- ColPlot + theme ( panel.border = element_blank())
BrazPlot <- BrazPlot + theme (axis.title.y = element_blank (), panel.border = element_blank())
AllPlot <- AllPlot + theme (axis.title.y = element_blank (), panel.border = element_blank())

figure1 <- ggarrange (NamPlot, CmrPlot, TrPlot, RoPlot, CzPlot, UkPlot, ColPlot, BrazPlot, AllPlot,labels = c("Namibia","Cameroon","Turkey","Romania","Czechia","UK", "Colombia", "Brazil","All cultures"), ncol = 3, nrow = 3, label.x = 0, label.y = 1, font.label = list(size = 14, face = "bold", color = "darkblue"),  common.legend = TRUE, legend = "bottom")


####
##Allometric
####

NamPlot<-namA+ theme(plot.margin = unit(c(top,0,bottom,0), "cm"))
CmrPlot<-cmrA+ theme(plot.margin = unit(c(top,0,bottom,0), "cm"))
TrPlot<-trA+ theme(plot.margin = unit(c(top,0,bottom,0), "cm"))
RoPlot<-roA+ theme(plot.margin = unit(c(top,0,bottom,0), "cm"))
CzPlot<-czA+ theme(plot.margin = unit(c(top,0,bottom,0), "cm"))
UkPlot<-ukA+ theme(plot.margin = unit(c(top,0,bottom,0), "cm"))
ColPlot<-colA+ theme(plot.margin = unit(c(top,0,bottom,0), "cm"))
BrazPlot<-brazA+ theme(plot.margin = unit(c(top,0,bottom,0), "cm"))
AllPlot<-allA+ theme(plot.margin = unit(c(top,0,bottom,0), "cm"))

AllPlotA2<-AllPlot

NamPlot <- NamPlot + theme (axis.title.x = element_blank (), panel.border = element_blank())
CmrPlot <- CmrPlot + theme (axis.title.x = element_blank(), axis.title.y = element_blank (), panel.border = element_blank())
TrPlot <- TrPlot + theme (axis.title.x = element_blank(), axis.title.y = element_blank (), panel.border = element_blank())

RoPlot <- RoPlot + theme (axis.title.x = element_blank(), panel.border = element_blank())
CzPlot <- CzPlot + theme (axis.title.x = element_blank (), axis.title.y = element_blank (), panel.border = element_blank())
UkPlot <- UkPlot + theme (axis.title.x = element_blank (), axis.title.y = element_blank (), panel.border = element_blank())

ColPlot <- ColPlot + theme ( panel.border = element_blank())
BrazPlot <- BrazPlot + theme (axis.title.y = element_blank (), panel.border = element_blank())
AllPlot <- AllPlot + theme (axis.title.y = element_blank (), panel.border = element_blank())

figureA <- ggarrange (NamPlot, CmrPlot, TrPlot, RoPlot, CzPlot, UkPlot, ColPlot, BrazPlot, AllPlot,labels = c("Namibia","Cameroon","Turkey","Romania","Czechia","UK", "Colombia", "Brazil","All cultures"), ncol = 3, nrow = 3, label.x = 0, label.y = 1, font.label = list(size = 14, face = "bold", color = "darkblue"),  common.legend = TRUE, legend = "bottom")


figureA



####
##Nonallometric
####

NamPlot<-namN + theme(plot.margin = unit(c(top,0,bottom,0), "cm"))
CmrPlot<-cmrN+ theme(plot.margin = unit(c(top,0,bottom,0), "cm"))
TrPlot<-trN+ theme(plot.margin = unit(c(top,0,bottom,0), "cm"))
RoPlot<-roN+ theme(plot.margin = unit(c(top,0,bottom,0), "cm"))
CzPlot<-czN+ theme(plot.margin = unit(c(top,0,bottom,0), "cm"))
UkPlot<-ukN+ theme(plot.margin = unit(c(top,0,bottom,0), "cm"))
ColPlot<-colN+ theme(plot.margin = unit(c(top,0,bottom,0), "cm"))
BrazPlot<-brazN+ theme(plot.margin = unit(c(top,0,bottom,0), "cm"))
AllPlot<-allN+ theme(plot.margin = unit(c(top,0,bottom,0), "cm"))

AllPlotN2<-AllPlot

NamPlot <- NamPlot + theme (axis.title.x = element_blank (), panel.border = element_blank())
CmrPlot <- CmrPlot + theme (axis.title.x = element_blank(), axis.title.y = element_blank (), panel.border = element_blank())
TrPlot <- TrPlot + theme (axis.title.x = element_blank(), axis.title.y = element_blank (), panel.border = element_blank())

RoPlot <- RoPlot + theme (axis.title.x = element_blank(), panel.border = element_blank())
CzPlot <- CzPlot + theme (axis.title.x = element_blank (), axis.title.y = element_blank (), panel.border = element_blank())
UkPlot <- UkPlot + theme (axis.title.x = element_blank (), axis.title.y = element_blank (), panel.border = element_blank())

ColPlot <- ColPlot + theme ( panel.border = element_blank())
BrazPlot <- BrazPlot + theme (axis.title.y = element_blank (), panel.border = element_blank())
AllPlot <- AllPlot + theme (axis.title.y = element_blank (), panel.border = element_blank())

figureN <- ggarrange (NamPlot, CmrPlot, TrPlot, RoPlot, CzPlot, UkPlot, ColPlot, BrazPlot, AllPlot,labels = c("Namibia","Cameroon","Turkey","Romania","Czechia","UK", "Colombia", "Brazil","All cultures"), ncol = 3, nrow = 3, label.x = 0, label.y = 1, font.label = list(size = 14, face = "bold", color = "darkblue"),  common.legend = TRUE, legend = "bottom")



Totalplot<- AllPlot2 + theme (panel.border = element_blank())
Alloplot<- AllPlotA2 + theme ( axis.title.y = element_blank (), panel.border = element_blank())
Nonalloplot<- AllPlotN2 + theme (axis.title.y = element_blank (), panel.border = element_blank())


figuremain <- ggarrange (Totalplot, Alloplot, Nonalloplot,labels = c("","",""), ncol = 3, nrow = 1, label.x = 0, label.y = 1, font.label = list(size = 14, face = "bold", color = "darkblue"),  common.legend = TRUE, legend = "bottom")

figuremain
