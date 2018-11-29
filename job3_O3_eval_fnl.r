#--------------------------------------------------------------------
# Updates:
#
# August 2018: Miguel Noyola P.
# Copyright 2018 CCA-UNAM.
# All rights reserved.
# This script uses stat_analisys output files
# from MET (job1_eca_aggr_stat_mpr.stat) to evaluate
# wrf model performance in comparisson with in situ observations.
#--------------------------------------------------------------------
f1 <-paste(path,'../test/urban/job3_eca_aggr_stat_mpr.stat',sep="",collapse=NULL)
f2 <-paste(path,'../test/nourban/job3_eca_aggr_stat_mpr.stat',sep="",collapse=NULL)
VAR <- "Ozono [ppbv]"

####  ************   read job file for Temperature (wrf-urbano) ************   ####
mydta<- read.table(file=f1,sep="",header= TRUE)
mydta_nu<- read.table(file=f2,sep="",header= TRUE)

datos <- matrix(data=NA,nrow=length(mydta[,5]),ncol=5)
datos <- data.frame(datos)
names(datos) <- (rownames=c("date","ID","obs","wrfu","wrfnu"))
date <- as.POSIXct(strptime(mydta[,5],format="%Y%m%d_%H%M%S",tz="GMT"))
attributes(date)$tzone <- "America/Mexico_City"
ID <- mydta[,25]
wrfu <- mydta[,30]
obs <- mydta[,31]
wrfnu <- mydta_nu[,30]
datos <- cbind(date,ID,obs,wrfu,wrfnu,datos[,-c(1,2,3,4,5)])

var <- datos
varwrfu <- dcast( var,date~ID,value.var=c("wrfu"))
varwrfnu <- dcast( var,date~ID,value.var=c("wrfnu"))
varobs <- dcast( var,date~ID,value.var=c("obs"))

####  ************   Create a subset according to stations ID to compare ************   ####
stcmpID <- subset( var,  ID=="HGM" | ID=="TLA" | ID=="UAX" | ID=="UIZ" | ID=="ACO" | ID=="CUA" | ID=="FAC" | ID=="MGH" | ID=="XAL" )

#### ************ Time series for ID Stations subset obs vs wrf-urban vs wrf-nourban ***********  ####
scatterPlot(stcmpID, x="obs",y="wrfnu",
linear=TRUE, col="red", cex=0.5, smooth=FALSE,mod.line = TRUE,fontsize=10,cex.lab=1.5,
main="Observaciones vs WRF-NoUrbano",sub=VAR,xlab="Observaciones",ylab="simulaciones")

scatterPlot(stcmpID, x="obs",y="wrfu",
linear=TRUE, col="red", cex=0.5, smooth=FALSE,mod.line = TRUE,fontsize=10,cex.lab=1.5,
main="Observaciones vs WRF-Urbano",sub=VAR,xlab="Observaciones",ylab="simulaciones")

#### ************ General time plots of whole subset Stations ************  ####
timePlot( var,pollutant=c("obs","wrfu","wrfnu"),group=TRUE,pch=c(1,14,6),cex=0.4,col=c("black","blue","red"),lty=0,date.breaks = 6,date.format =  "%m-%d\n%H% h",fill=c("black","blue","red"),
main="Series de Observaciones, WRF-Urbano y WRF-NoUrbano",ylab=VAR)

#### ************ time plots of whole subset Stations separately ************  ####
timePlot( var,pollutant=c("obs","wrfu","wrfnu"),group=FALSE,type="default",
pch=c(1,14,6),cex=0.3,
col=c("black","blue","red"),lty=0,
date.breaks = 6,date.format =  "%m-%d\n%H% h",
fill=c("black","blue","red"),
main="Series de Observaciones, WRF-Urbano y WRF-NoUrbano",ylab=VAR)

#### ************ time plots of whole subset Stations ************  ####
timePlot( var,pollutant=c("obs","wrfu","wrfnu"),group=TRUE,type="ID",lwd=c(2), col=c("black","blue","red"), date.format="%d\n%H",auto.text=FALSE, fontsize=10,cex.lab=1.3, cex.axis=3.5, avg.time="hour",main="Resumen",cex.main=1.3, ylab=VAR,xlab=expression("[día]/[hora]"),cex=0.5, name.pol=c("obs","WRF-urban","WRF-Nourban"))

#### ************ scatter plots of Stations ID subset with inear model regretion ************  ####
scatterPlot(stcmpID, x="obs",y="wrfu",z=NA,method="scatter",Group=NA,type="ID",
linear=TRUE, ci=TRUE, mod.line=TRUE, plot.type="p",col="red",
key=TRUE, key.title=group, key.columns=1, auto.text=TRUE,key.position="bottom",
cex=0.3, spline=FALSE, smooth=FALSE,strip=TRUE,fontsize=10,cex.lab=1.3,
main="Observaciones vs WRF-Urbano",sub=VAR,xlab="Observaciones",ylab="simulaciones")
dev.copy(png,'T_scatter_U.png')
dev.off()

scatterPlot(stcmpID, x="obs",y="wrfnu",z=NA,method="scatter",Group=NA,type="ID",
linear=TRUE, ci=TRUE, mod.line=TRUE, plot.type="p",col="red",
key=TRUE, key.title=group, key.columns=1, auto.text=TRUE,key.position="bottom",
cex=0.3, spline=FALSE, smooth=FALSE,strip=TRUE,fontsize=10,cex.lab=1.3,
main="Observaciones vs WRF-NoUrbano",sub=VAR,xlab="Observaciones",ylab="simulaciones")
dev.copy(png,'T_scatter_NU.png')
dev.off()

#### ************ Tylor Diagrams ************  ####
TaylorDiagram(stcmpID, obs ="obs",mod="wrfu",type="ID",main="OBS vs WRF-U", cex=0.5, spline=FALSE, smooth=FALSE,strip=TRUE,fontsize=10,cex.lab=1.3)
dev.copy(png,'TMP_Taylor_U.png')
dev.off()

TaylorDiagram(stcmpID, obs ="obs",mod="wrfnu",type="ID",main="OBS vs WRF-NU",cex=0.5, spline=FALSE, smooth=FALSE,strip=TRUE,fontsize=10,cex.lab=1.3)
dev.copy(png,'T_Taylor_NU.png')
dev.off()

##### ************ Metrics for Performance Model Evaluation  ************  ##### IC
Metrics = matrix(nrow=n,ncol=18)
dimnames(Metrics) <- list(stationID,
c("MEAN_OB","MEAN_WU","MEAN_WN","SD_OB","SD_WU","SD_WN","RAE_WU","RAE_WN","MAE_WU","MAE_WN",
"BIAS_WU","BIAS_WN","RMSE_WU","RMSE_WN","R2_WU","R2_WN","Ic_WU","Ic_WN"))

MetricsDaylight = matrix(nrow=n,ncol=24)
dimnames(MetricsDaylight) <- list(stationID,
c("MN_OB_DY","MN_WU_DY","MN_WN_DY","MN_OB_NGT","MN_WU_NGT","MN_WN_NGT",
"SD_OB_DY","SD_WU_DY","SD_WN_DY","SD_OB_NGT","SD_WU_NGT","SD_WN_NGT",
"RMSE_WU_DY","RMSE_WN_DY","RMSE_WU_NGT","RMSE_WN_NGT",
"R2_WU_DY","R2_WN_DY","R2_WU_NGT","R2_WN_NGT",
"Ic_WU_DY","Ic_WN_DY","Ic_WU_NGT","Ic_WN_NGT"))

#### ************ Time plots by Stations ID  ************  ####
for (i in 1:n) {
    IDst <- subset( var, ID==stationID[i])
    
    plotname <- paste("O3_Plot_",stationID[i],".png",sep="")
    print(plotname)
    title_name <- paste("Obsesvaciones vs simulaciones, estación: ",stationID[i])
    
    timePlot(IDst,pollutant=c("obs","wrfu","wrfnu"),data.thresh = 75,
    group=TRUE,type = "default",plot.type = "l",
    pch=c(19,15,17),cex=0.7,lty=c(1,5,2),lwd = 1.2,fill=FALSE,
    col=c("black","blue","red"),date.breaks = 6,date.format="%m-%d\n%H h",
    main=title_name,ylab=VAR)
    
    MetricsDaylight <- Mdl_Eval_DL(IDst,i,MetricsDaylight)
    Metrics <- Mdl_Eval(IDst,i,Metrics)
}
##### ************ compute mean by day for each station and write into a file  ************  #####
mean.day <- timeAverage(stcmpID,avg.time="day",data.thresh=0,statistic="mean",type="ID")
write.table(mean.day, file = "mean_day_O3_cdmx.csv", append = FALSE, quote = TRUE, sep = ",", eol = "\n", na = "NA", dec = ".", row.names = TRUE,col.names = TRUE, qmethod = c("escape", "double"))

##### ************ Write Metrics table into a csv file  ************  #####
write.table(Metrics, file = "Metrics_O3_cdmx.csv", append = FALSE, quote = TRUE, sep = ",", eol = "\n", na = "NA", dec = ".", row.names = TRUE,col.names = TRUE, qmethod = c("escape", "double"))

write.table(MetricsDaylight, file = "Metrics_dl_O3_cdmx.csv", append = FALSE, quote = TRUE, sep = ",", eol = "\n", na = "NA", dec = ".", row.names = TRUE,col.names = TRUE, qmethod = c("escape", "double"))
