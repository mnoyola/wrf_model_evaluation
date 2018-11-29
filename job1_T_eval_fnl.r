#--------------------------------------------------------------------
# Updates:
# August 2018: Miguel Noyola P.
# Copyright 2018 CCA-UNAM.
# All rights reserved.
# This script uses stat_analisys output files
# from MET (job1_eca_aggr_stat_mpr.stat) to evaluate
# wrf model performance in comparisson with in situ observations.
#--------------------------------------------------------------------
f1 <-paste(path,'../test/urban/job1_eca_aggr_stat_mpr.stat',sep="",collapse=NULL)
f2 <-paste(path,'../test/nourban/job1_eca_aggr_stat_mpr.stat',sep="",collapse=NULL)

####  ************   read job file for Temperature (wrf-urbano) ************   ####
mydta<- read.table(file= f1,sep="",header= TRUE)
mydta <- mydta[,-c(1,2,3,4,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,26,27,28,29,32,33)]
date <-(mydta[,1])
date <- as.POSIXct(strptime(date,format="%Y%m%d_%H",tz="GMT"))
attributes(date)$tzone <- "America/Mexico_City"
mydta<-cbind(date,mydta[,-1])
head(mydta)
obs <- dcast(mydta,date~OBS_SID,value.var=c("OBS"))
wrfu <- dcast(mydta,date~OBS_SID,value.var=c("FCST"))

####  ************   read job file for Temperature (wrf-NOurbano) ************   ####
mydta_nu<- read.table(file=f2,sep="",header= TRUE)
mydta_nu <- mydta_nu[,-c(1,2,3,4,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,26,27,28,29,32,33)]
date <-(mydta_nu[,1])
date <- as.POSIXct(strptime(date,format="%Y%m%d_%H","GMT"))
mydta_nu<-cbind(date,mydta_nu[,-1])
head(mydta_nu)
wrfnu <- dcast(mydta_nu,date~OBS_SID,value.var=c("FCST"))

####  ************   Create a subset according to stations ID to compare ************   ####
stcmpID <- subset(mydta,  OBS_SID=="HGM" | OBS_SID=="TLA" | OBS_SID=="UAX" | OBS_SID=="UIZ" | OBS_SID=="ACO" | OBS_SID=="CUA" | OBS_SID=="FAC" | OBS_SID=="MGH" | OBS_SID=="XAL" )

stcmpID_nu <- subset(mydta_nu,  OBS_SID=="HGM" | OBS_SID=="TLA" | OBS_SID=="UAX" | OBS_SID=="UIZ" | OBS_SID=="ACO" | OBS_SID=="CUA" | OBS_SID=="FAC" | OBS_SID=="MGH" | OBS_SID=="XAL" )

for (i in 1:23){
    if (names(obs[i]) != "date"){
        obs[i] <- obs[i] -273.15
        wrfu[i] <- wrfu[i] -273.15
        wrfnu[i] <- wrfnu[i] -273.15
    }
}
for (i in 3:4){
    stcmpID[i] <- stcmpID[i] -273.15
    stcmpID_nu[i] <- stcmpID_nu[i] -273.15
}

scat <- stcmpID
scat <- cbind(stcmpID_nu[,"FCST"],scat[,-c(5)])
names(scat)[1] <- "wrfnu"
names(scat)[4] <- "wrfu"
names(scat)[5] <- "obs"

#### ************ Time series for ID Stations subset obs vs wrf-urban vs wrf-nourban ***********  ####
scatterPlot(stcmpID_nu, x="OBS",y="FCST",
linear=TRUE, col="red", cex=0.5, smooth=FALSE,mod.line = TRUE,fontsize=10,cex.lab=1.5,
main="Observaciones vs WRF-NoUrbano",sub="Temperatura [ºC]",xlab="Observaciones",ylab="simulaciones")

scatterPlot(stcmpID, x="OBS",y="FCST",
linear=TRUE, col="red", cex=0.5, smooth=FALSE,mod.line = TRUE,fontsize=10,cex.lab=1.5,
main="Observaciones vs WRF-Urbano",sub="Temperatura [ºC]",xlab="Observaciones",ylab="simulaciones")

#### ************ General time plots of whole subset Stations ************  ####
timePlot(scat,pollutant=c("obs","wrfu","wrfnu"),group=TRUE,pch=c(1,14,6),cex=0.4,col=c("black","blue","red"),lty=0,date.breaks = 6,date.format =  "%m-%d\n%H% h",fill=FALSE,
main="Series de Observaciones, WRF-Urbano y WRF-NoUrbano",ylab="Temperatura [ºC]")

#fill=c("black","blue","red"),
#### ************ time plots of whole subset Stations separately ************  ####
timePlot(scat,pollutant=c("obs","wrfu","wrfnu"),group=FALSE,
type = "default",pch=c(1,14,6),cex=0.4,
col=c("black","blue","red"),lty=0,
date.breaks = 6,date.format =  "%m-%d\n%H% h",
fill=c("black","blue","red"),
main="Series de Observaciones, WRF-Urbano y WRF-NoUrbano",ylab="Temperatura [ºC]"
)

#### ************ time plots of whole subset Stations ************  ####
timePlot(scat,pollutant=c("obs","wrfu","wrfnu"),group=TRUE,type="OBS_SID",lwd=c(2), col=c("black","blue","red"), date.format="%d\n%H",auto.text=FALSE, fontsize=10,cex.lab=1.5, cex.axis=3.5, avg.time="hour",main="Temperatura",cex.main=1.5, ylab=expression(" [ºC]"),xlab=expression("[día]/[hora]"),cex=0.5, name.pol=c("OBS","WRF-urban","WRF-Nourban"))

#### ************ scatter plots of Stations ID subset with inear model regretion ************  ####
scatterPlot(stcmpID, x="OBS",y="FCST",z=NA,method="scatter",Group=NA,type="OBS_SID",
     linear=TRUE, ci=TRUE, mod.line=TRUE, plot.type="p",col="red",
     key=TRUE, key.title=group, key.columns=1, auto.text=TRUE,key.position="bottom",
     cex=0.3, spline=FALSE, smooth=FALSE,strip=TRUE,fontsize=10,cex.lab=1.5,
     main="Observaciones vs WRF-Urbano",sub="Temperatura [ºC]",xlab="Observaciones",ylab="simulaciones")
dev.copy(png,'T_scatter_U.png')
dev.off()

scatterPlot(stcmpID_nu, x="OBS",y="FCST",z=NA,method="scatter",Group=NA,type="OBS_SID",
    linear=TRUE, ci=TRUE, mod.line=TRUE, plot.type="p",col="red",
    key=TRUE, key.title=group, key.columns=1, auto.text=TRUE,key.position="bottom",
    cex=0.3, spline=FALSE, smooth=FALSE,strip=TRUE,fontsize=10,cex.lab=1.5,
    main="Observaciones vs WRF-NoUrbano",sub="Temperatura [ºC]",xlab="Observaciones",ylab="simulaciones")
dev.copy(png,'T_scatter_NU.png')
dev.off()

#### ************ Tylor Diagrams ************  ####
TaylorDiagram(stcmpID, obs ="OBS",mod="FCST",type="OBS_SID",main="Temperatura:  Observaciones vs WRF-Urbano", cex=0.5, spline=FALSE, smooth=FALSE,strip=TRUE,fontsize=10,cex.lab=1.5)
dev.copy(png,'TMP_Taylor_U.png')
dev.off()

TaylorDiagram(stcmpID_nu, obs ="OBS",mod="FCST",type="OBS_SID",main="Temperatura:  Observaciones vs WRF-NoUrbano",cex=0.5, spline=FALSE, smooth=FALSE,strip=TRUE,fontsize=10,cex.lab=1.5)
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
    IDst <- subset(scat, OBS_SID==stationID[i])
        
    plotname <- paste("T_Plot_",stationID[i],".png",sep="")
    print(plotname)
    title_name <- paste("Obsesvaciones vs simulaciones, estación: ",stationID[i])

    timePlot(IDst,pollutant=c("obs","wrfu","wrfnu"),
    group=TRUE,type = "default",plot.type = "l",
    pch=c(19,15,17),cex=0.6,lty=c(1,5,2),lwd = 1.2,fill=FALSE,
    col=c("black","blue","red"),date.breaks = 6,date.format="%m-%d\n%H h",
    main=title_name,ylab="Temperatura [ºC]")
    
    MetricsDaylight <- Mdl_Eval_DL(IDst,i,MetricsDaylight)
    Metrics <- Mdl_Eval(IDst,i,Metrics)
}
##### ************ compute mean by day for each station and write into a file  ************  #####
mean.day <- timeAverage(scat,avg.time="day",data.thresh=0,statistic="mean",type="OBS_SID")
write.table(mean.day, file = "mean_day_T_cdmx.csv", append = FALSE, quote = TRUE, sep = ",", eol = "\n", na = "NA", dec = ".", row.names = TRUE,col.names = TRUE, qmethod = c("escape", "double"))

##### ************ Write Metrics table into a csv file  ************  #####
write.table(Metrics, file = "Metrics_T_cdmx.csv", append = FALSE, quote = TRUE, sep = ",",
eol = "\n", na = "NA", dec = ".", row.names = TRUE,col.names = TRUE, qmethod = c("escape", "double"))
write.table(MetricsDaylight, file = "Metrics_dl_T_cdmx.csv", append = FALSE, quote = TRUE, sep = ",", eol = "\n", na = "NA", dec = ".", row.names = TRUE,col.names = TRUE, qmethod = c("escape", "double"))
