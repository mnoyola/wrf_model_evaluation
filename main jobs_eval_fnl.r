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

#set path

path<-'/Volumes/MIGUEL_HDD/BLC/sfclay_2/eval_all_mx/'
setwd(path)

#set libraries
Sys.setlocale("LC_TIME", "es_ES.UTF-8")
library("openair")
library("plyr")
require("lubridate") 
library("reshape2")
library("Metrics")
stationID= c("HGM","TLA","UAX","UIZ","ACO","CUA","FAC","MGH","XAL")
n <- length(stationID)

#source functions
source("../Ic.r")
source("../Mdl_Eval_DL.r")
source("../Mdl_Eval.r")

#set scripts for evaluation
#pdf("O3.pdf")
#source("job3_O3_eval_fnl.r")
#dev.off()
#pdf("CO.pdf")
#source("job6_CO_eval_fnl.r")
#dev.off()
#pdf("T.pdf")
#source("job1_T_eval_fnl.r")
#dev.off()
#pdf("VV.pdf")
#source("job_filter_UV_eval_fnl.r")
#dev.off()

pdf("NOX.pdf")
source("job4-5_NOX_eval_fnl.r")
dev.off()
