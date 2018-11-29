Mdl_Eval_DL <- function(data,i,MetricsDaylight){

day <- selectByDate(data, hour = 07:18)
night1 <- selectByDate(data, hour = 19:23)
night2 <- selectByDate(data, hour = 00:06)
night <- rbind(night1,night2)

#mean day
MetricsDaylight[i,1] <- mean(day[,"obs"])
MetricsDaylight[i,2] <- mean(day[,"wrfu"])
MetricsDaylight[i,3] <- mean(day[,"wrfnu"])
#mean night
MetricsDaylight[i,4] <- mean(night[,"obs"])
MetricsDaylight[i,5] <- mean(night[,"wrfu"])
MetricsDaylight[i,6] <- mean(night[,"wrfnu"])
#stdev day
MetricsDaylight[i,7] <- sd(day[,"obs"])
MetricsDaylight[i,8] <- sd(day[,"wrfu"])
MetricsDaylight[i,9] <- sd(day[,"wrfnu"])
#stdev night
MetricsDaylight[i,10] <- sd(night[,"obs"])
MetricsDaylight[i,11] <- sd(night[,"wrfu"])
MetricsDaylight[i,12] <- sd(night[,"wrfnu"])
#rmse day
MetricsDaylight[i,13] <- rmse(day[,"obs"], day[,"wrfu"])
MetricsDaylight[i,14] <- rmse(day[,"obs"], day[,"wrfnu"])
#rmse night
MetricsDaylight[i,15] <- rmse(night[,"obs"], night[,"wrfu"])
MetricsDaylight[i,16] <- rmse(night[,"obs"], night[,"wrfnu"])
#R2 day
MetricsDaylight[i,17] <- summary(lm(day[,"obs"]~ day[,"wrfu"]))$r.squared # R2_urban
MetricsDaylight[i,18] <- summary(lm(day[,"obs"]~ day[,"wrfnu"]))$r.squared # R2_nurban
#R2 night
MetricsDaylight[i,19] <- summary(lm(night[,"obs"]~ night[,"wrfu"]))$r.squared # R2_urban
MetricsDaylight[i,20] <- summary(lm(night[,"obs"]~ night[,"wrfnu"]))$r.squared # R2_nurban
#Ic day
MetricsDaylight[i,21] <- Ic(day[,"obs"],day[,"wrfu"]) # Ic urban
MetricsDaylight[i,22] <- Ic(day[,"obs"],day[,"wrfnu"]) # Ic nurban
#Ic day
MetricsDaylight[i,23] <- Ic(night[,"obs"],night[,"wrfu"]) # Ic urban
MetricsDaylight[i,24] <- Ic(night[,"obs"],night[,"wrfnu"]) # Ic nurban

return(MetricsDaylight)

}
