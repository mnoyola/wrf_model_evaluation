Mdl_Eval <- function(data,i,Metrics){

Metrics[i,1] <- mean(data[,"obs"])
Metrics[i,2] <- mean( data[,"wrfu"])
Metrics[i,3] <- mean( data[,"wrfnu"])
Metrics[i,4] <- sd(data[,"obs"])
Metrics[i,5] <- sd( data[,"wrfu"])
Metrics[i,6] <- sd( data[,"wrfnu"])
Metrics[i,7] <- rae(data[,"obs"], data[,"wrfu"]) #RAE urban
Metrics[i,8] <- rae(data[,"obs"], data[,"wrfnu"]) #RAE nurban
Metrics[i,9] <- mae(data[,"obs"], data[,"wrfu"]) #RAE urban
Metrics[i,10] <- mae(data[,"obs"], data[,"wrfnu"]) #RAE nurban
Metrics[i,11] <- bias(data[,"obs"], data[,"wrfu"])
Metrics[i,12] <- bias(data[,"obs"], data[,"wrfnu"])
Metrics[i,13] <- rmse(data[,"obs"], data[,"wrfu"]) # RMSE urban
Metrics[i,14] <- rmse(data[,"obs"], data[,"wrfnu"]) # RMSE urban
Metrics[i,15] <- summary(lm(data[,"obs"]~ data[,"wrfu"]))$r.squared # R2_urban
Metrics[i,16] <- summary(lm(data[,"obs"]~ data[,"wrfnu"]))$r.squared # R2_nurban
Metrics[i,17] <- Ic(data[,"obs"],data[,"wrfu"])
Metrics[i,18] <- Ic(data[,"obs"],data[,"wrfnu"]) # Ic urban

return(Metrics)

}
