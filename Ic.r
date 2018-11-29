Ic <- function(obs,wrf){
o <- obs
p <- wrf
om <- mean(o)
num <- sum(abs(p-o))
den <- 2*(sum(abs(o-om)))
if(is.finite(num) && is.finite(den)){
if (num <= den){ dr <- 1-(num/den)
} else {dr <-(den/num)-1 }
} else {dr <- NA}
return(dr)
}
