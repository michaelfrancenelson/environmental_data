acor<-function(Station1, num_of_lags=10, plotf=T) {
ts_length <- sum(is.na(Station1) == F)
ts_end <- length(Station1)
acor <- data.frame(lag_k = 0, 
                 sample_autocor = 0, 
                 limit_hi = 0, 
                 limit_lo = 0)

for (i in 0:num_of_lags) {
lagk <- i
Station1_t <- Station1[1:(ts_end-lagk)]
Station1_tplusk <- Station1[(1+lagk):ts_end]
Station1_mean <- mean(Station1, na.rm = T)
Station1_var <- var(Station1, na.rm = T)
acor[i+1, 1] <- lagk
acor[i+1, 2] <- (1/(ts_length-1)*
             (sum((Station1_t-Station1_mean)*
                  (Station1_tplusk-Station1_mean), na.rm = T)/
             Station1_var))
acor[i+1, 3] <- 1.96/sqrt(ts_length-lagk)
acor[i+1, 4] <- (-1.96)/sqrt(ts_length-lagk)
}

plot(c(0, num_of_lags), c(-1, 1), type = "n", 
     xlab = "time lag", ylab = "correlation")
lines(acor[, 1], acor[, 2], lwd = 2)
lines(acor[, 1], acor[, 3], lty = 2)
lines(acor[, 1], acor[, 4], lty = 2)
abline(h = 0)

return(acor)
}





ccor <- function (Station1, Station2, num_of_lags = 10, plotf = F) {
index <- is.na(Station1)  ==  F&is.na(Station2)  ==  F
ts_length <- sum(index)
ts_end <- length(Station1)
ccor <- data.frame(lag_k = 0, 
                 cross_cor = 0, 
                 limit_hi = 0, 
                 limit_lo = 0)
Station1_mean <- mean(Station1[index], na.rm = T)
Station2_mean <- mean(Station2[index], na.rm = T)
Station1_var <- sd(Station1[index], na.rm = T)
Station2_var <- sd(Station2[index], na.rm = T)

for (lagk in -num_of_lags:num_of_lags) {
     if (lagk >= 0) {
         Station1_tplusk <- Station1[(1 + lagk):ts_end]
         Station2_t <- Station2[1:(ts_end - lagk)]
     } else {
         Station1_tplusk <- Station1[1:(ts_end + lagk)]
         Station2_t <- Station2[(1 - lagk):ts_end]

     }
     ccor[num_of_lags + lagk + 1, 1] <- lagk
     ccor[num_of_lags + lagk + 1, 2] <- (1/(ts_length - 1)*
                     (sum((Station2_t - Station2_mean)*
                          (Station1_tplusk - Station1_mean), na.rm = T)/
                      Station1_var/Station2_var))
     ccor[num_of_lags + lagk + 1, 3] <- 1.96/sqrt(ts_length - abs(lagk))
     ccor[num_of_lags + lagk + 1, 4] <- (-1.96)/sqrt(ts_length - abs(lagk))
}
 if (plotf  ==  T) {
     plot(c(-num_of_lags, num_of_lags), c(-1, 1), type = "n", 
          xlab = "time lag", ylab = "correlation")
          lines(ccor[, 1], ccor[, 2], lwd = 2)
          lines(ccor[, 1], ccor[, 3], lty = 2)
          lines(ccor[, 1], ccor[, 4], lty = 2)
          abline(h = 0)
}
return(ccor)
}

