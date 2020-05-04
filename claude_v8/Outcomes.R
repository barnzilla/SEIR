

WDir <- choose.dir("D:/PED/Coronavirus/SEIR model/age stratification") # this does not generate a trailing slash or backslash
cat(WDir)  # show folder chosen
setwd(WDir)


# Import output file
setwd(WDir)
getwd()
big_out <- read.csv('final modelSEIR_results_5agegrp.csv',header = TRUE)

###################################################################################
##Calculate ouputs#######################
#############################################




### Number of infected over time
################################


matplot(x = big_out[,c("time")], y = big_out[,c('Ism_iso4')], type = "l",
        xlab = "Time", ylab = "individuals", main = "Infected",
        lwd = 2, lty = 1, bty = "l", col = 2)



# Max number of cases and date of peak
maxInfect<-max(big_out$I_tot)
maxInfect
datemaxInfect<-big_out$time[big_out$I_tot==max(big_out$I_tot)]
datemaxInfect



### Number of new cases each day, over time (incidence)
#######################################################

n<-nrow(big_out)

#Calculate the incidence


# Incidents 
big_out$IncI <-NA
big_out$IncI[1]<-big_out$I_tot[1]
for(i in 2:nrow(big_out))
  big_out$IncI[i] <- 
  (big_out$L_tot[i-1]) * 
  (curr_param$value[curr_param$name=='sigma'])
# Total number of cases
sumInfect <- sum(big_out$IncI)
sumInfect

#Calculate the incidence max and the related date
maxInc<-max(big_out$IncI)
maxInc
datemaxInc<-big_out$time[big_out$IncI==max(big_out$IncI)]
datemaxInc

# max number of cases and the related time
maxItot<-max(big_out$I_tot)
maxItot
datemaxItot<-big_out$time[big_out$I_tot==max(big_out$I_tot)]
datemaxItot


#sum(big_out$L_tot * params[names(params) == "sigma"] )

# Outbreak duration (when no new cases)

big_out$cumI<-NA
big_out$cumI[1]<-big_out$IncI[1]
for(i in 2:n)
  big_out$cumI[i] <- (big_out$IncI[i] +big_out$cumI[i-1]) 

matplot(x = big_out[,c("time")], y = big_out[,c('cumI')], type = "l",
        xlab = "Time", ylab = "individuals", main = "Cumulative incidence",
        lwd = 2, lty = 1, bty = "l", col = 6)

datemaxCumI<-big_out$time[big_out$cumI==max(big_out$cumI)]
min(datemaxCumI)


#outbreak duration 2
big_out$IncIbelow1 <- NA
big_out$IncIbelow1 = big_out$IncI <1 & big_out$time > daymaxInc
epilength2 <- which.max(big_out$IncIbelow1)-1 # For a logical vector x with both FALSE and TRUE values, which.max(x) return the index of the first TRUE


# Attack rate 2
AR <- (max(big_out$S) - min(big_out$S))/max(big_out$S)*100
AR


### Hospitalized

big_out$Hosp <- big_out$I_ssh
matplot(x = big_out[,c("time")], y = big_out[,c('Hosp')], type = "l",
        xlab = "Time", ylab = "individuals", main = "Hospitalized",
        lwd = 2, lty = 1, bty = "l", col = 3)



maxHosp<-max(big_out$Hosp)
maxHosp<-max(big_out$I_ssh)
maxHosp
datemaxHosp<-big_out$time[big_out$Hosp==max(big_out$Hosp)]
datemaxHosp

#total number of hopitalized

big_out$newH<-NA
big_out$newH[1]<-big_out$I_ssh[1]
for(i in 2:300)
  big_out$newH[i] <- 
  (
    big_out$I_ss[i-1] + big_out$I_ssr[i-1])*
  (curr_param$value[curr_param$name=='kappa'])*
  (curr_param$value[curr_param$name=='feish']) +
  
  
  (curr_param$value[curr_param$name=='delta'])*
  (1-curr_param$value[curr_param$name=='alpha'])*
  (curr_param$value[curr_param$name=='epsilonq'])*
  big_out$I_aq[i-1]

big_out$I_ssh %>% plot()
sum(big_out$newH, na.rm = T)

big_out$newH2<-NA
big_out$newH2[1]<-big_out$I_ssh[1]
for(i in 2:300){
  big_out$newH2[i]<-big_out$I_ssh[i-1]*
    ((1-(curr_param$value[curr_param$name=='mu']))*
       (curr_param$value[curr_param$name=='nus']) + 
       (curr_param$value[curr_param$name=='mu'])*
       (curr_param$value[curr_param$name=='nud']))
  
}
sum(big_out$newH2, na.rm = T)



sum(big_out$newH2, na.rm = T)/sum(big_out$IncI)


#######Number of death
max(big_out$D)


#### Quarantined

big_out$Quarant <- big_out$I_aq

matplot(x = big_out[,c("time")], y = big_out[,c('Quarant')], type = "l",
        xlab = "Time", ylab = "individuals", main = "Quarantined",
        lwd = 2, lty = 1, bty = "l", col = 4)

#legend(250, 1000000, c("Hospitalized"), pch = 1, col = 3, bty = "n")


maxQuarant<-max(big_out$Quarant)
maxQuarant
datemaxQuarant<-big_out$time[big_out$Quarant==max(big_out$Quarant)]
datemaxQuarant


##### Isolated
big_out$Isolat <- big_out$I_smis + big_out$I_ssis

matplot(x = big_out[,c("time")], y = big_out[,c('Isolat')], type = "l",
        xlab = "Time", ylab = "individuals", main = "Isolated",
        lwd = 2, lty = 1, bty = "l", col = 5)


maxIsolat<-max(big_out$Isolat)
maxIsolat
datemaxIsolat<-big_out$time[big_out$Isolat==max(big_out$Isolat)]
datemaxIsolat




