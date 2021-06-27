#----------Loading Packages----------------------------------------

load_pkg <- rlang::quos(rgdal,tidyverse,rgeos,data.table,raster,bilan,hydroGOF,
                        readxl,ggplot2,dplyr,airGR,DEoptim,lubridate, readxl)

invisible(lapply(lapply(load_pkg, rlang::quo_name),
                 library,
                 character.only = TRUE ))

#----------------------Importing Catchments data----------------------------------------------------------

#Setting Working directory
setwd('/Users/Doudou/Downloads/MB_data')


# set strings as factors to false
options(stringsAsFactors = F)

test<-rbind(KGEbilcal,KGEcalGr)
order<-test%>%arrange(K)
#-------------------------------BasinObs------------------------------------------------------

library(tcltk)
GMB<- tk_choose.files()
GMB<-readRDS(GMB)

#-----------------------------Hydrological Year----------------------------------------
library(Hmisc)
BObs<-GMB
#str(BObs)
#dtf<-describe(BObs)

#-----------------------SELECTION OF PERIODS -------------------------------------------------
curP<-getwd()
fls <- list.files(paste0(curP,'/M_AV/GMB/BILAN/CAL/bil_save/K2'), pattern = '.rds', full.names = TRUE)
nm <- gsub(list.files(paste0(curP,'/M_AV/GMB/BILAN/CAL/bil_save/K2'), pattern = '.rds', full.names = FALSE),
           pattern = '.rds',
           replacement = '')

for (n in seq_along(nm)) {
  assign(nm[n], readRDS(fls[n]))
}

#--------------------------parsmat---------------------------------

library(tcltk)
Params.curr<- tk_choose.files()
Params.curr<-read_excel(Params.curr)
Params.curr<-data.frame(Params.curr)
P<-Params.curr[,-1]
parsmat<-Params.curr
parsmat

RDYK4
RWYK4

#----------------------------Validation-----------------------------------------
PDY<-BObs[229:288]
RWY
PDYval<-PDY[1:36]
RWYval<-RWY[1:36]
Bval<-PDYval
Bval


b = bil.new("m")
begDTM=range(Bval$DTM)[1]
begDTM
bil.set.values(b,init_date = begDTM, Bval)
bil.pet(b, "latit", 13.48)
table<-bil.get.values(b)
dt<-as.data.table(table)
BasinObs<-cbind(Bval,PET=dt$vars.PET)
BasinObs001<-as.data.table(BasinObs)
BasinObs
Obs<-plot(table$vars$R,type='l', lwd=1, col='darkblue')
Bval



#-----------------------------------WARM UP VAL----------------------------------------------

warmupVal<-1:12
KGEwarmUpVal<- function(sim){
  obst<-BasinObs$R[-warmupVal]
  simt<-sim[-warmupVal]
  -1*hydroGOF::KGE(sim = simt, obs = obst)
}
#---------------------------------------------------------------------------------------


GetEnsVars={
  n_params = ncol(parsmat)
   print(n_params)
  n_ens=nrow(parsmat)
   print(parsmat)
  if(n_params == 4){
    rmatDat=list()
    for(i in 1:n_ens){
      #i=1
      bil.set.params.curr(b,list(Spa = parsmat$Spa[i],
                            Grd = parsmat$Grd[i],
                            Alf = parsmat$Alf[i]))
      bil.run(b)
      res=bil.get.values(b)
      res= data.table(res$vars)
      res[,ens:=i]
      # print(res)
      rmatDat[[i]]=res
    }
    rmatDat = rbindlist(rmatDat)
  }
  rmatDat
  print(rmatDat)
  
}


bil.set.params.curr(b,list(Spa =100.0000 ,
                           Grd =0.0657266 ,
                           Alf =0.002449900 ))
bil.run(b)
res=bil.get.values(b)
res= data.table(res$vars)
res
KGE = KGE(res$R[13:36], res$RM[13:36])
KGE


critvars<-rmatDat %>% group_by(ens) %>% 
  mutate(KGE = KGE(R[13:36], RM[13:36]),
         NSE = NSE(R[13:36], RM[13:36]),
         MAE = mae(R[13:36], RM[13:36]),
         MSE = mse(R[13:36], RM[13:36]),
         RMSE = rmse(R[13:36], RM[13:36]))%>%
  ungroup

only_crits<-data.table(ens=critvars$ens,KGE=critvars$KGE,NSE=critvars$NSE,MAE=critvars$MAE,MSE=critvars$MSE,RMSE=critvars$RMSE)
only_crits

period<-data.table(rbind('PD1','PD2','PD3','PW1','PW2','PW3','PW4','PW5','RD1','RD2','RD3','RW1',
                         'TD1','TD2','TD3','TD4','TW1'))
period
agg_crits<-aggregate(cbind(KGE,NSE,MAE,MSE,RMSE)~ens,only_crits,mean)
agg_crits<-cbind(period=period$V1,agg_crits)
agg_crits<-agg_crits[-2]
agg_crits
saveRDS(agg_crits,'M_AV/GMB/BILAN/VAL/bil_save/K2/VRW1K2.rds')

boxplot(agg_crits$KGE)
# Plot 

plot(critvars$R[13:36], type='l',col='darkblue',lwd=1.5)
lines(critvars$RM[13:36], col='red',lwd=1.5)

