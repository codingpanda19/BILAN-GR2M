#----------Loading Packages---------------------------------------
   
load_pkg <- rlang::quos(rgdal,tidyverse,rgeos,data.table,raster,bilan,hydroGOF,
                        readxl,ggplot2,dplyr,airGR,DEoptim,lubridate, readxl,Hmisc,
                        sqldf,caTools)

invisible(lapply(lapply(load_pkg, rlang::quo_name),
                 library,
                 character.only = TRUE ))

#----------------------Importing Catchments data----------------------------------------------------------

#Setting Working directory
setwd('/Users/Doudou/Downloads/MB_data')


# set strings as factors to false
options(stringsAsFactors = F)

curP<-getwd()


# nacti data z myPath  
fls <- list.files(paste0(curP,"/INPUTS/GR2M/VAL/"), pattern = '.rds', full.names = TRUE)
nm <- gsub(list.files(paste0(curP,"/INPUTS/GR2M/VAL/"), pattern = '.rds', full.names = FALSE),
           pattern = '.rds',
           replacement = '')

for (n in seq_along(nm)) {
  assign(nm[n], readRDS(fls[n]))
  
}
  
/Users/Doudou/Downloads/MB_data/OUTPUTS/BILAN/CAL/GMB/gmbcalbp.rds

# nacti data z myPath 
fls <- list.files(paste0(curP,"/OUTPUTS/BILAN/CAL/GMB/"), pattern = '.rds', full.names = TRUE)
nm <- gsub(list.files(paste0(curP,"/OUTPUTS/BILAN/CAL/GMB/"), pattern = '.rds', full.names = FALSE),
           pattern = '.rds',
           replacement = '')

for (n in seq_along(nm)) {
  assign(nm[n], readRDS(fls[n]))
}


  
wassadou<-read_excel("BasinObs/GMB.xlsx")

saveRDS(wassadou,"BasinObs/wassadou.rds")



#-------------------------------BasinObs------------------------------------------------------

#P<-file.choose(new=F)
#readRDS(PD1)

PERIODS
ENS_BASIN
allMyPeriods


#------------------------MY QUANTILES -----------------------------------------------
# estimate lenth of times Series
# influence of calibration  period 
#9 x 3 years period 28 years
#9 values sum of precipitations
#highest and lowest picks 
#is there any impact of the content of my data of description of HM which im working ?
#Why calibrate? Describing HM = describe Hydrology
#Whats the HM capable to represent my data?
# Simulations are very closed to reality?
#aggregate on annual values (Hydrological Years)
#average flows and temperatures
#then the quantiles

#-----------------------------Hydrological Year----------------------------------------

#------------------------MODEL CALIBATION on Dry year---------------------------------------------------------

#B02 <- sqldf("select PERIODS.ID, PERIODS.K,KDG.DTM, KDG.P, KDG.T, KDG.R from PERIODS inner join 
            # KDG on KDG.DTM >=PERIODS.begDTM and KDG.DTM<=PERIODS.endDTM")

#saveRDS(B02,'BasinObs/B02.rds')

#BO01<-B01 %>% group_by(K) %>% 
  #filter(K==2)

#---------------------------------------------------------------------

period<-allMyPeriods%>% group_by(ENS) %>% 
filter(ENS=='GMB')


GMB


ngone<-function(x){
  GMB[DTM >= x$begDTM
  & DTM <= x$endDTM,]
}


      K7<-period%>%group_by(K)%>%
        filter(K==7)
      input7=list()
      for(i in 1:nrow(K7))
      {  
        input7[[i]]<-GMB[DTM >= K7$begDTM[[i]]
                         & DTM <= K7$endDTM[[i]],]
        input7
        print(input7)
      
    
      K6<-period%>%group_by(K)%>%
        filter(K==6)
      input6=list()
      for(i in 1:nrow(K6))
      {  
        input6[[i]]<-GMB[DTM >= K6$begDTM[[i]]
                         & DTM <= K6$endDTM[[i]],]
        input6
        print(input6)
      
      K5<-period%>%group_by(K)%>%
        filter(K==5)
      input5=list()
      for(i in 1:nrow(K5))
      {  
        input5[[i]]<-GMB[DTM >= K5$begDTM[[i]]
                         & DTM <= K5$endDTM[[i]],]
        input5
        print(input5)

      K4<-period%>%group_by(K)%>%
        filter(K==4)
      input4=list()
      for(i in 1:nrow(K4))
      {  
        input4[[i]]<-GMB[DTM >= K4$begDTM[[i]]
                         & DTM <= K4$endDTM[[i]],]
        input4
        print(input4)
        
      K3<-period%>%group_by(K)%>%
        filter(K==3)
      input3=list()
      for(i in 1:nrow(K3))
      {  
        input3[[i]]<-GMB[DTM >= K3$begDTM[[i]]
                         & DTM <= K3$endDTM[[i]],]
        input3
        print(input3)
        
      K2<-period%>%group_by(K)%>%
      filter(K==2)
      input2=list()
      for(i in 1:nrow(K2))
      {
      
        input2[[i]]<-GMB[DTM >= K2$begDTM[[i]]
                         & DTM <= K2$endDTM[[i]],]
        input2
        print(input2)
      
        
      }
       break 
        
          
      
         
          K2<-period%>%group_by(K)%>%
            filter(K==2)
          input=list()
          for(i in 1:nrow(K2))
          {
            
            input[[i]]<-GMB[DTM >= K2$begDTM[[i]]
                             & DTM <= K2$endDTM[[i]],]
            
          }
          input
          print(input)
          RESULT=list()
          NSE=list()   
          KGE=list()
          MAE=list()
          MSEE=list()
          RMSE=list()
          res=list()
          pet=list()
          for(j in seq_along(input)){
            j=1
            n_inp=length(input)
            startDTM[[1]]<-input[[1]]$DTM[1]
            startDTM
            
            calibrage=bil.new('m')
            bil.set.values(calibrage, input[1],
                           init_date = startDTM)
            PET<-bil.pet(calibrage, "latit", 13.48)
            print(PET)
          }
          }
            warmupCal<-input[[j]][1:12]
            KGEwarmUpCal<- function(sim){
              obst<-input[[j]]$R[-warmupCal]
              simt<-sim[-warmupCal]
              -1*hydroGOF::KGE(sim = simt, obs = obst)
              

            bil.set.params.lower(calibrage, list(Spa =100,Grd= 0.001, 
                                             Alf=0.00001))
            bil.set.params.upper(calibrage, list(Spa = 2000, Grd= 1,
                                             Alf=0.003))
            
            bil.set.optim(calibrage, method = "DE", crit = "NS", DE_type = "best_one_bin",n_comp = 4,
                          comp_size = 10, cross = 0.95, mutat_f = 0.95, 
                          mutat_k = 0.85, maxn_shuffles = 30,
                          n_gen_comp = 15, ens_count = 1, seed = 446, 
                          weight_BF = 0, init_GS = 5)
            bil.set.critvars(model = calibrage,
                             weights = c(1),
                             obs_vars = c('R'),
                             mod_vars=c("RM"),
                             obs_values=c(-1),
                             crits=c('custom'),
                             funs=c(KGEwarmUpCal))
            
            model<-bil.optimize(calibrage)   
            res=bil.get.values(calibrage)
            plot( res$vars$DTM,res$vars$R,type='b', lwd=2, col='darkblue')
            lines(resvars$DTM,res$vars$RM,type='b', lwd=1, col='brown')
            #bil.run(calibrage)
            RESULT[[j]]<-res
            print(RESULT)
            NSE[[j]]<-NSE(RESULT[[j]][["vars"]]$R[13:36],
                          RESULT[[j]][["vars"]]$RM[13:36])
            KGE[[j]]<-KGE(RESULT[[j]][["vars"]]$R[13:36],
                          RESULT[[j]][["vars"]]$RM[13:36])
            MAE[[j]]<-mae(RESULT[[j]][["vars"]]$R[13:36],
                          RESULT[[j]][["vars"]]$RM[13:36])
            RMSE[[j]]<-rmse(RESULT[[j]][["vars"]]$R[13:36],
                            RESULT[[j]][["vars"]]$RM[13:36])
          
              }
            
            }
          }
        }
      
      startDTM=list()
      for(j in 1:length(input)){
      startDTM[[j]]<-input[[j]]$DTM[1]
      print(startDTM)
      }
      
      mydata=list()
      for (j in 1:length(input)){
     mydata[[j]]<-data.frame( 
      P=input[[j]]$P,
      T=input[[j]]$T,
      R=input[[j]]$R
     )
      }
      
      Homer<-function(x) {
      b=bil.new('m', modif = 'critvars')
      bil.set.values(b, x,
                     init_date = startDTM[[j]])
      bil.pet(b, "latit", 13.48)
      table<-bil.get.values(b)
      
      warmup<-1:12
      KGEwarmUp<- function(sim){
        obst<-x$R[-warmup]
        simt<-sim[-warmup]
        -1*hydroGOF::KGE(sim = simt, obs = obst)}
      
      
      bil.set.params.lower(b, list(Spa =100,Grd= 0.001, 
                                   Alf=0.00001))
      bil.set.params.upper(b, list(Spa = 2000, Grd= 1,
                                   Alf=0.003))
      
      bil.set.optim(b, method = "DE", crit = "NS", DE_type = "best_one_bin",
                    n_comp = 4,comp_size = 10, cross = 0.95, mutat_f = 0.95, 
                    mutat_k = 0.85, maxn_shuffles = 30,
                    n_gen_comp = 15, ens_count = 1, seed = 446, 
                    weight_BF = 0, init_GS = 5)
    
      bil.set.critvars(model = b,
                       weights = c(1),
                       obs_vars = c('R'),
                       mod_vars=c("RM"),
                       obs_values=c(-1),
                       crits=c('custom'),
                       funs=c(KGEwarmUp))
      
      model<-bil.optimize(b)   
      res=bil.get.values(b)
      plot( res$vars$DTM,res$vars$R,type='b', lwd=2, col='darkblue')
      plot(res$vars$DTM,res$vars$RM,type='b', lwd=1, col='brown')
      }
      
      
      
     
Homapply<-input%>%
  lapply(Homer)      
        



{
 RESULT=list()
          NSE=list()   
          KGE=list()
          MAE=list()
          MSEE=list()
          RMSE=list()
          PET=list()
          for(j in seq_along(input)){
            #i=1
            n_inp=length(input)
            startDTM<-input[[j]]$DTM[1]
            startDTM
            model<-bil.new('m')
            bil.set.values(model, input,
                           init_date = startDTM)
            PET[[j]]<-bil.pet(model, "latit", 13.48)
            
            warmupCal<-input[[j]][1:12]
            KGEwarmUpCal<- function(sim){
              obst<-input[[j]]$R[-warmupCal]
              simt<-sim[-warmupCal]
              -1*hydroGOF::KGE(sim = simt, obs = obst)
            }
            
            bil.set.params.lower(model, list(Spa =100,Grd= 0.001, 
                                             Alf=0.00001))
            bil.set.params.upper(model, list(Spa = 2000, Grd= 1,
                                             Alf=0.003))
         
            bil.set.optim(model, method = "DE", crit = "NS", DE_type = "best_one_bin",n_comp = 4,
                         comp_size = 10, cross = 0.95, mutat_f = 0.95, 
                          mutat_k = 0.85, maxn_shuffles = 30,
                          n_gen_comp = 15, ens_count = 1, seed = 446, 
                          weight_BF = 0, init_GS = 5)
            
            model<-bil.optimize(model)   
            dt<-bil.get.values(model)
            plot( dt$vars$DTM,dt$vars$R,type='b', lwd=2, col='darkblue')
            lines(dt$vars$DTM,dt$vars$RM,type='b', lwd=1, col='brown')
            bil.run(model)
            res=bil.get.values(model)
            RESULT[[j]]<-res
            print(RESULT)
          }
    }
} 

NSE[[j]]<-NSE(RESULT[[j]][["vars"]]$R[13:36],
    RESULT[[j]][["vars"]]$RM[13:36])
KGE[[j]]<-KGE(RESULT[[j]][["vars"]]$R[13:36],
    RESULT[[j]][["vars"]]$RM[13:36])
MAE[[j]]<-mae(RESULT[[j]][["vars"]]$R[13:36],
    RESULT[[j]][["vars"]]$RM[13:36])
RMSE[[j]]<-rmse(RESULT[[j]][["vars"]]$R[13:36],
    RESULT[[j]][["vars"]]$RM[13:36])

                      
                  }
          
   
       }



model<-bil.optimize(model)           
res = bil.get.values(model)
resEns<-bil.get.ens.resul(model)
resEns
            res
            max(res$crit)
            params<-bil.get.values(model)$params
            params
            dt<-bil.get.values(model)$params
            plot( dt$vars$DTM,dt$vars$R,type='b', lwd=2, col='darkblue')
            lines(dt$vars$RM, dt$vars$DTM,type='b', lwd=1, col='brown')
          }
          
      }
            
bil.set.critvars(model = model,
                 weights = c(1),
                 obs_vars = c('R'),
                 mod_vars=c("RM"),
                 obs_values=c(-1),
                 crits=c('custom'),
                 funs=c(KGEwarmUpCal))

            
            dt<-bil.get.values(model)
            plot( dt$vars$DTM,dt$vars$R,type='b', lwd=2, col='darkblue')
            lines(dt$vars$RM, dt$vars$DTM,type='b', lwd=1, col='brown')
            bil.run(model)
            res=bil.get.values(model)
            RESULT[[j]]<-res
            print(RESULT)
            
           
            
  }
}




  warmupCal<-1:12
  KGEwarmUpCal<- function(sim){
    obst<-BasinObs$R[-warmupCal]
    simt<-sim[-warmupCal]
    -1*hydroGOF::KGE(sim = simt, obs = obst)
  }
  
  bil.set.params.lower(model, list(Spa =100,Grd= 0.001, Alf=0.00001))
  bil.set.params.upper(model, list(Spa = 2000, Grd= 1, Alf=0.003))
  bil.set.critvars(model = model,
                   weights = c(1),
                   obs_vars = c('R'),
                   mod_vars=c("RM"),
                   obs_values=c(-1),
                   crits=c('custom'),
                   funs=c(KGEwarmUpCal))
  
  bil.set.optim(model, method = "DE", crit = "NS", DE_type = "best_one_bin", n_comp = 4,
                comp_size = 10, cross = 0.95, mutat_f = 0.95, mutat_k = 0.85, maxn_shuffles = 30,
                n_gen_comp = 15, ens_count = 1, seed = 446, weight_BF = 0, init_GS = 5)
  
  #--------------------------OPTIMIZATION----------------------------------------------------------
  
  model<-bil.optimize(model)
  res = bil.get.values(model)
  resEns<-bil.get.ens.resul(model)
  resEns
  res<-as.data.table(res)
  res
  max(res$crit)
  params<-bil.get.values(model)$params
  params
  bil.get.values(model)$params
}











for(i in 1:length(headws)){
  i=1
  dta=BObs
  headws[i]
  
  myUP1dat<-dta
  print(range(myUP1dat$DTM))
  print(i)
   #}
  b = bil.new("m",'critvars')
  input = data.frame(
    P =myUP1dat$P,
    R = myUP1dat$R,
    T = myUP1dat$T
    )
  
  calibrage = bil.new("m",modif = 'critvars')
  begDTM=range(PERIODS$begDTM)
  bil.set.values(calibrage, input,
                 init_date = begDTM)
  bil.pet(calibrage, "latit", 13.48)
  table<-bil.get.values(calibrage)
  dt<-as.data.table(table)
  BasinObs<-cbind(BasinObs,PET=dt$vars.PET)
  BasinObs<-as.data.table(BasinObs)
  Obs<-plot(table$vars$R,type='b', lwd=1, col='limegreen')
  
  params<- bil.get.params(b)
   input
}









BasinObs<-BObs
calibrage = bil.new("m",modif = 'critvars')
begDTM=range(PERIODS$begDTM)
bil.set.values(calibrage, BasinObs,
               init_date = begDTM)
bil.pet(calibrage, "latit", 13.48)
table<-bil.get.values(calibrage)
dt<-as.data.table(table)
BasinObs<-cbind(BasinObs,PET=dt$vars.PET)
BasinObs<-as.data.table(BasinObs)
Obs<-plot(table$vars$R,type='b', lwd=1, col='limegreen')

begDTM
begDTM<-substring(begDTM,1,7)
endDTM<-last(BasinObs$DTM)
endDTM
endDTM<-substring(endDTM,1,4)
endDTM<-substring(endDTM,3)
endDTM

begWarm<-range(BasinObs$DTM)[1]
begWarm<-substring(begWarm,1,7)
begWarm

endWarm<-(BasinObs$DTM)[12]
endWarm<-substring(endWarm,1,7)
endWarm

begRun<-(BasinObs$DTM)[13]
begRun<-substring(begRun,1,7)
begRun

endRun<-last(BasinObs$DTM)
endRun<-substring(endRun,1,7)
endRun
#-----------------------------------------------------------------------------

#-----------------------------------WARM UP----------------------------------------------

warmupCal<-1:12
KGEwarmUpCal<- function(sim){
  obst<-BasinObs$R[-warmupCal]
  simt<-sim[-warmupCal]
  -1*hydroGOF::KGE(sim = simt, obs = obst)
}

#---------------------PARAMETER SET-------------------------------------------------------------

bil.set.params.lower(calibrage, list(Spa =100,Grd= 0.001, Alf=0.00001))
bil.set.params.upper(calibrage, list(Spa = 2000, Grd= 1, Alf=0.003))
bil.set.critvars(model = calibrage,
                 weights = c(1),
                 obs_vars = c('R'),
                 mod_vars=c("RM"),
                 obs_values=c(-1),
                 crits=c('custom'),
                 funs=c(KGEwarmUpCal))

bil.set.optim(calibrage, method = "DE", crit = "NS", DE_type = "best_one_bin", n_comp = 4,
              comp_size = 10, cross = 0.95, mutat_f = 0.95, mutat_k = 0.85, maxn_shuffles = 30,
              n_gen_comp = 15, ens_count = 1, seed = 446, weight_BF = 0, init_GS = 5)

#--------------------------OPTIMIZATION----------------------------------------------------------

model<-bil.optimize(calibrage)
res = bil.get.values(calibrage)
resEns<-bil.get.ens.resul(calibrage)
resEns
res<-as.data.table(res)
res
max(res$crit)
params<-bil.get.values(calibrage)$params
params
bil.get.values(calibrage)$params

model

#NSE(obs=model$R,sim=model$RM)
NSE<-NSE(obs=model$R[13:36],sim=model$RM[13:36])
KGE<-KGE(obs=model$R[13:96],sim=model$RM[13:96])
MAE<-mae(obs=model$R[13:96],sim=model$RM[13:96])
MSE<-mse(obs=model$R[13:96],sim=model$RM[13:96])
RMSE<-rmse(obs=model$R[13:96],sim=model$RM[13:96])

begDTM<-substring(begDTM,1,4)
begDTM
k2<-paste(begDTM,endDTM, sep="-")
k2
OBJ<-data.table(k2,NSE,KGE,MSE,RMSE)
OBJ
saveRDS(OBJ,'M_AV/GMB/GR2M/CAL/Obj_Fun/k2/RW1.rds')
bil.write.file(Val, "M_AV/GMB/VAL/bil_save/K6/D4W2.txt")

curP<-getwd()
# nacti data z myPath
fls <- list.files(paste0(curP,"/M_AV/GMB/R/CAL/Obj_Fun/K3"), pattern = '.rds', full.names = TRUE)
nm <- gsub(list.files(paste0(curP,"/M_AV/GMB/R/CAL/Obj_Fun/K3"), pattern = '.rds', full.names = FALSE),
           pattern = '.rds',
           replacement = '')

for (n in seq_along(nm)) {
  assign(nm[n], readRDS(fls[n]))
}

RB<-rbind(PD1,PD2,PD3,PD4,PW1,PW2,PW3)
RB

str(PD1)

boxplot(PD1)
boxplot(RB$KGE,xlab=RB$k2)
boxplot(PD1$KGE,xlab=PD1$k2)

#----------------------------Graphs-------------------------------------------------------

ggplot(data = model[13:36]) +
  geom_line(aes(x = DTM, y = R, colour = factor(1))) +
  geom_line(aes(x = DTM, y = RM, colour = factor(2))) +
  geom_line(aes(x = DTM, y = BF, colour = factor(3))) +
  scale_color_manual(values = c('darkblue', 'red', 'yellow'),
                     labels = c('R', 'RM', 'BF'),
                     name = '') +
  theme_bw()


#plot
plot(x=model$DTM, y=res$vars.R,type='l', lwd=1.5, col='darkblue')
lines(x=model$DTM, y=res$vars.RM,type='l',lwd=1.5, col='brown')
lines(x=model$DTM, y=res$vars.BF, type='l', lwd=1.5, col='limegreen')

#----------------------Validation on Wet Year---------------------------------------
