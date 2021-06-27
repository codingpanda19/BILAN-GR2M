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

curP<-getwd()

# nacti data z myPath
fls <- list.files(paste0(curP,"/BasinObs"), pattern = '.rds', full.names = TRUE)
nm <- gsub(list.files(paste0(curP,"/BasinObs"), pattern = '.rds', full.names = FALSE),
           pattern = '.rds',
           replacement = '')

for (n in seq_along(nm)) {
                    assign(nm[n], readRDS(fls[n]))
}



#-----------------------------Hydrological Year----------------------------------------
library(Hmisc)
BObs<-ENS_BASIN
str(BObs)
dtf<-describe(BObs)

#-------------------------------------------------

HY<-ENS_BASIN%>% group_by(ENS) %>% 
                    transmute(P,T,R)


BB <- mutate(ENS_BASIN, year = as.POSIXlt(DTM)$year + 1900)
years <- group_by(BB, year)
years<-years%>% group_by(ENS,year)%>%
                    summarise(P= sum(P), 
          T= mean(T), 
          R= sum(R))


ENS6<-ENS_BASIN[1741:1891,]

#------------------------

dt=seq(from=as.Date("1971/01/16"),to=as.Date("1999/12/16"),by="month")
dt

year(dt[1])
ndt=length(dt)                   
yearHM=c()

for(i in 1:ndt){
                    if(month(dt[i])<4) yearHM[i] =year(dt[i])-1  
                    else yearHM[i] =year(dt[i])
}


year(dt)

#--------------------------------------------------------

Dt=data.table(BASIN=ENS_BASIN$BASIN,ENS=ENS_BASIN$ENS,DTM=yearHM,P=ENS_BASIN$P,T=ENS_BASIN$T,R=ENS_BASIN$R)
Dt

VV<-Dt[,.(P=sum(P),T=mean(T),R=sum(R)),by=.(ENS,DTM)]


E<-rbind(VV[2:30,],VV[32:60,],VV[62:90,],VV[92:120,],VV[122:150,])

WriteXLS::WriteXLS(E,'BasinObs/runMeanObs.xlsx') 

a<-E
a
library(caTools)
b=c(a$P)
b
mn<-data.table(b)
mn


runT<-E%>% group_by(ENS) %>% 
                    mutate(K2=runmean(T,k=2),K3=runmean(T,k=3),
                           K4=runmean(T,k=4),K5=runmean(T,k=5),
                           K6=runmean(T,k=6),K7=runmean(T,k=7))

runP<-E%>% group_by(ENS) %>% 
                    mutate(K2=runmean(P,k=2),K3=runmean(P,k=3),
                           K4=runmean(P,k=4),K5=runmean(P,k=5),
                           K6=runmean(P,k=6),K7=runmean(P,k=7))

runR<-E%>% group_by(ENS) %>% 
                    mutate(K2=runmean(R,k=2),K3=runmean(R,k=3),
                           K4=runmean(R,k=4),K5=runmean(R,k=5),
                           K6=runmean(R,k=6),K7=runmean(R,k=7))

WriteXLS::WriteXLS(runT,'BasinObs/runT.xlsx') 


q20R<-runR%>% group_by(ENS) %>% 
                    summarise(across(5:10,quantile, probs=0.2))

WriteXLS::WriteXLS(q20T,'BasinObs/q20T.xlsx') 

q80R<-runR%>% group_by(ENS) %>% 
                    summarise(across(5:10,quantile, probs=0.8))

q20P<-runP%>% group_by(ENS) %>% 
                    summarise(across(5:10,quantile, probs=0.2))
q80P<-runP%>% group_by(ENS) %>% 
                    summarise(across(5:10,quantile, probs=0.8))
q20T<-runT%>% group_by(ENS) %>% 
                    summarise(across(5:10,quantile, probs=0.2))
q80T<-runT%>% group_by(ENS) %>% 
                    summarise(across(5:10,quantile, probs=0.8))


q80R<-

runR%>%group_by(ENS)%>%
                    




filter_if(runR,across(5:10,~quantile,probs==0.8))

is_int<-function(x)all(quantile(x), probs>=0.8)

DYR<-runR%>% group_by(ENS ) %>% 
                    filter(across (5:10), =0)

filter(mtcars, across(starts_with("d"), ~ (.x %% 2) == 0))



DR<-runR[runR$k2<q20R$K2]
DR
DP<-p[p$P<q20P]
DP
DT<-t[t$T<q20T]
DT

Dryyear<-data.table(DP,DT,DR)
Dryyear
save<-saveRDS(Dryyear,'M_AV/WAM/R/CAL/bil_save/K7/DYK7.rds')

#------------------------------------------------------------------------------------------

q80R<-quantile(x=r$R, probs=0.8)
q80R

q80T<-quantile(x=t$T, probs=0.8)
q80T

q80P<-quantile(x=p$P, probs=0.8)
q80P

WR<-r[r$R>q80R]
WR
WP<-p[p$P>q80P]
WP
WT<-t[t$T>q80T]
WT

Wetyear<-data.table(WP,WT,WR)
Wetyear
