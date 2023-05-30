library(dplyr)
library(data.table)
library(tidyr)
library(readxl)
library(lubridate)
#PROCESSING AMERIFLUX BASE FILES
setwd("/Users/iwargowsky/Desktop/Ameriflux/AMF-BASE")
path <- "/Users/iwargowsky/Desktop/Ameriflux/AMF-BASE"
files <- list.files(path = path,pattern = '*_HH_',all.files = T,recursive = T)
#load in files as a list of df
base.dat <-  lapply(files,function(i){
  fread(i, na.strings =c("NA","-9999"), skip= 2)
})
#add year, month, day columns
base <- lapply(base.dat, function(df) df %>%
                 mutate( year = substr(df$TIMESTAMP_START, 1,4),
                         month = substr(df$TIMESTAMP_START, 5,6)))
#get comprehensive list of all columns names in each df
###CA-ARB####---------------------------------------------------------------------------
colnames(base[[1]]) 
#date range covered by Ameriflux
###CA-ARF####---------------------------------------------------------------------------
colnames(base[[2]])
#date range covered by Ameriflux
###CA-CF1####---------------------------------------------------------------------------
colnames(base[[3]])
#NO NEE, GPP, RECO, or CH4 so excluding this site
###CA-CF2####---------------------------------------------------------------------------
colnames(base[[4]])  
base[[4]] <- base[[4]] %>% group_by(year, month) %>%
  dplyr::summarise(percent_na = (sum(is.na(FCH4))/n()*100),
                  TA= mean(TA, na.rm= FALSE),
                   PPFD= mean(PPFD_IN, na.rm= FALSE),
                   P= sum(P_RAIN, na.rm= FALSE),
                   TS= mean(c(TS_1_1_1, TS_2_1_1, TS_3_1_1), na.rm= FALSE),
                   FCH4= mean(FCH4, na.rm = FALSE))
#not gapfilled
###CA-Gro####---------------------------------------------------------------------------
colnames(base[[5]])
#date range covered by Ameriflux
###CA-HPC####---------------------------------------------------------------------------
colnames(base[[6]])
base[[6]] <- base[[6]] %>% group_by(year, month) %>%
  dplyr::summarise(percent_na = (sum(is.na(NEE_PI_F))/n()*100),
                   SC= mean(SC, na.rm= FALSE),
                   TA= mean(TA, na.rm= FALSE),
                   TS= mean(TS_1_1_1, na.rm= FALSE),
                   PPFD= mean(PPFD_IN, na.rm= FALSE),
                   P= sum(P, na.rm= FALSE),
                   D_SNOW= mean(D_SNOW, na.rm= FALSE),
                   FCH4= mean(FCH4_PI_F, na.rm= FALSE),
                   SCH4= mean(SCH4, na.rm= FALSE),
                   NEE = mean(NEE_PI_F, na.rm= FALSE),
                   GPP= mean(GPP_PI_F, na.rm= FALSE),
                   RECO= mean(RECO_PI_F, na.rm= FALSE))
#NEE and RECO look mostly gapfilled, GPP is not gapfilled 
###CA-Man####---------------------------------------------------------------------------
colnames(base[[7]])
base[[7]] <- base[[7]] %>% group_by(year, month) %>%
  dplyr::summarise(percent_na = (sum(is.na(NEE_PI))/n()*100),
                   TA= mean(TA, na.rm= FALSE),
                   NEE= mean(NEE_PI, na.rm= FALSE),
                   SC= mean(SC, na.rm= FALSE),
                   TS= mean(TS_1, na.rm= FALSE),
                   P=sum(P, na.rm= FALSE),
                   SWC= mean(SWC_1, na.rm= FALSE),
                   PPFD= mean(PPFD_IN, na.rm= FALSE),
                   RECO= mean(RECO_PI, na.rm= FALSE))
base[[7]]$GPP <- base[[7]]$NEE-base[[7]]$RECO
###CA-NS1####---------------------------------------------------------------------------
colnames(base[[8]])
#date range covered by Fluxnet2015
###CA-NS2####---------------------------------------------------------------------------
colnames(base[[9]])
#date range covered by Fluxnet2015
###CA-NS3####---------------------------------------------------------------------------
colnames(base[[10]])
#date range covered by Fluxnet2015
###CA-NS4####---------------------------------------------------------------------------
colnames(base[[11]])
#date range covered by Fluxnet2015
###CA-NS5####---------------------------------------------------------------------------
colnames(base[[12]])
#date range covered by Fluxnet2015
###CA-NS6####---------------------------------------------------------------------------
colnames(base[[13]])
#date range covered by Fluxnet2015
###CA-NS7####---------------------------------------------------------------------------
colnames(base[[14]])
#date range covered by Fluxnet2015
###CA-NS8####-----------------------EMPTY DATA FRAME----------------------------------------------------
colnames(base[[15]])
#base[[15]] <- base[[15]] %>% group_by(year, month) %>%
  #dplyr::summarise(percent_na = (sum(is.na(NEE_PI))/n()*100),
                   #TA= mean(TA, na.rm= FALSE),
                  # NEE= mean(NEE_PI, na.rm= FALSE),
                  # SC= mean(SC, na.rm= FALSE),
                   #TS= mean(TS_1, na.rm= FALSE),
                  # P= sum(P, na.rm= FALSE),
                  # SWC= mean(SWC_1, na.rm= FALSE),
                  # PPFD= mean(PPFD_IN, na.rm= FALSE),
                  # RECO= mean(RECO_PI, na.rm= FALSE))
#base[[15]]$GPP <- base[[15]]$NEE-base[[15]]$RECO
###CA-Oas####---------------------------------------------------------------------------
colnames(base[[16]])
#date range covered by Fluxnet2015
###CA-Obs####---------------------------------------------------------------------------
colnames(base[[17]])
#date range covered by Fluxnet2015
###CA-Ojp####---------------------------------------------------------------------------
colnames(base[[18]]) 
base[[18]] <- base[[18]] %>% group_by(year, month) %>%
  dplyr::summarise(percent_na = (sum(is.na(NEE_PI))/n()*100),
                   SC= mean(c(SC_1_1_1, SC_1_1_2)),
                   NEE= mean(NEE_PI),
                   RECO= mean(RECO_PI),
                   GPP= mean(GPP_PI),
                   PPFD= mean(c(PPFD_IN_1_1_1, PPFD_IN_1_1_2)),
                   P= sum(c(P_1_1_1, P_1_1_2), na.rm=FALSE),
                   D_SNOW= mean(c(D_SNOW_1_1_1, D_SNOW_1_1_2)),
                   TS= mean(c(TS_1_1_1, TS_1_1_2), na.rm=FALSE),
                   TA= mean(c(TA_1_1_1, TA_1_1_2), na.rm=FALSE),
                   SWC= mean(SWC_1_1_1))
#Not gapfilled 
#Arctic data center is also not gap filled and same dates
###CA-Qc2####---------------------------------------------------------------------------
colnames(base[[19]])
#date range covered by Ameriflux
###CA-Qfo####---------------------------------------------------------------------------
colnames(base[[20]]) 
#date range covered by Fluxnet2015 and Ameriflux
###CA-SCB####---------------------------------------------------------------------------
colnames(base[[21]])
base[[21]] <- base[[21]] %>% group_by(year, month) %>%
  dplyr::summarise(percent_na = (sum(is.na(NEE_PI_F))/n()*100),
                   SC= mean(SC, na.rm=FALSE),
                   TA= mean(c(TA_1_1_1, TA_1_1_2), na.rm=FALSE),
                   TS= mean(TS_1_1_1, na.rm=FALSE),
                   P= sum(P_RAIN, na.rm=FALSE),
                   D_SNOW= mean(D_SNOW, na.rm=FALSE),
                   FCH4= mean(FCH4_PI_F, na.rm=FALSE),
                   SCH4= mean(SCH4, na.rm=FALSE),
                   NEE= mean(NEE_PI_F, na.rm=FALSE),
                   GPP= mean(GPP_PI, na.rm=FALSE),
                   RECO= mean(RECO_PI, na.rm=FALSE))
#gapfilled
#fluxnetch4 covers until 2017
###CA-SCC####---------------------------------------------------------------------------
colnames(base[[22]])
#date range covered by FluxnetCH4
###CA-SF1####---------------------------------------------------------------------------
colnames(base[[23]])
#date range covered by Fluxnet2015 and Ameriflux
###CA-SF2####---------------------------------------------------------------------------
colnames(base[[24]])
#date range covered by Ameriflux and Fluxnet2015 
###CA-SF3####---------------------------------------------------------------------------
colnames(base[[25]])
#date range covered by Fluxnet2015 
###CA-SJ1####---------------------------------------------------------------------------
colnames(base[[26]])
#NO NEE, GPP, RECO, or CH4 so excluding this site
###CA-SJ2####---------------------------------------------------------------------------
colnames(base[[27]])
#date range covered by Ameriflux 
###CA-SJ3####---------------------------------------------------------------------------
colnames(base[[28]])
base[[28]] <- base[[28]] %>% group_by(year, month) %>%
  dplyr::summarise(percent_na = (sum(is.na(NEE_PI_F))/n()*100),
                   NEE= mean(NEE_PI_F, na.rm=FALSE),
                   RECO= mean(RECO_PI_F, na.rm=FALSE),
                   GPP= mean(GPP_PI_F, na.rm=FALSE),
                   NEE= mean(NEE_PI_F, na.rm=FALSE),
                   PPFD= mean(PPFD_IN_PI_F_1_1_1, na.rm=FALSE),
                   TA= mean(c(TA_PI_F_1_1_1, TA_PI_F_1_1_2), na.rm=FALSE),
                   D_SNOW= mean(D_SNOW_PI_F, na.rm=FALSE),
                   TS= mean(c(TS_PI_F_1_1_1, TS_PI_F_2_1_1), na.rm=FALSE),
                   SWC= mean(SWC_PI_F_1, na.rm=FALSE))
# gapfilled only 2004-2006
###CA-SMC####---------------------------------------------------------------------------
colnames(base[[29]])
base[[29]] <- base[[29]] %>% group_by(year, month) %>%
  dplyr::summarise(percent_na = (sum(is.na(NEE_PI_F))/n()*100),
                   SC= mean(SC, na.rm=FALSE),
                   TA= mean(c(TA_1_1_1, TA_2_1_1), na.rm=FALSE),
                   TS= mean(TS_1_1_1, na.rm=FALSE),
                   PPFD= mean(PPFD_IN, na.rm=FALSE),
                   FCH4= mean(FCH4_PI_F, na.rm=FALSE),
                   SCH4= mean(SCH4, na.rm=FALSE),
                   NEE= mean(NEE_PI_F, na.rm=FALSE),
                   GPP= mean(GPP_PI, na.rm=FALSE),
                   RECO= mean(RECO_PI, na.rm=FALSE))
#Only NEE is gapfilled starting mid april - mid november
###CA-TVC####---------------------------------------------------------------------------
colnames(base[[30]])
base[[30]] <- base[[30]] %>% group_by(year, month) %>%
  dplyr::summarise(percent_na = (sum(is.na(NEE_PI_F))/n()*100),
                   SC= mean(SC, na.rm=FALSE),
                   TA= mean(TA_PI_F, na.rm=FALSE),
                   P= sum(P, na.rm=FALSE),
                   D_SNOW= mean(D_SNOW, na.rm=FALSE),
                   FCH4= mean(FCH4_PI_F, na.rm=FALSE),
                   SCH4= mean(SCH4, na.rm=FALSE),
                   NEE= mean(NEE_PI_F, na.rm=FALSE),
                   GPP= mean(GPP_PI, na.rm=FALSE),
                   RECO= mean(RECO_PI, na.rm=FALSE))
#Only NEE is gapfilled starting mid april - mid november
###CA-WP1####---------------------------------------------------------------------------
colnames(base[[31]])
#date range covered by Ameriflux 
###CA-WP2####---------------------------------------------------------------------------
colnames(base[[32]])
#date range covered by Ameriflux 
###CA-WP3####---------------------------------------------------------------------------
colnames(base[[33]])
#date range covered by Ameriflux 
###US-A03####---------------------------------------------------------------------------
colnames(base[[34]])
base[[34]] <- base[[34]] %>% group_by(year, month) %>%
  dplyr::summarise(percent_na = (sum(is.na(NEE_PI))/n()*100),
                   NEE= mean(NEE_PI, na.rm=FALSE),
                   FCH4= mean(FCH4, na.rm=FALSE),
                   PPFD= mean(PPFD_IN, na.rm=FALSE),
                   TS= mean(TS_PI_1_1_A, na.rm=FALSE),
                   SWC= mean(SWC_PI_1_1_A, na.rm=FALSE))
#not gapfilled, 2015-2018 in fluxnetch4
###US-A10####---------------------------------------------------------------------------
colnames(base[[35]])
base[[35]] <- base[[35]] %>% group_by(year, month) %>%
  dplyr::summarise(percent_na = (sum(is.na(NEE_PI))/n()*100),
                   NEE= mean(NEE_PI, na.rm=FALSE),
                   FCH4= mean(FCH4, na.rm=FALSE),
                   PPFD= mean(PPFD_IN, na.rm=FALSE),
                   TS= mean(TS_PI_1_1_A, na.rm=FALSE),
                   SWC= mean(SWC_PI_1_1_A, na.rm=FALSE))
#not gapfilled, 2012-2018 in fluxnetch4
###US-An1####---------------------------------------------------------------------------
colnames(base[[36]])
#NO NEE, GPP, RECO, or CH4 so excluding this site
###US-An2####---------------------------------------------------------------------------
colnames(base[[37]])
#NO NEE, GPP, RECO, or CH4 so excluding this site
###US-An3####---------------------------------------------------------------------------
colnames(base[[38]])
#NO NEE, GPP, RECO, or CH4 so excluding this site
###US-Atq####------------------NO NEE DATA---------------------------------------------------------
colnames(base[[39]])
base[[39]] <- base[[39]] %>% group_by(year, month) %>%
  dplyr::summarise(percent_na = (sum(is.na(NEE_PI))/n()*100),
                   TA= mean(TA, na.rm=FALSE),
                   NEE= mean(NEE_PI, na.rm=FALSE),
                   SC= mean(SC, na.rm=FALSE),
                   TS= mean(TS_1, na.rm=FALSE),
                   P= sum(P, na.rm=FALSE),
                   SWC= mean(SWC_1, na.rm=FALSE),
                   PPFD= mean(PPFD_IN, na.rm=FALSE),
                   RECO= mean(RECO_PI, na.rm=FALSE))
base[[39]]$GPP <- base[[39]]$NEE-base[[39]]$RECO
#Arctic data center is also not gap filled and same dates
###US-Bn1####-----------------------NO NEE DATA----------------------------------------------------
colnames(base[[40]])
base[[40]] <- base[[40]] %>% group_by(year, month) %>%
  dplyr::summarise(percent_na = (sum(is.na(NEE_PI))/n()*100),
                   TA= mean(TA, na.rm=FALSE),
                   NEE= mean(NEE_PI, na.rm=FALSE),
                   SC= mean(SC, na.rm=FALSE),
                   TS= mean(TS_1, na.rm=FALSE),
                   P= sum(P, na.rm=FALSE),
                   SWC= mean(SWC_1, na.rm=FALSE),
                   PPFD= mean(PPFD_IN, na.rm=FALSE),
                   RECO= mean(RECO_PI, na.rm=FALSE))
base[[40]]$GPP <- base[[40]]$NEE-base[[40]]$RECO
###US-Bn2####---------------------NO NEE DATA------------------------------------------------------
colnames(base[[41]])
base[[41]] <- base[[41]] %>% group_by(year, month) %>%
  dplyr::summarise(percent_na = (sum(is.na(NEE_PI))/n()*100),
                   TA= mean(TA, na.rm=FALSE),
                   NEE= mean(NEE_PI, na.rm=FALSE),
                   SC= mean(SC, na.rm=FALSE),
                   TS= mean(TS_1, na.rm=FALSE),
                   P= sum(P, na.rm=FALSE),
                   SWC= mean(SWC_1, na.rm=FALSE),
                   PPFD= mean(PPFD_IN, na.rm=FALSE),
                   RECO= mean(RECO_PI, na.rm=FALSE))
base[[41]]$GPP <- base[[41]]$NEE-base[[41]]$RECO
###US-Bn3####--------------------NO NEE DATA-------------------------------------------------------
colnames(base[[42]])
base[[42]] <- base[[42]] %>% group_by(year, month) %>%
  dplyr::summarise(percent_na = (sum(is.na(NEE_PI))/n()*100),
                   TA= mean(TA, na.rm=FALSE),
                   NEE= mean(NEE_PI, na.rm=FALSE),
                   SC= mean(SC, na.rm=FALSE),
                   TS= mean(TS_1, na.rm=FALSE),
                   P= sum(P, na.rm=FALSE),
                   SWC= mean(SWC_1, na.rm=FALSE),
                   PPFD= mean(PPFD_IN, na.rm=FALSE),
                   RECO= mean(RECO_PI, na.rm=FALSE))
base[[42]]$GPP <- base[[42]]$NEE-base[[42]]$RECO
###US-Brw####---------------------------------------------------------------------------
colnames(base[[43]])
base[[43]] <- base[[43]] %>% group_by(year, month) %>%
  dplyr::summarise(percent_na = (sum(is.na(NEE_PI))/n()*100),
                   TA= mean(TA, na.rm=FALSE),
                   NEE= mean(NEE_PI, na.rm=FALSE),
                   SC= mean(SC, na.rm=FALSE),
                   TS= mean(TS_1, na.rm=FALSE),
                   P= sum(P, na.rm=FALSE),
                   SWC= mean(SWC_1, na.rm=FALSE),
                   PPFD= mean(PPFD_IN, na.rm=FALSE),
                   RECO= mean(RECO_PI, na.rm=FALSE))
base[[43]]$GPP <- base[[43]]$NEE-base[[43]]$RECO
#RECO column is empty
#Not gapfilled 
#Arctic data center is also not gapfilled and same dates
###US-BZB####---------------------------------------------------------------------------
colnames(base[[44]])
base[[44]] <- base[[44]] %>% group_by(year, month) %>%
  dplyr::summarise(percent_na = (sum(is.na(GPP_PI))/n()*100),
                   TA= mean(TA_PI_F, na.rm=FALSE),
                   TS= mean(TS_PI_F_1, na.rm=FALSE),
                   P= sum(P_PI_F, na.rm=FALSE),
                   SWC= mean(SWC_PI_1, na.rm=FALSE),
                   PPFD= mean(PPFD_IN, na.rm=FALSE),
                   FCH4= mean(FCH4_PI_F, na.rm=FALSE),
                   D_SNOW= mean(D_SNOW_PI_F, na.rm=FALSE),
                   GPP= mean(GPP_PI, na.rm=FALSE),
                   RECO= mean(RECO_PI, na.rm=FALSE))
base[[44]]$NEE <- base[[44]]$RECO-base[[44]]$GPP
#summer months gapfilled but most dates covered by Ameriflux
###US-BZF####---------------------------------------------------------------------------
colnames(base[[45]])
base[[45]] <- base[[45]] %>% group_by(year, month) %>%
  dplyr::summarise(percent_na = (sum(is.na(GPP_PI))/n()*100),
                   FCH4= mean(FCH4_PI_F, na.rm=FALSE),
                   TA= mean(TA_PI_F, na.rm=FALSE),
                   TS= mean(TS_PI_F_1, na.rm=FALSE),
                   P= sum(P_PI_F, na.rm=FALSE),
                   SWC= mean(SWC_PI_1, na.rm=FALSE),
                   PPFD= mean(PPFD_IN_PI_F, na.rm=FALSE),
                   D_SNOW= mean(D_SNOW, na.rm=FALSE),
                   GPP= mean(GPP_PI, na.rm=FALSE),
                   RECO= mean(RECO_PI, na.rm=FALSE))
base[[45]]$NEE <- base[[45]]$RECO-base[[45]]$GPP
#summer months gapfilled but most dates covered by Ameriflux
###US-BZo####---------------------------------------------------------------------------
colnames(base[[46]])
base[[46]] <- base[[46]] %>% group_by(year, month) %>%
  dplyr::summarise(percent_na = (sum(is.na(GPP_PI))/n()*100),
                   FCH4= mean(FCH4_PI_F, na.rm=FALSE),
                   TA= mean(TA_PI_F, na.rm=FALSE),
                   TS= mean(TS_PI_F_1, na.rm=FALSE),
                   PPFD= mean(PPFD_IN, na.rm=FALSE),
                   P= sum(P, na.rm=FALSE),
                   SWC= mean(SWC_PI_1, na.rm=FALSE),
                   GPP= mean(GPP_PI, na.rm=FALSE),
                   RECO= mean(RECO_PI, na.rm=FALSE))
base[[46]]$NEE <- base[[46]]$RECO-base[[46]]$GPP
#summer months gapfilled but most dates covered by Ameriflux
###US-BZS####---------------------------------------------------------------------------
colnames(base[[47]])
base[[47]] <- base[[47]] %>% group_by(year, month) %>%
  dplyr::summarise(percent_na = (sum(is.na(GPP_PI))/n()*100),
                   TA= mean(TA_PI_F, na.rm=FALSE),
                   TS= mean(TS_PI_F_1, na.rm=FALSE),
                   PPFD= mean(PPFD_IN_PI_F, na.rm=FALSE),
                   P= sum(P_PI_F, na.rm=FALSE),
                   D_SNOW= mean(D_SNOW_PI_F, na.rm=FALSE),
                   SWC= mean(SWC_PI_1, na.rm=FALSE),
                   GPP= mean(GPP_PI, na.rm=FALSE),
                   RECO= mean(RECO_PI, na.rm=FALSE),
                   FCH4= mean(FCH4_PI_F, na.rm=FALSE))
base[[47]]$NEE <- base[[47]]$RECO-base[[47]]$GPP
#summer months gapfilled but most dates covered by Ameriflux
###US-EML####---------------------------------------------------------------------------
colnames(base[[48]])
base[[48]] <- base[[48]] %>% group_by(year, month) %>%
  dplyr::summarise(percent_na = (sum(is.na(NEE_PI_F))/n()*100),
                   TA= mean(TA_PI_F, na.rm=FALSE),
                   SWC= mean(c(SWC_1_1_1, SWC_2_1_1), na.rm=FALSE),
                   TS= mean(c(TS, TS_1_1_1, TS_2_1_1), na.rm=FALSE),
                   PPFD= mean(PPFD_IN_PI_F, na.rm=FALSE),
                   D_SNOW= mean(D_SNOW, na.rm=FALSE),
                   SC= mean(SC, na.rm=FALSE),
                   NEE= mean(NEE_PI_F, na.rm=FALSE),
                   GPP= mean(GPP_PI_F, na.rm=FALSE),
                   RECO= mean(RECO_PI_F, na.rm=FALSE),
                   FCH4= mean(FCH4, na.rm=FALSE))
#gapfilled
###US-Fcr####---------------------------------------------------------------------------
colnames(base[[49]])
base[[49]] <- base[[49]] %>% group_by(year, month) %>%
  dplyr::summarise(percent_na = (sum(is.na(NEE_PI))/n()*100),
                   NEE= mean(NEE_PI, na.rm=FALSE),
                   SC= mean(SC, na.rm=FALSE),
                   GPP= mean(GPP_PI_F, na.rm=FALSE),
                   RECO= mean(RECO_PI_F, na.rm=FALSE),
                   TA= mean(TA_PI_F_1_1_1, na.rm=FALSE),
                   P= sum(P_RAIN, na.rm=FALSE),
                   PPFD= mean(PPFD_IN_PI_F, na.rm=FALSE),
                   SWC= mean(SWC_1_1_2, na.rm=FALSE),
                   TS= mean(TS_PI_F_1_1_1, na.rm=FALSE))
#RECO AND GPP are gapfilled 
###CA-HVa####---------------------------------------------------------------------------
colnames(base[[50]])
base[[50]] <- base[[50]] %>% group_by(year, month) %>%
  dplyr::summarise(percent_na = (sum(is.na(NEE_PI))/n()*100),
                   TA= mean(TA, na.rm=FALSE),
                   NEE= mean(NEE_PI, na.rm=FALSE),
                   SC= mean(SC, na.rm=FALSE),
                   TS= mean(c(TS_1, TS_2), na.rm=FALSE),
                   P= sum(P, na.rm=FALSE),
                   SWC= mean(SWC_1, na.rm=FALSE),
                   PPFD= mean(PPFD_IN, na.rm=FALSE),
                   RECO= mean(RECO_PI, na.rm=FALSE))
#RECO column empty
#Not gapfilled but some months may be complete
###US-ICh####---------------------------------------------------------------------------
colnames(base[[51]])
base[[51]] <- base[[51]] %>% group_by(year, month) %>%
  dplyr::summarise(percent_na = (sum(is.na(GPP_PI))/n()*100),
                   TA= mean(TA_PI_F, na.rm=FALSE),
                   TS= mean(c(TS_PI_1, TS_PI_2), na.rm=FALSE),
                   P= sum(P, na.rm=FALSE),
                   SWC= mean(c(SWC_PI_1, SWC_PI_2), na.rm=FALSE),
                   PPFD= mean(PPFD_IN, na.rm=FALSE),
                   D_SNOW= mean(D_SNOW, na.rm=FALSE),
                   RECO= mean(RECO_PI, na.rm=FALSE),
                   GPP= mean(GPP_PI, na.rm=FALSE),
                   FCH4= mean(FCH4_PI_F, na.rm=FALSE))
base[[51]]$NEE <- base[[51]]$RECO-base[[51]]$GPP
#summer months gapfilled
###US-ICs####---------------------------------------------------------------------------
colnames(base[[52]])
base[[52]] <- base[[52]] %>% group_by(year, month) %>%
  dplyr::summarise(percent_na = (sum(is.na(GPP_PI))/n()*100),
                   FCH4= mean(FCH4_PI_F, na.rm=FALSE),
                   TA= mean(TA_PI_F, na.rm=FALSE),
                   TS= mean(c(TS_PI_1, TS_PI_2), na.rm=FALSE),
                   PPFD= mean(PPFD_IN, na.rm=FALSE),
                   P= sum(P, na.rm=FALSE),
                   D_SNOW= mean(D_SNOW, na.rm=FALSE),
                   SWC= mean(c(SWC_PI_1, SWC_PI_2), na.rm=FALSE),
                   RECO= mean(RECO_PI, na.rm=FALSE),
                   GPP= mean(GPP_PI, na.rm=FALSE))
base[[52]]$NEE <- base[[52]]$RECO-base[[52]]$GPP
#Data seems a little strange GPP= 0 A LOT
#gapfilled mid sept 2007 through 2021
#Ameriflux covers 2007-2021
###US-ICt####---------------------------------------------------------------------------
colnames(base[[53]])
base[[53]] <- base[[53]] %>% mutate(GPP_PI= as.numeric(GPP_PI)) 
base[[53]] <- base[[53]] %>% mutate(FCH4_PI_F= as.numeric(FCH4_PI_F))
base[[53]] <- base[[53]] %>% group_by(year, month) %>%
  dplyr::summarise(percent_na = (sum(is.na(GPP_PI))/n()*100),
                   TA= mean(TA_PI_F, na.rm=FALSE),
                   TS= mean(c(TS_1_1_1, TS_2_1_1), na.rm=FALSE),
                   PPFD= mean(PPFD_IN, na.rm=FALSE),
                   P= sum(P, na.rm=FALSE),
                   SWC= mean(c(SWC_1_1_1, SWC_2_1_1), na.rm=FALSE),
                   TS= mean(c(TS_1_1_1, TS_2_1_1), na.rm=FALSE),
                   RECO= mean(RECO_PI, na.rm=FALSE),
                   GPP= mean(GPP_PI, na.rm=FALSE),
                   FCH4= mean(FCH4_PI_F, na.rm=FALSE))
base[[53]]$NEE <- base[[53]]$RECO-base[[53]]$GPP
#Data seems a little strange GPP= 0 A LOT
#gapfilled 2021, rest NAs
#Ameriflux covers 2007-2021
###US-Ivo####------------------NO NEE DATA--------------------------------------------------------
colnames(base[[54]])
base[[54]] <- base[[54]] %>% group_by(year, month) %>%
  dplyr::summarise(percent_na = (sum(is.na(NEE_PI))/n()*100),
                   TA= mean(TA, na.rm=FALSE),
                   NEE= mean(NEE_PI, na.rm=FALSE),
                   SC= mean(SC, na.rm=FALSE),
                   TS= mean(c(TS_PI_1, TS_PI_2), na.rm=FALSE),
                   P= sum(P, na.rm=FALSE),
                   PPFD= mean(PPFD_IN, na.rm=FALSE),
                   FCH4= mean(FCH4, na.rm=FALSE),
                   SWC= mean(c(SWC_2_1_1, SWC_3_1_1), na.rm=FALSE))
#No NEE data 
#FCH4 not gapfilled 
#Arctic data center has NEE data but it is not gap filled 
###US-KPL####---------------------------------------------------------------------------
colnames(base[[55]])
base[[55]] <- base[[55]] %>% group_by(year, month) %>%
  dplyr::summarise(percent_na = (sum(is.na(FCH4))/n()*100),
                   FCH4= mean(FCH4, na.rm=FALSE),
                   PPFD= mean(PPFD_IN_1_1_1, na.rm=FALSE),
                   P= sum(P_RAIN_1_1_1, na.rm=FALSE),
                   SWC= mean(c(SWC_1_1_1, SWC_2_1_1, SWC_3_1_1), na.rm=FALSE),
                   TA= mean(TA_1_1_1, na.rm=FALSE),
                   TS= mean(c(TS_1_1_1, TS_2_1_1, TS_3_1_1), na.rm=FALSE))
#not gapfilled
#only dad for summer months
###US-NGB####---------------------------------------------------------------------------
colnames(base[[56]])
base[[56]] <- base[[56]] %>% group_by(year, month) %>%
  dplyr::summarise(percent_na = (sum(is.na(FCH4))/n()*100),
                   FCH4= mean(FCH4, na.rm=FALSE),
                   PPFD= mean(PPFD_IN, na.rm=FALSE))
#not gapfilled
#some dates covered by FluxnetCH4 and Ameriflux
###US-NGC####---------------------------------------------------------------------------
colnames(base[[57]])
base[[57]] <- base[[57]] %>% group_by(year, month) %>%
  dplyr::summarise(percent_na = (sum(is.na(FCH4))/n()*100),
                   FCH4= mean(FCH4, na.rm=FALSE),
                   PPFD= mean(PPFD_IN, na.rm=FALSE),
                   TS= mean(c(TS_1_1_1, TS_2_1_1, TS_3_1_1, TS_4_1_1, TS_5_1_1), na.rm=FALSE),
                   SWC= mean(c(SWC_1_1_1, SWC_2_1_1, SWC_3_1_1), na.rm=FALSE))
#not gapfilled 
#some dates covered by FluxnetCH4
###US-Rpf####---------------------------------------------------------------------------
colnames(base[[58]])
base[[58]] <- base[[58]] %>% group_by(year, month) %>%
  dplyr::summarise(percent_na = (sum(is.na(NEE_PI))/n()*100),
                   NEE= mean(NEE_PI, na.rm=FALSE),
                   SC= mean(SC, na.rm=FALSE),
                   GPP= mean(GPP_PI_F, na.rm=FALSE),
                   RECO= mean(RECO_PI_F, na.rm=FALSE),
                   TA= mean(TA_PI_F_1_1_1, na.rm=FALSE),
                   P= sum(P_RAIN, na.rm=FALSE),
                   PPFD= mean(PPFD_IN, na.rm=FALSE),
                   SWC= mean(c(SWC_1_1_1, SWC_1_1_2, SWC_1_1_3), na.rm=FALSE),
                   D_SNOW= mean(D_SNOW, na.rm=FALSE),
                   TS= mean(c(TS_PI_F_1_1_1, TS_PI_F_1_1_2, TS_PI_F_1_1_3,
                              TS_PI_F_1_1_4, TS_PI_F_1_1_5), na.rm=FALSE))
base[[58]]$NEE <- base[[58]]$RECO-base[[58]]$GPP
#NEE not gapfilled but RECO and GPP are mostly gapfilled starting aug 2008
###US-Uaf####---------------------------------------------------------------------------
colnames(base[[59]])
base[[59]] <- base[[59]] %>% group_by(year, month) %>%
  dplyr::summarise(percent_na = (sum(is.na(NEE_PI_F))/n()*100),
                   TA= mean(TA_PI_F_1_1_1, na.rm=FALSE),
                   P= sum(P_RAIN_PI_F, na.rm=FALSE),
                   SWC= mean(c(SWC_PI_F_1_1_1, SWC_PI_F_2_1_1), na.rm=FALSE),
                   FCH4= mean(FCH4, na.rm=FALSE),
                   SCH4= mean(SCH4, na.rm=FALSE),
                   SC= mean(SC_PI_F, na.rm=FALSE),
                   D_SNOW= mean(D_SNOW, na.rm=FALSE),
                   TS= mean(c(TS_PI_F_1_1_1, TS_PI_F_2_1_1, TS_PI_F_3_1_1), na.rm=FALSE),
                   NEE= mean(NEE_PI_F, na.rm=FALSE),
                   GPP= mean(GPP_PI_F, na.rm=FALSE),
                   RECO= mean(RECO_PI_F, na.rm=FALSE))
#gapfilled
###US-Upa####---------------------------------------------------------------------------
colnames(base[[60]])
#NO NEE, GPP, RECO, or CH4 so excluding this site
###US-xBA####---------------------------------------------------------------------------
colnames(base[[61]])
base[[61]] <- base[[61]] %>% group_by(year, month) %>%
  dplyr::summarise(percent_na = (sum(is.na(NEE_PI))/n()*100),
                   SC= mean(SC, na.rm=FALSE),
                   NEE= mean(NEE_PI, na.rm=FALSE),
                   TA= mean(c(TA_1_1_1, TA_1_1_2), na.rm=FALSE),
                   PPFD= mean(PPFD_IN_1_1_1, na.rm=FALSE),
                   SWC= mean(c(SWC_1_1_1, SWC_2_1_1,
                               SWC_3_1_1, SWC_4_1_1, SWC_5_1_1), na.rm=FALSE),
                   TS= mean(c(TS_1_1_1, TS_2_1_1,
                              TS_3_1_1, TS_4_1_1, TS_5_1_1), na.rm=FALSE),
                   P= sum(P, na.rm=FALSE))
#not gapfilled
###US-xBN####---------------------------------------------------------------------------
colnames(base[[62]])
base[[62]] <- base[[62]] %>% group_by(year, month) %>%
  dplyr::summarise(percent_na = (sum(is.na(NEE_PI))/n()*100),
                   SC= mean(SC, na.rm=FALSE),
                   NEE= mean(NEE_PI, na.rm=FALSE),
                   TA= mean(c(TA_1_1_1, TA_1_1_2), na.rm=FALSE),
                   PPFD= mean(PPFD_IN_1_1_1, na.rm=FALSE),
                   SWC= mean(c(SWC_1_1_1, SWC_2_1_1,
                               SWC_3_1_1, SWC_4_1_1, SWC_5_1_1), na.rm=FALSE),
                   TS= mean(c(TS_1_1_1, TS_2_1_1,
                              TS_3_1_1, TS_4_1_1, TS_5_1_1), na.rm=FALSE),
                   P= sum(P, na.rm=FALSE))
#not gapfilled
###US-xDJ####---------------------------------------------------------------------------
colnames(base[[63]])
base[[63]] <- base[[63]] %>% group_by(year, month) %>%
  dplyr::summarise(percent_na = (sum(is.na(NEE_PI))/n()*100),
                   SC= mean(SC, na.rm=FALSE),
                   NEE= mean(NEE_PI, na.rm=FALSE),
                   TA= mean(c(TA_1_1_1, TA_1_1_2), na.rm=FALSE),
                   PPFD= mean(PPFD_IN_1_1_1, na.rm=FALSE),
                   SWC= mean(c(SWC_1_1_1, SWC_2_1_1,
                               SWC_3_1_1, SWC_4_1_1, SWC_5_1_1), na.rm=FALSE),
                   TS= mean(c(TS_1_1_1, TS_2_1_1,
                              TS_3_1_1, TS_4_1_1, TS_5_1_1), na.rm=FALSE),
                   P= sum(P, na.rm=FALSE))
#not gapfilled
###US-xHE####---------------------------------------------------------------------------
colnames(base[[64]])
base[[64]] <- base[[64]] %>% group_by(year, month) %>%
  dplyr::summarise(percent_na = (sum(is.na(NEE_PI))/n()*100),
                   SC= mean(SC, na.rm=FALSE),
                   NEE= mean(NEE_PI, na.rm=FALSE),
                   TA= mean(c(TA_1_1_1, TA_1_1_2), na.rm=FALSE),
                   PPFD= mean(PPFD_IN_1_1_1, na.rm=FALSE),
                   SWC= mean(c(SWC_1_1_1, SWC_2_1_1,
                               SWC_3_1_1, SWC_4_1_1, SWC_5_1_1), na.rm=FALSE),
                   TS= mean(c(TS_1_1_1, TS_2_1_1,
                              TS_3_1_1, TS_4_1_1, TS_5_1_1), na.rm=FALSE),
                   P= sum(P, na.rm=FALSE))
#not gapfilled
###US-xTL####---------------------------------------------------------------------------
colnames(base[[65]])
base[[65]] <- base[[65]] %>% group_by(year, month) %>%
  dplyr::summarise(percent_na = (sum(is.na(NEE_PI))/n()*100),
                   SC= mean(SC, na.rm=FALSE),
                   NEE= mean(NEE_PI, na.rm=FALSE),
                   TA= mean(c(TA_1_1_1, TA_1_1_2), na.rm=FALSE),
                   PPFD= mean(PPFD_IN_1_1_1, na.rm=FALSE),
                   SWC= mean(c(SWC_1_1_1, SWC_2_1_1,
                               SWC_3_1_1, SWC_4_1_1, SWC_5_1_1), na.rm=FALSE),
                   TS= mean(c(TS_1_1_1, TS_2_1_1,
                              TS_3_1_1, TS_4_1_1, TS_5_1_1), na.rm=FALSE),
                   P= sum(P, na.rm=FALSE))
#not gapfilled

####Consolidate data###_--------------------------------------------------------------
names(base)<- substr(files, 5,10) #name each df
#remove dfs that do not have our variables of interest or dates are covered by another database
base2 <- base[-c(1:3,5,8:17,19,20,22:27,31:33,36:38,60)]
#turn list  into one df
base.monthly <- bind_rows(base2, .id = "site_id")
#Convert units to match ABCflux v2
base.monthly$ts = as.yearmon(paste(base.monthly$year, base.monthly$month,sep = '-'))
base.monthly$NEE <- base.monthly$NEE*1.0368 * days_in_month(base.monthly$ts)#micromole per sec to gC per day
base.monthly$GPP <- base.monthly$GPP*1.0368 * days_in_month(base.monthly$ts)
base.monthly$RECO <- base.monthly$RECO*1.0368 * days_in_month(base.monthly$ts)
base.monthly$SCH4 <- base.monthly$SCH4*0.0010368* days_in_month(base.monthly$ts) #nanomole per sec to gC per day
base.monthly$SC <- base.monthly$SC*0.0010368 * days_in_month(base.monthly$ts)
base.monthly$FCH4 <- base.monthly$FCH4*0.0010368 * days_in_month(base.monthly$ts)
base.monthly$ts <- NULL
#turn NaNs into NAs
base.monthly <- base.monthly %>% mutate_all(~ifelse(is.nan(.), NA, .))
#remove rows that do not contain flux data
base.monthly2 <- base.monthly %>%
  filter(if_all(c("NEE", "GPP", "RECO","FCH4"), ~ !is.na(.)))

### Adding in other variables
setwd("/Users/iwargowsky/Desktop/Ameriflux/AMF-BASE")
meta <- read_xlsx("AMF_AA-Flx_BIF_LEGACY_20221208.xlsx")
#filter for sites of interest
names <- unique(base.monthly2$site_id)
meta <- meta %>% filter(SITE_ID %in% names)
#make better format and group by site
meta.wide <- meta %>% pivot_wider(names_from = VARIABLE, values_from = DATAVALUE) 
meta.bysite <- meta.wide %>% group_by(SITE_ID) %>% summarise(country= mode(COUNTRY),
                                                             citation = mode(DOI),
                                                             site_name= mode(SITE_NAME),
                                                             latitude= mode(LOCATION_LAT),
                                                             longitude= mode(LOCATION_LONG))
#merge flux df and meta data
meta.bysite<- meta.bysite %>% dplyr::rename(site_id= SITE_ID)
base.ALL <- left_join(base.monthly2, meta.bysite)

#save
setwd("/Users/iwargowsky/Desktop/Ameriflux")
write_csv(base.ALL , "ameriflux.baseALL.csv")


####extract list of sites and dates covered###
base.monthly2$ts <- paste(base.monthly2$year, base.monthly2$month)
sites <- subset(base.monthly2, select = c(site_id,ts))
sites.datescovered2 <- sites %>% group_by(site_id) %>% dplyr::summarise(start_date = min(ts),
                                                                        end_date = max(ts))
#double checking that function above worked
checkdates <- sites %>% arrange(site_id, ts)
