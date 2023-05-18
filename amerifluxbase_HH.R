library(dplyr)
library(data.table)
library(tidyr)
library(readxl)
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
                              month = substr(df$TIMESTAMP_START, 5,6),
                              day = substr(df$TIMESTAMP_START, 7,8) ),
                              week = ymd(paste(year, month, day)))
names(base)<- substr(files, 5,10) #name each df
###CA-ARB####---------------------------------------------------------------------------
colnames(base[[1]]) 
#date range covered by Ameriflux
###CA-ARF####---------------------------------------------------------------------------
colnames(base[[2]])
#date range covered by Ameriflux 
###CA-CF1####---------------------------------------------------------------------------
colnames(base[[3]])
#only has FC
###CA-CF2####---------------------------------------------------------------------------
colnames(base[[4]])  
base[[4]] <- base[[4]] %>% group_by(year, month, day) %>%
  dplyr::summarise(TA= mean(TA, na.rm= TRUE),
                   PPFD= mean(PPFD_IN, na.rm= TRUE),
                   P= sum(P_RAIN, na.rm= TRUE),
                   TS= mean(c(TS_1_1_1, TS_2_1_1, TS_3_1_1), na.rm= TRUE),
                   FCH4= mean(FCH4, na.rm= TRUE))
x <- base[[4]] %>% mutate(week = week(as.Date(paste(year, month, day, sep = '-'))))


  group_by(year, month, day) %>%
  
  

  
  dplyr::summarise(percent_na = (sum(is.na(FCH4))/n()*100),
                   TA= mean(TA, na.rm= TRUE),
                   PPFD= mean(PPFD_IN, na.rm= TRUE),
                   P= sum(P_RAIN, na.rm= TRUE),
                   TS= mean(c(TS_1_1_1, TS_2_1_1, TS_3_1_1), na.rm= TRUE),
                   FCH4= mean(FCH4, na.rm= TRUE))




#CH4 not gapfilled
###CA-Gro####---------------------------------------------------------------------------
colnames(base[[5]])
#date range covered by Ameriflux
###CA-HPC####---------------------------------------------------------------------------
colnames(base[[6]])
base[[6]] <- base[[6]] %>% group_by(year, month, day) %>%
  dplyr::summarise(SC= mean(SC, na.rm= TRUE),
            TA= mean(TA, na.rm= TRUE),
            TS= mean(TS_1_1_1, na.rm= TRUE),
            PPFD= mean(PPFD_IN, na.rm= TRUE),
            P= sum(P, na.rm= TRUE),
            D_SNOW= mean(D_SNOW, na.rm= TRUE),
            FCH4= mean(FCH4_PI_F, na.rm= TRUE),
            SCH4= mean(SCH4, na.rm= TRUE),
            NEE = mean(NEE_PI_F, na.rm= TRUE),
            GPP= mean(GPP_PI_F, na.rm= TRUE),
            RECO= mean(RECO_PI_F, na.rm= TRUE))
#NEE and RECO look mostly gapfilled, GPP is not gapfilled 
###CA-Man####---------------------------------------------------------------------------
colnames(base[[7]])
#date range covered by Fluxnet2015
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
###CA-NS8####-------EMPTY DATA FRAME--------------------------------------------------------------------
colnames(base[[15]])
#base[[15]] <- base[[15]] %>% group_by(year, month, day) %>%
  #dplyr::summarise(TA= mean(TA, na.rm= TRUE),
                   #NEE= mean(NEE_PI, na.rm= TRUE),
                   #SC= mean(SC, na.rm= TRUE),
                   #TS= mean(TS_1, na.rm= TRUE),
                   #P= sum(P, na.rm= TRUE),
                  # SWC= mean(SWC_1, na.rm= TRUE),
                   #PPFD= mean(PPFD_IN, na.rm= TRUE),
                   #RECO= mean(RECO_PI, na.rm= TRUE))
#base[[15]]$GPP <- base[[15]]$NEE-base[[15]]$RECO
###CA-Oas####---------------------------------------------------------------------------
colnames(base[[16]])
#date range covered by Fluxnet2015
###CA-Obs####---------------------------------------------------------------------------
colnames(base[[17]])
#date range covered by Fluxnet2015
 ###CA-Ojp####---------------------------------------------------------------------------
colnames(base[[18]]) 
base[[18]] <- base[[18]] %>% group_by(year, month, day) %>%
  dplyr::summarise(SC= mean(c(SC_1_1_1, SC_1_1_2)),
                   NEE= mean(NEE_PI),
                   RECO= mean(RECO_PI),
                   GPP= mean(GPP_PI),
                   PPFD= mean(c(PPFD_IN_1_1_1, PPFD_IN_1_1_2)),
                   P= sum(c(P_1_1_1, P_1_1_2), na.rm=TRUE),
                   D_SNOW= mean(c(D_SNOW_1_1_1, D_SNOW_1_1_2)),
                   TS= mean(c(TS_1_1_1, TS_1_1_2), na.rm=TRUE),
                   TA= mean(c(TA_1_1_1, TA_1_1_2), na.rm=TRUE),
                   SWC= mean(SWC_1_1_1))
#NOT gapfilled
###CA-Qc2####---------------------------------------------------------------------------
#date range covered by Ameriflux 
colnames(base[[19]])
###CA-Qfo####---------------------------------------------------------------------------
colnames(base[[20]]) 
#date range covered by Fluxnet2015 and Ameriflux
###CA-SCB####---------------------------------------------------------------------------
colnames(base[[21]])
base[[21]] <- base[[21]] %>% group_by(year, month, day) %>%
  dplyr::summarise(SC= mean(SC, na.rm=TRUE),
                   TA= mean(c(TA_1_1_1, TA_1_1_2), na.rm=TRUE),
                   TS= mean(TS_1_1_1, na.rm=TRUE),
                   P= sum(P_RAIN, na.rm=TRUE),
                   D_SNOW= mean(D_SNOW, na.rm=TRUE),
                   FCH4= mean(FCH4_PI_F, na.rm=TRUE),
                   SCH4= mean(SCH4, na.rm=TRUE),
                   NEE= mean(NEE_PI_F, na.rm=TRUE),
                   GPP= mean(GPP_PI, na.rm=TRUE),
                   RECO= mean(RECO_PI, na.rm=TRUE))
#gapfilled
#fluxnetch4 covers until 2017
##CA-SCC####---------------------------------------------------------------------------
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
base[[28]] <- base[[28]] %>% group_by(year, month, day) %>%
  dplyr::summarise(NEE= mean(NEE_PI_F, na.rm=TRUE),
                   RECO= mean(RECO_PI_F, na.rm=TRUE),
                   GPP= mean(GPP_PI_F, na.rm=TRUE),
                   NEE= mean(NEE_PI_F, na.rm=TRUE),
                   PPFD= mean(PPFD_IN_PI_F_1_1_1, na.rm=TRUE),
                   TA= mean(c(TA_PI_F_1_1_1, TA_PI_F_1_1_2), na.rm=TRUE),
                   D_SNOW= mean(D_SNOW_PI_F, na.rm=TRUE),
                   TS= mean(c(TS_PI_F_1_1_1, TS_PI_F_2_1_1), na.rm=TRUE),
                   SWC= mean(SWC_PI_F_1, na.rm=TRUE))
# gapfilled only 2004-2006
###CA-SMC####---------------------------------------------------------------------------
colnames(base[[29]])
base[[29]] <- base[[29]] %>% group_by(year, month, day) %>%
  dplyr::summarise(SC= mean(SC, na.rm=TRUE),
                   TA= mean(c(TA_1_1_1, TA_2_1_1), na.rm=TRUE),
                   TS= mean(TS_1_1_1, na.rm=TRUE),
                   PPFD= mean(PPFD_IN, na.rm=TRUE),
                   FCH4= mean(FCH4_PI_F, na.rm=TRUE),
                   SCH4= mean(SCH4, na.rm=TRUE),
                   NEE= mean(NEE_PI_F, na.rm=TRUE),
                   GPP= mean(GPP_PI, na.rm=TRUE),
                   RECO= mean(RECO_PI, na.rm=TRUE))
#Only NEE is gapfilled starting mid april - mid november
###CA-TVC####---------------------------------------------------------------------------
colnames(base[[30]])
base[[30]] <- base[[30]] %>% group_by(year, month, day) %>%
  dplyr::summarise(SC= mean(SC, na.rm=TRUE),
                   TA= mean(TA_PI_F, na.rm=TRUE),
                   P= sum(P, na.rm=TRUE),
                   D_SNOW= mean(D_SNOW, na.rm=TRUE),
                   FCH4= mean(FCH4_PI_F, na.rm=TRUE),
                   SCH4= mean(SCH4, na.rm=TRUE),
                   NEE= mean(NEE_PI_F, na.rm=TRUE),
                   GPP= mean(GPP_PI, na.rm=TRUE),
                   RECO= mean(RECO_PI, na.rm=TRUE))
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
base[[34]] <- base[[34]] %>% group_by(year, month, day) %>%
  dplyr::summarise(NEE= mean(NEE_PI, na.rm=TRUE),
                   FCH4= mean(FCH4, na.rm=TRUE),
                   PPFD= mean(PPFD_IN, na.rm=TRUE),
                   TS= mean(TS_PI_1_1_A, na.rm=TRUE),
                   SWC= mean(SWC_PI_1_1_A, na.rm=TRUE))
#not gapfilled, 2015-2018 in fluxnetch4
###US-A10####---------------------------------------------------------------------------
colnames(base[[35]])
base[[35]] <- base[[35]] %>% group_by(year, month, day) %>%
  dplyr::summarise(NEE= mean(NEE_PI, na.rm=TRUE),
                   FCH4= mean(FCH4, na.rm=TRUE),
                   PPFD= mean(PPFD_IN, na.rm=TRUE),
                   TS= mean(TS_PI_1_1_A, na.rm=TRUE),
                   SWC= mean(SWC_PI_1_1_A, na.rm=TRUE))
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
###US-Atq####-----------NO NEE DATA----------------------------------------------------------------
colnames(base[[39]])
#base[[39]] <- base[[39]] %>% group_by(year, month, day) %>%
  #dplyr::summarise(TA= mean(TA, na.rm=TRUE),
                   #NEE= mean(NEE_PI, na.rm=TRUE),
                  # SC= mean(SC, na.rm=TRUE),
                   #TS= mean(TS_1, na.rm=TRUE),
                  # P= sum(P, na.rm=TRUE),
                  # SWC= mean(SWC_1, na.rm=TRUE),
                  # PPFD= mean(PPFD_IN, na.rm=TRUE),
                   #RECO= mean(RECO_PI, na.rm=TRUE))
#base[[39]]$GPP <- base[[39]]$NEE-base[[39]]$RECO
###US-Bn1####-------------NO NEE DATA--------------------------------------------------------------
colnames(base[[40]])
#base[[40]] <- base[[40]] %>% group_by(year, month, day) %>%
  #dplyr::summarise(TA= mean(TA, na.rm=TRUE),
                   #NEE= mean(NEE_PI, na.rm=TRUE),
                   #SC= mean(SC, na.rm=TRUE),
                   #TS= mean(TS_1, na.rm=TRUE),
                   #P= sum(P, na.rm=TRUE),
                   #SWC= mean(SWC_1, na.rm=TRUE),
                   #PPFD= mean(PPFD_IN, na.rm=TRUE),
                   #RECO= mean(RECO_PI, na.rm=TRUE))
#base[[40]]$GPP <- base[[40]]$NEE-base[[40]]$RECO
###US-Bn2####---------------NO NEE DATA------------------------------------------------------------
colnames(base[[41]])
#base[[41]] <- base[[41]] %>% group_by(year, month, day) %>%
  #dplyr::summarise(TA= mean(TA, na.rm=TRUE),
                   #NEE= mean(NEE_PI, na.rm=TRUE),
                   #SC= mean(SC, na.rm=TRUE),
                   #TS= mean(TS_1, na.rm=TRUE),
                   #P= sum(P, na.rm=TRUE),
                   #SWC= mean(SWC_1, na.rm=TRUE),
                   #PPFD= mean(PPFD_IN, na.rm=TRUE),
                   #RECO= mean(RECO_PI, na.rm=TRUE))
#base[[41]]$GPP <- base[[41]]$NEE-base[[41]]$RECO
###US-Bn3####------------------NO NEE DATA---------------------------------------------------------
colnames(base[[42]])
#base[[42]] <- base[[42]] %>% group_by(year, month, day) %>%
  #dplyr::summarise(TA= mean(TA, na.rm=TRUE),
                   #NEE= mean(NEE_PI, na.rm=TRUE),
                   #SC= mean(SC, na.rm=TRUE),
                   #TS= mean(TS_1, na.rm=TRUE),
                   #P= sum(P, na.rm=TRUE),
                   #SWC= mean(SWC_1, na.rm=TRUE),
                   #PPFD= mean(PPFD_IN, na.rm=TRUE),
                   #RECO= mean(RECO_PI, na.rm=TRUE))
#base[[42]]$GPP <- base[[42]]$NEE-base[[42]]$RECO
###US-Brw####---------------------------------------------------------------------------
colnames(base[[43]])
base[[43]] <- base[[43]] %>% group_by(year, month, day) %>%
  dplyr::summarise(TA= mean(TA, na.rm=TRUE),
                   NEE= mean(NEE_PI, na.rm=TRUE),
                   SC= mean(SC, na.rm=TRUE),
                   TS= mean(TS_1, na.rm=TRUE),
                   P= sum(P, na.rm=TRUE),
                   SWC= mean(SWC_1, na.rm=TRUE),
                   PPFD= mean(PPFD_IN, na.rm=TRUE),
                   RECO= mean(RECO_PI, na.rm=TRUE))
#RECO column is empty
#Not gapfilled 
###US-BZB####---------------------------------------------------------------------------
colnames(base[[44]])
base[[44]] <- base[[44]] %>% group_by(year, month, day) %>%
  dplyr::summarise(TA= mean(TA_PI_F, na.rm=TRUE),
                   TS= mean(TS_PI_F_1, na.rm=TRUE),
                   P= sum(P_PI_F, na.rm=TRUE),
                   SWC= mean(SWC_PI_1, na.rm=TRUE),
                   PPFD= mean(PPFD_IN, na.rm=TRUE),
                   FCH4= mean(FCH4_PI_F, na.rm=TRUE),
                   D_SNOW= mean(D_SNOW_PI_F, na.rm=TRUE),
                   GPP= mean(GPP_PI, na.rm=TRUE),
                   RECO= mean(RECO_PI, na.rm=TRUE))
base[[44]]$NEE <- base[[44]]$GPP-base[[44]]$RECO
#summer months gapfilled but most dates covered by Ameriflux
###US-BZF####---------------------------------------------------------------------------
colnames(base[[45]])
base[[45]] <- base[[45]] %>% group_by(year, month, day) %>%
  dplyr::summarise(FCH4= mean(FCH4_PI_F, na.rm=TRUE),
                   TA= mean(TA_PI_F, na.rm=TRUE),
                   TS= mean(TS_PI_F_1, na.rm=TRUE),
                   P= sum(P_PI_F, na.rm=TRUE),
                   SWC= mean(SWC_PI_1, na.rm=TRUE),
                   PPFD= mean(PPFD_IN_PI_F, na.rm=TRUE),
                   D_SNOW= mean(D_SNOW, na.rm=TRUE),
                   GPP= mean(GPP_PI, na.rm=TRUE),
                   RECO= mean(RECO_PI, na.rm=TRUE))
base[[45]]$NEE <- base[[45]]$GPP-base[[45]]$RECO
#summer months gapfilled but most dates covered by Ameriflux
###US-BZo####---------------------------------------------------------------------------
colnames(base[[46]])
base[[46]] <- base[[46]] %>% group_by(year, month, day) %>%
  dplyr::summarise(FCH4= mean(FCH4_PI_F, na.rm=TRUE),
                   TA= mean(TA_PI_F, na.rm=TRUE),
                   TS= mean(TS_PI_F_1, na.rm=TRUE),
                   PPFD= mean(PPFD_IN, na.rm=TRUE),
                   P= sum(P, na.rm=TRUE),
                   SWC= mean(SWC_PI_1, na.rm=TRUE),
                   GPP= mean(GPP_PI, na.rm=TRUE),
                   RECO= mean(RECO_PI, na.rm=TRUE))
base[[46]]$NEE <- base[[46]]$GPP-base[[46]]$RECO
#Mostly gapfilled and some dates covered by Ameriflux
###US-BZS####---------------------------------------------------------------------------
colnames(base[[47]])
base[[47]] <- base[[47]] %>% group_by(year, month, day) %>%
  dplyr::summarise(TA= mean(TA_PI_F, na.rm=TRUE),
                   TS= mean(TS_PI_F_1, na.rm=TRUE),
                   PPFD= mean(PPFD_IN_PI_F, na.rm=TRUE),
                   P= sum(P_PI_F, na.rm=TRUE),
                   D_SNOW= mean(D_SNOW_PI_F, na.rm=TRUE),
                   SWC= mean(SWC_PI_1, na.rm=TRUE),
                   GPP= mean(GPP_PI, na.rm=TRUE),
                   RECO= mean(RECO_PI, na.rm=TRUE),
                   FCH4= mean(FCH4_PI_F, na.rm=TRUE))
base[[47]]$NEE <- base[[47]]$GPP-base[[47]]$RECO
#Mostly gapfilled and some dates covered by Ameriflux
###US-EML####---------------------------------------------------------------------------
colnames(base[[48]])
base[[48]] <- base[[48]] %>% group_by(year, month, day) %>%
  dplyr::summarise(TA= mean(TA_PI_F, na.rm=TRUE),
                   SWC= mean(c(SWC_1_1_1, SWC_2_1_1), na.rm=TRUE),
                   TS= mean(c(TS, TS_1_1_1, TS_2_1_1), na.rm=TRUE),
                   PPFD= mean(PPFD_IN_PI_F, na.rm=TRUE),
                   D_SNOW= mean(D_SNOW, na.rm=TRUE),
                   SC= mean(SC, na.rm=TRUE),
                   NEE= mean(NEE_PI_F, na.rm=TRUE),
                   GPP= mean(GPP_PI_F, na.rm=TRUE),
                   RECO= mean(RECO_PI_F, na.rm=TRUE),
                   FCH4= mean(FCH4, na.rm=TRUE))
#gapfilled
###US-Fcr####---------------------------------------------------------------------------
colnames(base[[49]])
base[[49]] <- base[[49]] %>% group_by(year, month, day) %>%
  dplyr::summarise(SC= mean(SC, na.rm=TRUE),
                   GPP= mean(GPP_PI_F, na.rm=TRUE),
                   RECO= mean(RECO_PI_F, na.rm=TRUE),
                   TA= mean(TA_PI_F_1_1_1, na.rm=TRUE),
                   P= sum(P_RAIN, na.rm=TRUE),
                   PPFD= mean(PPFD_IN_PI_F, na.rm=TRUE),
                   SWC= mean(SWC_1_1_2, na.rm=TRUE),
                   TS= mean(TS_PI_F_1_1_1, na.rm=TRUE))
base[[49]]$NEE <- base[[49]]$RECO-base[[49]]$GPP
#RECO AND GPP are gapfilled 
###US-HVa####---------------------------------------------------------------------------
colnames(base[[50]])
base[[50]] <- base[[50]] %>% group_by(year, month, day) %>%
  dplyr::summarise(TA= mean(TA, na.rm=TRUE),
                   NEE= mean(NEE_PI, na.rm=TRUE),
                   SC= mean(SC, na.rm=TRUE),
                   TS= mean(c(TS_1, TS_2), na.rm=TRUE),
                   P= sum(P, na.rm=TRUE),
                   SWC= mean(SWC_1, na.rm=TRUE),
                   PPFD= mean(PPFD_IN, na.rm=TRUE),
                   RECO= mean(RECO_PI, na.rm=TRUE))
#RECO column empty
#Not gapfilled but some months may be complete
###US-ICh####---------------------------------------------------------------------------
colnames(base[[51]])
base[[51]] <- base[[51]] %>% group_by(year, month, day) %>%
  dplyr::summarise(TA= mean(TA_PI_F, na.rm=TRUE),
                   TS= mean(c(TS_PI_1, TS_PI_2), na.rm=TRUE),
                   P= sum(P, na.rm=TRUE),
                   SWC= mean(c(SWC_PI_1, SWC_PI_2), na.rm=TRUE),
                   PPFD= mean(PPFD_IN, na.rm=TRUE),
                   D_SNOW= mean(D_SNOW, na.rm=TRUE),
                   RECO= mean(RECO_PI, na.rm=TRUE),
                   GPP= mean(GPP_PI, na.rm=TRUE),
                   FCH4= mean(FCH4_PI_F, na.rm=TRUE))
base[[51]]$NEE <- base[[51]]$GPP-base[[51]]$RECO
#summer months gapfilled
###US-ICs####---------------------------------------------------------------------------
colnames(base[[52]])
base[[52]] <- base[[52]] %>% group_by(year, month, day) %>%
  dplyr::summarise(FCH4= mean(FCH4_PI_F, na.rm=TRUE),
                   TA= mean(TA_PI_F, na.rm=TRUE),
                   TS= mean(c(TS_PI_1, TS_PI_2), na.rm=TRUE),
                   PPFD= mean(PPFD_IN, na.rm=TRUE),
                   P= sum(P, na.rm=TRUE),
                   D_SNOW= mean(D_SNOW, na.rm=TRUE),
                   SWC= mean(c(SWC_PI_1, SWC_PI_2), na.rm=TRUE),
                   RECO= mean(RECO_PI, na.rm=TRUE),
                   GPP= mean(GPP_PI, na.rm=TRUE))
base[[52]]$NEE <- base[[52]]$RECO-base[[52]]$GPP
#Data seems a little strange GPP= 0 A LOT
#gapfilled mid sept 2007 through 2021
#Ameriflux covers 2007-2021
###US-ICt####---------------------------------------------------------------------------
colnames(base[[53]])
base[[53]] <- base[[53]] %>% mutate(GPP_PI= as.numeric(GPP_PI)) 
base[[53]] <- base[[53]] %>% mutate(FCH4_PI_F= as.numeric(FCH4_PI_F))
base[[53]] <- base[[53]] %>% group_by(year, month, day) %>%
  dplyr::summarise(TA= mean(TA_PI_F, na.rm=TRUE),
                   TS= mean(c(TS_1_1_1, TS_2_1_1), na.rm=TRUE),
                   PPFD= mean(PPFD_IN, na.rm=TRUE),
                   P= sum(P, na.rm=TRUE),
                   SWC= mean(c(SWC_1_1_1, SWC_2_1_1), na.rm=TRUE),
                   TS= mean(c(TS_1_1_1, TS_2_1_1), na.rm=TRUE),
                   RECO= mean(RECO_PI, na.rm=TRUE),
                   GPP= mean(GPP_PI, na.rm=TRUE),
                   FCH4= mean(FCH4_PI_F, na.rm=TRUE))
#Data seems a little strange GPP= 0 A LOT
#gapfilled 2021, rest NAs
#Ameriflux covers 2007-2021
###US-Ivo####------------NO NEE DATA---------------------------------------------------------------
colnames(base[[54]])
base[[54]] <- base[[54]] %>% group_by(year, month, day) %>%
  dplyr::summarise(TA= mean(TA, na.rm=TRUE),
                   NEE= mean(NEE_PI, na.rm=TRUE),
                   SC= mean(SC, na.rm=TRUE),
                   TS= mean(c(TS_PI_1, TS_PI_2), na.rm=TRUE),
                   P= sum(P, na.rm=TRUE),
                   PPFD= mean(PPFD_IN, na.rm=TRUE),
                   FCH4= mean(FCH4, na.rm=TRUE),
                   SWC= mean(c(SWC_2_1_1, SWC_3_1_1), na.rm=TRUE))
#No NEE data 
#FCH4 not gapfilled 
###US-KPL####---------------------------------------------------------------------------
colnames(base[[55]])
base[[55]] <- base[[55]] %>% group_by(year, month, day) %>%
  dplyr::summarise(FCH4= mean(FCH4, na.rm=TRUE),
                   PPFD= mean(PPFD_IN_1_1_1, na.rm=TRUE),
                   P= sum(P_RAIN_1_1_1, na.rm=TRUE),
                   SWC= mean(c(SWC_1_1_1, SWC_2_1_1, SWC_3_1_1), na.rm=TRUE),
                   TA= mean(TA_1_1_1, na.rm=TRUE),
                   TS= mean(c(TS_1_1_1, TS_2_1_1, TS_3_1_1), na.rm=TRUE))
#not gapfilled
#only dad for summer months
###US-NGB####---------------------------------------------------------------------------
colnames(base[[56]])
base[[56]] <- base[[56]] %>% group_by(year, month, day) %>%
  dplyr::summarise(FCH4= mean(FCH4, na.rm=TRUE),
                   PPFD= mean(PPFD_IN, na.rm=TRUE))
#not gapfilled
#some dates covered by FluxnetCH4 and Ameriflux
###US-NGC####---------------------------------------------------------------------------
colnames(base[[57]])
base[[57]] <- base[[57]] %>% group_by(year, month, day) %>%
  dplyr::summarise(FCH4= mean(FCH4, na.rm=TRUE),
                   PPFD= mean(PPFD_IN, na.rm=TRUE),
                   TS= mean(c(TS_1_1_1, TS_2_1_1, TS_3_1_1, TS_4_1_1, TS_5_1_1), na.rm=TRUE),
                   SWC= mean(c(SWC_1_1_1, SWC_2_1_1, SWC_3_1_1), na.rm=TRUE))
#not gapfilled 
#some dates covered by FluxnetCH4
###US-Rpf####---------------------------------------------------------------------------
colnames(base[[58]])
base[[58]] <- base[[58]] %>% group_by(year, month, day) %>%
  dplyr::summarise(NEE= mean(NEE_PI, na.rm=TRUE),
                   SC= mean(SC, na.rm=TRUE),
                   GPP= mean(GPP_PI_F, na.rm=TRUE),
                   RECO= mean(RECO_PI_F, na.rm=TRUE),
                   TA= mean(TA_PI_F_1_1_1, na.rm=TRUE),
                   P= sum(P_RAIN, na.rm=TRUE),
                   PPFD= mean(PPFD_IN, na.rm=TRUE),
                   SWC= mean(c(SWC_1_1_1, SWC_1_1_2, SWC_1_1_3), na.rm=TRUE),
                   D_SNOW= mean(D_SNOW, na.rm=TRUE),
                   TS= mean(c(TS_PI_F_1_1_1, TS_PI_F_1_1_2, TS_PI_F_1_1_3,
                              TS_PI_F_1_1_4, TS_PI_F_1_1_5), na.rm=TRUE))
base[[58]]$NEE <- base[[58]]$RECO-base[[58]]$GPP
#NEE not gapfilled but RECO and GPP are mostly gapfilled starting aug 2008
###US-Uaf####---------------------------------------------------------------------------
colnames(base[[59]])
base[[59]] <- base[[59]] %>% group_by(year, month, day) %>%
  dplyr::summarise(TA= mean(TA_PI_F_1_1_1, na.rm=TRUE),
                   P= sum(P_RAIN_PI_F, na.rm=TRUE),
                   SWC= mean(c(SWC_PI_F_1_1_1, SWC_PI_F_2_1_1), na.rm=TRUE),
                   FCH4= mean(FCH4, na.rm=TRUE),
                   SCH4= mean(SCH4, na.rm=TRUE),
                   SC= mean(SC_PI_F, na.rm=TRUE),
                   D_SNOW= mean(D_SNOW, na.rm=TRUE),
                   TS= mean(c(TS_PI_F_1_1_1, TS_PI_F_2_1_1, TS_PI_F_3_1_1), na.rm=TRUE),
                   NEE= mean(NEE_PI_F, na.rm=TRUE),
                   GPP= mean(GPP_PI_F, na.rm=TRUE),
                   RECO= mean(RECO_PI_F, na.rm=TRUE))
#gapfilled
###US-Upa####---------------------------------------------------------------------------
colnames(base[[60]])
#NO NEE, GPP, RECO, or CH4 so excluding this site
###US-xBA####---------------------------------------------------------------------------
colnames(base[[61]])
base[[61]] <- base[[61]] %>% group_by(year, month, day) %>%
  dplyr::summarise(SC= mean(SC, na.rm=TRUE),
                   NEE= mean(NEE_PI, na.rm=TRUE),
                   TA= mean(c(TA_1_1_1, TA_1_1_2), na.rm=TRUE),
                   PPFD= mean(PPFD_IN_1_1_1, na.rm=TRUE),
                   SWC= mean(c(SWC_1_1_1, SWC_2_1_1,
                               SWC_3_1_1, SWC_4_1_1, SWC_5_1_1), na.rm=TRUE),
                   TS= mean(c(TS_1_1_1, TS_2_1_1,
                              TS_3_1_1, TS_4_1_1, TS_5_1_1), na.rm=TRUE),
                   P= sum(P, na.rm=TRUE))
#not gapfilled
###US-xBN####---------------------------------------------------------------------------
colnames(base[[62]])
base[[62]] <- base[[62]] %>% group_by(year, month, day) %>%
  dplyr::summarise(SC= mean(SC, na.rm=TRUE),
                   NEE= mean(NEE_PI, na.rm=TRUE),
                   TA= mean(c(TA_1_1_1, TA_1_1_2), na.rm=TRUE),
                   PPFD= mean(PPFD_IN_1_1_1, na.rm=TRUE),
                   SWC= mean(c(SWC_1_1_1, SWC_2_1_1,
                               SWC_3_1_1, SWC_4_1_1, SWC_5_1_1), na.rm=TRUE),
                   TS= mean(c(TS_1_1_1, TS_2_1_1,
                              TS_3_1_1, TS_4_1_1, TS_5_1_1), na.rm=TRUE),
                   P= sum(P, na.rm=TRUE))
#not gapfilled
###US-xDJ####---------------------------------------------------------------------------
colnames(base[[63]])
base[[63]] <- base[[63]] %>% group_by(year, month, day) %>%
  dplyr::summarise(SC= mean(SC, na.rm=TRUE),
                   NEE= mean(NEE_PI, na.rm=TRUE),
                   TA= mean(c(TA_1_1_1, TA_1_1_2), na.rm=TRUE),
                   PPFD= mean(PPFD_IN_1_1_1, na.rm=TRUE),
                   SWC= mean(c(SWC_1_1_1, SWC_2_1_1,
                               SWC_3_1_1, SWC_4_1_1, SWC_5_1_1), na.rm=TRUE),
                   TS= mean(c(TS_1_1_1, TS_2_1_1,
                              TS_3_1_1, TS_4_1_1, TS_5_1_1), na.rm=TRUE),
                   P= sum(P, na.rm=TRUE))
#not gapfilled
###US-xHE####---------------------------------------------------------------------------
colnames(base[[64]])
base[[64]] <- base[[64]] %>% group_by(year, month, day) %>%
  dplyr::summarise(SC= mean(SC, na.rm=TRUE),
                   NEE= mean(NEE_PI, na.rm=TRUE),
                   TA= mean(c(TA_1_1_1, TA_1_1_2), na.rm=TRUE),
                   PPFD= mean(PPFD_IN_1_1_1, na.rm=TRUE),
                   SWC= mean(c(SWC_1_1_1, SWC_2_1_1,
                               SWC_3_1_1, SWC_4_1_1, SWC_5_1_1), na.rm=TRUE),
                   TS= mean(c(TS_1_1_1, TS_2_1_1,
                              TS_3_1_1, TS_4_1_1, TS_5_1_1), na.rm=TRUE),
                   P= sum(P, na.rm=TRUE))
#not gapfilled
###US-xTL####---------------------------------------------------------------------------
colnames(base[[65]])
base[[65]] <- base[[65]] %>% group_by(year, month, day) %>%
  dplyr::summarise(SC= mean(SC, na.rm=TRUE),
                   NEE= mean(NEE_PI, na.rm=TRUE),
                   TA= mean(c(TA_1_1_1, TA_1_1_2), na.rm=TRUE),
                   PPFD= mean(PPFD_IN_1_1_1, na.rm=TRUE),
                   SWC= mean(c(SWC_1_1_1, SWC_2_1_1,
                               SWC_3_1_1, SWC_4_1_1, SWC_5_1_1), na.rm=TRUE),
                   TS= mean(c(TS_1_1_1, TS_2_1_1,
                              TS_3_1_1, TS_4_1_1, TS_5_1_1), na.rm=TRUE),
                   P= sum(P, na.rm=TRUE))
#not gapfilled
####Consolidate data###_--------------------------------------------------------------
names(base)<- substr(files, 5,10) #name each df
#remove dfs that do not have our variables of interest
base2 <- base[-c(1,2,3,5,7:14,16,17,19,20,22:27,31:33,36:42,60)]
#turn list  into one df
base.daily <- bind_rows(base2, .id = "site_id")
#Convert units to match ABCflux v2
base.daily$NEE <- base.daily$NEE*1.0368 #micromole per sec to gC per day
base.daily$GPP <- base.daily$GPP*1.0368
base.daily$RECO <- base.daily$RECO*1.0368
base.daily$SCH4 <- base.daily$SCH4*0.0010368  #nanomole per sec to gC per day
base.daily$SC <- base.daily$SC*0.0010368
base.daily$FCH4 <- base.daily$FCH4*0.0010368
#get monthly values
base.monthly <-  group_by(base.daily, year, month, site_id) %>% 
  dplyr::summarise(TA= mean(TA, na.omit=TRUE),
                   PPFD= mean(PPFD, na.omit=TRUE),
                   TS= mean(TS, na.omit=TRUE),
                   SWC= mean(SWC, na.omit=TRUE),
                   D_SNOW= mean(D_SNOW, na.omit=TRUE),
                   FCH4= sum(FCH4, na.omit=FALSE),
                   NEE= sum(NEE, na.omit=FALSE),
                   SC= sum(SC, na.omit=FALSE),
                   RECO= sum(RECO, na.omit=FALSE),
                   SCH4= sum(SCH4, na.omit=FALSE),
                   GPP= sum(GPP, na.omit=FALSE),
                   P= sum(P, na.omit=FALSE))
#turn NaNs into NAs
base.monthly <- base.monthly %>% mutate_all(~ifelse(is.nan(.), NA, .))
#remove rows that do not contain flux data
base.monthly <- base.monthly %>%
  filter(if_all(c("NEE", "GPP", "RECO","FCH4"), ~ !is.na(.)))


### Adding in other variables
setwd("/Users/iwargowsky/Desktop/Ameriflux/AMF-BASE")
meta <- read_xlsx("AMF_AA-Flx_BIF_LEGACY_20221208.xlsx")
#filter for sites of interest
names <- unique(base.monthly$site_id)
meta <- meta %>% filter(SITE_ID %in% names)
#make better format and group by site
meta.wide <- meta %>% pivot_wider(names_from = VARIABLE, values_from = DATAVALUE) 
meta.bysite <- meta.wide %>% group_by(SITE_ID) %>% summarise(country= mode(COUNTRY),
                                                           citation = mode(DOI),
                                                           site_name= mode(SITE_NAME),
                                                           latitude= mode(LOCATION_LAT),
                                                           longitude= mode(LOCATION_LONG))
#merge flux df and meta data
meta.bysite<- meta.bysite %>% rename(site_id= SITE_ID)
base.ALL <- left_join(base.monthly, meta.bysite)

#save
setwd("/Users/iwargowsky/Desktop/Ameriflux")
write_csv(base.ALL , "ameriflux.baseALL.csv")



####extract list of sites and dates covered###
base.monthly$ts <- paste(base.monthly$year, base.monthly$month)
sites <- subset(base.monthly, select = c(site_id,ts))
sites.datescovered2 <- sites %>% group_by(site_id) %>% dplyr::summarise(start_date = min(ts),
                                                                       end_date = max(ts))
#double checking that function above worked
checkdates <- sites %>% arrange(site_id, ts)
