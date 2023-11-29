library(dplyr)
library(data.table)
library(tidyr)
library(readxl)
library(lubridate)
library(zoo)
library(readr)
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
###CA-BOU####---------------------------------------------------------------------------
colnames(base[[3]])
base[[3]] <- base[[3]] %>% group_by(year, month) %>%
  dplyr::summarise(percent_na = (sum(is.na(NEE_PI))/n()*100),
                   NEE= mean(NEE_PI, na.rm = FALSE),
                   RECO= mean(RECO_PI, na.rm= FALSE),
                   GPP= mean(GPP_PI, na.rm = FALSE),
                   TA= mean(TA, na.rm= FALSE),
                   PPFD= mean(PPFD_IN, na.rm= FALSE),
                   P= sum(P_RAIN, na.rm= FALSE),
                   TS= mean(TS, na.rm= FALSE),
                   FCH4= mean(FCH4, na.rm = FALSE))
#NEE gapfilled
###CA-CF1####---------------------------------------------------------------------------
colnames(base[[4]])
#NO NEE, GPP, RECO, or CH4 so excluding this site
###CA-CF2####---------------------------------------------------------------------------
colnames(base[[5]])  
base[[5]] <- base[[5]] %>% group_by(year, month) %>%
  dplyr::summarise(percent_na = (sum(is.na(FCH4))/n()*100),
                  TA= mean(TA, na.rm= FALSE),
                   PPFD= mean(PPFD_IN, na.rm= FALSE),
                   P= sum(P_RAIN, na.rm= FALSE),
                   TS= mean(c(TS_1_1_1, TS_2_1_1, TS_3_1_1), na.rm= FALSE),
                   FCH4= mean(FCH4, na.rm = FALSE))
#not gapfilled
###CA-Gro####---------------------------------------------------------------------------
colnames(base[[6]])
#date range covered by Ameriflux
###CA-HPC####---------------------------------------------------------------------------
#colnames(base[[7]])
#base[[7]] <- base[[7]] %>% group_by(year, month) %>%
#  dplyr::summarise(percent_na = (sum(is.na(NEE_PI_F))/n()*100),
#                   SC= mean(SC, na.rm= FALSE),
#                   TA= mean(TA, na.rm= FALSE),
#                   TS= mean(TS_1_1_1, na.rm= FALSE),
#                  PPFD= mean(PPFD_IN, na.rm= FALSE),
 #                  P= sum(P, na.rm= FALSE),
  #                 D_SNOW= mean(D_SNOW, na.rm= FALSE),
   #                FCH4= mean(FCH4_PI_F, na.rm= FALSE),
    #               NEE = mean(NEE_PI_F, na.rm= FALSE),
     #              GPP= mean(GPP_PI_F, na.rm= FALSE),
      #             RECO= mean(RECO_PI_F, na.rm= FALSE))
#NEE and RECO look mostly gapfilled, GPP is not gapfilled 
#Data received from Haley Alcock covers this date range
###CA-Man####---------------------------------------------------------------------------
colnames(base[[8]])
#date range covered by Fluxnet2015
###CA-NS1####---------------------------------------------------------------------------
colnames(base[[9]])
#date range covered by Fluxnet2015
###CA-NS2####---------------------------------------------------------------------------
colnames(base[[10]])
#date range covered by Fluxnet2015
###CA-NS3####---------------------------------------------------------------------------
colnames(base[[11]])
#date range covered by Fluxnet2015
###CA-NS4####---------------------------------------------------------------------------
colnames(base[[12]])
#date range covered by Fluxnet2015
###CA-NS5####---------------------------------------------------------------------------
colnames(base[[13]])
#date range covered by Fluxnet2015
###CA-NS6####---------------------------------------------------------------------------
colnames(base[[14]])
#date range covered by Fluxnet2015
###CA-NS7####---------------------------------------------------------------------------
colnames(base[[15]])
#date range covered by Fluxnet2015
###CA-NS8####-----------------------EMPTY DATA FRAME----------------------------------------------------
colnames(base[[16]])
#base[[16]] <- base[[16]] %>% group_by(year, month) %>%
  #dplyr::summarise(percent_na = (sum(is.na(NEE_PI))/n()*100),
                   #TA= mean(TA, na.rm= FALSE),
                  # NEE= mean(NEE_PI, na.rm= FALSE),
                   #TS= mean(TS_1, na.rm= FALSE),
                  # P= sum(P, na.rm= FALSE),
                  # SWC= mean(SWC_1, na.rm= FALSE),
                  # PPFD= mean(PPFD_IN, na.rm= FALSE),
                  # RECO= mean(RECO_PI, na.rm= FALSE))
#base[[16]]$GPP <- base[[16]]$NEE-base[[16]]$RECO
###CA-Oas####---------------------------------------------------------------------------
colnames(base[[17]])
#date range covered by Fluxnet2015
###CA-Obs####---------------------------------------------------------------------------
colnames(base[[18]])
#date range covered by Fluxnet2015
###CA-Ojp####---------------------------------------------------------------------------
colnames(base[[19]]) 
base[[19]] <- base[[19]] %>% group_by(year, month) %>%
  dplyr::summarise(percent_na = (sum(is.na(NEE_PI))/n()*100),
                   NEE= mean(NEE_PI, na.rm=FALSE),
                   RECO= mean(RECO_PI, na.rm=FALSE),
                   GPP= mean(GPP_PI, na.rm=FALSE),
                   PPFD= mean(c(PPFD_IN_1_1_1, PPFD_IN_1_1_2), na.rm=FALSE),
                   P= sum(c(P_1_1_1, P_1_1_2), na.rm=FALSE),
                   D_SNOW= mean(c(D_SNOW_1_1_1, D_SNOW_1_1_2), na.rm=FALSE),
                   TS= mean(c(TS_1_1_1, TS_1_1_2), na.rm=FALSE),
                   TA= mean(c(TA_1_1_1, TA_1_1_2), na.rm=FALSE),
                   SWC= mean(SWC_1_1_1, na.rm=FALSE))
#Not gapfilled 
#Arctic data center is also not gap filled and same dates
###CA-Qc2####---------------------------------------------------------------------------
colnames(base[[20]])
#date range covered by Ameriflux
###CA-Qfo####---------------------------------------------------------------------------
colnames(base[[21]]) 
#date range covered by Fluxnet2015 and Ameriflux
###CA-SCB####---------------------------------------------------------------------------
colnames(base[[22]])
base[[22]] <- base[[22]] %>% group_by(year, month) %>%
  dplyr::summarise(percent_na = (sum(is.na(NEE_PI_F))/n()*100),
                   TA= mean(c(TA_1_1_1, TA_1_1_2), na.rm=FALSE),
                   TS= mean(TS_1_1_1, na.rm=FALSE),
                   P= sum(P_RAIN, na.rm=FALSE),
                   D_SNOW= mean(D_SNOW, na.rm=FALSE),
                   FCH4= mean(FCH4_PI_F, na.rm=FALSE),
                   NEE= mean(NEE_PI_F, na.rm=FALSE),
                   GPP= mean(GPP_PI, na.rm=FALSE),
                   RECO= mean(RECO_PI, na.rm=FALSE))
#gapfilled
#fluxnetch4 covers until 2017
###CA-SCC####---------------------------------------------------------------------------
colnames(base[[23]])
#date range covered by FluxnetCH4
###CA-SF1####---------------------------------------------------------------------------
colnames(base[[24]])
#date range covered by Fluxnet2015 and Ameriflux
###CA-SF2####---------------------------------------------------------------------------
colnames(base[[25]])
#date range covered by Ameriflux and Fluxnet2015 
###CA-SF3####---------------------------------------------------------------------------
colnames(base[[26]])
#date range covered by Fluxnet2015 
###CA-SJ1####---------------------------------------------------------------------------
colnames(base[[27]])
#NO NEE, GPP, RECO, or CH4 so excluding this site
###CA-SJ2####---------------------------------------------------------------------------
colnames(base[[28]])
#date range covered by Ameriflux 
###CA-SJ3####---------------------------------------------------------------------------
colnames(base[[29]])
base[[29]] <- base[[29]] %>% group_by(year, month) %>%
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
#colnames(base[[30]])
#base[[30]] <- base[[30]] %>% group_by(year, month) %>%
 # dplyr::summarise(percent_na = (sum(is.na(NEE_PI_F))/n()*100),
  #                 SC= mean(SC, na.rm=FALSE),
   #                TA= mean(c(TA_1_1_1, TA_2_1_1), na.rm=FALSE),
    #               TS= mean(TS_1_1_1, na.rm=FALSE),
     #              PPFD= mean(PPFD_IN, na.rm=FALSE),
      #             FCH4= mean(FCH4_PI_F, na.rm=FALSE),
       #            NEE= mean(NEE_PI_F, na.rm=FALSE),
        #           GPP= mean(GPP_PI, na.rm=FALSE),
         #          RECO= mean(RECO_PI, na.rm=FALSE))
#Only NEE is gapfilled starting mid april - mid november
#Data received from Haley Alcock covers this date range
###CA-TVC####---------------------------------------------------------------------------
#colnames(base[[31]])
#base[[31]] <- base[[31]] %>% group_by(year, month) %>%
#  dplyr::summarise(percent_na = (sum(is.na(NEE_PI_F))/n()*100),
 #                  TA= mean(TA_PI_F, na.rm=FALSE),
  #                 P= sum(P, na.rm=FALSE),
   #                D_SNOW= mean(D_SNOW, na.rm=FALSE),
    #               FCH4= mean(FCH4_PI_F, na.rm=FALSE),
     #              NEE= mean(NEE_PI_F, na.rm=FALSE),
      #             GPP= mean(GPP_PI, na.rm=FALSE),
       #            RECO= mean(RECO_PI, na.rm=FALSE))
#Only NEE is gapfilled starting mid april - mid november
#Data received from Haley Alcock covers this date range
###CA-WP1####---------------------------------------------------------------------------
colnames(base[[32]])
#date range covered by Ameriflux 
###CA-WP2####---------------------------------------------------------------------------
colnames(base[[33]])
#date range covered by Ameriflux 
###CA-WP3####---------------------------------------------------------------------------
colnames(base[[34]])
#date range covered by Ameriflux 
###US-A03####---------------------------------------------------------------------------
colnames(base[[35]])
base[[35]] <- base[[35]] %>% group_by(year, month) %>%
  dplyr::summarise(percent_na = (sum(is.na(NEE_PI))/n()*100),
                   NEE= mean(NEE_PI, na.rm=FALSE),
                   FCH4= mean(FCH4, na.rm=FALSE),
                   PPFD= mean(PPFD_IN, na.rm=FALSE),
                   TS= mean(TS_PI_1_1_A, na.rm=FALSE),
                   SWC= mean(SWC_PI_1_1_A, na.rm=FALSE))
#not gapfilled, 2015-2018 in fluxnetch4
###US-A10####---------------------------------------------------------------------------
colnames(base[[36]])
base[[36]] <- base[[36]] %>% group_by(year, month) %>%
  dplyr::summarise(percent_na = (sum(is.na(NEE_PI))/n()*100),
                   NEE= mean(NEE_PI, na.rm=FALSE),
                   FCH4= mean(FCH4, na.rm=FALSE),
                   PPFD= mean(PPFD_IN, na.rm=FALSE),
                   TS= mean(TS_PI_1_1_A, na.rm=FALSE),
                   SWC= mean(SWC_PI_1_1_A, na.rm=FALSE))
#not gapfilled, 2012-2018 in fluxnetch4
###US-An1####---------------------------------------------------------------------------
colnames(base[[37]])
#NO NEE, GPP, RECO, or CH4 so excluding this site
###US-An2####---------------------------------------------------------------------------
colnames(base[[38]])
#NO NEE, GPP, RECO, or CH4 so excluding this site
###US-An3####---------------------------------------------------------------------------
colnames(base[[39]])
#NO NEE, GPP, RECO, or CH4 so excluding this site
###US-Atq####------------------NO NEE DATA---------------------------------------------------------
colnames(base[[40]])
base[[40]] <- base[[40]] %>% group_by(year, month) %>%
  dplyr::summarise(percent_na = (sum(is.na(NEE_PI))/n()*100),
                   TA= mean(TA, na.rm=FALSE),
                   NEE= mean(NEE_PI, na.rm=FALSE),
                   TS= mean(TS_1, na.rm=FALSE),
                   P= sum(P, na.rm=FALSE),
                   SWC= mean(SWC_1, na.rm=FALSE),
                   PPFD= mean(PPFD_IN, na.rm=FALSE),
                   percent_na_ppfd = (sum(is.na(PPFD_IN))/n()*100),
                   RECO= mean(RECO_PI, na.rm=FALSE),
                   percent_na_reco = (sum(is.na(RECO_PI))/n()*100))
base[[40]]$GPP <- base[[40]]$NEE-base[[40]]$RECO
#Arctic data center is also not gap filled and same dates
###US-Bn1####-----------------------NO NEE DATA----------------------------------------------------
colnames(base[[41]])
base[[41]] <- base[[41]] %>% group_by(year, month) %>%
  dplyr::summarise(percent_na = (sum(is.na(NEE_PI))/n()*100),
                   TA= mean(TA, na.rm=FALSE),
                   NEE= mean(NEE_PI, na.rm=FALSE),
                   TS= mean(TS_1, na.rm=FALSE),
                   P= sum(P, na.rm=FALSE),
                   SWC= mean(SWC_1, na.rm=FALSE),
                   PPFD= mean(PPFD_IN, na.rm=FALSE),
                   RECO= mean(RECO_PI, na.rm=FALSE))
base[[41]]$GPP <- base[[41]]$NEE-base[[41]]$RECO
###US-Bn2####---------------------NO NEE DATA------------------------------------------------------
colnames(base[[42]])
base[[42]] <- base[[42]] %>% group_by(year, month) %>%
  dplyr::summarise(percent_na = (sum(is.na(NEE_PI))/n()*100),
                   TA= mean(TA, na.rm=FALSE),
                   NEE= mean(NEE_PI, na.rm=FALSE),
                   TS= mean(TS_1, na.rm=FALSE),
                   P= sum(P, na.rm=FALSE),
                   SWC= mean(SWC_1, na.rm=FALSE),
                   PPFD= mean(PPFD_IN, na.rm=FALSE),
                   RECO= mean(RECO_PI, na.rm=FALSE))
base[[42]]$GPP <- base[[42]]$NEE-base[[42]]$RECO
###US-Bn3####--------------------NO NEE DATA-------------------------------------------------------
colnames(base[[43]])
base[[43]] <- base[[43]] %>% group_by(year, month) %>%
  dplyr::summarise(percent_na = (sum(is.na(NEE_PI))/n()*100),
                   TA= mean(TA, na.rm=FALSE),
                   NEE= mean(NEE_PI, na.rm=FALSE),
                   TS= mean(TS_1, na.rm=FALSE),
                   P= sum(P, na.rm=FALSE),
                   SWC= mean(SWC_1, na.rm=FALSE),
                   PPFD= mean(PPFD_IN, na.rm=FALSE),
                   RECO= mean(RECO_PI, na.rm=FALSE))
base[[43]]$GPP <- base[[43]]$NEE-base[[43]]$RECO
###US-Brw####---------------------------------------------------------------------------
colnames(base[[44]])
base[[44]] <- base[[44]] %>% group_by(year, month) %>%
  dplyr::summarise(percent_na_nee = (sum(is.na(NEE_PI))/n()*100),
                   TA= mean(TA, na.rm=FALSE),
                   NEE= mean(NEE_PI, na.rm=FALSE),
                   TS= mean(TS_1, na.rm=FALSE),
                   P= sum(P, na.rm=FALSE),
                   SWC= mean(SWC_1, na.rm=FALSE),
                   PPFD= mean(PPFD_IN, na.rm=FALSE),
                   percent_na_ppfd= (sum(is.na(PPFD_IN))/n()*100),
                   RECO= mean(RECO_PI, na.rm=FALSE),
                   percent_na_reco = (sum(is.na(RECO_PI))/n()*100))
base[[44]]$GPP <- base[[44]]$NEE-base[[44]]$RECO
#RECO column is empty
#Not gapfilled 
#Arctic data center is also not gapfilled and same dates
###US-BZB####---------------------------------------------------------------------------
colnames(base[[45]])
base[[45]] <- base[[45]] %>% group_by(year, month) %>%
  dplyr::summarise(percent_na = (sum(is.na(GPP_PI))/n()*100),
                   TA= mean(TA_PI_F, na.rm=FALSE),
                   TS= mean(TS_PI_F_1, na.rm=FALSE),
                   P= sum(P_PI_F, na.rm=FALSE),
                   SWC= mean(SWC_PI_1, na.rm=FALSE),
                   PPFD= mean(PPFD_IN, na.rm=FALSE),
                   FCH4= mean(FCH4_PI_F, na.rm=FALSE),
                   percent_na_ch4 = (sum(is.na(FCH4_PI_F))/n()*100),
                   D_SNOW= mean(D_SNOW_PI_F, na.rm=FALSE),
                   GPP= mean(GPP_PI, na.rm=FALSE),
                   RECO= mean(RECO_PI, na.rm=FALSE))
base[[45]]$NEE <- base[[45]]$RECO-base[[45]]$GPP
#summer months gapfilled but most dates covered by Ameriflux
###US-BZF####---------------------------------------------------------------------------
colnames(base[[46]])
base[[46]] <- base[[46]] %>% group_by(year, month) %>%
  dplyr::summarise(percent_na = (sum(is.na(GPP_PI))/n()*100),
                   FCH4= mean(FCH4_PI_F, na.rm=FALSE),
                   percent_na_ch4 = (sum(is.na(FCH4_PI_F))/n()*100),
                   TA= mean(TA_PI_F, na.rm=FALSE),
                   TS= mean(TS_PI_F_1, na.rm=FALSE),
                   P= sum(P_PI_F, na.rm=FALSE),
                   SWC= mean(SWC_PI_1, na.rm=FALSE),
                   PPFD= mean(PPFD_IN_PI_F, na.rm=FALSE),
                   D_SNOW= mean(D_SNOW, na.rm=FALSE),
                   GPP= mean(GPP_PI, na.rm=FALSE),
                   RECO= mean(RECO_PI, na.rm=FALSE))
base[[46]]$NEE <- base[[46]]$RECO-base[[46]]$GPP
#summer months gapfilled but most dates covered by Ameriflux
###US-BZo####---------------------------------------------------------------------------
colnames(base[[47]])
base[[47]] <- base[[47]] %>% group_by(year, month) %>%
  dplyr::summarise(percent_na = (sum(is.na(GPP_PI))/n()*100),
                   FCH4= mean(FCH4_PI_F, na.rm=FALSE),
                   percent_na_ch4 = (sum(is.na(FCH4_PI_F))/n()*100),
                   TA= mean(TA_PI_F, na.rm=FALSE),
                   TS= mean(TS_PI_F_1, na.rm=FALSE),
                   PPFD= mean(PPFD_IN, na.rm=FALSE),
                   P= sum(P, na.rm=FALSE),
                   SWC= mean(SWC_PI_1, na.rm=FALSE),
                   GPP= mean(GPP_PI, na.rm=FALSE),
                   RECO= mean(RECO_PI, na.rm=FALSE))
base[[47]]$NEE <- base[[47]]$RECO-base[[47]]$GPP
#summer months gapfilled but most dates covered by Ameriflux
###US-BZS####---------------------------------------------------------------------------
colnames(base[[48]])
base[[48]] <- base[[48]] %>% group_by(year, month) %>%
  dplyr::summarise(percent_na = (sum(is.na(GPP_PI))/n()*100),
                   TA= mean(TA_PI_F, na.rm=FALSE),
                   TS= mean(TS_PI_F_1, na.rm=FALSE),
                   PPFD= mean(PPFD_IN_PI_F, na.rm=FALSE),
                   P= sum(P_PI_F, na.rm=FALSE),
                   D_SNOW= mean(D_SNOW_PI_F, na.rm=FALSE),
                   SWC= mean(SWC_PI_1, na.rm=FALSE),
                   GPP= mean(GPP_PI, na.rm=FALSE),
                   RECO= mean(RECO_PI, na.rm=FALSE),
                   FCH4= mean(FCH4_PI_F, na.rm=FALSE),
                   percent_na_ch4 = (sum(is.na(FCH4_PI_F))/n()*100))
base[[48]]$NEE <- base[[48]]$RECO-base[[48]]$GPP
#summer months gapfilled but most dates covered by Ameriflux
###US-EML####---------------------------------------------------------------------------
colnames(base[[49]])
base[[49]] <- base[[49]] %>% group_by(year, month) %>%
  dplyr::summarise(percent_na = (sum(is.na(NEE_PI_F))/n()*100),
                   TA= mean(TA_PI_F, na.rm=FALSE),
                   SWC= mean(c(SWC_1_1_1, SWC_2_1_1), na.rm=FALSE),
                   TS= mean(c(TS, TS_1_1_1, TS_2_1_1), na.rm=FALSE),
                   PPFD= mean(PPFD_IN_PI_F, na.rm=FALSE),
                   D_SNOW= mean(D_SNOW, na.rm=FALSE),
                   NEE= mean(NEE_PI_F, na.rm=FALSE),
                   GPP= mean(GPP_PI_F, na.rm=FALSE),
                   RECO= mean(RECO_PI_F, na.rm=FALSE),
                   FCH4= mean(FCH4, na.rm=FALSE))
#gapfilled
###US-Fcr####---------------------------------------------------------------------------
#colnames(base[[50]])
#base[[50]] <- base[[50]] %>% group_by(year, month) %>%
 # dplyr::summarise(percent_na = (sum(is.na(NEE_PI))/n()*100),
  #                 NEE= mean(NEE_PI, na.rm=FALSE),
    #               GPP= mean(GPP_PI_F, na.rm=FALSE),
     #              RECO= mean(RECO_PI_F, na.rm=FALSE),
      #             TA= mean(TA_PI_F_1_1_1, na.rm=FALSE),
       #            P= sum(P_RAIN, na.rm=FALSE),
        #           PPFD= mean(PPFD_IN_PI_F, na.rm=FALSE),
         #          SWC= mean(SWC_1_1_2, na.rm=FALSE),
          #         TS= mean(TS_PI_F_1_1_1, na.rm=FALSE))
#RECO AND GPP are gapfilled 
#data from Masahito Ueyama covers this date range
###US-HVa####---------------------------------------------------------------------------
colnames(base[[51]])
base[[51]] <- base[[51]] %>% group_by(year, month) %>%
  dplyr::summarise(percent_na = (sum(is.na(NEE_PI))/n()*100),
                   TA= mean(TA, na.rm=FALSE),
                   NEE= mean(NEE_PI, na.rm=FALSE),
                   TS= mean(c(TS_1, TS_2), na.rm=FALSE),
                   P= sum(P, na.rm=FALSE),
                   SWC= mean(SWC_1, na.rm=FALSE),
                   PPFD= mean(PPFD_IN, na.rm=FALSE),
                   RECO= mean(RECO_PI, na.rm=FALSE))
#RECO column empty
#Not gapfilled but some months may be complete
#data from Masahito Ueyama covers SOME of this date range
###US-HVs####---------------------------------------------------------------------------
colnames(base[[52]])
base[[52]] <- base[[52]] %>% group_by(year, month) %>%
  dplyr::summarise(percent_na = (sum(is.na(NEE_PI_F_1_1_1))/n()*100),
                   TA= mean(c(TA_1_1_1, TA_2_1_1), na.rm=FALSE),
                   NEE= mean(NEE_PI_F_1_1_1, na.rm=FALSE),
                   TS= mean(TS_1_1_1, na.rm=FALSE),
                   RECO= mean(RECO_PI_F_1_1_1, na.rm=FALSE),
                   GPP= mean(GPP_PI_F_1_1_1, na.rm = TRUE))

###US-ICh####---------------------------------------------------------------------------
colnames(base[[53]])
base[[53]] <- base[[53]] %>% group_by(year, month) %>%
  dplyr::summarise(percent_na_gpp = (sum(is.na(GPP_PI))/n()*100),
                   TA= mean(TA_PI_F, na.rm=FALSE),
                   TS= mean(c(TS_PI_1, TS_PI_2), na.rm=FALSE),
                   P= sum(P, na.rm=FALSE),
                   SWC= mean(c(SWC_PI_1, SWC_PI_2), na.rm=FALSE),
                   PPFD= mean(PPFD_IN, na.rm=FALSE),
                   percent_na_ppfd = (sum(is.na(PPFD_IN))/n()*100),
                   D_SNOW= mean(D_SNOW, na.rm=FALSE),
                   RECO= mean(RECO_PI, na.rm=FALSE),
                   percent_na_reco = (sum(is.na(RECO_PI))/n()*100),
                   GPP= mean(GPP_PI, na.rm=FALSE),
                   FCH4= mean(FCH4_PI_F, na.rm=FALSE),
                   percent_na_fch4 = (sum(is.na(FCH4_PI_F))/n()*100))
base[[53]]$NEE <- base[[53]]$RECO-base[[53]]$GPP
#summer months gapfilled
###US-ICs####---------------------------------------------------------------------------
colnames(base[[54]])
base[[54]] <- base[[54]] %>% group_by(year, month) %>%
  dplyr::summarise(FCH4= mean(FCH4_PI_F, na.rm=FALSE),
                   percent_na_ch4 = (sum(is.na(FCH4_PI_F))/n()*100),
                   TA= mean(TA_PI_F, na.rm=FALSE),
                   TS= mean(c(TS_PI_1, TS_PI_2), na.rm=FALSE),
                   PPFD= mean(PPFD_IN, na.rm=FALSE),
                   percent_na_ppfd = (sum(is.na(PPFD_IN))/n()*100),
                   P= sum(P, na.rm=FALSE),
                   D_SNOW= mean(D_SNOW, na.rm=FALSE),
                   SWC= mean(c(SWC_PI_1, SWC_PI_2), na.rm=FALSE),
                   RECO= mean(RECO_PI, na.rm=FALSE),
                   percent_na_reco = (sum(is.na(RECO_PI))/n()*100),
                   GPP= mean(GPP_PI, na.rm=FALSE),
                   percent_na_gpp = (sum(is.na(GPP_PI))/n()*100))
base[[54]]$NEE <- base[[54]]$RECO-base[[54]]$GPP
#Data seems a little strange GPP= 0 A LOT
#gapfilled mid sept 2007 through 2021
#Ameriflux covers 2007-2021
###US-ICt####---------------------------------------------------------------------------
colnames(base[[55]])
base[[55]] <- base[[55]] %>% mutate(GPP_PI= as.numeric(GPP_PI)) 
base[[55]] <- base[[55]] %>% mutate(FCH4_PI_F= as.numeric(FCH4_PI_F))
base[[55]] <- base[[55]] %>% group_by(year, month) %>%
  dplyr::summarise(TA= mean(TA_PI_F, na.rm=FALSE),
                   TS= mean(c(TS_1_1_1, TS_2_1_1), na.rm=FALSE),
                   PPFD= mean(PPFD_IN, na.rm=FALSE),
                   P= sum(P, na.rm=FALSE),
                   SWC= mean(c(SWC_1_1_1, SWC_2_1_1), na.rm=FALSE),
                   TS= mean(c(TS_1_1_1, TS_2_1_1), na.rm=FALSE),
                   RECO= mean(RECO_PI, na.rm=FALSE),
                   percent_na_reco = (sum(is.na(RECO_PI))/n()*100),
                   GPP= mean(GPP_PI, na.rm=FALSE),
                   percent_na_gpp = (sum(is.na(GPP_PI))/n()*100),
                   FCH4= mean(FCH4_PI_F, na.rm=FALSE),
                   percent_na_fch4 = (sum(is.na(FCH4_PI_F))/n()*100))
base[[55]]$NEE <- base[[55]]$RECO-base[[55]]$GPP
#Data seems a little strange GPP= 0 A LOT
#gapfilled 2021, rest NAs
#Ameriflux covers 2007-2021
###US-Ivo####------------------NO NEE DATA--------------------------------------------------------
colnames(base[[56]])
USbase[[56]] <- base[[56]] %>% group_by(year, month) %>%
  dplyr::summarise(percent_na_nee = (sum(is.na(NEE_PI))/n()*100),
                   percent_na_rh = (sum(is.na(RH))/n()*100),
                   percent_na_fch4 = (sum(is.na(FCH4))/n()*100),
                   percent_na_PPFD = (sum(is.na(PPFD_IN))/n()*100),
                   percent_na_TA = (sum(is.na(TA))/n()*100),
                   TA= mean(TA, na.rm=FALSE),
                   NEE= mean(NEE_PI, na.rm=FALSE),
                   TS= mean(c(TS_PI_1, TS_PI_2), na.rm=FALSE),
                   P= sum(P, na.rm=FALSE),
                   PPFD= mean(PPFD_IN, na.rm=FALSE),
                   FCH4= mean(FCH4, na.rm=FALSE),
                   SWC= mean(c(SWC_2_1_1, SWC_3_1_1), na.rm=FALSE))
#No NEE data 
#FCH4 not gapfilled 
#Arctic data center has NEE data but it is not gap filled 
###US-KPL####---------------------------------------------------------------------------
colnames(base[[57]])
base[[57]] <- base[[57]] %>% group_by(year, month) %>%
  dplyr::summarise(percent_na = (sum(is.na(FCH4))/n()*100),
                   FCH4= mean(FCH4, na.rm=FALSE),
                   PPFD= mean(PPFD_IN_1_1_1, na.rm=FALSE),
                   P= sum(P_RAIN_1_1_1, na.rm=FALSE),
                   SWC= mean(c(SWC_1_1_1, SWC_2_1_1, SWC_3_1_1), na.rm=FALSE),
                   TA= mean(TA_1_1_1, na.rm=FALSE),
                   TS= mean(c(TS_1_1_1, TS_2_1_1, TS_3_1_1), na.rm=FALSE))
#not gapfilled
#only data for summer months
###US-NGB####---------------------------------------------------------------------------
colnames(base[[58]])
base[[58]] <- base[[58]] %>% group_by(year, month) %>%
  dplyr::summarise(percent_na = (sum(is.na(FCH4))/n()*100),
                   FCH4= mean(FCH4, na.rm=FALSE),
                   PPFD= mean(PPFD_IN, na.rm=FALSE))
#not gapfilled
#some dates covered by FluxnetCH4 and Ameriflux
###US-NGC####---------------------------------------------------------------------------
colnames(base[[59]])
base[[59]] <- base[[59]] %>% group_by(year, month) %>%
  dplyr::summarise(percent_na = (sum(is.na(FCH4))/n()*100),
                   FCH4= mean(FCH4, na.rm=FALSE),
                   PPFD= mean(PPFD_IN, na.rm=FALSE),
                   TS= mean(c(TS_1_1_1, TS_2_1_1, TS_3_1_1, TS_4_1_1, TS_5_1_1), na.rm=FALSE),
                   SWC= mean(c(SWC_1_1_1, SWC_2_1_1, SWC_3_1_1), na.rm=FALSE))
#not gapfilled 
#some dates covered by FluxnetCH4
###US-Rpf####---------------------------------------------------------------------------
#colnames(base[[60]])
#base[[60]] <- base[[60]] %>% group_by(year, month) %>%
 # dplyr::summarise(percent_na = (sum(is.na(NEE_PI))/n()*100),
  #                 NEE= mean(NEE_PI, na.rm=FALSE),
    #               GPP= mean(GPP_PI_F, na.rm=FALSE),
     #              RECO= mean(RECO_PI_F, na.rm=FALSE),
      #             TA= mean(TA_PI_F_1_1_1, na.rm=FALSE),
       #            P= sum(P_RAIN, na.rm=FALSE),
        #           PPFD= mean(PPFD_IN, na.rm=FALSE),
         #          SWC= mean(c(SWC_1_1_1, SWC_1_1_2, SWC_1_1_3), na.rm=FALSE),
          #         D_SNOW= mean(D_SNOW, na.rm=FALSE),
           #        TS= mean(c(TS_PI_F_1_1_1, TS_PI_F_1_1_2, TS_PI_F_1_1_3,
            #                  TS_PI_F_1_1_4, TS_PI_F_1_1_5), na.rm=FALSE))
#base[[60]]$NEE <- base[[60]]$RECO-base[[60]]$GPP
#NEE not gapfilled but RECO and GPP are mostly gapfilled starting aug 2008
#data from Masahito Ueyama covers this date range
###US-Uaf####---------------------------------------------------------------------------
#colnames(base[[61]])
#base[[61]] <- base[[61]] %>% group_by(year, month) %>%
 # dplyr::summarise(percent_na = (sum(is.na(NEE_PI_F))/n()*100),
  #                 TA= mean(TA_PI_F_1_1_1, na.rm=FALSE),
   #                P= sum(P_RAIN_PI_F, na.rm=FALSE),
    #               SWC= mean(c(SWC_PI_F_1_1_1, SWC_PI_F_2_1_1), na.rm=FALSE),
     #              FCH4= mean(FCH4, na.rm=FALSE),
       #            D_SNOW= mean(D_SNOW, na.rm=FALSE),
        #           TS= mean(c(TS_PI_F_1_1_1, TS_PI_F_2_1_1, TS_PI_F_3_1_1), na.rm=FALSE),
         #          NEE= mean(NEE_PI_F, na.rm=FALSE),
          #         GPP= mean(GPP_PI_F, na.rm=FALSE),
           #        RECO= mean(RECO_PI_F, na.rm=FALSE))
#data from Masahito Ueyama covers this date range
#gapfilled
###US-Upa####---------------------------------------------------------------------------
colnames(base[[62]])
#NO NEE, GPP, RECO, or CH4 so excluding this site
###US-xBA####---------------------------------------------------------------------------
colnames(base[[63]])
base[[63]] <- base[[63]] %>% group_by(year, month) %>%
  dplyr::summarise(percent_na = (sum(is.na(NEE_PI))/n()*100),
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
colnames(base[[64]])
base[[64]] <- base[[64]] %>% group_by(year, month) %>%
  dplyr::summarise(percent_na = (sum(is.na(NEE_PI))/n()*100),
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
colnames(base[[65]])
base[[65]] <- base[[65]] %>% group_by(year, month) %>%
  dplyr::summarise(percent_na = (sum(is.na(NEE_PI))/n()*100),
                   NEE= mean(NEE_PI, na.rm=FALSE),
                   TA= mean(c(TA_1_1_1, TA_1_1_2), na.rm=FALSE),
                   PPFD= mean(PPFD_IN_1_1_1, na.rm=FALSE),
                   SWC= mean(c(SWC_1_1_1, SWC_2_1_1,
                               SWC_3_1_1, SWC_4_1_1, SWC_5_1_1), na.rm=FALSE),
                   TS= mean(c(TS_1_1_1, TS_2_1_1,
                              TS_3_1_1, TS_4_1_1, TS_5_1_1), na.rm=FALSE),
                   P= sum(P, na.rm=FALSE))#not gapfilled
###US-xHE####---------------------------------------------------------------------------
colnames(base[[66]])
base[[66]] <- base[[66]] %>% group_by(year, month) %>%
  dplyr::summarise(percent_na = (sum(is.na(NEE_PI))/n()*100),
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
colnames(base[[67]])
base[[67]] <- base[[67]] %>% group_by(year, month) %>%
  dplyr::summarise(percent_na = (sum(is.na(NEE_PI))/n()*100),
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
base2 <- base[-c(1,2,4,6,7:18,20,21,23:28,30:34,37:39,50,60:62)]
#turn list  into one df
base.monthly <- bind_rows(base2, .id = "site_id")
#Convert units to match ABCflux v2
base.monthly$ts <- as.yearmon(paste(base.monthly$year, base.monthly$month, sep = "-"))
base.monthly$NEE <- base.monthly$NEE*1.0368 * days_in_month(base.monthly$ts)#micromole per sec to gC per day
base.monthly$GPP <- base.monthly$GPP*1.0368 * days_in_month(base.monthly$ts)
base.monthly$RECO <- base.monthly$RECO*1.0368 * days_in_month(base.monthly$ts)
base.monthly$FCH4 <- base.monthly$FCH4*0.0010368 * days_in_month(base.monthly$ts)#nanomole per sec to gC per day
base.monthly$ts <- NULL
#turn NaNs into NAs
base.monthly <- base.monthly %>% mutate_all(~ifelse(is.nan(.), NA, .))
#remove rows that do not contain flux data
base.monthly2 <- base.monthly %>%
  filter(if_any(c("NEE", "GPP", "RECO","FCH4"), ~ !is.na(.)))
#adding data usage policies
base.monthly2  <- base.monthly2  %>% 
  mutate(data_usage= ifelse(site_id %in% c('CA-HPC',"CA-NS1","CA-NS2","CA-NS3","CA-NS4","CA-NS5","CA-NS6",
                                           "CA-NS7","CA-NS8","CA-Ojp","CA-Qc2","CA-SCC","CA-SCB","CA-SJ1",
                                           "CA-SJ2","CA-SJ3","CA-SMC","CA-TVC","CA-WP1","CA-WP2","CA-WP3",
                                           "US-Atq","US-Beo","US-Bes","US-Bn1","US-Bn2","US-Bn3","US-Brw",
                                           "US-Cms","US-HVa","US-HVs","US-Ivo","US-Sag","US-Upa"), "Tier 2", "Tier 1"))
### Adding in other variables
setwd("/Users/iwargowsky/Desktop/Ameriflux/AMF-BASE")
meta <- read_xlsx("AMF_AA-Net_BIF_LEGACY_20221208.xlsx")
#filter for sites of interest
names <- unique(base.monthly2$site_id)
meta <- meta %>% filter(SITE_ID %in% names)
#make better format and group by site
meta.wide <- meta %>% pivot_wider(names_from = VARIABLE, values_from = DATAVALUE) 
meta.bysite <- meta.wide %>% group_by(SITE_ID) %>% summarise(citation = na.omit(DOI),
                                                             country= na.omit(COUNTRY),
                                                             site_name= na.omit(SITE_NAME),
                                                             latitude= na.omit(LOCATION_LAT),
                                                             longitude= na.omit(LOCATION_LONG))
#merge flux df and meta data
meta.bysite<- meta.bysite %>% dplyr::rename(site_id= SITE_ID)
base.ALL <- left_join(base.monthly2, meta.bysite)
base.ALL$extraction_source <- "Ameriflux BASE"
#save
setwd("/Users/iwargowsky/Desktop/ABCFlux v2")
write_csv(base.ALL , "ameriflux.base.gapfilled.csv")


####extract list of sites and dates covered###
base.monthly2$ts <- paste(base.monthly2$year, base.monthly2$month)
sites <- subset(base.monthly2, select = c(site_id,ts))
sites.datescovered2 <- sites %>% group_by(site_id) %>% dplyr::summarise(start_date = min(ts),
                                                                        end_date = max(ts))
#double checking that function above worked
checkdates <- sites %>% arrange(site_id, ts)





#----------------------------------------------------------------------------------
#PROCESSING ARCTIC DATA CENTER AMERIFLUX BASE FILES
setwd("/Users/iwargowsky/Desktop/arcticdatacenter/downloads/ameriflux")
path <- "/Users/iwargowsky/Desktop/arcticdatacenter/downloads/ameriflux"
files <- list.files(path = path,pattern = '*_HH_',all.files = T,recursive = T)
#load in files as a list of df
adcbase.dat <-  lapply(files,function(i){
  fread(i, na.strings =c("NA","-9999"), skip= 2)
})
#add year, month, day columns
adc.base <- lapply(adcbase.dat, function(df) df %>%
                 mutate( year = substr(df$TIMESTAMP_START, 1,4),
                         month = substr(df$TIMESTAMP_START, 5,6)))
###US-Ivo
colnames(adc.base[[12]])
adc.base[[12]] <- adc.base[[12]] %>% group_by(year, month) %>%
  dplyr::summarise(percent_na_nee = (sum(is.na(NEE_PI))/n()*100),
                   percent_na_rh = (sum(is.na(RH))/n()*100),
                   percent_na_PPFD = (sum(is.na(PPFD_IN))/n()*100),
                   percent_na_TA = (sum(is.na(TA))/n()*100),
                   TA= mean(TA, na.rm=FALSE),
                   NEE= mean(NEE_PI, na.rm=FALSE),
                   TS= mean(c(TS_1, TS_2), na.rm=FALSE),
                   P= sum(P, na.rm=FALSE),
                   PPFD= mean(PPFD_IN, na.rm=FALSE),
                   SWC= mean(c(SWC_1, SWC_2), na.rm=FALSE))
#US-Brw
colnames(adc.base[[7]])
adc.base[[7]] <- adc.base[[7]] %>% group_by(year, month) %>%
  dplyr::summarise(NEE= mean(NEE_PI, na.rm=FALSE),
                   percent_na_nee = (sum(is.na(NEE_PI))/n()*100),
                   TA= mean(TA, na.rm=FALSE),
                   percent_na_ta = (sum(is.na(TA))/n()*100),
                   TS= mean(TS_1, na.rm=FALSE),
                   P= sum(P, na.rm=FALSE),
                   SWC= mean(SWC_1, na.rm=FALSE),
                   PPFD= mean(PPFD_IN, na.rm=FALSE),
                   percent_na_ppfd= (sum(is.na(PPFD_IN))/n()*100),
                   RECO= mean(RECO_PI, na.rm=FALSE),
                   percent_na_reco = (sum(is.na(RECO_PI))/n()*100))
#US-Atq
colnames(adc.base[[6]])
adc.base[[6]] <- adc.base[[6]] %>% group_by(year, month) %>%
  dplyr::summarise(percent_na = (sum(is.na(NEE_PI))/n()*100),
                   TA= mean(TA, na.rm=FALSE),
                   NEE= mean(NEE_PI, na.rm=FALSE),
                   TS= mean(TS_1, na.rm=FALSE),
                   P= sum(P, na.rm=FALSE),
                   SWC= mean(SWC_1, na.rm=FALSE),
                   PPFD= mean(PPFD_IN, na.rm=FALSE),
                   percent_na_ppfd = (sum(is.na(PPFD_IN))/n()*100),
                   RECO= mean(RECO_PI, na.rm=FALSE),
                   percent_na_reco = (sum(is.na(RECO_PI))/n()*100))
#US-ICh
colnames(adc.base[[9]])
adc.base[[9]] <- adc.base[[9]] %>% group_by(year, month) %>%
  dplyr::summarise(percent_na_nee = (sum(is.na(NEE_PI))/n()*100),
                   TA= mean(TA, na.rm=FALSE),
                   percent_na_ta = (sum(is.na(TA))/n()*100),
                   PPFD= mean(PPFD_IN, na.rm=FALSE),
                   percent_na_ppfd = (sum(is.na(PPFD_IN))/n()*100),
                   RECO= mean(RECO_PI, na.rm=FALSE),
                   percent_na_reco = (sum(is.na(RECO_PI))/n()*100))
#US-ICs
colnames(adc.base[[10]])
adc.base[[10]] <- adc.base[[10]] %>% group_by(year, month) %>%
  dplyr::summarise(NEE= mean(NEE_PI, na.rm=FALSE),
                   percent_na_nee = (sum(is.na(NEE_PI))/n()*100),
                   PPFD= mean(PPFD_IN, na.rm=FALSE),
                   percent_na_ppfd = (sum(is.na(PPFD_IN))/n()*100),
                   RECO= mean(RECO_PI, na.rm=FALSE),
                   percent_na_reco = (sum(is.na(RECO_PI))/n()*100))
#US-ICt
colnames(adc.base[[11]])
adc.base[[11]] <- adc.base[[11]] %>% group_by(year, month) %>%
  dplyr::summarise(NEE= mean(NEE_PI, na.rm=FALSE),
                   percent_na_nee = (sum(is.na(NEE_PI))/n()*100),
                   PPFD= mean(PPFD_IN, na.rm=FALSE),
                   percent_na_ppfd = (sum(is.na(PPFD_IN))/n()*100),
                   RECO= mean(RECO_PI, na.rm=FALSE),
                   percent_na_reco = (sum(is.na(RECO_PI))/n()*100))
