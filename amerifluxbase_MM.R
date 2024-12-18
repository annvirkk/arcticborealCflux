library(dplyr)
library(data.table)
library(tidyr)
library(readxl)
library(lubridate)
library(zoo)
library(readr)

#this script is for processing Ameriflux BASE data
#this is data that may or may not be gapfilled 

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
  dplyr::summarise(percent_na_nee = (sum(is.na(NEE_PI_F))/n()*100),
                   percent_na_reco = (sum(is.na(RECO_PI))/n()*100),
                   percent_na_gpp = (sum(is.na(GPP_PI))/n()*100),
                   percent_na_ch4 = (sum(is.na(FCH4))/n()*100),
                   NEE= mean(NEE_PI_F, na.rm = TRUE),
                   RECO= mean(RECO_PI, na.rm= TRUE),
                   GPP= mean(GPP_PI, na.rm = TRUE),
                   TA= mean(TA, na.rm= TRUE),
                   PPFD= mean(PPFD_IN, na.rm= TRUE),
                   P= sum(P_RAIN, na.rm= TRUE),
                   TS= mean(TS, na.rm= TRUE),
                   FCH4= mean(FCH4, na.rm = TRUE))
#NEE gapfilled
###CA-CF1####---------------------------------------------------------------------------
colnames(base[[4]])
#date range covered by Ameriflux
###CA-CF2####---------------------------------------------------------------------------
colnames(base[[5]])  
base[[5]] <- base[[5]] %>% group_by(year, month) %>%
  dplyr::summarise(percent_na_ch4 = (sum(is.na(FCH4))/n()*100),
                  TA= mean(TA, na.rm= TRUE),
                   PPFD= mean(PPFD_IN, na.rm= TRUE),
                   P= sum(P_RAIN, na.rm= TRUE),
                   TS= mean(c(TS_1_1_1, TS_2_1_1, TS_3_1_1), na.rm= TRUE),
                   FCH4= mean(FCH4, na.rm = TRUE))
#not gapfilled
###CA-Gro####---------------------------------------------------------------------------
colnames(base[[6]])
#date range covered by Ameriflux
###CA-HPC####---------------------------------------------------------------------------
#colnames(base[[7]])
#base[[7]] <- base[[7]] %>% group_by(year, month) %>%
#  dplyr::summarise(percent_na = (sum(is.na(NEE_PI_F))/n()*100),
#                   SC= mean(SC, na.rm= TRUE),
#                   TA= mean(TA, na.rm= TRUE),
#                   TS= mean(TS_1_1_1, na.rm= TRUE),
#                  PPFD= mean(PPFD_IN, na.rm= TRUE),
 #                  P= sum(P, na.rm= TRUE),
  #                 D_SNOW= mean(D_SNOW, na.rm= TRUE),
   #                FCH4= mean(FCH4_PI_F, na.rm= TRUE),
    #               NEE = mean(NEE_PI_F, na.rm= TRUE),
     #              GPP= mean(GPP_PI_F, na.rm= TRUE),
      #             RECO= mean(RECO_PI_F, na.rm= TRUE))
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
                   #TA= mean(TA, na.rm= TRUE),
                  # NEE= mean(NEE_PI, na.rm= TRUE),
                   #TS= mean(TS_1, na.rm= TRUE),
                  # P= sum(P, na.rm= TRUE),
                  # SWC= mean(SWC_1, na.rm= TRUE),
                  # PPFD= mean(PPFD_IN, na.rm= TRUE),
                  # RECO= mean(RECO_PI, na.rm= TRUE))
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
  dplyr::summarise(percent_na_nee = (sum(is.na(NEE_PI))/n()*100),
                   percent_na_reco = (sum(is.na(RECO_PI))/n()*100),
                   percent_na_gpp = (sum(is.na(GPP_PI))/n()*100),
                   NEE= mean(NEE_PI, na.rm=TRUE),
                   RECO= mean(RECO_PI, na.rm=TRUE),
                   GPP= mean(GPP_PI, na.rm=TRUE),
                   PPFD= mean(c(PPFD_IN_1_1_1, PPFD_IN_1_1_2), na.rm=TRUE),
                   P= sum(c(P_1_1_1, P_1_1_2), na.rm=TRUE),
                   D_SNOW= mean(c(D_SNOW_1_1_1, D_SNOW_1_1_2), na.rm=TRUE),
                   TS= mean(c(TS_1_1_1, TS_1_1_2), na.rm=TRUE),
                   TA= mean(c(TA_1_1_1, TA_1_1_2), na.rm=TRUE),
                   SWC= mean(SWC_1_1_1, na.rm=TRUE))
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
  dplyr::summarise(percent_na_nee = (sum(is.na(NEE_PI_F))/n()*100),
                   percent_na_gpp = (sum(is.na(GPP_PI))/n()*100),
                   percent_na_reco = (sum(is.na(RECO_PI))/n()*100),
                   percent_na_ch4 = (sum(is.na(FCH4))/n()*100),
                   TA= mean(c(TA_1_1_1, TA_1_1_2), na.rm=TRUE),
                   TS= mean(TS_1_1_1, na.rm=TRUE),
                   P= sum(P_RAIN, na.rm=TRUE),
                   D_SNOW= mean(D_SNOW, na.rm=TRUE),
                   FCH4= mean(FCH4_PI_F, na.rm=TRUE),
                   NEE= mean(NEE_PI_F, na.rm=TRUE),
                   GPP= mean(GPP_PI, na.rm=TRUE),
                   RECO= mean(RECO_PI, na.rm=TRUE))
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
  dplyr::summarise(percent_na_nee = (sum(is.na(NEE_PI_F))/n()*100),
                   percent_na_reco = (sum(is.na(RECO_PI_F))/n()*100),
                   percent_na_gpp = (sum(is.na(GPP_PI_F))/n()*100),
                   NEE= mean(NEE_PI_F, na.rm=TRUE),
                   RECO= mean(RECO_PI_F, na.rm=TRUE),
                   GPP= mean(GPP_PI_F, na.rm=TRUE),
                   PPFD= mean(PPFD_IN_PI_F_1_1_1, na.rm=TRUE),
                   TA= mean(c(TA_PI_F_1_1_1, TA_PI_F_1_1_2), na.rm=TRUE),
                   D_SNOW= mean(D_SNOW_PI_F, na.rm=TRUE),
                   TS= mean(c(TS_PI_F_1_1_1, TS_PI_F_2_1_1), na.rm=TRUE),
                   SWC= mean(SWC_PI_F_1, na.rm=TRUE))
# gapfilled only 2004-2006
###CA-SMC####---------------------------------------------------------------------------
#colnames(base[[30]])
#base[[30]] <- base[[30]] %>% group_by(year, month) %>%
 # dplyr::summarise(percent_na = (sum(is.na(NEE_PI_F))/n()*100),
  #                 SC= mean(SC, na.rm=TRUE),
   #                TA= mean(c(TA_1_1_1, TA_2_1_1), na.rm=TRUE),
    #               TS= mean(TS_1_1_1, na.rm=TRUE),
     #              PPFD= mean(PPFD_IN, na.rm=TRUE),
      #             FCH4= mean(FCH4_PI_F, na.rm=TRUE),
       #            NEE= mean(NEE_PI_F, na.rm=TRUE),
        #           GPP= mean(GPP_PI, na.rm=TRUE),
         #          RECO= mean(RECO_PI, na.rm=TRUE))
#Only NEE is gapfilled starting mid april - mid november
#Data received from Haley Alcock covers this date range
###CA-TVC####---------------------------------------------------------------------------
#colnames(base[[31]])
#base[[31]] <- base[[31]] %>% group_by(year, month) %>%
#  dplyr::summarise(percent_na = (sum(is.na(NEE_PI_F))/n()*100),
 #                  TA= mean(TA_PI_F, na.rm=TRUE),
  #                 P= sum(P, na.rm=TRUE),
   #                D_SNOW= mean(D_SNOW, na.rm=TRUE),
    #               FCH4= mean(FCH4_PI_F, na.rm=TRUE),
     #              NEE= mean(NEE_PI_F, na.rm=TRUE),
      #             GPP= mean(GPP_PI, na.rm=TRUE),
       #            RECO= mean(RECO_PI, na.rm=TRUE))
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
  dplyr::summarise(percent_na_nee = (sum(is.na(NEE_PI))/n()*100),
                   percent_na_ch4 = (sum(is.na(FCH4))/n()*100),
                   NEE= mean(NEE_PI, na.rm=TRUE),
                   FCH4= mean(FCH4, na.rm=TRUE),
                   PPFD= mean(PPFD_IN, na.rm=TRUE),
                   TS= mean(TS_PI_1_1_A, na.rm=TRUE),
                   SWC= mean(SWC_PI_1_1_A, na.rm=TRUE))
#not gapfilled, 2015-2018 in fluxnetch4
###US-A10####---------------------------------------------------------------------------
colnames(base[[36]])
base[[36]] <- base[[36]] %>% group_by(year, month) %>%
  dplyr::summarise(percent_na_nee = (sum(is.na(NEE_PI))/n()*100),
                   percent_na_ch4 = (sum(is.na(FCH4))/n()*100),
                   NEE= mean(NEE_PI, na.rm=TRUE),
                   FCH4= mean(FCH4, na.rm=TRUE),
                   PPFD= mean(PPFD_IN, na.rm=TRUE),
                   TS= mean(TS_PI_1_1_A, na.rm=TRUE),
                   SWC= mean(SWC_PI_1_1_A, na.rm=TRUE))
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
  dplyr::summarise(percent_na_nee = (sum(is.na(NEE_PI))/n()*100),
                   percent_na_reco = (sum(is.na(RECO_PI))/n()*100),
                   TA= mean(TA, na.rm=TRUE),
                   NEE= mean(NEE_PI, na.rm=TRUE),
                   TS= mean(TS_1, na.rm=TRUE),
                   P= sum(P, na.rm=TRUE),
                   SWC= mean(SWC_1, na.rm=TRUE),
                   PPFD= mean(PPFD_IN, na.rm=TRUE),
                   RECO= mean(RECO_PI, na.rm=TRUE))
#Arctic data center is also not gap filled and same dates
###US-Bn1####-----------------------NO NEE DATA----------------------------------------------------
colnames(base[[41]])
base[[41]] <- base[[41]] %>% group_by(year, month) %>%
  dplyr::summarise(percent_na_nee = (sum(is.na(NEE_PI))/n()*100),
                   percent_na_reco = (sum(is.na(RECO_PI))/n()*100),
                   TA= mean(TA, na.rm=TRUE),
                   NEE= mean(NEE_PI, na.rm=TRUE),
                   TS= mean(TS_1, na.rm=TRUE),
                   P= sum(P, na.rm=TRUE),
                   SWC= mean(SWC_1, na.rm=TRUE),
                   PPFD= mean(PPFD_IN, na.rm=TRUE),
                   RECO= mean(RECO_PI, na.rm=TRUE))
###US-Bn2####---------------------NO NEE DATA------------------------------------------------------
colnames(base[[42]])
base[[42]] <- base[[42]] %>% group_by(year, month) %>%
  dplyr::summarise(percent_na_nee = (sum(is.na(NEE_PI))/n()*100),
                   percent_na_reco = (sum(is.na(RECO_PI))/n()*100),
                   TA= mean(TA, na.rm=TRUE),
                   NEE= mean(NEE_PI, na.rm=TRUE),
                   TS= mean(TS_1, na.rm=TRUE),
                   P= sum(P, na.rm=TRUE),
                   SWC= mean(SWC_1, na.rm=TRUE),
                   PPFD= mean(PPFD_IN, na.rm=TRUE),
                   RECO= mean(RECO_PI, na.rm=TRUE))

###US-Bn3####--------------------NO NEE DATA-------------------------------------------------------
colnames(base[[43]])
base[[43]] <- base[[43]] %>% group_by(year, month) %>%
  dplyr::summarise(percent_na_nee = (sum(is.na(NEE_PI))/n()*100),
                   percent_na_reco = (sum(is.na(RECO_PI))/n()*100),
                   TA= mean(TA, na.rm=TRUE),
                   NEE= mean(NEE_PI, na.rm=TRUE),
                   TS= mean(TS_1, na.rm=TRUE),
                   P= sum(P, na.rm=TRUE),
                   SWC= mean(SWC_1, na.rm=TRUE),
                   PPFD= mean(PPFD_IN, na.rm=TRUE),
                   RECO= mean(RECO_PI, na.rm=TRUE))

###US-Brw####---------------------------------------------------------------------------
# colnames(base[[44]])
# base[[44]] <- base[[44]] %>% group_by(year, month) %>%
#   dplyr::summarise(percent_na_nee = (sum(is.na(NEE_PI))/n()*100),
#                    percent_na_reco = (sum(is.na(RECO_PI))/n()*100),
#                    TA= mean(TA, na.rm=TRUE),
#                    NEE= mean(NEE_PI, na.rm=TRUE),
#                    TS= mean(TS_1, na.rm=TRUE),
#                    P= sum(P, na.rm=TRUE),
#                    SWC= mean(SWC_1, na.rm=TRUE),
#                    PPFD= mean(PPFD_IN, na.rm=TRUE),
#                    RECO= mean(RECO_PI, na.rm=TRUE))

#RECO column is empty
#Not gapfilled 
#Arctic data center is also not gapfilled and same dates
#Anna said to remove this whole time series 4/30/24
###US-BZB####---------------------------------------------------------------------------
colnames(base[[45]])
base[[45]] <- base[[45]] %>% group_by(year, month) %>%
  dplyr::summarise(percent_na_gpp = (sum(is.na(GPP_PI))/n()*100),
                   percent_na_reco = (sum(is.na(RECO_PI))/n()*100),
                   percent_na_ch4 = (sum(is.na(FCH4_PI_F))/n()*100),
                   TA= mean(TA_PI_F, na.rm=TRUE),
                   TS= mean(TS_PI_F_1, na.rm=TRUE),
                   P= sum(P_PI_F, na.rm=TRUE),
                   SWC= mean(SWC_PI_1, na.rm=TRUE),
                   PPFD= mean(PPFD_IN, na.rm=TRUE),
                   FCH4= mean(FCH4_PI_F, na.rm=TRUE),
                   D_SNOW= mean(D_SNOW_PI_F, na.rm=TRUE),
                   GPP= mean(GPP_PI, na.rm=TRUE),
                   RECO= mean(RECO_PI, na.rm=TRUE))

#summer months gapfilled but most dates covered by Ameriflux
###US-BZF####---------------------------------------------------------------------------
colnames(base[[46]])
base[[46]] <- base[[46]] %>% group_by(year, month) %>%
  dplyr::summarise(percent_na_gpp = (sum(is.na(GPP_PI))/n()*100),
                   percent_na_reco = (sum(is.na(RECO_PI))/n()*100),
                   percent_na_ch4 = (sum(is.na(FCH4_PI_F))/n()*100),
                   FCH4= mean(FCH4_PI_F, na.rm=TRUE),
                   TA= mean(TA_PI_F, na.rm=TRUE),
                   TS= mean(TS_PI_F_1, na.rm=TRUE),
                   P= sum(P_PI_F, na.rm=TRUE),
                   SWC= mean(SWC_PI_1, na.rm=TRUE),
                   PPFD= mean(PPFD_IN_PI_F, na.rm=TRUE),
                   D_SNOW= mean(D_SNOW, na.rm=TRUE),
                   GPP= mean(GPP_PI, na.rm=TRUE),
                   RECO= mean(RECO_PI, na.rm=TRUE))

#summer months gapfilled but most dates covered by Ameriflux
###US-BZo####---------------------------------------------------------------------------
colnames(base[[47]])
base[[47]] <- base[[47]] %>% group_by(year, month) %>%
  dplyr::summarise(percent_na_gpp = (sum(is.na(GPP_PI))/n()*100),
                   percent_na_reco = (sum(is.na(RECO_PI))/n()*100),
                   percent_na_ch4 = (sum(is.na(FCH4_PI_F))/n()*100),
                   FCH4= mean(FCH4_PI_F, na.rm=TRUE),
                   TA= mean(TA_PI_F, na.rm=TRUE),
                   TS= mean(TS_PI_F_1, na.rm=TRUE),
                   PPFD= mean(PPFD_IN, na.rm=TRUE),
                   P= sum(P, na.rm=TRUE),
                   SWC= mean(SWC_PI_1, na.rm=TRUE),
                   GPP= mean(GPP_PI, na.rm=TRUE),
                   RECO= mean(RECO_PI, na.rm=TRUE))

#summer months gapfilled but most dates covered by Ameriflux
###US-BZS####---------------------------------------------------------------------------
colnames(base[[48]])
base[[48]] <- base[[48]] %>% group_by(year, month) %>%
  dplyr::summarise(percent_na_gpp = (sum(is.na(GPP_PI))/n()*100),
                   percent_na_reco = (sum(is.na(RECO_PI))/n()*100),
                   percent_na_ch4 = (sum(is.na(FCH4_PI_F))/n()*100),
                   TA= mean(TA_PI_F, na.rm=TRUE),
                   TS= mean(TS_PI_F_1, na.rm=TRUE),
                   PPFD= mean(PPFD_IN_PI_F, na.rm=TRUE),
                   P= sum(P_PI_F, na.rm=TRUE),
                   D_SNOW= mean(D_SNOW_PI_F, na.rm=TRUE),
                   SWC= mean(SWC_PI_1, na.rm=TRUE),
                   GPP= mean(GPP_PI, na.rm=TRUE),
                   RECO= mean(RECO_PI, na.rm=TRUE),
                   FCH4= mean(FCH4_PI_F, na.rm=TRUE))

#summer months gapfilled but most dates covered by Ameriflux
###US-EML####---------------------------------------------------------------------------
colnames(base[[49]])
base[[49]] <- base[[49]] %>% group_by(year, month) %>%
  dplyr::summarise(percent_na_nee = (sum(is.na(NEE_PI_F))/n()*100),
                   percent_na_reco = (sum(is.na(RECO_PI_F))/n()*100),
                   percent_na_gpp = (sum(is.na(GPP_PI_F))/n()*100),
                   percent_na_ch4 = (sum(is.na(FCH4))/n()*100),
                   TA= mean(TA_PI_F, na.rm=TRUE),
                   SWC= mean(c(SWC_1_1_1, SWC_2_1_1), na.rm=TRUE),
                   TS= mean(c(TS, TS_1_1_1, TS_2_1_1), na.rm=TRUE),
                   PPFD= mean(PPFD_IN_PI_F, na.rm=TRUE),
                   D_SNOW= mean(D_SNOW, na.rm=TRUE),
                   NEE= mean(NEE_PI_F, na.rm=TRUE),
                   GPP= mean(GPP_PI_F, na.rm=TRUE),
                   RECO= mean(RECO_PI_F, na.rm=TRUE),
                   FCH4= mean(FCH4, na.rm=TRUE))
#gapfilled
###US-Fcr####---------------------------------------------------------------------------
#colnames(base[[50]])
#base[[50]] <- base[[50]] %>% group_by(year, month) %>%
 # dplyr::summarise(percent_na = (sum(is.na(NEE_PI))/n()*100),
  #                 NEE= mean(NEE_PI, na.rm=TRUE),
    #               GPP= mean(GPP_PI_F, na.rm=TRUE),
     #              RECO= mean(RECO_PI_F, na.rm=TRUE),
      #             TA= mean(TA_PI_F_1_1_1, na.rm=TRUE),
       #            P= sum(P_RAIN, na.rm=TRUE),
        #           PPFD= mean(PPFD_IN_PI_F, na.rm=TRUE),
         #          SWC= mean(SWC_1_1_2, na.rm=TRUE),
          #         TS= mean(TS_PI_F_1_1_1, na.rm=TRUE))
#RECO AND GPP are gapfilled 
#data from Masahito Ueyama covers this date range
###US-HVa####---------------------------------------------------------------------------
colnames(base[[51]])
base[[51]] <- base[[51]] %>% group_by(year, month) %>%
  dplyr::summarise(percent_na_nee = (sum(is.na(NEE_PI))/n()*100),
                   percent_na_reco = (sum(is.na(RECO_PI))/n()*100),
                   TA= mean(TA, na.rm=TRUE),
                   NEE= mean(NEE_PI, na.rm=TRUE),
                   TS= mean(c(TS_1, TS_2), na.rm=TRUE),
                   P= sum(P, na.rm=TRUE),
                   SWC= mean(SWC_1, na.rm=TRUE),
                   PPFD= mean(PPFD_IN, na.rm=TRUE),
                   RECO= mean(RECO_PI, na.rm=TRUE))
#RECO column empty
#Not gapfilled but some months may be complete
#data from Masahito Ueyama covers SOME of this date range
###US-HVs####---------------------------------------------------------------------------
colnames(base[[52]])
base[[52]] <- base[[52]] %>% group_by(year, month) %>%
  dplyr::summarise(percent_na_nee = (sum(is.na(NEE_PI_F_1_1_1))/n()*100),
                   percent_na_reco = (sum(is.na(RECO_PI_F_1_1_1))/n()*100),
                   percent_na_gpp = (sum(is.na(GPP_PI_F_1_1_1))/n()*100),
                   TA= mean(c(TA_1_1_1, TA_2_1_1), na.rm=TRUE),
                   NEE= mean(NEE_PI_F_1_1_1, na.rm=TRUE),
                   TS= mean(TS_1_1_1, na.rm=TRUE),
                   RECO= mean(RECO_PI_F_1_1_1, na.rm=TRUE),
                   GPP= mean(GPP_PI_F_1_1_1, na.rm = TRUE))

###US-ICh####---------------------------------------------------------------------------
colnames(base[[53]])
base[[53]] <- base[[53]] %>% group_by(year, month) %>%
  dplyr::summarise(percent_na_reco = (sum(is.na(RECO_PI))/n()*100),
                   percent_na_gpp = (sum(is.na(GPP_PI))/n()*100),
                   percent_na_ch4 = (sum(is.na(FCH4_PI_F))/n()*100),
                   TA= mean(TA_PI_F, na.rm=TRUE),
                   TS= mean(c(TS_PI_1, TS_PI_2), na.rm=TRUE),
                   P= sum(P, na.rm=TRUE),
                   SWC= mean(c(SWC_PI_1, SWC_PI_2), na.rm=TRUE),
                   PPFD= mean(PPFD_IN, na.rm=TRUE),
                   D_SNOW= mean(D_SNOW, na.rm=TRUE),
                   RECO= mean(RECO_PI, na.rm=TRUE),
                   GPP= mean(GPP_PI, na.rm=TRUE),
                   FCH4= mean(FCH4_PI_F, na.rm=TRUE))
#summer months gapfilled
###US-ICs####---------------------------------------------------------------------------
colnames(base[[54]])
base[[54]] <- base[[54]] %>% group_by(year, month) %>%
  dplyr::summarise(percent_na_ch4 = (sum(is.na(FCH4_PI_F))/n()*100),
                   percent_na_reco = (sum(is.na(RECO_PI))/n()*100),
                   percent_na_gpp = (sum(is.na(GPP_PI))/n()*100),
                   TA= mean(TA_PI_F, na.rm=TRUE),
                   TS= mean(c(TS_PI_1, TS_PI_2), na.rm=TRUE),
                   PPFD= mean(PPFD_IN, na.rm=TRUE),
                   P= sum(P, na.rm=TRUE),
                   D_SNOW= mean(D_SNOW, na.rm=TRUE),
                   SWC= mean(c(SWC_PI_1, SWC_PI_2), na.rm=TRUE),
                   RECO= mean(RECO_PI, na.rm=TRUE),
                   GPP= mean(GPP_PI, na.rm=TRUE),
                   FCH4= mean(FCH4_PI_F, na.rm=TRUE))

#Data seems a little strange GPP= 0 A LOT
#gapfilled mid sept 2007 through 2021
#Ameriflux covers 2007-2021
###US-ICt####---------------------------------------------------------------------------
colnames(base[[55]])
base[[55]] <- base[[55]] %>% group_by(year, month) %>%
  dplyr::summarise(TA= mean(TA_PI_F, na.rm=TRUE),
                   TS= mean(c(TS_1_1_1, TS_2_1_1), na.rm=TRUE),
                   PPFD= mean(PPFD_IN, na.rm=TRUE),
                   P= sum(P, na.rm=TRUE),
                   SWC= mean(c(SWC_1_1_1, SWC_2_1_1), na.rm=TRUE),
                   TS= mean(c(TS_1_1_1, TS_2_1_1), na.rm=TRUE),
                   percent_na_reco = (sum(is.na(RECO_PI))/n()*100),
                   percent_na_gpp = (sum(is.na(GPP_PI))/n()*100),
                   percent_na_ch4 = (sum(is.na(FCH4_PI_F))/n()*100),
                   RECO= mean(as.numeric(RECO_PI), na.rm=TRUE),
                   GPP= mean(as.numeric(GPP_PI), na.rm=TRUE),
                   FCH4= mean(as.numeric(FCH4_PI_F), na.rm=TRUE))
#Data seems a little strange GPP= 0 A LOT
#gapfilled 2021, rest NAs
#Ameriflux covers 2007-2021
###US-Ivo####------------------NO NEE DATA--------------------------------------------------------
colnames(base[[56]])
base[[56]] <- base[[56]] %>% group_by(year, month) %>%
  dplyr::summarise(percent_na_nee = (sum(is.na(NEE_PI))/n()*100),
                   percent_na_ch4 = (sum(is.na(FCH4))/n()*100),
                   TA= mean(TA, na.rm=TRUE),
                   NEE= mean(NEE_PI, na.rm=TRUE),
                   TS= mean(c(TS_PI_1, TS_PI_2), na.rm=TRUE),
                   P= sum(P, na.rm=TRUE),
                   PPFD= mean(PPFD_IN, na.rm=TRUE),
                   FCH4= mean(FCH4, na.rm=TRUE),
                   SWC= mean(c(SWC_2_1_1, SWC_3_1_1), na.rm=TRUE))
#No NEE data 
#FCH4 not gapfilled 
#Arctic data center has NEE data but it is not gap filled 
###US-KPL####---------------------------------------------------------------------------
colnames(base[[57]])
base[[57]] <- base[[57]] %>% group_by(year, month) %>%
  dplyr::summarise(percent_na_ch4 = (sum(is.na(FCH4))/n()*100),
                   FCH4= mean(FCH4, na.rm=TRUE),
                   PPFD= mean(PPFD_IN, na.rm=TRUE),
                   P= sum(P_RAIN, na.rm=TRUE),
                   SWC= mean(c(SWC_1_1_1, SWC_2_1_1, SWC_3_1_1), na.rm=TRUE),
                   TA= mean(TA, na.rm=TRUE))
#not gapfilled
#only data for summer months
###US-NGB####---------------------------------------------------------------------------
colnames(base[[58]])
base[[58]] <- base[[58]] %>% group_by(year, month) %>%
  dplyr::summarise(percent_na_ch4 = (sum(is.na(FCH4))/n()*100),
                   FCH4= mean(FCH4, na.rm=TRUE),
                   PPFD= mean(PPFD_IN, na.rm=TRUE))
#not gapfilled
#some dates covered by FluxnetCH4 and Ameriflux
###US-NGC####---------------------------------------------------------------------------
colnames(base[[59]])
base[[59]] <- base[[59]] %>% group_by(year, month) %>%
  dplyr::summarise(percent_na_ch4 = (sum(is.na(FCH4))/n()*100),
                   FCH4= mean(FCH4, na.rm=TRUE),
                   PPFD= mean(PPFD_IN, na.rm=TRUE),
                   TS= mean(c(TS_1_1_1, TS_2_1_1, TS_3_1_1, TS_4_1_1, TS_5_1_1), na.rm=TRUE),
                   SWC= mean(c(SWC_1_1_1, SWC_2_1_1, SWC_3_1_1), na.rm=TRUE))
#not gapfilled 
#some dates covered by FluxnetCH4
###US-Prr####---------------------------------------------------------------------------
colnames(base[[60]])
base[[60]] <- base[[60]] %>% group_by(year, month) %>%
  dplyr::summarise(percent_na_nee = (sum(is.na(FC_PI_F_1_1_1))/n()*100),
                   percent_na_gpp = (sum(is.na(GPP_PI_1_1_1))/n()*100),
                   percent_na_reco = (sum(is.na(RECO_PI_1_1_1))/n()*100),
                   NEE= mean(c(FC_PI_F_1_1_1, FC_PI_F_1_2_1), na.rm = TRUE),
                   GPP= mean(c(GPP_PI_1_1_1, GPP_PI_1_2_1), na.rm = TRUE),
                   RECO= mean(c(RECO_PI_1_1_1, RECO_PI_1_2_1), na.rm = TRUE),
                   TA= mean(c(TA_1_9_1, TA_1_7_1, TA_1_6_1, TA_1_3_1, TA_1_2_1, TA_1_1_1,
                              TA_1_5_1, TA_1_4_1, TA_1_8_1), na.rm = TRUE),
                   PPFD= mean(PPFD_IN, na.rm=TRUE),
                   P= sum(P_RAIN, na.rm = TRUE),
                   TS= mean(TS_1_1_1, na.rm=TRUE),
                   D_SNOW= mean(c(D_SNOW_1_1_1, D_SNOW_1_1_2, D_SNOW_1_1_3), na.rm= TRUE),
                   SWC= mean(SWC_1_1_1, na.rm=TRUE))
###US-Rpf####---------------------------------------------------------------------------
#colnames(base[[61]])
#base[[61]] <- base[[61]] %>% group_by(year, month) %>%
 # dplyr::summarise(percent_na = (sum(is.na(NEE_PI))/n()*100),
  #                 NEE= mean(NEE_PI, na.rm=TRUE),
    #               GPP= mean(GPP_PI_F, na.rm=TRUE),
     #              RECO= mean(RECO_PI_F, na.rm=TRUE),
      #             TA= mean(TA_PI_F_1_1_1, na.rm=TRUE),
       #            P= sum(P_RAIN, na.rm=TRUE),
        #           PPFD= mean(PPFD_IN, na.rm=TRUE),
         #          SWC= mean(c(SWC_1_1_1, SWC_1_1_2, SWC_1_1_3), na.rm=TRUE),
          #         D_SNOW= mean(D_SNOW, na.rm=TRUE),
           #        TS= mean(c(TS_PI_F_1_1_1, TS_PI_F_1_1_2, TS_PI_F_1_1_3,
            #                  TS_PI_F_1_1_4, TS_PI_F_1_1_5), na.rm=TRUE))
#base[[61]]$NEE <- base[[61]]$RECO-base[[61]]$GPP
#NEE not gapfilled but RECO and GPP are mostly gapfilled starting aug 2008
#data from Masahito Ueyama covers this date range
###US-Uaf####---------------------------------------------------------------------------
#colnames(base[[62]])
#base[[62]] <- base[[62]] %>% group_by(year, month) %>%
 # dplyr::summarise(percent_na = (sum(is.na(NEE_PI_F))/n()*100),
  #                 TA= mean(TA_PI_F_1_1_1, na.rm=TRUE),
   #                P= sum(P_RAIN_PI_F, na.rm=TRUE),
    #               SWC= mean(c(SWC_PI_F_1_1_1, SWC_PI_F_2_1_1), na.rm=TRUE),
     #              FCH4= mean(FCH4, na.rm=TRUE),
       #            D_SNOW= mean(D_SNOW, na.rm=TRUE),
        #           TS= mean(c(TS_PI_F_1_1_1, TS_PI_F_2_1_1, TS_PI_F_3_1_1), na.rm=TRUE),
         #          NEE= mean(NEE_PI_F, na.rm=TRUE),
          #         GPP= mean(GPP_PI_F, na.rm=TRUE),
           #        RECO= mean(RECO_PI_F, na.rm=TRUE))
#data from Masahito Ueyama covers this date range
#gapfilled
###US-Upa####---------------------------------------------------------------------------
colnames(base[[63]])
#NO NEE, GPP, RECO, or CH4 so excluding this site
###US-xBA####---------------------------------------------------------------------------
colnames(base[[64]])
base[[64]] <- base[[64]] %>% group_by(year, month) %>%
  dplyr::summarise(percent_na_nee = (sum(is.na(NEE_PI))/n()*100),
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
colnames(base[[65]])
base[[65]] <- base[[65]] %>% group_by(year, month) %>%
  dplyr::summarise(percent_na_nee = (sum(is.na(NEE_PI))/n()*100),
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
colnames(base[[66]])
base[[66]] <- base[[66]] %>% group_by(year, month) %>%
  dplyr::summarise(percent_na_nee = (sum(is.na(NEE_PI))/n()*100),
                   NEE= mean(NEE_PI, na.rm=TRUE),
                   TA= mean(c(TA_1_1_1, TA_1_1_2), na.rm=TRUE),
                   PPFD= mean(PPFD_IN_1_1_1, na.rm=TRUE),
                   SWC= mean(c(SWC_1_1_1, SWC_2_1_1,
                               SWC_3_1_1, SWC_4_1_1, SWC_5_1_1), na.rm=TRUE),
                   TS= mean(c(TS_1_1_1, TS_2_1_1,
                              TS_3_1_1, TS_4_1_1, TS_5_1_1), na.rm=TRUE),
                   P= sum(P, na.rm=TRUE))#not gapfilled
###US-xHE####---------------------------------------------------------------------------
colnames(base[[67]])
base[[67]] <- base[[67]] %>% group_by(year, month) %>%
  dplyr::summarise(percent_na_nee = (sum(is.na(NEE_PI))/n()*100),
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
colnames(base[[68]])
base[[68]] <- base[[68]] %>% group_by(year, month) %>%
  dplyr::summarise(percent_na_nee = (sum(is.na(NEE_PI))/n()*100),
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
names(base)<- files #name each df
#remove dfs that do not have our variables of interest or dates are covered by another database
base2 <- base[-c(1,2,4,6:18,20,21,23:28,30:34,37:39,44,50,61:63)]
#turn list  into one df
base.monthly <- bind_rows(base2, .id = "site_id")
#turn NaNs into NAs
base.monthly <- base.monthly %>% mutate_all(~ifelse(is.nan(.), NA, .))
# #filter for months with 98% of data
# base.monthly <- base.monthly %>% mutate(NEE= ifelse(percent_na_nee <2, NEE, NA))%>% 
#                                  mutate(GPP= ifelse(percent_na_gpp <2, GPP, NA))%>% 
#                                  mutate(RECO= ifelse(percent_na_reco <2, RECO, NA))%>% 
#                                  mutate(FCH4= ifelse(percent_na_gpp <2, FCH4, NA))
# base.monthly$percent_na_nee <- NULL
# base.monthly$percent_na_reco <- NULL
# base.monthly$percent_na_gpp <- NULL
# base.monthly$percent_na_ch4 <- NULL
base.monthly <- base.monthly %>% dplyr::rename("gap_fill_perc_nee"= "percent_na_nee",
                                               "gap_fill_perc_gpp"= "percent_na_gpp",
                                               "gap_fill_perc_reco"= "percent_na_reco",
                                               "gap_fill_perc_ch4"= "percent_na_ch4")

##add site_id and data version
base.monthly$data_version <- substr(base.monthly$site_id, 22,24)
base.monthly$data_version_ch4 <- substr(base.monthly$site_id, 22,24)
base.monthly$site_id <- substr(base.monthly$site_id, 5,10)
#Convert units to match ABCflux v2
base.monthly$ts <- as.yearmon(paste(base.monthly$year, base.monthly$month, sep = "-"))
base.monthly$NEE <- base.monthly$NEE*1.0368 * days_in_month(base.monthly$ts) #micromole per sec to gC per day
base.monthly$GPP <- base.monthly$GPP*-1.0368 * days_in_month(base.monthly$ts)
base.monthly$RECO <- base.monthly$RECO*1.0368 * days_in_month(base.monthly$ts)
base.monthly$FCH4 <- base.monthly$FCH4*0.0010368 * days_in_month(base.monthly$ts)#nanomole per sec to gC per day
base.monthly$ts <- NULL
#remove rows that do not contain flux data
base.monthly2 <- base.monthly %>%
  dplyr::filter(if_any(c("NEE", "GPP", "RECO","FCH4"), ~ !is.na(.)))
#adding data usage policies
base.monthly2  <- base.monthly2  %>% 
  mutate(data_usage= ifelse(site_id %in% c("CA-NS8","CA-Ojp","CA-Qc2","CA-SJ3","CA-WP1","CA-WP2","CA-WP3",
                                           "US-Atq","US-Beo","US-Bes","US-Bn1","US-Bn2","US-Bn3","US-Brw",
                                           "US-HVa","US-Ivo","US-Upa"), "Tier 2", "Tier 1")) %>%
  mutate(data_usage_ch4= ifelse(site_id %in% c("CA-NS8","CA-Ojp","CA-Qc2","CA-SJ3","CA-WP1","CA-WP2","CA-WP3",
                                           "US-Atq","US-Beo","US-Bes","US-Bn1","US-Bn2","US-Bn3","US-Brw",
                                           "US-HVa","US-Ivo","US-Upa"), "Tier 2", "Tier 1"))
### Adding in other variables
setwd("/Users/iwargowsky/Desktop/Ameriflux/AMF-BASE")
meta <- read_xlsx("AMF_AA-Net_BIF_LEGACY_20221208.xlsx")
#filter for sites of interest
names <- unique(base.monthly2$site_id)
meta <- meta %>% dplyr::filter(SITE_ID %in% names)
#make better format and group by site
meta.wide <- meta %>% pivot_wider(names_from = VARIABLE, values_from = DATAVALUE) 
meta.bysite <- meta.wide %>% group_by(SITE_ID) %>% dplyr::summarise(citation = na.omit(DOI),
                                                             country= na.omit(COUNTRY))
#merge flux df and meta data
meta.bysite<- meta.bysite %>% dplyr::rename(site_id= SITE_ID)
base.ALL <- left_join(base.monthly2, meta.bysite)

#add gap_fill and extraction source and citation
base.ALL <- base.ALL %>% 
  mutate(gap_fill = ifelse(gap_fill_perc_nee== 0, "Monthly Averages from gapfilled data","Monthly Averages from non-gapfilled data" )) %>%
  mutate(gap_fill_ch4= ifelse(gap_fill_perc_ch4== 0, "Monthly Averages from gapfilled data","Monthly Averages from non-gapfilled data" )) %>%
  mutate(gap_fill = ifelse(is.na(NEE), NA , gap_fill)) %>%
  mutate(gap_fill_ch4= ifelse(is.na(FCH4), NA, gap_fill_ch4 ))

base.ALL <- base.ALL %>% 
  mutate(extraction_source_ch4 = ifelse(!is.na(FCH4),  "AAmeriflux BASE", NA)) %>%
  mutate(extraction_source_co2 = ifelse(!is.na(NEE) | !is.na(GPP) | !is.na(RECO),  "AAmeriflux BASE", NA))

base.ALL <- base.ALL %>% 
  mutate(citation_ch4 = ifelse(!is.na(FCH4),  citation, NA)) %>%
  mutate(citation_co2 = ifelse(!is.na(NEE) | !is.na(GPP) | !is.na(RECO),  citation, NA))%>%
  mutate(citation= NULL)


#remove gap_fill_perc for gapfilled data because it isn't representative of actually gapfill percentage
base.ALL <- base.ALL %>%
  mutate(gap_fill_perc_reco= ifelse(gap_fill==  "Monthly Averages from gapfilled data",NA, gap_fill_perc_reco)) %>%
  mutate(gap_fill_perc_gpp= ifelse(gap_fill==  "Monthly Averages from gapfilled data",NA, gap_fill_perc_gpp)) %>%
  mutate(gap_fill_perc_nee= ifelse(gap_fill==  "Monthly Averages from gapfilled data",NA, gap_fill_perc_nee)) %>%
  mutate(gap_fill_perc_ch4= ifelse(gap_fill_ch4==  "Monthly Averages from gapfilled data",NA, gap_fill_perc_ch4))
  


#save
setwd("/Users/iwargowsky/Desktop/ABCFlux v2")
write_csv(base.ALL , "ameriflux.base.csv")


####extract list of sites and dates covered###
base.monthly2$ts <- paste(base.monthly2$year, base.monthly2$month)
sites <- subset(base.monthly2, select = c(site_id,ts))
sites.datescovered2 <- sites %>% group_by(site_id) %>% dplyr::summarise(start_date = min(ts),
                                                                        end_date = max(ts))
#double checking that function above worked
checkdates <- sites %>% arrange(site_id, ts)

