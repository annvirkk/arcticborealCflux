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
###1####---------------------------------------------------------------------------
colnames(base[[1]]) 
#NO NEE, GPP, RECO, or CH4 so excluding this site
###2####---------------------------------------------------------------------------
colnames(base[[2]])
#NO NEE, GPP, RECO, or CH4 so excluding this site
###3####---------------------------------------------------------------------------
colnames(base[[3]])
#NO NEE, GPP, RECO, or CH4 so excluding this site
###4####---------------------------------------------------------------------------
colnames(base[[4]])  
base[[4]] <- base[[4]] %>% group_by(year, month) %>%
  dplyr::summarise(percent_na = (sum(is.na(FCH4))/n()*100),
                  TA= mean(TA, na.rm= TRUE),
                   PPFD= mean(PPFD_IN, na.rm= TRUE),
                   P= sum(P_RAIN, na.rm= TRUE),
                   TS= mean(c(TS_1_1_1, TS_2_1_1, TS_3_1_1), na.rm= TRUE),
                   FCH4= mean(FCH4, na.rm = TRUE))
###5####---------------------------------------------------------------------------
colnames(base[[5]])
base[[5]] <- base[[5]] %>% group_by(year, month) %>%
  dplyr::summarise(percent_na = (sum(is.na(NEE_PI))/n()*100),
                  TA= mean(TA_1_1_1, na.rm= TRUE),
                   NEE = mean(NEE_PI, na.rm= TRUE),
                   SC= mean(SC_1_1_1, na.rm= TRUE),
                   TS= mean(TS_1_1_1, na.rm= TRUE),
                   P= sum(P_1_1_1, na.rm= TRUE),
                   SWC= mean(SWC_1_1_1, na.rm= TRUE),
                   PPFD= mean(PPFD_IN_1_1_1, na.rm= TRUE),
                   RECO= mean(RECO_PI, na.rm= TRUE))
base[[5]]$GPP <- base[[5]]$NEE-base[[5]]$RECO
###6####---------------------------------------------------------------------------
colnames(base[[6]])
base[[6]] <- base[[6]] %>% group_by(year, month) %>%
  dplyr::summarise(percent_na = (sum(is.na(NEE_PI_F))/n()*100),
                   SC= mean(SC, na.rm= TRUE),
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
###7####---------------------------------------------------------------------------
colnames(base[[7]])
base[[7]] <- base[[7]] %>% group_by(year, month) %>%
  dplyr::summarise(percent_na = (sum(is.na(NEE_PI))/n()*100),
                   TA= mean(TA, na.rm= TRUE),
                   NEE= mean(NEE_PI, na.rm= TRUE),
                   SC= mean(SC, na.rm= TRUE),
                   TS= mean(TS_1, na.rm= TRUE),
                   P=sum(P, na.rm= TRUE),
                   SWC= mean(SWC_1, na.rm= TRUE),
                   PPFD= mean(PPFD_IN, na.rm= TRUE),
                   RECO= mean(RECO_PI, na.rm= TRUE))
base[[7]]$GPP <- base[[7]]$NEE-base[[7]]$RECO
###8####---------------------------------------------------------------------------
colnames(base[[8]])
#NO NEE, GPP, RECO, or CH4 so excluding this site
###9####---------------------------------------------------------------------------
colnames(base[[9]])
#NO NEE, GPP, RECO, or CH4 so excluding this site
###10####---------------------------------------------------------------------------
colnames(base[[10]])
#NO NEE, GPP, RECO, or CH4 so excluding this site
###11####---------------------------------------------------------------------------
colnames(base[[11]])
#NO NEE, GPP, RECO, or CH4 so excluding this site
###12####---------------------------------------------------------------------------
colnames(base[[12]])
#NO NEE, GPP, RECO, or CH4 so excluding this site
###13####---------------------------------------------------------------------------
colnames(base[[13]])
#NO NEE, GPP, RECO, or CH4 so excluding this site
###14####---------------------------------------------------------------------------
colnames(base[[14]])
#NO NEE, GPP, RECO, or CH4 so excluding this site
###15####---------------------------------------------------------------------------
colnames(base[[15]])
base[[15]] <- base[[15]] %>% group_by(year, month) %>%
  dplyr::summarise(percent_na = (sum(is.na(NEE_PI))/n()*100),
                   TA= mean(TA, na.rm= TRUE),
                   NEE= mean(NEE_PI, na.rm= TRUE),
                   SC= mean(SC, na.rm= TRUE),
                   TS= mean(TS_1, na.rm= TRUE),
                   P= sum(P, na.rm= TRUE),
                   SWC= mean(SWC_1, na.rm= TRUE),
                   PPFD= mean(PPFD_IN, na.rm= TRUE),
                   RECO= mean(RECO_PI, na.rm= TRUE))
base[[15]]$GPP <- base[[15]]$NEE-base[[15]]$RECO
###16####---------------------------------------------------------------------------
colnames(base[[16]])
base[[16]] <- base[[16]] %>% group_by(year, month) %>%
  dplyr::summarise(percent_na = (sum(is.na(NEE_PI))/n()*100),
                   TA= mean(TA, na.rm= TRUE),
                   NEE= mean(NEE_PI, na.rm= TRUE),
                   SC= mean(SC, na.rm= TRUE),
                   TS= mean(TS_1, na.rm= TRUE),
                   P= sum(P, na.rm= TRUE),
                   SWC= mean(SWC_1, na.rm= TRUE),
                   PPFD= mean(PPFD_IN, na.rm= TRUE),
                   RECO= mean(RECO_PI, na.rm= TRUE))
base[[16]]$GPP <- base[[16]]$NEE-base[[16]]$RECO
###17####---------------------------------------------------------------------------
colnames(base[[17]])
base[[17]] <- base[[17]] %>% group_by(year, month) %>%
  dplyr::summarise(percent_na = (sum(is.na(NEE_PI))/n()*100),
                   TA= mean(TA, na.rm= TRUE),
                   NEE= mean(NEE_PI, na.rm= TRUE),
                   SC= mean(SC, na.rm= TRUE),
                   TS= mean(TS_1, na.rm= TRUE),
                   P= sum(P, na.rm= TRUE),
                   SWC= mean(SWC_1, na.rm= TRUE),
                   PPFD= mean(PPFD_IN, na.rm= TRUE),
                   RECO= mean(RECO_PI, na.rm= TRUE))
base[[17]]$GPP <- base[[17]]$NEE-base[[17]]$RECO
###18####---------------------------------------------------------------------------
colnames(base[[18]]) 
base[[18]] <- base[[18]] %>% group_by(year, month) %>%
  dplyr::summarise(percent_na = (sum(is.na(NEE_PI))/n()*100),
                   SC= mean(c(SC_1_1_1, SC_1_1_2)),
                   NEE= mean(NEE_PI),
                   RECO= mean(RECO_PI),
                   GPP= mean(GPP_PI),
                   PPFD= mean(c(PPFD_IN_1_1_1, PPFD_IN_1_1_2)),
                   P= sum(c(P_1_1_1, P_1_1_2), na.rm=TRUE),
                   D_SNOW= mean(c(D_SNOW_1_1_1, D_SNOW_1_1_2)),
                   TS= mean(c(TS_1_1_1, TS_1_1_2), na.rm=TRUE),
                   TA= mean(c(TA_1_1_1, TA_1_1_2), na.rm=TRUE),
                   SWC= mean(SWC_1_1_1))
###19####---------------------------------------------------------------------------
colnames(base[[19]])
base[[19]] <- base[[19]] %>% mutate(D_SNOW= as.numeric(D_SNOW))
base[[19]] <- base[[19]] %>% group_by(year, month) %>%
  dplyr::summarise(percent_na = (sum(is.na(NEE_PI_F))/n()*100),
                   TS= mean(c(TS_1_1_1, TS_2_1_1), na.rm=TRUE),
                   NEE= mean(NEE_PI_F, na.rm=TRUE),
                   SC= mean(SC, na.rm=TRUE),
                   RECO= mean(RECO_PI_F, na.rm=TRUE),
                   GPP= mean(GPP_PI_F, na.rm=TRUE),
                   PPFD= mean(PPFD_IN_PI_F_1_1_1, na.rm=TRUE),
                   P= sum(P_RAIN, na.rm=TRUE),
                   D_SNOW= mean(D_SNOW, na.rm=TRUE),
                   TA= mean(TA_PI_F_1, na.rm=TRUE),
                   SWC= mean(SWC_PI_1, na.rm=TRUE))
###20####---------------------------------------------------------------------------
colnames(base[[20]]) 
base[[20]] <- base[[20]] %>% group_by(year, month) %>%
  dplyr::summarise(percent_na = (sum(is.na(NEE_PI))/n()*100),
                   TA= mean(TA_1_1_1, na.rm=TRUE),
                   NEE= mean(NEE_PI, na.rm=TRUE),
                   SC= mean(SC, na.rm=TRUE),
                   TS= mean(TS_1_1_1, na.rm=TRUE),
                   P= sum(P, na.rm=TRUE),
                   SWC= mean(SWC_1_1_1, na.rm=TRUE),
                   PPFD= mean(PPFD_IN_1_1_1, na.rm=TRUE),
                   RECO= mean(RECO_PI, na.rm=TRUE))
base[[20]]$GPP <- base[[20]]$NEE-base[[20]]$RECO
###21####---------------------------------------------------------------------------
colnames(base[[21]])
base[[21]] <- base[[21]] %>% group_by(year, month) %>%
  dplyr::summarise(percent_na = (sum(is.na(NEE_PI_F))/n()*100),
                   SC= mean(SC, na.rm=TRUE),
                   TA= mean(c(TA_1_1_1, TA_1_1_2), na.rm=TRUE),
                   TS= mean(TS_1_1_1, na.rm=TRUE),
                   P= sum(P_RAIN, na.rm=TRUE),
                   D_SNOW= mean(D_SNOW, na.rm=TRUE),
                   FCH4= mean(FCH4_PI_F, na.rm=TRUE),
                   SCH4= mean(SCH4, na.rm=TRUE),
                   NEE= mean(NEE_PI_F, na.rm=TRUE),
                   GPP= mean(GPP_PI, na.rm=TRUE),
                   RECO= mean(RECO_PI, na.rm=TRUE))
##22####---------------------------------------------------------------------------
colnames(base[[22]])
base[[22]] <- base[[22]] %>% group_by(year, month) %>%
  dplyr::summarise(percent_na = (sum(is.na(NEE_PI_F))/n()*100),
                   SC= mean(SC, na.rm=TRUE),
                   TA= mean(c(TA_1_1_1, TA_1_1_2), na.rm=TRUE),
                   SWC= mean(c(SWC_1_1_1, SWC_2_1_1), na.rm=TRUE),
                   TS= mean(TS_1_1_1, na.rm=TRUE),
                   PPFD= mean(PPFD_IN, na.rm=TRUE),
                   P= sum(P_RAIN, na.rm=TRUE),
                   D_SNOW= mean(D_SNOW, na.rm=TRUE),
                   FCH4= mean(FCH4_PI_F, na.rm=TRUE),
                   SCH4= mean(SCH4, na.rm=TRUE),
                   NEE= mean(NEE_PI_F, na.rm=TRUE),
                   GPP= mean(GPP_PI, na.rm=TRUE),
                   RECO= mean(RECO_PI, na.rm=TRUE))
###23####---------------------------------------------------------------------------
colnames(base[[23]])
base[[23]] <- base[[23]] %>% group_by(year, month) %>%
  dplyr::summarise(percent_na = (sum(is.na(NEE_PI))/n()*100),
                   SWC= mean(SWC_1_1_1, na.rm=TRUE),
                   TA= mean(TA_1_1_1, na.rm=TRUE),
                   PPFD= mean(PPFD_IN, na.rm=TRUE),
                   NEE= mean(NEE_PI, na.rm=TRUE),
                   TS= mean(TS_1_1_1, na.rm=TRUE),
                   P= sum(P_RAIN, na.rm=TRUE))
###24####---------------------------------------------------------------------------
colnames(base[[24]])
base[[24]] <- base[[24]] %>% group_by(year, month) %>%
  dplyr::summarise(percent_na = (sum(is.na(NEE_PI))/n()*100),
                   NEE= mean(NEE_PI, na.rm=TRUE),
                   P= sum(P, na.rm=TRUE),
                   PPFD= mean(PPFD_IN, na.rm=TRUE),
                   SC= mean(SC, na.rm=TRUE),
                   SWC= mean(SWC_1_1_1, na.rm=TRUE),
                   TA= mean(TA_1_1_1, na.rm=TRUE),
                   TS= mean(TS_1_1_1, na.rm=TRUE))
###25####---------------------------------------------------------------------------
colnames(base[[25]])
base[[25]] <- base[[25]] %>% group_by(year, month) %>%
  dplyr::summarise(percent_na = (sum(is.na(NEE_PI))/n()*100),
                   SWC= mean(SWC_1_1_1, na.rm=TRUE),
                   TA= mean(TA_1_1_1, na.rm=TRUE),
                   PPFD= mean(PPFD_IN, na.rm=TRUE),
                   NEE= mean(NEE_PI, na.rm=TRUE),
                   TS= mean(TS_1_1_1, na.rm=TRUE),
                   P= sum(P_RAIN, na.rm=TRUE))
###26####---------------------------------------------------------------------------
colnames(base[[26]])
#NO NEE, GPP, RECO, or CH4 so excluding this site
###27####---------------------------------------------------------------------------
colnames(base[[27]])
base[[27]] <- base[[27]] %>% group_by(year, month) %>%
  dplyr::summarise(percent_na = (sum(is.na(NEE_PI_F))/n()*100),
                   RECO= mean(RECO_PI_F, na.rm=TRUE),
                   GPP= mean(GPP_PI_F, na.rm=TRUE),
                   NEE= mean(NEE_PI_F, na.rm=TRUE),
                   PPFD= mean(PPFD_IN_PI_F, na.rm=TRUE),
                   TA= mean(c(TA_PI_F_1_1_1, TA_PI_F_1_1_2), na.rm=TRUE),
                   P= sum(P, na.rm=TRUE),
                   D_SNOW= mean(D_SNOW, na.rm=TRUE),
                   TS= mean(c(TS_PI_F_1_1_1, TS_PI_F_2_1_1), na.rm=TRUE),
                   SWC= mean(SWC_PI_F_1, na.rm=TRUE))
#dataset contains both P and P_RAIN
###28####---------------------------------------------------------------------------
colnames(base[[28]])
base[[28]] <- base[[28]] %>% group_by(year, month) %>%
  dplyr::summarise(percent_na = (sum(is.na(NEE_PI_F))/n()*100),
                   NEE= mean(NEE_PI_F, na.rm=TRUE),
                   RECO= mean(RECO_PI_F, na.rm=TRUE),
                   GPP= mean(GPP_PI_F, na.rm=TRUE),
                   NEE= mean(NEE_PI_F, na.rm=TRUE),
                   PPFD= mean(PPFD_IN_PI_F_1_1_1, na.rm=TRUE),
                   TA= mean(c(TA_PI_F_1_1_1, TA_PI_F_1_1_2), na.rm=TRUE),
                   D_SNOW= mean(D_SNOW_PI_F, na.rm=TRUE),
                   TS= mean(c(TS_PI_F_1_1_1, TS_PI_F_2_1_1), na.rm=TRUE),
                   SWC= mean(SWC_PI_F_1, na.rm=TRUE))
###29####---------------------------------------------------------------------------
colnames(base[[29]])
base[[29]] <- base[[29]] %>% group_by(year, month) %>%
  dplyr::summarise(percent_na = (sum(is.na(NEE_PI_F))/n()*100),
                   SC= mean(SC, na.rm=TRUE),
                   TA= mean(c(TA_1_1_1, TA_2_1_1), na.rm=TRUE),
                   TS= mean(TS_1_1_1, na.rm=TRUE),
                   PPFD= mean(PPFD_IN, na.rm=TRUE),
                   FCH4= mean(FCH4_PI_F, na.rm=TRUE),
                   SCH4= mean(SCH4, na.rm=TRUE),
                   NEE= mean(NEE_PI_F, na.rm=TRUE),
                   GPP= mean(GPP_PI, na.rm=TRUE),
                   RECO= mean(RECO_PI, na.rm=TRUE))
###30####---------------------------------------------------------------------------
colnames(base[[30]])
base[[30]] <- base[[30]] %>% group_by(year, month) %>%
  dplyr::summarise(percent_na = (sum(is.na(NEE_PI_F))/n()*100),
                   SC= mean(SC, na.rm=TRUE),
                   TA= mean(TA_PI_F, na.rm=TRUE),
                   P= sum(P, na.rm=TRUE),
                   D_SNOW= mean(D_SNOW, na.rm=TRUE),
                   FCH4= mean(FCH4_PI_F, na.rm=TRUE),
                   SCH4= mean(SCH4, na.rm=TRUE),
                   NEE= mean(NEE_PI_F, na.rm=TRUE),
                   GPP= mean(GPP_PI, na.rm=TRUE),
                   RECO= mean(RECO_PI, na.rm=TRUE))
###31####---------------------------------------------------------------------------
colnames(base[[31]])
base[[31]] <- base[[31]] %>% group_by(year, month) %>%
  dplyr::summarise(percent_na = (sum(is.na(NEE_PI_F))/n()*100),
                   TA= mean(c(TA_1_1_1, TA_1_1_2), na.rm=TRUE),
                   PPFD= mean(PPFD_IN_1_1_1, na.rm=TRUE),
                   TS= mean(c(TS_1_1_1, TS_2_1_1), na.rm=TRUE),
                   P= sum(P_RAIN, na.rm=TRUE),
                   D_SNOW= mean(D_SNOW, na.rm=TRUE),
                   NEE= mean(NEE_PI_F, na.rm=TRUE),
                   GPP= mean(GPP_PI_F, na.rm=TRUE),
                   RECO= mean(RECO_PI_F, na.rm=TRUE),
                   SWC= mean(c(SWC_1_1_1, SWC_2_1_1), na.rm=TRUE))
###32####---------------------------------------------------------------------------
colnames(base[[32]])
base[[32]] <- base[[32]] %>% group_by(year, month) %>%
  dplyr::summarise(percent_na = (sum(is.na(NEE_PI))/n()*100),
                   NEE= mean(NEE_PI, na.rm=TRUE),
                   P= sum(P_RAIN, na.rm=TRUE),
                   TA= mean(TA, na.rm=TRUE),
                   PPFD= mean(PPFD_IN, na.rm=TRUE),
                   TS= mean(c(TS_1_1_1, TS_2_1_1), na.rm=TRUE),
                   D_SNOW= mean(D_SNOW, na.rm=TRUE))
###33####---------------------------------------------------------------------------
colnames(base[[33]])
base[[33]] <- base[[33]] %>% group_by(year, month) %>%
  dplyr::summarise(percent_na = (sum(is.na(NEE_PI))/n()*100),
                   NEE= mean(NEE_PI, na.rm=TRUE),
                   TA= mean(TA, na.rm=TRUE),
                   PPFD= mean(PPFD_IN, na.rm=TRUE),
                   TS= mean(c(TS_1_1_1, TS_2_1_1), na.rm=TRUE),
                   P= sum(P_RAIN, na.rm=TRUE),
                   D_SNOW= mean(D_SNOW, na.rm=TRUE))
###34####---------------------------------------------------------------------------
colnames(base[[34]])
base[[34]] <- base[[34]] %>% group_by(year, month) %>%
  dplyr::summarise(percent_na = (sum(is.na(NEE_PI))/n()*100),
                   NEE= mean(NEE_PI, na.rm=TRUE),
                   FCH4= mean(FCH4, na.rm=TRUE),
                   PPFD= mean(PPFD_IN, na.rm=TRUE),
                   TS= mean(TS_PI_1_1_A, na.rm=TRUE),
                   SWC= mean(SWC_PI_1_1_A, na.rm=TRUE))
###35####---------------------------------------------------------------------------
colnames(base[[35]])
base[[35]] <- base[[35]] %>% group_by(year, month) %>%
  dplyr::summarise(percent_na = (sum(is.na(NEE_PI))/n()*100),
                   NEE= mean(NEE_PI, na.rm=TRUE),
                   FCH4= mean(FCH4, na.rm=TRUE),
                   PPFD= mean(PPFD_IN, na.rm=TRUE),
                   TS= mean(TS_PI_1_1_A, na.rm=TRUE),
                   SWC= mean(SWC_PI_1_1_A, na.rm=TRUE))
###36####---------------------------------------------------------------------------
colnames(base[[36]])
#NO NEE, GPP, RECO, or CH4 so excluding this site
###37####---------------------------------------------------------------------------
colnames(base[[37]])
#NO NEE, GPP, RECO, or CH4 so excluding this site
###38####---------------------------------------------------------------------------
colnames(base[[38]])
#NO NEE, GPP, RECO, or CH4 so excluding this site
###39####---------------------------------------------------------------------------
colnames(base[[39]])
base[[39]] <- base[[39]] %>% group_by(year, month) %>%
  dplyr::summarise(percent_na = (sum(is.na(NEE_PI))/n()*100),
                   TA= mean(TA, na.rm=TRUE),
                   NEE= mean(NEE_PI, na.rm=TRUE),
                   SC= mean(SC, na.rm=TRUE),
                   TS= mean(TS_1, na.rm=TRUE),
                   P= sum(P, na.rm=TRUE),
                   SWC= mean(SWC_1, na.rm=TRUE),
                   PPFD= mean(PPFD_IN, na.rm=TRUE),
                   RECO= mean(RECO_PI, na.rm=TRUE))
base[[39]]$GPP <- base[[39]]$NEE-base[[39]]$RECO
###40####---------------------------------------------------------------------------
colnames(base[[40]])
base[[40]] <- base[[40]] %>% group_by(year, month) %>%
  dplyr::summarise(percent_na = (sum(is.na(NEE_PI))/n()*100),
                   TA= mean(TA, na.rm=TRUE),
                   NEE= mean(NEE_PI, na.rm=TRUE),
                   SC= mean(SC, na.rm=TRUE),
                   TS= mean(TS_1, na.rm=TRUE),
                   P= sum(P, na.rm=TRUE),
                   SWC= mean(SWC_1, na.rm=TRUE),
                   PPFD= mean(PPFD_IN, na.rm=TRUE),
                   RECO= mean(RECO_PI, na.rm=TRUE))
base[[40]]$GPP <- base[[40]]$NEE-base[[40]]$RECO
###41####---------------------------------------------------------------------------
colnames(base[[41]])
base[[41]] <- base[[41]] %>% group_by(year, month) %>%
  dplyr::summarise(percent_na = (sum(is.na(NEE_PI))/n()*100),
                   TA= mean(TA, na.rm=TRUE),
                   NEE= mean(NEE_PI, na.rm=TRUE),
                   SC= mean(SC, na.rm=TRUE),
                   TS= mean(TS_1, na.rm=TRUE),
                   P= sum(P, na.rm=TRUE),
                   SWC= mean(SWC_1, na.rm=TRUE),
                   PPFD= mean(PPFD_IN, na.rm=TRUE),
                   RECO= mean(RECO_PI, na.rm=TRUE))
base[[41]]$GPP <- base[[41]]$NEE-base[[41]]$RECO
###42####---------------------------------------------------------------------------
colnames(base[[42]])
base[[42]] <- base[[42]] %>% group_by(year, month) %>%
  dplyr::summarise(percent_na = (sum(is.na(NEE_PI))/n()*100),
                   TA= mean(TA, na.rm=TRUE),
                   NEE= mean(NEE_PI, na.rm=TRUE),
                   SC= mean(SC, na.rm=TRUE),
                   TS= mean(TS_1, na.rm=TRUE),
                   P= sum(P, na.rm=TRUE),
                   SWC= mean(SWC_1, na.rm=TRUE),
                   PPFD= mean(PPFD_IN, na.rm=TRUE),
                   RECO= mean(RECO_PI, na.rm=TRUE))
base[[42]]$GPP <- base[[42]]$NEE-base[[42]]$RECO
###43####---------------------------------------------------------------------------
colnames(base[[43]])
base[[43]] <- base[[43]] %>% group_by(year, month) %>%
  dplyr::summarise(percent_na = (sum(is.na(NEE_PI))/n()*100),
                   TA= mean(TA, na.rm=TRUE),
                   NEE= mean(NEE_PI, na.rm=TRUE),
                   SC= mean(SC, na.rm=TRUE),
                   TS= mean(TS_1, na.rm=TRUE),
                   P= sum(P, na.rm=TRUE),
                   SWC= mean(SWC_1, na.rm=TRUE),
                   PPFD= mean(PPFD_IN, na.rm=TRUE),
                   RECO= mean(RECO_PI, na.rm=TRUE))
base[[43]]$GPP <- base[[43]]$NEE-base[[43]]$RECO
###44####---------------------------------------------------------------------------
colnames(base[[44]])
base[[44]] <- base[[44]] %>% group_by(year, month) %>%
  dplyr::summarise(percent_na = (sum(is.na(GPP_PI))/n()*100),
                   TA= mean(TA_PI_F, na.rm=TRUE),
                   TS= mean(TS_PI_F_1, na.rm=TRUE),
                   P= sum(P_PI_F, na.rm=TRUE),
                   SWC= mean(SWC_PI_1, na.rm=TRUE),
                   PPFD= mean(PPFD_IN, na.rm=TRUE),
                   FCH4= mean(FCH4_PI_F, na.rm=TRUE),
                   D_SNOW= mean(D_SNOW_PI_F, na.rm=TRUE),
                   GPP= mean(GPP_PI, na.rm=TRUE),
                   RECO= mean(RECO_PI, na.rm=TRUE))
base[[44]]$NEE <- base[[44]]$RECO-base[[44]]$GPP
###45####---------------------------------------------------------------------------
colnames(base[[45]])
base[[45]] <- base[[45]] %>% group_by(year, month) %>%
  dplyr::summarise(percent_na = (sum(is.na(GPP_PI))/n()*100),
                   FCH4= mean(FCH4_PI_F, na.rm=TRUE),
                   TA= mean(TA_PI_F, na.rm=TRUE),
                   TS= mean(TS_PI_F_1, na.rm=TRUE),
                   P= sum(P_PI_F, na.rm=TRUE),
                   SWC= mean(SWC_PI_1, na.rm=TRUE),
                   PPFD= mean(PPFD_IN_PI_F, na.rm=TRUE),
                   D_SNOW= mean(D_SNOW, na.rm=TRUE),
                   GPP= mean(GPP_PI, na.rm=TRUE),
                   RECO= mean(RECO_PI, na.rm=TRUE))
base[[45]]$NEE <- base[[45]]$RECO-base[[45]]$GPP
###46####---------------------------------------------------------------------------
colnames(base[[46]])
base[[46]] <- base[[46]] %>% group_by(year, month) %>%
  dplyr::summarise(percent_na = (sum(is.na(GPP_PI))/n()*100),
                   FCH4= mean(FCH4_PI_F, na.rm=TRUE),
                   TA= mean(TA_PI_F, na.rm=TRUE),
                   TS= mean(TS_PI_F_1, na.rm=TRUE),
                   PPFD= mean(PPFD_IN, na.rm=TRUE),
                   P= sum(P, na.rm=TRUE),
                   SWC= mean(SWC_PI_1, na.rm=TRUE),
                   GPP= mean(GPP_PI, na.rm=TRUE),
                   RECO= mean(RECO_PI, na.rm=TRUE))
base[[46]]$NEE <- base[[46]]$RECO-base[[46]]$GPP
###47####---------------------------------------------------------------------------
colnames(base[[47]])
base[[47]] <- base[[47]] %>% group_by(year, month) %>%
  dplyr::summarise(percent_na = (sum(is.na(GPP_PI))/n()*100),
                   TA= mean(TA_PI_F, na.rm=TRUE),
                   TS= mean(TS_PI_F_1, na.rm=TRUE),
                   PPFD= mean(PPFD_IN_PI_F, na.rm=TRUE),
                   P= sum(P_PI_F, na.rm=TRUE),
                   D_SNOW= mean(D_SNOW_PI_F, na.rm=TRUE),
                   SWC= mean(SWC_PI_1, na.rm=TRUE),
                   GPP= mean(GPP_PI, na.rm=TRUE),
                   RECO= mean(RECO_PI, na.rm=TRUE),
                   FCH4= mean(FCH4_PI_F, na.rm=TRUE))
base[[47]]$NEE <- base[[47]]$RECO-base[[47]]$GPP
###48####---------------------------------------------------------------------------
colnames(base[[48]])
base[[48]] <- base[[48]] %>% group_by(year, month) %>%
  dplyr::summarise(percent_na = (sum(is.na(NEE_PI_F))/n()*100),
                   TA= mean(TA_PI_F, na.rm=TRUE),
                   SWC= mean(c(SWC_1_1_1, SWC_2_1_1), na.rm=TRUE),
                   TS= mean(c(TS, TS_1_1_1, TS_2_1_1), na.rm=TRUE),
                   PPFD= mean(PPFD_IN_PI_F, na.rm=TRUE),
                   D_SNOW= mean(D_SNOW, na.rm=TRUE),
                   SC= mean(SC, na.rm=TRUE),
                   NEE= mean(NEE_PI_F, na.rm=TRUE),
                   GPP= mean(GPP_PI_F, na.rm=TRUE),
                   RECO= mean(RECO_PI_F, na.rm=TRUE),
                   FCH4= mean(FCH4, na.rm=TRUE))
###49####---------------------------------------------------------------------------
colnames(base[[49]])
base[[49]] <- base[[49]] %>% group_by(year, month) %>%
  dplyr::summarise(percent_na = (sum(is.na(NEE_PI))/n()*100),
                   NEE= mean(NEE_PI, na.rm=TRUE),
                   SC= mean(SC, na.rm=TRUE),
                   GPP= mean(GPP_PI_F, na.rm=TRUE),
                   RECO= mean(RECO_PI_F, na.rm=TRUE),
                   TA= mean(TA_PI_F_1_1_1, na.rm=TRUE),
                   P= sum(P_RAIN, na.rm=TRUE),
                   PPFD= mean(PPFD_IN_PI_F, na.rm=TRUE),
                   SWC= mean(SWC_1_1_2, na.rm=TRUE),
                   TS= mean(TS_PI_F_1_1_1, na.rm=TRUE))
###50####---------------------------------------------------------------------------
colnames(base[[50]])
base[[50]] <- base[[50]] %>% group_by(year, month) %>%
  dplyr::summarise(percent_na = (sum(is.na(NEE_PI))/n()*100),
                   TA= mean(TA, na.rm=TRUE),
                   NEE= mean(NEE_PI, na.rm=TRUE),
                   SC= mean(SC, na.rm=TRUE),
                   TS= mean(c(TS_1, TS_2), na.rm=TRUE),
                   P= sum(P, na.rm=TRUE),
                   SWC= mean(SWC_1, na.rm=TRUE),
                   PPFD= mean(PPFD_IN, na.rm=TRUE),
                   RECO= mean(RECO_PI, na.rm=TRUE))
###51####---------------------------------------------------------------------------
colnames(base[[51]])
base[[51]] <- base[[51]] %>% group_by(year, month) %>%
  dplyr::summarise(percent_na = (sum(is.na(GPP_PI))/n()*100),
                   TA= mean(TA_PI_F, na.rm=TRUE),
                   TS= mean(c(TS_PI_1, TS_PI_2), na.rm=TRUE),
                   P= sum(P, na.rm=TRUE),
                   SWC= mean(c(SWC_PI_1, SWC_PI_2), na.rm=TRUE),
                   PPFD= mean(PPFD_IN, na.rm=TRUE),
                   D_SNOW= mean(D_SNOW, na.rm=TRUE),
                   RECO= mean(RECO_PI, na.rm=TRUE),
                   GPP= mean(GPP_PI, na.rm=TRUE),
                   FCH4= mean(FCH4_PI_F, na.rm=TRUE))
base[[51]]$NEE <- base[[51]]$RECO-base[[51]]$GPP
###52####---------------------------------------------------------------------------
colnames(base[[52]])
base[[52]] <- base[[52]] %>% group_by(year, month) %>%
  dplyr::summarise(percent_na = (sum(is.na(GPP_PI))/n()*100),
                   FCH4= mean(FCH4_PI_F, na.rm=TRUE),
                   TA= mean(TA_PI_F, na.rm=TRUE),
                   TS= mean(c(TS_PI_1, TS_PI_2), na.rm=TRUE),
                   PPFD= mean(PPFD_IN, na.rm=TRUE),
                   P= sum(P, na.rm=TRUE),
                   D_SNOW= mean(D_SNOW, na.rm=TRUE),
                   SWC= mean(c(SWC_PI_1, SWC_PI_2), na.rm=TRUE),
                   RECO= mean(RECO_PI, na.rm=TRUE),
                   GPP= mean(GPP_PI, na.rm=TRUE))
base[[52]]$NEE <- base[[52]]$RECO-base[[52]]$GPP
###53####---------------------------------------------------------------------------
colnames(base[[53]])
base[[53]] <- base[[53]] %>% mutate(GPP_PI= as.numeric(GPP_PI)) 
base[[53]] <- base[[53]] %>% mutate(FCH4_PI_F= as.numeric(FCH4_PI_F))
base[[53]] <- base[[53]] %>% group_by(year, month) %>%
  dplyr::summarise(percent_na = (sum(is.na(GPP_PI))/n()*100),
                   TA= mean(TA_PI_F, na.rm=TRUE),
                   TS= mean(c(TS_1_1_1, TS_2_1_1), na.rm=TRUE),
                   PPFD= mean(PPFD_IN, na.rm=TRUE),
                   P= sum(P, na.rm=TRUE),
                   SWC= mean(c(SWC_1_1_1, SWC_2_1_1), na.rm=TRUE),
                   TS= mean(c(TS_1_1_1, TS_2_1_1), na.rm=TRUE),
                   RECO= mean(RECO_PI, na.rm=TRUE),
                   GPP= mean(GPP_PI, na.rm=TRUE),
                   FCH4= mean(FCH4_PI_F, na.rm=TRUE))
base[[53]]$NEE <- base[[53]]$RECO-base[[53]]$GPP
###54####---------------------------------------------------------------------------
colnames(base[[54]])
base[[54]] <- base[[54]] %>% group_by(year, month) %>%
  dplyr::summarise(percent_na = (sum(is.na(NEE_PI))/n()*100),
                   TA= mean(TA, na.rm=TRUE),
                   NEE= mean(NEE_PI, na.rm=TRUE),
                   SC= mean(SC, na.rm=TRUE),
                   TS= mean(c(TS_PI_1, TS_PI_2), na.rm=TRUE),
                   P= sum(P, na.rm=TRUE),
                   PPFD= mean(PPFD_IN, na.rm=TRUE),
                   FCH4= mean(FCH4, na.rm=TRUE),
                   SWC= mean(c(SWC_2_1_1, SWC_3_1_1), na.rm=TRUE))
###55####---------------------------------------------------------------------------
colnames(base[[55]])
base[[55]] <- base[[55]] %>% group_by(year, month) %>%
  dplyr::summarise(percent_na = (sum(is.na(FCH4))/n()*100),
                   FCH4= mean(FCH4, na.rm=TRUE),
                   PPFD= mean(PPFD_IN_1_1_1, na.rm=TRUE),
                   P= sum(P_RAIN_1_1_1, na.rm=TRUE),
                   SWC= mean(c(SWC_1_1_1, SWC_2_1_1, SWC_3_1_1), na.rm=TRUE),
                   TA= mean(TA_1_1_1, na.rm=TRUE),
                   TS= mean(c(TS_1_1_1, TS_2_1_1, TS_3_1_1), na.rm=TRUE))
###56####---------------------------------------------------------------------------
colnames(base[[56]])
base[[56]] <- base[[56]] %>% group_by(year, month) %>%
  dplyr::summarise(percent_na = (sum(is.na(FCH4))/n()*100),
                   FCH4= mean(FCH4, na.rm=TRUE),
                   PPFD= mean(PPFD_IN, na.rm=TRUE))
###57####---------------------------------------------------------------------------
colnames(base[[57]])
base[[57]] <- base[[57]] %>% group_by(year, month) %>%
  dplyr::summarise(percent_na = (sum(is.na(FCH4))/n()*100),
                   FCH4= mean(FCH4, na.rm=TRUE),
                   PPFD= mean(PPFD_IN, na.rm=TRUE),
                   TS= mean(c(TS_1_1_1, TS_2_1_1, TS_3_1_1, TS_4_1_1, TS_5_1_1), na.rm=TRUE),
                   SWC= mean(c(SWC_1_1_1, SWC_2_1_1, SWC_3_1_1), na.rm=TRUE))
###58####---------------------------------------------------------------------------
colnames(base[[58]])
base[[58]] <- base[[58]] %>% group_by(year, month) %>%
  dplyr::summarise(percent_na = (sum(is.na(NEE_PI))/n()*100),
                   NEE= mean(NEE_PI, na.rm=TRUE),
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
###59####---------------------------------------------------------------------------
colnames(base[[59]])
base[[59]] <- base[[59]] %>% group_by(year, month) %>%
  dplyr::summarise(percent_na = (sum(is.na(NEE_PI_F))/n()*100),
                   TA= mean(TA_PI_F_1_1_1, na.rm=TRUE),
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
###60####---------------------------------------------------------------------------
colnames(base[[60]])
#NO NEE, GPP, RECO, or CH4 so excluding this site
###61####---------------------------------------------------------------------------
colnames(base[[61]])
base[[61]] <- base[[61]] %>% group_by(year, month) %>%
  dplyr::summarise(percent_na = (sum(is.na(NEE_PI))/n()*100),
                   SC= mean(SC, na.rm=TRUE),
                   NEE= mean(NEE_PI, na.rm=TRUE),
                   TA= mean(c(TA_1_1_1, TA_1_1_2), na.rm=TRUE),
                   PPFD= mean(PPFD_IN_1_1_1, na.rm=TRUE),
                   SWC= mean(c(SWC_1_1_1, SWC_2_1_1,
                               SWC_3_1_1, SWC_4_1_1, SWC_5_1_1), na.rm=TRUE),
                   TS= mean(c(TS_1_1_1, TS_2_1_1,
                              TS_3_1_1, TS_4_1_1, TS_5_1_1), na.rm=TRUE),
                   P= sum(P, na.rm=TRUE))
###62####---------------------------------------------------------------------------
colnames(base[[62]])
base[[62]] <- base[[62]] %>% group_by(year, month) %>%
  dplyr::summarise(percent_na = (sum(is.na(NEE_PI))/n()*100),
                   SC= mean(SC, na.rm=TRUE),
                   NEE= mean(NEE_PI, na.rm=TRUE),
                   TA= mean(c(TA_1_1_1, TA_1_1_2), na.rm=TRUE),
                   PPFD= mean(PPFD_IN_1_1_1, na.rm=TRUE),
                   SWC= mean(c(SWC_1_1_1, SWC_2_1_1,
                               SWC_3_1_1, SWC_4_1_1, SWC_5_1_1), na.rm=TRUE),
                   TS= mean(c(TS_1_1_1, TS_2_1_1,
                              TS_3_1_1, TS_4_1_1, TS_5_1_1), na.rm=TRUE),
                   P= sum(P, na.rm=TRUE))
###63####---------------------------------------------------------------------------
colnames(base[[63]])
base[[63]] <- base[[63]] %>% group_by(year, month) %>%
  dplyr::summarise(percent_na = (sum(is.na(NEE_PI))/n()*100),
                   SC= mean(SC, na.rm=TRUE),
                   NEE= mean(NEE_PI, na.rm=TRUE),
                   TA= mean(c(TA_1_1_1, TA_1_1_2), na.rm=TRUE),
                   PPFD= mean(PPFD_IN_1_1_1, na.rm=TRUE),
                   SWC= mean(c(SWC_1_1_1, SWC_2_1_1,
                               SWC_3_1_1, SWC_4_1_1, SWC_5_1_1), na.rm=TRUE),
                   TS= mean(c(TS_1_1_1, TS_2_1_1,
                              TS_3_1_1, TS_4_1_1, TS_5_1_1), na.rm=TRUE),
                   P= sum(P, na.rm=TRUE))
###64####---------------------------------------------------------------------------
colnames(base[[64]])
base[[64]] <- base[[64]] %>% group_by(year, month) %>%
  dplyr::summarise(percent_na = (sum(is.na(NEE_PI))/n()*100),
                   SC= mean(SC, na.rm=TRUE),
                   NEE= mean(NEE_PI, na.rm=TRUE),
                   TA= mean(c(TA_1_1_1, TA_1_1_2), na.rm=TRUE),
                   PPFD= mean(PPFD_IN_1_1_1, na.rm=TRUE),
                   SWC= mean(c(SWC_1_1_1, SWC_2_1_1,
                               SWC_3_1_1, SWC_4_1_1, SWC_5_1_1), na.rm=TRUE),
                   TS= mean(c(TS_1_1_1, TS_2_1_1,
                              TS_3_1_1, TS_4_1_1, TS_5_1_1), na.rm=TRUE),
                   P= sum(P, na.rm=TRUE))
###65####---------------------------------------------------------------------------
colnames(base[[65]])
base[[65]] <- base[[65]] %>% group_by(year, month) %>%
  dplyr::summarise(percent_na = (sum(is.na(NEE_PI))/n()*100),
                   SC= mean(SC, na.rm=TRUE),
                   NEE= mean(NEE_PI, na.rm=TRUE),
                   TA= mean(c(TA_1_1_1, TA_1_1_2), na.rm=TRUE),
                   PPFD= mean(PPFD_IN_1_1_1, na.rm=TRUE),
                   SWC= mean(c(SWC_1_1_1, SWC_2_1_1,
                               SWC_3_1_1, SWC_4_1_1, SWC_5_1_1), na.rm=TRUE),
                   TS= mean(c(TS_1_1_1, TS_2_1_1,
                              TS_3_1_1, TS_4_1_1, TS_5_1_1), na.rm=TRUE),
                   P= sum(P, na.rm=TRUE))

####Consolidate data###_--------------------------------------------------------------
names(base)<- substr(files, 5,10) #name each df
#remove dfs that do not have our variables of interest
base2 <- base[-c(1,2,3,8,9,10,11,12,13,14,26,36,37,38,60)]
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
#establishing a threshold for % NAs allowed
base.monthly3 <- base.monthly %>% filter(percent_na < 30)


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
meta.bysite<- meta.bysite %>% rename(site_id= SITE_ID)
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
