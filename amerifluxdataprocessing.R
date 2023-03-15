###Ameriflux data processing####
###Ameriflux data comes in two formats "FLUXNET" and "BASE"
#we have to load these formats separately
library(plyr)
library(dplyr)
library(purrr)
library(readr)
library(ggplot2)
library(vroom)
library(data.table)
#Downloaded data from https://ameriflux.lbl.gov/data/download-data/
# sites selected based on those present in "tower sheet major" on the ABCflux google drive
#load in all FLUXNET files ####
setwd("/Users/iwargowsky/Desktop/Ameriflux/AMF-FLUXNET")
path <- "/Users/iwargowsky/Desktop/Ameriflux/AMF-FLUXNET"
files <- list.files(path = path,pattern = '*FULLSET_DD_',all.files = T,recursive = T)
amerifluxdf <- files %>%
  setNames(nm = .) %>% 
  map_df(~read_csv(.x, col_types = cols(), col_names = TRUE, na=c("NA","-9999")), .id = "site_id")         
#clean up site id column
amerifluxdf$site_id <- substr(amerifluxdf$site_id, 5,10)
#select columns to keep
colnames(amerifluxdf) #see all column names
amerifluxdf <- amerifluxdf %>% dplyr::select(site_id, TIMESTAMP, TS_F_MDS_1, TS_F_MDS_1_QC,
                                         SWC_F_MDS_1, SWC_F_MDS_1_QC, 
                                         TA_F, TA_F_QC, 
                                         P_F,P_F_QC,
                                         NEE_VUT_REF,NEE_VUT_REF_QC,
                                         RECO_DT_VUT_REF, GPP_DT_VUT_REF,
                                         RECO_NT_VUT_REF, GPP_NT_VUT_REF)
#add month and year columns
amerifluxdf$year <- substr(amerifluxdf$TIMESTAMP,1,4)
amerifluxdf$month <- substr(amerifluxdf$TIMESTAMP,5,6)
#get cumulative NEE, GPP, and RECO for each month
ameriflux.permonth<-  group_by(amerifluxdf, year, month, site_id) %>% dplyr::summarise(TS_F_MDS_1 = mean(TS_F_MDS_1),
                                                                                   TS_F_MDS_1_QC = mean(TS_F_MDS_1_QC),
                                                                                   SWC_F_MDS_1 = mean(SWC_F_MDS_1),
                                                                                   SWC_F_MDS_1_QC = mean(SWC_F_MDS_1_QC),
                                                                                   TA_F = mean(TA_F),
                                                                                   TA_F_QC = mean(TA_F_QC),
                                                                                   P_F =sum(P_F),
                                                                                   P_F_QC = mean(P_F_QC),
                                                                                   NEE_VUT_REF = sum(NEE_VUT_REF),
                                                                                   NEE_VUT_REF_QC = mean(NEE_VUT_REF_QC),
                                                                                   RECO_DT_VUT_REF = sum(RECO_DT_VUT_REF),
                                                                                   GPP_DT_VUT_REF = sum(GPP_DT_VUT_REF),
                                                                                   RECO_NT_VUT_REF = sum(RECO_NT_VUT_REF),
                                                                                   GPP_NT_VUT_REF = sum(GPP_NT_VUT_REF))

###load in all ameriflux BASE files #### 
#These ameriflux files all have a bunch of different column names and 
#are at half-hourly resolution so there's LOTS of data to get through
#We'll first cycle through the folder and get daily averages for each file in the folder
newFolder <- "/Users/iwargowsky/Desktop/Ameriflux/AMF-BASE_DD"
dir.create(newFolder)
path <- "/Users/iwargowsky/Desktop/Ameriflux/AMF-BASE"
files <- list.files(path = path,pattern = '*_HH_',all.files = T,recursive = T)
setwd("/Users/iwargowsky/Desktop/Ameriflux/AMF-BASE")
for(f in files) {
  temp <- fread(f, na=c("NA", "-9999"))
  temp$year <- substr(temp$TIMESTAMP_START, 1,4)
  temp$month <- substr(temp$TIMESTAMP_START, 5,6)
  temp$day <- substr(temp$TIMESTAMP_START, 7,8)
  #aggregate to half-hourly to daily
  temp <- group_by(temp, year, month, day) %>% summarise(across(everything(), mean, na.rm = TRUE))
  
  fwrite(temp, file.path(newFolder, paste0('DD_', basename(f))))
}



setwd("/Users/iwargowsky/Desktop/Ameriflux/AMF-BASE_DD")
path <- "/Users/iwargowsky/Desktop/Ameriflux/AMF-BASE_DD"
files <- list.files(path = path,pattern = '*_HH_',all.files = T,recursive = T)
base.amerifluxdf <- files %>%
  setNames(nm = .) %>% 
  map_df(~read_csv(.x, col_types = cols(), col_names = TRUE, na= c("NA","-9999")), 
         .id = "site_id")     
#clean up site id column
base.amerifluxdf$site_id <- substr(base.amerifluxdf$site_id, 8,13)
#remove TIMESTAMP columns as they are means and we have month, year, day columns now
base.amerifluxdf <- base.amerifluxdf %>% select(-c(TIMESTAMP_START,TIMESTAMP_END))
#convert units umol m-2 s-1 to g C m-2 d-1
base.amerifluxdf$FC <- base.amerifluxdf$FC*1.0368
#get cumulative NEE, GPP, and RECO for each month
basedf.permonth<- group_by(base.amerifluxdf, year, month, site_id) %>% dplyr::summarise(VPD = mean(VPD_PI), 
                                                                                   TS_1_1_1 = mean(TS_1_1_1),
                                                                                   SWC_1_1_1 = mean(SWC_1_1_1),
                                                                                   D_SNOW = sum(D_SNOW),
                                                                                   FC = sum(FC))


###############################
#Examine fluxnet processing vs PI data
unique(basedf.perday$site_id)
unique(ameriflux.permonth$site_id)

FLUXNET <- ameriflux.permonth %>% filter(site_id == "CA-ARF")
FLUXNET$ts <- paste(FLUXNET$year, FLUXNET$month)
BASE <- basedf.permonth %>% filter(site_id == "CA-ARF")
BASE$ts <- paste(BASE$year, BASE$month)

ggplot()+geom_line(data= FLUXNET, aes(x= ts, y= NEE_VUT_REF, group=site_id, color="blue"))+
ggplot()+geom_line(data = BASE, aes(x=ts, y=FC, color="red"))




reg <- read_csv(skip=2,"/Users/iwargowsky/Desktop/Ameriflux/AMF-BASE/AMF_CA-ARB_BASE-BADM_1-5/AMF_CA-ARB_BASE_HH_1-5.csv",  na=c("NA", "-9999"))
mean <- read_csv("/Users/iwargowsky/Desktop/TEST/new_AMF_CA-ARB_BASE_HH_1-5.csv",  na=c("NA", "-9999"))
reg$year <- substr(reg$TIMESTAMP_START,1,4)
reg$month <- substr(reg$TIMESTAMP_START,5,6)
reg$day <- substr(reg$TIMESTAMP_START,7,8)
regdaily <- group_by(reg, year, month, day) %>% dplyr::summarise(FC= mean(FC))


