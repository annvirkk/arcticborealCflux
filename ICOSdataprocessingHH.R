###Combining ICOS data #### starting with half-hourly
library(dplyr)
library(ggplot2)
library(gdata)
library(janitor)
library(tidyverse)
library(stringr)
library(naniar)
#start with Warm Winters data
# Identify file names
setwd("/Users/iwargowsky/Desktop/ICOS/Warm Winters")
path <- "/Users/iwargowsky/Desktop/ICOS/Warm Winters"
wwlist_of_files <- list.files(path = path,pattern = '*_FULLSET_HH_',all.files = T,recursive = T)
#cycle through folders
wwicosdat <- wwlist_of_files %>%
  setNames(nm = .) %>% 
  map_df(~read_csv(.x, col_types = cols(), col_names = TRUE, na=c("NA","-9999")), .id = "site_id")          
wwcol<-colnames(wwicosdat)
#subset for only our variables of interest
wwdat <- wwicosdat %>% dplyr::select(site_id, TIMESTAMP_START, TA_F_MDS, TS_F_MDS_1, SWC_F_MDS_1, NEE_VUT_REF, 
                                        RECO_DT_VUT_REF, GPP_DT_VUT_REF, NEE_VUT_REF_QC)
#clean up site id column
wwdat$site_id <- substr(wwdat$site_id, 5,10)
#add year,month, day columns
wwdat$year <- substr(wwdat$TIMESTAMP_START, 1,4)
wwdat$month <- substr(wwdat$TIMESTAMP_START, 5,6)
wwdat$day <- substr(wwdat$TIMESTAMP_START, 7,8)
colnames(wwdat)
#aggregate to half-hourly to daily
wwdat$TIMESTAMP_START<- NULL
wwdat.perday<- group_by(wwdat, year, day, month, site_id) %>% dplyr::summarise(TA_F_MDS = mean(TA_F_MDS), 
                                                                        TS_F_MDS_1 = mean(TS_F_MDS_1),
                                                                        SWC_F_MDS_1 = mean(SWC_F_MDS_1),
                                                                        NEE_VUT_REF = mean(NEE_VUT_REF),
                                                                        NEE_VUT_REF_QC = mean(NEE_VUT_REF_QC),
                                                                        RECO_DT_VUT_REF = mean(RECO_DT_VUT_REF),
                                                                        GPP_DT_VUT_REF = mean(GPP_DT_VUT_REF))
unique(wwdat.perday$site_id) 
#convert units umol m-2 s-1 to g C m-2 d-1
wwdat.perday$NEE_VUT_REF <- wwdat.perday$NEE_VUT_REF*1.0368
wwdat.perday$RECO_DT_VUT_REF <- wwdat.perday$RECO_DT_VUT_REF*1.0368
wwdat.perday$GPP_DT_VUT_REF <- wwdat.perday$GPP_DT_VUT_REF*1.0368
#get cumulative NEE, GPP, and RECO for each month
wwdat.permonth<- group_by(wwdat.perday, year, month, site_id) %>% dplyr::summarise(TA_F_MDS = mean(TA_F_MDS), 
                                                                        TS_F_MDS_1 = mean(TS_F_MDS_1),
                                                                        SWC_F_MDS_1 = mean(SWC_F_MDS_1),
                                                                        NEE_VUT_REF = sum(NEE_VUT_REF),
                                                                        NEE_VUT_REF_QC = mean(NEE_VUT_REF_QC),
                                                                        RECO_DT_VUT_REF = sum(RECO_DT_VUT_REF),
                                                                        GPP_DT_VUT_REF = sum(GPP_DT_VUT_REF))

#quick plot by site
ggplot(wwdat.permonth, aes(x=site_id,y=NEE_VUT_REF))+geom_boxplot()
#Examine how QC changes over annual cycle
wwqualitycheck <- group_by(wwdat.permonth, month, year, site_id) %>% summarise(NEE_VUT_REF_QC = mean(NEE_VUT_REF_QC))
wwqualitycheck$timestamp <- paste(wwqualitycheck$year, wwqualitycheck$month)
ggplot(wwqualitycheck, mapping= aes(x=timestamp, y= NEE_VUT_REF_QC, group=site_id))+
  geom_line(mapping= aes(x=timestamp, y= NEE_VUT_REF_QC, color=site_id))

################################
###ICOS archive data####
# Identify file names
setwd("/Users/iwargowsky/Desktop/ICOS/ICOSETC")
path <- "/Users/iwargowsky/Desktop/ICOS/ICOSETC"
icoslist_of_files <- list.files(path = path,pattern = '*_FLUXNET_HH_',all.files = T,recursive = T) 
#NOTE had to remove csvs that had "VARINFO" and "product_description" from original ICOS download folders for this to work
#cycle through all folders
allicosdat <- icoslist_of_files %>%
  setNames(nm = .) %>% 
  map_df(~read_csv(.x, col_types = cols(), col_names = TRUE,na=c("NA","-9999")), .id = "site_id")          
icoscol <-colnames(allicosdat)
#subset for only our variables of interest
icosdat <- allicosdat %>% dplyr::select(site_id, TIMESTAMP_START, TA_F_MDS, TS_F_MDS_1, SWC_F_MDS_1, NEE_VUT_REF, 
                                 RECO_DT_VUT_REF, GPP_DT_VUT_REF, NEE_VUT_REF_QC)
#clean up site id column
icosdat$site_id <- substr(icosdat$site_id, 9,14)
#add year,month, day columns
icosdat$year <- substr(icosdat$TIMESTAMP_START, 1,4)
icosdat$month <- substr(icosdat$TIMESTAMP_START, 5,6)
icosdat$day <- substr(icosdat$TIMESTAMP_START, 7,8)
colnames(icosdat)
#aggregate to half-hourly to daily
icosdat$TIMESTAMP_START<- NULL
icosdat.perday<- group_by(icosdat, year, day, month, site_id) %>% dplyr::summarise(TA_F_MDS = mean(TA_F_MDS), 
                                                                            TS_F_MDS_1 = mean(TS_F_MDS_1),
                                                                            SWC_F_MDS_1 = mean(SWC_F_MDS_1),
                                                                            NEE_VUT_REF = mean(NEE_VUT_REF),
                                                                            NEE_VUT_REF_QC = mean(NEE_VUT_REF_QC),
                                                                            RECO_DT_VUT_REF = mean(RECO_DT_VUT_REF),
                                                                            GPP_DT_VUT_REF = mean(GPP_DT_VUT_REF))
unique(icosdat.perday$site_id)
#convert units umol m-2 s-1 to g C m-2 d-1
icosdat.perday$NEE_VUT_REF <- icosdat.perday$NEE_VUT_REF*1.0368
icosdat.perday$RECO_DT_VUT_REF <- icosdat.perday$RECO_DT_VUT_REF*1.0368
icosdat.perday$GPP_DT_VUT_REF <- icosdat.perday$GPP_DT_VUT_REF*1.0368
#get cumulative NEE, GPP, and RECO for each month
icosdat.permonth<- group_by(icosdat.perday, year, month, site_id) %>% dplyr::summarise(TA_F_MDS = mean(TA_F_MDS), 
                                                                                TS_F_MDS_1 = mean(TS_F_MDS_1),
                                                                                SWC_F_MDS_1 = mean(SWC_F_MDS_1),
                                                                                NEE_VUT_REF = sum(NEE_VUT_REF),
                                                                                NEE_VUT_REF_QC = mean(NEE_VUT_REF_QC),
                                                                                RECO_DT_VUT_REF = sum(RECO_DT_VUT_REF),
                                                                                GPP_DT_VUT_REF = sum(GPP_DT_VUT_REF))
unique(icosdat.permonth$site_id)
#quick plot of NEE_VUT_REF by site
ggplot(icosdat.permonth, aes(x=site_id,y=NEE_VUT_REF))+geom_boxplot()

#Examine how QC changes over annual cycle
icosqualitycheck <- group_by(icosdat.permonth, month, year, site_id) %>% summarise(NEE_VUT_REF_QC = mean(NEE_VUT_REF_QC))
icosqualitycheck$timestamp <- paste(icosqualitycheck$year, icosqualitycheck$month)
ggplot(icosqualitycheck, mapping= aes(x=timestamp, y= NEE_VUT_REF_QC, group=site_id))+
  geom_line(mapping= aes(x=timestamp, y= NEE_VUT_REF_QC, color=site_id))


#########Merging warm winters and icosdat #######
#combine all data
alldat.wdupes <- gdata::combine(wwdat.permonth, icosdat.permonth)
#find duplicates
dupes<- alldat.wdupes %>% get_dupes(site_id, year, month)  
#separate dupes by source
wwdupes <- dupes %>% filter(source=="wwdat.permonth")
icosdupes <- dupes %>% filter(source=="icosdat.permonth")
#remove duplicates from icosdat to get final df
alldatpermonth <- anti_join(alldat.wdupes, icosdupes, 
                            by = c("site_id","month","year", "source")) #final df
setwd("/Users/iwargowsky/Desktop/ICOS")
write_csv(alldatpermonth, "ICOSdatapermonth.csv")



#examine duplicate differences
wwdupes$timestamp <- paste(wwdupes$year, wwdupes$month)
icosdupes$timestamp <- paste(icosdupes$year, icosdupes$month)
ggplot()+geom_line(wwdupes, mapping= aes(x=timestamp, y=NEE_VUT_REF, group=site_id, color=site_id))+
  geom_line(icosdupes, mapping= aes(x=timestamp, y=NEE_VUT_REF, group=site_id, color=site_id),linetype="dashed")
#look at individual sites
Nor <- dupes %>% filter(site_id == "SE-Nor")
Nor$timestamp <- paste(Nor$year, Nor$month)
ggplot()+geom_line(Nor, mapping= aes(x=timestamp, y=NEE_VUT_REF, group=source, color=source))


####extract list of sites and dates covered###
alldatpermonth$ts <- paste(alldatpermonth$year, alldatpermonth$month)
sites <- subset(alldatpermonth, select = c(site_id,ts))
sites.datescovered <- sites %>% group_by(site_id) %>% dplyr::summarise(start_date = min(ts),
                                                                       end_date = max(ts))
#double checking that function above worked
checkdates <- sites %>% arrange(site_id, ts)