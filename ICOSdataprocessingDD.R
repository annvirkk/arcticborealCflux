#ICOS data processing starting with daily# 
###Combining ICOS data ####
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
wwlist_of_files <- list.files(path = path,pattern = '*_FULLSET_DD_',all.files = T,recursive = T)
#cycle through folders
wwicosdat <- wwlist_of_files %>%
  setNames(nm = .) %>% 
  map_df(~read_csv(.x, col_types = cols(), col_names = TRUE), .id = "site_id")          
wwcol<-colnames(wwicosdat)
#subset for only our variables of interest
wwdat <- wwicosdat %>% dplyr::select(site_id, TIMESTAMP, TS_F_MDS_1, TS_F_MDS_1_QC,
                                     SWC_F_MDS_1, SWC_F_MDS_1_QC, 
                                     TA_F, TA_F_QC, 
                                     P_F,P_F_QC,
                                     NEE_VUT_REF,NEE_VUT_REF_QC,
                                     RECO_DT_VUT_REF, GPP_DT_VUT_REF,
                                     RECO_NT_VUT_REF, GPP_NT_VUT_REF)
#turn -9999 to NA
wwdat[wwdat == -9999] <- NA
#clean up site id column
wwdat$site_id <- substr(wwdat$site_id, 5,10)
#add year,month, day columns
wwdat$year <- substr(wwdat$TIMESTAMP, 1,4)
wwdat$month <- substr(wwdat$TIMESTAMP, 5,6)
colnames(wwdat)
#get cumulative NEE, GPP, and RECO for each month
wwdat.permonth<- group_by(wwdat, year, month, site_id) %>% dplyr::summarise(TS_F_MDS_1 = mean(TS_F_MDS_1),
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

################################
###ICOS archive data####
# Identify file names
setwd("/Users/iwargowsky/Desktop/ICOS/ICOSETC")
path <- "/Users/iwargowsky/Desktop/ICOS/ICOSETC"
icoslist_of_files <- list.files(path = path,pattern = '*_FLUXNET_DD_',all.files = T,recursive = T) 
#NOTE had to remove csvs that had "VARINFO" and "product_description" from original ICOS download folders for this to work
#cycle through all folders
allicosdat <- icoslist_of_files %>%
  setNames(nm = .) %>% 
  map_df(~read_csv(.x, col_types = cols(), col_names = TRUE), .id = "site_id")          
icoscol <-colnames(allicosdat) #This dataset also partitions NEE by day and night if we want to include that
#subset for only our variables of interest
icosdat <- allicosdat %>% dplyr::select(site_id, TIMESTAMP, TS_F_MDS_1, TS_F_MDS_1_QC,
                                        SWC_F_MDS_1, SWC_F_MDS_1_QC, 
                                        TA_F, TA_F_QC, 
                                        P_F,P_F_QC,
                                        NEE_VUT_REF,NEE_VUT_REF_QC,
                                        RECO_DT_VUT_REF, GPP_DT_VUT_REF,
                                        RECO_NT_VUT_REF, GPP_NT_VUT_REF)
#turn -9999 to NA
icosdat[icosdat == -9999] <- NA
#clean up site id column
icosdat$site_id <- substr(icosdat$site_id, 9,14)
#add year,month, day columns
icosdat$year <- substr(icosdat$TIMESTAMP, 1,4)
icosdat$month <- substr(icosdat$TIMESTAMP, 5,6)
colnames(icosdat)
#get cumulative NEE, GPP, and RECO for each month
icosdat.permonth<- group_by(icosdat, year, month, site_id) %>% dplyr::summarise(TS_F_MDS_1 = mean(TS_F_MDS_1),
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