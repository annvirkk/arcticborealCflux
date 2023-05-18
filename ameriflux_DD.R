###Ameriflux data processing####
###Ameriflux data comes in two formats "FLUXNET" and "BASE"
#we have to load these formats separately
library(plyr)
library(dplyr)
library(purrr)
library(readr)
library(ggplot2)
library(vroom)
library(readxl)
library(data.table)
library(tidyr)
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
amerifluxdf <- amerifluxdf %>% dplyr::select(site_id, TIMESTAMP, TS_F_MDS_1,
                                             SWC_F_MDS_1, TA_F, P_F, PPFD_IN,
                                             NEE_CUT_REF, NEE_CUT_REF_QC,
                                             RECO_DT_CUT_REF, GPP_DT_CUT_REF,
                                             RECO_NT_CUT_REF, GPP_NT_CUT_REF)
#add month and year columns
amerifluxdf$year <- substr(amerifluxdf$TIMESTAMP,1,4)
amerifluxdf$month <- substr(amerifluxdf$TIMESTAMP,5,6)
#get cumulative NEE, GPP, and RECO for each month
ameriflux.permonth<-  group_by(amerifluxdf, year, month, site_id) %>% dplyr::summarise(TS_F_MDS_1 = mean(TS_F_MDS_1),
                                                                                       SWC_F_MDS_1 = mean(SWC_F_MDS_1),
                                                                                       TA_F = mean(TA_F),
                                                                                       P_F =sum(P_F),
                                                                                       PPFD_IN = mean(PPFD_IN),
                                                                                       NEE_CUT_REF = sum(NEE_CUT_REF),
                                                                                       NEE_CUT_REF_QC= mean(NEE_CUT_REF_QC),
                                                                                       RECO_DT_CUT_REF = sum(RECO_DT_CUT_REF),
                                                                                       GPP_DT_CUT_REF = sum(GPP_DT_CUT_REF),
                                                                                       RECO_NT_CUT_REF = sum(RECO_NT_CUT_REF),
                                                                                       GPP_NT_CUT_REF = sum(GPP_NT_CUT_REF))
#separate DT and NT approaches
ameriflux.permonthDT <- ameriflux.permonth %>% select(-c(GPP_NT_CUT_REF, RECO_NT_CUT_REF))
ameriflux.permonthDT$partition_method <- "DT"
ameriflux.permonthDT <- ameriflux.permonthDT %>% rename("GPP_CUT_REF"= "GPP_DT_CUT_REF",
                                                    "RECO_CUT_REF"= "RECO_DT_CUT_REF")
ameriflux.permonthNT <- ameriflux.permonth %>% select(-c(GPP_DT_CUT_REF, RECO_DT_CUT_REF))
ameriflux.permonthNT$partition_method <- "NT"
ameriflux.permonthNT <- ameriflux.permonthNT %>% rename("GPP_CUT_REF"= "GPP_NT_CUT_REF",
                                                    "RECO_CUT_REF"= "RECO_NT_CUT_REF")
#merge back together with new column "partition method"
ameriflux.permonth <- bind_rows(ameriflux.permonthNT, ameriflux.permonthDT) 

#load in all FLUXNETbeta files ####
setwd("/Users/iwargowsky/Desktop/Ameriflux/AMF-FLUXNETbeta")
path <- "/Users/iwargowsky/Desktop/Ameriflux/AMF-FLUXNETbeta"
betafiles <- list.files(path = path,pattern = '*FULLSET_DD_',all.files = T,recursive = T)
betaamerifluxdf <- betafiles %>%
  setNames(nm = .) %>% 
  map_df(~read_csv(.x, col_types = cols(), col_names = TRUE, na=c("NA","-9999")), .id = "site_id")         
#clean up site id column
betaamerifluxdf$site_id <- substr(betaamerifluxdf$site_id, 5,10)
#select columns to keep
colnames(betaamerifluxdf) #see all column names
betaamerifluxdf <- betaamerifluxdf %>% dplyr::select(site_id, TIMESTAMP, TS_F_MDS_1,
                                             SWC_F_MDS_1, TA_F, P_F, PPFD_IN,
                                             NEE_CUT_REF,NEE_CUT_REF_QC,
                                             RECO_DT_CUT_REF, GPP_DT_CUT_REF,
                                             RECO_NT_CUT_REF, GPP_NT_CUT_REF)
#add month and year columns
betaamerifluxdf$year <- substr(betaamerifluxdf$TIMESTAMP,1,4)
betaamerifluxdf$month <- substr(betaamerifluxdf$TIMESTAMP,5,6)
#get cumulative NEE, GPP, and RECO for each month
betaameriflux.permonth<-  group_by(betaamerifluxdf, year, month, site_id) %>% dplyr::summarise(TS_F_MDS_1 = mean(TS_F_MDS_1),
                                                                                       SWC_F_MDS_1 = mean(SWC_F_MDS_1),
                                                                                       TA_F = mean(TA_F),
                                                                                       P_F =sum(P_F),
                                                                                       PPFD_IN = mean(PPFD_IN),
                                                                                       NEE_CUT_REF = sum(NEE_CUT_REF),
                                                                                       NEE_CUT_REF_QC= mean(NEE_CUT_REF_QC),
                                                                                       RECO_DT_CUT_REF = sum(RECO_DT_CUT_REF),
                                                                                       GPP_DT_CUT_REF = sum(GPP_DT_CUT_REF),
                                                                                       RECO_NT_CUT_REF = sum(RECO_NT_CUT_REF),
                                                                                       GPP_NT_CUT_REF = sum(GPP_NT_CUT_REF))
#separate DT and NT approaches
betaameriflux.permonthDT <- betaameriflux.permonth %>% select(-c(GPP_NT_CUT_REF, RECO_NT_CUT_REF))
betaameriflux.permonthDT$partition_method <- "DT"
betaameriflux.permonthDT <- betaameriflux.permonthDT %>% rename("GPP_CUT_REF"= "GPP_DT_CUT_REF",
                                                    "RECO_CUT_REF"= "RECO_DT_CUT_REF")
betaameriflux.permonthNT <- betaameriflux.permonth %>% select(-c(GPP_DT_CUT_REF, RECO_DT_CUT_REF))
betaameriflux.permonthNT$partition_method <- "NT"
betaameriflux.permonthNT <- betaameriflux.permonthNT %>% rename("GPP_CUT_REF"= "GPP_NT_CUT_REF",
                                                    "RECO_CUT_REF"= "RECO_NT_CUT_REF")
#merge back together with new column "partition method"
betaameriflux.permonth <- bind_rows(betaameriflux.permonthNT, betaameriflux.permonthDT) 

#########Merging amerifluxx fluxnet and ameriflux fluxnet beta #######
#combine all data
alldat.wdupes <- gdata::combine(betaameriflux.permonth, ameriflux.permonth) 
#find duplicates
dupes<- alldat.wdupes %>% get_dupes(site_id, year, month, partition_method)  
#Not duplicates OK to combine
ameriflux.fluxnetall <- gdata::combine(betaameriflux.permonth, ameriflux.permonth) 


### Adding in other variables
setwd("/Users/iwargowsky/Desktop/Ameriflux/AMF-FLUXNET")
meta <- read_xlsx("AMF_AA-Net_BIF_LEGACY_20230331.xlsx")
#filter for sites of interest
names <- unique(ameriflux.fluxnetall$site_id)
meta <- meta %>% filter(SITE_ID %in% names)
#move to better format and group by site
meta.wide <- meta %>% pivot_wider(names_from = VARIABLE, values_from = DATAVALUE) 
meta.bysite <- meta.wide %>% group_by(SITE_ID) %>% reframe(country= na.omit(COUNTRY),
                                                           citation = na.omit(DOI),
                                                           site_name= na.omit(SITE_NAME),
                                                           latitude= na.omit(LOCATION_LAT),
                                                           longitude= na.omit(LOCATION_LONG),
                                                           flux_method= na.omit(FLUX_MEASUREMENTS_METHOD))
#merge flux df and meta data
meta.bysite<- meta.bysite %>% rename(site_id= SITE_ID)
ameriflux.ALL <- left_join(ameriflux.fluxnetall, meta.bysite)
#noting what U-star filtering was used 
ameriflux.ALL$tower_corrections <- "CUT"
#save
setwd("/Users/iwargowsky/Desktop/Ameriflux")
write_csv(ameriflux.fluxnetall, "ameriflux.fluxnetALL.csv")



####extract list of sites and dates covered###
ameriflux.fluxnetall$ts <- paste(ameriflux.fluxnetall$year, ameriflux.fluxnetall$month)
sites <- subset(ameriflux.fluxnetall, select = c(site_id,ts))
sites.datescovered <- sites %>% group_by(site_id) %>% dplyr::summarise(start_date = min(ts),
                                                                       end_date = max(ts))
#double checking that function above worked
checkdates <- sites %>% arrange(site_id, ts)
