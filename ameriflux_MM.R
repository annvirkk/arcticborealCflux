###Ameriflux data processing####
library(plyr)
library(dplyr)
library(purrr)
library(readr)
library(ggplot2)
library(vroom)
library(readxl)
library(data.table)
library(tidyr)
library(janitor)
library(lubridate)
library(zoo)
#Downloaded data from https://ameriflux.lbl.gov/data/download-data/

#load in all FLUXNET files ####
setwd("/Users/iwargowsky/Desktop/Ameriflux/AMF-FLUXNET")
path <- "/Users/iwargowsky/Desktop/Ameriflux/AMF-FLUXNET"
files <- list.files(path = path,pattern = '*FULLSET_MM_',all.files = T,recursive = T)
amerifluxdf <- files %>%
  setNames(nm = .) %>% 
  map_df(~read_csv(.x, col_types = cols(), col_names = TRUE, na=c("NA","-9999")), .id = "site_id")         
#clean up site id column
amerifluxdf$site_id <- substr(amerifluxdf$site_id, 5,10)
#select columns to keep
colnames(amerifluxdf) #see all column names
amerifluxdf <- amerifluxdf %>% dplyr::select(site_id, TIMESTAMP, TS_F_MDS_1,
                                             SWC_F_MDS_1, TA_F, P_F, PPFD_IN,
                                             NEE_CUT_REF, 
                                             RECO_DT_CUT_REF, GPP_DT_CUT_REF,
                                             RECO_NT_CUT_REF, GPP_NT_CUT_REF)
#add month and year columns
amerifluxdf$year <- substr(amerifluxdf$TIMESTAMP,1,4)
amerifluxdf$month <- substr(amerifluxdf$TIMESTAMP,5,6)
amerifluxdf$TIMESTAMP <- NULL
#separate DT and NT approaches
amerifluxdfDT <- amerifluxdf %>% dplyr::select(-c(GPP_NT_CUT_REF, RECO_NT_CUT_REF))
amerifluxdfDT$partition_method <- "Lasslop"
amerifluxdfDT <- amerifluxdfDT %>% dplyr::rename("GPP_CUT_REF"= "GPP_DT_CUT_REF",
                                                    "RECO_CUT_REF"= "RECO_DT_CUT_REF")
amerifluxdfNT <- amerifluxdf %>% dplyr::select(-c(GPP_DT_CUT_REF, RECO_DT_CUT_REF))
amerifluxdfNT$partition_method <- "Reichstein"
amerifluxdfNT <- amerifluxdfNT %>% dplyr::rename("GPP_CUT_REF"= "GPP_NT_CUT_REF",
                                                    "RECO_CUT_REF"= "RECO_NT_CUT_REF")
#merge back together with new column "partition method"
amerifluxdf <- bind_rows(amerifluxdfNT, amerifluxdfDT) 

#####GAP FIll % ####--------------------------------------------------
files <- list.files(path = path,pattern = '*_FULLSET_H',all.files = T,recursive = T)
#load in files as a list of df
ameriflux.dat2 <- lapply(files,function(i){
  fread(i, na.strings =c("NA","-9999"), header = TRUE, select=c('TIMESTAMP_START', 'NEE_CUT_REF_QC'))
})
names(ameriflux.dat2)<- substr(files, 5,10) #name each df
#replace 1,2,3 with 1, sum and divide by 48 to get gapfill percentage per day
ameriflux.dat2.gf <- lapply(ameriflux.dat2, function(df) df %>%
                            mutate( year = substr(df$TIMESTAMP_START, 1,4),
                                    month = substr(df$TIMESTAMP_START, 5,6) ) %>%
                            mutate(gapfill = case_when(NEE_CUT_REF_QC %in% c(1,2,3)~1,
                                                       NEE_CUT_REF_QC %in% 0 ~ 0))%>%
                            dplyr::select(year, month, gapfill, NEE_CUT_REF_QC) %>%
                            group_by(year,month) %>% 
                            dplyr::summarise(gap_fill_perc = sum(gapfill)/n()*100))

ameriflux.dat2.gf  <- bind_rows(ameriflux.dat2.gf , .id = "site_id") #turn list  into one df
amerifluxdf <- merge(ameriflux.dat2.gf, amerifluxdf) #merge with data

#load in all FLUXNETbeta files ####------------------------------------------------
setwd("/Users/iwargowsky/Desktop/Ameriflux/AMF-FLUXNETbeta")
path <- "/Users/iwargowsky/Desktop/Ameriflux/AMF-FLUXNETbeta"
betafiles <- list.files(path = path,pattern = '*FULLSET_MM_',all.files = T,recursive = T)
betaamerifluxdf <- betafiles %>%
  setNames(nm = .) %>% 
  map_df(~read_csv(.x, col_types = cols(), col_names = TRUE, na=c("NA","-9999")), .id = "site_id")         
#clean up site id column
betaamerifluxdf$site_id <- substr(betaamerifluxdf$site_id, 5,10)
#select columns to keep
colnames(betaamerifluxdf) #see all column names
betaamerifluxdf <- betaamerifluxdf %>% dplyr::select(site_id, TIMESTAMP, TS_F_MDS_1,
                                             SWC_F_MDS_1, TA_F, P_F, PPFD_IN,
                                             NEE_CUT_REF,
                                             RECO_DT_CUT_REF, GPP_DT_CUT_REF,
                                             RECO_NT_CUT_REF, GPP_NT_CUT_REF)
#add month and year columns
betaamerifluxdf$year <- substr(betaamerifluxdf$TIMESTAMP,1,4)
betaamerifluxdf$month <- substr(betaamerifluxdf$TIMESTAMP,5,6)
betaamerifluxdf$TIMESTAMP <- NULL
#separate DT and NT approaches
betaamerifluxdfDT <- betaamerifluxdf %>% dplyr::select(-c(GPP_NT_CUT_REF, RECO_NT_CUT_REF))
betaamerifluxdfDT$partition_method <- "Lasslop"
betaamerifluxdfDT <- betaamerifluxdfDT %>% dplyr::rename("GPP_CUT_REF"= "GPP_DT_CUT_REF",
                                                    "RECO_CUT_REF"= "RECO_DT_CUT_REF")
betaamerifluxdfNT <- betaamerifluxdf %>% dplyr::select(-c(GPP_DT_CUT_REF, RECO_DT_CUT_REF))
betaamerifluxdfNT$partition_method <- "Reichstein"
betaamerifluxdfNT <- betaamerifluxdfNT %>% dplyr::rename("GPP_CUT_REF"= "GPP_NT_CUT_REF",
                                                    "RECO_CUT_REF"= "RECO_NT_CUT_REF")
#merge back together with new column "partition method"
betaamerifluxdf <- bind_rows(betaamerifluxdfNT, betaamerifluxdfDT) 


#####GAP FIll % ####--------------------------------------------------
files <- list.files(path = path,pattern = '*_FULLSET_H',all.files = T,recursive = T)
#load in files as a list of df
beta.dat2 <- lapply(files,function(i){
  fread(i, na.strings =c("NA","-9999"), header = TRUE, select=c('TIMESTAMP_START', 'NEE_CUT_REF_QC'))
})
names(beta.dat2)<- substr(files, 5,10) #name each df
#replace 1,2,3 with 1, sum and divide by 48 to get gapfill percentage per day
beta.dat2.gf <- lapply(beta.dat2, function(df) df %>%
                              mutate( year = substr(df$TIMESTAMP_START, 1,4),
                                      month = substr(df$TIMESTAMP_START, 5,6) ) %>%
                              mutate(gapfill = case_when(NEE_CUT_REF_QC %in% c(1,2,3)~1,
                                                         NEE_CUT_REF_QC %in% 0 ~ 0))%>%
                              dplyr::select(year, month, gapfill, NEE_CUT_REF_QC) %>%
                              group_by(year,month) %>% 
                              dplyr::summarise(gap_fill_perc = sum(gapfill)/n()*100))

beta.dat2.gf  <- bind_rows(beta.dat2.gf , .id = "site_id") #turn list  into one df
betaamerifluxdf <- merge(beta.dat2.gf, betaamerifluxdf) #merge with data
#########Merging ameriflux fluxnet and ameriflux fluxnet beta #######
alldat.wdupes <- gdata::combine(betaamerifluxdf, amerifluxdf) 
#find duplicates
dupes<- alldat.wdupes %>% get_dupes(site_id, year, month, partition_method)  
#No duplicates OK to combine
ameriflux.fluxnetall <- gdata::combine(betaamerifluxdf, amerifluxdf) 
#adding data usage policies
ameriflux.fluxnetall <- ameriflux.fluxnetall %>% 
  mutate(data_usage= ifelse(site_id %in% c('CA-HPC',"CA-NS1","CA-NS2","CA-NS3","CA-NS4","CA-NS5","CA-NS6",
                                           "CA-NS7","CA-NS8","CA-Ojp","CA-Qc2","CA-SCC","CA-SCB","CA-SJ1",
                                           "CA-SJ2","CA-SJ3","CA-SMC","CA-TVC","CA-WP1","CA-WP2","CA-WP3",
                                           "US-Atq","US-Beo","US-Bes","US-Bn1","US-Bn2","US-Bn3","US-Brw",
                                           "US-Cms","US-HVa","US-HVs","US-Ivo","US-Sag","US-Upa"), "Tier 2", "Tier 1"))



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
                                                           longitude= na.omit(LOCATION_LONG))
#merge flux df and meta data
meta.bysite<- meta.bysite %>% dplyr::rename(site_id= SITE_ID)
ameriflux.ALL <- left_join(ameriflux.fluxnetall, meta.bysite)
#noting what U-star filtering was used 
ameriflux.ALL$tower_corrections <- "CUT"
#save

##change units from per day to per month
ameriflux.ALL$NEE_CUT_REF <- ameriflux.ALL$NEE_CUT_REF *days_in_month(as.yearmon(paste(ameriflux.ALL$year,ameriflux.ALL$month,sep = '-')))
ameriflux.ALL$RECO_CUT_REF <- ameriflux.ALL$RECO_CUT_REF *days_in_month(as.yearmon(paste(ameriflux.ALL$year,ameriflux.ALL$month,sep = '-')))
ameriflux.ALL$GPP_CUT_REF <- ameriflux.ALL$GPP_CUT_REF *days_in_month(as.yearmon(paste(ameriflux.ALL$year,ameriflux.ALL$month,sep = '-')))
ameriflux.ALL$P_F <- ameriflux.ALL$P_F  *days_in_month(as.yearmon(paste(ameriflux.ALL$year,ameriflux.ALL$month,sep = '-')))

setwd("/Users/iwargowsky/Desktop/Ameriflux")
write_csv(ameriflux.ALL, "ameriflux.fluxnetALL.csv")



####extract list of sites and dates covered###
ameriflux.ALL$ts <- paste(ameriflux.ALL$year, ameriflux.ALL$month)
sites <- subset(ameriflux.ALL, select = c(site_id,ts))
sites.datescovered <- sites %>% group_by(site_id) %>% dplyr::summarise(start_date = min(ts),
                                                                       end_date = max(ts))
#double checking that function above worked
checkdates <- sites %>% arrange(site_id, ts)
