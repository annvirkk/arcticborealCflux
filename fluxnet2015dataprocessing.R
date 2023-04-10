###Fluxnet2015 data processing
library(plyr)
library(dplyr)
library(purrr)
library(readr)
library(readxl)
library(tidyverse)
#Downloaded data from https://fluxnet.org/data/download-data/
# sites selected based on those present in "tower sheet major" on the ABCflux google drive
#load in all the files
setwd("/Users/iwargowsky/Desktop/Fluxnet2015")
path <- "/Users/iwargowsky/Desktop/Fluxnet2015"
files <- list.files(path = path,pattern = '*FULLSET_DD_',all.files = T,recursive = T)
fluxnetdf <- files %>%
  setNames(nm = .) %>% 
  map_df(~read_csv(.x, col_types = cols(), col_names = TRUE, na=c("NA","-9999")), .id = "site_id")   
#clean up site id column
fluxnetdf$site_id <- substr(fluxnetdf$site_id, 5,10)
#select columns to keep
colnames(fluxnetdf) #see all column names
fluxnetdf <- fluxnetdf %>% dplyr::select(site_id, TIMESTAMP, TS_F_MDS_1,
                                         SWC_F_MDS_1, TA_F, P_F, PPFD_IN,
                                         NEE_CUT_REF,
                                         RECO_DT_CUT_REF, GPP_DT_CUT_REF,
                                         RECO_NT_CUT_REF, GPP_NT_CUT_REF)
#add month and year columns
fluxnetdf$year <- substr(fluxnetdf$TIMESTAMP,1,4)
fluxnetdf$month <- substr(fluxnetdf$TIMESTAMP,5,6)
#get cumulative NEE, GPP, and RECO for each month
fluxnet.permonth<-  group_by(fluxnetdf, year, month, site_id) %>% dplyr::summarise(TS_F_MDS_1 = mean(TS_F_MDS_1),
                                                                                   SWC_F_MDS_1 = mean(SWC_F_MDS_1),
                                                                                   TA_F = mean(TA_F),
                                                                                   P_F =sum(P_F),
                                                                                   PPFD_IN = mean(PPFD_IN),
                                                                                   NEE_CUT_REF = sum(NEE_CUT_REF),
                                                                                   RECO_DT_CUT_REF = sum(RECO_DT_CUT_REF),
                                                                                   GPP_DT_CUT_REF = sum(GPP_DT_CUT_REF),
                                                                                   RECO_NT_CUT_REF = sum(RECO_NT_CUT_REF),
                                                                                   GPP_NT_CUT_REF = sum(GPP_NT_CUT_REF))

#separate DT and NT approaches
fluxnet.permonthDT <- fluxnet.permonth %>% select(-c(GPP_NT_CUT_REF, RECO_NT_CUT_REF))
fluxnet.permonthDT$partition_method <- "DT"
fluxnet.permonthDT <- fluxnet.permonthDT %>% rename("GPP_CUT_REF"= "GPP_DT_CUT_REF",
                                                    "RECO_CUT_REF"= "RECO_DT_CUT_REF")
fluxnet.permonthNT <- fluxnet.permonth %>% select(-c(GPP_DT_CUT_REF, RECO_DT_CUT_REF))
fluxnet.permonthNT$partition_method <- "NT"
fluxnet.permonthNT <- fluxnet.permonthNT %>% rename("GPP_CUT_REF"= "GPP_NT_CUT_REF",
                                                    "RECO_CUT_REF"= "RECO_NT_CUT_REF")
#merge back together with new column "partition method"
fluxnet.permonth <- bind_rows(fluxnet.permonthNT, fluxnet.permonthDT) 


#write_csv(fluxnet.permonth, "fluxnetpermonth.csv")


#Adding in some metadata####
setwd("/Users/iwargowsky/Desktop/Fluxnet2015/FLX_AA-Flx_BIF_ALL_20200501")
meta <- read_xlsx("FLX_AA-Flx_BIF_MM_20200501.xlsx")
#filter for sites of interest
names <- unique(fluxnet.permonth$site_id)
meta <- meta %>% filter(SITE_ID %in% names)
#more to better format and group by site
meta.wide <- meta %>% pivot_wider(names_from = VARIABLE, values_from = DATAVALUE) 
meta.bysite <- meta.wide %>% group_by(SITE_ID) %>% reframe(country= na.omit(COUNTRY),
                                             citation = na.omit(DOI),
                                             site_name= na.omit(SITE_NAME),
                                             latitude= na.omit(LOCATION_LAT),
                                             longitude= na.omit(LOCATION_LONG))

#RU-Sam site has moved over the years so there are multiple lat-long coords
#for now we'll just take the first set of coords 
RU.Sam <- meta.bysite %>% filter(SITE_ID== "RU-Sam") %>% summarise_all(first)
meta.bysite <- meta.bysite %>% filter(!SITE_ID== "RU-Sam")
meta.bysite <- rbind(meta.bysite, RU.Sam)

#merge flux df and meta data
meta.bysite<- meta.bysite %>% rename(site_id= SITE_ID)
fluxnetALL <- left_join(fluxnet.permonth, meta.bysite)
#save
setwd("/Users/iwargowsky/Desktop/Fluxnet2015")
write_csv(fluxnetALL, "fluxnetALL.csv")


####extract list of sites and dates covered###
fluxnet.permonth$ts <- paste(fluxnet.permonth$year, fluxnet.permonth$month)
sites <- subset(fluxnet.permonth, select = c(site_id,ts))
sites.datescovered <- sites %>% group_by(site_id) %>% dplyr::summarise(start_date = min(ts),
                                                                       end_date = max(ts))
#double checking that function above worked
checkdates <- sites %>% arrange(site_id, ts)