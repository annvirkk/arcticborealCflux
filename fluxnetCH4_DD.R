###FluxnetCH4 data processing
library(dplyr)
library(purrr)
library(readr)
library(tidyverse)
library(stringr)
#Downloaded data from https://fluxnet.org/data/download-data/
# sites selected based on those present in "tower sheet major" on the ABCflux google drive
#load in all the files
setwd("/Users/iwargowsky/Desktop/Fluxnet-CH4")
path <- "/Users/iwargowsky/Desktop/Fluxnet-CH4"
files <- list.files(path = path,pattern = '*_DD_',all.files = T,recursive = T)
CH4fluxnetdf <- files %>%
  setNames(nm = .) %>% 
  map_df(~read_csv(.x,  col_names = TRUE, na=c("NA","-9999")), .id = "site_id")   
#clean up site id column
CH4fluxnetdf$data_version <- substr(CH4fluxnetdf$site_id, 34,36)
CH4fluxnetdf$site_id <- substr(CH4fluxnetdf$site_id, 5,10)
#select columns to keep
colnames(CH4fluxnetdf) #see all column names
CH4fluxnet <- CH4fluxnetdf %>% dplyr::select(site_id, TIMESTAMP, FCH4_F, TA_F, P_F,
                                             D_SNOW_F, TS_1, SWC_F, GPP_NT, GPP_DT,
                                             RECO_NT, RECO_DT, PPFD_IN_F, NEE_F, data_version)
#add month and year columns
CH4fluxnet$year <- substr(CH4fluxnet$TIMESTAMP,1,4)
CH4fluxnet$month <- substr(CH4fluxnet$TIMESTAMP,5,6)
#get cumulative NEE, GPP, and RECO for each month
CH4fluxnet.permonth<-  group_by(CH4fluxnet, year, month, site_id, data_version) %>% 
  dplyr::summarise(FCH4_F = sum(FCH4_F, na.rm= FALSE),
                   TA_F = mean(TA_F),
                   P_F = sum(P_F),
                   D_SNOW_F = mean(D_SNOW_F),
                   TS_1 = mean(TS_1),
                   SWC_F= mean(SWC_F),
                   PPFD_IN_F = mean(PPFD_IN_F),
                   GPP_NT= sum(GPP_NT), GPP_DT=sum(GPP_DT),
                   RECO_NT= sum(RECO_NT), RECO_DT=sum(RECO_DT),
                   NEE_F= sum(NEE_F))

#separate DT and NT approaches
CH4fluxnet.permonthDT <- CH4fluxnet.permonth %>% select(-c(GPP_NT, RECO_NT))
CH4fluxnet.permonthDT$partition_method <- "Lasslop"
CH4fluxnet.permonthDT <- CH4fluxnet.permonthDT %>% dplyr::rename("GPP"= "GPP_DT",
                                                    "RECO"= "RECO_DT")
CH4fluxnet.permonthNT <- CH4fluxnet.permonth %>% select(-c(GPP_DT, RECO_DT))
CH4fluxnet.permonthNT$partition_method <- "Reichstein"
CH4fluxnet.permonthNT <- CH4fluxnet.permonthNT %>% dplyr::rename("GPP"= "GPP_NT",
                                                    "RECO"= "RECO_NT")
#merge back together with new column "partition method"
CH4fluxnet.permonth <- bind_rows(CH4fluxnet.permonthNT, CH4fluxnet.permonthDT) 

#units
CH4fluxnet.permonth$NEE_F <- CH4fluxnet.permonth$NEE_F*1.0368*days_in_month(as.yearmon(paste(CH4fluxnet.permonth$year,CH4fluxnet.permonth$month,sep = '-')))
CH4fluxnet.permonth$GPP <- CH4fluxnet.permonth$GPP*1.0368*days_in_month(as.yearmon(paste(CH4fluxnet.permonth$year,CH4fluxnet.permonth$month,sep = '-')))
CH4fluxnet.permonth$RECO <- CH4fluxnet.permonth$RECO*1.0368*days_in_month(as.yearmon(paste(CH4fluxnet.permonth$year,CH4fluxnet.permonth$month,sep = '-')))
CH4fluxnet.permonth$FCH4_F <- CH4fluxnet.permonth$FCH4_F*0.0010368*days_in_month(as.yearmon(paste(CH4fluxnet.permonth$year,CH4fluxnet.permonth$month,sep = '-')))


#Adding in some metadata####-----------------------------------------------------
setwd("/Users/iwargowsky/Desktop/Fluxnet-CH4")
meta <- read_csv("FLX_AA-Flx_CH4-META_20201112135337801132.csv")
#filter for sites of interest
names <- unique(CH4fluxnet.permonth$site_id)
meta <- meta %>% filter(SITE_ID %in% names)
colnames(meta)
#keep relevant columns
meta.2 <- meta %>% select(SITE_NAME, SITE_ID, COUNTRY, LAT, LON, SOIL_TEMP_PROBE_DEPTHS,
                          MOSS_BROWN, MOSS_SPHAGNUM, DOM_VEG, "FLUXNET-CH4_DATA_POLICY")
#convert columns to match variables in ABCflux v2
meta.2$sphagnum_cover[meta.2$MOSS_SPHAGNUM == "1"] <- "Present"
meta.2$other_moss_cover[meta.2$MOSS_BROWN == "1"] <- "Present"
meta.2$sphagnum_cover[meta.2$DOM_VEG == "moss_sphagnum"] <- "Dominant"
meta.2$other_moss_cover[meta.2$DOM_VEG == "moss_brown"] <- "Dominant"

colnames(meta.2)
meta.3 <- meta.2 %>% select(SITE_NAME, SITE_ID, COUNTRY, LAT, LON,
                            sphagnum_cover, other_moss_cover, "FLUXNET-CH4_DATA_POLICY")

#merge flux df and meta data
meta.3<- meta.3 %>% dplyr::rename(site_id= SITE_ID)
CH4fluxnetALL <- left_join(CH4fluxnet.permonth, meta.3)
#fix data usage to match ABCFlux v2
CH4fluxnetALL <- CH4fluxnetALL %>%
  mutate('FLUXNET-CH4_DATA_POLICY'= ifelse(`FLUXNET-CH4_DATA_POLICY`=="CCBY4.0","Tier 1",`FLUXNET-CH4_DATA_POLICY`),
         'FLUXNET-CH4_DATA_POLICY'= ifelse(`FLUXNET-CH4_DATA_POLICY`=="TIER2","Tier 2",`FLUXNET-CH4_DATA_POLICY`))
#adding FLUXNET-CH4 citation
CH4fluxnetALL$citation <- "https://doi.org/10.5194/essd-13-3607-2021"


setwd("/Users/iwargowsky/Desktop/Fluxnet-CH4")
write_csv(CH4fluxnetALL, "CH4fluxnetpermonth.csv")


####extract list of sites and dates covered###
CH4fluxnet.permonth$ts <- paste(CH4fluxnet.permonth$year, CH4fluxnet.permonth$month)
sites <- subset(CH4fluxnet.permonth, select = c(site_id,ts))
sites.datescovered <- sites %>% group_by(site_id) %>% dplyr::summarise(start_date = min(ts),
                                                                       end_date = max(ts))
#double checking that function above worked
checkdates <- sites %>% arrange(site_id, ts)
