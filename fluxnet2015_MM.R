###Fluxnet2015 data processing
library(plyr)
library(dplyr)
library(purrr)
library(readr)
library(readxl)
library(tidyverse)
library(data.table)
library(lubridate)
library(zoo)

#This script is for processing FLUXNET data

#Downloaded data from https://fluxnet.org/data/download-data/
####load in files######-----------------------------------------------------------
setwd("/Users/iwargowsky/Desktop/Fluxnet2015/Fluxnet2015")
path <- "/Users/iwargowsky/Desktop/Fluxnet2015/Fluxnet2015"
files <- list.files(path = path,pattern = '*FULLSET_MM_',all.files = T,recursive = T)
fluxnetdf <- files %>%
  setNames(nm = .) %>% 
  map_df(~read_csv(.x, col_types = cols(), col_names = TRUE, na=c("NA","-9999")), .id = "site_id")   
#clean up site id column
fluxnetdf$data_version <- substr(fluxnetdf$site_id, 42,44)
fluxnetdf$site_id <- substr(fluxnetdf$site_id, 5,10)
#select columns to keep
colnames(fluxnetdf) #see all column names
fluxnetdf <- fluxnetdf %>% dplyr::select(site_id, TIMESTAMP, TS_F_MDS_1,
                                         SWC_F_MDS_1, TA_F, P_F, PPFD_IN,
                                         NEE_CUT_REF, data_version,
                                         RECO_DT_CUT_REF, GPP_DT_CUT_REF,
                                         RECO_NT_CUT_REF, GPP_NT_CUT_REF)
#add month and year columns
fluxnetdf$year <- substr(fluxnetdf$TIMESTAMP,1,4)
fluxnetdf$month <- substr(fluxnetdf$TIMESTAMP,5,6)
fluxnetdf$TIMESTAMP <- NULL
#separate DT and NT approaches
fluxnetdfDT <- fluxnetdf %>% dplyr::select(-c(GPP_NT_CUT_REF, RECO_NT_CUT_REF))
fluxnetdfDT$partition_method <- "Lasslop"
fluxnetdfDT <- fluxnetdfDT %>% dplyr::rename("GPP_CUT_REF"= "GPP_DT_CUT_REF",
                                                    "RECO_CUT_REF"= "RECO_DT_CUT_REF")
fluxnetdfNT <- fluxnetdf %>% dplyr::select(-c(GPP_DT_CUT_REF, RECO_DT_CUT_REF))
fluxnetdfNT$partition_method <- "Reichstein"
fluxnetdfNT <- fluxnetdfNT %>% dplyr::rename("GPP_CUT_REF"= "GPP_NT_CUT_REF",
                                                    "RECO_CUT_REF"= "RECO_NT_CUT_REF")
#merge back together with new column "partition method"
fluxnetdf <- bind_rows(fluxnetdfNT, fluxnetdfDT) 
fluxnetdf$tower_corrections <- "CUT" #noting what U-star filtering was used 

#####GAP FIll % ####--------------------------------------------------
files <- list.files(path = path,pattern = '*_FULLSET_H',all.files = T,recursive = T)
#load in files as a list of df
fluxnet.dat2 <- lapply(files,function(i){
  fread(i, na.strings =c("NA","-9999"), header = TRUE, select=c('TIMESTAMP_START', 'NEE_CUT_REF_QC'))
})
names(fluxnet.dat2)<- substr(files, 5,10) #name each df
# QC= 1,2, 3 indicate gapfilled data 0 = measured value
#replace 1,2,3 with 1, sum and divide by 48 to get gapfill percentage per day
fluxnet.dat2.gf <- lapply(fluxnet.dat2, function(df) df %>%
                        mutate( year = substr(df$TIMESTAMP_START, 1,4),
                                month = substr(df$TIMESTAMP_START, 5,6) ) %>%
                        mutate(gapfill = case_when(NEE_CUT_REF_QC %in% c(1,2,3)~1,
                                                   NEE_CUT_REF_QC %in% 0 ~ 0))%>%
                        dplyr::select(year, month, gapfill, NEE_CUT_REF_QC) %>%
                        group_by(year,month) %>% 
                        dplyr::summarise(gap_fill_perc = sum(gapfill)/n()*100))

fluxnet.dat2.gf  <- bind_rows(fluxnet.dat2.gf , .id = "site_id") #turn list  into one df
fluxnetdf <- merge(fluxnet.dat2.gf, fluxnetdf) #merge with data

###VUT sites #####----------------------------------------------------------
#some sites only have Variable U-star Threshold so we'll process them separately
setwd("/Users/iwargowsky/Desktop/Fluxnet2015/VUT sites")
blv <- read_csv("FLX_SJ-Blv_FLUXNET2015_FULLSET_2008-2009_1-4/FLX_SJ-Blv_FLUXNET2015_FULLSET_MM_2008-2009_1-4.csv",
                na=c("NA","-9999"))
blv$site_id <- "SJ-Blv"
blv$data_version <- "1-4"
vrk <- read_csv("FLX_RU-Vrk_FLUXNET2015_FULLSET_2008-2008_1-4/FLX_RU-Vrk_FLUXNET2015_FULLSET_MM_2008-2008_1-4.csv",
                na=c("NA","-9999"))
vrk$site_id <- "RU-Vrk"
vrk$data_version <- "1-4"
#merge vut sites together
VUTsites <- bind_rows(vrk, blv)
#add year,month, day columns
VUTsites <- VUTsites %>% mutate(year= substr(VUTsites$TIMESTAMP, 1,4),
                                month= substr(VUTsites$TIMESTAMP, 5,6))
#subset for only our variables of interest
VUTsites  <- VUTsites  %>% dplyr::select(site_id, year, month, TS_F_MDS_1,
                                         SWC_F_MDS_1, TA_F, P_F, PPFD_IN,
                                         NEE_VUT_REF, data_version,
                                         RECO_DT_VUT_REF, GPP_DT_VUT_REF,
                                         RECO_NT_VUT_REF, GPP_NT_VUT_REF)
#separate DT and NT approaches
VUTsitesDT <- VUTsites %>% dplyr::select(-c(GPP_NT_VUT_REF, RECO_NT_VUT_REF))
VUTsitesDT$partition_method <- "Lasslop"
VUTsitesDT <- VUTsitesDT %>% dplyr::rename("GPP_CUT_REF"= "GPP_DT_VUT_REF",
                                           "RECO_CUT_REF"= "RECO_DT_VUT_REF",
                                           "NEE_CUT_REF"= "NEE_VUT_REF")
VUTsitesNT <- VUTsites %>% dplyr::select(-c(GPP_DT_VUT_REF, RECO_DT_VUT_REF))
VUTsitesNT$partition_method <- "Reichstein"
VUTsitesNT <- VUTsitesNT %>% dplyr::rename("GPP_CUT_REF"= "GPP_NT_VUT_REF", 
                                           "RECO_CUT_REF"= "RECO_NT_VUT_REF",
                                           "NEE_CUT_REF"= "NEE_VUT_REF") 
#intentionally renamed as CUT not VUT for merging purposes
#merge back together with new column "partition method"
VUTsites <- bind_rows(VUTsitesNT, VUTsitesDT) 
VUTsites$tower_corrections <- "VUT" #noting what U-star filtering was used 
#####GAP FIll % ####--------------------------------------------------
setwd("/Users/iwargowsky/Desktop/Fluxnet2015/VUT sites")
blv.gf <- read_csv("FLX_SJ-Blv_FLUXNET2015_FULLSET_2008-2009_1-4/FLX_SJ-Blv_FLUXNET2015_FULLSET_HR_2008-2009_1-4.csv")
blv.gf$site_id <- "SJ-Blv"
vrk.gf <- read_csv("FLX_RU-Vrk_FLUXNET2015_FULLSET_2008-2008_1-4/FLX_RU-Vrk_FLUXNET2015_FULLSET_HH_2008-2008_1-4.csv")
vrk.gf$site_id <- "RU-Vrk"
#merge vut sites together
VUTsites.gf <- bind_rows(blv.gf, vrk.gf)
# QC= 1,2, 3 indicate gapfilled data 0 = measured value
#replace 1,2,3 with 1, sum and divide by 48 to get gapfill percentage per day
VUT.dat2.gf <- VUTsites.gf %>%
  mutate( year = substr(TIMESTAMP_START, 1,4),
          month = substr(TIMESTAMP_START, 5,6) ) %>%
  mutate(gapfill = case_when(NEE_VUT_REF_QC %in% c(1,2,3)~1,
                             NEE_VUT_REF_QC %in% 0 ~ 0))%>%
  dplyr::select(year, month, gapfill, NEE_VUT_REF_QC) %>%
  group_by(year,month) %>% 
  dplyr::summarise(gap_fill_perc = sum(gapfill)/n()*100)
VUTsites <- merge(VUT.dat2.gf, VUTsites) #merge with data

#####merge CUT and VUT data####---------------------------------------------------
fluxnetdf<- bind_rows(fluxnetdf, VUTsites)

#Adding in some metadata####------------------------------------------------------
setwd("/Users/iwargowsky/Desktop/Fluxnet2015/FLX_AA-Flx_BIF_ALL_20200501")
meta <- read_xlsx("FLX_AA-Flx_BIF_MM_20200501.xlsx")
#filter for sites of interest
names <- unique(fluxnetdf$site_id)
meta <- meta %>% dplyr::filter(SITE_ID %in% names)
#more to better format and group by site
meta.wide <- meta %>% pivot_wider(names_from = VARIABLE, values_from = DATAVALUE) 
meta.bysite <- meta.wide %>% group_by(SITE_ID) %>% reframe(country= na.omit(COUNTRY),
                                                           citation = na.omit(DOI))
#merge flux df and meta data
meta.bysite<- meta.bysite %>% dplyr::rename(site_id= SITE_ID)
fluxnetALL <- left_join(fluxnetdf, meta.bysite)
#adding gap fill method
fluxnetALL$gap_fill <- "MDS"

##change units from per day to per month
fluxnetALL$NEE_CUT_REF <- fluxnetALL$NEE_CUT_REF *days_in_month(as.yearmon(paste(fluxnetALL$year,fluxnetALL$month,sep = '-')))
fluxnetALL$RECO_CUT_REF <- fluxnetALL$RECO_CUT_REF *days_in_month(as.yearmon(paste(fluxnetALL$year,fluxnetALL$month,sep = '-')))
fluxnetALL$GPP_CUT_REF <- fluxnetALL$GPP_CUT_REF *days_in_month(as.yearmon(paste(fluxnetALL$year,fluxnetALL$month,sep = '-')))
fluxnetALL$GPP_CUT_REF <- fluxnetALL$GPP_CUT_REF*-1 #reverse signs
fluxnetALL$P_F <- fluxnetALL$P_F *days_in_month(as.yearmon(paste(fluxnetALL$year,fluxnetALL$month,sep = '-')))
##adding data usage policies according to https://fluxnet.org/data/data-policy/
fluxnetALL <- fluxnetALL %>% 
  mutate(data_usage= ifelse(site_id %in% c('RU-Sam','RU-SkP','RU-Tks','RU-Vrk','SE-St1'), "Tier 2", "Tier 1"))


#####final df #####--------------------------------------------------------------
setwd("/Users/iwargowsky/Desktop/Fluxnet2015")
write_csv(fluxnetALL, "fluxnetpermonth.csv")


####extract list of sites and dates covered###
fluxnetALL$ts <- as.yearmon(paste(fluxnetALL$year, fluxnetALL$month, sep="-"))
sites <- subset(fluxnetALL, select = c(site_id,ts))
sites.datescovered <- sites %>% group_by(site_id) %>% dplyr::summarise(start_date = min(ts),
                                                                       end_date = max(ts))
