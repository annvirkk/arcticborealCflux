#ICOS data processing starting with daily# 
library(janitor)
library(tidyverse)
library(stringr)
library(naniar)
library(readr)
library(gdata)
library(DataCombine)
library(dplyr)
library(lubridate)
###Warm Winters data#####
## Identify file names
setwd("/Users/iwargowsky/Desktop/ICOS/Warm Winters")
path <- "/Users/iwargowsky/Desktop/ICOS/Warm Winters"
wwlist_of_files <- list.files(path = path,pattern = '*_FULLSET_MM_',all.files = T,recursive = T)
#cycle through folders
wwicosdat <- wwlist_of_files %>%
  setNames(nm = .) %>% 
  map_df(~read_csv(.x, col_types = cols(), col_names = TRUE, na=c("NA","-9999")), .id = "site_id")          
colnames(wwicosdat)
#subset for only our variables of interest
wwdat <- wwicosdat %>% dplyr::select(site_id, TIMESTAMP, TS_F_MDS_1,
                                     SWC_F_MDS_1, TA_F_MDS, P_F, PPFD_IN,
                                     NEE_CUT_REF, 
                                     RECO_DT_CUT_REF, GPP_DT_CUT_REF,
                                     RECO_NT_CUT_REF, GPP_NT_CUT_REF)
#clean up site id column
wwdat$site_id <- substr(wwdat$site_id, 5,10)
#add year,month, day columns
wwdat$year <- substr(wwdat$TIMESTAMP, 1,4)
wwdat$month <- substr(wwdat$TIMESTAMP, 5,6)
wwdat$TIMESTAMP <- NULL
#separate DT and NT approaches
wwdatDT <- wwdat %>% dplyr::select(-c(GPP_NT_CUT_REF, RECO_NT_CUT_REF)) 
wwdatDT$partition_method <- "DT"
wwdatDT <- wwdatDT %>% dplyr::rename("GPP_CUT_REF"= "GPP_DT_CUT_REF",
                                                    "RECO_CUT_REF"= "RECO_DT_CUT_REF")
wwdatNT <- wwdat %>% dplyr::select(-c(GPP_DT_CUT_REF, RECO_DT_CUT_REF))
wwdatNT$partition_method <- "NT"
wwdatNT <- wwdatNT %>% dplyr::rename("GPP_CUT_REF"= "GPP_NT_CUT_REF",
                                                    "RECO_CUT_REF"= "RECO_NT_CUT_REF")
#merge back together with new column "partition method"
wwdat <- bind_rows(wwdatNT, wwdatDT) 

#Adding in DOIs
wwdoi <- read_csv("warmwintersDOI.csv")
wwdat <- merge(wwdat, wwdoi)

#####GAP FIll % ####--------------------------------------------------
files <- list.files(path = path,pattern = '*_FULLSET_HH_',all.files = T,recursive = T)
#load in files as a list of df
ww.dat2 <- lapply(files,function(i){
  fread(i, na.strings =c("NA","-9999"), header = TRUE,select=c('TIMESTAMP_START', 'NEE_CUT_REF_QC'))
})
names(ww.dat2)<- substr(files, 5,10) #name each df
#replace 1,2,3 with 1, sum and divide by 48 to get gapfill percentage per day
ww.dat2.gf <- lapply(ww.dat2, function(df) df %>%
                         mutate( year = substr(df$TIMESTAMP_START, 1,4),
                                 month = substr(df$TIMESTAMP_START, 5,6) ) %>%
                         mutate(gapfill = case_when(NEE_CUT_REF_QC %in% c(1,2,3) ~ 1))%>%
                         dplyr::select(year, month, gapfill, NEE_CUT_REF_QC) %>%
                         group_by(year,month) %>% 
                         dplyr::summarise(gap_fill_perc = sum(gapfill, na.rm=TRUE)/n()*100))

ww.dat2.gf <- bind_rows(ww.dat2.gf, .id = "site_id") #turn list  into one df
wwdat <- merge(ww.dat2.gf, wwdat) #merge with data

###ICOS ETC archive data####-------------------------------------------------------
# Identify file names
setwd("/Users/iwargowsky/Desktop/ICOS/ICOSETC")
path <- "/Users/iwargowsky/Desktop/ICOS/ICOSETC"
icoslist_of_files <- list.files(path = path,pattern = '*_FLUXNET_MM_',all.files = T,recursive = T) 
#NOTE had to remove csvs that had "VARINFO" and "product_description" from original ICOS download folders for this to work
#cycle through all folders
allicosdat <- icoslist_of_files %>%
  setNames(nm = .) %>% 
  map_df(~read_csv(.x, col_types = cols(), col_names = TRUE, na=c("NA","-9999")), .id = "site_id")          
#subset for only our variables of interest
icosdat <- allicosdat %>% dplyr::select(site_id, TIMESTAMP, TS_F_MDS_1,
                                        SWC_F_MDS_1, TA_F_MDS, P_F, PPFD_IN,
                                        NEE_CUT_REF,
                                        RECO_DT_CUT_REF, GPP_DT_CUT_REF,
                                        RECO_NT_CUT_REF, GPP_NT_CUT_REF)
#clean up site id column
icosdat$site_id <- substr(icosdat$site_id, 9,14)
#add year,month, day columns
icosdat$year <- substr(icosdat$TIMESTAMP, 1,4)
icosdat$month <- substr(icosdat$TIMESTAMP, 5,6)
icosdat$TIMESTAMP <- NULL
#separate DT and NT approaches
icosdatDT <- icosdat %>% dplyr::select(-c(GPP_NT_CUT_REF, RECO_NT_CUT_REF))
icosdatDT$partition_method <- "DT"
icosdatDT <- icosdatDT %>% dplyr::rename("GPP_CUT_REF"= "GPP_DT_CUT_REF",
                                                    "RECO_CUT_REF"= "RECO_DT_CUT_REF")
icosdatNT <- icosdat %>% dplyr::select(-c(GPP_DT_CUT_REF, RECO_DT_CUT_REF))
icosdatNT$partition_method <- "NT"
icosdatNT <- icosdatNT %>% dplyr::rename("GPP_CUT_REF"= "GPP_NT_CUT_REF",
                                                    "RECO_CUT_REF"= "RECO_NT_CUT_REF")
#merge back together with new column "partition method"
icosdat <- bind_rows(icosdatNT, icosdatDT) 

#Adding in DOIs
etcdoi <- read_csv("ICOSETCDOI.csv")
icosdat <- merge(icosdat, etcdoi)

#####GAP FIll % ####--------------------------------------------------
files <- list.files(path = path,pattern = '*_HH_',all.files = T,recursive = T)
#load in files as a list of df
etc.dat2 <- lapply(files,function(i){
  fread(i, na.strings =c("NA","-9999"), header = TRUE, select=c('TIMESTAMP_START', 'NEE_CUT_REF_QC'))
})
names(etc.dat2)<- substr(files, 9,14) #name each df
#replace 1,2,3 with 1, sum and divide by 48 to get gapfill percentage per day
etc.dat2.gf <- lapply(etc.dat2, function(df) df %>%
                       mutate( year = substr(df$TIMESTAMP_START, 1,4),
                               month = substr(df$TIMESTAMP_START, 5,6) ) %>%
                       mutate(gapfill = case_when(NEE_CUT_REF_QC %in% c(1,2,3) ~ 1))%>%
                       dplyr::select(year, month, gapfill, NEE_CUT_REF_QC) %>%
                       group_by(year,month) %>% 
                       dplyr::summarise(gap_fill_perc = sum(gapfill, na.rm=TRUE)/n()*100))

etc.dat2.gf  <- bind_rows(etc.dat2.gf , .id = "site_id") #turn list  into one df
icosdat <- merge(etc.dat2.gf, icosdat) #merge with data

#####Merging warm winters and icosdat #######-------------------------------------------
alldat.wdupes <- gdata::combine(wwdat, icosdat)
#find duplicates
dupes<- alldat.wdupes %>% get_dupes(site_id, year, month, partition_method)  
#remove duplicates to get final df
alldatpermonth <-alldat.wdupes %>% 
  distinct(site_id, year, month, partition_method, .keep_all = TRUE)
##MAKE SURE #newdf= #df.wdupes - (#dupes/2)
alldatpermonth$tower_corrections <- "CUT" #noting what U-star filtering was used 

###VUT sites#####----------------------------------------------------------
#Adding in GL-Dsk and FI-Ken again since they doesnt have CUT, only VUT
setwd("/Users/iwargowsky/Desktop/ICOS/VUT sites")
dsk <- read_csv("ICOSETC_GL-Dsk_ARCHIVE_L2/ICOSETC_GL-Dsk_FLUXNET_MM_L2.csv")
dsk$site_id <- "GL-Dsk"
ken <- read_csv("ICOSETC_FI-Ken_ARCHIVE_L2/ICOSETC_FI-Ken_FLUXNET_MM_L2.csv")
ken$site_id <- "FI-Ken"
#merge vut sites together
VUTsites <- bind_rows(ken, dsk)
VUTsites$source <- "icosdat"
#add year,month, day columns
VUTsites <- VUTsites %>% mutate(year= substr(VUTsites$TIMESTAMP, 1,4),
                                month= substr(VUTsites$TIMESTAMP, 5,6))
#subset for only our variables of interest
VUTsites  <- VUTsites  %>% dplyr::select(site_id, year, month, source, TS_F_MDS_1,
                                        SWC_F_MDS_1, TA_F_MDS, P_F, PPFD_IN,
                                        NEE_VUT_REF, 
                                        RECO_DT_VUT_REF, GPP_DT_VUT_REF,
                                        RECO_NT_VUT_REF, GPP_NT_VUT_REF)
#separate DT and NT approaches
VUTsitesDT <- VUTsites %>% dplyr::select(-c(GPP_NT_VUT_REF, RECO_NT_VUT_REF))
VUTsitesDT$partition_method <- "DT"
VUTsitesDT <- VUTsitesDT %>% dplyr::rename("GPP_CUT_REF"= "GPP_DT_VUT_REF",
                                 "RECO_CUT_REF"= "RECO_DT_VUT_REF",
                                 "NEE_CUT_REF"= "NEE_VUT_REF")
VUTsitesNT <- VUTsites %>% dplyr::select(-c(GPP_DT_VUT_REF, RECO_DT_VUT_REF))
VUTsitesNT$partition_method <- "NT"
VUTsitesNT <- VUTsitesNT %>% dplyr::rename("GPP_CUT_REF"= "GPP_NT_VUT_REF", 
                                "RECO_CUT_REF"= "RECO_NT_VUT_REF",
                                "NEE_CUT_REF"= "NEE_VUT_REF") 
#intentionally renamed as CUT not VUT for merging purposes
#merge back together with new column "partition method"
VUTsites <- bind_rows(VUTsitesNT, VUTsitesDT) 

#Adding in DOIs
setwd("/Users/iwargowsky/Desktop/ICOS/ICOSETC")
etcdoi <- read_csv("ICOSETCDOI.csv")
VUTsites<- merge(VUTsites, etcdoi)
VUTsites$tower_corrections <- "VUT" #noting what U-star filtering was used 
#####GAP FIll % ####--------------------------------------------------
#load in files
setwd("/Users/iwargowsky/Desktop/ICOS/VUT sites")
dsk.gf <- read_csv("ICOSETC_GL-Dsk_ARCHIVE_L2/ICOSETC_GL-Dsk_FLUXNET_HH_L2.csv")
dsk.gf$site_id <- "GL-Dsk"
ken.gf <- read_csv("ICOSETC_FI-Ken_ARCHIVE_L2/ICOSETC_FI-Ken_FLUXNET_HH_L2.csv")
ken.gf$site_id <- "FI-Ken"
#merge vut sites together
VUTsites.gf <- bind_rows(ken.gf, dsk.gf)
#replace 1,2,3 with 1, sum and divide by 48 to get gapfill percentage per day
VUT.dat2.gf <- VUTsites.gf %>%
                        mutate( year = substr(TIMESTAMP_START, 1,4),
                                month = substr(TIMESTAMP_START, 5,6) ) %>%
                        mutate(gapfill = case_when(NEE_VUT_REF_QC %in% c(1,2,3) ~ 1))%>%
                        dplyr::select(year, month, gapfill, NEE_VUT_REF_QC) %>%
                        group_by(year,month) %>% 
                        dplyr::summarise(gap_fill_perc = sum(gapfill, na.rm=TRUE)/n()*100)
VUTsites <- merge(VUT.dat2.gf, VUTsites) #merge with data


###Final df#####----------------------------------------------------------
allICOSpermonth.wdupes <- bind_rows(alldatpermonth, VUTsites)
#find duplicates
dupes<- allICOSpermonth.wdupes  %>% get_dupes(site_id, year, month, partition_method)  
#remove duplicates to get final df
allICOSpermonth <-allICOSpermonth.wdupes  %>% 
  distinct(site_id, year, month, partition_method, .keep_all = TRUE)
##MAKE SURE #newdf= #df.wdupes - (#dupes/2)

setwd("/Users/iwargowsky/Desktop/ICOS")
write_csv(allICOSpermonth, "ICOSdatapermonth.csv")

