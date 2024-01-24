#ICOS data processing starting with daily# 
library(janitor)
library(tidyverse)
library(stringr)
library(readr)
library(data.table)
library(gdata)
library(DataCombine)
library(dplyr)
library(zoo)
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
#add data version
wwdat$data_version <- substr(wwdat$site_id, 42,47)
#clean up site id column
wwdat$site_id <- substr(wwdat$site_id, 5,10)
#add year,month, day columns
wwdat$year <- substr(wwdat$TIMESTAMP, 1,4)
wwdat$month <- substr(wwdat$TIMESTAMP, 5,6)
wwdat$TIMESTAMP <- NULL
#separate DT and NT approaches
wwdatDT <- wwdat %>% dplyr::select(-c(GPP_NT_CUT_REF, RECO_NT_CUT_REF)) 
wwdatDT$partition_method <- "Lasslop"
wwdatDT <- wwdatDT %>% dplyr::rename("GPP_CUT_REF"= "GPP_DT_CUT_REF",
                                                    "RECO_CUT_REF"= "RECO_DT_CUT_REF")
wwdatNT <- wwdat %>% dplyr::select(-c(GPP_DT_CUT_REF, RECO_DT_CUT_REF))
wwdatNT$partition_method <- "Reichstein"
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
                         mutate(gapfill = case_when(NEE_CUT_REF_QC %in% c(1,2,3)~1,
                                                    NEE_CUT_REF_QC %in% 0 ~ 0))%>%
                         dplyr::select(year, month, gapfill, NEE_CUT_REF_QC) %>%
                         group_by(year,month) %>% 
                         dplyr::summarise(gap_fill_perc = sum(gapfill)/n()*100))

ww.dat2.gf <- bind_rows(ww.dat2.gf, .id = "site_id") #turn list  into one df
wwdat <- merge(ww.dat2.gf, wwdat) #merge with data
wwdat$tower_corrections <- "CUT" #noting what U-star filtering was used 

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
#add data_version column (no data version is provided but need column for merging purposes)
icosdat$data_version <- ""
#clean up site id column
icosdat$site_id <- substr(icosdat$site_id, 9,14)
#add year,month, day columns
icosdat$year <- substr(icosdat$TIMESTAMP, 1,4)
icosdat$month <- substr(icosdat$TIMESTAMP, 5,6)
icosdat$TIMESTAMP <- NULL
#separate DT and NT approaches
icosdatDT <- icosdat %>% dplyr::select(-c(GPP_NT_CUT_REF, RECO_NT_CUT_REF))
icosdatDT$partition_method <- "Lasslop"
icosdatDT <- icosdatDT %>% dplyr::rename("GPP_CUT_REF"= "GPP_DT_CUT_REF",
                                                    "RECO_CUT_REF"= "RECO_DT_CUT_REF")
icosdatNT <- icosdat %>% dplyr::select(-c(GPP_DT_CUT_REF, RECO_DT_CUT_REF))
icosdatNT$partition_method <- "Reichstein"
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
                        mutate(gapfill = case_when(NEE_CUT_REF_QC %in% c(1,2,3)~1,
                                                   NEE_CUT_REF_QC %in% 0 ~ 0))%>%
                       dplyr::select(year, month, gapfill, NEE_CUT_REF_QC) %>%
                       group_by(year,month) %>% 
                       dplyr::summarise(gap_fill_perc = sum(gapfill)/n()*100))

etc.dat2.gf  <- bind_rows(etc.dat2.gf , .id = "site_id") #turn list  into one df
icosdat <- merge(etc.dat2.gf, icosdat) #merge with data
icosdat$tower_corrections <- "CUT" #noting what U-star filtering was used 


###VUT sites#####----------------------------------------------------------
setwd("/Users/iwargowsky/Desktop/ICOS/VUT sites")
sto <- read_csv("ICOSETC_SE-Sto_ARCHIVE_INTERIM_L2/ICOSETC_SE-Sto_FLUXNET_MM_INTERIM_L2.csv")
sto$site_id <- "SE-Sto"
#merge vut sites together
VUTsites <- sto
#add year,month, day columns
VUTsites <- VUTsites %>% mutate(year= substr(VUTsites$TIMESTAMP, 1,4),
                                month= substr(VUTsites$TIMESTAMP, 5,6))
#subset for only our variables of interest
VUTsites  <- VUTsites  %>% dplyr::select(site_id, year, month, TS_F_MDS_1,
                                        SWC_F_MDS_1, TA_F_MDS, P_F, PPFD_IN,
                                        NEE_VUT_REF, 
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

#Adding in DOIs
setwd("/Users/iwargowsky/Desktop/ICOS/ICOSETC")
etcdoi <- read_csv("ICOSETCDOI.csv")
VUTsites<- merge(VUTsites, etcdoi)
VUTsites$tower_corrections <- "VUT" #noting what U-star filtering was used 
VUTsites$data_version <- "" #no data version is provided but need column for merging purposes
#####GAP FIll % ####--------------------------------------------------
#load in files
setwd("/Users/iwargowsky/Desktop/ICOS/VUT sites")
sto.gf <- read_csv("ICOSETC_SE-Sto_ARCHIVE_INTERIM_L2/ICOSETC_SE-Sto_FLUXNET_HH_INTERIM_L2.csv")
sto.gf$site_id <- "SE-Sto"
#merge vut sites together
VUTsites.gf <- sto.gf
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

#merge VUTsites with icosdat
icosdat <- bind_rows(VUTsites, icosdat)

#####Merging warm winters and ecosystem theater#######-------------------------------------------
alldat.wdupes <- gdata::combine(wwdat, icosdat)
#find duplicates
dupes<- alldat.wdupes %>% get_dupes(site_id, year, month, partition_method)  
#remove duplicates to get final df
alldatpermonth <-alldat.wdupes %>% 
  distinct(site_id, year, month, partition_method, .keep_all = TRUE)
##MAKE SURE #newdf= #df.wdupes - (#dupes/2)


###ICOS Sweden SE-Sto####-----------------------------------------------------------
setwd("/Users/iwargowsky/Desktop/ICOS/Se-Sto")
path <- "/Users/iwargowsky/Desktop/ICOS/Se-Sto"
se.sto.files.flux <- list.files(path = path,pattern = '*SE-Sto_Fluxes_*',all.files = T,recursive = T)
se.sto.files.eco <- list.files(path = path,pattern = '*SE-Sto_eco_*',all.files = T,recursive = T)
se.sto.files.meteo <- list.files(path = path,pattern = '*SE-Sto_meteo_*',all.files = T,recursive = T)
#cycle through folders and subset for only our variables of interest
se.sto.flux <- rbindlist(sapply(se.sto.files.flux, fread, simplify = FALSE), use.names = TRUE, fill= TRUE)  %>%
  select(date, time, doy,Fch4_f_1_1_1, Reco_f_1_1_1, GPP_f_1_1_1, Reco_1_1_1, NEE_1_1_1 )
se.sto.eco <- rbindlist(sapply(se.sto.files.eco, fread, simplify = FALSE), use.names = TRUE, fill= TRUE)  %>%
  select(date, time, doy, starts_with("TS_"), starts_with("SWC_"), starts_with("GWL_") )
se.sto.meteo <- rbindlist(sapply(se.sto.files.meteo, fread, simplify = FALSE), use.names = TRUE, fill= TRUE) %>%
  select(date, time, doy, PPFD_IN_1_2_1, Ta_1_1_1, D_SNOW_1_1_1, P_1_1_1)
#Merge dfs
se.sto <- se.sto.flux %>% full_join(se.sto.eco, by= c("date", "time", "doy")) %>% 
  full_join(se.sto.meteo, by= c("date", "time", "doy"))
#create new time columns
se.sto <- se.sto %>% mutate(year= year(as.Date(date, format= "%d/%m/%Y")),
                            month= month(as.Date(date, format= "%d/%m/%Y")))


se.sto$ts <- as.yearmon(paste(se.sto$year, se.sto$month, sep="-"))
ggplot(se.sto) + 
  geom_point(aes(ts, Reco_f_1_1_1))
ggplot(se.sto) + 
  geom_point(aes(ts, GPP_f_1_1_1))
ggplot(se.sto) + 
  geom_point(aes(ts, NEE_1_1_1))
ggplot(data = subset(se.sto, se.sto$year=='2016')) + 
  geom_smooth(aes(ts, Ta_1_1_1))+
  ggtitle("2016")

se.sto <- as.data.frame(sapply(se.sto,as.numeric))
#aggregate by month
se.sto.monthly <- se.sto %>% group_by(year, month) %>%
  dplyr::summarise(NEE_CUT_REF= mean(NEE_1_1_1, na.rm= TRUE), #using naming conventions that match dfs above
            percent_na = (sum(is.na(NEE_1_1_1))/n()*100),
            RECO_CUT_REF= mean(c(Reco_f_1_1_1, Reco_1_1_1), na.rm= TRUE),
            GPP_CUT_REF= mean(GPP_f_1_1_1, na.rm= TRUE),
            TA_F_MDS= mean(Ta_1_1_1),
            SWC_F_MDS= mean(c(SWC_1_1_1, SWC_1_2_1, SWC_1_3_1, SWC_1_4_1, SWC_1_5_1, 
                              SWC_2_1_1, SWC_2_2_1, SWC_2_3_1, SWC_2_4_1, SWC_2_5_1,
                              SWC_3_1_1, SWC_3_2_1, SWC_3_3_1, SWC_3_4_1, SWC_3_5_1,
                              SWC_4_1_1, SWC_4_2_1, SWC_4_3_1, SWC_4_4_1, SWC_4_5_1)),
            tsoil_surface= mean(c(TS_1_1_1, TS_1_2_1, TS_1_3_1,
                                  TS_2_1_1, TS_2_2_1, TS_2_3_1,
                                  TS_3_1_1, TS_3_2_1, TS_3_3_1,
                                  TS_2_1_1, TS_4_2_1, TS_4_3_1)),
            tsoil_deep= mean(c(TS_1_4_1, TS_1_5_1, TS_2_4_1, TS_2_5_1,
                               TS_3_4_1, TS_3_5_1,  TS_4_5_1)),
            PPFD_IN = mean(PPFD_IN_1_2_1),
            P_F= sum(P_1_1_1),
            snow_depth= sum(D_SNOW_1_1_1),
            water_table_depth= mean(c(GWL_1_1_1, GWL_2_1_1, GWL_3_1_1, GWL_4_1_1)))

se.sto.monthly$site_id <- "SE-Sto"
se.sto.monthly$source <- "ICOS Sweden"
se.sto.monthly$data_version <- "Tier1"
se.sto.monthly$partition_method <- ""
se.sto.monthly$citation <- "ICOS Sweden, 2023. Collection of Abisko Stordalen Palsa Bog Swedish network data. https://doi.org/10.18160/Q6H6-B94B"
se.sto.monthly$tower_corrections <- ""
se.sto.monthly$moisture_depth <- "5"
se.sto.monthly$tsoil_surface_depth <- "5"
se.sto.monthly$tsoil_deep_depth <- "31"

###Final df#####----------------------------------------------------------------
x <- alldatpermonth %>% full_join(se.sto.monthly)
#find duplicates
dupes<- allICOSpermonth.wdupes  %>% get_dupes(site_id, year, month, partition_method)  
#remove duplicates to get final df
allICOSpermonth <-allICOSpermonth.wdupes  %>% 
  distinct(site_id, year, month, partition_method, .keep_all = TRUE)
##MAKE SURE #newdf= #df.wdupes - (#dupes/2)

##change units from per day to per month
allICOSpermonth$NEE_CUT_REF <- allICOSpermonth$NEE_CUT_REF *days_in_month(as.yearmon(paste(allICOSpermonth$year,allICOSpermonth$month,sep = '-')))
allICOSpermonth$RECO_CUT_REF <- allICOSpermonth$RECO_CUT_REF *days_in_month(as.yearmon(paste(allICOSpermonth$year,allICOSpermonth$month,sep = '-')))
allICOSpermonth$GPP_CUT_REF <- allICOSpermonth$GPP_CUT_REF *days_in_month(as.yearmon(paste(allICOSpermonth$year,allICOSpermonth$month,sep = '-')))
allICOSpermonth$P_F <- allICOSpermonth$P_F *days_in_month(as.yearmon(paste(allICOSpermonth$year,allICOSpermonth$month,sep = '-')))
#data usage
allICOSpermonth$data_usage <- "Tier 1" 
#adding gap fill method
allICOSpermonth$gap_fill <- "MDS"

setwd("/Users/iwargowsky/Desktop/ICOS")
write_csv(allICOSpermonth, "ICOSdatapermonth.csv")



