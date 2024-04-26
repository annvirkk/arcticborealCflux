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
                         dplyr::summarise(gap_fill_perc_nee = sum(gapfill)/n()*100))

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
                       dplyr::summarise(gap_fill_perc_nee = sum(gapfill)/n()*100))

etc.dat2.gf  <- bind_rows(etc.dat2.gf , .id = "site_id") #turn list  into one df
icosdat <- merge(etc.dat2.gf, icosdat) #merge with data
icosdat$tower_corrections <- "CUT" #noting what U-star filtering was used 


###VUT sites#####----------------------------------------------------------
setwd("/Users/iwargowsky/Desktop/ICOS/VUT sites")
sto <- read_csv("ICOSETC_SE-Sto_ARCHIVE_INTERIM_L2/ICOSETC_SE-Sto_FLUXNET_MM_INTERIM_L2.csv", na= c(NA, "-9999"))
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
                        dplyr::summarise(gap_fill_perc_nee = sum(gapfill)/n()*100)
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

#adding gap fill method
alldatpermonth$gap_fill<- "MDS"

###ICOS Sweden SE-Sto####-----------------------------------------------------------
setwd("/Users/iwargowsky/Desktop/ICOS/Se-Sto")
path <- "/Users/iwargowsky/Desktop/ICOS/Se-Sto"
se.sto.files.flux <- list.files(path = path,pattern = '*SE-Sto_Fluxes_*',all.files = T,recursive = T)
se.sto.files.eco <- list.files(path = path,pattern = '*SE-Sto_eco_*',all.files = T,recursive = T)
se.sto.files.meteo <- list.files(path = path,pattern = '*SE-Sto_meteo_*',all.files = T,recursive = T)
#cycle through folders and subset for only our variables of interest
se.sto.flux <- rbindlist(sapply(se.sto.files.flux, fread, simplify = FALSE, na.strings="NaN"), use.names = TRUE, fill= TRUE)  %>%
  select(date, time, Fch4_f_1_1_1, Reco_f_1_1_1, GPP_f_1_1_1, Reco_1_1_1, NEE_1_1_1 ) %>% dplyr::filter(!date== "dd/mm/yyyy")
se.sto.eco <- rbindlist(sapply(se.sto.files.eco, fread, simplify = FALSE, na.strings="NaN"), use.names = TRUE, fill= TRUE)  %>%
  select(date, time, starts_with("TS_"), starts_with("SWC_"), starts_with("GWL_") )%>% dplyr::filter(!date== "dd/mm/yyyy") 
se.sto.meteo <- rbindlist(sapply(se.sto.files.meteo, fread, simplify = FALSE, na.strings=c("NaN", "-9999.00000")), use.names = TRUE, fill= TRUE) %>%
  select(date, time, PPFD_IN_1_2_1, Ta_1_1_1, D_SNOW_1_1_1, P_1_1_1)%>% dplyr::filter(!date== "dd/mm/yyyy")
#consistent data column formats
se.sto.flux$date <- as.Date(se.sto.flux$date, format= "%d/%m/%Y")
se.sto.eco$date <- as.Date(se.sto.eco$date, format= "%d/%m/%Y")
se.sto.meteo$date <- as.Date(se.sto.meteo$date, format= "%d/%m/%Y")

#Merge dfs
se.sto <- se.sto.flux %>% full_join(se.sto.eco, by= c("date", "time")) %>% 
  full_join(se.sto.meteo, by= c("date", "time"))

#average by time to create consistent column names
se.sto.time <- se.sto %>% group_by(date, time) %>%
  dplyr::summarise(NEE_CUT_REF= mean(as.numeric(NEE_1_1_1), na.rm= TRUE), #using naming conventions that match dfs above
          RECO_CUT_REF= mean(as.numeric(c(Reco_f_1_1_1, Reco_1_1_1)), na.rm= TRUE),
          GPP_CUT_REF= mean(as.numeric(GPP_f_1_1_1), na.rm= TRUE),
          ch4_flux_total= mean(as.numeric(Fch4_f_1_1_1), na.rm = TRUE),
          TA_F_MDS= mean(as.numeric(Ta_1_1_1), na.rm= TRUE),
          SWC_F_MDS_1= mean(as.numeric(c(SWC_1_1_1, SWC_1_2_1, SWC_1_3_1, SWC_1_4_1, SWC_1_5_1, 
                            SWC_2_1_1, SWC_2_2_1, SWC_2_3_1, SWC_2_4_1, SWC_2_5_1,
                            SWC_3_1_1, SWC_3_2_1, SWC_3_3_1, SWC_3_4_1, SWC_3_5_1,
                            SWC_4_1_1, SWC_4_2_1, SWC_4_3_1, SWC_4_4_1, SWC_4_5_1)), na.rm= TRUE),
          TS_F_MDS_1= mean(as.numeric(c(TS_1_1_1, TS_1_2_1, TS_1_3_1,
                                TS_2_1_1, TS_2_2_1, TS_2_3_1,
                                TS_3_1_1, TS_3_2_1, TS_3_3_1,
                                TS_2_1_1, TS_4_2_1, TS_4_3_1)), na.rm= TRUE),
          tsoil_deep= mean(as.numeric(c(TS_1_4_1, TS_1_5_1, TS_2_4_1, TS_2_5_1,
                             TS_3_4_1, TS_3_5_1,  TS_4_5_1)), na.rm= TRUE),
          PPFD_IN = mean(as.numeric(PPFD_IN_1_2_1), na.rm= TRUE),
          P_F= mean(as.numeric(P_1_1_1), na.rm= TRUE),
          snow_depth= sum(as.numeric(D_SNOW_1_1_1), na.rm= TRUE),
          water_table_depth= mean(as.numeric(c(GWL_1_1_1, GWL_2_1_1, GWL_3_1_1, GWL_4_1_1)), na.rm= TRUE))

#remove rows that do not contain flux data
se.sto.time <- se.sto.time %>%
  dplyr::filter(!if_all(c(NEE_CUT_REF, RECO_CUT_REF, GPP_CUT_REF, ch4_flux_total), ~ is.na(.)))

#create new time columns
se.sto.time <- se.sto.time %>% mutate(year= year(date), month= month(date))

#aggregate by month
se.sto.monthly <- se.sto.time %>% group_by(year, month) %>%
  dplyr::summarise( percent_na_nee= sum(is.na(NEE_CUT_REF)/n())*100,
                    percent_na_reco= sum(is.na(RECO_CUT_REF)/n())*100,
                    percent_na_gpp= sum(is.na(GPP_CUT_REF)/n())*100,
                    percent_na_ch4= sum(is.na(ch4_flux_total)/n())*100,
            NEE_CUT_REF= mean(NEE_CUT_REF, na.rm= TRUE), #using naming conventions that match dfs above
            RECO_CUT_REF= mean(RECO_CUT_REF, na.rm= TRUE),
            GPP_CUT_REF= mean(GPP_CUT_REF, na.rm= TRUE),
            ch4_flux_total= mean(ch4_flux_total, na.rm= TRUE),
            TA_F_MDS= mean(TA_F_MDS, na.rm= TRUE),
            SWC_F_MDS_1= mean(SWC_F_MDS_1, na.rm= TRUE),
            TS_F_MDS_1= mean(TS_F_MDS_1, na.rm= TRUE),
            tsoil_deep= mean(tsoil_deep, na.rm= TRUE),
            PPFD_IN = mean(PPFD_IN, na.rm= TRUE),
            P_F= mean(P_F, na.rm= TRUE),
            snow_depth= sum(snow_depth, na.rm= TRUE),
            #water_table_depth= mean(water_table_depth, na.rm= TRUE)
  )

se.sto.monthly <- se.sto.monthly %>% dplyr::rename(gap_fill_perc_nee = "percent_na_nee",
                                                   gap_fill_perc_reco = "percent_na_reco",
                                                   gap_fill_perc_gpp = "percent_na_gpp",
                                                   gap_fill_perc_ch4 = "percent_na_ch4")

#remove 2020 data according to email from Jutta
se.sto.monthly <- se.sto.monthly %>% dplyr::filter(!year== 2020)

#convert units from umol m-2 s-1 to g C m-2 day-1
se.sto.monthly$NEE_CUT_REF <- se.sto.monthly$NEE_CUT_REF *1.0368 
se.sto.monthly$RECO_CUT_REF <- se.sto.monthly$RECO_CUT_REF *1.0368
se.sto.monthly$GPP_CUT_REF <- se.sto.monthly$GPP_CUT_REF *1.0368 
se.sto.monthly$ch4_flux_total <- se.sto.monthly$ch4_flux_total *1.0368
se.sto.monthly$P_F <- se.sto.monthly$P_F *100 #units online say mm but I suspect its cm so converting

#clarifying partition method because they used Lasslop for GPP fluxes and Reichstein for RECO
se.sto.monthly.gpp <- se.sto.monthly %>% mutate(partition_method= "Lasslop") %>%
                                         mutate(RECO_CUT_REF= NA)
se.sto.monthly.reco <- se.sto.monthly %>% mutate(partition_method= "Reichstein")%>%
                                          mutate(GPP_CUT_REF= NA)
se.sto.monthly <- se.sto.monthly.gpp %>% full_join(se.sto.monthly.reco)


#Static info
se.sto.monthly$site_id <- "SE-Sto"
se.sto.monthly$source <- "Sweden"
se.sto.monthly$extraction_source_ch4 <- "ICOS Sweden"
se.sto.monthly$citation <- "ICOS Sweden, 2023. Collection of Abisko Stordalen Palsa Bog Swedish network data. https://doi.org/10.18160/Q6H6-B94B"
se.sto.monthly$citation_ch4 <- "ICOS Sweden, 2023. Collection of Abisko Stordalen Palsa Bog Swedish network data. https://doi.org/10.18160/Q6H6-B94B"
se.sto.monthly$tower_corrections <- ""
se.sto.monthly$moisture_depth <- "5"
se.sto.monthly$tsoil_surface_depth <- "5"
se.sto.monthly$tsoil_deep_depth <- "31"
se.sto.monthly$gap_fill <- "REddyProc"

#se.sto.monthly$ts <- as.yearmon(paste(se.sto.monthly$year, se.sto.monthly$month, sep="-"))
#ggplot(data = subset(se.sto.monthly, se.sto.monthly$year== "2020"))+
#  geom_line(aes(ts, NEE_CUT_REF))

###Final df#####----------------------------------------------------------------
alldatpermonth$year <- as.integer(alldatpermonth$year)
alldatpermonth$month <- as.integer(alldatpermonth$month)
allICOSpermonth.wdupes <- alldatpermonth %>% full_join(se.sto.monthly)

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
allICOSpermonth$GPP_CUT_REF <- allICOSpermonth$GPP_CUT_REF* -1
allICOSpermonth$P_F <- allICOSpermonth$P_F *days_in_month(as.yearmon(paste(allICOSpermonth$year,allICOSpermonth$month,sep = '-')))
#data usage
allICOSpermonth$data_usage <- "Tier 1" 
#flux method
allICOSpermonth$flux_method <- "EC"



setwd("/Users/iwargowsky/Desktop/ICOS")
write_csv(allICOSpermonth, "ICOSdatapermonth.csv")



