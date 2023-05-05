#ICOS data processing starting with daily# 
library(ggplot2)
library(janitor)
library(tidyverse)
library(stringr)
library(naniar)
library(readr)
library(gdata)
library(DataCombine)
library(dplyr)
#start with Warm Winters data
# Identify file names
setwd("/Users/iwargowsky/Desktop/ICOS/Warm Winters")
path <- "/Users/iwargowsky/Desktop/ICOS/Warm Winters"
wwlist_of_files <- list.files(path = path,pattern = '*_FULLSET_DD_',all.files = T,recursive = T)
#cycle through folders
wwicosdat <- wwlist_of_files %>%
  setNames(nm = .) %>% 
  map_df(~read_csv(.x, col_types = cols(), col_names = TRUE), .id = "site_id")          
colnames(wwicosdat)
#subset for only our variables of interest
wwdat <- wwicosdat %>% dplyr::select(site_id, TIMESTAMP, TS_F_MDS_1,
                                     SWC_F_MDS_1, TA_F, P_F, PPFD_IN,
                                     NEE_CUT_REF, NEE_CUT_REF_QC,
                                     RECO_DT_CUT_REF, GPP_DT_CUT_REF,
                                     RECO_NT_CUT_REF, GPP_NT_CUT_REF)

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
                                                                            SWC_F_MDS_1 = mean(SWC_F_MDS_1),
                                                                            TA_F = mean(TA_F),
                                                                            P_F =sum(P_F, na.rm = FALSE),
                                                                            PPFD_IN = mean(PPFD_IN),
                                                                            NEE_CUT_REF = sum(NEE_CUT_REF, na.rm = FALSE),
                                                                            NEE_CUT_REF_QC= mean(NEE_CUT_REF_QC),
                                                                            RECO_DT_CUT_REF = sum(RECO_DT_CUT_REF, na.rm = FALSE),
                                                                            GPP_DT_CUT_REF = sum(GPP_DT_CUT_REF, na.rm = FALSE),
                                                                            RECO_NT_CUT_REF = sum(RECO_NT_CUT_REF, na.rm = FALSE),
                                                                            GPP_NT_CUT_REF = sum(GPP_NT_CUT_REF, na.rm = FALSE))

#separate DT and NT approaches
wwdat.permonthDT <- wwdat.permonth %>% dplyr::select(-c(GPP_NT_CUT_REF, RECO_NT_CUT_REF)) 
wwdat.permonthDT$partition_method <- "DT"
wwdat.permonthDT <- wwdat.permonthDT %>% dplyr::rename("GPP_CUT_REF"= "GPP_DT_CUT_REF",
                                                    "RECO_CUT_REF"= "RECO_DT_CUT_REF")
wwdat.permonthNT <- wwdat.permonth %>% dplyr::select(-c(GPP_DT_CUT_REF, RECO_DT_CUT_REF))
wwdat.permonthNT$partition_method <- "NT"
wwdat.permonthNT <- wwdat.permonthNT %>% dplyr::rename("GPP_CUT_REF"= "GPP_NT_CUT_REF",
                                                    "RECO_CUT_REF"= "RECO_NT_CUT_REF")
#merge back together with new column "partition method"
wwdat.permonth <- bind_rows(wwdat.permonthNT, wwdat.permonthDT) 

#Adding in DOIs
wwdoi <- read_csv("warmwintersDOI.csv")
wwdat.permonth <- merge(wwdat.permonth, wwdoi)

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
icosdat <- allicosdat %>% dplyr::select(site_id, TIMESTAMP, TS_F_MDS_1,
                                        SWC_F_MDS_1, TA_F, P_F, PPFD_IN,
                                        NEE_CUT_REF, NEE_CUT_REF_QC,
                                        RECO_DT_CUT_REF, GPP_DT_CUT_REF,
                                        RECO_NT_CUT_REF, GPP_NT_CUT_REF)
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
                                                                                SWC_F_MDS_1 = mean(SWC_F_MDS_1),
                                                                                TA_F = mean(TA_F),
                                                                                P_F =sum(P_F, na.rm = FALSE),
                                                                                PPFD_IN = mean(PPFD_IN),
                                                                                NEE_CUT_REF = sum(NEE_CUT_REF, na.rm = FALSE),
                                                                                NEE_CUT_REF_QC= mean(NEE_CUT_REF_QC),
                                                                                RECO_DT_CUT_REF = sum(RECO_DT_CUT_REF, na.rm = FALSE),
                                                                                GPP_DT_CUT_REF = sum(GPP_DT_CUT_REF, na.rm = FALSE),
                                                                                RECO_NT_CUT_REF = sum(RECO_NT_CUT_REF, na.rm = FALSE),
                                                                                GPP_NT_CUT_REF = sum(GPP_NT_CUT_REF, na.rm = FALSE))
#separate DT and NT approaches
icosdat.permonthDT <- icosdat.permonth %>% dplyr::select(-c(GPP_NT_CUT_REF, RECO_NT_CUT_REF))
icosdat.permonthDT$partition_method <- "DT"
icosdat.permonthDT <- icosdat.permonthDT %>% dplyr::rename("GPP_CUT_REF"= "GPP_DT_CUT_REF",
                                                    "RECO_CUT_REF"= "RECO_DT_CUT_REF")
icosdat.permonthNT <- icosdat.permonth %>% dplyr::select(-c(GPP_DT_CUT_REF, RECO_DT_CUT_REF))
icosdat.permonthNT$partition_method <- "NT"
icosdat.permonthNT <- icosdat.permonthNT %>% dplyr::rename("GPP_CUT_REF"= "GPP_NT_CUT_REF",
                                                    "RECO_CUT_REF"= "RECO_NT_CUT_REF")
#merge back together with new column "partition method"
icosdat.permonth <- bind_rows(icosdat.permonthNT, icosdat.permonthDT) 

#Adding in DOIs
etcdoi <- read_csv("ICOSETCDOI.csv")
icosdat.permonth <- merge(icosdat.permonth, etcdoi)


#########Merging warm winters and icosdat #######
#combine all data
alldat.wdupes <- gdata::combine(wwdat.permonth, icosdat.permonth)
#find duplicates
dupes<- alldat.wdupes %>% get_dupes(site_id, year, month, partition_method)  
#remove duplicates to get final df
alldatpermonth <-alldat.wdupes %>% 
  distinct(site_id, year, month, partition_method, .keep_all = TRUE)
##MAKE SURE #newdf= #df.wdupes - (#dupes/2)

setwd("/Users/iwargowsky/Desktop/ICOS")
write_csv(alldatpermonth, "ICOSdatapermonth.csv")


#examine duplicate differences
#separate dupes by source
wwdupes <- dupes %>% filter(source=="wwdat.permonth")
icosdupes <- dupes %>% filter(source=="icosdat.permonth")

wwdupes$timestamp <- paste(wwdupes$year, wwdupes$month)
icosdupes$timestamp <- paste(icosdupes$year, icosdupes$month)
ggplot()+geom_line(wwdupes, mapping= aes(x=timestamp, y=NEE_CUT_REF, group=site_id, color=site_id))+
  geom_line(icosdupes, mapping= aes(x=timestamp, y=NEE_CUT_REF, group=site_id, color=site_id),linetype="dashed")
#look at individual sites
Nor <- dupes %>% filter(site_id == "SE-Nor")
Nor$timestamp <- paste(Nor$year, Nor$month)
ggplot()+geom_line(Nor, mapping= aes(x=timestamp, y=NEE_CUT_REF, group=source, color=source))


####extract list of sites and dates covered###
alldatpermonth$ts <- paste(alldatpermonth$year, alldatpermonth$month)
sites <- subset(alldatpermonth, select = c(site_id,ts))
sites.datescovered <- sites %>% group_by(site_id) %>% dplyr::summarise(start_date = min(ts),
                                                                       end_date = max(ts))
#double checking that function above worked
checkdates <- sites %>% arrange(site_id, ts)



