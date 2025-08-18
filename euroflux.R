#LOADING IN data from the European Flux Database Cluster
library(dplyr)
library(readr)
library(purrr)
library(rquery)

## Identify file names
setwd("/Users/iwargowsky/Desktop/Euroflux/Data")
path <- "/Users/iwargowsky/Desktop/Euroflux/Data"
list_of_files <- list.files(path = path,pattern = 'CEIP_EC_L4_m_*',all.files = T,recursive = T)
#cycle through folders
ceip <- list_of_files %>%
  setNames(nm = .) %>% 
  map_df(~read_csv(.x, col_types = cols(), col_names = TRUE, na=c("NA","-9999.00", "-9999")), .id = "site_reference")          
#convert units to per month by multiplying by number of days
ceip$NEE_or_fMDS <- ceip$NEE_or_fMDS *ceip$n_days
ceip$Reco_or <- ceip$Reco_or *ceip$n_days
ceip$GPP_or_MDS <- ceip$GPP_or_MDS *ceip$n_days
ceip$GPP_or_MDS <- ceip$GPP_or_MDS *-1
ceip$Precip <- ceip$Precip *ceip$n_days
#rename columns to match ABCflux v2
colnames(ceip)
ceip.renamed <- ceip %>% dplyr::rename('month'= 'Month',
                                       'tair'='Ta_f',
                                       'tsoil_surface'= 'Ts_f',
                                       'precip'='Precip',
                                       'soil_moisture'= 'SWC',
                                       'nee'= 'NEE_or_fMDS',
                                       'reco'= 'Reco_or',
                                       'gpp'='GPP_or_MDS')
#add year, site_id, and partition method columns
ceip.renamed <- ceip.renamed %>% mutate(data_version= substr(site_reference, 23,25),
                                        year= substr(site_reference, 18, 21),
                                        site_reference= paste(substr(site_reference, 12, 13), '-',substr(site_reference, 14, 16), sep=''),
                                        partition_method= "Reichstein")
#select only relevant columns
ceip.renamed <- ceip.renamed %>% select(year, month, site_reference, tair, tsoil_surface, data_version,
                                        precip, soil_moisture, nee, gpp, reco, partition_method)

#####GAP FIll % ####--------------------------------------------------
files.gf <- list.files(path = path,pattern = 'CEIP_EC_L4_h_*',all.files = T,recursive = T)
#load in files as a list of df
#cycle through folders
ceipdat.gf <- files.gf %>%
  setNames(nm = .) %>% 
  map_df(~read_csv(.x, col_types = cols(), col_names = TRUE, na = c("NA",'-9999',"-9999.00")), .id = "site_reference")  
ceipdat.gf[ceipdat.gf=='-9999'] <- NA #didnt read -9999 as NA initially so fixing that
#add year and month columns
ceip.gf <- ceipdat.gf %>% mutate(year= substr(site_reference, 18, 21), month= Month)
#clean up site_reference 
ceip.gf$site_reference <- paste(substr(ceip.gf$site_reference, 12, 13), '-',substr(ceip.gf$site_reference, 14, 16), sep='')
#replace 1,2,3 with 1, sum and divide by 48 to get gapfill percentage per day
ceip.gfmonthly <- ceip.gf %>% mutate(gapfill = case_when(NEE_or_fMDSqc %in% c(1,2,3) ~ 1,
                                                         NEE_or_fMDSqc %in% 0 ~ 0))%>%
                            dplyr::select(year, month, site_reference, gapfill) %>%
                            group_by(year,month, site_reference) %>% 
                            dplyr::summarise(gap_fill_perc_nee = sum(gapfill)/n()*100)
#merge with df 
ceip.all <- merge(ceip.gfmonthly, ceip.renamed, by= c("site_reference", "year", "month"))  

#remove rows that do not contain flux data
ceip.all <-ceip.all %>% dplyr::filter(if_all(c("nee", "gpp", "reco"), ~ !is.na(.)))

#add data policy according http://www.europe-fluxdata.eu/home/data/data-policy
ceip.all <- ceip.all %>%
  mutate(data_usage= ifelse(site_reference %in% "FI-Kns", "Tier 2","Tier 1"))

ceip.all$gap_fill <- "MDS"

setwd("/Users/iwargowsky/Desktop/Euroflux")
write_csv(ceip.all , "eurofluxdata.csv")








#LOADING IN data from the European Flux Database Cluster
## Identify file names
setwd("/Users/iwargowsky/Desktop/Euroflux")
path <- "/Users/iwargowsky/Desktop/Euroflux"

files <- list.files(path = path,
                    pattern = "^EFDC_L2_Flx_.*\\.txt$",
                    recursive = TRUE,
                    full.names = TRUE)
no.gf <- bind_rows(lapply(files, function(file) {
  df <- read_delim(file, delim = ",", na= c("-9999.0000", "-9999") ) 
  df$source_file <- basename(file)     
  return(df)
}))

no.gf <- no.gf %>%
  mutate(site_reference = substr(source_file, 13, 17)) %>%
  mutate(site_reference = sub("^(.{2})(.*)$", "\\1-\\2", site_reference)) %>%
  mutate(data_version  = str_extract(source_file, "v\\d+")) %>%
  mutate(year= substr(TIMESTAMP_START, 1,4),
         month= substr(TIMESTAMP_START, 5,6))

no.gf.monthly <- no.gf %>% group_by(year, month, site_reference, data_version) %>%
  dplyr::summarise(water_table_depth= mean(WTD, na.rm= TRUE),
                   tair= mean(TA, na.rm=TRUE),
                   tsoil_surface= mean(c(TS, TS_2, TS_3), na.rm=TRUE),
                   ppfd= mean(PPFD_IN, na.rm=TRUE),
                   soil_moisture= mean(c(SWC, SWC_2, SWC_3), na.rm=TRUE),
                   snow_depth= mean(D_SNOW, na.rm=TRUE),
                   precip= sum(P, na.rm=FALSE),
                   nee= mean(FC, na.rm=TRUE),
                   ch4_flux_total= mean(FCH4, na.rm=TRUE))

no.gf.monthly$gap_fill <- "Monthly Averages from non-gapfilled data"
no.gf.monthly$dataentry_person <- "Wargowsky"
no.gf.monthly$flux_method <- "EC"
no.gf.monthly$extraction_source_co2 <- "European Fluxes Database Cluster"
no.gf.monthly$extraction_source_ch4 <- "European Fluxes Database Cluster"

no.gf.monthly$nee <- no.gf.monthly$nee*1.0368*days_in_month(as.yearmon(paste(no.gf.monthly$year,no.gf.monthly$month,sep = '-')))
no.gf.monthly$ch4_flux_total <- no.gf.monthly$ch4_flux_total*.001368*days_in_month(as.yearmon(paste(no.gf.monthly$year,no.gf.monthly$month,sep = '-')))

setwd("/Users/iwargowsky/Desktop/ABCFlux v2")
static <- read_csv("static.towersites.csv") %>%
  dplyr::filter(site_reference %in% c("FI-Hyy","IS-Gun","SE-Fla","FI-Kaa","GL-ZaH","RU-Ha2","RU-Zot",
                                      "FI-Kns","RU-Ha3","FI-Sii","SE-Faj","SE-Kno","FI-Let","FI-Si2","SE-Htm","RU-Fy3"))
#join to fill NAs
no.gf.monthly.static <- natural_join(no.gf.monthly, static, by= "site_reference", jointype= "FULL")

no.gf.monthly.static <- no.gf.monthly.static %>%
  mutate(year= as.integer(year)) %>%
  mutate(data_usage = case_when(
    (site_reference %in% c("FI-Kns", "FI-Sii", "FI-Si2", "SE-Htm")) |
    (site_reference %in% "FI-Hyy" & year > 2015) |
    (site_reference %in% "SE-Faj" & year > 2007) |
    (site_reference %in% "FI-Let" & year < 2017) ~ "Tier 2",
    TRUE ~ "Tier 1"
  ))

no.gf.monthly.co2 <- no.gf.monthly.static %>% 
  dplyr::filter(!is.na(nee)) %>%
  select(-c(ch4_flux_total,extraction_source_ch4)) 
no.gf.monthly.ch4 <- no.gf.monthly.static %>% dplyr::filter(!is.na(ch4_flux_total))%>%
  select(-c(nee,  extraction_source_co2))%>%
  dplyr::rename("data_usage_ch4"= "data_usage",
                "gap_fill_ch4"= "gap_fill",
                "data_version_ch4"= "data_version")

setwd("/Users/iwargowsky/Desktop/Euroflux")
write_csv(no.gf.monthly.co2 , "euroflux.no.gf.monthly.co2.csv")
write_csv(no.gf.monthly.ch4 , "euroflux.no.gf.monthly.ch4.csv")

