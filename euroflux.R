#LOADING IN data from the European Flux Database Cluster
library(dplyr)
library(readr)
library(purrr)
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
                            dplyr::summarise(gap_fill_perc = sum(gapfill)/n()*100)
#merge with df 
ceip.all <- merge(ceip.gfmonthly, ceip.renamed, by= c("site_reference", "year", "month"))  

#remove rows that do not contain flux data
ceip.all <-ceip.all %>% filter(if_all(c("nee", "gpp", "reco"), ~ !is.na(.)))

#add data policy according http://www.europe-fluxdata.eu/home/data/data-policy
ceip.all <- ceip.all %>%
  mutate(data_usage= ifelse(site_reference %in% "FI-Kns", "Tier 2","Tier 1"))

setwd("/Users/iwargowsky/Desktop/Euroflux")
write_csv(ceip.all , "eurofluxdata.csv")
