###Fluxnet2015 data processing
library(plyr)
library(dplyr)
library(purrr)
library(readr)
#Downloaded data from https://fluxnet.org/data/download-data/
# sites selected based on those present in "tower sheet major" on the ABCflux google drive
#load in all the files
setwd("/Users/iwargowsky/Desktop/Fluxnet2015")
path <- "/Users/iwargowsky/Desktop/Fluxnet2015"
files <- list.files(path = path,pattern = '*_DD_',all.files = T,recursive = T)
fluxnetdf <- files %>%
  setNames(nm = .) %>% 
  map_df(~read_csv(.x, col_types = cols(), col_names = TRUE, na=c("NA","-9999")), .id = "site_id")         
#clean up site id column
fluxnetdf$site_id <- substr(fluxnetdf$site_id, 5,10)
#select columns to keep
colnames(fluxnetdf) #see all column names
fluxnetdf <- fluxnetdf %>% dplyr::select(site_id, TIMESTAMP, TS_F_MDS_1, TS_F_MDS_1_QC,
                                         SWC_F_MDS_1, SWC_F_MDS_1_QC, 
                                         TA_F, TA_F_QC, 
                                         P_F,P_F_QC,
                                         NEE_VUT_REF,NEE_VUT_REF_QC,
                                         RECO_DT_VUT_REF, GPP_DT_VUT_REF,
                                         RECO_NT_VUT_REF, GPP_NT_VUT_REF)
#add month and year columns
fluxnetdf$year <- substr(fluxnetdf$TIMESTAMP,1,4)
fluxnetdf$month <- substr(fluxnetdf$TIMESTAMP,5,6)
#get cumulative NEE, GPP, and RECO for each month
fluxnet.permonth<-  group_by(fluxnetdf, year, month, site_id) %>% dplyr::summarise(TS_F_MDS_1 = mean(TS_F_MDS_1),
                                                                            TS_F_MDS_1_QC = mean(TS_F_MDS_1_QC),
                                                                            SWC_F_MDS_1 = mean(SWC_F_MDS_1),
                                                                            SWC_F_MDS_1_QC = mean(SWC_F_MDS_1_QC),
                                                                            TA_F = mean(TA_F),
                                                                            TA_F_QC = mean(TA_F_QC),
                                                                            P_F =sum(P_F),
                                                                            P_F_QC = mean(P_F_QC),
                                                                            NEE_VUT_REF = sum(NEE_VUT_REF),
                                                                            NEE_VUT_REF_QC = mean(NEE_VUT_REF_QC),
                                                                            RECO_DT_VUT_REF = sum(RECO_DT_VUT_REF),
                                                                            GPP_DT_VUT_REF = sum(GPP_DT_VUT_REF),
                                                                            RECO_NT_VUT_REF = sum(RECO_NT_VUT_REF),
                                                                            GPP_NT_VUT_REF = sum(GPP_NT_VUT_REF))
write_csv(fluxnet.permonth, "fluxnetpermonth.csv")

####extract list of sites and dates covered###
fluxnet.permonth$ts <- paste(fluxnet.permonth$year, fluxnet.permonth$month)
sites <- subset(fluxnet.permonth, select = c(site_id,ts))
sites.datescovered <- sites %>% group_by(site_id) %>% dplyr::summarise(start_date = min(ts),
                                                                         end_date = max(ts))
#double checking that function above worked
checkdates <- sites %>% arrange(site_id, ts)

