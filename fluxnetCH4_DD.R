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
CH4fluxnet <- CH4fluxnetdf %>% dplyr::select(site_id, TIMESTAMP, FCH4_F_ANNOPTLM, TA_F, 
                                             P_F, D_SNOW_F, TS_1, SWC_F, GPP_NT, GPP_DT,
                                             RECO_NT, RECO_DT, PPFD_IN_F, NEE_F_ANNOPTLM,
                                             data_version, FCH4_F_ANNOPTLM_QC)
# ###  Adding gap_fill_perc####
# CH4fluxnet <- CH4fluxnet %>% mutate(gap_fill_perc_ch4= case_when(FCH4_F_ANNOPTLM_QC %in% 3 ~100, 
#                                                              FCH4_F_ANNOPTLM_QC %in% 1 ~0))

# 5/21/24 no gaps longer than 2 months
CH4fluxnet <- CH4fluxnet %>%
  mutate(FCH4_F_ANNOPTLM= ifelse(FCH4_F_ANNOPTLM_QC %in% 3, NA, FCH4_F_ANNOPTLM))



##some extreme partitioned fluxes that we want to remove
CH4fluxnet <- CH4fluxnet %>%
  mutate(GPP_DT= ifelse(GPP_DT> 1000, NA, GPP_DT)) %>%
  mutate(GPP_NT= ifelse(GPP_NT> 1000, NA, GPP_NT)) %>%
  mutate(RECO_DT= ifelse(RECO_DT> 1000, NA, RECO_DT)) %>%
  mutate(RECO_NT= ifelse(RECO_NT> 1000, NA, RECO_NT))



#add month and year columns
CH4fluxnet$year <- substr(CH4fluxnet$TIMESTAMP,1,4)
CH4fluxnet$month <- substr(CH4fluxnet$TIMESTAMP,5,6)
#get cumulative NEE, GPP, and RECO for each month
CH4fluxnet.permonth<- CH4fluxnet %>% group_by(year, month, site_id, data_version) %>% 
  dplyr::summarise(percent_na_ch4 = (sum(is.na(FCH4_F_ANNOPTLM))/n()*100),
                   percent_na_nee = (sum(is.na(NEE_F_ANNOPTLM))/n()*100),
                   percent_na_gpp.dt = (sum(is.na(GPP_DT))/n()*100),
                   percent_na_reco.dt = (sum(is.na(RECO_DT))/n()*100),
                   percent_na_gpp.nt = (sum(is.na(GPP_NT))/n()*100),
                   percent_na_reco.nt = (sum(is.na(RECO_NT))/n()*100),
                   FCH4_F = mean(FCH4_F_ANNOPTLM, na.rm= TRUE),
                   TA_F = mean(TA_F, na.rm= TRUE),
                   P_F = sum(P_F, na.rm= TRUE),
                   D_SNOW_F = mean(D_SNOW_F, na.rm= TRUE),
                   TS_1 = mean(TS_1, na.rm= TRUE),
                   SWC_F= mean(SWC_F, na.rm= TRUE),
                   PPFD_IN_F = mean(PPFD_IN_F, na.rm= TRUE),
                   GPP_NT= mean(GPP_NT, na.rm=TRUE), GPP_DT=mean(GPP_DT, na.rm= TRUE),
                   RECO_NT= mean(RECO_NT, na.rm= TRUE), RECO_DT=mean(RECO_DT, na.rm= TRUE),
                   NEE_F= mean(NEE_F_ANNOPTLM, na.rm= TRUE))
#remove rows with more than 10% data missing in a month
CH4fluxnet.permonth <- CH4fluxnet.permonth %>% 
  mutate(FCH4_F = ifelse(percent_na_ch4> 10, NA, FCH4_F)) %>%
  mutate(NEE_F = ifelse(percent_na_nee> 10, NA, NEE_F)) %>%
  mutate(GPP_NT = ifelse(percent_na_gpp.nt> 10, NA, GPP_NT)) %>%
  mutate(GPP_DT = ifelse(percent_na_gpp.dt> 10, NA, GPP_DT)) %>%
  mutate(RECO_NT = ifelse(percent_na_reco.nt> 10, NA, RECO_NT)) %>%
  mutate(RECO_DT = ifelse(percent_na_reco.dt> 10, NA, RECO_DT))

CH4fluxnet.permonth <- CH4fluxnet.permonth %>% select(-c(percent_na_ch4, percent_na_nee, 
                                                        percent_na_gpp.nt, percent_na_gpp.dt,
                                                        percent_na_reco.nt, percent_na_reco.dt))

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
CH4fluxnet.permonth$ts <- as.yearmon(paste(CH4fluxnet.permonth$year, CH4fluxnet.permonth$month, sep = "-"))
CH4fluxnet.permonth$NEE_F <- CH4fluxnet.permonth$NEE_F*1.0368*days_in_month(CH4fluxnet.permonth$ts)
CH4fluxnet.permonth$GPP <- CH4fluxnet.permonth$GPP*1.0368 * -1*days_in_month(CH4fluxnet.permonth$ts)
CH4fluxnet.permonth$RECO <- CH4fluxnet.permonth$RECO*1.0368*days_in_month(CH4fluxnet.permonth$ts)
CH4fluxnet.permonth$FCH4_F <- CH4fluxnet.permonth$FCH4_F*0.0010368*days_in_month(CH4fluxnet.permonth$ts)
CH4fluxnet.permonth$ts <- NULL
  
#Adding in some metadata####-----------------------------------------------------
setwd("/Users/iwargowsky/Desktop/Fluxnet-CH4")
meta <- read_csv("FLX_AA-Flx_CH4-META_20201112135337801132.csv")
#filter for sites of interest
names <- unique(CH4fluxnet.permonth$site_id)
meta <- meta %>% dplyr::filter(SITE_ID %in% names)
colnames(meta)
#keep relevant columns
meta.2 <- meta %>% select(SITE_ID, COUNTRY, SOIL_TEMP_PROBE_DEPTHS,
                          MOSS_BROWN, MOSS_SPHAGNUM, DOM_VEG, "FLUXNET-CH4_DATA_POLICY")
#convert columns to match variables in ABCflux v2
meta.2$sphagnum_cover[meta.2$MOSS_SPHAGNUM == "1"] <- "Present"
meta.2$other_moss_cover[meta.2$MOSS_BROWN == "1"] <- "Present"
meta.2$sphagnum_cover[meta.2$DOM_VEG == "moss_sphagnum"] <- "Dominant"
meta.2$other_moss_cover[meta.2$DOM_VEG == "moss_brown"] <- "Dominant"

colnames(meta.2)
meta.3 <- meta.2 %>% select(SITE_ID, COUNTRY, sphagnum_cover, other_moss_cover, "FLUXNET-CH4_DATA_POLICY")

#merge flux df and meta data
meta.3<- meta.3 %>% dplyr::rename(site_id= SITE_ID)
CH4fluxnetALL <- left_join(CH4fluxnet.permonth, meta.3)
#fix data usage to match ABCFlux v2
CH4fluxnetALL <- CH4fluxnetALL %>%
  mutate('FLUXNET-CH4_DATA_POLICY'= ifelse(`FLUXNET-CH4_DATA_POLICY`=="CCBY4.0","Tier 1",`FLUXNET-CH4_DATA_POLICY`),
         'FLUXNET-CH4_DATA_POLICY'= ifelse(`FLUXNET-CH4_DATA_POLICY`=="TIER2","Tier 2",`FLUXNET-CH4_DATA_POLICY`))
#adding FLUXNET-CH4 citation
CH4fluxnetALL$citation <- "https://doi.org/10.5194/essd-13-3607-2021"
#adding gap fill method
CH4fluxnetALL$gap_fill <- "ANNOPTLM"

#-----------------------------------------------------------------------------------
setwd("/Users/iwargowsky/Desktop/Fluxnet-CH4")
write_csv(CH4fluxnetALL, "CH4fluxnetpermonth.csv")

x <- CH4fluxnet.permonth %>% dplyr::filter(percent_na_ch4 ==100)

CH4fluxnetALL$ts <- as.yearmon(paste(CH4fluxnetALL$year, CH4fluxnetALL$month,sep = '-')) #add timestamp

ggplot(data = subset(CH4fluxnetALL,CH4fluxnetALL$site_id=='US-EML'))+theme_bw()+ggtitle('US-EML')+
  geom_hline(yintercept = 0)+
  geom_line(aes(ts,FCH4_F, group= site_id))+
  geom_point(aes(ts,FCH4_F, color= percent_na_ch4))+
  scale_color_gradient(low="blue", high="green")



ggplot(CH4fluxnetALL, aes(x = ts, y = GPP, color= percent_na_gpp)) +
  geom_point()+
  geom_smooth(method = "lm")+
  labs( x = "Date",
        y = "GPP g C m-2 month-1") +
  theme_minimal()

ggplot(CH4fluxnetALL, aes(x = ts, y = RECO, color= percent_na_reco)) +
  geom_point()+
  geom_smooth(method = "lm")+
  labs( x = "Date",
        y = "RECO g C m-2 month-1") +
  theme_minimal()
















####extract list of sites and dates covered###
CH4fluxnet.permonth$ts <- paste(CH4fluxnet.permonth$year, CH4fluxnet.permonth$month)
sites <- subset(CH4fluxnet.permonth, select = c(site_id,ts))
sites.datescovered <- sites %>% group_by(site_id) %>% dplyr::summarise(start_date = min(ts),
                                                                       end_date = max(ts))
#double checking that function above worked
checkdates <- sites %>% arrange(site_id, ts)
