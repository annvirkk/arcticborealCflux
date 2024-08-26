#Combining data from major data repositories (ICOS, Fluxnet2015, Ameriflux, FluxnetCH4)
library(tidyr)
library(janitor)
library(dplyr)
library(readr)
library(gdata)
library(DataCombine)
library(data.table)
library(rquery)
###ICOS####-------------------------------------------------------------------------------------------------
setwd("/Users/iwargowsky/Desktop/ICOS")
ICOSdat <- read.csv("ICOSdatapermonth.csv")
ICOSdat$extraction_source_co2 <- paste("ICOS",ICOSdat$source)
ICOSdat$source <- NULL
#rename variables to match ABCv2
colnames(ICOSdat)  
ICOSdat.renamed <- ICOSdat %>% dplyr::rename("site_reference"="site_id",
                                             "nee"="NEE_CUT_REF",
                                             "gpp"="GPP_CUT_REF",
                                             "reco"="RECO_CUT_REF",
                                             "ppfd"="PPFD_IN",
                                             "tsoil_surface"="TS_F_MDS_1",
                                             "soil_moisture"="SWC_F_MDS_1",
                                             "tair"="TA_F_MDS",
                                             "precip"="P_F",
                                             "citation_co2"= "citation")

###FLUXNET2015####--------------------------------------------------------------------
setwd("/Users/iwargowsky/Desktop/Fluxnet2015")
fluxnetdat <- read.csv("fluxnetpermonth.csv")
fluxnetdat$extraction_source_co2 <- "Fluxnet2015"
#rename variables to match ABCv2
colnames(fluxnetdat)
fluxnetdat.renamed <- fluxnetdat %>% dplyr::rename("site_reference"="site_id",
                                                   "nee"="NEE_CUT_REF",
                                                   "gpp"="GPP_CUT_REF",
                                                   "reco"="RECO_CUT_REF",
                                                   "ppfd"="PPFD_IN",
                                                   "tsoil_surface"="TS_F_MDS_1",
                                                   "soil_moisture"="SWC_F_MDS_1",
                                                   "tair"="TA_F",
                                                   "precip"="P_F",
                                                   "gap_fill_perc_nee"= "gap_fill_perc",
                                                   "citation_co2"= "citation")

#merging icos and fluxnet
icos.fluxnet.wdupes <- bind_rows(ICOSdat.renamed, fluxnetdat.renamed)

#check if there are number of  duplicates
dupes<- icos.fluxnet.wdupes %>% get_dupes(site_reference, year, month, partition_method) 
setwd("/Users/iwargowsky/Desktop/ABCFlux v2")
write.csv(dupes, "icos.fluxnet.dupes.csv")

#remove duplicates
icos.fluxnet <- icos.fluxnet.wdupes %>%  
  arrange(extraction_source_co2) %>%  #give preference to fluxnet data
  distinct(site_reference, year, month, partition_method, .keep_all = TRUE)
##MAKE SURE #newdf= #df.wdupes - (#dupes/2)

###AMERIFLUX#####-----------------------------------------------------------------
setwd("/Users/iwargowsky/Desktop/Ameriflux")
amerifluxdat <- read_csv("ameriflux.fluxnetALL.csv")

#rename variables to match ABCv2
colnames(amerifluxdat)
amerifluxdat.renamed <- amerifluxdat %>% dplyr::rename("site_reference"="site_id",
                                                       "nee"="NEE_CUT_REF",
                                                       "gpp"="GPP_CUT_REF",
                                                       "reco"="RECO_CUT_REF",
                                                       "ppfd"="PPFD_IN",
                                                       "tsoil_surface"="TS_F_MDS_1",
                                                       "soil_moisture"="SWC_F_MDS_1",
                                                       "tair"="TA_F",
                                                       "precip"="P_F",
                                                       "gap_fill_perc_nee"= "gap_fill_perc",
                                                       "citation_co2"= "citation",
                                                       "extraction_source_co2"= "source")

amerifluxdat.renamed$year <- as.integer(amerifluxdat.renamed$year)
amerifluxdat.renamed$month <- as.integer(amerifluxdat.renamed$month)

#merging ameriflux with what we have so far
icos.fluxnet.AMF.wdupes <- bind_rows(icos.fluxnet, amerifluxdat.renamed)

#check if there are duplicates
dupes<- icos.fluxnet.AMF.wdupes %>% get_dupes(site_reference, year, month, partition_method)  
setwd("/Users/iwargowsky/Desktop/ABCFlux v2")
write.csv(dupes, "icos.fluxnet.AMF.dupes.csv")

#remove duplicates
icos.fluxnet.AMF <- icos.fluxnet.AMF.wdupes %>%  
  arrange((extraction_source_co2)) %>%  
  distinct(site_reference, year, month, partition_method, .keep_all = TRUE)
##MAKE SURE #newdf= #df.wdupes - (#dupes/2)


###EUROFLUX#####-----------------------------------------------------------------
setwd("/Users/iwargowsky/Desktop/Euroflux")
euroflux <- read_csv("eurofluxdata.csv")
euroflux$extraction_source_co2 <- "European Fluxes Database Cluster"
euroflux$year <- as.integer(euroflux$year)
euroflux$month <- as.integer(euroflux$month)
euroflux.renamed <- euroflux %>% dplyr::rename("gap_fill_perc_nee"= "gap_fill_perc")

#merging with what we have so far
icos.fluxnet.AMF.euro.wdupes <- bind_rows(icos.fluxnet.AMF, euroflux.renamed)

#check if there are duplicates
dupes<- icos.fluxnet.AMF.euro.wdupes %>% get_dupes(site_reference, year, month, partition_method)  
#no duplicates
icos.fluxnet.AMF.euro <-  icos.fluxnet.AMF.euro.wdupes
##MAKE SURE #newdf= #df.wdupes - (#dupes/2)

###FLUXNET CH4#####---------------------------------------------------------------
setwd("/Users/iwargowsky/Desktop/Fluxnet-CH4")
CH4fluxnet <- read.csv("CH4fluxnetpermonth.csv")
CH4fluxnet$extraction_source_co2 <- "Fluxnet-CH4"
CH4fluxnet$extraction_source_ch4 <- "Fluxnet-CH4"
#rename variables to match ABCv2
colnames(CH4fluxnet)
CH4fluxnet.renamed <- CH4fluxnet %>% dplyr::rename("site_reference"="site_id",
                                                   "ch4_flux_total"="FCH4_F",
                                                   "tair"="TA_F","precip"="P_F",
                                                   "snow_depth"="D_SNOW_F",
                                                   "tsoil_surface"="TS_1",
                                                   "soil_moisture"="SWC_F",
                                                   "ppfd"="PPFD_IN_F",
                                                   "gpp"="GPP","reco"="RECO",
                                                   "country"="COUNTRY",
                                                   "nee"= "NEE_F",
                                                   "data_usage_ch4"="FLUXNET.CH4_DATA_POLICY",
                                                   "citation_ch4"= "citation",
                                                   "data_version_ch4"= "data_version")
CH4fluxnet.renamed$data_usage<- CH4fluxnet.renamed$data_usage_ch4
CH4fluxnet.renamed$citation_co2 <- CH4fluxnet.renamed$citation_ch4
CH4fluxnet.renamed$data_version <- CH4fluxnet.renamed$data_version_ch4
#have to convert month column from character
CH4fluxnet.renamed$year <- as.integer(CH4fluxnet.renamed$year)
CH4fluxnet.renamed$month <- as.integer(CH4fluxnet.renamed$month)
#some sites have ch4 flux along with nee, gpp, and reco so we'll separate ch4 fluxes and merge them with our df first
CH4fluxnet.renamedCH4 <- CH4fluxnet.renamed %>%
  dplyr::select (year, month, site_reference, ch4_flux_total, data_usage_ch4, data_version_ch4, 
          citation_ch4, extraction_source_ch4)
#merge methane fluxes with dataframe
icos.fluxnet.AMF.euro.CH4 <- left_join(icos.fluxnet.AMF.euro, CH4fluxnet.renamedCH4,
                                   by= c('site_reference', 'year', 'month'))
#merge original CH4 df with what we have so far, doing this to catch any nee, reco, or gpp observations that are present in Fluxnet-CH4 but not in the other repositories
icos.fluxnet.AMF.euro.CH4.wdupes <- bind_rows(icos.fluxnet.AMF.euro.CH4, CH4fluxnet.renamed)
#check if there are duplicates
dupes<- icos.fluxnet.AMF.euro.CH4.wdupes %>% get_dupes(site_reference, year, month, partition_method)
setwd("/Users/iwargowsky/Desktop/ABCFlux v2")
write.csv(dupes, "icos.fluxnet.AMF.euro.CH4.dupes.csv")
#remove duplicates
icos.fluxnet.AMF.euro.CH4 <- icos.fluxnet.AMF.euro.CH4.wdupes %>%
  distinct(site_reference, year, month, partition_method, .keep_all = TRUE)
##MAKE SURE #newdf= #df.wdupes - (#dupes/2)

#merge ch4_flux_total columns (only methane flux not coming from fluxnet-ch4 is for SE-Sto which is not in fluxnet-ch4)
icos.fluxnet.AMF.euro.CH4 <- icos.fluxnet.AMF.euro.CH4 %>%
  unite("ch4_flux_total", c(ch4_flux_total, ch4_flux_total.x, ch4_flux_total.y), na.rm= TRUE, remove= TRUE)%>% 
  unite("citation_ch4", c(citation_ch4, citation_ch4.y , citation_ch4.x), na.rm= TRUE, remove= TRUE) %>%
  unite("extraction_source_ch4", c(extraction_source_ch4, extraction_source_ch4.y, extraction_source_ch4.x), na.rm= TRUE, remove= TRUE)

###AMERIFLUX BASE#####-----------------------------------------------------------------
setwd("/Users/iwargowsky/Desktop/ABCFlux v2")
base <- read_csv("ameriflux.base.csv")
colnames(base)
base.renamed <- base  %>% dplyr::rename("site_reference"="site_id",
                                        "nee"="NEE",
                                        "gpp"="GPP",
                                        "reco"="RECO",
                                        "ppfd"="PPFD",
                                        "tsoil_surface"="TS",
                                        "ch4_flux_total"="FCH4",
                                        "soil_moisture"="SWC",
                                        "snow_depth"= "D_SNOW",
                                        "tair"="TA",
                                        "precip"="P")
#merging
base.renamed$year <- as.integer(base.renamed$year)
base.renamed$month <- as.integer(base.renamed$month)
#merge
icos.fluxnet.AMF.euro.CH4.base.wdupes <- rbindlist(list(icos.fluxnet.AMF.euro.CH4, base.renamed), fill = TRUE)
#check if there are number of  duplicates
dupes<- icos.fluxnet.AMF.euro.CH4.base.wdupes  %>% get_dupes(site_reference, year, month) 
setwd("/Users/iwargowsky/Desktop/ABCFlux v2") 
#write.csv(dupes.base , "icos.fluxnet.AMF.euro.CH4.base.dupes.csv")

#Identify duplicates by dupes= 3 and source= Ameriflux BASE
to.remove <- dupes %>% dplyr::filter(extraction_source_co2 %in% 'AAmeriflux BASE' | 
                                       extraction_source_ch4 %in% 'AAmeriflux BASE')
icos.fluxnet.AMF.euro.CH4.base <- anti_join(icos.fluxnet.AMF.euro.CH4.base.wdupes, to.remove, 
                                            by = c("year", "month", "site_reference", "extraction_source_co2",
                                                   "extraction_source_ch4"))

### ASIAFLUX #####-----------------------------------------------------------------
setwd("/Users/iwargowsky/Desktop/ABCFlux v2")
asiaflux <- read_csv("asia.monthly.csv")
asiaflux$year <- as.integer(asiaflux$year)
asiaflux$month <- as.integer(asiaflux$month)
asiaflux.renamed <- asiaflux  %>% dplyr::rename("site_reference"= "site_id",
                                                "extraction_source_co2"= "extraction_source")

icos.fluxnet.AMF.euro.CH4.base.asia <- icos.fluxnet.AMF.euro.CH4.base %>%
  full_join(asiaflux.renamed)

### ARCTIC DATA CENTER #####-----------------------------------------------------------------
setwd("/Users/iwargowsky/Desktop/ABCFlux v2")
ADC <- read_csv("ADC.ec.csv")
ADC$year <- as.integer(ADC$year)
ADC$month <- as.integer(ADC$month)
colnames(ADC)
ADC.renamed <- ADC  %>% dplyr::rename("gap_fill_perc_nee"= "percent_na_nee",
                                      "gap_fill_perc_ch4"= "percent_na_ch4")
icos.fluxnet.AMF.euro.CH4.base.asia.ADC.wdupes <- rbindlist(list(icos.fluxnet.AMF.euro.CH4.base.asia, ADC.renamed), fill = TRUE) 

#check if there are duplicates
dupes<- icos.fluxnet.AMF.euro.CH4.base.asia.ADC.wdupes  %>% get_dupes(site_reference, year, month) 

#Identify ADC duplicates and removed since these are not gapfilled 
to.remove <- dupes %>% dplyr::filter(extraction_source_co2 %in% 'Arctic Data Center' | 
                                extraction_source_ch4 %in% 'Arctic Data Center')
icos.fluxnet.AMF.euro.CH4.base.asia.ADC <- anti_join(icos.fluxnet.AMF.euro.CH4.base.asia.ADC.wdupes, to.remove, 
                                            by = c("year", "month", "site_reference", "extraction_source_co2",
                                                   "extraction_source_ch4"))


### Fixing some columns  #####--------------------------------------------------------------------------------------------
#fix column names
colnames(icos.fluxnet.AMF.euro.CH4.base.asia.ADC)
icos.fluxnet.AMF.euro.CH4.base.asia.ADC <- icos.fluxnet.AMF.euro.CH4.base.asia.ADC %>% 
  mutate(gap_fill= paste("CO2:", gap_fill, "CH4:", gap_fill_ch4, sep = " ")) %>%
  mutate(data_usage= paste("CO2:", data_usage, "CH4:", data_usage_ch4, sep = " ")) %>%
  mutate(data_version= paste("CO2:", data_version, "CH4:", data_version_ch4, sep = " ")) %>%
  mutate(gap_fill_ch4= NULL, data_usage_ch4= NULL, data_version_ch4= NULL)

#replacing extraction source names to be more clear where data came from
icos.fluxnet.AMF.euro.CH4.base.asia.ADC <- icos.fluxnet.AMF.euro.CH4.base.asia.ADC %>% 
  mutate(extraction_source_co2= ifelse(extraction_source_co2== "amerifluxdf","Ameriflux", extraction_source_co2))%>%
  mutate(extraction_source_co2= ifelse(extraction_source_co2== "betaamerifluxdf", "Ameriflux- beta ONEFLUX", extraction_source_co2))%>%
  mutate(extraction_source_co2= ifelse(extraction_source_co2== "ICOS icosdat","ICOS Ecosystem Thematic Centre", extraction_source_co2))%>%
  mutate(extraction_source_co2= ifelse(extraction_source_co2== "ICOS wwdat","ICOS Warm Winters", extraction_source_co2))%>%
  mutate(extraction_source_co2= ifelse(extraction_source_co2== "Arctic Data Center","AArctic Data Center", extraction_source_co2))%>% # used for merging purposes
  mutate(extraction_source_ch4= ifelse(extraction_source_ch4== "amerifluxdf","Ameriflux", extraction_source_ch4))%>%
  mutate(extraction_source_ch4= ifelse(extraction_source_ch4== "betaamerifluxdf", "Ameriflux- beta ONEFLUX", extraction_source_ch4))%>%
  mutate(extraction_source_ch4= ifelse(extraction_source_ch4== "ICOS icosdat","ICOS Ecosystem Thematic Centre", extraction_source_ch4))%>%
  mutate(extraction_source_ch4= ifelse(extraction_source_ch4== "ICOS wwdat","ICOS Warm Winters", extraction_source_ch4)) %>%
  mutate(extraction_source_ch4= ifelse(extraction_source_ch4== "Arctic Data Center","AArctic Data Center", extraction_source_ch4))# used for merging purposes





#check if there are duplicates
x<- icos.fluxnet.AMF.euro.CH4.base.asia.ADC  %>% get_dupes(site_reference, year, month, partition_method) 


#####Thaw depth and water table data from Arctic Data Center for Barrow, Aquasuk, and Ivotuk--------------------
setwd("/Users/iwargowsky/Desktop/arcticdatacenter/downloads")
tdwt <- read_csv("tdwt.csv")#adding year and month columns
tdwt <- tdwt %>% mutate(year= as.integer(year(as.Date(date, format= "%m/%d/%Y"))), 
                        month= as.character(month(as.Date(date, format= "%m/%d/%Y"))) ) 

tdwt <- tdwt %>% mutate(water_table= ifelse(site %in% c("US-Bes", "US-Beo", "US-Atq") & water_table < 0, NA, water_table ))

tdwt.permonth <- tdwt %>% group_by(year, month, site) %>%
  dplyr::summarise(thaw_depth = mean(thaw_depth, na.rm = TRUE) *-1,
            water_table_depth= mean(water_table, na.rm = TRUE)*-1)%>%
  dplyr::rename("site_reference"= "site")

#merge with df
tdwt.permonth$year <- as.integer(tdwt.permonth$year)
tdwt.permonth$month <- as.integer(tdwt.permonth$month)
tdwt.permonth$notes <- "water_table_depth and thaw_depth data from doi:10.18739/A26H4CQ8J"
icos.fluxnet.AMF.euro.CH4.base.asia.ADC <-full_join(icos.fluxnet.AMF.euro.CH4.base.asia.ADC, tdwt.permonth,
                                        by = c("year", "month", "site_reference"))
icos.fluxnet.AMF.euro.CH4.base.asia.ADC <- icos.fluxnet.AMF.euro.CH4.base.asia.ADC %>%
  unite("notes", c( notes.y , notes.x), na.rm= TRUE, remove= TRUE)  %>%
  unite("thaw_depth", c( thaw_depth.y , thaw_depth.x), na.rm= TRUE, remove= TRUE) 


#save
#setwd("/Users/iwargowsky/Desktop/ABCFlux v2")
#write_csv(icos.fluxnet.AMF.euro.CH4.base.asia, "towerrepositorydata.csv")

#towersites <- as.data.frame(unique(icos.fluxnet.AMF.euro.CH4.base.asia$site_reference) )
#write_csv(towersites, "repository.towersites.csv")



#########adding static variables-------------------------------------------------------------
setwd("/Users/iwargowsky/Desktop/ABCFlux v2")
static <- read_csv("static.towersites.csv")
#join to fill NAs
icos.fluxnet.AMF.euro.CH4.base.asia.ADC.static <- natural_join(icos.fluxnet.AMF.euro.CH4.base.asia.ADC, static,
                                          by= "site_reference", jointype= "FULL")



# x<- anti_join(icos.fluxnet.AMF.euro.CH4.base.asia.static, icos.fluxnet.AMF.euro.CH4.base.asia,
# by= c("site_reference", "year", "month"))


##Unifying naming conventions
#US-Bes and US-Beo (changing from fluxnet-ch4 naming)
# icos.fluxnet.AMF.euro.CH4.base.asia.static<- icos.fluxnet.AMF.euro.CH4.base.asia.static %>%
#   mutate(site_name= ifelse(site_reference== "US-Beo", "Barrow-BEO", site_name)) %>%
#   mutate(site_name= ifelse(site_reference== "US-Bes", "Barrow-BES", site_name))
# #SE-St1 (changing from fluxnet-ch4 naming)
# icos.fluxnet.AMF.euro.CH4.base.asia.static <- icos.fluxnet.AMF.euro.CH4.base.asia.static %>%
#   mutate(site_name= ifelse(site_reference== "SE-St1", "Stordalen - Fen", site_name))
# #RU-Che and RU-Ch2
# icos.fluxnet.AMF.euro.CH4.base.asia.static <- icos.fluxnet.AMF.euro.CH4.base.asia.static %>%
#   mutate(site_name= ifelse(site_reference== "RU-Che", "Cherski", site_name))%>%
#   mutate(site_name= ifelse(site_reference== "RU-Ch2", "Cherski reference", site_name))
# #US-EML
# icos.fluxnet.AMF.euro.CH4.base.asia.static <- icos.fluxnet.AMF.euro.CH4.base.asia.static %>%
#   mutate(site_name= ifelse(site_reference== "US-EML", "Eight Mile Lake", site_name))
# #Kaamanen
# icos.fluxnet.AMF.euro.CH4.base.asia.static <- icos.fluxnet.AMF.euro.CH4.base.asia.static %>%
#   mutate(site_name= ifelse(site_reference== "FI-Kaa", "Kaamanen", site_name))
#Svalbard and Jan Mayen 
icos.fluxnet.AMF.euro.CH4.base.asia.ADC.static <- icos.fluxnet.AMF.euro.CH4.base.asia.ADC.static %>%
  mutate(country= ifelse(country== "Svalbard and Jan Mayen", "Norway", country))
#United States
icos.fluxnet.AMF.euro.CH4.base.asia.ADC.static <- icos.fluxnet.AMF.euro.CH4.base.asia.ADC.static %>%
  mutate(country= ifelse(country== "United States", "USA", country))

####save###----------------------------------------------------------------------
icos.fluxnet.AMF.euro.CH4.base.asia.ADC.static$dataentry_person <- "Wargowsky"
icos.fluxnet.AMF.euro.CH4.base.asia.ADC.static$flux_method <- "EC"
setwd("/Users/iwargowsky/Desktop/ABCFlux v2")
write_csv(icos.fluxnet.AMF.euro.CH4.base.asia.ADC.static, "towerrepositorydata.static.csv")



colnames(icos.fluxnet.AMF.euro.CH4.base.asia.ADC.static)



####checking things for Anna 8/18/23#### 
v2.annual <- icos.fluxnet.AMF.euro.CH4  %>%
  filter(site_reference== "CA-Gro"|site_reference== "FI-Kaa") %>%
  filter(partition_method== "Reichstein") %>%
  group_by(site_reference, year)%>%  
  dplyr::summarise(n=n(), annee=sum(nee, na.rm=TRUE),
                   angpp=sum(gpp, na.rm=TRUE),
                   anreco=sum(reco, na.rm=TRUE),
                   gap_fill_perc= mean(gap_fill_perc, na.rm = TRUE)) %>% filter(n >= 12)

v2.monthly <- icos.fluxnet.AMF.euro.CH4  %>%
  filter(site_reference== "CA-Gro"|site_reference== "FI-Kaa") %>%
  filter(partition_method== "Reichstein")


setwd("/Users/iwargowsky/Desktop/ABCFlux v2")
write.csv(v2.monthly, "CA-Gro.FI-Kaa.monthly.csv")
write.csv(v2.annual, "CA-Gro.FI-Kaa.annual.csv")

