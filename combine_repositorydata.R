#Combining data from major data repositories (ICOS, Fluxnet2015, Ameriflux, FluxnetCH4)
library(tidyr)
library(janitor)
library(dplyr)
library(readr)
library(gdata)
library(DataCombine)
library(rquery)
###ICOS####-------------------------------------------------------------------------------------------------
setwd("/Users/iwargowsky/Desktop/ICOS")
ICOSdat <- read.csv("ICOSdatapermonth.csv")
ICOSdat$extraction_source <- paste("ICOS",ICOSdat$source)
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
                                             "precip"="P_F")

###FLUXNET2015####--------------------------------------------------------------------
setwd("/Users/iwargowsky/Desktop/Fluxnet2015")
fluxnetdat <- read.csv("fluxnetpermonth.csv")
fluxnetdat$extraction_source <- "Fluxnet2015"
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
                                                   "precip"="P_F")

#merging icos and fluxnet
icos.fluxnet.wdupes <- bind_rows(ICOSdat.renamed, fluxnetdat.renamed)

#check if there are number of  duplicates
dupes<- icos.fluxnet.wdupes %>% get_dupes(site_reference, year, month, partition_method)  
#remove duplicates
icos.fluxnet <- icos.fluxnet.wdupes %>%  
  arrange(extraction_source) %>%  #give preference to fluxnet data since it has more variables 
  distinct(site_reference, year, month, partition_method, .keep_all = TRUE)
##MAKE SURE #newdf= #df.wdupes - (#dupes/2)

###AMERIFLUX#####-----------------------------------------------------------------
setwd("/Users/iwargowsky/Desktop/Ameriflux")
amerifluxdat <- read_csv("ameriflux.fluxnetALL.csv")

#rename variables to match ABCv2
colnames(amerifluxdat)
amerifluxdat.renamed <- amerifluxdat %>% dplyr::rename("extraction_source"= "source",
                                                       "site_reference"="site_id",
                                                       "nee"="NEE_CUT_REF",
                                                       "gpp"="GPP_CUT_REF",
                                                       "reco"="RECO_CUT_REF",
                                                       "ppfd"="PPFD_IN",
                                                       "tsoil_surface"="TS_F_MDS_1",
                                                       "soil_moisture"="SWC_F_MDS_1",
                                                       "tair"="TA_F",
                                                       "precip"="P_F")

#merging ameriflux with what we have so far
amerifluxdat.renamed$month <- as.numeric(amerifluxdat.renamed$month)
icos.fluxnet.AMF.wdupes <- bind_rows(icos.fluxnet, amerifluxdat.renamed)

#check if there are duplicates
dupes<- icos.fluxnet.AMF.wdupes %>% get_dupes(site_reference, year, month, partition_method)  
#remove duplicates
icos.fluxnet.AMF <- icos.fluxnet.AMF.wdupes %>% 
  distinct(site_reference, year, month, partition_method, .keep_all = TRUE)
##MAKE SURE #newdf= #df.wdupes - (#dupes/2)

###EUROFLUX#####-----------------------------------------------------------------
setwd("/Users/iwargowsky/Desktop/Euroflux")
euroflux <- read_csv("eurofluxdata.csv")
euroflux$extraction_source <- "European Fluxes Database Cluster"
#merging with what we have so far
icos.fluxnet.AMF.euro.wdupes <- bind_rows(icos.fluxnet.AMF, euroflux)

#check if there are duplicates
dupes<- icos.fluxnet.AMF.euro.wdupes %>% get_dupes(site_reference, year, month, partition_method)  
#no duplicates
icos.fluxnet.AMF.euro <-  icos.fluxnet.AMF.euro.wdupes
##MAKE SURE #newdf= #df.wdupes - (#dupes/2)

###FLUXNET CH4#####---------------------------------------------------------------
setwd("/Users/iwargowsky/Desktop/Fluxnet-CH4")
CH4fluxnet <- read.csv("CH4fluxnetpermonth.csv")
CH4fluxnet$extraction_source <- "Fluxnet-CH4"

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
                                                   "latitude"= "LAT",
                                                   "longitude"="LON",
                                                   "site_name"="SITE_NAME",
                                                   "nee"= "NEE_F",
                                                   "data_usage_ch4"="FLUXNET.CH4_DATA_POLICY")
#have to convert month column from character
CH4fluxnet.renamed$month <- as.numeric(CH4fluxnet.renamed$month)
#some sites have ch4 flux along with nee, gpp, and reco so we'll separate ch4 fluxes and merge them with our df first
CH4fluxnet.renamedCH4 <- CH4fluxnet.renamed %>% select (year, month, site_reference, ch4_flux_total, data_usage_ch4) %>%
  mutate(notes= "Methane flux from Fluxnet-CH4") #add note about where methane flux came from 
#remove any rows that don't have CH4 flux data
CH4fluxnet.renamedCH4 <- CH4fluxnet.renamedCH4 %>% filter(if_all("ch4_flux_total", ~ !is.na(.)))
#merge methane fluxes with dataframe
icos.fluxnet.AMF.euro.CH4 <- left_join(icos.fluxnet.AMF.euro, CH4fluxnet.renamedCH4,
                                   by= c('site_reference', 'year', 'month'))
#merge original CH4 df with what we have so far, doing this to catch any nee, reco, or gpp observations that are present in Fluxnet-CH4 but not in the other repositories
icos.fluxnet.AMF.euro.CH4.wdupes <- bind_rows(icos.fluxnet.AMF.euro.CH4, CH4fluxnet.renamed)
#check if there are duplicates
dupes<- icos.fluxnet.AMF.euro.CH4.wdupes %>% get_dupes(site_reference, year, month, partition_method) 
#remove duplicates
icos.fluxnet.AMF.euro.CH4 <- icos.fluxnet.AMF.euro.CH4.wdupes %>% 
  distinct(site_reference, year, month, partition_method, .keep_all = TRUE)
##MAKE SURE #newdf= #df.wdupes - (#dupes/2)
#merging data usage columns 
icos.fluxnet.AMF.euro.CH4 <- icos.fluxnet.AMF.euro.CH4 %>%
  mutate(data_usage= paste(data_usage,"CH4:",data_usage_ch4)) %>%
  mutate(data_usage_ch4= NULL) 
unique(icos.fluxnet.AMF.euro.CH4$data_usage)
icos.fluxnet.AMF.euro.CH4 <- icos.fluxnet.AMF.euro.CH4 %>% 
  mutate(data_usage= ifelse(data_usage== "NA CH4: NA", NA,data_usage),
         data_usage= ifelse(data_usage== "NA CH4: Tier 1", "Tier 1",data_usage),
         data_usage= ifelse(data_usage== "NA CH4: Tier 2", "Tier 2",data_usage),
         data_usage= ifelse(data_usage== "Tier 1 CH4: NA", "Tier 1",data_usage),
         data_usage= ifelse(data_usage== "Tier 1 CH4: Tier 1", "Tier 1",data_usage)) #Note check unique(icos.fluxnet.AMF.euro.CH4$data_usage) to make sure co2 and methane data have same data usage policy 

###AMERIFLUX BASE#####-----------------------------------------------------------------
setwd("/Users/iwargowsky/Desktop/ABCFlux v2")
base <- read_csv("ameriflux.base.gapfilled.csv")
colnames(base)
base$percent_na <- NULL
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
#merging icos and fluxnet
icos.fluxnet.AMF.euro.CH4.base.wdupes <- rbindlist(list(icos.fluxnet.AMF.euro.CH4, base.renamed), fill = TRUE)

#check if there are number of  duplicates
dupes<- icos.fluxnet.AMF.euro.CH4.base.wdupes  %>% get_dupes(site_reference, year, month)  
#Identify duplicates by dupes= 3 and source= Ameriflux BASE
to.remove <- dupes %>% filter(extraction_source== 'Ameriflux BASE' & dupe_count== '3')
icos.fluxnet.AMF.euro.CH4.base <- anti_join(icos.fluxnet.AMF.euro.CH4.base.wdupes, to.remove, 
                                            by = c("year", "month", "site_reference", "extraction_source"))

#replacing extraction source names to be more clear where data came from
icos.fluxnet.AMF.euro.CH4.base <- icos.fluxnet.AMF.euro.CH4.base %>% 
  mutate(extraction_source= ifelse(extraction_source== "amerifluxdf","Ameriflux", extraction_source),
         extraction_source= ifelse(extraction_source== "betaamerifluxdf", "Ameriflux- beta ONEFLUX", extraction_source),
         extraction_source= ifelse(extraction_source== "ICOS icosdat","ICOS Ecosystem Thematic Centre", extraction_source),
         extraction_source= ifelse(extraction_source== "ICOS wwdat","ICOS Warm Winters", extraction_source))


#Thaw depth and water table data from arctic data center for Barrow, Aquasuk, and Ivotuk
setwd("/Users/iwargowsky/Desktop/arcticdatacenter/downloads")
tdwt <- read_csv("tdwt.csv")#adding year and month columns
tdwt <- tdwt %>% mutate(year= as.integer(year(as.Date(date, format= "%m/%d/%Y"))), 
                        month= as.character(month(as.Date(date, format= "%m/%d/%Y"))) ) 
tdwt.permonth <- tdwt %>% group_by(year, month, site) %>%
  dplyr::summarise(thaw_depth = mean(thaw_depth, na.rm = TRUE),
            water_table_depth= mean(water_table, na.rm = TRUE))%>%
  dplyr::rename("site_reference"= "site")

#merge with df
icos.fluxnet.AMF.euro.CH4.base <-full_join(icos.fluxnet.AMF.euro.CH4.base, tdwt.permonth,
                                        by = c("year", "month", "site_reference"))


#save
setwd("/Users/iwargowsky/Desktop/ABCFlux v2")
write_csv(icos.fluxnet.AMF.euro.CH4.base, "towerrepositorydata.csv")

towersites <- as.data.frame(unique(icos.fluxnet.AMF.euro.CH4.base$site_reference) )
write_csv(towersites, "repository.towersites.csv")




#adding static variables
setwd("/Users/iwargowsky/Desktop/ABCFlux v2")
static <- read_csv("static.towersites.csv", skip= 1)
#join to fill NAs
icos.fluxnet.AMF.euro.CH4.base.static <- natural_join(icos.fluxnet.AMF.euro.CH4.base, static,
                                          by= "site_reference", jointype= "FULL")


#x<- anti_join(icos.fluxnet.AMF.euro.CH4.base.static, icos.fluxnet.AMF.euro.CH4.base,
              #by= c("site_reference", "year", "month"))


##Unifying naming conventions
#US-Bes and US-Beo (changing from fluxnet-ch4 naming)
icos.fluxnet.AMF.euro.CH4.base.static <- icos.fluxnet.AMF.euro.CH4.base.static %>%
  mutate(site_name= ifelse(site_reference== "US-Beo", "Barrow-BEO", site_name)) %>%
  mutate(site_name= ifelse(site_reference== "US-Bes", "Barrow-BES", site_name))
#RU-Che and RU-Ch2
icos.fluxnet.AMF.euro.CH4.base.static <- icos.fluxnet.AMF.euro.CH4.base.static %>%
  mutate(site_name= ifelse(site_reference== "RU-Che", "Cherski", site_name))%>%
  mutate(site_name= ifelse(site_reference== "RU-Ch2", "Cherski reference", site_name))
#US-EML
icos.fluxnet.AMF.euro.CH4.base.static <- icos.fluxnet.AMF.euro.CH4.base.static %>%
  mutate(site_name= ifelse(site_reference== "US-EML", "Eight Mile Lake", site_name))
#Kaamanen
icos.fluxnet.AMF.euro.CH4.base.static <- icos.fluxnet.AMF.euro.CH4.base.static %>%
  mutate(site_name= ifelse(site_reference== "FI-Kaa", "Kaamanen", site_name))
#Svalbard and Jan Mayen
icos.fluxnet.AMF.euro.CH4.base.static <- icos.fluxnet.AMF.euro.CH4.base.static %>%
  mutate(country= ifelse(country== "Svalbard and Jan Mayen", "Norway", country))
#United States
icos.fluxnet.AMF.euro.CH4.base.static <- icos.fluxnet.AMF.euro.CH4.base.static %>%
  mutate(country= ifelse(country== "United States", "USA", country))

####save###----------------------------------------------------------------------
icos.fluxnet.AMF.euro.CH4.base.static$dataentry_person <- "Isabel"
setwd("/Users/iwargowsky/Desktop/ABCFlux v2")
write_csv(icos.fluxnet.AMF.euro.CH4.base.static, "towerrepositorydata.static.csv")




####checking things for Anna 8/18#### 
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
