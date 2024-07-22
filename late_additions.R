#Late Additions (or data sets I put off integrating until the end)
library(dplyr)
library(readr)
library(readxl)
library(purrr)
library(data.table)
library(lubridate)
library(zoo)
library(rio)
library(janitor)
library(stringr)
library(rquery)

# NEW data-------------------------------------------------------------------
setwd("/Users/iwargowsky/Desktop/Data from PIs/April additions")  

#Ted Schuur----------------------------------------------------------------------
files <- list.files(path= "/Users/iwargowsky/Desktop/Data from PIs/April additions", 
                    pattern = "*US-EML_",all.files = T,recursive = T)
schuur.ec <- files %>%
  map_df(~read_csv(.x, col_types = cols(), col_names = TRUE, na=c("NA","-9999", "NaN")))          
#year, month columns
schuur.ec$year <- substr(schuur.ec$TIMESTAMP_START, 1,4)
schuur.ec$month <- substr(schuur.ec$TIMESTAMP_START, 5,6)
#select variables of interest
schuur.ec <- schuur.ec %>% dplyr::select(year, month, FCH4, TA_F, PPFD_IN_F, D_SNOW,
                                  SWC_1_1_1, SWC_2_1_1, TS_1_1_1,	TS_1_2_1,	TS_1_3_1,
                                  TS_1_4_1,	TS_2_1_1,	TS_2_2_1,	TS_2_3_1,	TS_2_4_1,	NEE_F,	GPP_F,	RECO_F)
#summarize by month
schuur.ec <- schuur.ec %>%
  group_by(year, month) %>%
  dplyr::summarise(gap_fill_perc_ch4 = (sum(is.na(FCH4))/n()*100),
                   ch4_flux_total= mean(FCH4, na.rm = T),
                   tair= mean(TA_F, na.rm = T),
                   ppfd= mean(PPFD_IN_F, na.rm = T),
                   snow_depth= mean(D_SNOW, na.rm = T),
                   soil_moisture= mean(c(SWC_1_1_1, SWC_2_1_1), na.rm = T),
                   tsoil_surface= mean(c(TS_1_1_1,	TS_1_2_1,	TS_1_3_1, TS_1_4_1,	TS_2_1_1,	TS_2_2_1,	TS_2_3_1,	TS_2_4_1), na.rm = T),
                   nee= mean(NEE_F, na.rm = T),
                   gpp= mean(GPP_F, na.rm = T),
                   reco= mean(RECO_F, na.rm = T))
#convert units 
schuur.ec$ch4_flux_total <- schuur.ec$ch4_flux_total*0.0010368*days_in_month(as.yearmon(paste(schuur.ec$year, schuur.ec$month ,sep = '-')))
schuur.ec$nee <- schuur.ec$nee*1.0368*days_in_month(as.yearmon(paste(schuur.ec$year, schuur.ec$month ,sep = '-')))
schuur.ec$gpp <- schuur.ec$gpp*1.0368*-1*days_in_month(as.yearmon(paste(schuur.ec$year, schuur.ec$month ,sep = '-')))
schuur.ec$reco <- schuur.ec$reco*1.0368*days_in_month(as.yearmon(paste(schuur.ec$year, schuur.ec$month ,sep = '-')))


#static info
schuur.ec$site_name <- "Eight Mile Lake"
schuur.ec$site_reference <- "US-EML"
schuur.ec$data_contributor_or_author <- "Edward A. G. Schuur"
schuur.ec$email <- "ted.Schuur@nau.edu"
schuur.ec$country <- "USA"
schuur.ec$latitude <- "63.8784"
schuur.ec$longitude <- "-149.2536"
schuur.ec$biome <- "Tundra"
schuur.ec$land_cover <- "110"
schuur.ec$land_cover_bawld <- "Moist Tundra"
schuur.ec$flux_method <- "EC"

#Adrian Rocha---------------------------------------------------------------------------
rocha.ec.unburned <- read_csv("UnburnedABC.csv")
rocha.ec.unburned$site_name <- "Anaktuvuk River Unburned"
rocha.ec.unburned$site_reference <- "US-An3"
  
rocha.ec.severe <- read_csv("SevereABC.csv")
rocha.ec.severe$disturb_year <- 2007
rocha.ec.severe$site_name <- "Anaktuvuk River Severe Burn"
rocha.ec.severe$site_reference <- "US-An1"

rocha.ec.moderate <- read_csv("ModerateABC.csv")
rocha.ec.moderate$disturb_year <- 2007
rocha.ec.moderate$site_name <- "Anaktuvuk River Moderate Burn"
rocha.ec.moderate$site_reference <- "US-An2"

rocha.ec <- rbindlist(list(rocha.ec.moderate, rocha.ec.severe, rocha.ec.unburned))%>%
  dplyr::rename("gap_fill_perc_nee"= "gap_fill_perc") %>%
  dplyr::filter(month %in% c(6,7,8)) #winter and shoulders season fluxes are entirely gapfilled as 0 so we're removing
rocha.ec$data_contributor_or_author <- "Adrian Rocha"
rocha.ec$gpp <- rocha.ec$gpp *-1

#Mary Farina---------------------------------------------------------------------------
farina.ch <- read_csv("Data_for_ABCfluxV2_Terrestrial_Chamber_Flux_BigTrailLake_Fens_July2021_mfarina20240414.csv", na= "N/A")
farina.ch$site_reference <-  str_split(farina.ch$site_id, "Farina_BTL_") %>% sapply(`[`, 2) 
farina.ch$tair <- as.numeric(sub(".*\\(([^)]+)\\).*", "\\1", farina.ch$tair))

farina.ch <- farina.ch %>%
  group_by(site_name, site_reference, year, month, longitude, latitude) %>%
  dplyr::summarise(across(where(is.numeric),list(mean = ~ mean(.x, na.rm = TRUE)) ),
                   across(where(is.character), list(unique = ~toString(unique(.[!is.na(.)]))))) %>%
  rename_with(~gsub("_unique", "", .), everything()) %>%  # Remove "_unique" from column names
  rename_with(~gsub("_mean", "", .), everything()) # Remove "_mean" from column names

#NOTE: Isabel manually fixed chamber measurement days in excel
farina.ch <- farina.ch %>%
  mutate(site_reference= ifelse(chamber_nr_measurement_days %in% 2, paste(site_reference, "_agg", sep=""), site_reference))%>%
  mutate(chamber_nr_measurement_days_co2= chamber_nr_measurement_days)%>%
  dplyr::rename("chamber_nr_measurement_days_ch4"="chamber_nr_measurement_days" )

farina.ch$gpp <- farina.ch$gpp * -1
farina.ch$soil_moisture <- farina.ch$soil_moisture*100



#Natascha Kljun---------------------------------------------------------------------------
kljun.ec <- read_csv("ABCfluxv2.vars_Ijusdal.csv")%>%
  dplyr::rename("gap_fill_perc_nee"= "gap_fill_perc")
kljun.ec$soil_moisture <- kljun.ec$soil_moisture*100

#Daniel Nadeau---------------------------------------------------------------------------
nadeau.ec <- read_csv("ABC_Bernard_spruce_moss_valley.csv") 
nadeau.ec$gpp <- nadeau.ec$gpp * -1
nadeau.ec$gap_fill_perc_nee <- nadeau.ec$gap_fill_perc_nee *100
nadeau.ec$gap_fill_perc_ch4 <- nadeau.ec$gap_fill_perc_ch4 *100
nadeau.ec$stock_depth <- "100"
nadeau.ec$c_stock <- "7"
nadeau.ec$notes <- "Carbon stock: 7 kg C m-2 for the soil under the 17cm organic layer. C content of the organic layer hasn't been characterized."

#Christian Knoblauch----------------------------------------------------------------
knoblauch.ch <- read_csv("ABCfluxv2_Knoblauch.csv")
knoblauch.ch <- knoblauch.ch %>%
  mutate(site_reference= str_split(site_id, "Knoblauch_") %>% sapply(`[`, 2)) %>%
  group_by(site_name, site_reference, year, month, longitude, latitude) %>%
  dplyr::summarise(across(where(is.numeric),list(mean = ~ mean(.x, na.rm = TRUE)) ),
                   across(where(is.character), list(unique = ~toString(unique(.[!is.na(.)]))))) %>%
  rename_with(~gsub("_unique", "", .), everything()) %>%  # Remove "_unique" from column names
  rename_with(~gsub("_mean", "", .), everything()) %>% # Remove "_mean" from column names
  mutate(chamber_nr_measurement_days_co2= chamber_nr_measurement_days)%>%
  dplyr::rename("chamber_nr_measurement_days_ch4"="chamber_nr_measurement_days" )

#June Skeeter------------------------------------------------------------------
skeeter.chamber <- read_csv("ABCFluxV2_Skeeter.chamber.csv", na= c("NA", "N/A"))
skeeter.tower <- read_csv("ABCFluxV2_Skeeter.tower.csv", na= c("NA", "N/A"))

#Domine, Florent------------------------------------------------------------------
domine.ec.co2 <- read.table("Domine-etal_2024/datasets/Umiujaq_turbulent_CO2_flux.tab", header = T, sep = "\t", fill= TRUE, skip=29 )
domine.ec.precip <- read.table("Domine-etal_2024/datasets/Umiujaq_precip_tundra.tab", header = T, sep = "\t", fill= TRUE, skip=27 )
domine.ec.soil <- read.table("Domine-etal_2024/datasets/Umiujaq_soil_temp_water.tab", header = T, sep = "\t", fill= TRUE, skip=29 )%>% dplyr::filter(Event== "Umiujaq_2012-2021_Tundra")
domine.ec.air <- read.table("Domine-etal_2024/datasets/Umiujaq_temp_wind_tundra.tab", header = T, sep = "\t", fill= TRUE, skip=32)
domine.ec.snow <- read.table("Domine-etal_2024/datasets/Umiujaq_snow_height.tab", header = T, sep = "\t", fill= TRUE, skip=27) %>% dplyr::filter(Event== "Umiujaq_2012-2021_Tundra")

domine.ecdf <- domine.ec.co2 %>%
  full_join(domine.ec.precip, by = join_by(Date.Time)) %>%
  full_join(domine.ec.soil, by = join_by(Date.Time)) %>%
  full_join(domine.ec.air, by = join_by(Date.Time)) %>%
  full_join(domine.ec.snow, by = join_by(Date.Time))

domine.ec <- domine.ecdf %>%
  mutate(year= year(as.Date(Date.Time, format= "%Y-%m-%dT%H:%M")),
         month= month(as.Date(Date.Time, format= "%Y-%m-%dT%H:%M")),
         day= day(as.Date(Date.Time, format= "%Y-%m-%dT%H:%M"))) %>%
  mutate(tsoil_surface= ifelse(Depth.soil..m.< 0.11, T.soil...C., NA),
         tsoil_surface_depth= ifelse(Depth.soil..m.< 0.11, Depth.soil..m., NA),
         tsoil_deep= ifelse(Depth.soil..m.> 0.10, T.soil...C., NA),
         tsoil_deep_depth= ifelse(Depth.soil..m.> 0.10, Depth.soil..m., NA)) %>%
  group_by(year, month) %>%
  dplyr::summarise(n= n_distinct(day),
                   precip= sum(Precip..mm.h., na.rm = T)/100,
                   tair= mean(TTT...C...Humidity.Temperature.probe..R...., na.rm = T),
                   snow_depth= mean(Snow.h..m., na.rm = T),
                   tsoil_surface= mean(tsoil_surface, na.rm= T),
                   tsoil_surface_depth= mean(tsoil_surface_depth , na.rm= T),
                   tsoil_deep= mean(tsoil_deep, na.rm= T),
                   tsoil_deep_depth= mean(tsoil_deep_depth , na.rm= T),
                   gap_fill_perc_nee = sum(QF.CO2.flux)/n()*100,
                   nee=  mean(CO2.flux..µmol.m..2.s., na.rm = T)) %>%
  dplyr::filter(n>27) %>%
  dplyr::filter(!is.na(nee))
domine.ec$n <- NULL

#convert units umol m-2 s-1 to g C m-2 month-1
domine.ec$nee <- domine.ec$nee*1.0368*days_in_month(as.yearmon(paste(domine.ec$year, domine.ec$month,sep = '-')))
domine.ec$n <- NULL
domine.ec$tsoil_deep_depth <- domine.ec$tsoil_deep_depth *100
domine.ec$tsoil_surface_depth <- domine.ec$tsoil_surface_depth *100

#add static info
domine.ec$site_name <- "Umiujaq"
domine.ec$latitude <- "56.556557"
domine.ec$longitude <- "-76.48234"
domine.ec$flux_method <- "EC"
domine.ec$gap_fill <- "interpolation and a self-organizing linear output map (SOLO) (see Hsu et al., 2002, and Abramowitz, 2005)."
domine.ec$instrumentation <- "IRGASON Campbell Scientific"
domine.ec$veg_detail <- "a mixture of lichen and low shrub tundra"
domine.ec$biome <- "Tundra"
domine.ec$citation <- "Domine, Florent; Sarrazin, Denis; Nadeau, Daniel; Lackner, Georg; Belke-Brea, Maria (2024): Hydrometeorological, snow and soil data from a low-Arctic valley in the forest-tundra ecotone in Northern Quebec. PANGAEA, https://doi.org/10.1594/PANGAEA.964743"
domine.ec$data_contributor_or_author <- "Domine, Florent; Sarrazin, Denis; Nadeau, Daniel; Lackner, Georg; Belke-Brea, Maria"
domine.ec$country <- "Canada"
domine.ec$tair_height <- 4.2
domine.ec$c_stock <- 5.7

#Kathleen Savage --------------------------------------------------------
boreas.fluxdf <- read_xlsx("data from Kathleen Savage/1994 flux summary of Anna.xlsx", sheet = 1, skip=13, na= "na") %>%
  dplyr::filter(!Flux== "CO2")#removing co2 flux cause its from a forest

boreas.flux <- boreas.fluxdf %>%
  mutate(year= 1994,
         Date= gsub("Sept", "Sep", boreas.fluxdf$`Date (1994)`),
         month= month(as.Date(Date, format= "%b %d")),
         day= day(as.Date(Date, format= "%b %d")))%>%
  group_by(year, month, `Site name`, Flux)%>%
  dplyr::summarise(ch4_flux_total= mean(c(C1,C2,C3,C4,C5,C6), na.rm = T),
                   chamber_nr_measurement_days_ch4= n_distinct(day))
#convert units from mg CH4 m-2 d-1 to g C m-2 month-1
boreas.flux$ch4_flux_total <- boreas.flux$ch4_flux_total/1000/16.04*12.01* days_in_month(as.yearmon(paste(boreas.flux$year, boreas.flux$month,sep = '-')))

boreas.tempdf <- read_xlsx("data from Kathleen Savage/1994 flux summary of Anna.xlsx", sheet = 2, skip=4, na= "na")

boreas.temp <- boreas.tempdf %>%
  mutate(date= as.Date(DOY, origin= "1-1-1994", format= "%j"),
         year= 1994,
         month= month(date),
         day= day(date)) %>%
  mutate(tsoil_5 = ifelse(!is.na(`5`), 5, NA),  tsoil_10 = ifelse(!is.na(`10`), 10, NA),
         tsoil_20 = ifelse(!is.na(`20`), 20, NA), tsoil_30 = ifelse(!is.na(`30`), 30, NA),
         tsoil_40 = ifelse(!is.na(`40`), 40, NA), tsoil_50 = ifelse(!is.na(`50`), 50, NA),
         tsoil_60 = ifelse(!is.na(`60`), 60, NA), tsoil_100 = ifelse(!is.na(`100`), 100, NA)) %>%
  group_by(year, month, `Site name`)%>%
  dplyr::summarise(tair= mean(`Ambient air T`, na.rm = T),
                   tsoil_surface= mean(c(`5`,`10`), na.rm = T),
                   tsoil_surface_depth= mean(c(tsoil_5, tsoil_10), na.rm = T),
                   tsoil_deep= mean(c(`20`,`30`,`40`,`50`,`60`,`100`), na.rm = T),
                   tsoil_deep_depth= mean(c(tsoil_20, tsoil_30, tsoil_40, tsoil_50, tsoil_60, tsoil_100), na.rm = T))
boreas <- boreas.flux %>% full_join(boreas.temp, by= c("year", "month", "Site name"))

#static info
boreas$site_name <- "BOREAS NSA"
boreas$biome <- "Boreal"
boreas$flux_method <- "Chamber"
boreas$flux_method_detail <- "Static chambers, six replicates in each 'site_reference'"
boreas$flux_method_description <- "Chambers were 7-9 L plastic bottles with an open surface area at the bottom of 0.047m"
boreas$email <- "savage@woodwellclimate.org"
boreas$citation <- "https://doi.org/10.1029/97JD02233"
boreas$data_contributor_or_author <- "Kathleen Savage"
boreas$country <- "Canada"
boreas$longitude <- "-97.866667"
boreas$latitude <- "55.666667"
boreas$veg_detail <- "The area is primarily covered with black spruce (Picea mariana) and some paper birch (Betula papyrifera), trembling aspen (Populus tremuloides), and jack pine (Pinus banksiana) stand"
boreas$ev_needle_tree <- "Dominant"
boreas$dec_broad_tree <- "Present"
boreas <- boreas %>% dplyr::rename("site_reference"= "Site name")
boreas <- boreas %>%
  mutate(disturbance= ifelse(site_reference %in% c("89Burn_moss","89Burn_Spruce"), "Fire", NA),
         disturb_year= ifelse(site_reference %in% c("89Burn_moss","89Burn_Spruce"), 1989, NA))
boreas$gap_fill <- "Average"
boreas$instrumentation <- "Shimadzu 14A gas chromatograph"
boreas <- boreas %>%
  mutate(precip = case_when(month %in% 5 ~ 41.6,
                            month %in% 6 ~ 45.8,
                            month %in% 7 ~ 69.5,
                            month %in% 8 ~ 31.8,
                            month %in% 9 ~ 21.8))

#Beth Holmes-------------------------------------------------------------------- 
ch4df <- read_xlsx("chamber_CO2_and_CH4_with_T.xlsx", sheet=1 )%>%
  mutate(month = month(as.Date(Date, format= "%Y-%m-%d")),
         day= day(as.Date(Date, format= "%Y-%m-%d"))) %>%
  dplyr::select(year, month, day, site, `gC/m2/d`)

co2df <- read_xlsx("chamber_CO2_and_CH4_with_T.xlsx", sheet=2 )%>%
  mutate(month = month(as.Date(Date, format= "%Y-%m-%d")),
         day= day(as.Date(Date, format= "%Y-%m-%d"))) %>%
  dplyr::select(year, month, day, site, `gC/m2/d filled`)
tempdf <- read_xlsx("chamber_CO2_and_CH4_with_T.xlsx", sheet=3 )%>%
  mutate(year=year(as.Date(Date, format= "%m-%d/%y")),
         month = month(as.Date(Date, format= "%m-%d/%y"))) %>%
  group_by(year, month) %>%
  dplyr::summarise(tsoil_surface= mean(T_.10, na.rm=T),
                   tsoil_deep= mean(c(`T at -50 cm in fen`, `T_.20`), na.rm=T)) 

holmesflux <- ch4df %>% full_join(co2df) %>%
  group_by(year, month, site) %>%
  dplyr::summarise(ch4_flux_total= sum(`gC/m2/d`, na.rm = T),
                   nee= sum(`gC/m2/d filled`, na.rm = T)) %>%
  dplyr::filter(!site==4)


holmes <- holmesflux %>% full_join(tempdf) %>%
  mutate(site_reference= case_when(site %in% 1~ "Palsa",
                                  site %in% 2~ "Bog",
                                  site %in% 3~ "Fen"),
        site= NULL) 

#static info
holmes$site_name <- "Stordalen Mire"
holmes$country <- "Sweden"
holmes$latitude <- "68.333333"
holmes$longitude <- "19.05"
holmes$citation <- " https://doi.org/10.1029/2021GB007113"
holmes$extraction_source <- "EMERGE-DB"
holmes$biome <- "Tundra"
holmes <- holmes %>%
  mutate(veg_detail = case_when(site_reference %in% "Palsa" ~ "Ericaceous shrubs and cryptogams (mosses and lichens) dominate palsa vegetation",
                                site_reference %in% "Bog"~ "Vegetation in the bog sites is dominated by Sphagnum spp., feather mosses (e.g., Dicranum elongatum), and sedges (e.g., Eriophorum vaginatum and Carex biglowii)",
                                site_reference %in% "Fen"~ "Eriophorum angustifolium, Carex rostrata, and Esquisetum spp., as well as Sphagnum spp"))
holmes$permafrost <- "Yes"
holmes$permafrost_thaw <- "Yes"
holmes$flux_method <- "Chamber"
holmes$flux_method_detail <- "Automatic closed chamber"
holmes$flux_method_description <- "Sampling was done every 3 hr, and the chamber was closed for 5–8 min for each measurement (0.178 m2 basal area)"
holmes$instrumentation <- "Los Gatos Research (LGR) Fast Greenhouse Gas Analyzer"
holmes$diurnal_coverage <- "Day and Night"
holmes$tsoil_surface_depth <- 10
holmes$tsoil_deep_depth <- 35
holmes$data_contributor_or_author <- "Beth Holmes, Patrick Crill"
holmes$email <- "bhuettel@fsu.edu, patrick.crill@geo.su.se"

#Lake Hazen --------------------------------------------------------------------
hazen.co2 <- read_xlsx("Lake Hazen/Lake_Hazen_EC_Towers_ABCfluxv2_Request_2023_ikw.edits.xlsx", sheet= 4, skip=1, na= c("n/a", "-")) 
hazen.co2 <- hazen.co2 %>%
  filter(!is.na(`Soil CO2 Flux (umol/m2/s)`)) %>%
  mutate(year= year(as.Date(DATE, format= "%d-%b-%Y")),
         month= month(as.Date(DATE, format= "%d-%b-%Y")),
         day= day(as.Date(DATE, format= "%d-%b-%Y"))) %>%
  group_by(year, month, site_reference) %>%
  dplyr::summarise(reco = mean(as.numeric(`Soil CO2 Flux (umol/m2/s)`), na.rm=T),
                   tsoil_surface = mean(as.numeric(`Soil temperature (deg. C)`), na.rm=T),
                   chamber_nr_measurement_days_co2= n_distinct(day))
#convert units umol m-2 s-1 to g C m-2 month-1
hazen.co2$reco <- hazen.co2$reco*1.0368*days_in_month(as.yearmon(paste(hazen.co2$year, hazen.co2$month,sep = '-')))

hazen.ch4.08 <- read_xlsx("Lake Hazen/Lake_Hazen_EC_Towers_ABCfluxv2_Request_2023_ikw.edits.xlsx", sheet= 5, skip=2, na= c("n/a", "-")) %>%
  dplyr::select(site_reference, Date, `Field Temp...17`,  `Oxidation Rate...23` )
hazen.ch4.09 <- read_xlsx("Lake Hazen/Lake_Hazen_EC_Towers_ABCfluxv2_Request_2023_ikw.edits.xlsx", sheet= 6, skip=2, na= c("n/a", "-"))  %>%
  dplyr::select(site_reference, Date, `Field Temp...17`,  `Oxidation Rate...24` )
hazen.ch4.10 <- read_xlsx("Lake Hazen/Lake_Hazen_EC_Towers_ABCfluxv2_Request_2023_ikw.edits.xlsx", sheet= 7, skip=2, na= c("n/a", "-"))  %>%
  dplyr::select(site_reference, Date, `Field Temp...17`,  `Oxidation Rate...24` )
hazen.ch4.11 <- read_xlsx("Lake Hazen/Lake_Hazen_EC_Towers_ABCfluxv2_Request_2023_ikw.edits.xlsx", sheet= 8, skip=2, na= c("n/a", "-")) %>%
  dplyr::select(site_reference, Date, `Field Temp...17`,  `Oxidation Rate...24` ) 
hazen.ch4.12 <- read_xlsx("Lake Hazen/Lake_Hazen_EC_Towers_ABCfluxv2_Request_2023_ikw.edits.xlsx", sheet= 9, skip=2, na= c("n/a", "-"))  %>%
  dplyr::select(site_reference, Date, `Field Temp...17`,  `Oxidation Rate...24` )

hazen.ch4 <- rbindlist(list(hazen.ch4.08, hazen.ch4.09, hazen.ch4.10, hazen.ch4.11, hazen.ch4.12),fill=TRUE) %>%
  mutate(year= year(as.Date(Date)),
         month= month(as.Date(Date)),
         day= day(as.Date(Date))) %>%
  dplyr::filter(!is.na(year))%>%
  group_by(year, month, site_reference) %>%
  dplyr::summarise(ch4_flux_total = mean(as.numeric(c(`Oxidation Rate...24`, `Oxidation Rate...23`)), na.rm=T),
                   tair= mean(as.numeric(`Field Temp...17`), na.rm= T),
                   chamber_nr_measurement_days_ch4= n_distinct(day)) 

#convert units	(mg/m2/d) to g C m-2 month-1
hazen.ch4$ch4_flux_total <- hazen.ch4$ch4_flux_total/1000*days_in_month(as.yearmon(paste(hazen.ch4$year, hazen.ch4$month,sep = '-')))


hazen.ch <- merge(hazen.ch4, hazen.co2)

#static info 
hazen.static <- read_xlsx("Lake Hazen/Lake_Hazen_EC_Towers_ABCfluxv2_Request_2023_ikw.edits.xlsx", sheet=2, na= c("n/a", "-")) %>%
  dplyr::filter(measure_type %in%  c("Chambers - CO2", "Chambers - CH4")) %>%
  group_by(site_reference) %>%
  dplyr::summarise(across(where(is.numeric),list(mean = ~ mean(.x, na.rm = TRUE)) ),
                   across(where(is.character), list(unique = ~toString(unique(.[!is.na(.)]))))) %>%
  rename_with(~gsub("_unique", "", .), everything()) %>%  # Remove "_unique" from column names
  rename_with(~gsub("_mean", "", .), everything())  # Remove "_mean" from column names

hazen.ch <- hazen.ch %>% full_join(hazen.static, by = join_by(site_reference))
hazen.ch$Column <- NULL
hazen.ch$measure_type <- NULL


#hazen.thaw <- read_xlsx("Lake Hazen/Lake_Hazen_EC_Towers_ABCfluxv2_Request_2023_ikw.edits.xlsx", sheet=2, na= c("n/a", "-")) %>%
  



#BAWLD -------------------------------------------------------------------------
setwd("/Users/iwargowsky/Desktop/ABCFlux v2/chamber data extractions")
BAWLD.new <- read_csv("abcFluxv1_additional_chamber_CH4_uptake_extractions_april2024.csv", na= c(NA, "-")) %>%
  dplyr::rename("Disturbance_Category"= "disturbance_Category")

BAWLD.new$dataentry_person <- "Kuhn"

BAWLDsinglemonth <- read_csv("ABCflux2_single_month_chambers_ikwedits.csv", na= c(NA, "-"))%>%
  dplyr::filter(!site_name== "Tanana_River")%>%
  dplyr::rename("ch4_flux_total"= "ch4_flux_total_CONVERTED",
                "chamber_nr_measurement_days_ch4"="chamber_nr_measurement_days") %>%
  mutate(chamber_nr_measurement_days_co2= ifelse(!is.na(nee) | !is.na(gpp) | !is.na(reco), chamber_nr_measurement_days_ch4, NA))%>%
  group_by(site_name, site_reference, year, month) %>%
  dplyr::summarise(across(where(is.numeric),list(mean = ~ mean(.x, na.rm = TRUE)) ),
                   across(where(is.character), list(unique = ~toString(unique(.[!is.na(.)]))))) %>%
  rename_with(~gsub("_unique", "", .), everything()) %>%  # Remove "_unique" from column names
  rename_with(~gsub("_mean", "", .), everything()) # Remove "_mean" from column names

BAWLDsinglemonth$dataentry_person <- "Windholz"

#CONDENSE-------------------------------------------------------------------------
newPIdata <- rbindlist(list(schuur.ec, rocha.ec, kljun.ec, nadeau.ec , farina.ch, 
                          knoblauch.ch, skeeter.chamber, skeeter.tower, domine.ec,
                          boreas, hazen.ch),  fill = TRUE)

newPIdata$extraction_source <- "User-Contributed"
newPIdata$dataentry_person <- "Wargowsky"

newBAWLDdata <- rbindlist(list(BAWLDsinglemonth, BAWLD.new ),  fill = TRUE)

newBAWLDdata$extraction_source <- "BAWLD-CH4-Publication"

late.additions <- rbindlist(list(newPIdata, newBAWLDdata, holmes),  fill = TRUE)
#remove rows that do not contain flux data
late.additions <- late.additions %>% 
  dplyr::filter(!if_all(c(nee, gpp, reco, ch4_flux_total, nee_seasonal, ch4_flux_seasonal), ~ is.na(.))) %>%
  dplyr::filter(!site_name %in% c("Y", NA, "Site name as specified in data source. E.g. Hyytiälä"))

late.additions <- late.additions %>% 
  mutate(citation_ch4 = ifelse(!is.na(ch4_flux_total)| !is.na(ch4_flux_seasonal), citation, NA)) %>%
  mutate(citation_co2 = ifelse(!is.na(nee) | !is.na(gpp) | !is.na(reco)| !is.na(nee_seasonal),  citation, NA)) %>%
  mutate(extraction_source_ch4 = ifelse(!is.na(ch4_flux_total)| !is.na(ch4_flux_seasonal), extraction_source, NA)) %>%
  mutate(extraction_source_co2 = ifelse(!is.na(nee) | !is.na(gpp) | !is.na(reco)| !is.na(nee_seasonal), extraction_source, NA)) %>%
  mutate(citation = NULL, extraction_source= NULL)



#CLEANING#########-----------------------------------------------------------
late.additions$date <- as.Date(paste(late.additions$year, late.additions$month,sep = '-'), format = "%Y-%m") #add time stamp

late.additions$ts <- as.yearmon(paste(late.additions$year, late.additions$month,sep = '-')) #add time stamp

##FLUXES
# Plotting CO2
lapply(unique(late.additions$site_name), function(site) {
  p <- ggplot(subset(late.additions, site_name == site)) +
    geom_line( aes(x = ts, y = as.numeric(nee))) +
    geom_point(aes(x = ts, y = as.numeric(nee)))+
    geom_line( aes(x = ts, y = as.numeric(gpp)), linetype= "dashed") +
    geom_point(aes(x = ts, y = as.numeric(gpp)), shape= 2)+
    geom_line( aes(x = ts, y = as.numeric(reco)), linetype= "dashed") +
    geom_point(aes(x = ts, y = as.numeric(reco)), shape= 0)+
    theme(legend.position = "bottom") +
    geom_hline(yintercept = 0)+   
    labs(title = site,
         x = "Date",
         y = "g C m-2 month-1")
  
  return(p)
})  

# Plotting CH4
lapply(unique(late.additions$site_name), function(site) {
  p <- ggplot(subset(late.additions, site_name == site)) +
    geom_line( aes(x = ts, y = ch4_flux_total, color = extraction_source)) +
    geom_point(aes(x = ts, y = ch4_flux_total, color = extraction_source))+
    theme(legend.position = "bottom") +
    geom_hline(yintercept = 0)+   
    labs(title = site,
         x = "Date",
         y = "g C m-2 month-1")
  
  return(p)
})  

#Plotting TAIR
lapply(unique(late.additions$site_name), function(site) {
  p <- ggplot(subset(late.additions, site_name == site), aes(x = ts, y = as.numeric(tair), color= extraction_source)) +
    geom_line() +
    geom_point()+
    labs(title = paste("Tair", site),
         x = "Date",
         y = "Air Temperature") +
    theme_minimal()
  
  return(p)
})


#Plotting PRECIP
lapply(unique(late.additions$site_name), function(site) {
  p <- ggplot(subset(late.additions, site_name == site), aes(x = ts, y = as.numeric(precip), color= extraction_source)) +
    geom_line() +
    geom_point()+
    labs(title = paste("Precip", site),
         x = "Date",
         y = "Precip") +
    theme_minimal()
  
  return(p)
})

#Plotting TSOIL
lapply(unique(late.additions$site_name), function(site) {
  p <- ggplot(subset(late.additions, site_name == site)) +
    geom_line( aes(x = ts, y = as.numeric(tsoil_surface), color= extraction_source)) +
    geom_point( aes(x = ts, y = as.numeric(tsoil_surface), color= extraction_source))+
    geom_line( aes(x = ts, y = as.numeric(tsoil_deep), color= extraction_source), linetype= "dashed") +
    geom_point( aes(x = ts, y = as.numeric(tsoil_deep), color= extraction_source))+
    labs(title = paste("Soil temps", site),
         x = "Date",
         y = "soil temps") +
    theme_minimal()
  
  return(p)
})

#water table depth
lapply(unique(late.additions$site_name), function(site) {
  p <- ggplot(subset(late.additions, site_name == site)) +
    geom_line( aes(x = ts, y = as.numeric(water_table_depth), color= extraction_source)) +
    geom_point( aes(x = ts, y = as.numeric(water_table_depth), color= extraction_source))+
    labs(title = paste("Water table depth", site),
         x = "Date",
         y = "Water table depth") +
    theme_minimal()
  
  return(p)
})


#soil moisture
lapply(unique(late.additions$site_name), function(site) {
  p <- ggplot(subset(late.additions, site_name == site), aes(x = ts, y = as.numeric(soil_moisture), color= extraction_source)) +
    geom_line() +
    geom_point()+
    labs(title = paste("Soil Moisture", site),
         x = "Date",
         y = "Soil Moisture") +
    theme_minimal()
  
  return(p)
})


#thaw_depth
lapply(unique(late.additions$site_name), function(site) {
  p <- ggplot(subset(late.additions, site_name == site), aes(x = ts, y = as.numeric(thaw_depth), color= extraction_source)) +
    geom_line() +
    geom_point()+
    labs(title = paste("thaw_depth", site),
         x = "Date",
         y = "thaw_depth") +
    theme_minimal()
  
  return(p)
})

#snow_depth
lapply(unique(late.additions$site_name), function(site) {
  p <- ggplot(subset(late.additions, site_name == site), aes(x = ts, y = as.numeric(snow_depth), color= extraction_source)) +
    geom_line() +
    geom_point()+
    labs(title = paste("snow_depth", site),
         x = "Date",
         y = "snow_depth") +
    theme_minimal()
  
  return(p)
})

#ppfd
lapply(unique(late.additions$site_name), function(site) {
  p <- ggplot(subset(late.additions, site_name == site), aes(x = ts, y = as.numeric(ppfd), color= extraction_source)) +
    geom_line() +
    geom_point()+
    labs(title = paste("ppfd", site),
         x = "Date",
         y = "ppfd") +
    theme_minimal()
  
  return(p)
})

unique(late.additions$alt)

late.additions$ts <- NULL

### DISCRETE VARIABLES ######_-----------------------------------------------------------------------------------------

unique(late.additions$country)
late.additions <- late.additions %>% mutate(country= ifelse(country %in% "CA","Canada", country)) %>%
  mutate(country= ifelse(country %in% "Sweeden","Sweden", country))
#check lat long coords

# # Load world country borders
# world <- ne_countries(scale = 110, returnclass = "sf") %>% dplyr::filter(subregion %in% c("Northern America", "Central Asia", "Eastern Europe",
#                                                   "Northern Europe", "Western Europe", "Southern Asia",
#                                                   "Southern Europe", "Eastern Asia", "Western Asia"))
# # Read additional spatial data
# setwd("/Users/iwargowsky/Desktop/ABCFlux v2/Figures/AGU")
# permafrost <- st_read("UiO_PEX_PERZONES_5.0_20181128_2000_2016_NH") %>% dplyr::rename("Permafrost zone"= "EXTENT")
# 
# listofsites <- late.additions %>%
#   group_by(site_name, flux_method, country) %>%
#   dplyr::summarise(latitude = mean(as.numeric(latitude), na.rm=T), longitude = mean(as.numeric(longitude), na.rm=T))
# 
# # Convert to sf object
# sites <- st_as_sf(listofsites, coords = c("longitude", "latitude"), crs = st_crs("+proj=longlat +datum=WGS84"))
# 
# # Plot the map
# ggplot() +
#   geom_sf(data = permafrost, aes(fill = `Permafrost zone`), color = "NA") +
#   scale_fill_brewer(palette = "Blues", direction = -1)+
#   geom_sf(data = world, color = "black", fill = NA , size= 1) +
#   geom_sf(data = sites, aes(color = country), fill = "black", size = 1) +
#   scale_color_brewer(palette = "Set3")

#time vars
unique(late.additions$year)

unique(late.additions$month)
late.additions$month <- as.numeric(late.additions$month)

#seasonal intervals
unique(late.additions$nee_seasonal_interval)  
late.additions <- late.additions %>%
  mutate(nee_seasonal_interval = ifelse(nee_seasonal_interval %in% c("06/23/2017-09/13/2017"), "06/23-09/13", nee_seasonal_interval)) %>%
  mutate(nee_seasonal_interval = ifelse(nee_seasonal_interval %in% c("07/08/2016-08/07/2016"), "07/08-08/07", nee_seasonal_interval)) 

unique(late.additions$ch4_flux_seasonal_interval)
late.additions <- late.additions %>%
  mutate(ch4_flux_seasonal_interval = ifelse(ch4_flux_seasonal_interval %in% c("06/23/2017-09/13/2017"), "06/23-09/13", ch4_flux_seasonal_interval)) %>%
  mutate(ch4_flux_seasonal_interval = ifelse(ch4_flux_seasonal_interval %in% c("07/08/2016-08/07/2016"), "07/08-08/07", ch4_flux_seasonal_interval)) 

#####vegetation ####
unique(late.additions$canopy_height)
late.additions <- late.additions %>%
  mutate(canopy_height= ifelse(canopy_height %in% "15m", 15, canopy_height)) %>%
  mutate(canopy_height= ifelse(canopy_height %in% "~0.02", 0.02, canopy_height))%>%
  mutate(canopy_height= ifelse(canopy_height %in% "~0.05-0.10", 0.075, canopy_height))

unique(late.additions$forest_age)
late.additions <- late.additions %>%
  mutate(forest_age= ifelse(forest_age %in% c("N/A","Unknown. Mature forest"), NA, forest_age))

unique(late.additions$partition_method)
late.additions <- late.additions %>%
  mutate(partition_method= ifelse(partition_method %in% "N/A", NA, partition_method))

unique(late.additions$tair_height)
late.additions <- late.additions %>%
  mutate(tair_height= ifelse(tair_height %in% "25m", 25, tair_height))

unique(late.additions$tsoil_surface_depth)
late.additions <- late.additions %>%
  mutate(tsoil_surface_depth= ifelse(tsoil_surface_depth %in% "2 cm",2, tsoil_surface_depth))

unique(late.additions$tsoil_deep_depth)
unique(late.additions$moisture_depth)
late.additions <- late.additions %>%
  mutate(moisture_depth= ifelse(moisture_depth %in% "0-30", 15, moisture_depth))

unique(late.additions$soil_depth)
summary(late.additions$soil_depth)
unique(late.additions$soil_ph)
summary(late.additions$soil_ph)

unique(late.additions$soil_perc_c)
summary(late.additions$soil_perc_c)
unique(late.additions$soil_perc_n)
summary(late.additions$soil_perc_n)

unique(late.additions$c_stock)
unique(late.additions$stock_depth)

unique(late.additions$site_name)
unique(late.additions$site_reference)

unique(late.additions$data_contributor_or_author)
unique(late.additions$email)

unique(late.additions$biome)
unique(late.additions$veg_detail)
unique(late.additions$dec_shrub)
late.additions <- late.additions %>%
  mutate(dec_shrub= ifelse(dec_shrub %in% c("Preset","P"), "Present", dec_shrub))
unique(late.additions$ev_shrub)
late.additions <- late.additions %>%
  mutate(ev_shrub= ifelse(ev_shrub %in% c("A"), "Absent", ev_shrub))
unique(late.additions$sedge)
late.additions <- late.additions %>%
  mutate(sedge= ifelse(sedge %in% c("P"), "Present", sedge))
unique(late.additions$non_sedge_herbaceous)
late.additions <- late.additions %>%
  mutate(non_sedge_herbaceous= ifelse(non_sedge_herbaceous %in% c("P"), "Present", non_sedge_herbaceous))
unique(late.additions$ev_needle_tree)
late.additions <- late.additions %>%
  mutate(ev_needle_tree= ifelse(ev_needle_tree %in% c("A"), "Absent", ev_needle_tree))
unique(late.additions$dec_needle_tree)
late.additions <- late.additions %>%
  mutate(dec_needle_tree= ifelse(dec_needle_tree %in% c("A"), "Absent", dec_needle_tree))
unique(late.additions$dec_broad_tree)
late.additions <- late.additions %>%
  mutate(dec_broad_tree= ifelse(dec_broad_tree %in% c("A"), "Absent", dec_broad_tree))
unique(late.additions$sphagnum_cover)
late.additions <- late.additions %>%
  mutate(sphagnum_cover= ifelse(sphagnum_cover %in% c("A"), "Absent", sphagnum_cover))
unique(late.additions$other_moss_cover)
late.additions <- late.additions %>%
  mutate(other_moss_cover= ifelse(other_moss_cover %in% c("P"), "Present", other_moss_cover))

unique(late.additions$soil_moisture_class)
late.additions <- late.additions %>%
  mutate(soil_moisture_class= ifelse(soil_moisture_class %in% "moist", "Moist", soil_moisture_class)) %>%
  mutate(soil_moisture_class= ifelse(soil_moisture_class %in% "wet", "Wet", soil_moisture_class))

unique(late.additions$permafrost)
late.additions <- late.additions %>%
  mutate(permafrost= ifelse(permafrost %in% "Y", "Yes", permafrost)) %>%
  mutate(permafrost= ifelse(permafrost %in% c("Unknown", "NA"), NA, permafrost))

unique(late.additions$permafrost_thaw)
late.additions <- late.additions %>%
  mutate(permafrost_thaw= ifelse(permafrost_thaw %in% "Y", "Yes", permafrost_thaw)) %>%
  mutate(permafrost_thaw= ifelse(permafrost_thaw %in% c("Unknown", "NA"), NA, permafrost_thaw))

unique(late.additions$landform)
unique(late.additions$disturbance)
late.additions <- late.additions %>%
  mutate(Disturbance_Category= case_when(disturbance %in% c("Fire",
                                                            "Wildfire in July 2018, in Pinus sylvestris plantation, trees were about 54 years old at the time of the fire. All trees survived but were clearcut in winter 2018/2019 and soil scarification was performed (soil ploughing which creates ridges with the soil organic layer remaining and furrows where the soil organic layer as been removed). Pinus sylvestris seeds were spread after the soil scarification. The fire consumed all the understory vegetation and part of the soil organic layer. Since the fire herbaceous and shrub vegetation has been regenerating naturally and the seedlings of deciduous trees like Betula dominate the site. Although Pinus sylvestris seeds were spread there are few seedlings present at the site",
                                                            "Wildfire in July 2018 in a Pinus sylvestris plantation, trees were 10 years old at the time, all trees died and were left at the site. Pinus sylvestris seedlings (2 years old) were planted in 2020. Mostly herbaceous and shrubby vegetation has regenerated naturally at the site since the fire but vegetation cover is still sparse. The fire consumed the majority of the soil organic layer and the mineral layer is exposed in many areas across the site. Since the fire, more and more of the dead trees have fallen down every year. The planted pine seedlings are thriving at the site but we do not have an exact age estimate for them" ,
                                                            "Burn")~"Fire",
                                         disturbance %in% c("retrogressive thaw slump",
                                                            "Thaw")~ "Thaw",
                                         disturbance %in% "Lake was artificually drained in August 1978 and has been undergoing monitoring since"~ "Drained_Lake",
                                         disturbance %in% "Forestry"~ "Forestry",
                                         disturbance %in% c("No",
                                                            "No (protected National Park)")~"No"))
unique(late.additions$Disturbance_Category)
unique(late.additions$disturb_year)
unique(late.additions$disturb_severity)

unique(late.additions$site_activity)
late.additions <- late.additions %>%
  mutate(site_activity= case_when(site_activity %in% c("Non-active", "Non-Active")~"No",
                                  site_activity %in% "Active"~ "Yes"))


unique(late.additions$flux_method)
unique(late.additions$flux_method_detail)
unique(late.additions$flux_method_description)

unique(late.additions$diurnal_coverage)
late.additions <- late.additions %>%
  mutate(diurnal_coverage= ifelse(diurnal_coverage %in% "day", "Day", diurnal_coverage)) %>%
  mutate(diurnal_coverage= ifelse(diurnal_coverage %in% "day and night", "Day and Night", diurnal_coverage))

unique(late.additions$instrumentation)
unique(late.additions$soil_type_detail)

unique(late.additions$chamber_nr_measurement_days)
unique(late.additions$chamber_nr_measurement_days_ch4)
unique(late.additions$chamber_nr_measurement_days_co2)


unique(late.additions$gap_fill_perc)
unique(late.additions$gap_fill_perc_nee)
unique(late.additions$gap_fill_perc_ch4)
late.additions$gap_fill_perc <- NULL


dupes <- late.additions %>% get_dupes(site_name, site_reference,  year, month, partition_method)  


late.additions.ch <- late.additions %>% dplyr::filter(flux_method== "Chamber")
late.additions.ec <- late.additions %>% dplyr::filter(flux_method== "EC")


setwd("/Users/iwargowsky/Desktop/ABCFlux v2")
write_csv(late.additions.ch, "late.additions.ch.csv")
write_csv(late.additions.ec, "late.additions.ec.csv")

