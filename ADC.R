library(dplyr)
library(purrr)
library(readr)
library(data.table)
library(readxl)
library(lubridate)
library(zoo)
library(tidyr)
library(vctrs)
library(gdata)
library(tidyverse)
library(DataCombine)
library(janitor)

# This script is for processing datasets downloaded from https://arcticdata.io/catalog/data

###Carbon dioxide fluxes by the AmeriFlux network (dat1) NOT USING####
#Date ranges of these files are covered by those in Amerflux BASE 

# setwd("/Users/iwargowsky/Desktop/arcticdatacenter/downloads/ameriflux")
# path <- "/Users/iwargowsky/Desktop/arcticdatacenter/downloads/ameriflux"
# # AMF sites in Arctic Data Center
# files1 <- list.files(path = path,pattern = '*_HH_',all.files = T,recursive = T)
# #load in files as one df
# ameriflux <- files1 %>% setNames(nm = .) %>% 
#   map_df(~read_csv(.x, col_types = cols(), col_names = TRUE, skip=2,
#                    na=c("NA","-9999")), .id = "site_id")    
# #clean up site id column
# ameriflux$site_id <- substr(ameriflux$site_id, 5,10)
# #select columns to keep
# colnames(ameriflux) #see all column names
# ameriflux.1 <- ameriflux %>% dplyr::select(site_id, TIMESTAMP_START, 
#                                  NEE_PI_F, NEE_PI, RECO_PI_F, RECO_PI,
#                                  GPP_PI_F, TA, SC, TS_1, TS_2, TS,
#                                  P, SWC_1, SWC_2, SWC, PPFD_IN, D_SNOW)
# #add month and year columns
# ameriflux.1$year <- substr(ameriflux.1$TIMESTAMP_START,1,4)
# ameriflux.1$month <- substr(ameriflux.1$TIMESTAMP_START,5,6)
# #get means for each month
# ameriflux.monthly <-  group_by(ameriflux.1, year, month, site_id) %>% 
#   dplyr::summarise(tair= mean(TA, na.rm = TRUE),
#                    nee= mean(c(NEE_PI_F, NEE_PI), na.rm = TRUE),
#                    reco= mean(c(RECO_PI_F, RECO_PI), na.rm = TRUE), 
#                    gpp= mean(GPP_PI_F, na.rm = TRUE),
#                    tsoil_surface= mean(c(TS_1, TS), na.rm = TRUE),
#                    precip = sum(P),
#                    soil_moisture= mean(c(SWC_1, SWC), na.rm = TRUE),
#                    ppfd= mean(PPFD_IN, na.rm = TRUE),
#                    snow_depth= mean(D_SNOW, na.rm = TRUE))
# #create time stamp variable and multiple by number of days in each month and convert units
# ameriflux.monthly$ts = as.yearmon(paste(ameriflux.monthly$year,ameriflux.monthly$month,sep = '-'))
# ameriflux.monthly$nee <- ameriflux.monthly$nee * days_in_month(ameriflux.monthly$ts)*1.0368
# ameriflux.monthly$reco <- ameriflux.monthly$reco * days_in_month(ameriflux.monthly$ts)*1.0368
# ameriflux.monthly$gpp <- ameriflux.monthly$gpp * days_in_month(ameriflux.monthly$ts)*1.0368
# ameriflux.monthly$ts <- NULL
# #Adding in some metadata
# setwd("/Users/iwargowsky/Desktop/arcticdatacenter/downloads/ameriflux")
# path <- "/Users/iwargowsky/Desktop/arcticdatacenter/downloads/ameriflux"
# files1meta <- list.files(path = path,pattern = '*_BIF_',all.files = T,recursive = T)
# #load in files as a list of df
# meta1 <- files1meta %>%
#   setNames(nm = .) %>% 
#   map_df(~read_xlsx(.x))    
# #more to better format and group by site
# meta1.wide <- meta1 %>% pivot_wider(names_from = VARIABLE, values_from = DATAVALUE) 
# meta1.bysite <- meta1.wide %>% group_by(SITE_ID) %>% reframe(country= na.omit(COUNTRY),
#                                                            citation = na.omit(DOI),
#                                                            site_name= na.omit(SITE_NAME),
#                                                            latitude= na.omit(LOCATION_LAT),
#                                                            longitude= na.omit(LOCATION_LONG))
# #merge flux df and meta data
# meta1.bysite<- meta1.bysite %>% dplyr::rename(site_id= SITE_ID)
# dat1 <- left_join(ameriflux.monthly, meta1.bysite)

###Brown Lemming Herbivory Experiment Data (dat3) - Jessica Plein COMPLETED####-----------------------------------------------------------------------------
setwd("/Users/iwargowsky/Desktop/arcticdatacenter/downloads/Brown lemming herbivory")
#load data files
fluxes <- read_csv("Plein_et_al__Carbon_Flux_Data.csv")
enviro <- read_csv("Plein_et_al__Environmental_Data.csv") #has mutliple "date" columns so we'll split into 2 dfs
TDSM <- enviro %>% select('Year', 'Plot set', 'Treatment', 'Date_TDSM', 'Thaw Depth (cm)','Soil Moisture (%)') %>%
  dplyr::rename("Date" = "Date_TDSM")
STAT <- enviro %>% select('Year', 'Plot set', 'Treatment', 'Date_STAT', 'Soil Temp. (°C)', 'Air Temp. (°C)')%>%
  dplyr::rename("Date" = "Date_STAT")
ndvi <- read_csv("Plein_et_al__NDVI_Data.csv")
#merge data files
pleindat <- rbindlist(list(fluxes, STAT, TDSM, ndvi), fill = TRUE)
pleindat <- pleindat %>% filter(Treatment== "Control") %>% # we only want control plots
  mutate(year= year(as.Date(Date, format= "%d-%b-%y")), #adding year and month columns
         month= month(as.Date(Date, format= "%d-%b-%y")))
#renaming variables
pleindat <- pleindat %>% 
  dplyr::rename(ch4_flux_total= "CH4 flux \n(mgC - CH4 m-2h-1)",
                nee= "NEE                 \nCO2 flux  (light) \n(gC - CO2 m-2h-1)",
                gpp= "GPP\nCO2 flux            \n(gC - CO2 m-2h-1)",
                reco= "Respiration\nCO2 flux  (dark)\n(gC - CO2 m-2h-1)",
                tsoil_surface= "Soil Temp. (°C)",
                tair= "Air Temp. (°C)",
                thaw_depth= "Thaw Depth (cm)",
                soil_moisture="Soil Moisture (%)" )
#summarize by month
pleindat.monthly <- pleindat %>% group_by(year, month) %>%
  dplyr::summarise(ch4_flux_total= mean(ch4_flux_total, na.rm = TRUE),
            nee= mean(nee, na.rm = TRUE),
            gpp= mean(gpp, na.rm = TRUE),
            reco= mean(reco, na.rm = TRUE),
            tsoil_surface= mean(tsoil_surface, na.rm = TRUE),
            tair= mean(tair, na.rm = TRUE),
            thaw_depth= mean(thaw_depth, na.rm = TRUE ),
            soil_moisture= mean(soil_moisture, na.rm = TRUE),
            ndvi= mean(NDVI, na.rm = TRUE))
#convert units 
pleindat.monthly$ch4_flux_total <- pleindat.monthly$ch4_flux_total /1000 * 24 *days_in_month(as.yearmon(paste(pleindat.monthly$year,pleindat.monthly$month,sep = '-')))
pleindat.monthly$nee <- pleindat.monthly$nee * 24 *days_in_month(as.yearmon(paste(pleindat.monthly$year,pleindat.monthly$month,sep = '-')))
pleindat.monthly$gpp <- pleindat.monthly$gpp * 24 *-1 *days_in_month(as.yearmon(paste(pleindat.monthly$year,pleindat.monthly$month,sep = '-')))
pleindat.monthly$reco <- pleindat.monthly$reco * 24 *days_in_month(as.yearmon(paste(pleindat.monthly$year,pleindat.monthly$month,sep = '-')))

##adding in other vars
chamberdays <- read_csv("chamber_nr_measurement_days.csv")
pleindat.monthly <- full_join(pleindat.monthly, chamberdays, by= c('year', 'month'))
pleindat.monthly$site_name <- 'Utqiaġvik'
pleindat.monthly$site_reference <- 'Utqiaġvik plots aggregated'
pleindat.monthly$data_contributor_or_author <- 'Jessica Plein, Donatella Zona, Rulon Clark, Kyle Arndt, Walter Oechel, and Douglas Stow'
pleindat.monthly$site_id <- 'Plein_Utqiaġvik_agg'
pleindat.monthly$email <- 'jlplein@sdsu.edu'
pleindat.monthly$extraction_source <- 'Arctic Data Center/Publication'
pleindat.monthly$citation <- 'DOI:10.18739/A2S17ST8F; https://doi.org/10.5194/bg-19-2779-2022, 2022'
pleindat.monthly$country <- 'USA'
pleindat.monthly$latitude <- '71.322527'
pleindat.monthly$longitude <- '-156.60917'
pleindat.monthly$flux_method <- 'Chamber'
pleindat.monthly$flux_method_detail <- 'Manual chamber'
pleindat.monthly$flux_method_description <- "We built a clear plexiglass acrylic chamber (Davidson et al., 2016; McEwing et al., 2015) and this chamber was placed on a metal frame positioned in the ground outside of the plots and had clear polyvinyl material weighed down by heavy metal chains to produce a seal inside the chamber. These measurements were performed in a closed loop, where tubes connected the chamber to the gas analyzer and then air was circulated back to the chamber. We positioned a small fan inside the chamber to ensure appropriate air mixing."
pleindat.monthly$instrumentation <- 'Los Gatos Research (LGR) Ultraportable Greenhouse Gas Analyzer (UGGA Model 915-0011)'
pleindat.monthly$gap_fill <- 'Average'
pleindat.monthly$veg_detail <- 'The major vegetation type at this site is graminoid-dominated wetlands, consisting of mosses, lichens, graminoids (grasses), and wet sedges'
pleindat.monthly$sedge <- "Present"
pleindat.monthly$non_sedge_herbaceous <- "Present"
pleindat.monthly$sphagnum_cover <- "Present"
pleindat.monthly$other_moss_cover <- "Present"
pleindat.monthly$permafrost <- 'Yes'
pleindat.monthly$biome <- "Tundra"
pleindat.monthly$land_cover <- "180"
pleindat.monthly$land_cover_bawld <- "PermWet"
pleindat.monthly$diurnal_coverage <- "Day"
#remove the last row that doesn't contain data
pleindat.monthly <- pleindat.monthly[(!is.na(pleindat.monthly$year)), ]

dat3 <- pleindat.monthly
###Thule experimental warming Data (dat4) - Robert Jespersen COMPLETED####-----------------------------------------------------------------------------
setwd("/Users/iwargowsky/Desktop/arcticdatacenter/downloads/Thule experimental warming")
flux <- read_csv("Thule_flux_2018.csv")
flux <- flux %>% filter(trt=='C') %>% #filter for only control plots
  mutate(year= year(as.Date(datetime, format= "%m/%d/%Y")),
         month= month(as.Date(datetime, format= "%m/%d/%Y")),
         day= day(as.Date(datetime, format= "%m/%d/%Y")))
thule.monthly <- flux %>% group_by(year, month)%>% 
  reframe(nee= mean(flux, na.rm = TRUE),
          chamber_nr_measurement_days= n_distinct(day))
#convert units umol m-2 s-1 to g C m-2 month-1
thule.monthly$nee <- thule.monthly$nee*1.0368*days_in_month(as.yearmon(paste(thule.monthly$year,thule.monthly$month,sep = '-')))
#Adding in static vars
thule.monthly$site_name <- "Thule"
thule.monthly$site_reference <- "Thule chambers aggregated"
thule.monthly$data_contributor_or_author <- "Robert Jespersen, Jeffrey Welker, Maria Vaisanen, and A Joshua Leffler"
thule.monthly$site_id <- "Jesperson_Thule_agg"
thule.monthly$email <- "gusjespers@gmail.com"
thule.monthly$extraction_source <- "Arctic Data Center/Publication"
thule.monthly$citation <- "DOI:10.18739/A2KH0F09B; DOI: 10.1111/gcb.16027"
thule.monthly$country <-"Greenland"
thule.monthly$latitude <- '76.55'
thule.monthly$longitude <- '-68.566667'
thule.monthly$flux_method <- "Chamber"
thule.monthly$flux_method_detail <- "Automatic chambers"
thule.monthly$flux_method_description <- "Transparent chambers were attached to 20 cm diameter PVC collars installed in the vegetated part of the experimental plots to 5 cm depth"
thule.monthly$instrumentation <- "chambers (model 8100-104C, Li-Cor Inc., Lincoln, NE, USA),manifold (model 8150, Li-Cor Inc., Lincoln, NE, USA),infrared gas analyzer (IRGA; model 8100 A/81 50, Licor, Inc., Lincoln, NE)  "
thule.monthly$gap_fill <- "Average"
thule.monthly$veg_detail <- "The main vascular plant species are Dryas integrifolia (an evergreen dwarf shrub), Salix  arctica (a deciduous shrub), and Carex  rupestris (a graminoid), that together comprise 93% of the vascular vegetative cover (Sharp and Sullivan, unpublished data)."
thule.monthly$canopy_height <- "0.15"
thule.monthly$soil_type_detail <- "The soils are 54%–64% sand, 33%– 38% silt, and 3%–7% clay (Sullivan et al., 2008)."
thule.monthly$dec_shrub <- "Present"
thule.monthly$ev_shrub <- "Present"
thule.monthly$sedge <- "Present"
thule.monthly$biome <- "Tundra"
thule.monthly$land_cover <- "120"
thule.monthly$land_cover_bawld <- "Dry tundra"
thule.monthly$diurnal_coverage <- "Day and Night"
dat4 <- thule.monthly
###Caribou-Poker Creek chambers (dat5) - Jennifer Watts COMPLETED####-----------------------------------------------------------------------------
setwd("/Users/iwargowsky/Desktop/arcticdatacenter/downloads/Caribou-Poker Creek chambers")
flux <- read_csv("Clear_Chamber_Fluxes.csv")
CPCflux <- flux %>% filter(light_or_dark=='D') %>% #filter for only control plots
  mutate(year= year(as.Date(date, format= "%m/%d/%Y")),
         month= month(as.Date(date, format= "%m/%d/%Y")),
         day= day(as.Date(date, format= "%m/%d/%Y")),
         site_reference= case_when(plot %in% c(1:6)~ "lichen",
                                   plot %in% c(7:13)~ "deep_moss",
                                   plot %in% c(14:21)~ "mosaic"))
CPC.monthly <- CPCflux %>% group_by(year, month, site_reference)%>% 
  reframe(ch4_flux_total= mean(CH4_flux, na.rm = TRUE), #only taking ch4 because understory measurements
          ppfd= mean(PAR_chamber, na.rm = TRUE),
          soil_moisture= mean(soil_moisture_12cm, na.rm = TRUE),
          tsoil_deep= mean(soil_temp_12cm),
          chamber_nr_measurement_days= n_distinct(day),
          tsoil_deep_depth = "12",
          moisture_depth= "12")
#convert units umol m-2 s-1 to g C m-2 month-1
CPC.monthly$ch4_flux_total <- CPC.monthly$ch4_flux_total*0.0010368*days_in_month(as.yearmon(paste(CPC.monthly$year, CPC.monthly$month,sep = '-')))
#Adding in static vars
CPC.monthly <- CPC.monthly %>% mutate(ev_needle_tree = case_when(site_reference %in% c('lichen','deep_moss')~ "Dominant"),
                                      dec_broad_tree= case_when(site_reference %in% 'mosaic'~ "Present"),
                                      dec_needle_tree= case_when(site_reference %in% 'mosaic'~ "Present"),
                                      soil_depth= case_when(site_reference %in% 'lichen'~ "8.5-18.5cm",
                                                            site_reference %in% 'deep_moss'~ "21-35cm",
                                                            site_reference %in% 'mosaic'~ "5.5-15cm"))
CPC.monthly$site_name <- "Caribou-Poker Creek "
CPC.monthly$data_contributor_or_author <- "Jennifer Watts and Jonas Noomah"
CPC.monthly$site_id <- paste("Watts_Caribou-Poker-Creek_",CPC.monthly$site_reference, "_agg")
CPC.monthly$email <- "jwatts@woodwellclimate.org"
CPC.monthly$extraction_source <- "Arctic Data Center"
CPC.monthly$citation <- "DOI:10.18739/A2P55DJ3K"
CPC.monthly$country <- "USA"
CPC.monthly$latitude <- "65.1574753"
CPC.monthly$longitude <- "-147.4997501"
CPC.monthly$flux_method <- "Chamber"
CPC.monthly$flux_method_detail <- "Manual closed chamber"
CPC.monthly$flux_method_description <- "The clear chamber is a 50cmx50cmx50cm box with an open bottom made of acrylic sheets connected with corner braces and sealed with silicone and rubber washers. A custom-made gasket consisting of aluminum angle and foam weatherproofing tape runs along the bottom edge of the chamber, allowing it to form a seal with the collar. Inside the chamber are a fan to circulate air, a shielded air temperature sensor (ONSET HOBO® Temperature/Relative Humidity Smart Sensor) at ground level, a photosynthetic light sensor (ONSET HOBO® Photosynthetic light (PAR) Smart Sensor) at the top of the chamber, and a data logger (ONSET HOBO® H21-USB Microstation) attached to the sensors. An open-design pressure valve consisting of 6m of coiled ⅛” tubing allows the chamber pressure to equilibrate with the outside pressure, but minimizes CO2 and CH4 diffusion through the valve"
CPC.monthly$instrumentation <- "LICOR LI-7810 Gas Analyzer"
CPC.monthly$gap_fill <- "Average"
CPC.monthly$veg_detail <- "A black spruce-dominated canopy at the lower elevations (Plots 1-13) and a deciduous-dominated canopy at the higher elevations (Plots 14-21). Ground cover varied along the transect. Plots 1-6 are lichen dominated, while plots 7-13 are completely covered by deep moss. Plots 14-21 are a mosaic of shallow moss and litter-covered bare ground. The woody evergreens Vaccinium vitis-idaea and Rhododendron groenlandicum are common across the entire transect and can grow densely. Small forbs (including Cornus unalaskensis and Geocaulon lividum) and graminoids are common but sparse. Shrubby Salix spp. can be found throughout the transect, while Betula nenana/Betula glandulosa can be found among the black spruce"
CPC.monthly$dec_shrub <- "Present"
CPC.monthly$ev_shrub <- "Present"
CPC.monthly$non_sedge_herbaceous <- "Present"
CPC.monthly$biome <- "Boreal"
CPC.monthly$diurnal_coverage <- "Day"
CPC.monthly$land_cover <- "90"
CPC.monthly$land_cover_bawld <- "Boreal forest"
dat5 <- CPC.monthly
###Tutakoke Field Site Trace Gas fluxes (dat6) - Jeffrey Welker COMPLETED####-----------------------------------------------------------------------------
setwd("/Users/iwargowsky/Desktop/arcticdatacenter/downloads/Tutakoke chambers")
co2flux <- read_csv("Tutakoke_CO2_data_2015.csv")
ch4flux <- read_csv("Tutakoke_CH4_N2O_2015.csv")
tutakoke <- full_join(co2flux, ch4flux, by = c("doy", "ID1","ID2","ID3"))
tutakoke <- tutakoke %>% mutate( year = '2015', 
                                 month= month(as.Date(doy, origin= "2014-12-31")),
                                 day= day(as.Date(doy, origin= "2014-12-31"))) #add in time vars
tutakoke.monthly <- tutakoke %>% group_by(year, month, ID3) %>% #summarise by month and group by plot veg type
  dplyr::summarise(nee= mean(Flux[lt_dk=='0'], na.rm = TRUE),
            reco= mean(Flux[lt_dk=='1'], na.rm = TRUE),
            ch4_flux_total= mean(CH4flux, na.rm = TRUE),
            tsoil_surface= mean(SoilT, na.rm = TRUE),
            chamber_nr_measurement_days= n_distinct(day),
            water_table_depth= mean(WaterLevel, na.rm = TRUE),
            canopy_height= mean(VegHtcm, na.rm = TRUE)/100)%>% #convert to cm to m
  mutate(gpp= nee-reco)
#convert units umol m-2 s-1 to g C m-2 month-1
tutakoke.monthly$nee <- tutakoke.monthly$nee*1.0368*days_in_month(as.yearmon(paste(tutakoke.monthly$year,tutakoke.monthly$month,sep = '-')))
tutakoke.monthly$gpp <- tutakoke.monthly$gpp*1.0368*days_in_month(as.yearmon(paste(tutakoke.monthly$year,tutakoke.monthly$month,sep = '-')))
tutakoke.monthly$reco <- tutakoke.monthly$reco*1.0368*days_in_month(as.yearmon(paste(tutakoke.monthly$year,tutakoke.monthly$month,sep = '-')))
tutakoke.monthly$ch4_flux_total <- tutakoke.monthly$ch4_flux_total*1.0368*days_in_month(as.yearmon(paste(tutakoke.monthly$year,tutakoke.monthly$month,sep = '-')))
#ID3 refers to veg type so we'll change values to reflect this
tutakoke.monthly <- tutakoke.monthly %>% dplyr::rename(site_reference= ID3)
tutakoke.monthly$site_reference[tutakoke.monthly$site_reference == "1"] <- "pond margin"
tutakoke.monthly$site_reference[tutakoke.monthly$site_reference == "2"] <- "grazing lawn"
tutakoke.monthly$site_reference[tutakoke.monthly$site_reference == "3"] <- "Carex meadows"
tutakoke.monthly$site_reference[tutakoke.monthly$site_reference == "4"] <- "slough levee"
#adding in climate data
climate <- read_csv("TutakokeWeatherData2015.csv")
climate <- climate %>% dplyr::rename(year= Year) %>%
  mutate(month= month(as.Date(Date_Time, format= "%m/%d/%y")))
climate.monthly <- climate %>% group_by(year, month) %>%
  dplyr::summarise(tair= mean(Ave_AirT_DegC, na.rm = TRUE),
            ppfd= mean(`PAR_Avg_umol/s/m^2`, na.rm = TRUE),
            precip= sum(Rain_mm))
climate.monthly <- climate.monthly %>% filter(!month==8)
climate.monthly <-climate.monthly %>% mutate(year= as.numeric(year))
tutakoke.monthly <- tutakoke.monthly %>% mutate(year= as.numeric(year))
tutakoke.monthly <- full_join(tutakoke.monthly, climate.monthly)
#Adding in static vars
tutakoke.monthly$site_name <- "Tutakoke Field Site"
tutakoke.monthly$data_contributor_or_author <- "Jeffrey Welker"
tutakoke.monthly$site_id <- paste("Welker_Tutakoke_",tutakoke.monthly$site_reference, "_agg")
tutakoke.monthly$email <- "jmwelker@uaa.alaska.edu"
tutakoke.monthly$extraction_source <- "Arctic Data Center"
tutakoke.monthly$citation <- "DOI:10.18739/A29S1KK98"
tutakoke.monthly$country <- "USA"
tutakoke.monthly$latitude <- "61.416667"
tutakoke.monthly$longitude <- "-165.833333"
tutakoke.monthly$flux_method <- "Chamber"
tutakoke.monthly$flux_method_detail <- "manual chamber"
tutakoke.monthly$flux_method_description <- "Gas measurements were made using a translucent PVC chamber 21 cm tall and 13 cm in diameter with a 4.2 cm wide flange at the base that was lined with a rubber gasket. Flux measurements were performed using the translucent chamber first to obtain a measurement of net ecosystem exchange (NEE). Once the chamber was removed from the collar following the NEE measurement, and CO2 concentration and relative humidity in the chamber returned to ambient conditions, a second measurement was performed with the chamber covered in opaque cloth to obtain a measurement of ecosystem respiration (ER). Gross primary productivity (GPP) was calculated as the difference between ER and NEE. "
tutakoke.monthly$instrumentation <- "Licor 820 Infrared gas analyzer (for CO2) Picarro Model G2308 gas analyzer (for CH4)"
tutakoke.monthly$gap_fill <- "Average"
tutakoke.monthly$veg_detail <- "The study site is located in the active floodplain portion of the delta where tidally influenced sloughs dissect wet sedge and graminoid meadows."
tutakoke.monthly$biome <- "Tundra"
tutakoke.monthly$permafrost <- "No"
tutakoke.monthly$partition_method <- "GPP= NEE-RECO"
tutakoke.monthly$soil_depth <- "0"
tutakoke.monthly$land_cover <- "180"
tutakoke.monthly$land_cover_bawld <- ""
tutakoke.monthly$diurnal_coverage <- "Day"
dat6 <- tutakoke.monthly
###Barrow chambers 2015 (dat7) - Robert Wagner COMPLETED####-----------------------------------------------------------------------------
setwd("/Users/iwargowsky/Desktop/arcticdatacenter/downloads/Barrow chambers 2015")
barrow <- read_csv("2019.05.15.data.all.csv")
barrow <- barrow %>% mutate(year= year(as.Date(date, format= "%m/%d/%y")),
                            month= month(as.Date(date, format= "%m/%d/%y")),
                            day= day(as.Date(date, format= "%m/%d/%y")))
#remove rows that do not contain flux data
barrow <- barrow[!is.na(barrow$ch4.rep1.flux.ug.m2.hour),]
#summarise
barrow.monthly <- barrow %>% group_by(year, month, chamber.type) %>%
 dplyr::summarise(ch4_flux_total= mean(c(ch4.rep1.flux.ug.m2.hour, ch4.rep2.flux.ug.m2.hour), na.rm= TRUE),
            thaw_depth= mean(thaw.depth.cm, na.rm = TRUE),
            water_table_depth= mean(water.table.cm, na.rm = TRUE),
            latitude= mean(dgps.lat),
            longitude= mean(dgps.long),
            chamber_nr_measurement_days= n_distinct(day))
#change units from ug m-2 hr-1
barrow.monthly$ch4_flux_total <- barrow.monthly$ch4_flux_total*24*0.000001*days_in_month(as.yearmon(paste(barrow.monthly$year, barrow.monthly$month,sep = '-')))
#change units from mm to cm
barrow.monthly$thaw_depth <- barrow.monthly$thaw_depth/100
barrow.monthly$water_table_depth <- barrow.monthly$water_table_depth/100
#chambers are grouped by location (chamber.type) this will be site_reference
barrow.monthly <- barrow.monthly %>% dplyr::rename(site_reference= chamber.type)
barrow.monthly$site_reference[barrow.monthly$site_reference == "fc"] <- "Flat Center"
barrow.monthly$site_reference[barrow.monthly$site_reference == "lc"] <- "Low Center"
barrow.monthly$site_reference[barrow.monthly$site_reference == "r"] <- "Rim"
barrow.monthly$site_reference[barrow.monthly$site_reference == "t"] <- "Trough"
#Adding in static vars
barrow.monthly$site_name <- "Barrow"
barrow.monthly$email <- "robertleafwagner@gmail.com"
barrow.monthly$data_contributor_or_author <- "Robert Wagner"
barrow.monthly$site_id <- paste("Wagner_Barrow_",barrow.monthly$site_reference, "_agg")
barrow.monthly$extraction_source <- "Arctic Data Center"
barrow.monthly$citation <- "DOI:10.18739/A23T9D62D"
barrow.monthly$country <- "USA"
barrow.monthly$flux_method <- "Chamber"
barrow.monthly$instrumentation <- "LGR (Los Gatos Research) ultra-portable gas analyzer"
barrow.monthly$gap_fill <- "Average"
barrow.monthly$biome <- "Tundra"
barrow.monthly$diurnal_coverage <- "Day"


dat7 <- barrow.monthly
###Barrow chambers 2012 (dat8) -K Miller COMPLETED ####-----------------------------------------------------------------------------
setwd("/Users/iwargowsky/Desktop/arcticdatacenter/downloads/Barrow chambers 2012")
friedmanco2 <- read_csv("CO2_Flux_Friedman2012.csv") %>% filter(Collar=="Control")
friedmanch4 <- read_csv("CH4_Flux_Friedman2012.csv") %>% filter(Collar=="Control")
friedman <- full_join(friedmanch4, friedmanco2)
#summarise by month
friedman.monthly <- friedman %>% filter(Collar=="Control") %>%
  mutate(year= year(as.Date(Date, format= "%m/%d/%y")), 
         month= month(as.Date(Date, format= "%m/%d/%y")),
         day= day(as.Date(Date, format= "%m/%d/%y"))) %>% 
  dplyr::rename(site_reference= Polygon) %>%
  group_by(year, month, site_reference) %>% 
  dplyr::summarise(ch4_flux_total= mean(CH4_Flux, na.rm = TRUE),
                   nee= mean(C02_Flux, na.rm = TRUE),
                   chamber_nr_measurement_days= n_distinct(day))
#change units from mgC/m2/h to gC/m2/month
friedman.monthly$ch4_flux_total <- friedman.monthly$ch4_flux_total/1000*24*days_in_month(as.yearmon(paste(friedman.monthly$year, friedman.monthly$month,sep = '-')))
friedman.monthly$nee <- friedman.monthly$nee/1000*24*days_in_month(as.yearmon(paste(friedman.monthly$year, friedman.monthly$month,sep = '-')))
#adding in static vars
friedman.monthly$site_name <- "Barrow"
friedman.monthly$site_id <- paste("Friedman_Barrow_agg_",friedman.monthly$site_reference)
friedman.monthly$email <- "support@arcticdata.io"
friedman.monthly$data_contributor_or_author <- "K Miller, E Friedman, L Angenent, and D A Lipson"
friedman.monthly$extraction_source <- "Arctic Data Center"
friedman.monthly$citation <- "DOI:10.18739/A2T727H0D"
friedman.monthly$country <- "USA"
friedman.monthly$biome <- "Tundra"
friedman.monthly$latitude <- "71.294"
friedman.monthly$longitude <- "-156.602"
friedman.monthly$flux_method <- "Chamber"
friedman.monthly$instrumentation <- "Los Gatos Research portable trace gas analyzer"
friedman.monthly$gap_fill <- "Average"

dat8 <- friedman.monthly
###Pleistocene Park (dat9) - Syndonia Bret-Harte check if FC= NEE####-----------------------------------------------------------------------------
#Data is already in ABCFlux v1

###Barrow towers (dat10) -Eric Wilkman ####-----------------------------------------------------------------------------
setwd("/Users/iwargowsky/Desktop/arcticdatacenter/downloads/Barrow Towers")
wilkman.vwc <- read_csv("BES_WC_1.csv")
wilkman.05 <- read_csv("BES_All_2005_Line_3.csv")
wilkman.06 <- read_csv("BES_All_2006_Line_4.csv")
wilkman.flux <- read_csv("BES_eddy_2005_2007_running_2.csv")
# Convert Julian day to date
wilkman.05$Date <- as.Date(wilkman.05$`Julian Date`, origin = as.Date("2004-12-31"))
wilkman.06$Date <- as.Date(wilkman.06$`Julian Date`, origin = as.Date("2005-12-31"))
#merging dfs 
wilkman <- full_join(wilkman.vwc, wilkman.flux, by= 'Julian Date') 
wilkman <- wilkman %>% mutate(Date= as.Date(wilkman$Date, format = '%m/%d/%y')) %>%
  full_join(wilkman.05) %>% full_join(wilkman.06)
#add time columns
wilkman <- wilkman %>% mutate(year= year(wilkman$Date),
                              month= month(wilkman$Date))
#replace soil depth to remove units
Replaces <- data.frame(from = c("Surface", "10cm", "7cm", "30cm", "15cm", "20cm", "40cm"), 
                       to = c("0", "10", "7", "30", "15", "20", "40"))
wilkman <- FindReplace(data = as.data.frame(wilkman), Var = "Depth", replaceData = Replaces,
                                         from = "from", to = "to", exact = FALSE)
#summarise soil info by month 
wilkman.soilmonthly <- wilkman %>% group_by(year, month, Depth) %>%
  dplyr::summarise(soil_moisture= mean(Saturation, na.rm = TRUE),
            tsoil= mean(Temperature, na.rm = TRUE),
            thaw_depth= mean(Thaw, na.rm = TRUE))%>%
  mutate(tsoil_surface= case_when(Depth %in% c("0","10","7")~ tsoil),
         tsoil_deep= case_when(Depth %in% c("30","15","20","40")~ tsoil),
         tsoil_surface_depth= case_when(Depth %in% c("0","10","7")~ Depth),
         tsoil_deep_depth= case_when(Depth %in% c("30","15","20","40")~ Depth))%>% 
  group_by(year, month) %>%
  dplyr::summarise(soil_moisture= mean(as.numeric(soil_moisture), na.rm = TRUE),
                   tsoil_surface= mean(as.numeric(tsoil_surface), na.rm = TRUE),
                   tsoil_surface_depth= mean(as.numeric(tsoil_surface_depth), na.rm = TRUE),
                   tsoil_deep= mean(as.numeric(tsoil_deep), na.rm = TRUE),
                   tsoil_deep_depth= mean(as.numeric(tsoil_deep_depth), na.rm = TRUE),
                   thaw_depth= mean(as.numeric(thaw_depth), na.rm = TRUE))
wilkman.soilmonthly$Depth <- NULL
wilkman.soilmonthly$tsoil <- NULL
#summarise flux by month 
wilkman.monthly <- wilkman %>% group_by(year, month) %>%
  dplyr::summarise(nee= mean(CO2_flx_gC_d, na.rm = TRUE),
            percent_na_nee = (sum(is.na(CO2_flx_gC_d))/n()*100))
#merge together
wilkman.monthly <- merge(wilkman.monthly, wilkman.soilmonthly, by= c("year", "month"))
#convert units
wilkman.monthly$nee <- wilkman.monthly$nee *days_in_month(as.yearmon(paste(wilkman.monthly$year, wilkman.monthly$month,sep = '-')))

#adding static info
wilkman.monthly$site_reference <- "US-Bes"
wilkman.monthly$site_name <- "Barrow-BES"
wilkman.monthly$email <- "ewilkman-w@sdsu.edu"
wilkman.monthly$data_contributor_or_author <- "Eric Wilkman"
wilkman.monthly$citation <- "doi:10.18739/A20P0WR91"
wilkman.monthly$country <- "USA"
wilkman.monthly$latitude <- "71.280881"
wilkman.monthly$longitude <- "-156.596467"
wilkman.monthly$flux_method <- "EC"
wilkman.monthly$instrumentation <- "LI-7500"
wilkman.monthly$flux_method_detail <- "EC_open"
wilkman.monthly$gap_fill <- "Monthly Averages from non-gapfilled data"

dat10<- wilkman.monthly
###Greenhouse gas flux (dat2) - Donatella Zona####-----------------------------------------------------------------------------
#Note: I changed the names of files  to be more consistent and descriptive
# I also manually added year and month columns in excel to files that did not already have them
setwd("/Users/iwargowsky/Desktop/arcticdatacenter/downloads/Zona sites")
path <- "/Users/iwargowsky/Desktop/arcticdatacenter/downloads/Zona sites"
files2 <- list.files(path = path,pattern= "*_fluxes", all.files = T,recursive = T)
#load in files as a list of df
zona <-  lapply(files2,function(i){
  fread(i, na.strings =c("NA","-9999"), skip= 1)
})
#subset
zona.2 <- lapply(zona, function(df) df %>%
                   dplyr::select(year, month, ch4_flux,
                                 air_temperature, co2_flux))
#convert column classes
zona.2 <- lapply(zona.2, function(df) df %>%
                  mutate( ch4_flux= as.character(ch4_flux),
                           co2_flux= as.character(co2_flux),
                           air_temperature= as.character(air_temperature)))
#turn list  into one df
names(zona.2)<- substr(files2, 1,3) 
zona.dat <- bind_rows(zona.2, .id = "site_id")

#remove outliers
zona.dat <- zona.dat %>%
  mutate(co2_flux= ifelse(as.numeric(co2_flux) >1000, NA, co2_flux),
         ch4_flux= ifelse(as.numeric(ch4_flux) >100, NA, ch4_flux)) %>%
  mutate(co2_flux= ifelse(as.numeric(co2_flux) < -1000, NA, co2_flux),
         ch4_flux= ifelse(as.numeric(ch4_flux) < -100, NA, ch4_flux),
         air_temperature= ifelse(as.numeric(air_temperature) >500, NA, air_temperature))
#monthly means
zona.monthly <- group_by(zona.dat, year, month, site_id) %>% 
  dplyr::summarise( ch4_flux_total = mean(as.numeric(ch4_flux), na.rm = TRUE),
                    percent_na_ch4 = (sum(is.na(ch4_flux))/n()*100),
                    nee = mean(as.numeric(co2_flux), na.rm = TRUE),
                    percent_na_nee = (sum(is.na(co2_flux))/n()*100),
                    tair = mean(as.numeric(air_temperature), na.rm = TRUE))
#remove rows without flux data
zona.monthly <- zona.monthly %>% dplyr::filter(if_any(c('nee', 'ch4_flux_total'), ~ !is.na(.)))
#time formats
#convert units umol m-2 s-1 to g C m-2 month-1
zona.monthly$nee <- zona.monthly$nee *1.0368*days_in_month(as.yearmon(paste(zona.monthly$year,zona.monthly$month ,sep = '-')))
zona.monthly$ch4_flux_total <- zona.monthly$ch4_flux_total *1.0368*days_in_month(as.yearmon(paste(zona.monthly$year,zona.monthly$month,sep = '-')))
zona.monthly$tair <- zona.monthly$tair -273.15
#remove NEE fluxes less than -10 for Dec- Feb
zona.monthly <- zona.monthly %>%
  mutate(nee= ifelse(nee < -10 & month %in% c(12,1,2), NA, nee)) 



#Change site_id names
zona.monthly <- zona.monthly %>%
  mutate(site_id= case_when(site_id %in% 'ATQ'~ "US-Atq",
                            site_id %in% 'BES'~ "US-Bes",
                            site_id %in% 'BEO'~ "US-Beo",
                            site_id %in% 'CMD'~ "US-Brw",
                            site_id %in% 'IVO'~ "US-Ivo"))
#####adding in meteorological vars
##There is not a consistent naming format for these data between sites
#ATQ 
ATQ1 <- read_csv("ATQ_meteo_2013-2017.csv", na= c("-6999", "-9999", "6999")) #these two have the same format so I'll process them together
ATQ3 <- read_csv("ATQ_meteo_2019_2022.csv", na= c("-6999", "-9999", "6999"))
ATQ1.3 <- rbindlist(list(ATQ1, ATQ3), fill= TRUE )%>% 
  select(year, month, `PAR_AVG  L`,  `Soil_1_AVG  L`,`Soil_2_AVG  L`, 
         `Air_T_AVG  L`, `PPT_TOT  L`, `SWC_1_AVG  L`, `SnowDepth  L`) %>%
  dplyr::rename(ppfd=`PAR_AVG  L`, tsoil_surface= `Soil_1_AVG  L`,
                tsoil_deep = `Soil_2_AVG  L`, tair=`Air_T_AVG  L`, precip= `PPT_TOT  L`, 
                soil_moisture= `SWC_1_AVG  L`, snow_depth = `SnowDepth  L`)
ATQ2 <- read_csv("ATQ_meteo_2018_2019.csv", na= c("-6999", "-9999", "6999")) %>% 
  select(year, month, PAR_AVG_L, Soil_1_AVG_L, Soil_2_AVG_L, Air_T_AVG_L, 
         PPT_TOT_L, SWC_1_AVG_L, SnowDepth_L) %>%
  dplyr::rename(ppfd= PAR_AVG_L, tsoil_surface=Soil_1_AVG_L, 
                tsoil_deep= Soil_2_AVG_L, tair= Air_T_AVG_L, precip= PPT_TOT_L,
                soil_moisture=SWC_1_AVG_L, snow_depth=SnowDepth_L ) 
ATQ <- rbindlist(list(ATQ1.3, ATQ2), fill= TRUE)%>%  
  mutate(tsoil_surface_depth= '5', tsoil_deep_depth= '15', moisture_depth= '5', site_id= 'US-Atq')
#BEO 
BEO1 <- read_csv("BEO_meteo_2013_2017.csv", na= c("-999999", "99999") ) #all have same format
BEO2 <- read_csv("BEO_meteo_2017_2019.csv", na= c("-999999", "99999"))
BEO3 <- read_csv("BEO_meteo_2019_2022.csv", na= c("-999999", "99999"))
BEO <- rbindlist(list(BEO1, BEO2, BEO3), fill= TRUE) %>%
  select(year, month, PAR_Up_AVG, HMP_AirT_AVG, ST_1_AVG, ST_2_AVG, 
         Rain_tot, `Snow depth sensor`, `SCW_High-10cm`) %>%
  dplyr::rename(ppfd= PAR_Up_AVG, tair= HMP_AirT_AVG, 
                tsoil_surface= ST_1_AVG, tsoil_deep= ST_2_AVG, precip= Rain_tot,
                soil_moisture= `SCW_High-10cm`, snow_depth=`Snow depth sensor`) %>%
  mutate(tsoil_surface_depth= '10', tsoil_deep_depth= '20', moisture_depth= '10', site_id= 'US-Beo')
#BES 
BES1 <- read_csv("BES_meteo_2013_2017.csv", na= c("-999999", "99999")) #these two have the same format so I'll process them together
BES3 <- read_csv("BES_meteo_2017_2019.csv", na= c("-999999", "99999"))
BES1.3 <- rbindlist(list(BES1, BES3), fill= TRUE )%>%
 select(year, month, PAR_Up_AVG, HMP_AirT_AVG, ST_1_AVG, ST_2_AVG,
         Rain_tot, `Snow depth sensor`, `SCW_High-10cm`)%>%
  dplyr::rename(ppfd= PAR_Up_AVG, tair= HMP_AirT_AVG, 
                tsoil_surface= ST_1_AVG, tsoil_deep= ST_2_AVG, precip= Rain_tot,
                soil_moisture= `SCW_High-10cm`, snow_depth=`Snow depth sensor`) 
BES2 <- read_csv("BES_meteo_2019_2022.csv", na= c("-999999", "99999")) %>%
  select(year, month, HMP_AirT_AVG, SoilD_T_1_AVG, SoilD_T_2_AVG, PAR_In_AVG, VWC_1_AVG) %>%
  dplyr::rename(ppfd= PAR_In_AVG, tair= HMP_AirT_AVG, tsoil_surface=  SoilD_T_1_AVG, 
                tsoil_deep= SoilD_T_2_AVG, soil_moisture= VWC_1_AVG)
BES <- rbindlist(list(BES1.3, BES2), fill= TRUE) %>%
  mutate(tsoil_surface_depth= '10', tsoil_deep_depth= '20', moisture_depth= '10', site_id= 'US-Bes')
#CMDL
CMDL1 <- read_csv("CMDL_meteo_2013-2017.csv",locale=locale(encoding="latin1"), na= c("-6999", "-9999") )
CMDL3 <- read_csv("CMDL_meteo_2019_2022.csv",locale=locale(encoding="latin1"), na= c("-6999", "-9999"))
CMDL1.3 <- rbindlist(list(CMDL1, CMDL3), fill= TRUE) %>%
  select(year, month, PAR_AVG, Air_T_AVG, tsoil_surface, tsoil_deep, soil_moisture, PPT) %>%
  #note: I had to change column names in excel bc R wouldn't recognize original names
  #soil_moisture= "SWC1_0_5_AVG  L", tsoil_surface= "SoilT1_5_AVG  L", tsoil_deep= "SoilT1_15_AVG  L"
  dplyr::rename(ppfd= PAR_AVG, tair= Air_T_AVG, precip= PPT)
CMDL2 <- read_csv("CMDL_meteo_2018_2019.csv", na= c("-6999", "-9999") )%>%
  select(year, month, PAR_AVG, Air_T_AVG, PPT, SWC1_0_10_AVG_L, SoilT1_5_AVG_L,
         SoilT1_15_AVG_L,SnowDepth_AVG ) %>%
  dplyr::rename(ppfd= PAR_AVG, tair= Air_T_AVG, precip= PPT, soil_moisture= SWC1_0_10_AVG_L,
                tsoil_surface= SoilT1_5_AVG_L, tsoil_deep= SoilT1_15_AVG_L, snow_depth= SnowDepth_AVG) 
CMDL <- rbindlist(list(CMDL1.3, CMDL2) , fill= TRUE)%>%
  mutate(tsoil_surface_depth= '5', tsoil_deep_depth= '15', moisture_depth= '10', site_id= "US-Brw")
#IVO
IVO1 <- read_csv("IVO_meteo_2013_2017.csv") 
IVO1.1 <- as.data.frame(lapply(IVO1, as.numeric))
IVO2 <- read_csv("IVO_meteo_2017_2019.csv")
IVO2.1 <- as.data.frame(lapply(IVO2, as.numeric))
IVO3 <- read_csv("IVO_meteo_2019_2021.csv")
IVO3.1 <- as.data.frame(lapply(IVO3, as.numeric))
IVO <- bind_rows(IVO1.1, IVO2.1, IVO3.1) %>% 
  select(year, month, PAR_in_Avg, Tair_hmp_Avg, Rain_Tot, SoilT1_5_Avg,
         SoilT1_15_Avg, `P1_5.cm`, Snow_Depth_Avg) %>%
  dplyr::rename(ppfd= PAR_in_Avg, tair= Tair_hmp_Avg, precip= Rain_Tot, tsoil_surface= SoilT1_5_Avg,
                tsoil_deep= SoilT1_15_Avg, soil_moisture= `P1_5.cm`, snow_depth= Snow_Depth_Avg ) %>%
  mutate(tsoil_surface_depth= '5', tsoil_deep_depth= '15', moisture_depth= '5', site_id= "US-Ivo")

meteo <- rbindlist(list(ATQ, BES, BEO, CMDL, IVO), fill= TRUE)

#remove outliers
meteo <- meteo %>%
  mutate(tsoil_surface= ifelse(as.numeric(tsoil_surface)< -20, NA, tsoil_surface)) %>%
  mutate(tsoil_surface= ifelse(as.numeric(tsoil_surface)> 30, NA, tsoil_surface)) %>%  
  mutate(tsoil_deep= ifelse(as.numeric(tsoil_deep)< -20, NA, tsoil_deep)) %>%
  mutate(tsoil_deep= ifelse(as.numeric(tsoil_deep)> 30, NA, tsoil_deep)) %>% 
  mutate(soil_moisture= ifelse(as.numeric(soil_moisture) <0, NA, soil_moisture)) %>% 
  mutate(snow_depth= ifelse(as.numeric(snow_depth) <0, NA, snow_depth))
  
meteo.monthly <- group_by( meteo, year, month, site_id) %>% 
  dplyr::summarise( ppfd = mean(as.numeric(ppfd), na.rm = FALSE),
                    tsoil_surface = mean(as.numeric(tsoil_surface), na.rm = FALSE),
                    tsoil_deep = mean(as.numeric(tsoil_deep), na.rm = FALSE),
                    #tair = mean(tair, na.rm = FALSE), tair from zona.monthly is more gapfilled
                    precip = sum(as.numeric(precip), na.rm = FALSE),
                    soil_moisture = mean(as.numeric(soil_moisture), na.rm = TRUE),
                    tsoil_surface_depth= mean(as.numeric(tsoil_surface_depth), na.rm = TRUE),
                    tsoil_deep_depth = mean(as.numeric(tsoil_deep_depth), na.rm = TRUE),
                    moisture_depth = mean(as.numeric(moisture_depth), na.rm = FALSE)) 

zona.dat.full <- full_join(zona.monthly, meteo.monthly, by= c("site_id", "year", "month")) %>%
  dplyr::rename("site_reference"= "site_id")

#Removing strange data
#removing air temp
zona.dat.full  <- zona.dat.full %>%
  mutate(tair= ifelse(year %in% c(2020, 2021, 2022)& site_reference %in% c("US-Bes","US-Brw"), NA, tair)) %>%
  mutate(tair= ifelse(site_reference %in% c("US-Beo", "US-Ivo", "US-Atq"), NA, tair)) 
zona.dat.full  <- zona.dat.full %>%
  mutate(tsoil_surface= ifelse(site_reference %in% c("US-Atq"), NA, tsoil_surface)) %>%
  mutate(tsoil_surface= ifelse(site_reference %in% c("US-Brw") & year < 2017, NA, tsoil_surface)) %>%
  mutate(tsoil_deep= ifelse(site_reference %in% c("US-Atq"), NA, tsoil_deep)) %>%
  mutate(tsoil_deep= ifelse(site_reference %in% c("US-Brw") & year < 2017, NA, tsoil_deep))




###Merging for gapfilling exercise####
# zona.dat <- zona.dat %>%  mutate(site_id= case_when(site_id %in% 'ATQ'~ "US-Atq",
#                                                      site_id %in% 'BES'~ "US-Bes",
#                                                      site_id %in% 'BEO'~ "US-Beo",
#                                                      site_id %in% 'CMD'~ "US-Brw",
#                                                      site_id %in% 'IVO'~ "US-Ivo")) %>%
#   mutate(air_temperature = as.integer(air_temperature)-273.15)
# 
# zona.HH <- left_join(zona.dat, meteo, by=c('site_id', 'year', 'month'))
#adding static info
zona.dat.full$email <- "dzona@mail.sdsu.edu, woechel@mail.sdsu.edu"
zona.dat.full$data_contributor_or_author <- "Donatella Zona, Walter Oechel"
zona.dat.full$citation <- "doi:10.18739/A20Z70Z1H"
zona.dat.full$gap_fill <- "Monthly Averages from non-gapfilled data"


dat2 <- zona.dat.full






####FINAL COMBINE######-----------------------------
ADC.ec <- rbindlist(list( dat2, dat10 ), fill = TRUE) 
#turn NaN into NA
ADC.ec<- ADC.ec  %>% mutate_all(~ifelse(is.nan(.), NA, .))
#remove rows that do not contain flux data
ADC.ec  <- ADC.ec %>% filter(!if_all(c(nee, ch4_flux_total), ~ is.na(.)))

ADC.ec$extraction_source <- "Arctic Data Center"
ADC.ec$data_usage <- "Tier 1"
ADC.ec$dataentry_person <- "Wargowsky"
ADC.ch <- rbindlist(list(dat3, dat4, dat5, dat6, dat7, dat8), fill = TRUE)
ADC.ch$dataentry_person <- "Wargowsky"
ADC.ch$data_usage <- "Tier 1"

ADC.ch<- ADC.ch %>% 
  mutate(citation_ch4 = ifelse(!is.na(ch4_flux_total), citation, NA)) %>%
  mutate(citation_co2 = ifelse(!is.na(nee) | !is.na(gpp) | !is.na(reco), citation, NA)) %>%
  mutate(extraction_source_ch4 = ifelse(!is.na(ch4_flux_total), extraction_source, NA)) %>%
  mutate(extraction_source_co2 = ifelse(!is.na(nee) | !is.na(gpp) | !is.na(reco), extraction_source, NA)) %>%
  mutate(chamber_nr_measurement_days_ch4 = ifelse(!is.na(ch4_flux_total), chamber_nr_measurement_days, NA)) %>%
  mutate(chamber_nr_measurement_days_co2 = ifelse(!is.na(nee) | !is.na(gpp) | !is.na(reco), chamber_nr_measurement_days, NA)) %>%
  mutate(citation = NULL, extraction_source= NULL, chamber_nr_measurement_days= NULL)



ADC.ec<- ADC.ec %>% 
  mutate(citation_ch4 = ifelse(!is.na(ch4_flux_total), citation, NA)) %>%
  mutate(citation_co2 = ifelse(!is.na(nee) , citation, NA)) %>%
  mutate(extraction_source_ch4 = ifelse(!is.na(ch4_flux_total), extraction_source, NA)) %>%
  mutate(extraction_source_co2 = ifelse(!is.na(nee), extraction_source, NA)) %>%
  mutate(citation = NULL, extraction_source= NULL)


dupes<- ADC.ec %>% get_dupes(site_reference, year, month)
#no dupes 

setwd("/Users/iwargowsky/Desktop/ABCFlux v2") 
write_csv(ADC.ec, "ADC.ec.csv")
write_csv(ADC.ch, "ADC.ch.csv")
