#Data from Zenodo
library(readxl)
library(dplyr)
library(lubridate)
library(zoo)
setwd("/Users/iwargowsky/Desktop/Zenodo")


#####dat 1 Nykänen et al. ###----------------------------------------------------
nykanen <- read_excel("Nykanen_et_al-_data_CH4-resp_151019b.xls", 4) #calling on fourth sheet
#adding time columns and changing names
nykanen <- nykanen %>% mutate(year= paste("19",nykanen$year, sep= ""),
                                month= nykanen$MONTH,
                              site_name= case_when( LAK1SA2 %in% 1 ~ "Lakkasuo",
                                                     LAK1SA2 %in% 2 ~ "Särkkä"),
                              site_reference= case_when(NAT1DRA2 %in% 1 ~ "Natural",
                                                        NAT1DRA2 %in% 2 ~ "Drained"))
nykanen.permonth <- nykanen %>% group_by(year, month, site_name, site_reference) %>%
  summarise(ch4_flux_total = mean(FLCH4, na.rm=TRUE),
            nee= mean(FLCO2, na.rm = TRUE),
            water_table_depth= mean(`water table`, na.rm=TRUE),
            tair= mean(air_temp, na.rm = TRUE),
            tsoil_surface= mean(c(`surface temp`, `temp_5 cm`, `temp_3 cm`, `temp 10_cm`), na.rm=TRUE),
            tsoil_deep= mean(c(`temp_'15 cm`,`temp_25 cm`,`temp_30 cm`),na.rm=TRUE),
            chamber_nr_measurement_days= n_distinct(DATYE))
#fix units from mg CO2/CH4 to g C
nykanen.permonth$nee <- nykanen.permonth$nee/1000/44.01*12.01*days_in_month(as.yearmon(paste(nykanen.permonth$year,nykanen.permonth$month,sep = '-')))
nykanen.permonth$ch4_flux_total <- nykanen.permonth$ch4_flux_total/1000/16.04*12.01*days_in_month(as.yearmon(paste(nykanen.permonth$year,nykanen.permonth$month,sep = '-')))
#adding in other variables
nykanen.permonth <- nykanen.permonth %>% mutate(latitude= case_when(site_name %in% "Lakkasuo" ~ "61.792",
                                                                    site_name %in% "Särkkä" ~ "62.802"),
                                                longitude= case_when(site_name %in% "Lakkasuo" ~ "24.2761",
                                                                    site_name %in% "Särkkä" ~ "30.97916"))
nykanen.permonth <- nykanen.permonth %>% mutate(disturbance= case_when(site_reference %in% "Drained" ~ "Drained"),
                                                disturb_year= case_when(site_reference %in% "Drained" ~ ""))
nykanen.permonth$data_contributor_or_author <- "Nykänen, Hannu; Alm, Jukka; Martikainen, Pertti; Silvola, Jouko"
nykanen.permonth$site_id <- paste("Nykanen_", nykanen.permonth$site_name, nykanen.permonth$site_reference, "agg", sep= "_")
nykanen.permonth$email <- "hannu.nykanen@uef.fi"
nykanen.permonth$extraction_source <- "Paper/Zenodo"
nykanen.permonth$citation <- "Nykänen Hannu, Alm Jukka, Martikainen Pertti, & Silvola Jouko. (2022). Chamber flux data (CH4 , CO2) from Finland (1991 -1993) originally used in Nykänen et al. GBC, 12(1), 53 - 69, 1988, [Data set]. Zenodo. https://doi.org/10.5281/zenodo.6952933"
nykanen.permonth$country <- "Finland"
nykanen.permonth$biome <- "Boreal"
nykanen.permonth$flux_method <- "Chamber"
nykanen.permonth$flux_method_detail <- "Closed Chamber"
nykanen.permonth$flux_method_description <- "Aluminium collars (60 cm x  60 cm) equipped with water grooves for gas-tight connection of the chamber (60 cm x 60 cm x 20 cm) were inserted in the soil, two collars at each site in summer 1991 and a  new collar at each site at Lakkasuo from September 1991 onward. At the natural sites having drained counterparts, chamber bases were randomly inserted in the soil 30-50 m upstream of the ditch, while at the drained sites the chambers were approximately in the middle of the 50 m wide strip"
nykanen.permonth$gap_fill <- "Average"
nykanen.permonth$tsoil_surface_depth <- "4.5"
nykanen.permonth$tsoil_deep_depth <- "23.33"
nykanen.permonth$tair_height <- "2"
dat1 <- nykanen.permonth
  
####dat 2 Magnani et al ###-------------------------------------------------------
magnani <- read.csv("Magnani et al/fluxes-meteoclimate-NyAlesund.csv", sep= ";")
#establishing site name and site_reference
magnani <- magnani %>% 
  mutate(site_reference= paste(vegetation.class, vasular.species)) %>%
  mutate(site_reference= case_when(site_reference %in% "NV NA"~ "Non vascular",
                                   site_reference %in% "MIX NA"~ "Mixed vegetation",
                                   site_reference %in% "V SI"~ "Silene acaulis",
                                   site_reference %in% "BS NA"~ "Bare soil",
                                   site_reference %in% "V DR"~ "Dryas octopetala",
                                   site_reference %in% "V SX"~ "Saxifraga oppostifolia",
                                   site_reference %in% "V CX"~ "Carex spp.",
                                   site_reference %in% "V SL"~ "Salix Polaris"))
magnani.permonth <- magnani %>% group_by(year, month, site_reference) %>%
  summarise(nee= mean(NEE..mol.m2.day., na.rm = TRUE),
            reco= mean(ER..mol.m2.day., na.rm = TRUE),
            tsoil_surface= mean(Ts..degr.C., na.rm = TRUE),
            tair= mean(Ta..degr.C., na.rm = TRUE),
            soil_moisture = mean(VWC...., na.rm = TRUE),
            chamber_nr_measurement_days= n_distinct(day))
#adding other variables
magnani.permonth$site_name <- "NyÅlesund, Spitzbergen"
magnani.permonth$data_contributor_or_author <- "Marta Magnani; Ilaria Baneschi;  Mariasilvia Giamberini; Brunella Raco; Antonello Provenzale"
magnani.permonth$site_id <- paste("Magnani_NyÅlesund_", magnani.permonth$site_reference, "_agg", sep="")
magnani.permonth$latitude <- "78.923806"
magnani.permonth$longitude <- "11.8915"
magnani.permonth$email <- "marta.magnani@edu.unito.it"
magnani.permonth$extraction_source <- "Paper/Zenodo"
magnani.permonth$citation <- "Marta Magnani, Ilaria Baneschi, Mariasilvia Giamberini, Brunella Raco, & Antonello Provenzale. (2022). CO2 NEE and ER + air and soil meteorological and climate parameters in Arctic tundra, Ny Ålesund (Svalbard, NO) - summer 2019 [Data set]. Zenodo. https://doi.org/10.5281/zenodo.5815579"
magnani.permonth$country <- "Norway"
magnani.permonth$biome <- "Tundra"
magnani.permonth$permafrost <- "Yes"
magnani.permonth$flux_method <- "Chamber"
magnani.permonth$flux_method_detail <- "non-steady state, closed dynamic flux chamber"
magnani.permonth$flux_method_description <- "Fluxes were measured by the non-steady state, closed dynamic flux chamber method80 using a LI-COR LI-840 IRGA (InfraRed Gas Analyser) spectrophotometer and a circular stainless- steel collar (661 cm2 area, well within the range used for this site40,41,45) inserted into the soil just prior to the measurement (to a depth of about 2 cm) where to place the transparent chamber (10 cm height)."
magnani.permonth$instrumentation <- "LI-COR LI-840 IRGA spectrophotometer"
magnani.permonth$gap_fill <- "Average"
magnani.permonth$veg_detail <- "The vegetation cover is heterogeneous, typical of the bioclimate subzone B-C77 with vascular plants constellating the matrix of mosses and lichens, and can be classified as prostrate dwarf-shrub and herbs tundra "
#adjusting units from mol/m2/day to g C /m2/month
magnani.permonth$nee <- magnani.permonth$nee *12.01*days_in_month(as.yearmon(paste(magnani.permonth$year,magnani.permonth$month,sep = '-')))
magnani.permonth$reco <- magnani.permonth$reco *12.01*days_in_month(as.yearmon(paste(magnani.permonth$year,magnani.permonth$month,sep = '-')))

dat2 <- magnani.permonth
#####dat 3  ###----------------------------------------------------


Zenodo.ch <- rbindlist(list(dat1, dat2), fill = TRUE)
Zenodo.ch$extraction_source <- "Zenodo"
Zenodo.ch$data_usage <- "Tier 1"
setwd("/Users/iwargowsky/Desktop/ABCFlux v2") 
write_csv(Zenodo.ch, "Zenodo.ch.csv")
