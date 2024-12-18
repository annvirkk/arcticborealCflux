###Cleaning terrestrial data####
library(tidyr)
library(dplyr)
library(readr)
library(rquery)
library(stringr)
library(janitor)
library(purrr)

setwd("/Users/iwargowsky/Desktop/arcticborealCflux")   
abc.order <- read_csv("ABC.v2.dec24.csv") # to reorder columns later on
abc <- read_csv("ABC.v2.dec24.csv")
abc$extraction_source <- paste("CO2:", abc$extraction_source_co2, "CH4:", abc$extraction_source_ch4, sep= " ")
abc$citation <- paste("CO2:", abc$citation_co2, "CH4:", abc$citation_ch4, sep= " ")
abc.order$extraction_source <- ""
abc.order$citation <- ""

abc <- abc %>% mutate_all(~ifelse(is.nan(.), NA, .))


setwd("/Users/iwargowsky/Desktop/ABCFlux v2")
##FINAL CLASSIFICATIONS
kuhn_landcover_disturb <- read_csv("abc.static.bysite.updated.april9_kuhn.csv") 

kuhn_landcover_disturb <- kuhn_landcover_disturb %>%
  mutate(Disturbance_Category= ifelse(site_reference %in% "US-Bes", "Drained_Lake", Disturbance_Category )) %>%
  mutate(Disturbance_Category= ifelse(site_reference%in% "CA-sOBS", "Fire", Disturbance_Category )) %>%
  mutate(Disturbance_Category= ifelse(site_name%in% "Lettosuo", "Drainage, Forestry", Disturbance_Category )) %>%
  mutate(Disturbance_Category= ifelse(site_reference%in%"GL-ZaF", "Other", Disturbance_Category )) %>%
  mutate(Disturbance_Category= ifelse(site_reference%in% "RU-Tur", "No", Disturbance_Category )) %>%
  mutate(Disturbance_Category= ifelse(site_name%in% "Abisko Stordalen birch forest", "Insect_herbivory", Disturbance_Category ))


kuhn_landcover_disturb <- kuhn_landcover_disturb %>%
  dplyr::select(site_name, site_reference, land_cover_bawld_Kuhn, Disturbance_Category)%>% 
  distinct()

#correction from Anna 8/15/24
kuhn_landcover_disturb <- kuhn_landcover_disturb %>%
  mutate(land_cover_bawld_Kuhn= ifelse(site_reference %in% "FI-Sii", "Fen", land_cover_bawld_Kuhn))


x <- kuhn_landcover_disturb %>% get_dupes(site_reference, site_name)

abc <- abc %>% left_join( kuhn_landcover_disturb, by= c("site_reference", "site_name")) %>%
  unite("Disturbance_Category", c(Disturbance_Category.x, Disturbance_Category.y), na.rm= TRUE, remove= TRUE)

abc <- abc %>% dplyr::rename("land_cover_bawld_old"= "land_cover_bawld",
                             "land_cover_bawld"= "land_cover_bawld_Kuhn")


#specifying that if disturbance is NA then Disturbance_Category should also be NA
abc <- abc %>% 
  mutate(Disturbance_Category= ifelse(is.na(disturbance), NA, Disturbance_Category))

#setwd("/Users/iwargowsky/Desktop/arcticborealCflux")
#write_csv(abc.x , "ABC.v2.may24.bawld.disturb.csv")

### remove true duplicate fluxes #####_--------------------------------------------------------
dupes <- abc %>% get_dupes(site_name, site_reference, year, month, flux_method)

to.remove.bouleau <- dupes %>% dplyr::filter(site_name %in% "Bouleau peatland" & extraction_source_co2 %in% "Ameriflux BASE")

to.remove.council <- dupes %>% dplyr::filter(site_name %in% "Council, Alaska" & extraction_source_co2 %in% "ABCflux v1- Pangaea")

to.remove.disko <- dupes %>% dplyr::filter(site_name %in% "Disko" & extraction_source_co2 %in% "ABCflux v1- User-contributed")



abc <- abc %>% 
  anti_join(to.remove.bouleau, by = c("year", "month", "site_reference", "site_name", "extraction_source_co2")) %>%
  anti_join(to.remove.council, by = c("year", "month", "site_reference", "site_name", "extraction_source_co2")) %>%
  anti_join(to.remove.disko, by = c("year", "month", "site_reference", "site_name", "extraction_source_co2")) 

# setwd("/Users/iwargowsky/Desktop/ABCFlux v2")
# write_csv(abc, "ABC.v2.may24.full.csv")

### Tair  and tair_height #####_--------------------------------------------------------

abc <- abc %>%
  mutate(tair= ifelse(site_id %in% c("Larsen_Abisko2_Ch02","Larsen_Abisko3_Ch03"), NA, tair)) %>%
  mutate(tair= ifelse(site_id %in% "Christensen_NO-Adv_tower1", NA, tair)) %>%
  mutate(tair= ifelse(site_name %in% "Adventdalen, Svalbard" & data_contributor_or_author %in% c("Mats P. Bjoerkman", "Elke Morgner", "Philipp R. Semenchuk"), NA, tair)) %>%
  mutate(tair= ifelse(site_id %in% "Zona_US-Atq_tower1", NA, tair)) %>%
  mutate(tair= ifelse(site_name %in% "Council, AK", NA, tair)) %>%
  mutate(tair= ifelse(site_id %in% "Christiansen_DaringLake_Ch01", NA, tair)) %>%
  mutate(tair= ifelse(site_name %in% "Disko Island", NA, tair)) %>%
  mutate(tair= ifelse(site_name %in% "Endalen, Svalbard", NA, tair)) %>%
  mutate(tair= ifelse(site_name %in% "Imnavait Creek", NA, tair)) %>%
  mutate(tair= ifelse(site_id %in% "Shaver_US-ICh_tower1", NA, tair)) %>%
  mutate(tair= ifelse(site_id %in% "Bret-Harte_US-ICt_tower2", NA, tair)) %>%
  mutate(tair= ifelse(site_id %in% "Bret-Harte_US-ICs_tower1", NA, tair)) %>%
  mutate(tair= ifelse(site_id %in% "Zona_US-Ivo_tower1", NA, tair)) %>%
  mutate(tair= ifelse(site_name %in% "Latnjajaure", NA, tair)) %>%
  mutate(tair= ifelse(site_id %in% "Kim_SouthBrooksRange1_Ch02", NA, tair)) %>%
  mutate(tair= ifelse(site_id %in% "Friborg_Svalbard_Ch01", NA, tair)) %>%
  mutate(tair= ifelse(site_id %in% c("Christiansen_Zackenberg1_Ch03", "Christiansen_Zackenberg2_Ch04", "Christiansen_Zackenberg3_Ch05", "Pirk_Zackenberg_Ch_snow01", "Pirk_Zackenberg_Diff01"), NA, tair)) %>%
  mutate(tair= ifelse(site_id %in% "Lund_GL-ZaH_tower1", NA, tair))%>%
  mutate(tair= ifelse(site_id %in% "Lund_GL-ZaH_tower2" & year %in% c(2010, 2011, 2012, 2013, 2014), NA, tair)) 

unique(abc$tair_height)
abc <- abc %>%
  mutate(tair_height= ifelse(tair_height %in% "2m", 2, tair_height)) %>%
  mutate(tair_height= ifelse(tair_height %in% "3m above AGL", 3, tair_height)) %>%
  mutate(tair_height= as.numeric(tair_height))

### PRECIP #####_--------------------------------------------------------

abc <- abc %>%
  mutate(precip= ifelse(site_id %in% c("Larsen_Abisko2_Ch02","Larsen_Abisko3_Ch03"), NA, precip)) %>%
  mutate(precip= ifelse(site_id %in% "Christensen_NO-Adv_tower1", NA, precip)) %>%
  mutate(precip= ifelse(site_name %in% "Adventdalen, Svalbard" & data_contributor_or_author %in% c("Mats P. Bjoerkman", "Elke Morgner", "Philipp R. Semenchuk"), NA, precip)) %>%
  mutate(precip= ifelse(site_id %in% "Zona_US-Atq_tower1", NA, precip)) %>%
  mutate(precip= ifelse(site_id %in% "Kwon_US-Bes_tower1", NA, precip)) %>%
  mutate(precip= ifelse(site_name %in% "Council, AK", NA, precip)) %>%
  mutate(precip= ifelse(site_name %in% "Disko Island", NA, precip)) %>%
  mutate(precip= ifelse(site_name %in% "Endalen, Svalbard", NA, precip)) %>%
  mutate(precip= ifelse(site_name %in% "Imnavait Creek", NA, precip)) %>%
  mutate(precip= ifelse(site_id %in% "Shaver_US-ICh_tower1", NA, precip)) %>%
  mutate(precip= ifelse(site_id %in% "Bret-Harte_US-ICt_tower2", NA, precip)) %>%
  mutate(precip= ifelse(site_id %in% "Bret-Harte_US-ICs_tower1", NA, precip)) %>%
  mutate(precip= ifelse(site_id %in% "Zona_US-Ivo_tower1", NA, precip)) %>%
  mutate(precip= ifelse(site_name %in% "Latnjajaure", NA, precip)) %>%
  mutate(precip= ifelse(site_id %in% c("Schimel_ToolikLake_Diff01","Schimel_ToolikLake_Diff02"), NA, precip)) %>%
  mutate(precip= ifelse(site_id %in% c("Pirk_Zackenberg_Ch_snow01", "Pirk_Zackenberg_Diff01"), NA, precip)) %>%
  mutate(precip= ifelse(site_id %in% "Lund_GL-ZaH_tower1", NA, precip)) %>%
  mutate(precip= ifelse(site_id %in% "Lund_GL-ZaH_tower2" & year %in% c(2010, 2011, 2012, 2013, 2014), NA, precip)) 

### TSOIL#####_------------------------------------------------------------------
abc <- abc %>%
  mutate(tsoil_surface= ifelse(site_name %in% "APEX Beta" & as.numeric(tsoil_surface) >20, NA, tsoil_surface)) %>%
  mutate(tsoil_surface= ifelse(site_reference %in% "US-Atq" & 
                                 extraction_source %in% c("CO2: Arctic Data Center CH4: Arctic Data Center", "CO2: Arctic Data Center CH4: NA",                                    
                                                          "CO2: NA CH4: Arctic Data Center"), NA, tsoil_surface)) %>%
  mutate(tsoil_deep= ifelse(site_reference %in% "US-Atq" & 
                                 extraction_source %in% c("CO2: Arctic Data Center CH4: Arctic Data Center", "CO2: Arctic Data Center CH4: NA",                                    
                                                          "CO2: NA CH4: Arctic Data Center"), NA, tsoil_deep)) %>%
  mutate(tsoil_surface= ifelse(site_name %in% "Imnavait Creek Watershed Wet Sedge Tundra" &
                                 year %in% 2010, NA, tsoil_surface)) %>%
  mutate(tsoil_surface= ifelse(site_name %in% "Imnavait Creek Watershed Wet Sedge Tundra" &
                                 year %in% 2009 & month > 3, NA, tsoil_surface)) %>%
  mutate(tsoil_surface= ifelse(site_name %in% "Saskatchewan - Western Boreal, Jack Pine harvested in 2002" &
                                 year %in% 2007, NA, tsoil_surface)) %>%
  mutate(tsoil_surface= ifelse(site_name %in% "Saskatchewan - Western Boreal, Jack Pine harvested in 2002" &
                                 year %in% 2008 & month < 10, NA, tsoil_surface)) %>%
  mutate(tsoil_surface= ifelse(site_name %in% "Zackenberg Fen" & year %in% 2010 & month >6 , NA, tsoil_surface)) %>%
  mutate(tsoil_surface= ifelse(site_name %in% "Zackenberg Fen" & year %in% 2011 & month < 8, NA, tsoil_surface))

unique(abc$tsoil_surface_depth)
abc <- abc %>%
  mutate(tsoil_surface_depth= ifelse(tsoil_surface_depth %in% "0-10", 5, tsoil_surface_depth )) %>%
  mutate(tsoil_surface_depth= ifelse(tsoil_surface_depth %in% "-10", 10, tsoil_surface_depth )) %>%
  mutate(tsoil_surface_depth= ifelse(tsoil_surface_depth %in% "5cm", 5, tsoil_surface_depth )) %>%
  mutate(tsoil_surface_depth= ifelse(tsoil_surface_depth %in% "0-6 cm below soil surface", 3, tsoil_surface_depth )) %>%
  mutate(tsoil_surface_depth= ifelse(tsoil_surface_depth %in% "0-5cm", 2.5, tsoil_surface_depth )) %>%
  mutate(tsoil_surface_depth= ifelse(tsoil_surface_depth %in% "NaN", NA, tsoil_surface_depth ))

  
unique(abc$tsoil_deep_depth)
abc <- abc %>%
  mutate(tsoil_deep_depth= ifelse(tsoil_deep_depth %in% "N/A", NA, tsoil_deep_depth )) %>%
  mutate(tsoil_deep_depth= ifelse(tsoil_deep_depth %in% "15-60", 37.5, tsoil_deep_depth )) %>%
  mutate(tsoil_deep_depth= ifelse(tsoil_deep_depth %in% "NaN", NA, tsoil_deep_depth ))


### WATER TABLE DEPTH#####_------------------------------------------------------------------
abc <- abc %>%
  mutate(water_table_depth = ifelse(site_name %in% c("Lettosuo",
                                                     "Poplar Fen",
                                                     "Saskatchewan - Western Boreal, Mature Black Spruce",
                                                     "Salmisuo",
                                                     "Halmyran",
                                                     "Halsingfors",
                                                     "Storjarn",
                                                     "Tervalamminsuo",
                                                     "Vaisjeaggi, northern Finland",
                                                     "Ahvensalo",
                                                     "Bibai bog",
                                                     "Bouleau peatland",
                                                     "Eight Mile Lake",
                                                     "IBP-II",
                                                     "University of Alaska, Fairbanks",
                                                     "Middle Taiga Zone",
                                                     "Scotty Creek Bog",
                                                     "Stordalen Mire",
                                                     "U-PAD",
                                                     "Utqiagvik", "Utqiagvik Central", "Utqiagvik North", "Utqiagvik South",
                                                     "Nuuk Fen",
                                                     "Rylekaerene"),
                                    as.numeric(water_table_depth) * -1, water_table_depth)) %>%
  mutate(water_table_depth = ifelse(site_name %in% "Siikaneva" &
                                      extraction_source %in% "CO2: ABCflux v1 - SMEAR CH4: NA", as.numeric(water_table_depth) * -1, water_table_depth)) %>%
  mutate(water_table_depth = ifelse(site_name %in% "North Star Yedoma-Regrowth", NA, water_table_depth))
  
### SOIL MOISTURE#####_------------------------------------------------------------------
abc <- abc %>%
  mutate(soil_moisture= ifelse(site_name %in% c("Barrow-CMDL",
                                                 "Borgarnes",
                                                "Duvanny Yar",
                                              "Hakasia 5 yr",
                                              "Hakasia 10 yr",
                                              "Kurungnakh",
                                              "Scotty Creek",
                                              "Seida",
                                              "Siikaneva2",
                                              "Tombstone_slavin",
                                              "Wolf_creek_Buckbrush",
                                              "Wolf_creek_forest",
                                              "Wolf_creek_SparseShrub",
                                              "Wolf_creek_upper_forest"), soil_moisture *100, soil_moisture))%>%
  mutate(soil_moisture= ifelse(site_name %in% "Atqasuk" & 
                                 data_contributor_or_author %in% "Donatella Zona", soil_moisture *100, soil_moisture))%>%
  mutate(soil_moisture= ifelse(site_name %in% "Barrow-BEO" & 
                                 extraction_source_ch4 %in% "Arctic Data Center", soil_moisture *100, soil_moisture)) %>%
  mutate(soil_moisture= ifelse(site_name %in% "Bayelva, Spitsbergen" & 
                                 extraction_source_co2 %in% "User-contributed", soil_moisture *100, soil_moisture)) %>%
  mutate(soil_moisture= ifelse(site_name %in% "Degero" & 
                                 extraction_source_co2 %in% "ICOS Warm Winters" &
                                 is.na(extraction_source_ch4), NA, soil_moisture)) %>%
  mutate(soil_moisture= ifelse(site_name %in% "Fyodorovskoye" , NA, soil_moisture)) %>%
  mutate(soil_moisture= ifelse(site_name %in% "Imnavait Creek Watershed Tussock Tundra" & 
                                 year %in% c(2007:2011), NA, soil_moisture)) %>%
  mutate(soil_moisture= ifelse(site_name %in% "Imnavait Creek Watershed Wet Sedge Tundra" & 
                                 year %in% c(2007:2011), NA, soil_moisture)) %>%
  mutate(soil_moisture= ifelse(site_name %in% "Ivotuk" & 
                                 extraction_source_co2 %in% "Arctic Data Center", soil_moisture *100, soil_moisture)) %>%
  mutate(soil_moisture= ifelse(site_reference %in% c("CA-LUT-P","CA-LUT-PE","CA-LUT-YB","CA-LUT-MB", "CA-LUT-B07"), soil_moisture *100, soil_moisture))%>%
  mutate(soil_moisture= ifelse(site_name %in% c("NEON Barrow Environmental Observatory (BARR)",
                                                "NEON Caribou Creek - Poker Flats Watershed (BONA)"), NA, soil_moisture)) %>%
  mutate(soil_moisture= ifelse(site_name %in% "NGEE Arctic Council" & 
                                 extraction_source_co2 %in% "Ameriflux" , NA, soil_moisture)) %>%
  mutate(soil_moisture= ifelse(site_name %in% "Siikaneva" & 
                                 year %in% 2017, NA, soil_moisture)) %>%
  mutate(soil_moisture= ifelse(site_name %in% "Smith Creek" & 
                                 extraction_source_ch4 %in% "User-contributed", soil_moisture *100, soil_moisture)) %>%
  mutate(soil_moisture= ifelse(soil_moisture> 100, 100, soil_moisture)) %>%
  mutate(soil_moisture= ifelse(soil_moisture< 0, 0, soil_moisture))%>%
  mutate(soil_moisture= ifelse(soil_moisture %in% c("-", 
                                                    "n/a variabile moisture in talik (mostly water unsaturated except a few thin (10-cm thick) layers between 4 and 5.5 m; base of talik at 7 m is unsaturated (borehole/soil core observations)",
                                                    "N/A"), NA, soil_moisture))
  
  
unique(abc$moisture_depth)
abc <- abc %>%
  mutate(moisture_depth= ifelse(moisture_depth %in% "0-10", 5, moisture_depth )) %>%
  mutate(moisture_depth= ifelse(moisture_depth %in% "0-6", 3, moisture_depth )) %>%
  mutate(moisture_depth= ifelse(moisture_depth %in% "0-6 cm", 3, moisture_depth )) %>%
  mutate(moisture_depth= ifelse(moisture_depth %in% "0-6 cm below soil surface", 3, moisture_depth )) %>%
  mutate(moisture_depth= ifelse(moisture_depth %in% "0-5", 2.5, moisture_depth )) %>%
  mutate(moisture_depth= ifelse(moisture_depth %in% "0-12", 6, moisture_depth )) %>%
  mutate(moisture_depth= ifelse(moisture_depth %in% "2.5 cm", 2.5, moisture_depth ))

### ACTIVE LAYER THICKNESS#####_-----------------------------------------------------
unique(abc$alt)
hist(as.numeric(abc$alt))
abc <- abc %>%
  mutate(alt= ifelse(alt %in% c("NaN", "-", "N/A"), NA, alt )) %>%
  mutate(alt= ifelse(alt %in% "> 150", 150, alt )) %>%
  mutate(alt= ifelse(alt %in% "50- 100", 75, alt )) %>%
  mutate(alt= ifelse(alt %in% "40-80", 60, alt )) %>%
  mutate(alt= ifelse(alt %in% "20 to 50", 35, alt ))

#fixing sites where alt was really thaw_depth (but max thaw_depth should be alt)
special.thaw_depth <- abc %>%
  dplyr::filter(site_name %in% c("Eight Mile Lake", "Stordalen Mire", "Zackenberg Fen")) %>%
  group_by(year, site_name, site_reference)%>%
  dplyr::summarise(alt= max(as.numeric(alt), na.rm=T)) %>%
  dplyr::filter(!alt== "-Inf")

abc <- abc %>%
  mutate(thaw_depth= ifelse(site_name %in% "Eight Mile Lake" & data_contributor_or_author %in% "Edward A. G. Schuur", alt, thaw_depth)) %>%
  mutate(alt = ifelse(site_name %in% "Eight Mile Lake" & data_contributor_or_author %in% "Edward A. G. Schuur", NA, alt))%>%
  mutate(thaw_depth= ifelse(site_name %in% "Stordalen Mire", alt, thaw_depth)) %>%
  mutate(alt = ifelse(site_name %in% "Stordalen Mire", NA, alt))%>%
  mutate(thaw_depth= ifelse(site_name %in% "Zackenberg Fen", alt, thaw_depth)) %>%
  mutate(alt = ifelse(site_name %in% "Zackenberg Fen", NA, alt))

abc <- abc %>% natural_join(special.thaw_depth, by= c("site_name", "site_reference", "year"), jointype = "FULL" )

#find sites where they give thaw_depth but not ALT, and make max value from thaw_depth ALT
x <- abc %>% dplyr::filter(is.na(alt)) %>% #data without alt
  dplyr::filter(!is.na(thaw_depth)) %>% #data with thaw_depth
  mutate(thaw_depth= ifelse(as.numeric(thaw_depth)<0 , as.numeric(thaw_depth)*-1, thaw_depth)) %>%
  group_by(year, site_name, site_reference)%>%
  slice(which.max(thaw_depth)) %>%
  dplyr::filter(month>6) %>% #ensure measurement comes from late in the season
  group_by(year, site_name, site_reference)%>%
  dplyr::summarise(alt= max(as.numeric(thaw_depth), na.rm=T)) 

abc <- abc %>% natural_join(x, by= c("site_name", "site_reference", "year"), jointype = "FULL" )



### THAW DEPTH#####_------------------------------------------------
unique(abc$thaw_depth) 
abc <- abc %>%
  mutate(thaw_depth= ifelse(thaw_depth %in% c("NaN", "N/A", "NA"), NA, thaw_depth )) %>%
  mutate(thaw_depth= ifelse(thaw_depth %in% "> 150", 150, thaw_depth )) %>%
  mutate(thaw_depth= ifelse(thaw_depth %in% "> 60", 60, thaw_depth )) %>%
  mutate(thaw_depth= ifelse(thaw_depth %in% "20 - 50 cm", 35, thaw_depth )) %>%
  mutate(notes = ifelse(thaw_depth %in% "5 to 9 m thick talik", "5 to 9 m thick talik", notes)) %>%
  mutate(notes = ifelse(thaw_depth %in% "5 to 9 m", "5 to 9 m", notes)) %>%
  mutate(notes = ifelse(thaw_depth %in% "Ice lens at depth of 40 cm in strings (below no frost)", "Ice lens at depth of 40 cm in strings (below no frost)", notes)) %>%
  mutate(notes = ifelse(thaw_depth %in% "frozen at the surface, unfrozen between 67 and 150", "frozen at the surface, unfrozen between 67 and 150", notes)) %>%
  mutate(notes = ifelse(thaw_depth %in% "frozen at the surface, unfrozen between 47 and 150", "frozen at the surface, unfrozen between 47 and 150", notes)) %>%
  mutate(thaw_depth= ifelse(thaw_depth %in% c("5 to 9 m thick talik",
                                              "5 to 9 m",
                                              "Ice lens at depth of 40 cm in strings (below no frost)",
                                              "frozen at the surface, unfrozen between 67 and 150",
                                              "frozen at the surface, unfrozen between 47 and 150"), NA, thaw_depth )) %>%
  mutate(thaw_depth= ifelse(as.numeric(thaw_depth)<0 , as.numeric(thaw_depth)*-1, thaw_depth))
  
summary(as.numeric(abc$thaw_depth))
### SNOW DEPTH #####_------------------------------------------------
unique(abc$snow_depth)
abc <- abc %>%
  mutate(snow_depth= ifelse(snow_depth %in% c("NaN", "N/A", "NA"), NA, snow_depth )) %>%
  mutate(snow_depth= ifelse(snow_depth %in% "28.1 cm", 28.1, snow_depth )) %>%
  mutate(snow_depth= ifelse(snow_depth %in% "28.8 cm", 28.8, snow_depth )) %>%
  mutate(snow_depth= ifelse(snow_depth %in% "27.6 cm", 27.6, snow_depth )) %>%
  mutate(snow_depth= ifelse(snow_depth %in% "25.3 cm", 25.3, snow_depth )) %>%
  mutate(snow_depth= ifelse(snow_depth %in% "24.75 cm", 24.75, snow_depth )) %>%
  mutate(snow_depth= ifelse(site_name %in% "Adventdalen", NA, snow_depth )) %>%
  mutate(snow_depth= ifelse(site_name %in% "Adventdalen, Svalbard" & data_contributor_or_author %in% c("Mats P. Bjoerkman", "Elke Morgner", "Philipp R. Semenchuk"), NA, snow_depth)) %>%
  mutate(snow_depth= ifelse(site_name %in% "North Star Yedoma", NA, snow_depth )) %>%
  mutate(snow_depth= ifelse(site_name %in% "ARM-NSA- Olitok", NA, snow_depth )) %>%
  mutate(snow_depth= ifelse(site_name %in% "Stordalen Palsa Bog (ICOS)" &
                              year %in% c(2018:2022), NA, snow_depth )) %>%
  mutate(snow_depth= ifelse(as.numeric(snow_depth)<0, NA, snow_depth ))

summary(as.numeric(abc$snow_depth))
################ KENZIE'S CODE #-----------------------------------------------
#Fixing country

unique(abc$country)

abc  <- abc %>%
  mutate(country = ifelse(country %in% c("Greenland (Denmark)" ), "Greenland", country)) %>%
  mutate(country= ifelse(country %in% "Southwest Sweden", "Sweden", country)) %>%
  mutate(country = ifelse(country %in% "US", "USA", country)) %>%
  mutate(country = ifelse(country %in% "CA", "Canada", country)) %>%
  mutate(country = ifelse(country %in% NA, "USA", country)) 


#Fixing biomes

unique(abc$biome)

abc  <- abc %>%
  mutate(biome= ifelse(biome %in% c("Cool temperate" ), "Temperate", biome)) %>%
  mutate(biome= ifelse(biome %in% c("tundra" ), "Tundra", biome)) 




## Deciduous shrubs

unique(abc$dec_shrub)

abc  <- abc %>%
  mutate(dec_shrub = ifelse(dec_shrub %in% c("A" ), 
                            "Absent", dec_shrub)) %>%
  mutate(dec_shrub = ifelse(dec_shrub %in% c("D","DomiNAt"), "Dominant", dec_shrub)) %>%
  mutate(dec_shrub= ifelse(dec_shrub %in% c("P","present","Present (12%)",
                                            "Present (14%)", "Present (15%)",
                                            "Present (16%)", "Present (26%)",
                                            "present; Rubus chamaemorus"), "Present", dec_shrub))


## Evergreen shrub

unique(abc$ev_shrub)

abc  <- abc %>%
  mutate(ev_shrub = ifelse(ev_shrub %in% c("A", "absent" ), 
                           "Absent", ev_shrub)) %>%
  mutate(ev_shrub = ifelse(ev_shrub %in% c("D"), "Dominant", ev_shrub)) %>%
  mutate(ev_shrub= ifelse(ev_shrub %in% c("P","present","Present (16%)",
                                          "Present (18%)", "Present (20%)",
                                          "Present (23%)", "Present (26%)"), "Present", ev_shrub))




## Sedge cover

unique(abc$sedge)

abc  <- abc %>%
  mutate(sedge = ifelse(sedge %in% c("A", "Absent?" ), 
                        "Absent", sedge)) %>%
  mutate(sedge = ifelse(sedge%in% c("D", "dominant", "Dominent",
                                    "Dominant (Scheuchzeria palustre)"), 
                        "Dominant", sedge)) %>%
  mutate(sedge= ifelse(sedge %in% c("P","present","Present (28%)",
                                    "Present (32%)", "Present (36%)",
                                    "Present (40%)"), "Present", sedge))




## non_sedge_herbaceous

unique(abc$non_sedge_herbaceous)

abc  <- abc %>%
  mutate(non_sedge_herbaceous = ifelse(non_sedge_herbaceous %in% c("A"), 
                                       "Absent", non_sedge_herbaceous)) %>%
  mutate(non_sedge_herbaceous = ifelse(non_sedge_herbaceous%in% c("D"), 
                                       "Dominant", non_sedge_herbaceous)) %>%
  mutate(non_sedge_herbaceous= ifelse(non_sedge_herbaceous %in% c("P","present","Present (1%)",
                                                                  "Present (3%)"), "Present", non_sedge_herbaceous))



## Evergreen needle tree
unique(abc$ev_needle_tree)

#
abc  <- abc %>%
  mutate(ev_needle_tree = ifelse(ev_needle_tree %in% c("A","absent","absent,"), 
                                 "Absent", ev_needle_tree)) %>%
  mutate(ev_needle_tree = ifelse(ev_needle_tree%in% c("DomiNAt"), 
                                 "Dominant", ev_needle_tree)) %>%
  mutate(ev_needle_tree= ifelse(ev_needle_tree %in% c("P"), "Present", ev_needle_tree))



## Dec needle tree
unique(abc$dec_needle_tree)

abc  <- abc %>%
  mutate(dec_needle_tree = ifelse(dec_needle_tree %in% c("A","absent","absent,"), 
                                  "Absent", dec_needle_tree)) %>%
  mutate(dec_needle_tree= ifelse(dec_needle_tree%in% c("DomiNAt"), 
                                 "Dominant", dec_needle_tree)) %>%
  mutate(dec_needle_tree= ifelse(dec_needle_tree %in% c("P"), "Present",
                                 dec_needle_tree))

## Dec broad tree
unique(abc$dec_broad_tree)

abc  <- abc %>%
  mutate(dec_broad_tree = ifelse(dec_broad_tree %in% c("A","absent"), 
                                  "Absent", dec_broad_tree)) 


## Sphagnum cover
unique(abc$sphagnum_cover)

abc  <- abc %>%
  mutate(sphagnum_cover = ifelse(sphagnum_cover %in% c("A","absent","absent,"), 
                                 "Absent", sphagnum_cover)) %>%
  mutate(sphagnum_cover = ifelse(sphagnum_cover %in% c("D","dominant"), 
                                 "Dominant", sphagnum_cover)) %>%
  mutate(sphagnum_cover = ifelse(sphagnum_cover %in% c("P", "present",
                                                       "Present (18%)",
                                                       "Present (19%)",
                                                       "Present (22%)",
                                                       "Present (29%)",
                                                       "Present (42%)"), "Present",
                                 sphagnum_cover))

## Other moss cover
unique(abc$other_moss_cover)

abc  <- abc %>%
  mutate(other_moss_cover = ifelse(other_moss_cover%in% c("A","absent","absent,"), 
                                   "Absent", other_moss_cover)) %>%
  mutate(other_moss_cover = ifelse(other_moss_cover%in% c("D","dominant"), 
                                   "Dominant", other_moss_cover)) %>%
  mutate(other_moss_cover = ifelse(other_moss_cover %in% c("P", "present",
                                                           "Present (1%)",
                                                           "Present (4%)"), "Present",
                                   other_moss_cover))


## Soil moisture class (Dry, Moist, Wet)
unique(abc$soil_moisture_class)

abc  <- abc %>%
  mutate(soil_moisture_class = ifelse(soil_moisture_class%in% c("dry"), 
                                      "Dry", soil_moisture_class)) %>%
  mutate(soil_moisture_class= ifelse(soil_moisture_class%in% c("moist","moist tundra",
                                                               "Moist/Wet"), 
                                     "Moist", soil_moisture_class)) %>%
  mutate(soil_moisture_class = ifelse(soil_moisture_class %in% c("wet", 
                                                                 "wet(41%),mixed(32%),dry(25%)"),
                                      "Wet",
                                      soil_moisture_class))


## Permafrost near surface (Yes, No, U, NA)

unique(abc$permafrost)

abc  <- abc %>%
  mutate(permafrost = ifelse(permafrost%in% c("no", "Absence"), 
                             "No", permafrost)) %>%
  mutate(permafrost= ifelse(permafrost%in% c("moist","permafrost present",
                                             "yes", "Yes (sporadic)",
                                             "(Present) Note: ice lens during summer at depth of 40 cm Below this lens of 10-20 cm thick soil is non-frozen in strings"), 
                            "Yes", permafrost)) %>%
  mutate(permafrost= ifelse(permafrost %in% c("unknown", 
                                              "Unknown",
                                              "Unknown- reported as discontinuous but the Fen is likely a permafrost free zone where higher gravely spruce dominated areas nearby have permafrost"),
                            "U",
                            permafrost))

## Permafrost thaw


unique(abc$permafrost_thaw)

abc  <- abc %>%
  mutate(permafrost_thaw = ifelse(permafrost_thaw%in% c("no", "Absence", "summer thaw"), 
                                  "No", permafrost_thaw)) %>%
  mutate(permafrost_thaw= ifelse(permafrost_thaw%in% c("moist","permafrost present",
                                                       "yes"), 
                                 "Yes", permafrost_thaw)) %>%
  mutate(permafrost_thaw= ifelse(permafrost_thaw %in% c("unknown", 
                                                        "Unknown"
  ),
  "U",
  permafrost_thaw))


## Disturbance severity

unique(abc$disturb_severity)


abc <- abc %>%
  mutate(disturb_severity = ifelse(disturb_severity %in% c("high","High, the tree growth was increased remarkably" ), "High", disturb_severity)) %>%
  mutate(disturb_severity = ifelse(disturb_severity %in% c("low", "No"), "Low", disturb_severity)) %>%
  mutate(disturb_severity = ifelse(disturb_severity %in% "Moderate, High", "Moderate", disturb_severity)) %>%
  mutate(disturb_severity = ifelse(disturb_severity %in% "N/A", NA, disturb_severity)) 


## Moist

##Get rid of this column completely?

## Site_activity


unique(abc$site_activity)

abc <- abc %>%
  mutate(site_activity= ifelse(site_activity %in% c(NA,"Non-active","Inactive",
                                                    "non-active", "NO",
                                                    "Measurements ended in June 2028",
                                                    "Measurements ended in June 2029",
                                                    "Measurements ended in June 2030",
                                                    "Measurements ended in June 2031",
                                                    "Measurements ended in June 2032",
                                                    "Measurements ended in June 2033",
                                                    "Measurements ended in June 2034",
                                                    "Measurements ended in June 2035",
                                                    "Measurements ended in June 2036",
                                                    "Measurements ended in June 2037",
                                                    "Measurements ended in June 2038",
                                                    "Measurements ended in June 2039",
                                                    "Measurements ended in June 2040",
                                                    "Measurements ended in June 2041",
                                                    "Measurements ended in June 2042",
                                                    "Measurements ended in June 2043",
                                                    "Measurements ended in June 2044",
                                                    "Measurements ended in June 2045",
                                                    "Measurements ended in June 2046",
                                                    "Measurements ended in June 2047",
                                                    "Measurements ended in June 2048",
                                                    "Measurements ended in June 2049",
                                                    "Measurements ended in June 2050",
                                                    "Measurements ended in June 2051",
                                                    "Measurements ended in June 2052",
                                                    "Measurements ended in June 2053",
                                                    "Measurements ended in June 2054",
                                                    "Measurements ended in June 2055",
                                                    "Measurements ended in June 2056",
                                                    "Measurements ended in June 2057",
                                                    "Measurements ended in June 2058",
                                                    "Measurements ended in June 2059",
                                                    "Measurements ended in June 2060",
                                                    "Measurements ended in June 2061",
                                                    "Measurements ended in June 2062",
                                                    "Measurements ended in June 2063",
                                                    "Measurements ended in June 2064"
  ),
  "No", site_activity)) %>%
  mutate(site_activity = ifelse(site_activity%in% c("Active", "active", "Y", "Active?"), 
                                "Yes", site_activity))

## Flux method 

unique(abc$flux_method)


abc <- abc %>%
  mutate(flux_method= ifelse(flux_method %in% c("5-min chamber flux" ), "Chamber", flux_method)) %>%
  mutate(flux_method= ifelse(flux_method %in% c("Diff" ), "Other", flux_method))


## Diurnal Cover


unique(abc$diurnal_coverage)

abc <- abc %>%
  mutate(diurnal_coverage = ifelse(diurnal_coverage %in% c("10:00-15:00","9:00-16:00" ), 
                                   "Day", diurnal_coverage)) %>%
  mutate(diurnal_coverage = ifelse(diurnal_coverage%in% c("Day and night"), "Day and Night", diurnal_coverage)) 

### canopy height

unique(abc$canopy_height)

abc <- abc %>%
  mutate(canopy_height = ifelse(canopy_height %in% c("0.05-0.1"), "0.075", canopy_height)) %>%
  mutate(canopy_height = ifelse(canopy_height %in% c("0.1-0.3"), "0.2", canopy_height)) %>%
  mutate(canopy_height = ifelse(canopy_height %in% c("0.05-0.15","0.2-2"), "0.1", canopy_height)) %>%
  mutate(canopy_height = ifelse(canopy_height %in% c("0.5-2.0"), "1.25", canopy_height)) %>%
  mutate(canopy_height = ifelse(canopy_height %in% c("3-Jan","2-Jan" ), "NA", canopy_height)) %>%
  mutate(canopy_height = ifelse(canopy_height %in% c("20-30"), "25", canopy_height)) %>%
  mutate(canopy_height = ifelse(canopy_height %in% c("A few (0-10) cm"), "0.05", canopy_height)) %>%
  mutate(canopy_height = ifelse(canopy_height %in% c("0.2-3"), "1.5", canopy_height)) %>%
  mutate(canopy_height = ifelse(canopy_height %in% c("0.1-0.2" ), "0.15", canopy_height)) %>%
  mutate(canopy_height = ifelse(canopy_height %in% c("0.1-2" ), "1", canopy_height)) %>%
  mutate(canopy_height = ifelse(canopy_height %in% c("0.05-0.7","0.05-0.8",
                                                     "0.05-0.9","0.05-0.10"), "0.6", canopy_height)) %>%
  mutate(canopy_height = ifelse(canopy_height %in% c("N/A","no","No","NA"), NA, canopy_height)) 

#changing to numeric values
abc$canopy_height = as.numeric(abc$canopy_height)
hist(abc$canopy_height) #non numeric values


#Forest age 


unique(abc$forest_age)


abc <- abc %>%
  mutate(forest_age = ifelse(forest_age %in% c("80-180"), "130", forest_age)) %>%
  mutate(forest_age = ifelse(forest_age %in% c("70-150"), "110", forest_age)) %>%
  mutate(forest_age = ifelse(forest_age %in% c("~100 years"), "100", forest_age)) %>%
  mutate(forest_age = ifelse(forest_age %in% c("N/A","no", "NaN","-9998",
                                               "-9997","-9996","-9995",
                                               "-9994","-9993","-9992","-9991",
                                               "-9990","-9989","-9988","-9987",
                                               "-9986","-9985","-9984","-9983",
                                               "-9982","-9981","-9980","-9979",
                                               "-9978","-9977","-9976","-9975","-9974",
                                               "-9973","-9972","?","NA (not a forest)",
                                               "-","2000", "NaN"), NA, forest_age))

#changing to numeric
abc$forest_age = as.numeric(abc$forest_age)
hist(abc$forest_age) 

### NEE flux season interval

unique(abc$nee_seasonal_interval)

abc <- abc %>%
  mutate(nee_seasonal_interval = ifelse(nee_seasonal_interval %in% c("06/01/2019-10/10/2019"), "06/01-10/10", nee_seasonal_interval)) %>%
  mutate(nee_seasonal_interval = ifelse(nee_seasonal_interval %in% c("06/01/2019-10/10/2020"), "06/01-10/10", nee_seasonal_interval)) %>%
  mutate(nee_seasonal_interval = ifelse(nee_seasonal_interval %in% c("06/01/2019-10/10/2021"), "06/01-10/10", nee_seasonal_interval)) %>%
  mutate(nee_seasonal_interval = ifelse(nee_seasonal_interval %in% c("06/01/2019-10/10/2022"), "06/01-10/10", nee_seasonal_interval)) %>%
  mutate(nee_seasonal_interval = ifelse(nee_seasonal_interval %in% c("06/01/2020-10/10/2020"), "06/01-10/10", nee_seasonal_interval)) %>%
  mutate(nee_seasonal_interval = ifelse(nee_seasonal_interval %in% c("06/29/2019-08/24/2019"), "06/29-08/24", nee_seasonal_interval)) %>%
  mutate(nee_seasonal_interval = ifelse(nee_seasonal_interval %in% c("06/15/2019-08/27/2019"), "06/15-08/27", nee_seasonal_interval)) %>%
  mutate(nee_seasonal_interval = ifelse(nee_seasonal_interval %in% c("06/14/2018-10/31/2018"), "06/14-10/31", nee_seasonal_interval)) %>%
  mutate(nee_seasonal_interval = ifelse(nee_seasonal_interval %in% c("06/01/2019-10/31/2019"), "06/01-10/31", nee_seasonal_interval)) %>%
  
  mutate(nee_seasonal_interval = ifelse(nee_seasonal_interval %in% c("N/A","no", "NaN"), NA, nee_seasonal_interval))



### methane flux season interval (mm/dd - mm/dd)

unique(abc$ch4_flux_seasonal_interval)


abc <- abc %>%
  mutate(ch4_flux_seasonal_interval = ifelse(ch4_flux_seasonal_interval %in% c("06/01/2019-10/10/2019"), "06/01-10/10", ch4_flux_seasonal_interval)) %>%
  mutate(ch4_flux_seasonal_interval = ifelse(ch4_flux_seasonal_interval %in% c("06/01/2020-10/10/2020"), "06/01-10/10", ch4_flux_seasonal_interval)) %>%
  mutate(ch4_flux_seasonal_interval = ifelse(ch4_flux_seasonal_interval %in% c("06/15/2019-08/27/2019"), "06/15-08/27", ch4_flux_seasonal_interval)) %>%
  mutate(ch4_flux_seasonal_interval = ifelse(ch4_flux_seasonal_interval %in% c("06/29/2019-08/24/2019"), "06/29-08/24", ch4_flux_seasonal_interval)) %>%
  mutate(ch4_flux_seasonal_interval = ifelse(ch4_flux_seasonal_interval %in% c("06/14/2018-10/31/2018"), "06/14-10/31", ch4_flux_seasonal_interval)) %>%
  mutate(ch4_flux_seasonal_interval = ifelse(ch4_flux_seasonal_interval %in% c("06/01/2019-10/10/2020"), "06/01-10/10", ch4_flux_seasonal_interval)) %>%
  mutate(ch4_flux_seasonal_interval = ifelse(ch4_flux_seasonal_interval %in% c("06/01/2019-10/10/2021"), "06/01-10/10", ch4_flux_seasonal_interval)) %>%
  mutate(ch4_flux_seasonal_interval = ifelse(ch4_flux_seasonal_interval %in% c("06/01/2019-10/10/2022"), "06/01-10/10", ch4_flux_seasonal_interval)) %>%
  mutate(ch4_flux_seasonal_interval = ifelse(ch4_flux_seasonal_interval %in% c("06/16/2021-08/30/2021"), "06/16-08/30", ch4_flux_seasonal_interval)) %>%
  mutate(ch4_flux_seasonal_interval = ifelse(ch4_flux_seasonal_interval %in% c("05/30/2021-06/15/2021"), "05/30-06/15", ch4_flux_seasonal_interval)) %>%
  mutate(ch4_flux_seasonal_interval = ifelse(ch4_flux_seasonal_interval %in% c("06/01/2019-10/31/2019"), "06/01-10/31", ch4_flux_seasonal_interval)) %>%
  mutate(ch4_flux_seasonal_interval = ifelse(ch4_flux_seasonal_interval %in% c("N/A","no", "NaN"), NA, ch4_flux_seasonal_interval))



# methane seasonal fluxes 
#two sites are high, check and make sure they are a fen/marsh etc

#hist(abc$ch4_flux_seasonal)

unique(abc$ch4_flux_seasonal)

abc <- abc %>%
  mutate(ch4_flux_seasonal = ifelse(ch4_flux_seasonal %in% c("N/A","NaN"), NA, ch4_flux_seasonal)) 



##soil pH

#hist(abc$soil_ph)
unique(abc$soil_ph)

abc <- abc %>%
  mutate(soil_ph = ifelse(soil_ph %in% c("N/A","-"), NA, soil_ph)) 

abc$soil_ph  = as.numeric(abc$soil_ph )

##soil depth

#hist(abc$soil_depth)
unique(abc$soil_depth)

abc <- abc %>%
  mutate(soil_depth = ifelse(soil_depth %in% c(">400",
                                               "see Pelletier et al. 2015, https://doi.org/10.1177/0959683617693899" ,
                                               "see Heffernan et al. 2020, https://agupubs.onlinelibrary.wiley.com/doi/abs/10.1029/2019JG005501"), "400", soil_depth)) %>%
  mutate(soil_depth = ifelse(soil_depth %in% c("between 0 and more than 30 cm",
                                               "between 4 and more than 30 cm",
                                               "10 to 20",
                                               "0-30"), "15", soil_depth)) %>%
  mutate(soil_depth = ifelse(soil_depth %in% c("between 0 and 19 cm,"), "10", soil_depth)) %>%
  mutate(soil_depth = ifelse(soil_depth %in% c("10-May"), "10", soil_depth)) %>%
  mutate(soil_depth = ifelse(soil_depth %in% c("15-20" ), "17.5", soil_depth)) %>%
  mutate(soil_depth = ifelse(soil_depth %in% c("20-30"), "25", soil_depth)) %>%
  mutate(soil_depth = ifelse(soil_depth %in% c("15-Oct"), "15", soil_depth)) %>%
  mutate(soil_depth = ifelse(soil_depth %in% c(">200","> 200"), "200", soil_depth)) %>%
  mutate(soil_depth = ifelse(soil_depth %in% c("300-400" ), "350", soil_depth)) %>%
  mutate(soil_depth = ifelse(soil_depth %in% c("21-35cm"), "28", soil_depth)) %>%
  mutate(soil_depth = ifelse(soil_depth %in% c("8.5-18.5cm"), "13", soil_depth)) %>%
  mutate(soil_depth = ifelse(soil_depth %in% c("5.5-15cm" ), "10", soil_depth)) %>%
  mutate(soil_depth = ifelse(soil_depth %in% c("35-45"), "40", soil_depth)) %>%
  mutate(soil_depth = ifelse(soil_depth %in% c(">100" ), "100", soil_depth)) %>%
  mutate(soil_depth = ifelse(soil_depth %in% c("5-10" ), "7.5", soil_depth)) %>%
  mutate(soil_depth = ifelse(soil_depth %in% c("150-200" ), "175", soil_depth)) %>%
  mutate(soil_depth = ifelse(soil_depth %in% c("100-200" ), "150", soil_depth)) %>%
  mutate(soil_depth = ifelse(soil_depth %in% c(">100" ), "100", soil_depth)) %>%
  mutate(soil_depth = ifelse(soil_depth %in% c(">50" ), "50", soil_depth)) %>%
  
  mutate(soil_depth = ifelse(soil_depth %in% c("N/A","no", "NaN"), NA, soil_depth))


abc$soil_depth = as.numeric(abc$soil_depth)


##soil_perc_c

#hist(abc$soil_perc_c)
unique(abc$soil_perc_c)

abc <- abc %>%
  mutate(soil_perc_c = ifelse(soil_perc_c %in% c("see Pelletier et al. 2015, https://doi.org/10.1177/0959683617693899" ,
                                                 "see Heffernan et al. 2020, https://agupubs.onlinelibrary.wiley.com/doi/abs/10.1029/2019JG005501"    ), NA, soil_perc_c)) %>%
  mutate(soil_perc_c = ifelse(soil_perc_c %in% c("44.74%" ), "44.74", soil_perc_c)) %>%
  mutate(soil_perc_c = ifelse(soil_perc_c %in% c("45 (estimate based on similar sites)"), "45", soil_perc_c)) %>%
  mutate(soil_perc_c = ifelse(soil_perc_c %in% c(">100" ), "100", soil_perc_c)) %>%
  
  mutate(soil_perc_c= ifelse(soil_perc_c %in% c("N/A","no", "NaN"), NA, soil_perc_c))

abc$soil_perc_c = as.numeric(abc$soil_perc_c)

##soil_perc_n

unique(abc$soil_perc_n)

abc <- abc %>%
  mutate(soil_perc_n = ifelse(soil_perc_n %in% c("see Pelletier et al. 2015, https://doi.org/10.1177/0959683617693899" ,
                                                 "see Heffernan et al. 2020, https://agupubs.onlinelibrary.wiley.com/doi/abs/10.1029/2019JG005501"    ), NA, soil_perc_n)) %>%
  mutate(soil_perc_n = ifelse(soil_perc_n %in% c("0.27%" ), "0.27", soil_perc_n)) %>%
  
  mutate(soil_perc_n= ifelse(soil_perc_n %in% c("N/A","no", "NaN"), NA, soil_perc_n))

abc$soil_perc_n = as.numeric(abc$soil_perc_n)

##stock_depth


unique(abc$stock_depth)

abc <- abc %>%
  mutate(stock_depth = ifelse(stock_depth %in% c("see Pelletier et al. 2015, https://doi.org/10.1177/0959683617693899" ,
                                                 "see Heffernan et al. 2020, https://agupubs.onlinelibrary.wiley.com/doi/abs/10.1029/2019JG005501"    ), NA, stock_depth)) %>%
  mutate(stock_depth= ifelse(stock_depth %in% c("0-50" ), "25", stock_depth)) %>%
  mutate(stock_depth= ifelse(stock_depth %in% c("max: 428"), "428", stock_depth)) %>%
  mutate(stock_depth= ifelse(stock_depth %in% c("700 cm (7-m deep talik) top of permafrost at 7m in borehole/soil core; mean dry bulk density 1.21 g cm-3, mean OC = 1.21%" ), "700", stock_depth)) %>%
  mutate(stock_depth= ifelse(stock_depth %in% c("N/A","no","Unknown","Total", "NaN"), NA, stock_depth))



abc$stock_depth = as.numeric(abc$stock_depth)
##says NAs introduced by coercion by I can't find the problem cell

##stock_depth

unique(abc$c_stock)

abc <- abc %>%
  mutate(c_stock= ifelse(c_stock %in% c("133.1 (±5.1)"), "133.1", c_stock)) %>%
  mutate(c_stock= ifelse(c_stock %in% c("N/A","no","Unknown","Total", "NaN"), NA, c_stock))


abc$c_stock = as.numeric(abc$c_stock)

##disturbance year

## note that we need to fix rows that say "0' which could either mean the year of the 
## measurement if there is a disturbance or it could mean no disturbance
## need to code this still, right now it is coded as NA
##need to fix ongoing and current using the same logic as above

unique(abc$disturb_year)

abc <- abc %>%
  mutate(disturb_year = ifelse(disturb_year %in% c("N/A", "No", "unknown", "Unknown","-", "0", "NaN" ), NA, disturb_year)) %>%
  mutate(disturb_year= ifelse(disturb_year %in% c("1960, end of March 2021"), "2021", disturb_year)) %>%
  mutate(disturb_year= ifelse(disturb_year %in% c("Artifical drainage was installed in 2004" ), "2004", disturb_year)) %>%
  mutate(disturb_year= ifelse(disturb_year %in% c("~2019"), "2019", disturb_year)) %>%
  mutate(disturb_year= ifelse(disturb_year %in% c("~2003"), "2003", disturb_year)) %>%
  mutate(disturb_year= ifelse(disturb_year %in% c("< 1995"), "1995", disturb_year)) %>%
  mutate(disturb_year= ifelse(disturb_year %in% c("<1985","< 1985"), "1985", disturb_year)) %>%
  mutate(disturb_year= ifelse(disturb_year %in% c("<1967","< 1967"), "1967", disturb_year)) %>%
  mutate(disturb_year= ifelse(disturb_year %in% c("<1949"), "1949", disturb_year)) %>%
  mutate(disturb_year= ifelse(disturb_year %in% c("<1976","1967-1985"), "1976", disturb_year)) %>%
  mutate(disturb_year= ifelse(disturb_year %in% c("<1978"), "1978", disturb_year)) %>%
  mutate(disturb_year= ifelse(disturb_year %in% c("<1986"), "1986", disturb_year)) %>%
  mutate(disturb_year= ifelse(disturb_year %in% c("sometime between 1976 and 1978","1976-1978","1974-1977"), "1977", disturb_year)) %>%
  mutate(disturb_year= ifelse(disturb_year %in% c("1949-1967"), "1958", disturb_year)) %>%
  mutate(disturb_year= ifelse(disturb_year %in% c("Late 1960's","late 1960s"), "1968", disturb_year)) %>%
  mutate(disturb_year= ifelse(disturb_year %in% c("1965, 1925"), "1965", disturb_year)) %>%
  mutate(disturb_year= ifelse(disturb_year %in% c("30 years since time of measurement","30 years ago"), "1986", disturb_year)) %>%
  mutate(disturb_year= ifelse(disturb_year %in% c("30 years since time of measurement"), "1986", disturb_year)) %>%
  mutate(disturb_year= ifelse(disturb_year %in% c("200 years since time of measurement","200 years ago"), "1816", disturb_year)) %>%
  mutate(disturb_year= ifelse(disturb_year %in% c("thaw ~500 years ago"), "1518", disturb_year)) %>%
  mutate(disturb_year= ifelse(disturb_year %in% c("thaw ~100 years ago"), "1914", disturb_year)) %>%
  mutate(disturb_year= ifelse(disturb_year %in% c("1960s"), "1965", disturb_year)) %>%
  mutate(disturb_year= ifelse(disturb_year %in% c("every year"), "Ongoing", disturb_year)) %>%
  mutate(disturb_year= ifelse(disturb_year %in% c("estimated 100 years since time of measurement"), "1916", disturb_year)) %>%
  mutate(disturb_year= ifelse(disturb_year %in% c("1949-1967"), "1958", disturb_year))
  


### More cleaning by Isabel ###-----------------------------------------------------------
#check lat long coords

# # Load world country borders
# world <- ne_countries(scale = 110, returnclass = "sf") %>% dplyr::filter(subregion %in% c("Northern America", "Central Asia", "Eastern Europe",
#                                                   "Northern Europe", "Western Europe", "Southern Asia",
#                                                   "Southern Europe", "Eastern Asia", "Western Asia"))
# # Read additional spatial data
# setwd("/Users/iwargowsky/Desktop/ABCFlux v2/Figures/AGU")
# permafrost <- st_read("UiO_PEX_PERZONES_5.0_20181128_2000_2016_NH") %>% dplyr::rename("Permafrost zone"= "EXTENT")
#
# listofsites <- abc %>%
#   group_by(site_name, flux_method, country) %>%
#   dplyr::summarise(latitude = mean(latitude, na.rm=T), longitude = mean(longitude, na.rm=T)) %>%
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

#Isabel:I found a few mistakes but fixed errors directly in excel for 5 sites (Blaesedalen, Hospitaldalen, Mellemlandet, Skarvefjed, UaF)

# gap_fill_perc_ #####
abc <- abc %>%
  mutate(gap_fill_perc_nee = str_remove(gap_fill_perc_nee, "%"),
         gap_fill_perc_nee = as.numeric(gap_fill_perc_nee)) %>%
  mutate(gap_fill_perc_ch4 = str_remove(gap_fill_perc_ch4, "%"),
         gap_fill_perc_ch4 = as.numeric(gap_fill_perc_ch4))


### removing blank/unused columns #####_-----------------------------------------------
abc <-abc[names(abc.order )]

colnames(abc)

unique(abc$chamber_nr_measurement_days)
abc$chamber_nr_measurement_days <- NULL

abc$gap_fill_perc_gpp <- NULL
abc$gap_fill_perc_reco <- NULL

unique(abc$gap_fill_perc)
abc$gap_fill_perc<- NULL

unique(abc$date)
abc$date <- NULL

unique(abc$sep)
abc$sep <- NULL

unique(abc$Flux)
abc$Flux <- NULL


abc$citation_ch4 <- NULL
abc$citation_co2 <- NULL

abc$Column <- NULL
abc$ts <- NULL

### removing weird fluxes #####_-----------------------------------------------

abc <- abc %>% dplyr::filter(!(site_name== "Bayelva, Spitsbergen" &
                                  year== 2009 &
                                  month > 4))

abc <- abc %>% dplyr::filter(!(site_name== "Bonanza Creek Black Spruce"&
                                 extraction_source_co2== "Ameriflux" &
                                 year %in% c(2013, 2014)))

abc <- abc %>% dplyr::filter(!(site_name== "Bonanza Creek Rich Fen"&
                                 extraction_source_co2== "Ameriflux" &
                                 year == 2013))

abc <- abc %>% dplyr::filter(!(site_name== "Bonanza Creek Thermokarst Bog"&
                                 extraction_source_co2== "Ameriflux" &
                                 year == 2013))

abc <- abc %>% dplyr::filter(!(site_name== "Manitoba - Northern Old Black Spruce (former BOREAS Northern Study Area)"&
                                 extraction_source_co2== "Fluxnet2015" &
                                 year %in% c(2004, 2005)))

abc <- abc %>% dplyr::filter(!(site_name== "NEON Healy (HEAL)"& year == 2017)) %>%
               dplyr::filter(!(site_name== "NEON Healy (HEAL)"&  year == 2019 & month > 8))  %>%
               dplyr::filter(!(site_name== "NEON Healy (HEAL)"&  year == 2020 & month < 7)) 

  
abc <- abc %>% dplyr::filter(!(site_name== "Ontario - Groundhog River, Boreal Mixedwood Forest"&
                                 extraction_source_co2== "Fluxnet2015" &
                                 year %in% 2003))

abc <- abc %>% dplyr::filter(!(site_name== "Ontario - Groundhog River, Boreal Mixedwood Forest"&
                                 extraction_source_co2== "Fluxnet2015" &
                                 year %in% 2014 &
                                 gap_fill_perc_nee == 100))

abc <- abc %>% dplyr::filter(!(site_name== "Poker Flat Research Range Black Spruce Forest" &
                                 extraction_source_co2== "Fluxnet2015" &
                                 year == 2010))

abc <- abc %>% dplyr::filter(!(site_name== "Poker Flat Research Range: Succession from fire scar to deciduous forest"&
                                 extraction_source_co2== "Ameriflux" &
                                 year == 2008))

abc <- abc %>% dplyr::filter(!(site_name== "Quebec - 1975 Harvested Black Spruce"&
                                 year == 2007))

abc <- abc %>% dplyr::filter(!(site_name== "Quebec - Eastern Boreal, Mature Black Spruce"&
                                 year == 2003 & month < 9))

abc <- abc %>% dplyr::filter(!(site_name== "Rosinedal-3"&
                                 year == 2014 & month < 8))

abc <- abc %>% dplyr::filter(!(site_name== "Saskatchewan - Western Boreal, forest burned in 1977"&
                                 year == 2003 &
                                 month < 8))

abc <- abc %>% dplyr::filter(!(site_name== "Saskatchewan - Western Boreal, forest burned in 1989"&
                                 year %in% c(2006, 2001)))

abc <- abc %>% dplyr::filter(!(site_name== "Saskatchewan - Western Boreal, Jack Pine harvested in 2002" &
                                 year < 2004)) %>%
                dplyr::filter(!(site_name== "Saskatchewan - Western Boreal, Jack Pine harvested in 2002" &
                        year > 2007))

abc <- abc %>% dplyr::filter(!(site_name== "Svartberget" &
                                 year == 2017))

abc <- abc %>% dplyr::filter(!(site_name== "Tiksi" &
                                 flux_method == "EC" &
                                 year == 2010 &
                                 month < 7))

abc <- abc %>% dplyr::filter(!(site_name== "UCI-1850 burn site" & year < 2002)) %>%
               dplyr::filter(!(site_name== "UCI-1850 burn site" & year == 2002 & month < 6)) 
  
abc <- abc %>% dplyr::filter(!(site_name== "UCI-1930 burn site" & year == 2001 & month <7 )) %>%
               dplyr::filter(!(site_name== "UCI-1930 burn site" & year == 2005 & month > 8 ))

abc <- abc %>% dplyr::filter(!(site_name== "UCI-1964 burn site" & year < 2002 & month < 8))

abc <- abc %>% dplyr::filter(!(site_name== "UCI-1964 burn site wet" & year < 2003))

abc <- abc %>% dplyr::filter(!(site_name== "UCI-1981 burn site" & year < 2002))

abc <- abc %>% dplyr::filter(!(site_name== "UCI-1989 burn site" & year < 2002)) %>%
               dplyr::filter(!(site_name== "UCI-1989 burn site" & year == 2005 & month >9))

abc <- abc %>% dplyr::filter(!(site_name== "UCI-1998 burn site" & year < 2002)) %>%
               dplyr::filter(!(site_name== "UCI-1998 burn site" & year == 2002 & month < 6))

abc <- abc %>% dplyr::filter(!(site_name== "Hyytiala" & year < 1997 & month < 7))

abc <- abc %>% dplyr::filter(!(site_name== "Norunda" & year > 2022))

abc <- abc %>% dplyr::filter(!(site_name== "Cherskii" & year == 2002 & month <7))


abc$extraction_ch4 <- NULL
abc$extraction_co2 <- NULL

#save df with duplicates
setwd("/Users/iwargowsky/Desktop/arcticborealCflux")   
write_csv(abc, "ABC.v2.dec24.cleanish.wdupes.csv")


### Removing duplicate fluxes #####_--------------------------------------------------------
unique(abc$extraction_source)
abc <- abc %>%
  mutate(pref1= case_when(extraction_source %in% c("CO2: User-contributed CH4: User-contributed",
                                                  "CO2: User-contributed CH4: NA",                                      
                                                  "CO2: NA CH4: User-contributed",
                                                  "CO2: NA CH4: BAWLD-CH4-Publication/User-contributed",                
                                                  "CO2: User-contributed/Publication CH4: User-contributed/Publication",
                                                  "CO2: User-contributed/NGEE CH4: User-contributed/NGEE",  
                                                  "CO2: NA CH4: User-contributed/Publication")~ 1,
                          extraction_source %in% c("CO2: Publication CH4: NA",                                           
                                                   "CO2: ICOS Warm Winters CH4: NA" ,                                    
                                                   "CO2: ICOS Warm Winters CH4: Fluxnet-CH4",                            
                                                   "CO2: ICOS Sweden CH4: ICOS Sweden",                                  
                                                   "CO2: ICOS Ecosystem Thematic Centre CH4: NA",                        
                                                   "CO2: Fluxnet2015 CH4: NA",                                           
                                                   "CO2: Fluxnet2015 CH4: Fluxnet-CH4" ,                                 
                                                   "CO2: Fluxnet-CH4 CH4: Fluxnet-CH4",                                  
                                                   "CO2: European Fluxes Database Cluster CH4: NA",                      
                                                   "CO2: AsiaFlux CH4: NA",                                              
                                                   "CO2: Ameriflux- beta ONEFLUX CH4: NA",                               
                                                   "CO2: Ameriflux- beta ONEFLUX CH4: Fluxnet-CH4" ,                     
                                                   "CO2: Ameriflux CH4: NA",                                             
                                                   "CO2: Ameriflux CH4: Fluxnet-CH4"  )~ 2, 
                          extraction_source %in% c("CO2: Arctic Data Center CH4: Arctic Data Center",   
                                                   "CO2: Arctic Data Center/Publication CH4: Arctic Data Center/Publication",        
                                                   "CO2: Arctic Data Center CH4: NA" ,      
                                                   "CO2: Arctic Data Center/Publication CH4: NA" ,        
                                                   "CO2: NA CH4: Arctic Data Center",                     
                                                   "CO2: NA CH4: Arctic Data Center/Publication",   
                                                   "CO2: Zenodo/Publication CH4: Zenodo/Publication" ,                                           
                                                   "CO2: Zenodo/Publication CH4: NA",    
                                                   "CO2: Ameriflux BASE CH4: NA",                                        
                                                   "CO2: Ameriflux BASE CH4: Ameriflux BASE",                            
                                                   "CO2: NA CH4: Ameriflux BASE") ~3,
                          extraction_source %in% c("CO2: ABCflux v1- Natali synthesis CH4: NA" ,                         
                                                   "CO2: ABCflux v1- Publication CH4: NA",                               
                                                   "CO2: ABCflux v1- User-contributed CH4: NA" ,                         
                                                   "CO2: ABCflux v1- Publication/User-contributed CH4: NA" ,             
                                                   "CO2: NA CH4: BAWLD-CH4-Publication",                                 
                                                   "CO2: BAWLD-CH4-Publication CH4: BAWLD-CH4-Publication",   
                                                   "CO2: BAWLD-CH4-Publication CH4: NA", 
                                                   "CO2: ABCflux v1- Virkkala synthesis CH4: NA" ,                       
                                                   "CO2: ABCflux v1- SMEAR CH4: NA" ,                                    
                                                   "CO2: ABCflux v1- Euroflux/User-contributed CH4: NA",                 
                                                   "CO2: ABCflux v1- Euroflux CH4: NA",                                  
                                                   "CO2: ABCflux v1- Ameriflux CH4: NA")~4))


#remove duplicates
abc.nodupes <- abc %>%
  arrange(desc(partition_method)) %>% # prefer Reichstein over Lasslop 
  arrange(pref1) %>% 
  distinct(site_name, site_reference, year, month, .keep_all = TRUE)

abc.nodupes$pref1 <- NULL

dupes <- abc.nodupes %>% get_dupes(site_name, site_reference, year, month, flux_method)
#woo


####Samoylov Island special dupe removal####
# we have data for Samoylov Island from Fluxnet as well as ABCFlux v1 for the same dates
# data from ABCFlux v1 has both open and closed path (specified in site_reference) but Fluxnet does not
# here I remove Fluxnet dupes that were not removed because they have diff site_reference
sam.fluxnet.dupes <- abc.nodupes %>% get_dupes(site_name, year, month, flux_method) %>% 
  dplyr::filter(site_name == "Samoylov Island") %>%
  dplyr::filter(extraction_source == "CO2: Fluxnet2015 CH4: NA" )

#remove dupes
abc.nodupes <- abc.nodupes %>% anti_join(sam.fluxnet.dupes, by= c("site_name", "site_reference", "year", "month"))
  


setwd("/Users/iwargowsky/Desktop/arcticborealCflux")   
write_csv(abc.nodupes, "ABC.v2.dec24.cleanish.nodupes.csv")




### removing fluxes based on gapfill percent #####--------------------------------------------------------
#adding this so we can find avg gap fill percent of fluxes removed
abc.intact <- abc.nodupes

#zona sites

abc.nodupes <- abc.nodupes %>% mutate(nee= ifelse(site_name %in% "Barrow-BES" & year %in% 2016 
                                                    & month %in% 8, NA, nee)) %>% 
                               mutate(nee= ifelse(site_name %in% "Barrow-BES" & year %in% 2017 
                                                    & month %in% 2, NA, nee))

abc.nodupes <- abc.nodupes %>% mutate(nee= ifelse(site_name %in% "Barrow-BEO" & year %in% 2013, NA, nee))


abc.nodupes <- abc.nodupes %>% mutate(ch4_flux_total= ifelse(site_name %in% "Ivotuk" & year %in% 2017 
                                                  & month %in% 3, NA, ch4_flux_total))

abc.nodupes <- abc.nodupes %>% mutate(nee= ifelse(site_name %in% "Atqasuk" & year %in% 2016 
                                                             & month %in% 2, NA, nee)) %>%
                               mutate(nee= ifelse(site_name %in% "Atqasuk" & year %in% 2003, NA, nee))

abc.nodupes <- abc.nodupes %>% mutate(ch4_flux_total= ifelse(site_name %in% "Barrow-CMDL" & year %in% 2018 
                                                             & month %in% c(2,4), NA, ch4_flux_total))



#CO2
abc.nodupes <- abc.nodupes %>%  dplyr::filter(!(site_name %in% "Alberta - Western Peatland - LaBiche River,Black Spruce,Larch Fen" & 
                                                year %in% 2003 & month < 8)) %>%
  dplyr::filter(!(site_name %in% "Alberta - Western Peatland - LaBiche River,Black Spruce,Larch Fen" & 
                    year %in% 2009 & gap_fill_perc_nee %in% 100))

abc.nodupes <- abc.nodupes %>% dplyr::filter(!(site_name %in% "Alberta - Western Peatland - Poor Fen (Sphagnum moss)" & 
                                                    year %in% 2004 & month < 5))

abc.nodupes <- abc.nodupes %>% dplyr::filter(!(site_name %in% "Alberta - Western Peatland - Rich Fen  (Carex)" & 
                                                 year %in% 2004 & month < 5))

abc.nodupes <- abc.nodupes %>% dplyr::filter(!(site_name %in% "Attawapiskat River Bog" & 
                                                 year %in% 2011 & month < 4))

abc.nodupes <- abc.nodupes %>% dplyr::filter(!(site_name %in% "Attawapiskat River Fen" & 
                                                 year %in% 2011 & month < 4))

abc.nodupes <- abc.nodupes %>%  dplyr::filter(!(site_name %in% "Bonanza Creek Black Spruce" & 
                                                  gap_fill_perc_nee %in% 100 & year %in% 2010))

abc.nodupes <- abc.nodupes %>%  dplyr::filter(!(site_name %in% "Bonanza Creek Old Thermokarst Bog" & 
                                                  gap_fill_perc_nee %in% 100 & year %in% 2018))

abc.nodupes <- abc.nodupes %>%  dplyr::filter(!(site_name %in% "Bonanza Creek Rich Fen" & 
                                                  month < 5  & year %in% 2011))

abc.nodupes <- abc.nodupes %>%  dplyr::filter(!(site_name %in% "Fyodorovskoye" & 
                                                  month < 5 & year %in% 1998))

abc.nodupes <- abc.nodupes %>%  dplyr::filter(!(site_name %in% "Fyodorovskoye2" & 
                                                  gap_fill_perc_nee %in% 100 & year %in% 2015))

abc.nodupes <- abc.nodupes %>%  dplyr::filter(!(site_name %in% "Hakasia Steppe" & 
                                                  month <7 & year %in% 2002))

abc.nodupes <- abc.nodupes %>%  dplyr::filter(!(site_name %in% "Imnavait Creek Watershed Heath Tundra" & 
                                                  gap_fill_perc_nee %in% 100 & year %in% 2007))

abc.nodupes <- abc.nodupes %>%  dplyr::filter(!(site_name %in% "Imnavait Creek Watershed Tussock Tundra" & 
                                                  month <7 & year %in% 2007))

abc.nodupes <- abc.nodupes %>%  dplyr::filter(!(site_name %in% "Imnavait Creek Watershed Wet Sedge Tundra" & 
                                                  gap_fill_perc_nee %in% 100 & year %in% 2007))

abc.nodupes <- abc.nodupes %>%  dplyr::filter(!(site_name %in% "Eight Mile Lake" & 
                                                  gap_fill_perc_nee %in% 100 & year %in% 2008))

abc.nodupes <- abc.nodupes %>%  dplyr::filter(!(site_name %in% "Ivotuk" & 
                                                  month >9 & year %in% 2007))

abc.nodupes <- abc.nodupes %>%  dplyr::filter(!(site_name %in% "Lettosuo" & 
                                                  gap_fill_perc_nee %in% 100 & year %in% 2009))

abc.nodupes <- abc.nodupes %>%  dplyr::filter(!(site_name %in% "Manitoba - Northern Old Black Spruce (former BOREAS Northern Study Area)" & 
                                                  gap_fill_perc_nee %in% 100 & year %in% 1994)) %>%  
                                dplyr::filter(!(site_name %in% "Manitoba - Northern Old Black Spruce (former BOREAS Northern Study Area)" & 
                                                  month <5 & year %in% 2006))

abc.nodupes <- abc.nodupes %>%  dplyr::filter(!(site_name %in% "NEON Barrow Environmental Observatory (BARR)" & 
                                                  month <6 & year %in% 2019))

abc.nodupes <- abc.nodupes %>%  dplyr::filter(!(site_name %in% "NGEE Arctic Barrow" & 
                                                  year %in% 2012)) %>%
                                dplyr::filter(!(site_name %in% "NGEE Arctic Barrow" & 
                                                    year %in% 2013 & month < 5)) 

abc.nodupes <- abc.nodupes %>% mutate(nee= ifelse(site_name %in% "NGEE Arctic Council" & 
                                                    month < 8 & year %in% 2017, NA, nee),
                                      gpp= ifelse(site_name %in% "NGEE Arctic Council" & 
                                                    month < 8 & year %in% 2017, NA, gpp),
                                      reco= ifelse(site_name %in% "NGEE Arctic Council" &
                                                     month < 8 & year %in% 2017, NA, reco)) 

abc.nodupes <- abc.nodupes %>%  dplyr::filter(!(site_name %in% "Samoylov Island" & 
                                                  month > 7 & year %in% 2004)) %>%  
                                dplyr::filter(!(site_name %in% "Samoylov Island" & 
                                                  month < 7 & year %in% 2005))

abc.nodupes <- abc.nodupes %>% dplyr::filter(!(site_name== "Saskatchewan - Western Boreal, forest burned in 1989"&
                                                 gap_fill_perc_nee %in% 100))

abc.nodupes <- abc.nodupes %>% dplyr::filter(!(site_name== "Saskatchewan - Western Boreal, forest burned in 1998"&
                                                 gap_fill_perc_nee %in% 100))

abc.nodupes <- abc.nodupes %>%  dplyr::filter(!(site_name %in% "Saskatchewan - Western Boreal, Mature Aspen" & 
                                                  gap_fill_perc_nee %in% 100 & year %in% 1996))

abc.nodupes <- abc.nodupes %>%  dplyr::filter(!(site_name %in% "Saskatchewan - Western Boreal, Mature Black Spruce" & 
                                                  month < 5 & year %in% 1999))

abc.nodupes <- abc.nodupes %>%  dplyr::filter(!(site_name %in% "Saskatchewan - Western Boreal, Mature Jack Pine" &  year %in% 1999))

abc.nodupes <- abc.nodupes %>%  dplyr::filter(!(site_name %in% "Stordalen - Fen" & 
                                                  gap_fill_perc_nee %in% 100 & year %in% 2012)) %>%
  mutate(nee= ifelse(site_name %in% "Stordalen - Fen" & year %in% 2014 & month >9, NA, nee)) %>%
  mutate(nee= ifelse(site_name %in% "Stordalen - Fen" & year %in% 2015 & month <5, NA, nee))
  

abc.nodupes <- abc.nodupes %>%  dplyr::filter(!(site_name %in% "Stordalen Palsa Bog (ICOS)" & 
                                                  month > 8 & year %in% 2023))

abc.nodupes <- abc.nodupes %>%  dplyr::filter(!(site_name %in% "UCI-1850 burn site" & 
                                                  month > 9 & year %in% 2005))

abc.nodupes <- abc.nodupes %>%  dplyr::filter(!(site_name %in% "UCI-1964 burn site" & 
                                                  gap_fill_perc_nee %in% 100))

abc.nodupes <- abc.nodupes %>%  dplyr::filter(!(site_name %in% "UCI-1964 burn site wet" & 
                                                  gap_fill_perc_nee %in% 100 & year %in% 2005))

abc.nodupes <- abc.nodupes %>%  dplyr::filter(!(site_name %in% "UCI-1981 burn site" & 
                                                  gap_fill_perc_nee %in% 100 & year %in% 2005))

abc.nodupes <- abc.nodupes %>% dplyr::filter(!(site_name %in% "UCI-1998 burn site" & 
                                                  gap_fill_perc_nee %in% 100 & year %in% 2005))

abc.nodupes <- abc.nodupes %>%  dplyr::filter(!(site_name %in% "Cherskii" & 
                                                  month > 9 & year %in% 2004))%>% 
                               dplyr::filter(!(site_name %in% "Cherskii" & 
                                              month < 7& year %in% 2005))

abc.nodupes <- abc.nodupes %>%  dplyr::filter(!(site_name %in% "Eight Mile Lake" & 
                                                  gap_fill_perc_nee %in% 100 & year %in% 2020))

abc.nodupes <- abc.nodupes %>%  dplyr::filter(!(site_name %in% c("Nuuk Fen", "Zackenberg Fen", "Zackenberg Heath") & 
                                                  extraction_source %in% "CO2: Fluxnet2015 CH4: NA"))

### methane

 abc.nodupes <- abc.nodupes %>%  mutate(ch4_flux_total= ifelse(site_name %in% "University of Alaska, Fairbanks" 
                                                              & year %in% c(2022, 2023), NA, ch4_flux_total))

abc.nodupes <- abc.nodupes %>%  mutate(ch4_flux_total= ifelse(site_name %in% "University of Alaska, Fairbanks"
                                                              & year %in% 2021 & month %in% 1, NA, ch4_flux_total))

abc.nodupes <- abc.nodupes %>%  mutate(ch4_flux_total= ifelse(site_name %in% "University of Alaska, Fairbanks"
                                                              & year %in% 2019 & month %in% 12, NA, ch4_flux_total))

#partitioned fluxes

abc.nodupes <- abc.nodupes %>%  dplyr::filter(!(site_name %in% c("Anaktuvuk River Moderate Burn", "Anaktuvuk River Severe Burn", "Anaktuvuk River Unburned")
                                                    & year %in% 2020 & month %in% 6))

abc.nodupes <- abc.nodupes %>%  mutate(reco= ifelse(site_name %in% "Imnavait Creek Watershed Tussock Tundra" 
                                       & year %in% c(2008, 2015), NA, reco)) %>%
                             mutate(gpp= ifelse(site_name %in% "Imnavait Creek Watershed Tussock Tundra" 
                                      & year %in% c(2008, 2015), NA, gpp)) 
    
abc.nodupes <- abc.nodupes %>%  dplyr::filter(!(site_name %in% "Imnavait Creek Watershed Wet Sedge Tundra" 
                                                    & year %in% 2010)) 

abc.nodupes <- abc.nodupes %>%  mutate(reco= ifelse(site_name %in% "NGEE Arctic Barrow" 
                                                    & year %in% 2013 & month <7 , NA, reco)) %>%
                                 mutate(reco= ifelse(site_name %in% "NGEE Arctic Barrow" 
                                                   & year %in% 2015  , NA, reco)) %>%
                                mutate(gpp= ifelse(site_name %in% "NGEE Arctic Barrow" 
                                                  & year %in% 2015  , NA, gpp)) 
                                 
abc.nodupes <- abc.nodupes %>%  mutate(reco= ifelse(site_name %in% "Samoylov Island" 
                                                    & year %in% 2004 , NA, reco)) %>%
                                mutate(gpp= ifelse(site_name %in% "Samoylov Island" 
                                                    & year %in% 2004 , NA, gpp))  

abc.nodupes <- abc.nodupes %>%  mutate(reco= ifelse(site_name %in% "Saskatchewan - Western Boreal, forest burned in 1989" 
                                                    & year %in% 2002 & month <6 , NA, reco))


data.removed <- anti_join(abc.intact, abc.nodupes) %>% 
  mutate(gap_fill_perc_nee= as.numeric(gap_fill_perc_nee))
mean(data.removed$gap_fill_perc_nee, na.rm=T)

### removing winter nee fluxes < -10  #####--------------------------------------------------------
# abc.nodupes <- abc.nodupes %>% 
#   mutate(nee= ifelse(month %in% c(12,1,2) & nee < -10, NA, nee)) %>%
#   mutate(gpp= ifelse(month %in% c(12,1,2) & nee < -10, NA, gpp)) %>%
#   mutate(reco= ifelse(month %in% c(12,1,2) & nee < -10, NA, reco))
# 
# ###investigating winter fluxes
# winter <- abc.nodupes %>% dplyr::filter(month %in% c(12,1,2)) 
# 
# winter.sum <- winter %>% group_by(biome, month) %>%
#   dplyr::summarise (mean= mean(nee, na.rm= T),
#                     sd= sd(nee, na.rm= T))


### fixing extraction source #####--------------------------------------------------------

unique(abc.nodupes$extraction_source)
abc.nodupes <- abc.nodupes %>% 
  mutate(extraction_source = case_when(extraction_source %in% c("CO2: User-contributed CH4: NA", 
                                                                "CO2: User-contributed CH4: User-contributed",
                                                                "CO2: NA CH4: User-contributed")~"User-contributed",
                                       extraction_source %in% "CO2: NA CH4: BAWLD-CH4-Publication/User-contributed"~"BAWLD-CH4-Publication/User-contributed",
                                       extraction_source %in% c("CO2: User-contributed/Publication CH4: User-contributed/Publication",
                                                                "CO2: NA CH4: User-contributed/Publication",
                                                                "CO2: User-contributed/Publication CH4: NA")~ "User-contributed/Publication",
                                       extraction_source %in% c("CO2: User-contributed/NGEE CH4: User-contributed/NGEE",
                                                                "CO2: User-contributed/NGEE CH4: NA")~ "User-contributed/NGEE",
                                       extraction_source %in% "CO2: Fluxnet-CH4 CH4: Fluxnet-CH4"~ "Fluxnet-CH4",
                                       extraction_source %in% "CO2: Fluxnet2015 CH4: NA"~ "Fluxnet2015",
                                       extraction_source %in%  "CO2: Ameriflux- beta ONEFLUX CH4: NA"~ "Ameriflux- beta ONEFLUX",
                                       extraction_source %in%  "CO2: Ameriflux CH4: NA"~ "Ameriflux",
                                       extraction_source %in%  "CO2: ICOS Warm Winters CH4: NA"~ "ICOS Warm Winters",
                                       extraction_source %in%  "CO2: ICOS Ecosystem Thematic Centre CH4: NA"~ "ICOS Ecosystem Thematic Centre",
                                       extraction_source %in%  "CO2: European Fluxes Database Cluster CH4: NA"~ "European Fluxes Database Cluster",
                                       extraction_source %in%  "CO2: AsiaFlux CH4: NA" ~ "AsiaFlux",
                                       extraction_source %in%  "CO2: ICOS Sweden CH4: ICOS Sweden" ~ "ICOS Sweden",
                                       extraction_source %in%  "CO2: Publication CH4: NA" ~ "Publication",
                                       extraction_source %in%  c("CO2: Arctic Data Center CH4: Arctic Data Center",
                                                                 "CO2: NA CH4: Arctic Data Center",
                                                                 "CO2: Arctic Data Center CH4: NA") ~ "Arctic Data Center",
                                       extraction_source %in%  c("CO2: Ameriflux BASE CH4: NA",
                                                                 "CO2: NA CH4: Ameriflux BASE",
                                                                 "CO2: Ameriflux BASE CH4: Ameriflux BASE") ~ "Ameriflux BASE",
                                       extraction_source %in%  c("CO2: Zenodo/Publication CH4: Zenodo/Publication",
                                                                 "CO2: Zenodo/Publication CH4: NA") ~ "Zenodo/Publication",
                                       extraction_source %in%  c("CO2: Arctic Data Center/Publication CH4: NA",
                                                                 "CO2: Arctic Data Center/Publication CH4: Arctic Data Center/Publication") ~ "Arctic Data Center/Publication",
                                       extraction_source %in%  c("CO2: NA CH4: BAWLD-CH4-Publication" ,
                                                                 "CO2: BAWLD-CH4-Publication CH4: BAWLD-CH4-Publication",
                                                                 "CO2: BAWLD-CH4-Publication CH4: NA") ~ "BAWLD-CH4-Publication",
                                       extraction_source %in%  "CO2: EMERGE-DB CH4: EMERGE-DB" ~ "EMERGE-DB",
                                       extraction_source %in%  "CO2: ABCflux v1- Publication CH4: NA"  ~ "ABCflux v1- Publication",
                                       extraction_source %in%  "CO2: ABCflux v1- User-contributed CH4: NA"  ~ "ABCflux v1- User-contributed",
                                       extraction_source %in%  "CO2: ABCflux v1- Euroflux/User-contributed CH4: NA"  ~ "ABCflux v1- Euroflux/User-contributed",
                                       extraction_source %in%  "CO2: ABCflux v1- Euroflux CH4: NA"  ~ "ABCflux v1- Euroflux",
                                       extraction_source %in%  "CO2: ABCflux v1- Natali synthesis CH4: NA"  ~ "ABCflux v1- Natali synthesis",
                                       extraction_source %in%  "CO2: ABCflux v1- Publication/User-contributed CH4: NA"  ~ "ABCflux v1- Publication/User-contributed",
                                       extraction_source %in%  "CO2: ABCflux v1- SMEAR CH4: NA"  ~ "ABCflux v1- SMEAR",
                                       extraction_source %in%  "CO2: ABCflux v1- Virkkala synthesis CH4: NA"  ~ "ABCflux v1- Virkkala synthesis", .default= extraction_source))

unique(abc.nodupes$extraction_source)

abc.nodupes$extraction_source_co2 <- NULL
abc.nodupes$extraction_source_ch4 <- NULL

## Richard's cleaning code #####-------------------------------------------------


# data_usage cleaning
abc.nodupes = abc.nodupes |>
  mutate(data_usage = if_else(data_usage %in% c("Tier2", "Tier2 = data producers must have opportunities to collaborate and consult with data users", "CO2: Tier 2 CH4: TIER2", "CO2: Tier 2 CH4: Tier 2", "Tier 1 and Tier 2", "CO2: Tier 2 CH4: NA"), "Tier 2", data_usage)) |>
  mutate(data_usage = if_else(data_usage %in% c("Tier1", "CO2: Tier 1 CH4: Tier 1", "CO2: Tier 1 CH4: NA"), "Tier 1", data_usage)) |>
  mutate(data_usage = if_else(data_usage == "CO2: NA CH4: NA", "NA", data_usage))
#unique(abc.nodupes$data_usage)

# data_version cleaning
abc.nodupes = abc.nodupes |>
  mutate(data_version = if_else(data_version %in% c("N/A", "CO2:  CH4: NA", "CO2: NA CH4: NA"), "NA", data_version)) |>
  mutate(data_version = if_else(data_version %in% c("CO2: 1-1 CH4: 1-1", "original", "CO2: v01 CH4: NA"), "1", data_version)) |>
  mutate(data_version = if_else(data_version == "CO2: v02 CH4: NA", "2", data_version)) |>
  mutate(data_version = if_else(data_version == "CO2: 4-5 CH4: 4-5", "4-5", data_version)) |>
  mutate(data_version = if_else(data_version == "CO2: 2-5 CH4: 2-5", "2-5", data_version)) |>
  mutate(data_version = if_else(data_version == "CO2: 2-1 CH4: 2-1", "2-1", data_version)) |>
  mutate(data_version = if_else(data_version == "CO2: 1-5 CH4: 1-5", "1-5", data_version)) |>
  mutate(data_version = if_else(data_version == "CO2: 7-5 CH4: 7-5", "7-5", data_version)) |>
  mutate(data_version = if_else(data_version == "CO2: 8-5 CH4: 8-5", "8-5", data_version)) |>
  mutate(data_version = if_else(data_version == "CO2: 5-5 CH4: 5-5", "5-5", data_version)) |>
  mutate(data_version = if_else(data_version %in% c("CO2: 3-5 CH4: 3-5", "CO2: AMF CH4: NA", "CO2: 3-5 CH4: NA"), "3-5", data_version)) |>
  mutate(data_version = if_else(data_version %in% c("stage averages submission, https://doi.org/10.7939/r3-5ce6-yx27", "fluxes all version resubmission, https://doi.org/10.7939/r3-5ce6-yx27"), "https://doi.org/10.7939/r3-5ce6-yx27", data_version)) |>
  mutate(data_version = if_else(data_version == "CO2: 1-4 CH4: NA", "1-4", data_version)) |>
  mutate(data_version = if_else(data_version == "CO2: beta-5 CH4: NA", "beta-5", data_version)) |>
  mutate(data_version = if_else(data_version == "CO2: 2-4 CH4: NA", "2-4", data_version)) |>
  mutate(data_version = if_else(data_version == "CO2: beta-3 CH4: NA", "beta-3", data_version)) |>
  mutate(data_version = if_else(data_version == "CO2: beta-4 CH4: NA", "beta-4", data_version))
unique(abc.nodupes$data_version)

# lai cleaning
abc.nodupes = abc.nodupes |>
  mutate(lai = if_else(lai == "1.6 ¬± 0.7 m2 m‚Äì2", "1.6", lai)) |>
  mutate(lai = if_else(lai == "0 (no vegetation areas), 0.3‚Äì0.7, Cannone et al. (2016)", NA, lai))
unique(abc.nodupes$lai)

# ndvi cleaning
unique(abc.nodupes$ndvi)
abc.nodupes = abc.nodupes |>
  mutate(ndvi = if_else(ndvi %in% c("No usable MODIS data", "NaN"), NA, ndvi))

# removing veg_n_stock
unique(abc.nodupes$veg_n_stock)
abc.nodupes = abc.nodupes |> select(-veg_n_stock)

# removing n_stock
unique(abc.nodupes$n_stock)
abc.nodupes = abc.nodupes |> select(-n_stock)

# removing org_n_stock
unique(abc.nodupes$org_n_stock)
abc.nodupes = abc.nodupes |> select(-org_n_stock)

# removing c_stem
unique(abc.nodupes$c_stem)
abc.nodupes = abc.nodupes |> select(-c_stem)

# removing n_stem
unique(abc.nodupes$n_stem)
abc.nodupes = abc.nodupes |> select(-n_stem)

# removing n_uptake
unique(abc.nodupes$n_uptake)
abc.nodupes = abc.nodupes |> select(-n_uptake)

# c_mineral cleaning
unique(abc.nodupes$c_mineral)
abc.nodupes = abc.nodupes |>
  mutate(c_mineral = if_else(c_mineral == "SOCC between 1000 to 29000 SOCC (excluding the natural occuring coal in the profile), see table F1 in Boike et al. 2018 (https://essd.copernicus.org/articles/10/355/2018/)", "NA", c_mineral))

# removing c_shallow
unique(abc.nodupes$c_shallow)
abc.nodupes = abc.nodupes |> select(-c_shallow)

# removing c_moss
unique(abc.nodupes$c_moss)
abc.nodupes = abc.nodupes |> select(-c_moss)

# removing soil_ch4_age
unique(abc.nodupes$soil_ch4_age)
abc.nodupes = abc.nodupes |> select(-soil_ch4_age)

# soil_co2_age cleaning/extracting
unique(abc.nodupes$soil_co2_age)
abc.nodupes = abc.nodupes |>
  mutate(soil_co2_age = if_else(soil_co2_age == "-", NA, soil_co2_age)) |>
  mutate(soil_co2_age = if_else(soil_co2_age == "see Estop-Aragones et al. 2018, https://doi.org/10.1088/1748-9326/aad5f0", "1600", soil_co2_age))

abc.nodupes = abc.nodupes |> mutate(citation = if_else(citation == "N/A", "NA", citation))

# land_cover cleaning
abc.nodupes = abc.nodupes |>
  mutate(land_cover = if_else(land_cover == "140=Lichens and mosses", "140", land_cover)) |>
  mutate(land_cover = if_else(land_cover %in% c("70=Tree cover, needleleaved, evergreen", "70=Tree cover, needleleaved, evergreen;", "Tree cover, needleleaved, evergreen, closed to open (>15%)"), "70", land_cover)) |>
  mutate(land_cover = if_else(land_cover == "122=Shrubland deciduous", "122", land_cover)) |>
  mutate(land_cover = if_else(land_cover %in% c("110=Mosaic herbaceous cover (>50%) / tree and shrub (<50%);", "110=Mosaic herbaceous cover (>50%) / tree and shrub (<50%)"), "110", land_cover)) |>
  mutate(land_cover = if_else(land_cover == "130=Grassland", "130", land_cover)) |>
  mutate(land_cover = if_else(land_cover %in% c("180=Shrub or herbaceous cover", "180=Shrub or herbaceous cover, flooded, fresh/saline/brakish water", "Shrub or herbaceous cover, flooded, fresh/saline/brakish water"), "180", land_cover)) |>
  mutate(land_cover = if_else(land_cover == "80=Tree cover, needleleaved, deciduous;", "80", land_cover)) |>
  mutate(land_cover = if_else(land_cover == "Sparse vegetation (tree, shrub, herbaceous cover) (<15%)", "150", land_cover)) |>
  mutate(land_cover = if_else(land_cover == "140=Lichens and mosses; 150=Sparse vegetation (tree, shrub, herbaceous cover) (<15%)", "140/150", land_cover)) |>
  mutate(land_cover = if_else(land_cover == "150/153 Sparse herbaceous cover (<15%)", "150/153", land_cover)) |>
  mutate(land_cover = if_else(land_cover == "Mix between 130 and 150", "130/150", land_cover))

# tsoil_deep cleaning
abc.nodupes = abc.nodupes |>
  mutate(tsoil_deep = if_else(tsoil_deep %in% c("N/A", "NaN"), "NA", tsoil_deep))

# moisture_depth cleaning
abc.nodupes = abc.nodupes |>
  mutate(moisture_depth = if_else(moisture_depth == "NaN", "NA", moisture_depth))

# water_table_depth cleaning
abc.nodupes = abc.nodupes |>
  mutate(water_table_depth = if_else(water_table_depth %in% c("N/A", "NaN"), "NA", water_table_depth))

# Disturbance_Category cleaning
abc.nodupes = abc.nodupes |>
  mutate(Disturbance_Category = if_else(Disturbance_Category == "No_No", "No", Disturbance_Category)) |>
  mutate(Disturbance_Category = if_else(Disturbance_Category == "Fire_Fire", "Fire", Disturbance_Category)) |>
  mutate(Disturbance_Category = if_else(is.na(Disturbance_Category), "NA", Disturbance_Category))

# disturbance cleaning
abc.nodupes = abc.nodupes |>
  mutate(disturbance = if_else(disturbance %in% c("NO", "no"), "No", disturbance))

# disturb_year cleaning
abc.nodupes = abc.nodupes |>
  mutate(disturb_year = if_else(disturb_year == "NaN", "NA", disturb_year))

# soil_type_detail cleaning
abc.nodupes = abc.nodupes |>
  mutate(soil_type_detail = if_else(soil_type_detail == "peat", "Peat", soil_type_detail)) |>
  mutate(soil_type_detail = if_else(soil_type_detail == "Histosols", "Histosol", soil_type_detail)) |>
  mutate(soil_type_detail = if_else(soil_type_detail == "Gelisols", "Gelisol", soil_type_detail))

# land_cover_bawld cleaning
abc.nodupes = abc.nodupes |>
  mutate(land_cover_bawld = if_else(land_cover_bawld == "Dry Tundra", "Dry tundra", land_cover_bawld)) |>
  mutate(land_cover_bawld = if_else(land_cover_bawld %in% c("Permafrost Bog", "PermBog"), "Permafrost bog", land_cover_bawld)) |>
  mutate(land_cover_bawld = if_else(land_cover_bawld == "Tundra Wetland", "Tundra wetland", land_cover_bawld)) |>
  mutate(land_cover_bawld = if_else(land_cover_bawld %in% c("UpTundra", "Upland Tundra"), "Upland tundra", land_cover_bawld)) |>
  mutate(land_cover_bawld = if_else(land_cover_bawld == "Wet Tundra", "Wet tundra", land_cover_bawld)) |>
  mutate(land_cover_bawld = if_else(land_cover_bawld == "Boreal Forests", "Boreal Forest", land_cover_bawld)) |>
  mutate(land_cover_bawld = if_else(land_cover_bawld == "Marshes", "Marsh", land_cover_bawld))

# landform cleaning
abc.nodupes = abc.nodupes |>
  mutate(landform = if_else(landform %in% c("Palsas", "palsas"), "Palsa", landform))

# adding c_stock data
abc.nodupes = abc.nodupes |>
  mutate(c_stock = if_else(c_stock %in% "see Heffernan et al. 2020, https://agupubs.onlinelibrary.wiley.com/doi/abs/10.1029/2019JG005501", 187.18, c_stock)) |>
  mutate(c_stock = if_else(c_stock %in% "see Pelletier et al. 2015, https://doi.org/10.1177/0959683617693899", 167, c_stock))

#adding c_stock data that Richard found in literature search
setwd("/Users/iwargowsky/Desktop/ABCFlux v2")
c_stock.extra <- read_csv("c_stock.richard.csv") %>%
  select(site_name, site_reference, c_stock, stock_depth, soil_depth, soil_perc_c, source) %>%
  dplyr::filter(!is.na(source))

c_stock.extra$notes <- paste("Additional soil carbon data from ", c_stock.extra$source) 
c_stock.extra$source <- NULL

#separating notes and data for merging purposes
c_stock.extra.notes <- c_stock.extra %>% select(site_name, site_reference, notes)
c_stock.extra.soildata <- c_stock.extra %>% select(site_name, site_reference,  c_stock, stock_depth, soil_depth, soil_perc_c)

#adding in soil carbon data
abc.nodupes.x <- natural_join(abc.nodupes, c_stock.extra.soildata,
                              by= c("site_reference", "site_name"), jointype= "FULL")
abc.nodupes.x <-abc.nodupes.x[names(abc.nodupes)] # natural_join reorders columns so this is so fix that

#adding in notes about where soil carbon data came from 
abc.nodupes <- abc.nodupes.x  %>% full_join(c_stock.extra.notes, by= c("site_name", "site_reference"))

abc.nodupes <- abc.nodupes %>%
  unite("notes", c( notes.y , notes.x), na.rm= TRUE, remove= TRUE)

## Fixing citation column #####---------------------------------------------------
abc.nodupes <- abc.nodupes %>%
  mutate(citation= ifelse(citation %in%  "CO2: NA CH4: NA", NA, citation))

# Use mutate to create new columns citation_co2 and citation_ch4
abc.nodupes <- abc.nodupes %>%
  mutate(citation_co2 = str_extract(citation, "(?<=CO2:).*?(?= CH4:)"),
         citation_ch4 = str_extract(citation, "(?<=CH4:).*")) 
  
abc.nodupes <- abc.nodupes %>%
  mutate(citation_co2 = ifelse(citation_co2 %in%  c("NA", " NA ", " NA", " N/A "), NA, citation_co2)) %>%
  mutate(citation_ch4 = ifelse(citation_ch4 %in%  c("NA", " NA ", " NA", " N/A "), NA, citation_ch4))

abc.nodupes <- abc.nodupes %>%
  mutate(citation = case_when(
    citation_co2 == citation_ch4 ~ citation_co2,                        # If CO2 and CH4 citations are the same, use that value
    is.na(citation_ch4) & !is.na(citation_co2) ~ citation_co2,          # If CH4 is NA and CO2 has an entry, use CO2 value
    is.na(citation_co2) & !is.na(citation_ch4) ~ citation_ch4,          # If CO2 is NA and CH4 has an entry, use CH4 value
    TRUE ~ paste0("CO2: ", citation_co2, " CH4: ", citation_ch4)        # If they are different, combine them
  )) %>%
  mutate(citation= ifelse(citation %in%  "CO2: NA CH4: NA", NA, citation),
         citation_co2= NULL,
         citation_ch4= NULL)

## Fixing site_reference #####---------------------------------------------------
#10/15/24 creating column FluxID to be used when fixing site_reference later on 
abc.nodupes <- abc.nodupes %>%
  mutate(FluxID= ifelse( flux_method == "EC" & !is.na(site_reference), site_reference, NA))

abc.nodupes <- abc.nodupes %>%
  mutate(site_reference = ifelse(
    flux_method == "EC",
    site_reference,
    pmap_chr(list(site_name, site_reference, flux_method), 
             ~ paste(na.omit(c(...)), collapse = "_"))
  ))

abc.nodupes <- abc.nodupes %>%
  mutate(site_reference = ifelse(
    flux_method == "EC" & is.na(site_reference),
    pmap_chr(list(site_name, flux_method), 
             ~ paste(na.omit(c(...)), collapse = "_")),
    site_reference
  ))



#check to see if site_reference is a unique column
x <- abc.nodupes %>% get_dupes(site_reference, year, month, flux_method)



##### Disturbance_Category fixing####--------------------------------------------

##Round 2 of disturbance categories
setwd("/Users/iwargowsky/Desktop/ABCFlux v2")
disturb.ikw <- read_csv("abc.static.bysite.sep24.disturb_ikw.csv") %>% select(-disturbance)

abc.nodupes$Disturbance_Category <- NULL
abc.nodupes<- abc.nodupes %>% left_join(disturb.ikw, by= c("site_reference")) 

##### Land cover classes from Anna ####--------------------------------------------
setwd("/Users/iwargowsky/Desktop/ABCFlux v2")
landcover <- read_csv("abc.static.bysite.sep24_av.csv") %>% 
  distinct(site_reference, land_cover_eco, land_cover_plot, data_contributor_or_author)
#check to see if there are duplicates (which would cause duplicates in merging)
dupes <- landcover %>% get_dupes(c("site_reference", "data_contributor_or_author"))
unique(dupes$site_reference)
##these duplicates are caused by changes in landcover over time but we will change them to be static
landcover <- landcover %>% 
  mutate(land_cover_eco= ifelse(site_reference %in% "US-Rpf", 60, land_cover_eco)) %>%
  mutate(land_cover_plot= ifelse(site_reference %in% "US-Rpf", 60, land_cover_plot)) %>%
  mutate(land_cover_plot= ifelse(site_reference %in% c("dry heath_HD Ctr 1_Chamber",
                                                       "dry heath_HD Ctr 3_Chamber"), 120, land_cover_plot)) %>% 
  distinct(site_reference, land_cover_eco, land_cover_plot, data_contributor_or_author)

#merge with data  
abc.nodupes <- abc.nodupes %>% full_join(landcover, by= c("site_reference", "data_contributor_or_author"))


#check to see make sure nothing was duplicated
x <- abc.nodupes %>% get_dupes(site_reference, year, month, flux_method)

#remove old land cover
abc.nodupes$land_cover <- NULL

## Fixing site_reference for towers to match aquatic data 10/15/24 #####---------------------------------------------------
abc.nodupes <- abc.nodupes %>%
  mutate(site_reference = ifelse(
    flux_method == "EC",
    pmap_chr(list(site_name,FluxID, "tower"), 
             ~ paste(na.omit(c(...)), collapse = "_")),
    site_reference
  ))

#check to see if site_reference is a unique column
x <- abc.nodupes %>% get_dupes(site_reference, year, month, flux_method)

#remove FluxID column
abc.nodupes$FluxID <- NULL
##### BAWLD classes from Kenzie 11/04/24 ####--------------------------------------------
setwd("/Users/iwargowsky/Desktop/ABCFlux v2")
bawld.class <- read_csv("abc.static.bysite.oct24_Kuhn.csv")%>%
  distinct(site_reference, land_cover_bawld2, data_contributor_or_author)
#merge with df
abc.nodupes <- abc.nodupes %>% full_join(bawld.class, by= c("site_reference", "data_contributor_or_author"))
#remove old bawld column
abc.nodupes$land_cover_bawld <- NULL
abc.nodupes <- abc.nodupes %>% dplyr::rename("land_cover_bawld"="land_cover_bawld2")

## Unifying some site_names #####---------------------------------------------------
abc.nodupes  <- abc.nodupes %>%
  mutate(site_name= ifelse(site_name %in% "Stordalen Palsa Bog (ICOS)", "Stordalen Palsa Bog", site_name ),
         site_reference= ifelse(site_reference %in% "Stordalen Palsa Bog (ICOS)_SE-Sto_tower", "Stordalen Palsa Bog_SE-Sto_tower", site_reference)) %>%
  mutate(site_name= ifelse(site_name %in% "Stordalen - Fen", "Stordalen Fen", site_name ),
         site_reference= ifelse(site_reference %in% "Stordalen - Fen_SE-St1_tower", "Stordalen Fen_SE-St1_tower", site_reference)) %>%
  mutate(site_name= ifelse(site_name %in% c("Stordalen Mire", "Stordalen, Sweden"), "Stordalen", site_name))
 

## Unifying flux_method_detail #####---------------------------------------------------
#paste info from detail to description before unifying so information is lost when unifying
abc.nodupes <- abc.nodupes %>%
  mutate(flux_method_description= paste(flux_method_detail, flux_method_description, sep="-"))

#unifying
abc.nodupes <- abc.nodupes %>%
  mutate(flux_method_detail = case_when(
    flux_method_detail %in% c("Automated", "automated chamber", "Automated chamber measurements", 
                              "Automated chamber system", "automated chamber, every  1 hr", 
                              "automated chamber, every  3 hr" , "Automatic chambers", 
                              "Automatic closed chamber", "automatic, closed", "Chambers_mostly_automatic") ~ "Automatic chamber",
    
    flux_method_detail %in% c("CO2: Closed path CH4: Open path", "CO2: closed-path; CH4: open-path", 
                              "Closed path CO2, Open path CH4", "Enclosed CO2; Open-path CH4", 
                              "enclosed-path CO2, open-path CH4") ~ "CO2: Closed-path eddy covariance, CH4: Open-path eddy covariance",
    
    flux_method_detail %in% c("CH4: closed path eddy covariance, CO2: open-path eddy covariance", 
                              "Open-path CO2 measurements (LI7500RS), closed-path CH4 measurements (DLT-100)") ~ "CO2: Open-path eddy covariance, CH4: Closed-path eddy covariance",
    
    flux_method_detail %in% c("Closed path", "closed-path", "Closed-path eddy covariance", 
                              "closed-path eddy covariance", "EC_closed", "EC_enclosed", 
                              "Enclosed", "enclosed", "enclosed eddy covariance", "enclosed-path", 
                              "enclosed-path eddy covariance", "Encolsed path") ~ "Closed-path eddy covariance",
    
    flux_method_detail %in% c("Open", "Open path", "Open Path CO2 & CH4", "Open Path EC", 
                              "Open-path CO2 measurements (LI7500RS)", 
                              "Open-path CO2 measurements (LI7500RS), open-path CH4 measurements (LI7700)", 
                              "Open-path EC", "open-path EC", "open-path eddy covariance", 
                              "Open-path eddy covariance", "Open-path eddy-covariance", "EC_open") ~ "Open-path eddy covariance",
    
    flux_method_detail %in% c("Manual chamber measurement. Flux estimates from three repetitions were averaged together.", 
                              "Manual chamber measurement. Flux estimates from three repetitions were averaged together., Manual chamber measurement. Three replicate fluxes were estimated and averaged using an opaque cover, and three replicate fluxes were estimated and averaged without the opaque cover.", 
                              "Manual chamber measurements (dark chamber)", 
                              "manual chamber measurements",
                              "Manual chamber with a volume of 8.5 L and surface of 706.5 cm2", 
                              "Manual one-time chamber measurements, average of 4 collars", 
                              "Manual samples 3,10,15,20min", "Manual closed chamber", 
                              "Manual closed chambers", "mostly manual chambers", "mostly manual chamber measurements",
                              "Manual chamber", "Manual chamber measurements" , "Mostly manual chamber measurements"  ,
                              "Manual measurements", "Manual chamber measurement" , "Manual",
                              "Manual non-steady-state (closed) chamber measurements during snow-free period", 
                              "non-steady state, closed dynamic flux chamber", "Static chamber", 
                              "Static chambers", "Static chambers, six replicates in each 'site_reference'", 
                              "Static closed chambers", "Static manual chamber", "Chambers_mostly_manual",
                              "Manual chambers", "Manual chamber with a  volume of 8.5 L and surface of 706.5 cm2" ,
                              "Static manual chambers", "Chambers consisted of opaque, plastic 20-L buckets with bottoms removed and resealable air-tight lids (Gamma Seal Lid, Encore Plastics, Sandusky, OH, USA) Chambers blocked 88% (41%) of photosynthetically active radiation measured with a cosign quantum flux sensor (Apogee Instruments, Logan, UT) in full sun (shade).", 
                              "Closed chamber", "Closed Chamber", "Closed chamber and gas bottles", 
                              "Closed chamber and LI7810 gas analyser", "Closed chambers", 
                              "Closed chambers on 56 cm * 56 cm permanent frames", "close chamber, evry 4 h", 
                              "Closed manual chamber", "Closed-loop chambers", 
                              "Closed loop system", "Closed dynamic chamber method", "Manual Chamber",
                              "Closed dynamic manual", "Chambers_snow", "manual chamber",
                              "Following methods described in Shaver et al (2007)") ~ "Manual chamber",
    
    flux_method_detail %in% c("EC_open & closed", "closed and open-path eddy covariance", 
                              "Closed path analyzers were mainly used, at times when closed path analyzers was not functioning, open path data were used if available. OP (Li-7500 Licor), CP (FGGA Los Gatos)") ~ "Open and Closed-path eddy covariance",
   
     flux_method_detail %in% "CO2: EC_open CH4: Combination of the gradient and eddy covariance methods" ~ "CO2: Open-path eddy covariance CH4: Combination of the gradient and eddy covariance methods" ,
    
    flux_method_detail %in% "Chambers_CUV" ~ "Chamber/Cuvettes" ,
    
    TRUE ~ flux_method_detail
  )) 

abc.nodupes <- abc.nodupes %>% 
  mutate(flux_method_detail= ifelse(is.na(flux_method_detail), flux_method, flux_method_detail))

unique(abc.nodupes$flux_method_detail)
#### fix site name for Ranskalankorpi ####---------------------------------------------------------
abc.nodupes <- abc.nodupes %>% 
  mutate(site_name = ifelse(site_name %in% "Ranskalankorpi, Continuous cover forestry treatment", "Ranskalankorpi", site_name)) %>% 
  mutate(site_reference= ifelse(site_reference %in% "Ranskalankorpi, Continuous cover forestry treatment_FI-Ran forestry treatment_tower", "Ranskalankorpi_FI-Ran forestry treatment_tower", site_reference))

#### QUALITY FLAG####---------------------------------------------------------

#df of quantiles CO2
quantiles.co2 <- abc.nodupes %>%
  group_by(month, biome) %>%
  reframe(lower.nee= quantile(nee, .025, na.rm=T),
          upper.nee= quantile(nee, .975, na.rm=T),
          lower.reco= quantile(reco, .025, na.rm=T),
          upper.reco= quantile(reco, .975, na.rm=T),
          lower.gpp= quantile(gpp, .025, na.rm=T),
          upper.gpp= quantile(gpp, .975, na.rm=T))


# Join quantiles with abc.nodupes and add QC column for CO2 
abc.nodupes <- abc.nodupes %>%
  left_join(quantiles.co2, by = c("month", "biome")) %>%
  mutate(expert_flag_co2 = ifelse(nee > lower.nee & nee < upper.nee, 0, 1),
         expert_flag_reco= ifelse(reco > lower.reco & reco < upper.reco, 0, 1),
         expert_flag_gpp= ifelse(gpp > lower.gpp & gpp < upper.gpp, 0, 1),
         expert_flag_partitioned = pmax(expert_flag_reco, expert_flag_gpp, na.rm = T)) %>%
  mutate(expert_flag_co2= ifelse(is.na(expert_flag_co2), expert_flag_partitioned, expert_flag_co2))

#df of quantiles CH4
quantiles.ch4 <- abc.nodupes %>%
  group_by(month, biome, land_cover_bawld) %>%
  reframe(lower.ch4.2.5 = quantile(ch4_flux_total, .025, na.rm=T),
          upper.ch4.97.5= quantile(ch4_flux_total, .975, na.rm=T),
          lower.ch4.1 = quantile(ch4_flux_total, .1, na.rm=T),
          upper.ch4.99= quantile(ch4_flux_total, .99, na.rm=T))

# Join quantiles with abc.nodupes and add QC column for Ch4
abc.nodupes <- abc.nodupes %>%
  left_join(quantiles.ch4, by = c("month", "biome", "land_cover_bawld")) %>%
  mutate(expert_flag_ch4.97.5 = ifelse(ch4_flux_total > lower.ch4.2.5 & ch4_flux_total < upper.ch4.97.5, 0, 1),
         expert_flag_ch4.99 = ifelse(ch4_flux_total > lower.ch4.1 & ch4_flux_total < upper.ch4.99, 0, 1))

         
abc.nodupes <- abc.nodupes %>% select(-c(lower.nee, upper.nee, lower.reco, upper.reco,
                                        lower.gpp, upper.gpp, expert_flag_reco, 
                                        expert_flag_gpp, expert_flag_partitioned,
                                        lower.ch4.2.5, lower.ch4.1, upper.ch4.97.5, upper.ch4.99))

#Quality flag 2
# abc.nodupes <- abc.nodupes %>%
#   mutate(expert_flag_co2= ifelse(site_name %in% c("RU-Eusk_cher2"), 2, expert_flag_co2),
#          expert_flag_ch4= ifelse(site_name %in% c("RU-Eusk_cher2"), 2, expert_flag_ch4))
# 
# 
# #Quality flag 3
# abc.nodupes <- abc.nodupes %>%
#   mutate(expert_flag_co2= ifelse(site_reference %in% c("Iskoras_NO-Isk-fen_tower",
#                                                        "Iskoras_NO-Isk-palsa_tower",
#                                                        "Ranskalankorpi_FI-Ran forestry treatment_tower"), 3, expert_flag_co2),
#          expert_flag_ch4= ifelse(site_reference %in% c("Iskoras_NO-Isk-fen_tower",
#                                                   "Iskoras_NO-Isk-palsa_tower",
#                                                   "Ranskalankorpi_FI-Ran forestry treatment_tower"), 3 expert_flag_ch4)))

#### SAVE FINAL DF ####---------------------------------------------------------


#remove rows that do not contain flux data
abc.nodupes <- abc.nodupes %>% dplyr::filter(!if_all(c(nee, gpp, reco, ch4_flux_total, nee_seasonal, ch4_flux_seasonal,
                                                       ch4_flux_diffusion,ch4_flux_ebullition, ch4_flux_storage,co2_flux_storage, 
                                                       ch4_flux_storage_bubble, co2_flux_storage_bubble), ~ is.na(.)))


setwd("/Users/iwargowsky/Desktop/arcticborealCflux")   
write_csv(abc.nodupes, "ABC.v2.dec24.cleanish.nodupes.csv")






















 




#list of potential co-authors
unique(abc.nodupes$extraction_source)
coauthors <- abc.nodupes %>% 
  dplyr::filter(!(extraction_source %in% c("Publication" , "ABCflux v1- Publication",
                                           "ABCflux v1- User-contributed", "ABCflux v1- Euroflux/User-contributed",                     
                                           "ABCflux v1- Euroflux", "ABCflux v1- Natali synthesis",                              
                                           "ABCflux v1- Publication/User-contributed",                                     
                                           "BAWLD-CH4-Publication" , "ABCflux v1- SMEAR",                                         
                                          "ABCflux v1- Virkkala synthesis" ))) %>%
  dplyr::select(site_name, data_contributor_or_author, email, extraction_source, flux_method) %>%
  distinct(site_name, data_contributor_or_author, email, flux_method, .keep_all = TRUE) %>%
  arrange(data_contributor_or_author)


setwd("/Users/iwargowsky/Desktop/ABCFlux v2")   
write_csv(coauthors, "terr.coauthors2.csv")







#carbon stock for Richard
x <- abc.nodupes %>%
  dplyr::filter(is.na(c_stock)) %>%
  dplyr::filter(flux_method== "EC") %>%
  group_by(site_name, site_reference, latitude, longitude, veg_detail, land_cover, land_cover_bawld, c_stock, stock_depth, soil_depth, soil_perc_c) %>%
  summarise(n= n())

x$source <- ""


setwd("/Users/iwargowsky/Desktop")   
write_csv(x, "sites.wo.c_stock.csv")



#list of EC towers for Ted and Grant
ECsites.datescovered <- abc %>% 
  dplyr::filter(flux_method %in% "EC" ) %>%
  mutate(ts= as.yearmon(paste(year, month,sep = '-'))) %>%
  group_by(site_name, site_reference, data_contributor_or_author, email, latitude, longitude) %>% 
  dplyr::summarise (start= first(ts), 
                    end= last(ts), 
                    num_months= n()) 

setwd("/Users/iwargowsky/Desktop")   
write_csv(ECsites.datescovered, "ABCv2.ECsites.datescovered.csv")



##which sites have full time series

x.co2 <- abc.nodupes %>% dplyr::filter(flux_method== "EC") %>%
  dplyr::filter(!gap_fill_perc_nee>99.9) %>%
  dplyr::filter(!is.na(nee)) %>%
  group_by(year, site_name, site_reference, month)%>%  dplyr::summarise(nee= mean(nee, na.rm= TRUE)) %>%
  group_by(year, site_name, site_reference)%>% dplyr::summarise(n=n(), annee=sum(nee, na.rm= FALSE))%>%
  filter(n == 12) %>% group_by(site_name, site_reference)%>%dplyr::summarise(n=n(), annee=sum(annee, na.rm= FALSE))







ru.sam <- abc %>% dplyr::filter(site_name== "Samoylov Island") 
ru.sam$ts <- as.yearmon(paste(ru.sam$year, ru.sam$month, sep="-"))


ggplot(ru.sam)+ geom_line(aes(x=ts, y=nee, color= site_reference)) +
  geom_point(aes(x=ts, y=nee, color= site_reference))






### static by site ##########--------------------------------------------------------------
#remove dynamic variables
setwd("/Users/iwargowsky/Desktop/ABCFlux v2") 
staticvars <- read_csv("ABCfluxv2.staticvars.csv") 
staticvars$land_cover_bawld<- ""
staticvars$Disturbance_Category <- ""

abc.static <- abc.nodupes %>% dplyr::select(colnames(staticvars)) %>%
  dplyr::select(-c(canopy_height))

#look
abc.static.condense<-  abc.static  %>% distinct()
setwd("/Users/iwargowsky/Desktop")   
#write_csv(abc.static.condense , "abc.static.bysite.dec24.csv")

#looking at which sites are NA
# z <- abc.static.condense %>% filter(is.na(land_cover_bawld_Kuhn)) 
# unique(z$site_name)
# y <- abc.static.condense %>% filter(is.na(Disturbance_Category)) 
# unique(y$site_name)


  
####Land_cover_bawld and Disturb_Category ROUND 2
abc.static <- abc.nodupes %>% group_by(site_reference, data_contributor_or_author, extraction_source, citation,
                                      country, latitude, longitude, biome, veg_detail,
                                      land_cover, land_cover_bawld, landform,
                                      permafrost, permafrost_thaw, disturbance, disturb_year, disturb_severity, 
                                      flux_method, flux_method_detail, flux_method_description, c_stock) %>%
  dplyr::summarise(min_ch4= min(ch4_flux_total),
                   max_ch4= max(ch4_flux_total))

setwd("/Users/iwargowsky/Desktop")   
write_csv(abc.static , "abc.static.bysite.ch4.dec24.csv")



####Disturb_Category ROUND 2
abc.static <- abc.nodupes %>% select(site_reference, disturbance, Disturbance_Category) %>% distinct()

setwd("/Users/iwargowsky/Desktop")   
write_csv(abc.static , "abc.static.bysite.dec24.disturb.csv")

####Land_cover_bawld for Kenzie
abc.static <- abc.nodupes %>% group_by(site_reference, data_contributor_or_author, extraction_source, citation,
                                       country, latitude, longitude, biome, veg_detail,
                                       land_cover_plot, land_cover_eco, land_cover_bawld, landform,
                                       permafrost, permafrost_thaw, disturbance, Disturbance_Category, flux_method) %>%
  dplyr::summarise(min_ch4= min(ch4_flux_total),
                   max_ch4= max(ch4_flux_total))


setwd("/Users/iwargowsky/Desktop")   
write_csv(abc.static , "abc.static.bysite.dec24.csv")

###NEW LAND COVER CLASSES


