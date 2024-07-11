###Cleaning terrestrial data####
library(dplyr)
library(readr)
library(rquery)
library(stringr)

setwd("/Users/iwargowsky/Desktop/arcticborealCflux")   
abc <- read_csv("ABC.v2.jul24.csv")
abc$extraction_source <- paste("CO2:", abc$extraction_source_co2, "CH4:", abc$extraction_source_ch4, sep= " ")
abc$citation <- paste("CO2:", abc$citation_co2, "CH4:", abc$citation_ch4, sep= " ")


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


x <- kuhn_landcover_disturb %>% get_dupes(site_reference, site_name)

abc <- abc %>% left_join( kuhn_landcover_disturb, by= c("site_reference", "site_name")) %>%
  unite("Disturbance_Category", c(Disturbance_Category.x, Disturbance_Category.y), na.rm= TRUE, remove= TRUE)


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
                                                     "Lutose",
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
  mutate(soil_moisture= ifelse(soil_moisture< 0, 0, soil_moisture))
  
  
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
  mutate(biome= ifelse(biome %in% c("Cool temperate" ), 
                       "Temperate", biome)) 




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
  mutate(flux_method= ifelse(flux_method %in% c("5-min chamber flux" ), 
                             "Chamber", flux_method))


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
  mutate(c_stock = ifelse(c_stock %in% c("see Pelletier et al. 2015, https://doi.org/10.1177/0959683617693899" ,
                                         "see Heffernan et al. 2020, https://agupubs.onlinelibrary.wiley.com/doi/abs/10.1029/2019JG005501"    ), NA, c_stock)) %>%
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
  mutate(disturb_year = ifelse(disturb_year %in% c("N/A", "No", "unknown", "Unknown","-", "0" ), NA, disturb_year)) %>%
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
# 
#Isabel:I found a few mistakes but fixed errors directly in excel for 5 sites (Blaesedalen, Hospitaldalen, Mellemlandet, Skarvefjed, UaF)

### gap_fill_perc_ #####
abc <- abc %>%
  mutate(gap_fill_perc_nee = str_remove(gap_fill_perc_nee, "%"),
         gap_fill_perc_nee = as.numeric(gap_fill_perc_nee)) %>%
  mutate(gap_fill_perc_ch4 = str_remove(gap_fill_perc_ch4, "%"),
         gap_fill_perc_ch4 = as.numeric(gap_fill_perc_ch4)) %>%
  mutate(gap_fill_perc_gpp = str_remove(gap_fill_perc_gpp, "%"),
         gap_fill_perc_gpp = as.numeric(gap_fill_perc_gpp)) %>%
  mutate(gap_fill_perc_reco = str_remove(gap_fill_perc_reco, "%"),
         gap_fill_perc_reco = as.numeric(gap_fill_perc_reco)) 


### removing blank columns #####_-----------------------------------------------
colnames(abc)
unique(abc$chamber_nr_measurement_days)



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
               dplyr::filter(!(site_name== "NEON Healy (HEAL)"&  year == 2019 & month < 9)) 
  
abc <- abc %>% dplyr::filter(!(site_name== "Ontario - Groundhog River, Boreal Mixedwood Forest"&
                                 extraction_source_co2== "Fluxnet2015" &
                                 year %in% c(2003, 2014)))

abc <- abc %>% dplyr::filter(!(site_name== "Poker Flat Research Range Black Spruce Forest" &
                                 extraction_source_co2== "Fluxnet2015" &
                                 year == 2010))

abc <- abc %>% dplyr::filter(!(site_name== "Poker Flat Research Range: Succession from fire scar to deciduous forest"&
                                 extraction_source_co2== "Ameriflux" &
                                 year == 2008))

abc <- abc %>% dplyr::filter(!(site_name== "Quebec - 1975 Harvested Black Spruce"&
                                 year == 2007))

abc <- abc %>% dplyr::filter(!(site_name== "Quebec - Eastern Boreal, Mature Black Spruce"&
                                 year == 2003))

abc <- abc %>% dplyr::filter(!(site_name== "Rosinedal-3"&
                                 year == 2014))

abc <- abc %>% dplyr::filter(!(site_name== "Saskatchewan - Western Boreal, forest burned in 1977"&
                                 year == 2003 &
                                 month < 8))

abc <- abc %>% dplyr::filter(!(site_name== "Saskatchewan - Western Boreal, forest burned in 1989"&
                                 year %in% c(2006, 2001)))

abc <- abc %>% dplyr::filter(!(site_name== "Saskatchewan - Western Boreal, Jack Pine harvested in 2002" &
                                 year < 2004)) %>%
                dplyr::filter(!(site_name== "Saskatchewan - Western Boreal, Jack Pine harvested in 2002" &
                        year %in% c(2008,2009)))

abc <- abc %>% dplyr::filter(!(site_name== "Svartberget" &
                                 year == 2017))

abc <- abc %>% dplyr::filter(!(site_name== "Tiksi" &
                                 flux_method == "EC" &
                                 year == 2010 &
                                 month < 7))

abc <- abc %>% dplyr::filter(!(site_name== "UCI-1850 burn site" & year < 2002)) %>%
               dplyr::filter(!(site_name== "UCI-1850 burn site" & year == 2003 & month < 6)) 
  
abc <- abc %>% dplyr::filter(!(site_name== "UCI-1930 burn site" & gap_fill_perc_nee > 75)) 

abc <- abc %>% dplyr::filter(!(site_name== "UCI-1964 burn site" & year < 2002))

abc <- abc %>% dplyr::filter(!(site_name== "UCI-1964 burn site wet" & year < 2003))

abc <- abc %>% dplyr::filter(!(site_name== "UCI-1981 burn site" & year < 2002))

abc <- abc %>% dplyr::filter(!(site_name== "UCI-1989 burn site" & year < 2002))

abc <- abc %>% dplyr::filter(!(site_name== "UCI-1998 burn site" & year < 2002)) %>%
               dplyr::filter(!(site_name== "UCI-1998 burn site" & year == 2003 & month < 6))

abc <- abc %>% dplyr::filter(!(site_name== "UCI-1989 burn site" & year < 2002))

abc <- abc %>% dplyr::filter(!(site_name== "Hyytiala" & year < 1997))

abc <- abc %>% dplyr::filter(!(site_name== "Norunda" & year > 2022))

abc <- abc %>% dplyr::filter(!(site_name== "Cherskii" & year == 2002 & month <7))

#removing these columns since we no longer need them
abc$extraction_source_co2 <- NULL
abc$extraction_source_ch4 <- NULL

setwd("/Users/iwargowsky/Desktop/arcticborealCflux")   
write_csv(abc, "ABC.v2.jul24.cleanish.wdupes.csv")

### duplicate fluxes #####_--------------------------------------------------------
abc <- abc %>%
  mutate(pref1= case_when(extraction_source %in% c("CO2: User-contributed CH4: User-contributed",
                                                  "CO2: User-contributed CH4: NA",                                      
                                                  "CO2: NA CH4: User-contributed",
                                                  "CO2: NA CH4: BAWLD-CH4-Publication/User-contributed",                
                                                  "CO2: User-contributed/Publication CH4: User-contributed/Publication",
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
                                                   "CO2: Arctic Data Center CH4: NA" ,                                   
                                                   "CO2: NA CH4: Arctic Data Center",                                    
                                                   "CO2: Zenodo CH4: Zenodo" ,                                           
                                                   "CO2: Zenodo CH4: NA",    
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



setwd("/Users/iwargowsky/Desktop/arcticborealCflux")   
write_csv(abc.nodupes, "ABC.v2.jul24.cleanish.nodupes.csv")

#carbon stock for Richard
x <- abc.nodupes %>%
  dplyr::filter(is.na(c_stock)) %>%
  dplyr::filter(flux_method== "EC") %>%
  group_by(site_name, site_reference, latitude, longitude, veg_detail, land_cover, land_cover_bawld, c_stock, stock_depth, soil_depth, soil_per_c) %>%
  summarise(n= n())

x$source <- ""


setwd("/Users/iwargowsky/Desktop")   
write_csv(x, "sites.wo.c_stock.csv")








### static by site ##########--------------------------------------------------------------
#remove dynamic variables
setwd("/Users/iwargowsky/Desktop/ABCFlux v2") 
staticvars <- read_csv("ABCfluxv2.staticvars.csv") 
staticvars$land_cover_bawld_Kuhn <- ""
staticvars$Disturbance_Category <- ""

abc.static <- abc.nodupes %>% dplyr::select(colnames(staticvars))

#look
abc.static.condense<-  abc.static  %>% distinct()
setwd("/Users/iwargowsky/Desktop")   
#write_csv(abc.static.condense , "abc.static.bysite.jul24.csv")

#looking at which sites are NA
# z <- abc.static.condense %>% filter(is.na(land_cover_bawld_Kuhn)) 
# unique(z$site_name)
# y <- abc.static.condense %>% filter(is.na(Disturbance_Category)) 
# unique(y$site_name)


  