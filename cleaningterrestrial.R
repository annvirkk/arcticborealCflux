###Cleaning terrestrial data####
library(dplyr)
library(readr)
library(rquery)

setwd("/Users/iwargowsky/Desktop/arcticborealCflux")   
abc <- read_csv("ABC.v2.apr24.csv")
abc$extraction_source <- paste("CO2:", abc$extraction_source_co2, "CH4:", abc$extraction_source_ch4, sep= " ")



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
  select(site_name, site_reference, land_cover_bawld_Kuhn, Disturbance_Category)%>% 
  distinct()


x <- kuhn_landcover_disturb %>% get_dupes(site_reference, site_name)

abc <- abc %>% left_join( kuhn_landcover_disturb, by= c("site_reference", "site_name"))


#setwd("/Users/iwargowsky/Desktop/arcticborealCflux")
#write_csv(abc.x , "ABC.v2.apr24.bawld.disturb.csv")

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
# write_csv(abc, "ABC.v2.apr24.full.csv")

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

### Precip #####_--------------------------------------------------------

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
                                                     "Vaisjeaggi, northern Finland"), as.numeric(water_table_depth) * -1, water_table_depth)) %>%
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
                                 extraction_source_ch4 %in% "User-contributed", soil_moisture *100, soil_moisture))
  
unique(abc$moisture_depth)
abc <- abc %>%
  mutate(moisture_depth= ifelse(moisture_depth %in% "0-10", 5, moisture_depth )) %>%
  mutate(moisture_depth= ifelse(moisture_depth %in% "0-6", 3, moisture_depth )) %>%
  mutate(moisture_depth= ifelse(moisture_depth %in% "0-6 cm", 3, moisture_depth )) %>%
  mutate(moisture_depth= ifelse(moisture_depth %in% "0-6 cm below soil surface", 3, moisture_depth )) %>%
  mutate(moisture_depth= ifelse(moisture_depth %in% "0-5", 2.5, moisture_depth )) %>%
  mutate(moisture_depth= ifelse(moisture_depth %in% "2.5 cm", 2.5, moisture_depth ))

### Active layer thickness#####_------------------------------------------------------------------

### removing weird fluxes #####_--------------------------------------------------------

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

abc <- abc %>% dplyr::filter(!(site_name== "Poker Flat Research Range Black Spruce Forest"&
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

setwd("/Users/iwargowsky/Desktop/arcticborealCflux")   
 write_csv(abc, "ABC.v2.apr24.cleanish.wdupes.csv")

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
write_csv(abc, "ABC.v2.apr24.cleanish.nodupes.csv")















### static by site ##########--------------------------------------------------------------
#remove dynamic variables
setwd("/Users/iwargowsky/Desktop/ABCFlux v2") 
staticvars <- read_csv("ABCfluxv2.staticvars.csv") 
staticvars$land_cover_bawld_Kuhn <- ""
staticvars$Disturbance_Category <- ""

abc.static <- abc.x.y %>% select(colnames(staticvars))

#look
abc.static.condense<-  abc.static  %>% distinct()
setwd("/Users/iwargowsky/Desktop/ABCFlux v2")
#write_csv(abc.static.condense , "abc.static.bysite.updated.april9.csv")

#looking at which sites are NA
# z <- abc.static.condense %>% filter(is.na(land_cover_bawld_Kuhn)) 
# unique(z$site_name)
# y <- abc.static.condense %>% filter(is.na(Disturbance_Category)) 
# unique(y$site_name)


  