##This script it meant to convert data from ABCflux V1 format to ABCflux V2 format
library(dplyr)
library(readr)
#Load data you are trying to convert
setwd("/Users/iwargowsky/Desktop/ABC flux v1")
dat <- read_csv("Arctic_Boreal_CO2_Flux.csv", na =c("NA", "-9999")) 
#load list of ABC V2 variables
setwd("/Users/iwargowsky/Desktop/ABCFlux v2")
v2.vars <- read_csv("abc.v2.vars.csv")

#rename variables from ABC V1 format to V2
dat.renamed <- dat %>% dplyr::rename("site_id"= "study_id",
                                     "year" = "meas_year",
                                     "month" = "interval_month",
                                     "tair"= "tair_int",
                                     "precip"="precip_int",
                                     "ppfd"="par_ppfd",
                                     "soil_depth"= "sol_depth",
                                      "soil_perc_c"= "soil_perc_carbon",
                                     "gap_fill_perc_nee"= "gap_fill_perc")
dat.renamed <- dat.renamed %>% mutate(tsoil_surface_depth= ifelse(tsoil_depth<=10, tsoil_depth, NA),
                                      tsoil_deep_depth= ifelse(tsoil_depth>10, tsoil_depth, NA),
                                      tsoil_surface= ifelse(tsoil_depth<=10, tsoil, NA),
                                      tsoil_deep= ifelse(tsoil_depth>10, tsoil, NA))
#Merge renamed dataframe with the column names for ABC V2
dat.merged <-  rbindlist(list(dat.renamed, v2.vars), fill = TRUE)
#Keep only columns that exist in ABC V2
dat.v2 <- dat.merged %>% select(colnames(v2.vars))

#write_csv(dat.v2, "ABCfluxv1.v2format.csv")

############ERRORS in V1######----------------------------------------------------
#Se-St1 to SE-St1 
dat.v2 <- dat.v2 %>% 
  mutate(site_reference = ifelse(site_reference == "Se-St1", "SE-St1", site_reference),
         site_id = ifelse(site_id == "Friborg_Se-St1_tower1", "Friborg_SE-St1_tower1", site_id))
#DK-Zah, Heath to GL-ZaH                   
dat.v2 <- dat.v2 %>% 
  mutate(site_reference = ifelse(site_reference == "DK-Zah, Heath", "GL-ZaH", site_reference),
         site_id = ifelse(site_id == "Lund_DK-ZaH_tower1", "Lund_GL-ZaH_tower1", site_id))
#NO-Blv to SJ-Blv
dat.v2 <- dat.v2 %>% 
  mutate(site_reference = ifelse(site_reference == "NO-Blv", "SJ-Blv", site_reference),
         site_id = ifelse(site_id == "Boike_NO-Blv_tower1", "Boike_SJ-Blv_tower1", site_id),
         site_name = ifelse(site_reference == "SJ-Blv", "Bayelva, Svalbard", site_name))
#NO-Adv to SJ-Adv
dat.v2 <- dat.v2 %>% 
  mutate(site_reference = ifelse(site_reference == "NO-Adv", "SJ-Adv", site_reference),
         site_id = ifelse(site_id == "Boike_NO-Adv_tower1", "Boike_SJ-Adv_tower1", site_id))
#RU-Skp to RU-SkP
dat.v2 <- dat.v2 %>% 
  mutate(site_reference = ifelse(site_reference == "Ru-Skp", "RU-SkP", site_reference))
#US-BES to US-Bes
dat.v2 <- dat.v2 %>% 
  mutate(site_reference = ifelse(site_reference == "US-BES", "US-Bes", site_reference),
         site_id = ifelse(site_id == "Kwon_US-BES  _tower1", "Kwon_US-Bes_tower1", site_id),
         site_name= ifelse(site_reference == "US-Bes", "Barrow-BES", site_name))
#US-BEO to US-Beo
dat.v2 <- dat.v2 %>% 
  mutate(site_reference = ifelse(site_reference == "US-BEO", "US-Beo", site_reference),
         site_id = ifelse(site_id == "Kwon_US-BEO_tower1", "Kwon_US-Beo_tower1", site_id),
         site_name= ifelse(site_reference == "US-Beo", "Barrow-BEO", site_name))
#Separating US-Brw to US-Brw, US-Bes, US-Beo
dat.v2 <- dat.v2 %>%
  mutate(site_reference= ifelse(site_id == "Zona_US-Brw_tower1", "US-Beo", site_reference)) %>%
  mutate(site_reference= ifelse(site_id == "Zona_US-Brw_tower2", "US-Bes", site_reference)) %>%
  mutate(site_reference= ifelse(site_id == "Zona_US-Brw_tower3", "US-Brw", site_reference)) %>%
  mutate(site_name= ifelse(site_id == "Zona_US-Brw_tower1", "Barrow-BEO", site_name)) %>%
  mutate(site_name= ifelse(site_id == "Zona_US-Brw_tower2", "Barrow-BES", site_name)) %>%
  mutate(site_name= ifelse(site_id == "Zona_US-Brw_tower3", "Barrow-CMDL", site_name)) %>%
  mutate(site_id= ifelse(site_id == "Zona_US-Brw_tower1", "Zona_US-Beo_tower1", site_id)) %>%
  mutate(site_id= ifelse(site_id == "Zona_US-Brw_tower2", "Zona_US-Bes_tower1", site_id)) %>%
  mutate(site_id= ifelse(site_id == "Zona_US-Brw_tower3", "Zona_US-Brw_tower1", site_id))
#US-KOC, Council to US-KOC
dat.v2 <- dat.v2 %>% 
  mutate(site_reference = ifelse(site_reference == "US-KOC, Council", "US-KOC", site_reference),
         site_name= ifelse(site_reference == "US-KOC", "Council, Alaska", site_name))
#Remove special character from Andøya
dat.v2 <- dat.v2 %>% 
  mutate(site_name= ifelse(site_reference == "NO-And", "Andoya", site_name) )
#Adding more specific names to Imnavait Creek sites
dat.v2 <- dat.v2 %>% 
  mutate(site_name= ifelse(site_reference == "US-ICh", "Imnavait Creek Watershed Heath Tundra", site_name) )%>%
  mutate(site_name= ifelse(site_reference == "US-ICs", "Imnavait Creek Watershed Wet Sedge Tundra", site_name) )%>%
  mutate(site_name= ifelse(site_reference == "US-ICt", "Imnavait Creek Watershed Tussock Tundra", site_name) )
#removing RU-Murk since this data was given to us by Aleksandr Sabrekov
dat.v2 <- dat.v2 %>% dplyr::filter(!site_reference %in% "RU-Murk")
#Zackenberg specifying names
dat.v2 <- dat.v2 %>% 
  mutate(site_name= ifelse(site_reference == "GL-ZaH", "Zackenberg Heath", site_name) )%>%
  mutate(site_name= ifelse(site_reference == "GL-ZaF", "Zackenberg Fen", site_name) )
# Samoylov Island to Samoylov to match fluxnet
#Poker Flats to Poker Flat Research Range Black Spruce Forest
dat.v2 <- dat.v2 %>% 
  mutate(site_name= ifelse(site_reference == "US-Prr", "Poker Flat Research Range Black Spruce Forest", site_name) )
#Bayelva, Svalbard to Bayelva, Spitsbergen
dat.v2 <- dat.v2 %>% 
  mutate(site_name= ifelse(site_reference == "SJ-Blv", "Bayelva, Spitsbergen", site_name) )
#Disko Island to Disko
dat.v2 <- dat.v2 %>% 
  mutate(site_name= ifelse(site_reference == "GL-Dsk", "Disko", site_name) )
#Knottåsen to Knottasen
dat.v2 <- dat.v2 %>% 
  mutate(site_name= ifelse(site_reference == "FI-Kno", "Knottasen", site_name) )
#Cherskiy to Cherski
dat.v2 <- dat.v2 %>% 
  mutate(site_name= ifelse(site_reference == "RU-Che", "Cherski", site_name) )
#Kobbefjord to Nuuk Fen
dat.v2 <- dat.v2 %>% 
  mutate(site_name= ifelse(site_reference == "GL-NuF", "Nuuk Fen", site_name) )
#Stordalen grassland to Stordalen - Fen
dat.v2 <- dat.v2 %>% 
  mutate(site_name= ifelse(site_reference == "SE-St1", "Stordalen - Fen", site_name) )
#Tura; Nizhnyaya Tunguska River to Tura
dat.v2 <- dat.v2 %>% 
  mutate(site_name= ifelse(site_reference == "RU-Tur", "Tura", site_name) )
#Delta junction sites 
dat.v2 <- dat.v2 %>% 
  mutate(site_reference= ifelse(site_id == "Welp_US-Bn1_tower1", "US-Bn1", site_reference) ) %>%
  mutate(site_name = ifelse(site_id == "Welp_US-Bn1_tower1", "Delta Junction deciduous broadleaf forest", site_name) ) %>%
  mutate(site_name= ifelse(site_id== "Welp_US-Bn2_tower2", "Delta Junction  evergreen conifer forest", site_name))
#Clarifying names for RU-Sam open and closed path
dat.v2 <- dat.v2 %>%
  mutate(site_name= ifelse(site_reference== "RU-Sam", "Samoylov Island", site_name ))%>%
  mutate(site_reference= ifelse(site_id== "Kutzbach_Samoylov_Tower_3_closedpath", "RU-Sam (closed)", site_reference ))%>%
  mutate(site_reference= ifelse(site_id== "Kutzbach_Samoylov_Tower_3_openpath", "RU-Sam (open)", site_reference)) 
#Seida
dat.v2 <- dat.v2 %>% 
  mutate(site_reference= ifelse(site_name == "Seida", "RU-Vrk", site_reference) )
#UNifying cherski naming
dat.v2 <- dat.v2 %>% 
  mutate(site_name= ifelse(site_name %in% c("Chersky Tower 1", "Cherskiy", "Cherski"), "Cherskii", site_name )) %>%
  mutate(site_name= ifelse(site_name %in% c("Chersky Tower 2",  "Cherski reference"), "Cherskii reference", site_name ))

#
dat.v2 <- dat.v2 %>% 
  mutate(site_reference= ifelse(site_id == "Dolman_RU-Ypn_tower1", "RU-SkP", site_reference) ) %>%
  mutate(site_name= ifelse(site_id == "Dolman_RU-Ypn_tower1", "Yakutsk Spasskaya Pad larch", site_name) )
#Ca-sObs to CA-Obs
dat.v2 <- dat.v2 %>% 
  mutate(site_reference= ifelse(site_reference == "CA-sOBS", "CA-Obs", site_reference) ) %>%
  mutate(site_name= ifelse(site_name == "Southern Old Black Spruce", "Saskatchewan - Western Boreal, Mature Black Spruce", site_name) )
#Lake Hazen
dat.v2 <- dat.v2 %>% 
  mutate(site_reference= ifelse(site_reference== "CA-LHazen1", "CA-LHazen1-semidesert", site_reference)) %>%
  mutate(site_reference= ifelse(site_reference== "CA-LHazen2", "CA-LHazen2-meadow wetland", site_reference))


#fixing extraction_source naming conventions
dat.v2$extraction_source <- sub('paper','Publication',dat.v2$extraction_source)
dat.v2$extraction_source <- sub('PI','User-contributed',dat.v2$extraction_source)


#fixing flux_method to match v2
dat.v2 <- dat.v2 %>%
  mutate(flux_method= ifelse(flux_method== "Ch", "Chamber", flux_method))

#removing Maija Marushchak Seida measurements since she provided us with updated values
dat.v2 <- dat.v2 %>% 
  dplyr::filter(!data_contributor_or_author%in% "Maija E. Marushchak")

#removing Eugenies second tower according to emails between Sue and Anna 2.12.24
#now not removing per discussion with Anna 10/31/24
#dat.v2 <- dat.v2 %>% 
#  dplyr::filter(!site_reference%in% "RU-Eusk_cher2")

#fixing name of Euskirchen_US-TFBS_tower1 to US-BZS
dat.v2 <- dat.v2 %>% 
  mutate(site_name= ifelse(site_id == "Euskirchen_US-TFBS_tower1", "Bonanza Creek Black Spruce", site_name) ) %>%
  mutate(site_reference= ifelse(site_id == "Euskirchen_US-TFBS_tower1", "US-BZS", site_reference) )
#changing Chokurdakh to Kytalyk, Russia
dat.v2 <- dat.v2 %>% 
  mutate(site_name = ifelse(site_name %in% "Chokurdakh", "Kytalyk, Russia", site_name))


#reformatting 
dat.v2$extraction_source<- paste("ABCflux v1-", dat.v2$extraction_source)
dat.v2 <- dat.v2 %>% 
  dplyr::rename("citation_co2"= "citation",
                "extraction_source_co2"= "extraction_source",
                "chamber_nr_measurement_days_co2"= "chamber_nr_measurement_days")

### US-Bes should NOT have fire according to Kyle 4/11/24
dat.v2 <- dat.v2 %>% 
  mutate(disturbance= ifelse(site_reference== "US-Bes", "Drained Lake", disturbance ),
         disturb_year= ifelse(site_reference== "US-Bes", "50-300 years ago", disturb_year ),
         disturb_severity= ifelse(site_reference== "US-Bes", NA, disturb_severity ))

##EDITS from Anna 4/12/24 #######
# MO-UFRS should NOT have fire
dat.v2 <- dat.v2 %>% 
  mutate(disturbance= ifelse(site_reference== "MO-UFRS", NA, disturbance ),
         disturb_year= ifelse(site_reference== "MO-UFRS", NA, disturb_year ),
         disturb_severity= ifelse(site_reference== "MO-UFRS", NA, disturb_severity ))
#Lund_Kobbefjord_Ch fluxes were wrong
dat.v2 <- dat.v2 %>% 
  dplyr::filter(!(site_id %in% "Lund_Kobbefjord_Ch01"))
#Humphreys_CA-CB_tower1: GPP fluxes should probably be NEE instead
dat.v2 <- dat.v2 %>% 
  mutate(nee= ifelse(site_id %in% "Humphreys_CA-CB_tower1", gpp, nee),
         gpp= ifelse(site_id %in% "Humphreys_CA-CB_tower1", NA, gpp))
#Goulden_CA-NS2 and "Goulden_CA-NS3" remove Jan 2002 data 
dat.v2 <- dat.v2 %>% 
  dplyr::filter(!(site_id %in% c("Goulden_CA-NS2_tower2", "Goulden_CA-NS3_tower3") & year %in% 2002 & month %in% 1))
## McCaughey_CA-Man high winter uptake
dat.v2 <- dat.v2 %>% 
  dplyr::filter(!(site_id %in% "McCaughey_CA-Man_tower1" & year %in% 1997 & month %in% c(1, 2)))



#### changing soil moisture class from Kenzie ####
setwd("/Users/iwargowsky/Desktop/ABCFlux v2") 
# soilmoisture <- read_csv("abcfluxV1_new_moisture_classes.csv")
# soilmoisture$site_unique <- paste(soilmoisture$site_name, soilmoisture$site_reference, sep= "_")
dat.v2$site_unique <- paste(dat.v2$site_name, dat.v2$site_reference, sep = "_")
unique(dat.v2$site_unique)

dat.v2 <- dat.v2 %>%
  mutate(soil_moisture_class= ifelse(site_unique %in% c("Mackenzie Valley, Anzac, Mid-Boreal - Peat Plateau_mid boreal - peat plateau",
                                                        "Mackenzie Valley, Fort Simpson, High Boreal - Peat Plateau_boreal forest - peat plateau",   
                                                        "Mackenzie Valley, Inuvik, High Sub-Arctic - Peat Plateau_high subarctic - peat plateau",  
                                                        "Mackenzie Valley, Inuvik, High Sub-Arctic - Upland_high subarctic - upland", 
                                                        "Mackenzie Valley, Normal Wells, Low Sub-Arctic - Peat Plateau_low subarctic - peat plateau",
                                                        "HJP02 Jack Pine_CA-HJP02",
                                                        "HJP75 Jack Pine_CA-HJP75",
                                                        "HJP94 Jack Pine_CA-HJP94",                                                               
                                                        "Varrio_FI-Var",                                                                 
                                                        "Smith Creek_CA-SMC",                                                                     
                                                        "Trail Valley Creek_CA-TVC",                                                            
                                                        "Igarka_RU-IG",                                                             
                                                        "Zotino; Central Siberia_RU-Zfw"), "Dry", soil_moisture_class)) %>%
  mutate(soil_moisture_class= ifelse(site_unique %in% c("Utqia?vik South_South",                                                                     
                                                        "Utqia?vik Central_Central"), "Wet", soil_moisture_class)) %>%
  mutate(soil_moisture_class= ifelse(site_unique %in% c("Council, Alaska_US-KOC", "Ahvensalo_Hollows Site",                                                                    
                                                        "Ahvensalo_Sphagnum angustifolium Site", "Ahvensalo_Sphagnum fuscum Site",                                                            
                                                        "Ahvensalo_Hummocks Site", "Salmisuo_Hummocks",                                                                         
                                                        "Stordalen Mire_Palsa Site","Stordalen Mire_Sphagnum Site",                                                              
                                                        "Adventdalen_P5","Eight Mile Lake_moist acidic tundra",                                                       
                                                        "Middle Taiga Zone_large hollow","Middle Taiga Zone_small ridge",                                                             
                                                        "Stordalen Mire_Site B (ombrotrophic)","Petsikko_Mire Hummocks (HM), HM1",                                                         
                                                        "Petsikko_Mire Hummocks (HM), HM2", "Petsikko_Mire Hummocks (HM), HM3",                                                         
                                                        "Eight Mile Lake_minimal thaw" ,"Eight Mile Lake_moderate thaw",                                                             
                                                        "Eight Mile Lake_extensive thaw","Salmisuo_FI-Salm" ,                                                                         
                                                        "Southern Old Black Spruce_CA-sOBS", "Lac Le Caron (hereafter referred to as LLC) peatland, an ombrotrophic bog\xa0_CA-LLC" ,     
                                                        "Alberta - Western Peatland - LaBiche River,Black Spruce/Larch Fen_CA-WP1",                  
                                                        "Siikaneva_FI-Sii","Scotty Creek Bog_CA-SCB" ,                                                                  
                                                        "Andoya_NO-And" ), "Moist", soil_moisture_class))
dat.v2$site_unique <- NULL

#remove rows without flux data
dat.v2 <- dat.v2 %>%
  dplyr::filter(!if_all(c(nee, gpp, reco, nee_seasonal), ~ is.na(.)))
#find number of  duplicates
dupes<- dat.v2 %>% get_dupes(site_name, site_reference, year, month, partition_method) 

dat.v2 <- dat.v2 %>%
  mutate(site_reference = ifelse(site_id %in% "Arneth_RU-Zfw_tower1", "RU-Zfw 1", site_reference)) %>%
  mutate(site_reference = ifelse(site_id %in% "Arneth_RU-Zfw_tower2", "RU-Zfw 2", site_reference)) %>%
  mutate(site_reference = ifelse(site_id %in% "Chae_US-KOC_Ch1", "US-KOC_Ch1", site_reference)) %>%
  mutate(site_reference = ifelse(site_id %in% "Chae_US-KOC_Ch2", "US-KOC_Ch2", site_reference)) %>%
  mutate(site_reference = ifelse(site_id %in% "Chae_US-KOC_Ch3", "US-KOC_Ch3", site_reference)) %>%
  mutate(site_reference = ifelse(site_id %in% "Chae_US-KOC_Ch4", "US-KOC_Ch4", site_reference)) %>%
  mutate(site_reference = ifelse(site_id %in% "Strebel_Adventdalen_Ch01", "Ch01", site_reference)) %>%
  mutate(site_reference = ifelse(site_id %in% "Strebel_Adventdalen_Ch02", "Ch02", site_reference)) %>%
  mutate(site_name = ifelse(site_id %in% c("Strebel_Adventdalen_Ch01", "Strebel_Adventdalen_Ch02"), "Adventdalen", site_name)) %>%
  mutate(site_reference = ifelse(site_id %in% "Morgner_Adventdalen_Ch01", "Ch01", site_reference)) %>%
  mutate(site_reference = ifelse(site_id %in% "Morgner_Adventdalen_Ch02", "Ch02", site_reference)) %>%
  mutate(site_reference = ifelse(site_id %in% "Kim_NorthSlope1_Ch03", "Subalpine tundra Ch03", site_reference)) %>%
  mutate(site_reference = ifelse(site_id %in% "Kim_NorthSlope3_Ch06", "Subalpine tundra Ch06", site_reference)) %>%
  mutate(site_reference = ifelse(site_id %in% "Kim_NorthSlope2_Ch04", "Upland tundra Ch03", site_reference)) %>%
  mutate(site_reference = ifelse(site_id %in% "Kim_NorthSlope4_Ch07", "Upland tundra Ch07", site_reference)) %>%
  mutate(site_reference = ifelse(site_id %in% "Kim_NorthSlope5_Ch08", "Coastal tundra Ch08", site_reference)) %>%
  mutate(site_reference = ifelse(site_id %in% "Kim_SouthBrooksRange1_Ch02", "Tundra-boreal ecotone Ch02", site_reference)) %>%
  mutate(site_reference = ifelse(site_id %in% "Kim_SouthBrooksRange2_Ch05", "Tundra-boreal ecotone Ch05", site_reference)) %>%
  mutate(site_name = ifelse(site_id %in% "Falk_Zackenberg_Ch01", "Zackenberg", site_name)) %>%
  mutate(site_name = ifelse(site_id %in% "Larsen_Abisko1_Ch01", "Abisko", site_name)) %>%
  mutate(site_name = ifelse(site_id %in% "Maanavilja_Kaamanen_Ch01", "Kaamanen", site_name)) %>%
  mutate(site_name = ifelse(site_id %in% "Uchida_Svalbard_Ch01", "Svalbard", site_name))

#aggregate dupes
dat.v2.ch <- dat.v2 %>%
  dplyr::filter(flux_method %in% "Chamber") %>%
  group_by(site_name, site_reference, year, month, longitude, latitude) %>%
  dplyr::summarise(across(where(is.numeric), list(mean = mean)),
                   across(where(is.character), list(unique = ~toString(unique(.[!is.na(.)]))))) %>%
  rename_with(~gsub("_unique", "", .), everything()) %>%  # Remove "_unique" from column names
  rename_with(~gsub("_mean", "", .), everything()) # Remove "_mean" from column names

#remove dupes keeping the rows with most data
dat.v2.ec <- dat.v2 %>%
  dplyr::filter(!flux_method %in% "Chamber")

dat.v2.ec$na_count <- rowSums(is.na(dat.v2.ec))

dat.v2.ec <- dat.v2.ec %>%
  arrange(na_count) %>%
  distinct(site_name, site_reference, partition_method, year, month, .keep_all = TRUE) %>% #Yakutsk Spasskaya Pad larch and US-bes are the only sites with dupes
  mutate(na_count = NULL)
  
dat.v2 <- rbind(dat.v2.ch, dat.v2.ec)  
#double check
dupes<- dat.v2 %>% get_dupes(site_name, site_reference, year, month, partition_method) 

  
setwd("/Users/iwargowsky/Desktop/ABCFlux v2") 
write_csv(dat.v2, "ABCfluxv1.v2format.csv")
