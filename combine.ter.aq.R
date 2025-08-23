###Combining terrestrial and aquatic
library(tidyr)
library(dplyr)
library(readr)
library(readxl)
library(janitor)
library(data.table)
library(zoo)
library(stringr)

#Loading datasets
setwd("/Users/iwargowsky/Desktop/arcticborealCflux")
ter <- read.csv("ABC.v2.jun25.cleanish.nodupes.csv")
ter$dataset <- "Terrestrial"


setwd("/Users/iwargowsky/Desktop/ABCFlux v2") # from Judith Vogt
aq <- read.csv("abcfluxv2_freshwater_data.csv")
aq$dataset <- "Aquatic"


unique(aq$ch4_flux_ebullition_seasonal)
#Cleaning before merging and matching column names
ter <- ter %>% dplyr::rename("bawld_class"= "land_cover_bawld",
                             "ch4_flux_total_seasonal" = "ch4_flux_seasonal",#make ch4_flux_seasonal  -> ch4_flux_total_seasonal to match aquatic
                             "gap_fill_perc_co2"= "gap_fill_perc_nee") %>%
  mutate(bawld_class = case_when(
         bawld_class %in% "Dry tundra" ~ "Dry Tundra",
         bawld_class %in% c("Permafrost bog", "Peramfrost Bog", "Permarost Bog") ~ "Permafrost Bog",
         bawld_class %in% c("Tundra wetland", "Tundra Wetland", "Peramfrost Wetland", "Permafrost Wetland") ~ "Wet Tundra",
         bawld_class %in% "Boreal forest" ~ "Boreal Forest",
         bawld_class %in% "Moist tundra" ~ "Moist Tundra",
    TRUE ~ bawld_class )) %>%
  select(-(waterbody_type_bawld))

ter$water_iceon <- NULL # have to remove this empty column because class is different


aq <- aq %>% dplyr::rename("bawld_class"= "waterbody_type_bawld",
                           "Disturbance_Category"= "disturbance_new" ) %>%
  #make Stordalen SE-St1 names match
  mutate(site_name= ifelse(site_name %in% "Stordalen_Villasjoen", "Stordalen Fen", site_name),
         site_reference= ifelse(site_reference %in% "Stordalen_Villasjoen_Lake Villasjoen_SE-St1_tower",
                                "Stordalen Fen_Lake Villasjoen_SE-St1_tower" , site_reference)) %>%
  mutate(site_reference= ifelse(site_reference %in% "Iskoras_NO-Isk_pond_tower", "Iskoras_NO-Isk-pond_tower", site_reference)) #match terrestrial formatting

aq$land_cover <- NULL #blank columns for merging

aq <- aq %>% dplyr::filter(!is.na(latitude))

#Combine aquatic and terrestrial
abc.full<- rbindlist(list(ter, aq), fill = T) %>%
  dplyr::rename("disturbance_category"= "Disturbance_Category" ) #all other columms are lower case 

#making ecosystem column
abc.full<- abc.full%>%
  mutate(ecosystem = case_when(bawld_class %in% "Boreal Forest"~ "Boreal Forest",
                             bawld_class %in% c("Dry Tundra", "Moist Tundra", "Rocklands", "Rockland")~ "Tundra",
                             bawld_class %in% c("Bog", "Permafrost Bog", "Fen", "Wet Tundra", "Marsh")~ "Wetland",
                             waterbody_type %in% "Lotic"~ "Lotic",
                             waterbody_type %in% "Lentic"~ "Lentic"))


dupes <- abc.full%>% get_dupes(site_reference, year, month)
# Dupes at Ket River are okay for now


#Yay more data cleaning ---------------------------------------------------------

#latitude/longitude##
summary(abc.full$latitude)
summary(abc.full$longitude)

#email
unique(abc.full$email)
abc.full<- abc.full%>% mutate(email = ifelse(email %in% "nan", NA, email))

#year/month##
unique(abc.full$year)
summary(abc.full$year) # should be numeric

unique(abc.full$month)
summary(abc.full$month)

#canopy height
unique(abc.full$canopy_height)

#forest age
summary(abc.full$forest_age)

#partition_method
unique(abc.full$partition_method)

#tair height
summary(abc.full$tair_height)

#tair
summary(abc.full$tair)

#precip
summary(precip)

#water_table_depth
summary(abc.full$water_table_depth)

#tsoil depths
summary(abc.full$tsoil_surface_depth)
summary(abc.full$tsoil_deep_depth)
summary(abc.full$moisture_depth)

#biome
unique(abc.full$biome)

#making all Stordalen Tundra
abc.full<- abc.full%>%
  mutate(biome = ifelse(site_name %in% c("Abisko Stordalen birch forest",
                                         "Stordalen",
                                         "Stordalen Fen",
                                         "Stordalen Palsa Bog",
                                         "Stordalen_Harrsjoen"), "Tundra", biome))

#BAWLD classes
unique(abc.full$bawld_class)

#flux_method_detail
unique(abc.full$flux_method)



#flux_method_detail
unique(abc.full$flux_method_detail)
#unifying
abc.full<- abc.full%>% mutate(flux_method_detail = ifelse(flux_method_detail %in%  "CO2: Enclosed-path eddy covariance, CH4: Open-path eddy covariance" , 
                                                 "CO2: Closed-path eddy covariance, CH4: Open-path eddy covariance" , flux_method_detail)) %>%
  mutate(flux_method_detail = ifelse(flux_method_detail %in%  "Manual chamber measurements"  , 
                                     "Manual chamber" , flux_method_detail)) 

#chamber_nr_measurement_days
unique(abc.full$chamber_nr_measurement_days_ch4)
unique(abc.full$chamber_nr_measurement_days_co2)
abc.full<- abc.full%>%
  mutate(chamber_nr_measurement_days_ch4 = ifelse(
      chamber_nr_measurement_days_ch4 == "Continuous",
      as.numeric(format(as.Date(paste0(year, "-", month, "-01")), "%d")), # Calculate days in month
      as.integer(chamber_nr_measurement_days_ch4)) ) %>%
  mutate(chamber_nr_measurement_days_co2 = ifelse(
    chamber_nr_measurement_days_co2 == "Continuous",
    as.numeric(format(as.Date(paste0(year, "-", month, "-01")), "%d")), # Calculate days in month
    as.integer(chamber_nr_measurement_days_co2)) )

#site_activity
unique(abc.full$site_activity)
abc.full<- abc.full%>%
  mutate(site_activity= ifelse(site_activity %in% "No", "Non-active", site_activity))



unique(abc.full$gap_fill)
  
#disturbance
unique(abc.full$disturbance_category)
abc.full<- abc.full%>%
  mutate(disturbance_category = ifelse(disturbance_category %in% "No", "None", disturbance_category)) %>%
  mutate(disturbance_category = ifelse(disturbance_category %in% "Artificial drainage", "Drainage", disturbance_category)) %>%
  mutate(disturbance_category = ifelse(disturbance_category %in% "Unknown" & is.na(disturbance), NA, disturbance_category)) 

unique(abc.full$disturbance)
abc.full<- abc.full%>%
  mutate(disturbance = ifelse(disturbance %in% c("N", "No"), "None", disturbance))

#disturbance year
unique(abc.full$disturb_year)
abc.full<- abc.full%>%
  mutate(disturb_year= ifelse(disturb_year %in% "Ongoing",0, disturb_year)) %>%
  mutate(disturb_year= ifelse(disturb_year %in% c("thaw ~500 years ago",
                                                  "50-300 years ago"),NA, disturb_year)) %>%
  mutate(disturb_year= as.numeric(disturb_year))
  
#permafrost
unique(abc.full$permafrost)
abc.full<- abc.full%>% 
  mutate(permafrost= ifelse(permafrost %in% "U", "Unknown", permafrost))

#permafrost_thaw
unique(abc.full$permafrost_thaw)
abc.full<- abc.full%>% 
  mutate(permafrost_thaw= ifelse(permafrost_thaw %in% "U", "Unknown", permafrost_thaw))

#data_usage
unique(abc.full$data_usage)
abc.full<- abc.full%>% 
  mutate(data_usage = ifelse(data_usage %in% "Tier 1", "Tier1", data_usage)) %>%
  mutate(data_usage = ifelse(data_usage %in% "Tier 2", "Tier2", data_usage))

#benthic_veg
unique(abc.full$benthic_veg)
abc.full<- abc.full%>% 
  mutate(benthic_veg = ifelse(benthic_veg %in% "NO", "No", benthic_veg))


#"Unknown"" to "NA"
abc.full <- abc.full%>% 
  mutate(across(c(permafrost, permafrost_thaw, water_mixing_regime, fetch_screening, 
                  disturb_severity, site_activity, benthic_veg, emergent_veg, 
                  aquatic_site_sampling_location), ~na_if(., "Unknown")))

#partition_method method now that gpp sign has switched
unique(abc.full$partition_method)
abc.full <- abc.full%>% 
  mutate(partition_method= ifelse(partition_method %in% c("GPP=Reco-NEE (primarily at chamber sites)",
                                                             "GPP=Reco-NEE",
                                                             "GPP= NEE- ER",
                                                             "GPP= NEE- RECO",
                                                             "GPP= NEE-RECO"),"GPP=Reco-NEE", partition_method))


#remove columns we no longer want to include
colnames(abc.full) 
abc.full$site_id <- NULL
abc.full$site_representativeness <- NULL
abc.full$npp <- NULL
abc.full$veg_c_stock <- NULL
abc.full$c_leaf <- NULL
abc.full$c_root <- NULL
abc.full$n_leaf <- NULL
abc.full$n_root <- NULL
abc.full$c_deep <- NULL
abc.full$c_mineral <- NULL
abc.full$soil_co2_age <- NULL
abc.full$sediment_fe <- NULL
abc.full$sediment_hno3 <- NULL
abc.full$sediment_so4 <- NULL
abc.full$pattern <- NULL

#columns that are completely NA
cols_to_delete <- which(apply(abc.full, 2, function(col) all(is.na(col))))

#Remove the columns 
abc.full<- abc.full%>% select(-all_of(cols_to_delete))
# which were deleted
#cols_to_delete


# Replace special characters in all character columns
library(stringi)

replace_special_chars <- function(df) {
  df[] <- lapply(df, function(col) {
    if (is.character(col)) {
      # Replace accented characters with closest ASCII equivalents
      stri_trans_general(col, "Latin-ASCII")
    } else {
      col
    }
  })
  return(df)
}

abc.full<- replace_special_chars(abc.full)

#remove crazy aquatic fluxes
abc.full<- abc.full%>%
  dplyr::filter(!ch4_flux_total >200 | is.na(ch4_flux_total))
  

#-----------EDITS FROM COAUTHORS--------------------------------------------------------
#edits from Mats P. Bjorkman
abc.full<- abc.full%>%
  mutate(site_name= ifelse(site_name %in% c("dry heath", "dry meadow", "mesic meadow","tussock tundra", "wet meadow"), "Latnjajaure", site_name)) %>%
  mutate(site_reference = gsub("dry heath_HD Ctr", "Latnjajaure_dry heath Ctr", site_reference)) %>%
  mutate(site_reference = gsub("dry meadow_MD Ctr", "Latnjajaure_dry meadow Ctr", site_reference)) %>%
  mutate(site_reference = gsub("mesic meadow_MM Ctr", "Latnjajaure_mesic meadow Ctr", site_reference)) %>%
  mutate(site_reference = gsub("tussock tundra_TT Ctr", "Latnjajaure_tussock tundra Ctr", site_reference)) %>%
  mutate(site_reference = gsub("wet meadow_MW Ctr", "Latnjajaure_wet meadow Ctr", site_reference)) 

#adding in coauthors
abc.full<- abc.full%>%
  mutate(data_contributor_or_author = ifelse(data_contributor_or_author %in% c("Donatella Zona, Walter Oechel", "Donatella Zona"), 
                                             "Donatella Zona, Walter Oechel, Kyle Lunneberg", data_contributor_or_author)) %>%
  mutate(email= ifelse(email %in% c("dzona@mail.sdsu.edu, woechel@mail.sdsu.edu"),
                       "dzona@mail.sdsu.edu, woechel@mail.sdsu.edu, klunneberg@sdsu.edu", email)) %>%
  mutate(data_contributor_or_author = ifelse(data_contributor_or_author %in% c("Christopher Schulze, Oliver Sonnentag, Craig Emmerton, Lorna Harris, Haley Alcock, Kate Marouelli, Gabriel Hould Gosselin, and David Olefeldt", "Oliver Sonnentag"), 
                                             "Christopher Schulze, Oliver Sonnentag, Craig Emmerton, Bo Qu, Lorna Harris, Haley Alcock, Kate Marouelli, Gabriel Hould Gosselin, David Olefeldt", data_contributor_or_author)) %>%
  mutate(data_contributor_or_author = ifelse(data_contributor_or_author %in% c("Sean Carey"), 
                                             "Sean Carey, Erin Nicholls, Graham Clark", data_contributor_or_author)) %>%
  mutate(data_contributor_or_author = ifelse(data_contributor_or_author %in% c("Alejandro Salazar"), 
                                             "Alejandro Salazar, Xenia Uffrecht, Ben Amoah", data_contributor_or_author)) %>% 
  mutate(email = ifelse(site_name %in% "Saskatchewan - Western Boreal, Mature Jack Pine", "andrew.black@ubc.ca",email))


#Matthias Peichl edits
abc.full<- abc.full%>%
  mutate(data_contributor_or_author = ifelse(site_name %in% "Degero" & flux_method_detail %in% "Automatic chamber", 
                                             "Matthias Peichl, Järvi Järveoja", data_contributor_or_author)) %>%
  mutate(email = ifelse(site_name %in% "Degero" & flux_method_detail %in% "Automatic chamber", 
                        "matthias.peichl@slu.se; jarvi.jarveoja@slu.se", email)) 

#Oliver Sonnentag edits
abc.full<- abc.full%>%
  mutate(data_contributor_or_author = ifelse(site_reference %in% c("Scotty Creek Landscape_CA-SCC_tower", "Scotty Creek Bog_CA-SCB_tower"),
                                             "Oliver Sonnentag, Gabriel Hould Gosselin, Haley Alcock, Matteo Detto",data_contributor_or_author)) %>%
  mutate(email = ifelse(site_reference %in% c("Scotty Creek Landscape_CA-SCC_tower", "Scotty Creek Bog_CA-SCB_tower"),
                        "oliver.sonnentag@umontreal.ca, gabriel.hould-gosselin@umontreal.ca, haley.alcock@umontreal.ca, mdetto@princeton.edu", email)) %>%
  mutate(data_contributor_or_author = ifelse(site_name %in% "Smith Creek_CA-SMC_tower",
                                             "Oliver Sonnentag, Gabriel Hould Gosselin, Muhammad Umair",data_contributor_or_author)) %>%
  mutate(email = ifelse(site_reference %in% "Smith Creek_CA-SMC_tower",
                        "oliver.sonnentag@umontreal.ca, gabriel.hould-gosselin@umontreal.ca, muhammad.umair.1@umontreal.ca", email))  %>%
  mutate(data_contributor_or_author = ifelse(site_reference %in% c("Havikpak Creek_CA-HPC_tower", "Trail Valley Creek_CA-TVC_tower", "Trail Valley Creek_Big Bear Lake_tower"),
                                             "Oliver Sonnentag, Gabriel Hould Gosselin, Philip Marsh",data_contributor_or_author)) %>%
  mutate(email = ifelse(site_reference %in% c("Havikpak Creek_CA-HPC_tower", "Trail Valley Creek_CA-TVC_tower", "Trail Valley Creek_Big Bear Lake_tower"),
                        "oliver.sonnentag@umontreal.ca, gabriel.hould-gosselin@umontreal.ca, pmarsh@wlu.ca", email))


#Anatoly Prokushkin edits
abc.full<- abc.full%>%
  mutate(data_contributor_or_author = ifelse(data_contributor_or_author %in% "Zyryanov V.I., Prokushkin A.S.",
                                             "Anatoly Prokushkin, Alexey V. Panov",data_contributor_or_author)) %>%
  mutate(email = ifelse(site_name %in% "zyryanov-vi@ya.ru, prokushkin@ksc.krasn.ru",
                        "prokushkin@ksc.krasn.ru, alexey.v.panov@gmail.com", email))

#Jeffery Welker
abc.full<- abc.full%>%
  mutate(site_name = ifelse(site_name %in% "Tutakoke Field Site", "Toolik Moist Tussock Tundra", site_name)) %>%
  mutate(site_reference= ifelse(site_reference %in% "Tutakoke Field Site_Carex meadows_Chamber", "Toolik Moist Tussock Tundra_Carex meadows_Chamber", site_reference)) %>%
  mutate(site_reference= ifelse(site_reference %in% "Tutakoke Field Site_grazing lawn_Chamber", "Toolik Moist Tussock Tundra_grazing lawn_Chamber", site_reference)) %>%
  mutate(site_reference= ifelse(site_reference %in% "Tutakoke Field Site_pond margin_Chamber", "Toolik Moist Tussock Tundra_pond margin_Chamber", site_reference)) %>%
  mutate(site_reference= ifelse(site_reference %in% "Tutakoke Field Site_slough levee_Chamber", "Toolik Moist Tussock Tundra_slough levee_Chamber", site_reference)) %>%
  mutate(c_stock= ifelse(site_name %in% "Toolik Moist Tussock Tundra", 1.3, c_stock ))



#Hailey Webb remove methane 
abc.full<- abc.full%>%
  mutate(ch4_flux_total= ifelse(site_name %in% "APEX Beta" & year %in% c(2020,2021,2022,2023), NA, ch4_flux_total))

#Erik Sahlée (from google sheet)
abc.full<- abc.full%>%
  mutate(data_contributor_or_author= ifelse(site_reference %in% c("Taemnaren_tower", "Skogaryd_Lake Erssjoen_tower"), 
                                            "Erik Sahlee, Anna Rutgersson", data_contributor_or_author)) %>%
  mutate(email= ifelse(site_reference %in% c("Taemnaren_tower", "Skogaryd_Lake Erssjoen_tower"), 
                       "erik.sahlee@met.uu.se, anna.rutgersson@met.uu.se", email)) %>%
  mutate(data_contributor_or_author= ifelse(site_reference %in% "Skogaryd_Erssjoen_chamber" , 
                                            "David Bastviken, Erik Sahlee", data_contributor_or_author)) %>%
  mutate(email= ifelse(site_reference %in% "Skogaryd_Erssjoen_chamber", 
                       "david.bastviken@liu.se, erik.sahlee@met.uu.se", email)) 


# Efren Lopez-Blanco 
abc.full<- abc.full%>%
  mutate(data_contributor_or_author= ifelse(data_contributor_or_author %in% "Rasmus Jensen; Efr�n L�pez-Blanco; Torben R. Christensen", 
                                            "Rasmus Jensen; Efren Lopez-Blanco; Torben R. Christensen", data_contributor_or_author)) 



#emily pederson 
abc.full<- abc.full%>%
  mutate(data_contributor_or_author= ifelse(data_contributor_or_author %in% "Emily Pederson, Bo Erberling, Anders Michelsen", 
                                            "Emily Pickering Pedersen, Bo Erberling, Anders Michelsen", data_contributor_or_author)) 



#Alex Sabrekov
abc.full<- abc.full%>%
  mutate(data_contributor_or_author = ifelse(data_contributor_or_author %in% "A. F. Sabrekov",
                                             "Aleksandr F. Sabrekov", data_contributor_or_author)) %>%
  
  mutate(site_name= ifelse(site_reference %in% "Khanty-Mansiysk and Yamalo-Nenets Autonomous Districts_Noya 18_concentration",
                           "Noyabrsk", site_name)) %>% 
  mutate(site_reference = ifelse(site_reference %in% "Khanty-Mansiysk and Yamalo-Nenets Autonomous Districts_Noya 18_concentration",
                                 "Noyabrsk_Noya 18_concentration", site_reference)) %>%
  
  mutate(site_name= ifelse(site_reference %in% "Khanty-Mansiysk and Yamalo-Nenets Autonomous Districts_Noya Gr._concentration",
                           "Noyabrsk", site_name)) %>% 
  mutate(site_reference = ifelse(site_reference %in% "Khanty-Mansiysk and Yamalo-Nenets Autonomous Districts_Noya Gr._concentration",
                                 "Noyabrsk_Noya Gr._concentration", site_reference)) %>%
  
  mutate(site_name= ifelse(site_reference %in% "Khanty-Mansiysk and Yamalo-Nenets Autonomous Districts_Noya Kr._concentration",
                           "Noyabrsk", site_name)) %>% 
  mutate(site_reference = ifelse(site_reference %in% "Khanty-Mansiysk and Yamalo-Nenets Autonomous Districts_Noya Kr._concentration",
                                 "Noyabrsk_Noya Kr._concentration", site_reference)) %>%
  
  mutate(site_name= ifelse(site_reference %in% "Khanty-Mansiysk and Yamalo-Nenets Autonomous Districts_Noya Ol. Mch._concentration",
                           "Noyabrsk", site_name)) %>% 
  mutate(site_reference = ifelse(site_reference %in% "Khanty-Mansiysk and Yamalo-Nenets Autonomous Districts_Noya Ol. Mch._concentration",
                                 "Noyabrsk_Noya Ol. Mch._concentration", site_reference)) %>%
  
  mutate(site_name= ifelse(site_reference %in% "Khanty-Mansiysk and Yamalo-Nenets Autonomous Districts_PM_concentration",
                           "Purpe", site_name)) %>% 
  mutate(site_reference = ifelse(site_reference %in% "Khanty-Mansiysk and Yamalo-Nenets Autonomous Districts_PM_concentration",
                                 "Purpe_PM_concentration", site_reference)) %>%
  
  mutate(site_name= ifelse(site_reference %in% "Khanty-Mansiysk and Yamalo-Nenets Autonomous Districts_Pur Ol. 1_concentration",
                           "Purpe", site_name)) %>% 
  mutate(site_reference = ifelse(site_reference %in% "Khanty-Mansiysk and Yamalo-Nenets Autonomous Districts_Pur Ol. 1_concentration",
                                 "Purpe_Pur Ol. 1_concentration", site_reference)) %>%
  
  mutate(site_name= ifelse(site_reference %in% "Khanty-Mansiysk and Yamalo-Nenets Autonomous Districts_Pur Oz. 1_concentration",
                           "Purpe", site_name)) %>% 
  mutate(site_reference = ifelse(site_reference %in% "Khanty-Mansiysk and Yamalo-Nenets Autonomous Districts_Pur Oz. 1_concentration",
                                 "Purpe_Pur Oz. 1_concentration", site_reference)) %>%
  
  mutate(site_name= ifelse(site_reference %in% "Khanty-Mansiysk and Yamalo-Nenets Autonomous Districts_Sir_concentration",
                           "Mukhrino", site_name)) %>% 
  mutate(site_reference = ifelse(site_reference %in% "Khanty-Mansiysk and Yamalo-Nenets Autonomous Districts_Sir_concentration",
                                 "Mukhrino_Sir_concentration", site_reference)) %>%
  
  mutate(site_name= ifelse(site_reference %in% "Khanty-Mansiysk and Yamalo-Nenets Autonomous Districts_T.Mu.GMK.Ry.2_concentration",
                           "Mukhrino", site_name)) %>% 
  mutate(site_reference = ifelse(site_reference %in% "Khanty-Mansiysk and Yamalo-Nenets Autonomous Districts_T.Mu.GMK.Ry.2_concentration",
                                 "Mukhrino__T.Mu.GMK.Ry.2_concentration", site_reference)) 



#Inge Althuizen
abc.full<- abc.full%>%
  mutate(data_contributor_or_author= ifelse(site_reference %in% c("Iskoras_bare soil palsa_Chamber",
                                                                  "Iskoras_thaw slump_Chamber",
                                                                  "Iskoras_vegetated palsa_Chamber",
                                                                  "Iskoras_vegetated thaw pond_Chamber"),
                                            "Hanna Lee, Inge H.J. Althuizen, Casper T. Christiansen", data_contributor_or_author)) %>%
  mutate(email= ifelse(site_reference %in% c("Iskoras_bare soil palsa_Chamber",
                                             "Iskoras_thaw slump_Chamber",
                                             "Iskoras_vegetated palsa_Chamber",
                                             "Iskoras_vegetated thaw pond_Chamber"),
                       "hanna.lee@ntnu.no, ialt@norceresearch.no, casper.christiansen@bio.ku.dk", email))

#ruth varner edits from Kenzie 
abc.full<- abc.full%>%
  mutate(data_contributor_or_author= ifelse(data_contributor_or_author %in% "Beth Holmes, Patrick Crill",
                                            "Beth Holmes, Patrick Crill, Ruth Varner", data_contributor_or_author)) %>%
  mutate(email= ifelse(site_reference %in% "bhuettel@fsu.edu, patrick.crill@geo.su.se",
                       "bhuettel@fsu.edu, patrick.crill@geo.su.se, ruth.k.varner@unh.edu", email))

#fixing things I noticed - Isabel
abc.full<- abc.full%>%
  mutate(site_reference= ifelse(site_reference %in% "Igarka_RU-IG_tower",
                                "Igarka_RU-Iga_tower", site_reference))  %>%
  mutate(site_reference= case_when(site_reference %in% "Igarka_Igarka_lake 1, point A. SIL1AWT_Chamber_chamber" ~ "Igarka_lake 1, point A. SIL1AWT_Chamber",
                                   site_reference %in% "Igarka_Igarka_lake 1, point B. SIL1BWT_Chamber_chamber" ~ "Igarka_lake 1, point B. SIL1BWT_Chamber",
                                   site_reference %in% "Igarka_Igarka_lake 2, point A. SIL2AWT_Chamber_chamber" ~ "Igarka_lake 2, point A. SIL2AWT_Chamber",
                                   site_reference %in% "Igarka_Igarka_lake 2, point B. SIL2BWT_Chamber_chamber" ~ "Igarka_lake 2, point B. SIL2BWT_Chamber",
                                   
                                   site_reference %in% "Igarka_Igarka_lake 3, point A. SIL3AWT_Chamber_chamber" ~ "Igarka_lake 3, point A. SIL3AWT_Chamber",
                                   site_reference %in% "Igarka_Igarka_lake 3, point B. SIL3BWT_Chamber_chamber" ~ "Igarka_lake 3, point B. SIL3BWT_Chamber",
                                   site_reference %in% "Igarka_Igarka_lake 4, point A. SIL4AWT_Chamber_chamber" ~ "Igarka_lake 4, point A. SIL4AWT_Chamber",
                                   site_reference %in% "Igarka_Igarka_lake 4, point B. SIL4BWT_Chamber_chamber" ~ "Igarka_lake 4, point B. SIL4BWT_Chamber",
                                   site_reference %in% "Igarka_Igarka_peatland 3, point A. SIP3AWT_Chamber_chamber" ~ "Igarka_peatland 3, point A. SIP3AWT_Chamber", 
                                   TRUE~ site_reference))


#Frans-Jan 
abc.full<- abc.full%>%
  mutate(site_name= ifelse(site_name %in% c ("Chokurdakh tundra and floodplain", "Kytalyk"),
                           "Kytalyk, Russia", site_name)) %>%
  mutate(site_reference= case_when(site_reference %in% "Chokurdakh tundra and floodplain_FD1_Chamber"~
                                   "Kytalyk, Russia_FD1_Chamber",
                                   site_reference %in% "Chokurdakh tundra and floodplain_FD2_Chamber"~
                                     "Kytalyk, Russia_FD2_Chamber",
                                   site_reference %in% "Chokurdakh tundra and floodplain_FW1_Chamber"~
                                     "Kytalyk, Russia_FW1_Chamber",
                                   site_reference %in% "Chokurdakh tundra and floodplain_FW2_Chamber"~
                                     "Kytalyk, Russia_FW2_Chamber",
                                   site_reference %in% "Chokurdakh tundra and floodplain_FW3_Chamber"~
                                     "Kytalyk, Russia_FW3_Chamber",
                                   site_reference %in% "Chokurdakh tundra and floodplain_FW4_Chamber"~
                                     "Kytalyk, Russia_FW4_Chamber",
                                   site_reference %in% "Chokurdakh tundra and floodplain_TD1_Chamber"~
                                     "Kytalyk, Russia_TD1_Chamber",
                                   site_reference %in% "Chokurdakh tundra and floodplain_TD2_Chamber"~
                                     "Kytalyk, Russia_TD2_Chamber",
                                   site_reference %in% "Chokurdakh tundra and floodplain_TD3_Chamber"~
                                     "Kytalyk, Russia_TD3_Chamber",
                                   site_reference %in% "Chokurdakh tundra and floodplain_TD4_Chamber"~
                                     "Kytalyk, Russia_TD4_Chamber",
                                   site_reference %in% "Chokurdakh tundra and floodplain_TW1_Chamber"~
                                     "Kytalyk, Russia_TW1_Chamber",
                                   site_reference %in% "Chokurdakh tundra and floodplain_TW2_Chamber"~
                                     "Kytalyk, Russia_TW2_Chamber",
                                   site_reference %in% "Chokurdakh tundra and floodplain_TW3_Chamber"~
                                     "Kytalyk, Russia_TW3_Chamber",
                                   site_reference %in% "Chokurdakh tundra and floodplain_TW4_Chamber"~
                                     "Kytalyk, Russia_TW4_Chamber",
                                   #aquatic
                                   site_reference %in% "Kytalyk_Lake_N1_chamber"~
                                     "Kytalyk, Russia_Lake_N1_chamber",
                                   site_reference %in% "Kytalyk_Lake_N2_chamber"~
                                     "Kytalyk, Russia_Lake_N2_chamber",
                                   site_reference %in% "Kytalyk_Lake_N3_chamber"~
                                     "Kytalyk, Russia_Lake_N3_chamber",
                                   site_reference %in% "Kytalyk_Lake_N4_chamber"~
                                     "Kytalyk, Russia_Lake_N4_chamber",
                                   site_reference %in% "Kytalyk_Lake_N5_chamber"~
                                     "Kytalyk, Russia_Lake_N5_chamber",
                                   site_reference %in% "Kytalyk_Lake_N6_chamber"~
                                     "Kytalyk, Russia_Lake_N6_chamber",
                                   site_reference %in% "Kytalyk_Lake_N7_chamber"~
                                     "Kytalyk, Russia_Lake_N7_chamber",
                                   site_reference %in% "Kytalyk_Lake_N8_chamber"~
                                     "Kytalyk, Russia_Lake_N8_chamber",
                                   site_reference %in% "Kytalyk_Lake_N9_chamber"~
                                     "Kytalyk, Russia_Lake_N9_chamber",
                                   site_reference %in% "Kytalyk_Lake_S1_chamber"~
                                     "Kytalyk, Russia_Lake_S1_chamber",
                                   site_reference %in% "Kytalyk_Lake_S2_chamber"~
                                     "Kytalyk, Russia_Lake_S2_chamber", TRUE ~ site_reference ))
#Marcus Klaus
abc.full<- abc.full%>%
  mutate(site_name= ifelse(site_name %in% "Lillsjolidtjarnen", "Lillsjolidtjarnen inlet", site_name)) %>%
  mutate(site_name= ifelse(site_name %in% "OvreBjorntjarn", "OvreBjorntjarn inlet", site_name)) %>%
  mutate(site_name= ifelse(site_name %in% "Struptjarn", "Struptjarn inlet", site_name))
  
  
#Marta Magnani
abc.full<- abc.full%>%
  mutate(email = ifelse(site_name %in% "Nyalesund, Spitzbergen", "marta.magnani@cnr.it; mariasilvia.giamberini@cnr.it", email)) %>%
  mutate(bawld_class = ifelse(site_name %in% "Nyalesund, Spitzbergen", "Dry Tundra", bawld_class)) 

#removing other special character issues
unique(abc.full$citation)
abc.full<- abc.full%>%
  mutate(email= ifelse(email %in% "Michal Ga�ovic (michal.gazovic@gmail.com)", "Michal Gazovic (michal.gazovic@gmail.com)", email)) %>%
  mutate(email= ifelse(email %in% "Torben R�jle Christensen (torben.christensen@au.dk)", "Torben Christensen (torben.christensen@au.dk)", email)) %>%
  mutate(citation= ifelse(citation %in% "Alm, J., Schulman, L., Walden, J., Nyk�nen, H., Martikainen, P. J., et al. (1999). Carbon balance of a boreal bog during a year with an exceptionally dry summer. Ecology, 80(1), 161�174. https://doi.org/10.1890/0012-9658(1999)080[0161:CBOABB]2.0.CO;2" ,
                          "Alm, J., Schulman, L., Walden, J., Nykanen, H., Martikainen, P. J., et al. (1999). Carbon balance of a boreal bog during a year with an exceptionally dry summer. Ecology, 80(1), 161 174. https://doi.org/10.1890/0012-9658(1999)080[0161:CBOABB]2.0.CO;2"   , citation)) %>%
  mutate(citation= ifelse(citation %in% "J�rveoja, J., Nilsson, M. B., Ga�ovic, M., Crill, P. M., & Peichl, M. (2018). Partitioning of the net CO2 exchange using an automated chamber system reveals plant phenology as key control of production and respiration fluxes in a boreal peatland. Global Change Biology, 24(8), 3436�3451. https://doi.org/10.1111/gcb.14292." ,
                          "Jarveoja, J., Nilsson, M. B., Ga�ovic, M., Crill, P. M., & Peichl, M. (2018). Partitioning of the net CO2 exchange using an automated chamber system reveals plant phenology as key control of production and respiration fluxes in a boreal peatland. Global Change Biology, 24(8), 3436�3451. https://doi.org/10.1111/gcb.14292."  , citation)) %>%
  mutate(citation= ifelse(citation %in% "B�ckstrand, K., Crill, P. M., Mastepanov, M., Christensen, T. R., & Bastviken, D. (2008). Total hydrocarbon flux dynamics at a subarctic mire in northern Sweden. Journal of Geophysical Research, 113(G3). https://doi.org/10.1029/2008JG000703" ,
                          "Backstrand, K., Crill, P. M., Mastepanov, M., Christensen, T. R., & Bastviken, D. (2008). Total hydrocarbon flux dynamics at a subarctic mire in northern Sweden. Journal of Geophysical Research, 113(G3). https://doi.org/10.1029/2008JG000703"  , citation)) %>%
  mutate(citation= ifelse(citation %in% "�Junninen, H., Lauri, A., Keronen,�P., Aalto, P., Hiltunen, V., et al.�(2009). Smart-SMEAR: on-line data exploration and visualization tool for SMEAR�stations. Boreal Environment Research 14, 447�457."    ,
                          "Junninen, H., Lauri, A., Keronen,�P., Aalto, P., Hiltunen, V., et al.(2009). Smart-SMEAR: on-line data exploration and visualization tool for SMEAR stations. Boreal Environment Research 14, 447�457."  , citation)) %>%
  mutate(citation= ifelse(citation %in% "Lund,�M.,�J. M. Falk,�T. Friborg,�H. N. Mbufong,�C. Sigsgaard,�H. Soegaard, and�M. P. Tamstorf�(2012),�Trends in CO2�exchange in a high Arctic tundra heath, 2000�2010,�J. Geophys. Res.,�117, G02001, doi:10.1029/2011JG001901." ,
                          "Lund, M., J. M. Falk, T. Friborg, H. N. Mbufong, C. Sigsgaard, H. Soegaard, and�M. P. Tamstorf (2012), Trends in CO2 exchange in a high Arctic tundra heath, 2000 2010, J. Geophys. Res.,�117, G02001, doi:10.1029/2011JG001901." , citation)) %>%
  mutate(citation = case_when(
    str_detect(citation, "Lohila, A., Minkkinen, K., Aurela, M., Tuovinen,") ~
      "Lohila, A., Minkkinen, K., Aurela, M., Tuovinen, J.-P., Penttila, T., et al. (2011). Greenhouse gas flux measurements in a forestry-drained peatland indicate a large carbon sink. Biogeosciences, 8(11), 3203-3218. https://doi.org/10.5194/bg-8-3203-2011. ; Minkkinen, K., Ojanen, P., Penttil�, T., Aurela, M., Laurila, T., et al. (2018). Persistent carbon sink at a boreal drained bog forest. Biogeosciences, 15(11), 3603-3624. https://doi.org/10.5194/bg-15-3603-2018",
    TRUE ~ citation
  ))



# Norunda
abc.full<- abc.full%>%
  mutate(data_contributor_or_author = ifelse(site_name %in% "Norunda", "Natascha Kljun, Meelis Molder", data_contributor_or_author),
         email= ifelse(site_name %in% "Norunda", "natascha.kljun@cec.lu.se; meelis.molder@nateko.lu.se", email),
         disturbance= ifelse(site_name %in% "Norunda", "During late summer/autumn 2022 an area 300 m around the main tower (30.5ha) was turned into a complete clear-cut. All (>14000) of the 100-130 years old stand of Scots pine (Pinus sylvestris L.) and Norway spruce (Picea abies L.) and a small fraction (7%) of deciduous trees as well as the understory was logged. Only the stems were removed (in total ~11000m3fpb, ~320m3fpb/ha); branches and tree tops as well smaller trees were left on-site. The shrub layer was up to now dominated by blueberry, cranberry, moss, and flowers. After a soil scarification in autumn 2023 the area will be replanted in spring 2024 with mainly pine (~90%) and spruce.",disturbance),
         disturbance_category= ifelse(site_name %in% "Norunda", "Forestry", disturbance_category ),
         disturb_year= ifelse(site_name %in% "Norunda", 2022, disturb_year),
         disturb_severity= ifelse(site_name %in% "Norunda", "High: Change to the ecosystem that is lasting with consequences for ecosystem identity and function", disturb_severity))
### Google form information---------------------------------------------------------
setwd("/Users/iwargowsky/Desktop/ABCflux v2")
googleform <- read_csv("check.googleform.csv") 

dupes <- googleform %>% get_dupes(site_reference)

colnames(googleform)

googleform.unite <- googleform %>% select(site_reference, c_stock, stock_depth, strahler_order)

abc.full <- abc.full %>% 
  full_join(googleform.unite, by="site_reference") %>%
  unite("c_stock", c(c_stock.x, c_stock.y), na.rm= TRUE, remove= TRUE)%>% 
  unite("stock_depth", c(stock_depth.x, stock_depth.y), na.rm= TRUE, remove= TRUE)%>%
  unite("strahler_order", c(strahler_order.x, strahler_order.y), na.rm= TRUE, remove= TRUE)%>%
  mutate(c_stock = if_else(
    str_detect(c_stock, "^([0-9.]+)_\\1$"),
    str_extract(c_stock, "^[0-9.]+"),
    c_stock )) %>%
  mutate(stock_depth = if_else(
    str_detect(stock_depth, "^([0-9.]+)_\\1$"),
    str_extract(stock_depth, "^[0-9.]+"),
    stock_depth )) %>%
  mutate(strahler_order = if_else(
    str_detect(strahler_order, "^([0-9.]+)_\\1$"),
    str_extract(strahler_order, "^[0-9.]+"),
    strahler_order )) 

unique(abc.full$c_stock)
unique(abc.full$stock_depth)
unique(abc.full$strahler_order)


googleform.merge <- googleform %>% select(site_reference, disturbance, disturbance_category,
                                    disturb_year, disturb_severity, permafrost, permafrost_thaw)


# Define the columns to be set to NA
cols_to_na <- c("disturbance", "disturbance_category", "disturb_year", "disturb_severity",
                "permafrost", "permafrost_thaw")

# Define the columns to be set to NA
cols_to_update <- c("disturbance", "disturbance_category", "disturb_year", "disturb_severity",
                "permafrost", "permafrost_thaw")

# Update abc.full
abc.full <- abc.full %>%
  mutate(across(all_of(cols_to_na),
                ~ if_else(site_reference %in% googleform.merge$site_reference, NA, .)))


# Perform the join
abc.full <- abc.full %>%
  mutate(disturb_year= as.character(disturb_year) ) %>%
  left_join(googleform.merge %>% select(site_reference, all_of(cols_to_update)),
            by = "site_reference", suffix = c("", ".form")) %>%
  mutate(across(all_of(cols_to_update),
                ~ coalesce(., get(paste0(cur_column(), ".form"))))) %>%
  select(-ends_with(".form"))  # Remove extra columns after merging


googleform.newcolumns <- googleform %>% select(site_reference, disturb_dominant, disturb_extent, water_depth_location,
                                               thaw_dominant, thaw_category, thaw_severity, thaw_extent) %>%
  mutate(water_depth_location= ifelse(water_depth_location %in% "Average", "Mean", water_depth_location))


abc.full <- abc.full %>% full_join(googleform.newcolumns, by= "site_reference")

## cleaning these columns

unique(abc.full$disturbance_category)
abc.full <- abc.full %>%
  mutate( disturbance_category= case_when(
  disturbance_category %in% c("Animal_herbivory") ~ "Animal herbivory",
  disturbance_category %in% c("Seismic lines", "Seismic Lines", "Seismic_lines", "Seismic_Lines") ~ "Seismic lines",
  disturbance_category %in% c("Drained lake", "Drained Lake", "Drained_Lake") ~ "Drained lake",
  disturbance_category %in% c("Human path", "Human paths") ~ "Human paths",
  disturbance_category %in% "Wastewater, Beaver" ~ "Wastewater, Beavers",
  disturbance_category %in% c("Beaver" ) ~ "Beavers",
  TRUE ~ disturbance_category))

unique(abc.full$disturb_dominant)
abc.full <- abc.full %>%
  mutate( disturb_dominant= case_when(
    disturb_dominant %in% c("Seismic Lines") ~ "Seismic lines",
    TRUE ~ disturb_dominant))

unique(abc.full$disturb_severity)
abc.full <- abc.full %>%
  mutate(disturb_severity= ifelse(disturb_severity %in% "Low", "Low: Change to the ecosystem that is temporary or does not have substantial impact on ecosystem identity or function", disturb_severity)) %>%
  mutate(disturb_severity= ifelse(disturb_severity %in% "Moderate", "Moderate: Change to the ecosystem that is temporary but has a substantial impact on ecosystem function", disturb_severity)) %>%
  mutate(disturb_severity= ifelse(disturb_severity %in% "High", "High: Change to the ecosystem that is lasting with consequences for ecosystem identity and function", disturb_severity))

unique(abc.full$disturb_extent)
unique(abc.full$disturb_year)

unique(abc.full$thaw_category)

unique(abc.full$thaw_dominant)

#data_usage

abc.full <- abc.full %>%
  mutate(data_usage = case_when(
    data_contributor_or_author %in% c(
      "Joshua Dean",
      "Bo Elberling",
      "Masahito Ueyama",
      "Masahito Ueyama | Hideki Iwata",
      "Masahito Ueyama | Yoshinobu Harazono",
      "Mats P. Bjoerkman",
      "Mats P. Bjorkman, Abbey Serrone",
      "Johannesson, C-F., Larsen, K.S., Silvennoinen, H.M., Norden, J.",
      "Amy Townsend-Small",
      "Anna-Maria Virkkala, Konsta Happonen",
      "Hannu Nykanen, Pertti Martikainen",
      "Nykanen, Hannu; Alm, Jukka; Martikainen, Pertti; Silvola, Jouko",
      "John Kochendorfer, Praveena Krishnan, Mark Heuer",
      "Dr. Sujan Pal, Dr. Ryan Sullivan",
      "Mathias Goeckede",
      "David Bastviken, Erik Sahlee",
      "David Bastviken",
      "Jarvi Jarveoja | Matthias Peichl",
      "Matthias Peichl, J√§rvi J√§rveoja",
      "Norbert Pirk",
      "Norbert Pirk | Frans-Jan W. Parmentier",
      "Andrej Varlagin",
      "Erik Sahlee, Anna Rutgersson",
      "Hanna Lee, Inge H.J. Althuizen, Casper T. Christiansen",
      "Anatoly Prokushkin, Alexey V. Panov",
      "Frederic Bouchard",
      "Helena Rautakoski",
      "Scott Davidson",
      "Hideki Kobayashi",
      "Torbern Tagesson; Department of Physical Geography and Ecosystems Science, Lund University, Solvegatan 12, 223 62, Lund, Sweden,",
      "Torbern Tagesson",
      "Pierre Taillardat & Michelle Garneau",
      "Vincent Jassey",
      "Katey Walter Anthony, Nick Hasson",
      "Katey Walter Anthony, Colin Edgar",
      "Gerard Rocher-Ros",
      "Mika Korkiakoski",
      "D. Nadeau, A. Thiboult, M. Helbig, S. Heerah, D. Spearns, A. Tremblay",
      "Mikhail Mastepanov, Torben R. Christensen",
      "Mikhail Mastepanov",
      "Patrick Sullivan",
      "Patrick F. Sullivan",
      "Maialen Barret, Lea Cabrol, Armando Sepulveda-Jauregui, Frederic Thalasso",
      "Lea Cabrol",
      "Eugenie Euskirchen, Colin Edgar",
      "Eugenie Euskirchen",
      "Elena Blanc-Betes; Jeffrey M. Welker",
      "Katrin Attermeyer, Pascal Bodmer",
      "Oliver Sonnentag, Gabriel Hould Gosselin, Haley Alcock, Matteo Detto",
      "Oliver Sonnentag, Gabriel Hould Gosselin, Muhammad Umair",
      "Oliver Sonnentag, Gabriel Hould Gosselin, Philip Marsh",
      "Frans-Jan Parmentier",
      "Frans-Jan W. Parmentier, Maarten van Hardenbroek",
      "Kukka-Maaria Kohonen",
      "Rasmus Jensen; Thomas Friborg",
      "Rasmus Jensen, Andreas Westergaard-Nielsen; Efren Lopez-Blanco",
      "Rasmus Jensen; Efren Lopez-Blanco; Torben R. Christensen",
      "Christina Biasi",
      "Lars Kutzbach, Christian Wille, Torsten Sachs",
      "Koffi Dodji Noumonvi; Matthias Peichl; Mats.B.Nilsson",
      "Liam Heffernan",
      "Han Dolman, Geert Hensgens",
      "Glagolev M.V., Runkov R.A. and Mochenov S.Yu.",
      "Maija E. Marushchak",
      "Edward A. G. Schuur",
      "Avni Malhotra, Nigel Roulet, Ruth Varner",
      "Ruth Varner",
      "Beth Holmes, Patrick Crill, Ruth Varner",
      "Lars Kutzbach, Torsten Sachs",
      "Marta Magnani; Ilaria Baneschi;  Mariasilvia Giamberini; Brunella Raco; Antonello Provenzale",
      "Luca Belelli, Dario Papale" ) ~ "Tier1",
    
    data_contributor_or_author %in% c(
      "Egor A. Dyukarev",
      "Carolina Voigt",
      "Kuno Kasak",
      "Elena D. Lapshina, Pavel Alekseychik",
      "Jon Gudmundsson",
      "Scott Zolkos",
      "Roger Seco, Riikka Rinnan, Thomas Holst",
      "Katharina Jentzsch & Julia Boike",
      "Craig Emmerton | Vince St. Louis",
      "Craig Emmerton",
      "E. E. Veretennikova",
      "Sean Carey, Erin Nicholls, Graham Clark",
      "Claire Treat, Lona van Delden, Joshua Hashemi",
      "Sofie Sjogersten",
      "Jessica Lagroix, David Olefeldt, Glynnis A. Hood",
      "Anna-Maria Virkkala, Miska Luoto",
      "Klaus Steenberg Larsen",
      "Eeva-Stiina Tuittila/Elisa Mannisto",
      "Eeva-Stiina Tuittila/Aino Korrensalo/Elisa Mannisto",
      "Namyi Chae",
      "Egle Koster",
      "Kyra St. Pierre",
      "Irina E. Terentieva, Aleksandr F. Sabrekov",
      "Aleksandr F. Sabrekov, Irina E. Terentieva",
      "Aleksandr F. Sabrekov",
      "Terhi Riutta, Aino Korrensalo, Anna M. Laine, Jukka Laine, and Eeva-Stiina Tuittila",
      "T. Andrew Black",
      "Trofim Maximov, Ayumi Kotani, Roman Petrov, Tetsuya Hiyama",
      "Adrian Rocha",
      "Katharina Jentzsch & Julia Boike" ) ~ "Tier2",    TRUE ~ data_usage ))

  
### LAST MINUTE CLEANING ####-------------------------------
unique(abc.full$site_reference)
abc.full <- abc.full %>% mutate(site_reference = sub("_Chamber$", "_chamber", site_reference)) 

#edits from julia kelly
abc.full <- abc.full %>% 
  mutate(site_reference= ifelse(site_reference %in% "Ljusdal_SLM_SLM_tower", "Ljusdal_SLM_tower", site_reference)) %>%
  mutate(site_reference= ifelse(site_reference %in% "Ljusdal_HY_HY_tower", "Ljusdal_HY_tower", site_reference))


unique(abc.full$extraction_source)
abc.full <- abc.full %>% mutate(extraction_source= gsub("/", "_", extraction_source))

unique(abc.full$nee_seasonal_interval)
abc.full <- abc.full %>%
  mutate(nee_seasonal_interval = case_when(
      is.na(nee_seasonal_interval) ~ NA_character_,
      TRUE ~ str_replace_all(
        nee_seasonal_interval,
        "(\\d{2}/\\d{2})-(\\d{2}/\\d{2})",
        paste0(year, "/\\1", " - ", year, "/\\2")     )   )  )%>%
  mutate(   co2_flux_seasonal_interval = if_else(
    is.na(co2_flux_seasonal_interval), NA_character_,
    str_replace_all(
      co2_flux_seasonal_interval,
      "(\\d{2})/(\\d{2})/(\\d{4})",
      "\\3/\\1/\\2" )  ) )

unique(abc.full$co2_flux_seasonal_interval)
abc.full <- abc.full %>%
  mutate(   co2_flux_seasonal_interval = if_else(
    is.na(co2_flux_seasonal_interval), NA_character_,
    str_replace_all(
      co2_flux_seasonal_interval,
      "(\\d{2})/(\\d{2})/(\\d{4})",
      "\\3/\\1/\\2" )  ) )



unique(abc.full$ch4_flux_seasonal_interval)
abc.full <- abc.full %>%
  mutate(   ch4_flux_seasonal_interval = if_else(
      is.na(ch4_flux_seasonal_interval), NA_character_,
      ch4_flux_seasonal_interval %>%
        #Fill missing years
        str_replace_all(
          "(?<!\\d)(\\d{2}/\\d{2})-(\\d{2}/\\d{2})(?!\\d)",
          paste0(year, "/\\1-", year, "/\\2")
        ) %>%
        # Reformat full dates
        str_replace_all(
          "(\\d{2})/(\\d{2})/(\\d{4})",
          "\\3/\\1/\\2"  ) )) %>%
  # case where ch4 and co2 were measured different years so year should be NA
  mutate(year= ifelse(ch4_flux_seasonal_interval %in% "2007/07/01-2007/11/31", NA, year))


unique(abc.full$site_activity)
abc.full <- abc.full %>% mutate(site_activity = ifelse(site_activity%in% "Yes", "Active", site_activity)) %>% 
  mutate(site_activity = ifelse(site_activity%in% "Non-Active", "Non-active", site_activity))

unique(abc.full$flux_method)
unique(abc.full$flux_method_detail) 
abc.full <- abc.full %>%
  mutate(flux_method_detail= ifelse(flux_method_detail %in% c("Open-path"), "Open-path eddy covariance", flux_method_detail)) %>%
  mutate(flux_method_detail= ifelse(flux_method_detail %in% c("Snow_diffusion"), "Snow diffusion", flux_method_detail)) %>%
  mutate(flux_method_detail= ifelse(flux_method_detail %in% c("Closed- and enclosed eddy covariance"), "Closed-path eddy covariance", flux_method_detail)) %>%
  mutate(flux_method_description= ifelse(flux_method_description %in% c( "static opaque chamber coupled in a loop to the UGGA, dissolved gases were equilibrated with a continuous flow of CH4- and CO2-free nitrogen through a gas-liquid membrane, and the resulting gas was measured in a ultraportable greenhouse gas analyzer UGGA Model 30P, Los Gatos Research"),
                                         "static opaque chamber coupled in a loop to the UGGA, dissolved gases were equilibrated with a continuous flow of CH4- and CO2-free nitrogen through a gas-liquid membrane, and the resulting gas was measured in a ultraportable greenhouse gas analyzer UGGA Model 30P, Los Gatos Research- 0.102 m2 floating chamber (7.8 L)", flux_method_description)) %>%
  mutate(flux_method_description= ifelse(flux_method_description %in% c( "static opaque chamber coupled in a loop to the UGGA"),
                                         "static opaque chamber coupled in a loop to the UGGA- 0.102 m2 floating chamber (7.8 L)", flux_method_description)) %>%
  
  mutate(flux_method_detail= ifelse(flux_method_detail %in% c("static opaque chamber coupled in a loop to the UGGA, dissolved gases were equilibrated with a continuous flow of CH4- and CO2-free nitrogen through a gas-liquid membrane, and the resulting gas was measured in a ultraportable greenhouse gas analyzer UGGA Model 30P, Los Gatos Research",
                                                              "static opaque chamber coupled in a loop to the UGGA"), "Manual chamber", flux_method_detail)) 



abc.full <- abc.full %>%
  mutate(flux_method_description= ifelse(flux_method_description %in% c("k600 estimated and departure from saturation, k600 estimated from wind speed and water body area based on Vachon & Prairie 2020",
                                                                        "k600 estimated and departure from saturation, k600 estimated from wind speed and water body area based on Vachon & Prairie 2021",
                                                                        "k600 estimated and departure from saturation, k600 estimated from wind speed and water body area based on Vachon & Prairie 2022",
                                                                        "k600 estimated and departure from saturation, k600 estimated from wind speed and water body area based on Vachon & Prairie 2023",
                                                                        "k600 estimated and departure from saturation, k600 estimated from wind speed and water body area based on Vachon & Prairie 2024",
                                                                        "k600 estimated and departure from saturation, k600 estimated from wind speed and water body area based on Vachon & Prairie 2025",
                                                                        "k600 estimated and departure from saturation, k600 estimated from wind speed and water body area based on Vachon & Prairie 2026",
                                                                        "k600 estimated and departure from saturation, k600 estimated from wind speed and water body area based on Vachon & Prairie 2027",
                                                                        "k600 estimated and departure from saturation, k600 estimated from wind speed and water body area based on Vachon & Prairie 2028"),
                                         "k600 estimated and departure from saturation, k600 estimated from wind speed and water body area based on Vachon & Prairie 2013",flux_method_description))


#Make gpp + -----

abc.full$gpp <- abc.full$gpp * -1



###reorder columns
setwd("/Users/iwargowsky/Desktop/ABCFlux v2")
column.order <- read_csv("ABCFluxv2.finalvariables.csv")
column.order$dataset <- ""
column.order$ecosystem <- ""
column.order$water_size <- ""

# Get the correct column order
desired_order <- names(column.order)

# Reorder abc.fullbased on desired order
abc.full.ordered <- abc.full[, desired_order]



setwd("/Users/iwargowsky/Desktop/arcticborealCflux") 
write_csv(abc.full.ordered, "ABCFluxv2.ter.aq.csv")
# 

abc.full$dataentry_person <- NULL

### ORNL DAAC formatting ####---------------------------------------------------------
unique(abc.full.ordered$citation)

abc.full.ordered <- abc.full.ordered %>%
  mutate(citation = str_replace_all(
    citation,
    "https?://\\s*doi\\.org/\\s*([[:alnum:]/\\.-]+)",
    "doi: \\1" )) %>%
  #manually fixing some
  mutate(citation= ifelse(citation== "https://ameriflux.lbl.gov/sites/siteinfo/CA-TVC",
                          "Oliver Sonnentag, Philip Marsh (2025), AmeriFlux BASE CA-TVC Trail Valley Creek, Ver. 2-5, AmeriFlux AMP, (Dataset). doi: 10.17190/AMF/1767831", citation)) %>%
  mutate(citation= ifelse(citation== "https://ameriflux.lbl.gov/sites/siteinfo/CA-HPC",
                          "Oliver Sonnentag, Phil Marsh (2025), AmeriFlux FLUXNET-1F CA-HPC Havikpak Creek, Ver. 4-7, AmeriFlux AMP, (Dataset). doi: 10.17190/AMF/2469437", citation)) %>%
  mutate(citation= ifelse(citation== "https://ameriflux.lbl.gov/sites/siteinfo/CA-SMC",
                          "Oliver Sonnentag (2025), AmeriFlux FLUXNET-1F CA-SMC Smith Creek, Ver. 3-7, AmeriFlux AMP, (Dataset). doi: 10.17190/AMF/2571111", citation)) %>%
  mutate(citation= ifelse(citation== "https://www.nature.com/articles/s41597-022-01759-8",
                          "doi: 10.1038/s41597-022-01759-8", citation)) %>%
  mutate(citation= ifelse(citation== "https://bg.copernicus.org/articles/20/2031/2023/",
                          "doi: 10.5194/bg-20-2031-2023", citation)) %>%
  mutate(citation= ifelse(citation== "https://essopenarchive.org/doi/full/10.22541/essoar.168394762.23256034/v1",
                          "doi: 10.1029/2024GL109283", citation)) %>%
  mutate(citation= ifelse(citation== "https://bg.copernicus.org/articles/21/335/2024/bg-21-335-2024.html",
                          "doi: 10.5194/bg-21-335-2024", citation)) %>%
  mutate(citation= ifelse(citation== "https://besjournals.onlinelibrary.wiley.com/doi/full/10.1111/1365-2745.13832",
                          "doi: 10.1111/1365-2745.13832", citation)) %>%
  mutate(citation= ifelse(citation== "https://hdl.handle.net/11676.1/ybhPJrDDYxoK828S257178WU",
                          "PID: 11676.1/ybhPJrDDYxoK828S257178WU", citation)) %>%
  mutate(citation= ifelse(citation== "https://hdl.handle.net/11676.1/pQUigq71o1huOxfF-VSSVRax",
                          "PID: 11676.1/pQUigq71o1huOxfF-VSSVRax", citation)) %>%
  mutate(citation= ifelse(citation== "https://hdl.handle.net/11676.1/aCdHCo2c_EGwoRSC0JMA3lRG",
                          "PID: 11676.1/aCdHCo2c_EGwoRSC0JMA3lRG", citation)) %>%
  mutate(citation= ifelse(citation== "https://www.borenv.net/BER/archive/pdfs/ber9/ber9-421.pdf",
                          "Huttunen, Jari & Hammar, Taina & Manninen, Pertti & Servomaa, Kristina & Martikainen, Pertti & Manninen, T & Servomaa, P & Martikainen, K. (2004). Potential springtime greenhouse gas emissions from a small southern boreal lake (Keihäsjärvi, Finland). Boreal Environ. Res.. 9.", citation)) %>%
  mutate(citation= ifelse(citation== "http://hdl.handle.net/2115/18631",
                          "Measurement of methane flux in a Tundra region near Tiksi, Eastern Siberia in 1992, NAKAYAMA, Tomoko; FUKUDA, Masami; SONE, Toshio; NAGAOKA, Daisuke. 1994-03-15", citation)) %>%
  mutate(citation= ifelse(citation== "https://dx.doi.org/doi:10.22663/KOPRI-KPDC-00000587.1",
                          "doi:10.22663/KOPRI-KPDC-00000587", citation)) %>%
  mutate(citation= ifelse(citation== "https://dx.doi.org/doi:10.22663/KOPRI-KPDC-00000678.1",
                          "", citation)) %>%
  mutate(citation= ifelse(citation== "https://dx.doi.org/doi:10.22663/KOPRI-KPDC-00000677.1",
                          "doi:10.22663/KOPRI-KPDC-00000677", citation)) %>%
  mutate(citation= ifelse(citation== "https://dx.doi.org/doi:10.22663/KOPRI-KPDC-00000864.1",
                          "doi:10.22663/KOPRI-KPDC-00000864", citation)) %>%
  mutate(citation= ifelse(citation== "https://dx.doi.org/doi:10.22663/KOPRI-KPDC-00001134.2",
                          "doi:10.22663/KOPRI-KPDC-00001134", citation)) %>%
  mutate(citation= ifelse(citation== "https://dx.doi.org/doi:10.22663/KOPRI-KPDC-00001416.1",
                          "doi.org/doi:10.22663/KOPRI-KPDC-00001416", citation)) 



#other columns with URLS
abc.full.ordered <- abc.full.ordered %>%
  #one entry in flux_method_description but its very long so well use this
  mutate(flux_method_description = str_replace_all(
    flux_method_description,
    "https?://\\s*doi\\.org/\\s*([[:alnum:]/\\.-]+)",
    "doi: \\1"  )) %>%
  #soil type detail
  mutate(soil_type_detail= ifelse(soil_type_detail== "see Heffernan et al. 2020, https://agupubs.onlinelibrary.wiley.com/doi/abs/10.1029/2019JG005501",
                                  "see Heffernan et al. 2020, doi: 10.1029/2019JG005501", soil_type_detail)) %>%
  mutate(soil_type_detail= ifelse(soil_type_detail== "see Pelletier et al. 2015, https://doi.org/10.1177/0959683617693899",
                                  "see Pelletier et al. 2015, doi: 10.1177/0959683617693899", soil_type_detail)) %>%
  #multiple in data_version
  mutate(data_version = str_replace_all(
    data_version,
    "https?://\\s*doi\\.org/\\s*([[:alnum:]/\\.-]+)",
    "doi: \\1"  )) %>%
  #notes
  mutate(notes = notes %>%
      # DOI.org links → doi: ...
      str_replace_all("https?://\\s*doi\\.org/\\s*([[:alnum:]/\\.-]+)", "doi: \\1") %>%
      # Publisher DOI links → doi: ...
      str_replace_all("https?://[^\\s]*/doi(?:/full)?/([[:alnum:]/.]+)", "doi: \\1") %>%
      # PII links → pii: ...
      str_replace_all("https?://[^\\s]*/pii/([[:alnum:]-]+)", "pii: \\1") %>%
      # Articles/ID → doi: ...
      str_replace_all("https?://[^ ]*/articles?/([0-9a-zA-Z\\.\\-/]+)(?:[#?][^ ]*)?", "doi: \\1") %>%
      # Remove extra whitespace
      str_squish() ) %>%
  #manual fixes
  mutate(notes= ifelse(notes %in% "Environmental data from repository (https://doi.pangaea.de/10.1594/PANGAEA.911742)",
                       "Environmental data from repository (doi: 10.1594/PANGAEA.911742)", notes )) %>%
  mutate(notes= ifelse(notes %in% "Additional soil carbon data from https://go.gale.com/ps/i.do?id=GALE%7CA356354219&sid=googleScholar&v=2.1&it=r&linkaccess=abs&issn=12396095&p=AONE&sw=w&userGroupName=mlin_s_abingpl&aty=ip",
                       "Additional soil carbon data from separate publication", notes )) %>%
  mutate(notes= ifelse(notes %in% "Dissolved concentrations with sample condition OK and above detection limit from external lab data were used. Data extracted from NEON, please look at original source for specific details and more information: NEON (National Ecological Observatory Network). Dissolved gases in surface water (DP1.20097.001), RELEASE-2024. doi: 10.48443/69rp-5y44. Dataset accessed from https://data.neonscience.org/data-products/DP1.20097.001/RELEASE-2024 on July 30, 2024.",
                       "Dissolved concentrations with sample condition OK and above detection limit from external lab data were used. Data extracted from NEON, please look at original source for specific details and more information: NEON (National Ecological Observatory Network). Dissolved gases in surface water (DP1.20097.001), RELEASE-2024. doi: 10.48443/69rp-5y44. Dataset accessed from NEON data portal on July 30, 2024.", notes )
         ) 
  


# unique row number 
abc.full.ordered <- abc.full.ordered %>%
  arrange(site_name, year, month) %>%
  mutate(id = row_number())



# 
#SAVE FOR ORNL DAACC


abc.full.ordered.ornldaac <- abc.full.ordered %>%
  mutate(dataset= NULL,
         water_size= NULL,
         ecosystem= NULL)
setwd("/Users/iwargowsky/Desktop/arcticborealCflux") 
abc.full.ordered.ornldaac %>%
  # Replace -9999 in numeric columns
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), -9999, .))) %>%
  # Replace literal "NA" in text columns
  mutate(across(where(is.character), ~ ifelse(is.na(.), "NA", .))) %>%
  # Write CSV with all text fields quoted
  write_csv("ABCFluxv2.csv", quote = "all")










# Terrestrial sites
abc.tersites <- abc.full  %>%
  filter(dataset %in% "Terrestrial") %>%
  group_by(latitude, longitude, site_name, site_reference, flux_method, country, land_cover_eco, land_cover_plot, bawld_class) %>%
  dplyr::summarise(
    Flux = case_when(
      all(is.na(nee) & is.na(gpp) & is.na(reco) & is.na(nee_seasonal)) &
        any(!is.na(ch4_flux_total) | !is.na(ch4_flux_total_seasonal)) ~ "CH4",
      
      any(!is.na(nee) | !is.na(gpp) | !is.na(reco) | !is.na(nee_seasonal)) &
        all(is.na(ch4_flux_total) & is.na(ch4_flux_total_seasonal)) ~ "CO2",
      
      any(!is.na(nee) | !is.na(gpp) | !is.na(reco) | !is.na(nee_seasonal)) &
        any(!is.na(ch4_flux_total) | !is.na(ch4_flux_total_seasonal)) ~ "CO2 and CH4",
      
      TRUE ~ NA_character_
    ),
    .groups = "drop"
  ) %>% group_by( site_reference, site_name, flux_method, country, latitude, longitude, land_cover_eco, land_cover_plot, bawld_class ) %>%
  dplyr::summarise(Flux = paste(unique(na.omit(Flux)), collapse = ", "),
                   .groups = "drop") %>%
  mutate(Flux= ifelse(Flux %in%  c("CO2 and CH4, CO2",
                                   "CO2, CH4",
                                   "CH4, CO2" ,
                                   "CO2, CO2 and CH4"),  "CO2 and CH4", Flux)) %>%
  mutate(siteID = paste(site_reference, latitude, longitude, sep = ", "))
  
  

#data for Stephano
abc.terfluxes <- abc.full  %>%
  filter(dataset %in% "Terrestrial") %>%
  select(latitude, longitude, site_name, site_reference, flux_method, country, nee, gpp, reco, ch4_flux_total, year, month, land_cover_eco, land_cover_plot, bawld_class)%>%
  mutate(siteID = paste(site_reference, latitude, longitude, sep = ", "))
# 
# 
setwd("/Users/iwargowsky/Desktop/GEE_exports")
#write_csv(abc.tersites, "abc.tersites.csv")
#write_csv(abc.terfluxes , "abc.terfluxes.csv")


x <- abc.tersites %>% get_dupes(site_reference, flux_method)

# 
# 
# #disturbances
# disturbcats <- abc.full%>% select(disturbance_category, disturbance, dataset) %>% distinct()
# setwd("/Users/iwargowsky/Desktop") 
# write_csv(disturbcats, "disturbcats.csv")
# 
# 



fire <- abc.full.ordered %>%
  filter(str_detect(disturbance_category, regex("fire", ignore_case = TRUE))) %>% 
  select(site_reference, disturbance, disturbance_category, disturb_year, disturb_severity, disturb_extent) %>% 
  distinct()
setwd("/Users/iwargowsky/Desktop") 
write_csv(fire, "abcflux.firesites.csv")

# Example: pick specific sites to highlight
highlight_sites <- c("Kytalyk, Russia_RU-Cok_tower",
                     "Igarka_RU-Iga Feathermoss cover_chamber",
                     "Tura_RU-Tur Lichen cover_chamber",
                     "Bouleau peatland_automatedchamber2_HummockSphagnum_chamber")  

abc.full.ordered$highlight <- ifelse(abc.full.ordered$site_reference %in% highlight_sites, "highlight", "normal")

ggplot(abc.full.ordered, aes(x = month, y = gpp)) +
  geom_point(aes(color = highlight)) +
  geom_line(aes(group = site_reference), alpha = 0.4) +
  scale_x_continuous(breaks = 1:12) +
  scale_color_manual(values = c("highlight" = "red", "normal" = "black")) +
  labs(x = "Month", y = "GPP", title = "GPP by Month (Highlighted Sites)") +
  theme_minimal()



