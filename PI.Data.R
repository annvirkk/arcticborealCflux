#PI data
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

setwd("/Users/iwargowsky/Desktop/Data from PIs") 
###Sonnentag and Alcock####-----------------------------------------------------
files <- list.files(path= "/Users/iwargowsky/Desktop/Data from PIs", 
                    pattern = "*Sonnentag_",all.files = T,recursive = T)
sonnentag <- files %>%
  map_df(~read_csv(.x, col_types = cols(), col_names = TRUE, na=c("NA","-9999", "NaN")))          
sonnentag <- sonnentag %>% 
  dplyr::filter(!site_name=="") %>% #removing blank rows
  dplyr::rename("gap_fill_perc_nee"= "gap_fill_perc")


###Norbert Pirk####-------------------------------------------------------------
pirk.finse <- read_csv("ABCfluxv2_finse.csv") %>%
  dplyr::rename("gap_fill_perc_nee"= "gap_fill_perc")

pirk.iskoras <- read_csv("ABCfluxv2_iskoras.csv")

###Masahito Ueyama####----------------------------------------------------------
masa <- read_csv("Arctic_Boreal_CO2_Flux_ueyama_V2_2024.03.11.csv") %>%
  dplyr::rename("gap_fill_perc_nee"= "gap_fill_perc")

###Julia Boike####--------------------------------------------------------------
boike <- read_csv("ABCfluxv2.varsAWI_Bayelva.csv")%>%
  dplyr::rename("gap_fill_perc_nee"= "gap_fill_perc")
#changing site name to be consistent with fluxnet name for tower 
# Bayelva, Svalbard -> Bayelva, Spitsbergen
boike$site_name <- "Bayelva, Spitsbergen" 

###Sang Jong####----------------------------------------------------------------
jong <- read_csv("ABCfluxv2.vars_US-KOC_Sangjong.csv")%>%
  dplyr::rename("gap_fill_perc_nee"= "gap_fill_perc")

###Aleksandr Sabrekov####-------------------------------------------------------
sabrekov.ec <- read_csv("Russian data/Lapshina CO2 tower.csv")%>%
  dplyr::rename("gap_fill_perc_nee"= "gap_fill_perc")
sabrekov.ch <- read_csv("Russian data/Sabrekov methane chambers.csv")%>%
  dplyr::rename("gap_fill_perc_ch4"= "gap_fill_perc",
                "chamber_nr_measurement_days_ch4"= "chamber_nr_measurement_days")

###Egor Dyukarev####------------------------------------------------------------
dyukarev.ec <- read_csv("Russian data/ABCfluxv2_vars - Dyukarev-Veretennikova.tower.csv") %>%
  mutate(gpp= ifelse(site_reference %in% "RU-Plt", gpp *-1, gpp)) %>%#fixing because gpp is not negative for this site
  dplyr::rename("gap_fill_perc_nee"= "gap_fill_perc")
dyukarev.ch <- read_csv("Russian data/ABCfluxv2_vars - Dyukarev-Veretennikova.chamber.csv") %>%
  dplyr::rename("gap_fill_perc_nee"= "gap_fill_perc")%>%
  mutate(chamber_nr_measurement_days_co2= ifelse(is.na(ch4_flux_total), chamber_nr_measurement_days, NA)) %>%
  mutate(chamber_nr_measurement_days_ch4= ifelse(is.na(nee), chamber_nr_measurement_days, NA)) %>%
  mutate(chamber_nr_measurement_days = NULL)

###Mike Peacock####-------------------------------------------------------------
peacock <- read_csv("ABCfluxv2.vars_Mike_Peacock_terrestrialchamber.csv") %>%
  mutate(chamber_nr_measurement_days_ch4= chamber_nr_measurement_days) %>%
  dplyr::rename("chamber_nr_measurement_days_co2"= "chamber_nr_measurement_days") #chamber that was used measures both CO2 and CH4

###Ji Young Jung####------------------------------------------------------------
jung <- read_csv("ABCfluxv2.vars_JYJ_230639.csv") %>% 
group_by(site_name, site_reference, year, month, longitude, latitude, site_id) %>%
  dplyr::summarise(across(where(is.numeric),list(mean = ~ mean(.x, na.rm = TRUE)) ),
                   across(where(is.character), list(unique = ~toString(unique(.[!is.na(.)]))))) %>%
  rename_with(~gsub("_unique", "", .), everything()) %>%  # Remove "_unique" from column names
  rename_with(~gsub("_mean", "", .), everything()) %>%  # Remove "_mean" from column names
  dplyr::rename("chamber_nr_measurement_days_co2"= "chamber_nr_measurement_days") 

###Inge Althuizen ####----------------------------------------------------------
althuizen <- read_csv("ABCfluxv2_Iskoras_IngeAlthuizen.vars_updateApril2024.csv")

### Christopher Schulze####-----------------------------------------------------
schulze.ch <- read_csv("ABCflux_SMC_STR_LUT_vCS_2023-12-25.csv") %>% 
  mutate(reco= as.numeric(reco)) %>%#to remove soil respiration data
  mutate(chamber_nr_measurement_days_co2= chamber_nr_measurement_days)%>%
  dplyr::rename("chamber_nr_measurement_days_ch4"="chamber_nr_measurement_days" )
schulze.lut.ec <- read_csv("ABCflux_LUT_vCS_2023-12-25.csv") %>%
  dplyr::rename("gap_fill_perc_nee"= "gap_fill_perc")
schulze.str.ec <- read_csv("ABCflux_STR_vCS_2023-12-25.csv") %>%
  dplyr::rename("gap_fill_perc_nee"= "gap_fill_perc")

### Vincent Jassey ####---------------------------------------------------------
jassey <- read_csv("ABCfluxv2.vars_JASSEY.csv") %>%
  dplyr::rename("chamber_nr_measurement_days_co2"= "chamber_nr_measurement_days") 
jassey <- jassey %>% dplyr::filter(!site_name %in% 'Lapazeuil')

### Alexandre Roy ####----------------------------------------------------------
roy <- read_csv("ABCflux_MES.csv") %>%
  dplyr::rename("gap_fill_perc_nee"= "gap_fill_perc")

### Jackie Hung ####------------------------------------------------------------
hung.ec <- read_csv("ABCfluxv2_Hung_Minions_tower.csv") %>%
  dplyr::rename("gap_fill_perc_nee"= "gap_fill_perc")
hung.ch <- read_csv("ABCfluxv2_Hung_Minions_chamber.csv")
hung.ch.ykd <- hung.ch %>%
  dplyr::filter(!site_name %in% "CBAWO wet sedge")%>%
  mutate(site_reference= ifelse(site_name== "YKD (burned degraded plateau)", "burned degraded plateau", site_reference)) %>%
  mutate(site_reference= ifelse(site_name== "YKD (burned shrub edge)", "burned shrub edge", site_reference)) %>%
  mutate(site_reference= ifelse(site_name== "YKD (exposed burned soil)", "exposed burned soil", site_reference)) %>%
  mutate(site_reference= ifelse(site_name== "YKD (unburned degraded plateau)", "unburned degraded plateau", site_reference)) %>%
  mutate(site_reference= ifelse(site_name== "YKD (unburned shrub edge)", "unburned shrub edge", site_reference)) %>%
  mutate(site_reference= ifelse(site_name== "YKD (unburned lichen peat plateau)", "unburned lichen peat plateau", site_reference)) %>%
  mutate(site_reference= ifelse(site_name== "YKD (unburned Fen)", "unburned Fen", site_reference)) %>%
  mutate(site_reference= ifelse(site_name== "YKD (Burned Fen)", "Burned Fen", site_reference)) %>%
  mutate(site_name= "YKD") %>%
  mutate(site_reference= paste(site_reference, str_split(site_id, "Hung_YKD") %>% sapply(`[`, 2)), sep="") %>%
  mutate(chamber_nr_measurement_days_ch4 = ifelse(!is.na(ch4_flux_total)| !is.na(ch4_flux_seasonal), chamber_nr_measurement_days, NA)) %>%
  mutate(chamber_nr_measurement_days_co2 = ifelse(!is.na(nee) | !is.na(gpp) | !is.na(reco) | !is.na(nee_seasonal), chamber_nr_measurement_days, NA),
         chamber_nr_measurement_days= NULL)
hung.ch.cbawo <- hung.ch %>%
  dplyr::filter(site_name %in% "CBAWO wet sedge")%>%
  mutate(chamber_nr_measurement_days_ch4 = ifelse(!is.na(ch4_flux_total)| !is.na(ch4_flux_seasonal), chamber_nr_measurement_days, NA)) %>%
  mutate(chamber_nr_measurement_days_co2 = ifelse(!is.na(nee) | !is.na(gpp) | !is.na(reco) | !is.na(nee_seasonal), chamber_nr_measurement_days, NA),
         chamber_nr_measurement_days= NULL)


### Liam Heffernan ####---------------------------------------------------------
heffernan <- read_csv("ABCfluxv2.vars.liamheffernan.lutose.csv")

### Helena Rautakoski###--------------------------------------------------------
rautakoski <- read_csv("ABCfluxv2_Ranskala.csv")
rautakoski <- rautakoski %>%
  dplyr::rename("gap_fill_perc_nee"= "gap_fill_perc")%>%
  mutate(site_name= ifelse(notes=="Treatment: Area that will be harvested in the end of March 2021", "Ränskälänkorpi, Continuous cover forestry treatment", site_name))%>%
  mutate(site_name= ifelse(notes=="Treatment: Continuous cover forestry treatment. Area harvested in the end of March 2021", "Ränskälänkorpi, Continuous cover forestry treatment", site_name)) %>%
  mutate(site_reference= ifelse(site_name=="Ränskälänkorpi, Continuous cover forestry treatment", "FI-Ran forestry treatment", site_reference))


### Efren Lopez-Blanco###-------------------------------------------------------
lopezblanco.ec <- read_csv("ABCflux_GEM2022data_upto2022.tower.csv", na = "-9999") %>%
  dplyr::rename("gap_fill_perc_nee"= "gap_fill_perc")
lopezblanco.ch <- read_csv("ABCflux_GEM2022data_upto2022.ch.csv", na = "-9999")
lopezblanco.ch$chamber_nr_measurement_days_ch4 <- "Continuous" #automatic chambers

### Mika Aurela####-------------------------------------------------------------
aurela.terv <- read_csv("ABCfluxv2_tervalaminsuotower.csv") %>%
  dplyr::rename("gap_fill_perc_nee"= "gap_fill_perc")
aurela.lett.ec <- read_csv("ABCfluxv2_lettosuo.vars.tower.csv")%>%
  dplyr::rename("gap_fill_perc_nee"= "gap_fill_perc")
aurela.lett.ch <- read_csv("ABCfluxv2_lettosuo.vars.chamber.csv")%>%
  dplyr::rename("gap_fill_perc_ch4"= "gap_fill_perc")

###Pertti J. Martikainen#####---------------------------------------------------
martikainen <- read_csv("Kaamanen aapa mire_Pertti J. Martikainen.csv") %>%
  dplyr::rename("chamber_nr_measurement_days_ch4"= "chamber_nr_measurement_days") 
  

###Pierre Tallidart#####--------------------------------------------------------
tallidart.ec <- read_csv("ABCfluxv5_CA-BOU_tower.csv")
tallidart.ch <- read_csv("ABCfluxv5_CA-BOU_chamber.csv")%>%
  mutate(site_reference= str_split(site_id, "Bouleau_") %>% sapply(`[`, 2)) %>%
  mutate(chamber_nr_measurement_days_co2= chamber_nr_measurement_days)%>%
  dplyr::rename("chamber_nr_measurement_days_ch4"="chamber_nr_measurement_days" ) %>%
  dplyr::rename("gap_fill_perc_nee"= "gap_fill_perc")


tallidart.ec$data_usage <- "Tier 2"
tallidart.ch$data_usage <- "Tier 2"

### Kajar Köster ####-----------------------------------------------------------
koster.ch <- read_csv("ABCfluxv2_Koster.csv")

### Sofie Sjogersten ####-------------------------------------------------------
sjogersten.ch <- read_csv("Sjogersten wetland sites ABCfluxv2.vars.csv") %>%
  group_by(site_name, site_reference, year, month, longitude, latitude, site_id) %>%
  dplyr::summarise(across(where(is.numeric),list(mean = ~ mean(.x, na.rm = TRUE)) ),
                   across(where(is.character), list(unique = ~toString(unique(.[!is.na(.)]))))) %>%
  rename_with(~gsub("_unique", "", .), everything()) %>%  # Remove "_unique" from column names
  rename_with(~gsub("_mean", "", .), everything()) %>%# Remove "_mean" from column names
  mutate(site_reference= paste(site_reference, "agg", sep="_")) 

###Patrick Sullivan ####--------------------------------------------------------
sullivan.ec <- read_csv("ABCfluxv2.vars_Sullivan.csv", na= "NA")

###Geert Hensgens####-----------------------------------------------------------
hensgens.ec <- read_csv("ABCfluxv2_New.KYT.csv") %>%
  mutate(gpp=  gpp *-1) %>% #fixing because gpp is not negative 
  dplyr::rename("gap_fill_perc_ch4"= "gap_fill_perc_CH4",
                "gap_fill_perc_nee"= "gap_fill_perc_NEE")

###Danila Illyasov####---------------------------------------------------------
ilyasov.ch <- read_csv("ABCfluxv2_vars_Mukhrino_Ilyasov_Niyazova.csv") %>%	
  mutate(site_reference= str_split(site_id, "Ilyasov_Mukhrino_") %>% sapply(`[`, 2)) %>%
  group_by(site_name, site_reference, year, month, longitude, latitude, site_id) %>%
  dplyr::summarise(across(where(is.numeric),list(mean = ~ mean(.x, na.rm = TRUE)) ),
                   across(where(is.character), list(unique = ~toString(unique(.[!is.na(.)]))))) %>%
  rename_with(~gsub("_unique", "", .), everything()) %>%  # Remove "_unique" from column names
  rename_with(~gsub("_mean", "", .), everything()) %>% # Remove "_mean" from column names
  mutate(site_reference= paste(site_reference, "agg", sep="_")) %>%
  dplyr::rename("chamber_nr_measurement_days_co2"= "chamber_nr_measurement_days") 
  


 glagolev.ch <- read_csv("ABCfluxv2_var_Dorokhovo_Glagolev_Runkov_Mochenov.ch.csv") %>%
  mutate(ch4_flux_total= as.numeric(ch4_flux_total)) %>% 
   mutate(water_table_depth= as.numeric(water_table_depth)) %>%
  group_by(site_name, site_reference, year, month, longitude, latitude, site_id) %>%
   dplyr::summarise(across(where(is.numeric),list(mean = ~ mean(.x, na.rm = TRUE)) ),
                   across(where(is.character), list(unique = ~toString(unique(.[!is.na(.)]))))) %>%
  rename_with(~gsub("_unique", "", .), everything()) %>%  # Remove "_unique" from column names
  rename_with(~gsub("_mean", "", .), everything()) %>% # Remove "_mean" from column names
  mutate(site_reference= paste(site_reference, "agg", sep="_"))%>%
   dplyr::rename("chamber_nr_measurement_days_ch4"= "chamber_nr_measurement_days") 


###Joachim Jansen####-----------------------------------------------------------
jansen.ec <- read_csv("ABCfluxv2_vars_SE-St1_v3.csv")

###Sean Carey####---------------------------------------------------------------
carey.ec <- read_csv("ABCfluxv2.vars_Carey_Dec20.csv")%>%
   dplyr::rename("gap_fill_perc_nee"= "gap_fill_perc")
 #manually switched gpp and reco for Wolf_creek_SparseShrub and Wolf_creek_Buckbrush because i believe it was a data entry error

###Carolina Voigt####-----------------------------------------------------------
voigt.ch <- read_csv("ABCfluxv2_CarolinaVoigt.csv") %>% 
  mutate(site_reference= ifelse(flux_method_detail== "automated chamber", paste(site_reference, "Automatic Chamber"), site_reference)) %>%
  dplyr::filter(!(nee_seasonal_interval %in% "06/27/2019-08/24/2019")) %>%
  dplyr::filter(!(ch4_flux_seasonal_interval %in% "05/31/2021-06/09/2021"))%>%
   dplyr::rename("gap_fill_perc_nee"= "gap_fill_perc") %>%
   dplyr::rename("chamber_nr_measurement_days_ch4"= "chamber_nr_measurement_days") 

###Roger Seco####---------------------------------------------------------------
seco.ec <- read_csv("ABCfluxv2.vars_RogerSeco_birch.csv")%>%
   dplyr::rename("gap_fill_perc_nee"= "gap_fill_perc") 

###Sujan Pal and Ryan Sullivan####----------------------------------------------
pal.bar.ec <- read_csv("BAR_ABCfluxv2.vars_SP_rcs.csv", na= "-9999")%>% 
  mutate(ch4_flux_total= ch4_flux_total/1000) # values are suspiciously high so i suspect its mg not g
pal.oli.ec <- read_csv("OLI_ABCfluxv2.vars_SP_rcs.csv", na= "-9999")

###Matthias Peichl####----------------------------------------------------------
peichl.deg.ch <- read_csv("ABCfluxv2.vars_SE-Deg_ch.csv")%>%
  dplyr::rename("chamber_nr_measurement_days_ch4"= "chamber_nr_measurement_days") %>%
  mutate(chamber_nr_measurement_days_co2 = ifelse(!is.na(nee), chamber_nr_measurement_days_ch4, NA))
peichl.hlf.ec <- read_csv("ABCfluxv2.vars_SE-Hlf.csv")%>%
  dplyr::rename("gap_fill_perc_nee"= "gap_fill_perc")%>%
  mutate(gap_fill_perc_ch4= gap_fill_perc_nee) %>%
  mutate(site_reference= "SE-Hfm")
peichl.hlm.ec <- read_csv("ABCfluxv2.vars_SE-Hlm.csv")%>%
  dplyr::rename("gap_fill_perc_nee"= "gap_fill_perc")%>%
  mutate(gap_fill_perc_ch4= gap_fill_perc_nee)
peichl.stj.ec <- read_csv("ABCfluxv2.vars_SE-Stj.csv") %>%
  mutate(gpp= gpp *-1) %>%#fixing because gpp is not negative for this site
  dplyr::rename("gap_fill_perc_nee"= "gap_fill_perc")%>%
  mutate(gap_fill_perc_ch4= gap_fill_perc_nee)

###Claire Treat, Lona van Delden####--------------------------------------------
treat.ch <- read_csv("ABCfluxv2_Siikaneva2022.csv") %>%
  mutate(chamber_nr_measurement_days_ch4= chamber_nr_measurement_days) %>%
  dplyr::rename("chamber_nr_measurement_days_co2"= "chamber_nr_measurement_days") #chamber that was used measures both CO2 and CH4
#fix units from ug CO2/CH4 to g C 
treat.ch$nee <- treat.ch$nee*0.000001/44.01*12.01
treat.ch$reco <- treat.ch$reco*0.000001/44.01*12.01
treat.ch$ch4_flux_total <- treat.ch$ch4_flux_total*0.000001/16.04*12.01

treat.ch <- treat.ch %>% 
  mutate(nee= ifelse(site_reference %in% c("A3","A4","A5","A6","B3","B4","B5","B6"), NA, nee)) %>%
  mutate(reco= ifelse(site_reference %in% c("A3","A4","A5","A6","B3","B4","B5","B6"), NA, reco)) %>%
  mutate(data_contributor_or_author= "Claire Treat, Lona van Delden, Joshua Hashemi") %>%
  mutate(email= "claire.treat@awi.de; lona.van.delden@awi.de; joshua.hashemi@awi.de")

###Eeva-Stiina Tuittila/Elisa Männistö####--------------------------------------
tuittila.ch <- read_csv("ABCfluxv2.vars_Siikaneva2.csv") %>%
  mutate(site_reference= ifelse(!is.na(ch4_flux_total), paste(site_reference, "ch4"), site_reference)) %>%
  mutate(site_reference= ifelse(!is.na(nee), paste(site_reference, "nee"), site_reference)) %>%
  mutate(site_reference= ifelse(!is.na(reco), paste(site_reference, "reco"), site_reference)) %>%
  mutate(chamber_nr_measurement_days_ch4= ifelse(is.na(nee) & is.na(reco), chamber_nr_measurement_days, NA)) %>%
  mutate(chamber_nr_measurement_days_co2= ifelse(is.na(ch4_flux_total), chamber_nr_measurement_days, NA)) %>%
  mutate(chamber_nr_measurement_days= NULL)


###Kuno Kasak####---------------------------------------------------------------
kasak.ec <- read_csv("ABCFlux_data_Estonia.csv")%>%
  mutate(chamber_nr_measurement_days_co2= ifelse(is.na(ch4_flux_total), chamber_nr_measurement_days, NA)) %>%
  mutate(chamber_nr_measurement_days_ch4= ifelse(is.na(nee), chamber_nr_measurement_days, NA)) %>%
  mutate(chamber_nr_measurement_days = NULL) %>%
  mutate(gap_fill_perc_ch4= gap_fill_perc) %>%
  dplyr::rename("gap_fill_perc_nee"= "gap_fill_perc")

###Katey Walter Anthony####-----------------------------------------------------
anthony.ch <- read_csv("2024_01_04_ABCfluxv2.vars-1_ThermokarstMounds.chamber.csv")
anthony.ec <- read_csv("2023_01_04_ABCfluxv2.vars-1_ThermokarstMounds.tower.csv")

anthony.met <- read_xlsx("2024_05_16_NSYtowerdata.xlsx", sheet = "Temperature") %>%
  mutate(year= year(as.Date(Date)), month= month(as.Date(Date))) %>%
  group_by(year, month)%>%
  dplyr::summarise(tair= mean(Tair_degC, na.rm=T),
                   tsoil_surface= mean(Tsoil_10cm_DegC, na.rm=T),
                   tsoil_surface_depth= "10")
anthony.ec <- anthony.ec %>%
  mutate(tair= NULL, tsoil_surface= NULL, tsoil_surface_depth=NULL, tsoil_deep= NULL, tsoil_deep_depth=NULL) %>%
  natural_join(anthony.met, by= c("year", "month"), jointype= "FULL")

### Mats Bjorkman####-----------------------------------------------------------
bjorkman.ch <- read_csv("Flux Latnjajaure.csv", na= "NA")


### Alexander Salazar ####------------------------------------------------------
salazar.ch <- read_csv("ABCfluxv2.vars_AS.csv", na= "NA") %>% 
  mutate(site_reference= ifelse(veg_detail== "Anthelia biocrust", paste(site_reference, "_biocrust", sep= ""), site_reference)) %>%
  mutate(site_reference= ifelse(veg_detail== "Mixed of moss, lichens and anthelia biocrust", paste(site_reference, "_mixedveg", sep=""), site_reference)) %>%
  mutate(site_reference= ifelse(veg_detail== "Bare ground", paste(site_reference, "_bareground", sep= ""), site_reference)) %>%
  mutate(chamber_nr_measurement_days_ch4= chamber_nr_measurement_days) %>%
  dplyr::rename("chamber_nr_measurement_days_co2"= "chamber_nr_measurement_days") #chamber that was used measures both CO2 and CH4

### Kyle Arndt ###--------------------------------------------------------------
arndt.ec.cf <- read_csv("ABCfluxv2.vars_arndt_cf3.csv")
arndt.ec.iq <- read_csv("ABCfluxv2.vars_IQ1.csv")%>%
  dplyr::rename("gap_fill_perc_nee"= "gap_fill_perc")

### Hannu Nykanen ###-----------------------------------------------------------
nykanen.ch <- read_csv("Kevo-ABCfluxv_Nykänen_231120.csv")%>%
  mutate(chamber_nr_measurement_days_co2= ifelse(is.na(ch4_flux_total), chamber_nr_measurement_days, NA)) %>%
  mutate(chamber_nr_measurement_days_ch4= ifelse(is.na(nee), chamber_nr_measurement_days, NA)) %>%
  mutate(chamber_nr_measurement_days = NULL)


### Maija E. Marushchak ###-----------------------------------------------------
marushchak.ch <- read_csv("ABCfluxv2.MaijaMarushchak.csv", na= "NA")

### Hailey Webb ###--------------------------------------------------------------
webb.ch <- read_csv("ABCfluxv2vars_HWebb.csv", na= "NA") %>%
  mutate(site_reference= paste(site_reference, str_split(site_id, "Turetsky_APEXBeta_") %>% sapply(`[`, 2), sep="_"))%>%
  mutate(month= as.integer(factor(month, levels = month.name)))

#redid unit conversions 
webb.ch$nee <- webb.ch$nee_0/10^6 *60*60*24*12.01*days_in_month(as.yearmon(paste(webb.ch$year, webb.ch$month,sep = '-')))
webb.ch$reco<- webb.ch$reco_0/10^6 *60*60*24*12.01*days_in_month(as.yearmon(paste(webb.ch$year, webb.ch$month,sep = '-')))
webb.ch$gpp <- webb.ch$nee - webb.ch$reco
webb.ch$ch4_flux_total <- webb.ch$ch4_flux_total_0/10^6*60*24*12.01*days_in_month(as.yearmon(paste(webb.ch$year, webb.ch$month,sep = '-')))

webb.ch$nee_0 <- NULL
webb.ch$reco_0 <- NULL
webb.ch$ch4_flux_total_0 <- NULL

webb.ch <- webb.ch %>%
  group_by(site_name, site_reference, year, month, longitude, latitude, site_id) %>%
  dplyr::summarise(across(where(is.numeric),list(mean = ~ mean(.x, na.rm = TRUE)) ),
                   across(where(is.character), list(unique = ~toString(unique(.[!is.na(.)]))))) %>%
  rename_with(~gsub("_unique", "", .), everything()) %>%  # Remove "_unique" from column names
  rename_with(~gsub("_mean", "", .), everything()) %>%# Remove "_mean" from column names
  mutate(site_reference= paste(site_reference, "agg", sep="_"))

### Carl-Fredrik Johannesson ###------------------------------------------------
johannesson.ch <-read_delim("MethaneData_CFJ.csv", delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                          encoding = "Latin1"), trim_ws = TRUE)[,-1]
johannesson.ch  <- johannesson.ch %>%
  mutate(site_reference= str_split(site_id, "Johannesson_") %>% sapply(`[`, 2), sep="_") %>%
  dplyr::rename("chamber_nr_measurement_days_ch4"= "chamber_nr_measurement_days")

### David Olefeldt ###----------------------------------------------------------
olefeldt.ch <- read_csv("ABCflux.kashakempton.lutose.2023.csv")%>%
  dplyr::rename("chamber_nr_measurement_days_ch4"= "chamber_nr_measurement_days")


### Anatoly Prokushkin ###------------------------------------------------------
prokushkin.ch <- read_csv("Data basr ABC flux_KJA_ver 19-01-23.chamber.csv") %>%
  dplyr::rename("chamber_nr_measurement_days_co2"= "chamber_nr_measurement_days")

prokushkin.ec <- read_csv("Data basr ABC flux_KJA_ver 19-01-23.tower.csv") %>%
  dplyr::rename("gap_fill_perc_nee"= "gap_fill_perc")

### Matthias Goeckede/Abdullah Bolek ###----------------------------------------
goeckede.1.ec <- read_csv("ABCfluxv2.vars_Tower1.csv") %>%
  mutate(site_name="Cherskii")
goeckede.2.ec <- read_csv("ABCfluxv2.vars_Tower2.csv")%>%
  mutate(site_name="Cherskii reference")

### Kyra St.Pierre ###----------------------------------------------------------
stpierre.ch <- read_csv("ABCfluxv2.vars_KSP.csv")%>%
  dplyr::rename("chamber_nr_measurement_days_ch4"= "chamber_nr_measurement_days")


### Elena Blanc-Betes ###-------------------------------------------------------
blancbetes.ch <- read_csv("ABCflux_blanc-betes-with_ch4.csv") %>%
  mutate(site_name= "Toolik Lake- Long-term US ITEX") %>%
  mutate(site_reference= str_split(site_id,"Blanc-Betes_ToolikITEX_") %>% sapply(`[`, 2), sep="_")%>%
  dplyr::rename("chamber_nr_measurement_days_co2"= "chamber_nr_measurement_days",
                "gap_fill_perc_nee"= "gap_fill_perc") 


### Räsänen Aleksi ###----------------------------------------------------------
rasanen.ch <- read_csv("ABCfluxv2vars_AR_corrected.csv") %>%
  dplyr::rename("chamber_nr_measurement_days_ch4"= "chamber_nr_measurement_days")

### Anna Virkkala ###----------------------------------------------------------
virkkala.2017.ch <- read_csv("abcflux_kilpisjarvi_2017_co2_virkkala_march2024.csv") %>%
  mutate(site_reference= str_extract(site_id, "chamber_\\d+"))%>%
  dplyr::rename("chamber_nr_measurement_days_co2"= "chamber_nr_measurement_days",
                "soil_depth" = "org_dep",
                "soil_moisture" = "moist",
                "soil_perc_c"= "soil_per_c")
virkkala.2018.ch <- read_csv("abcflux_kilpisjarvi_2018_ch4_virkkala_march2024.csv") %>%
  mutate(site_reference= str_extract(site_id, "chamber_\\d+"))%>%
  dplyr::rename("chamber_nr_measurement_days_co2"= "chamber_nr_measurement_days")


## PROCESSING DATA FROM PIs #############################################################################

### Scott Davidson####----------------------------------------------------------
#####Processing davidson.16####
davidson.16 <- read_excel("Davidson/Davidson_et al. 2016 Ecosystems.xlsx", sheet= 2)
davidson.16.monthly <- davidson.16 %>% 
  mutate(year= substr(date, 1,4), month= substr(date,5,6), day= substr(date,7,8)) %>%
  group_by(year, month, site_position, wetness, vegetation) %>%
  dplyr::summarise(tsoil_surface= mean(c(`temp_10 (deg C)`, `temp_5 (deg C)`), na.rm = TRUE) ,
            thaw_depth= mean(`thaw depth  (cm)`, na.rm = TRUE),
            gpp= mean(`GPP (gC - CO2 m2 hr1)`, na.rm = TRUE),
            nee= mean(`NEE (gC - CO2 m2 hr1)`, na.rm = TRUE),
            reco= mean(`ER (gC - CO2 m2 hr1)`, na.rm = TRUE),
            water_table_depth= mean(`wt (cm)`, na.rm = TRUE),
            soil_moisture= mean(`soil_moist (%)`, na.rm = TRUE),
            ch4_flux_total= mean(`avCH4 (mgC-CH4 m2 hr1)`, na.rm = TRUE),
            ppfd= mean(par, na.rm = TRUE),
            tair= mean(air_temp, na.rm = TRUE),
            chamber_nr_measurement_days= n_distinct(day))
#change units to gC - CO2 m2 per month
davidson.16.monthly$nee <- davidson.16.monthly$nee *24* days_in_month(as.yearmon(paste(davidson.16.monthly$year,davidson.16.monthly$month,sep = '-')))
davidson.16.monthly$gpp <- davidson.16.monthly$gpp *24* -1*days_in_month(as.yearmon(paste(davidson.16.monthly$year,davidson.16.monthly$month,sep = '-')))
davidson.16.monthly$reco <- davidson.16.monthly$reco *24* days_in_month(as.yearmon(paste(davidson.16.monthly$year,davidson.16.monthly$month,sep = '-')))
davidson.16.monthly$ch4_flux_total <- davidson.16.monthly$ch4_flux_total /1000*24* days_in_month(as.yearmon(paste(davidson.16.monthly$year,davidson.16.monthly$month,sep = '-')))
davidson.16.monthly$water_table_depth <- davidson.16.monthly$water_table_depth *-1
#rename column
davidson.16.monthly <- davidson.16.monthly %>% dplyr::rename("site_reference"="site_position",
                                                             "soil_moisture_class"= "wetness",
                                                             "veg_detail"= "vegetation")
#Adding static info
davidson.16.monthly$tsoil_surface_depth <- "7.5"
davidson.16.monthly$data_contributor_or_author <- "Scott Davidson"
davidson.16.monthly$email <- "scott.davidson@plymouth.ac.uk"
davidson.16.monthly$citation <- "https://doi.org/10.1007/s10021-016-9991-0"
davidson.16.monthly$country <- "USA"
davidson.16.monthly$biome <- "Tundra"
davidson.16.monthly$gap_fill <- "Average"
davidson.16.monthly$flux_method <- "Chamber"
davidson.16.monthly$flux_method_detail <-  "Manual Chamber"
davidson.16.monthly$flux_method_description <- "PVC collars (height 15 cm 9 diameter 20 cm). The UGGA was used to measure both CH4 and CO2 concentrations using a closed system with a 1 Hz sampling rate. The chamber was left in place for two minutes to achieve a stable increase in CH4 and CO2 concentration within the chamber headspace. NEE was measured with transparent chamber and ER was measured with opaque chamber (hood) that is used to calculate GPP (GPP = NEE + ER)"
davidson.16.monthly$instrumentation <- "LGRTM, Ultraportable Greenhouse Gas Analyser (UGGA), Model 915-0011 (Los Gatos, Research, Palo Alto, CA, USA)"
davidson.16.monthly$diurnal_coverage <- "Day"
davidson.16.monthly <- davidson.16.monthly %>% 
  mutate(site_name = case_when(site_reference %in% c("BEO_flat_centre","BEO_high_centre", "BEO_low_centre",
                                                     "BEO_rim", "BEO_trough")~ "Barrow-BEO",
                               site_reference %in% "BES_drained_lake"~ "Barrow-BES",
                               site_reference %in% c("IVO_plateau_hollow", "IVO_plateau_inter_tussock",
                                                     "IVO_plateau_tussock","IVO_wetland")~ "Ivotuk",
                               site_reference %in% c("ATQ_pool", "ATQ_ridge_inter_tussock",
                                                     "ATQ_ridge_tussock")~ "Atqasuk"))
davidson.16.monthly <- davidson.16.monthly %>% 
  mutate(latitude= case_when(site_name %in% "Barrow-BEO"~ 71.281111,
                             site_name %in% "Barrow-BES"~ 71.281003,
                             site_name %in% "Ivotuk"~ 68.49,
                             site_name %in% "Atqasuk"~ 70.477778),
         longitude= case_when(site_name %in% "Barrow-BEO"~ -156.612222,
                             site_name %in% "Barrow-BES"~ -156.612344,
                             site_name %in% "Ivotuk"~ -155.74,
                             site_name %in% "Atqasuk"~ -157.418056))
davidson.16.monthly <- davidson.16.monthly %>% 
  mutate(soil_type_detail= case_when(site_name %in% c("Barrow-BEO","Barrow-BES")~ "Soils within the Barrow field sites are classified as Gelisols with three suborders (Turbels, 77%, Orthels, 8.7% and organic soils, 1% underlain by permafrost) within 100 cm of the surface",
                                     site_name %in% "Atqasuk"~ "Soils are approximately 95% sand and 5% clay and silt to a depth of 1 m (Walker and others 1989), silt loam- textured mineral material and underlying permafrost",
                                     site_name %in% "Ivotuk"~ "classified as mostly Ruptic Pergelic and Cryaquept acid (Edwards and others 2000)"))
davidson.16.monthly <- davidson.16.monthly %>% 
  mutate(soil_depth= case_when(site_name %in% c("Barrow-BEO","Barrow-BES")~ "between 0 and more than 30 cm",
                                     site_name %in% "Atqasuk"~ "between 0 and 19 cm,",
                                     site_name %in% "Ivotuk"~ "between 4 and more than 30 cm"))
davidson.16.monthly <- davidson.16.monthly %>% 
  mutate(permafrost= case_when(site_name %in% c("Barrow-BEO","Barrow-BES")~ "Unknown",
                               site_name %in% "Atqasuk"~ "Yes",
                               site_name %in% "Ivotuk"~ "No"))
davidson.16.monthly <- davidson.16.monthly %>% 
  mutate(landform= case_when(site_name %in% "Barrow-BEO"~ "Well-developed high-centre, flat- centre and low-centre polygons",
                             site_name %in% "Barrow-BES"~ "Drained thaw lake basin with a modest development of low centre polygon",
                             site_name %in% "Atqasuk"~ "Well-developed, low-centred, ice- wedge polygons with well-drained rims",
                             site_name %in% "Ivotuk"~ "Lacks substantial ice-wedge polygon development and comprises a gentle north-west facing slope and a lower lying wet meadow on the margins of a stream"))
davidson.16.monthly <- davidson.16.monthly %>% 
  mutate(dec_shrub = case_when(site_reference %in% c("ATQ_ridge_tussock", "ATQ_ridge_inter_tussock",
                               "IVO_plateau_inter_tussock", "IVO_wetland")~ "Present",
                               site_reference %in% c("BEO_high_centre", "BEO_flat_centre", "BEO_low_centre",
                               "BEO_rim","BEO_trough", "BES_drained_lake","IVO_plateau_hollow",
                               "IVO_plateau_tussock", "ATQ_pool")~ "Absent"))
davidson.16.monthly <- davidson.16.monthly %>% 
  mutate(ev_shrub = case_when(site_reference %in% c("ATQ_ridge_tussock", "ATQ_ridge_inter_tussock",
                              "IVO_plateau_tussock","IVO_plateau_inter_tussock")~ "Present",
                              site_reference %in% c("BEO_flat_centre","BEO_high_centre", "BEO_low_centre",
                              "BEO_rim","BEO_trough" ,"BES_drained_lake", "IVO_plateau_hollow",
                               "IVO_wetland","ATQ_pool")~ "Absent"))
davidson.16.monthly <- davidson.16.monthly %>% 
  mutate(sedge = case_when(site_reference %in% c( "BEO_rim","BEO_flat_centre","BEO_low_centre",
                           "BEO_trough", "BES_drained_lake" ,"ATQ_ridge_tussock" ,"ATQ_pool",
                           "IVO_plateau_inter_tussock","IVO_plateau_tussock","IVO_wetland") ~ "Present",
                           site_reference %in% c("BEO_high_centre", "IVO_plateau_hollow",
                           "ATQ_ridge_inter_tussock")~ "Absent"))
davidson.16.monthly <- davidson.16.monthly %>% 
  mutate(non_sedge_herbaceous = case_when(site_reference %in% c("BEO_high_centre","BEO_rim",
                                          "BEO_flat_centre", "BEO_trough" )~ "Present",
                                         site_reference %in% c("BEO_low_centre","BES_drained_lake" ,"IVO_plateau_hollow",
                                          "IVO_plateau_inter_tussock","IVO_plateau_tussock", "IVO_wetland" ,"ATQ_pool", 
                                           "ATQ_ridge_inter_tussock", "ATQ_ridge_tussock")~ "Absent" ))
davidson.16.monthly <- davidson.16.monthly %>% 
  mutate(sphagnum_cover = case_when(site_reference %in% c("BEO_low_centre","BEO_trough" ,"ATQ_ridge_inter_tussock",
                                    "IVO_plateau_inter_tussock","IVO_plateau_hollow","IVO_wetland")~ "Present",
                                    site_reference %in% c("BEO_high_centre","BEO_flat_centre","BEO_rim",
                                    "IVO_plateau_tussock","ATQ_pool", "ATQ_ridge_tussock") ~ "Absent",
                                    site_reference %in% "BES_drained_lake" ~ "Dominant"))
davidson.16.monthly <- davidson.16.monthly %>% 
  mutate(other_moss_cover = case_when(site_reference %in% c("BEO_high_centre","BEO_flat_centre","BEO_low_centre",
                                      "BEO_trough", "ATQ_ridge_tussock", "ATQ_ridge_inter_tussock","IVO_plateau_tussock",
                                      "IVO_plateau_inter_tussock","IVO_plateau_hollow", "IVO_wetland") ~ "Present",
                                      site_reference %in% c("ATQ_pool","BES_drained_lake" ,"BEO_rim")~ "Absent"))
davidson.16.monthly <- davidson.16.monthly %>% 
  mutate(land_cover_bawld = case_when(site_reference %in% "BEO_flat_centre"~ "UpTundra",
                                site_reference %in% c("BEO_high_centre","BEO_rim", "IVO_plateau_hollow",
                                                      "IVO_plateau_tussock","ATQ_ridge_inter_tussock",
                                                      "ATQ_ridge_tussock","IVO_plateau_inter_tussock" ) ~ "PermBog",
                                site_reference %in% c("BEO_low_centre", "BEO_trough", "BES_drained_lake",
                                                      "IVO_wetland") ~ "PermWet",
                                site_reference %in% "ATQ_pool"~ "Marshes"))
davidson.16.monthly <- davidson.16.monthly %>% 
  mutate(land_cover= case_when(site_name %in% "Barrow-BEO"~ "",
                            site_name %in% "Barrow-BES"~ "",
                            site_name %in% "Atqasuk"~ "",
                            site_name %in% "Ivotuk"~ ""))
davidson.16.monthly$site_id <- paste("Davidson_",davidson.16.monthly$site_reference,"_agg", sep = "")
davidson.16.monthly <- davidson.16.monthly   %>%
  mutate(chamber_nr_measurement_days_ch4= chamber_nr_measurement_days) %>%
  dplyr::rename("chamber_nr_measurement_days_co2"= "chamber_nr_measurement_days")

#####Processing davidson.19####
davidson.19 <- read_excel("Davidson/Davidson et al. 2019 Biogeosciences_data.xlsx", sheet= 2)
davidson.19.monthly <- davidson.19 %>% 
  mutate(year= year(as.Date(Date)),
         month= month(as.Date(Date)),
         day= day(as.Date(Date)),
         site_reference= paste(Microform, Burn, sep="_")) %>%
  group_by(year, month, site_reference, Site,`Peatland type` ) %>%
  dplyr::summarise(ch4_flux_total= mean(`mg CH4 m2 d1`, na.rm = TRUE),
            water_table_depth= mean(`Water table (cm) below ground surface`, na.rm = TRUE),
            chamber_nr_measurement_days_ch4= n_distinct(day))

#change units
davidson.19.monthly$ch4_flux_total <- davidson.19.monthly$ch4_flux_total /1000* days_in_month(as.yearmon(paste(davidson.19.monthly$year,davidson.19.monthly$month,sep = '-')))
#rename columns
davidson.19.monthly <- davidson.19.monthly %>% dplyr::rename('site_name'='Site',
                                                             'veg_detail'='Peatland type')
#adding in static info
davidson.19.monthly$site_name <- "Poplar Fen"
davidson.19.monthly$latitude <- "56.938833"
davidson.19.monthly$longitude <- "-111.5489"
davidson.19.monthly$site_id <- paste("Davidson_PoplarFen", davidson.19.monthly$site_reference, "agg", sep = "_")
davidson.19.monthly$email <- "scott.davidson@plymouth.ac.uk"
davidson.19.monthly$data_contributor_or_author <- "Scott Davidson"
davidson.19.monthly$citation <- "https://doi.org/10.5194/bg-16-2651-2019"
davidson.19.monthly$country <- "Canada"
davidson.19.monthly$biome <- "Boreal"
davidson.19.monthly$veg_detail <- "This treed fen is dominated by Larix laricina (Du Roi) K.Koch, Picea mariana (Mill.) Britton, Betula pumila (L.), Equisetum fluviatile (L.), Smilacina trifolia (L.) Sloboda, Carex spp., and Sphagnum fuscum (Schimp.) Klinggr and brown mosses, largely Tomenthypnum nitens (Hedwig) Loeske"
davidson.19.monthly$dec_shrub <- "Present"
davidson.19.monthly$sedge <- "Present"
davidson.19.monthly$ev_needle_tree <- "Present"
davidson.19.monthly$dec_needle_tree <- "Present"
davidson.19.monthly$sphagnum_cover <- "Present"
davidson.19.monthly$other_moss_cover <- "Present"
davidson.19.monthly$gap_fill <- "Average"
davidson.19.monthly$flux_method <- "Chamber"
davidson.19.monthly$flux_method_detail <- "Closed chambers"
davidson.19.monthly$flux_method_description <- "A cylindrical opaque chamber (20 cm × 50 cm) was placed on the collar, with water poured around the collar edge to create a seal. A battery-powered fan was used to mix the chamber headspace. A thermocouple located within the chamber, attached to a thermometer, was used to measure temperature during sampling. A 20 mL syringe was used to collect gas samples at intervals of 7, 15, 25, and 35 min following chamber closure and injected into Exetainers (Labco, UK). A gas chromatograph with a flame ionization detector (250 ◦ C), helium gas carrier, and standards of 5 and 50 ppm was used to determine CH4 concentration"
davidson.19.monthly$instrumentation <- "Shimadzu GC2014, Mandel Scientific"
davidson.19.monthly$diurnal_coverage <- "Day"
davidson.19.monthly$land_cover_bawld <- "Fen"
davidson.19.monthly$land_cover <- "170"

#####Processing davidson.21####
davidson.21 <- read_excel("Davidson/Davidson_et al. 2021 JGR Biogeosciences.xlsx", sheet= 2)
davidson.21 <- davidson.21 %>% mutate(year= year(as.Date(Date, format= "%m/%d/%Y")),
                                      month= month(as.Date(Date, format= "%m/%d/%Y")),
                                      day= day(as.Date(Date, format= "%m/%d/%Y")),
                                      site_reference= paste(Peatland, Position, Veg_type, sep='_'))
davidson.21.monthly <- davidson.21 %>% group_by(Site, site_reference, year, month ) %>%
  group_by(year, month, Site, site_reference, Veg_type ) %>%
  dplyr::summarise(gpp= mean(GPP, na.rm=TRUE),
            chamber_nr_measurement_days_co2= n_distinct(day))
#change unit
davidson.21.monthly$gpp <- davidson.21.monthly$gpp *days_in_month(as.yearmon(paste(davidson.21.monthly$year,davidson.21.monthly$month,sep = '-')))
#rename
davidson.21.monthly <- davidson.21.monthly %>% dplyr::rename('site_name'= "Site")
#adding in static info
davidson.21.monthly$site_id <- paste("Davidson", davidson.21.monthly$site_name, davidson.21.monthly$site_reference, "agg", sep="_")
davidson.21.monthly$data_contributor_or_author <- "Scott Davidson"
davidson.21.monthly$email <- "scott.davidson@plymouth.ac.uk"
davidson.21.monthly$citation <- "https://doi.org/10.1029/2021JG006403"
davidson.21.monthly$country <- "Canada"
davidson.21.monthly$biome <- "Boreal"
davidson.21.monthly$gap_fill <- "Average"
davidson.21.monthly$flux_method <- "Chamber"
davidson.21.monthly$flux_method_detail <- "Closed dynamic chamber method"
davidson.21.monthly$flux_method_description <- "A clear acrylic chamber (60 × 60 × 30 cm) was placed on a stainless-steel collar (60 × 60 cm).The concentration of CO2 (ppm) was determined inside the chamber at 15 s intervals for a maximum of 2.5 min. The linear change in CO2 concentration over time was used to calcu- late net ecosystem exchange (NEE; g CO2 m2 d−1). Ecosystem respiration (ER; g CO2 m2 d−1) was determined by darkening the chamber with an opaque cloth shroud. Gross primary production (GPP; g CO2 m2 d−1) was calculated as the difference between NEE and ER."
davidson.21.monthly$instrumentation <- "portable infrared gas analyzer (EGM-4, PP systems)"
davidson.21.monthly$diurnal_coverage <- "Day"
davidson.21.monthly$land_cover <- "170"
davidson.21.monthly <- davidson.21.monthly %>% 
  mutate(latitude= case_when(site_name %in% c("Carmon Creek", "Carmon Creek 2")~ "56.36222",
                             site_name %in% "IPAD"~ "56.397561"),
         longitude= case_when(site_name %in% c("Carmon Creek", "Carmon Creek 2")~ "-116.795833",
                             site_name %in% "IPAD"~ "-116.891"))
davidson.21.monthly <- davidson.21.monthly %>% 
  mutate(disturbance= case_when(site_name %in% c("Carmon Creek", "Carmon Creek 2")~ "Seismic lines- approximately 3 m wide and run both north to south and east to west",
                                site_name %in% "IPAD"~ "Seismic lines- approximately 7 m wide, running both southwest to northeast and east to west"))
davidson.21.monthly <- davidson.21.monthly %>% 
  mutate(disturb_year = case_when(site_name %in% c("Carmon Creek", "Carmon Creek 2")~ "~2003",
                                site_name %in% "IPAD"~ "~2019"))
davidson.21.monthly <- davidson.21.monthly %>% 
  mutate(land_cover_bawld = case_when(site_name %in% c("Carmon Creek", "Carmon Creek 2")~ "Bog",
                                  site_name %in% "IPAD"~ "Fen"))
davidson.21.monthly <- davidson.21.monthly %>% 
  mutate(veg_detail2 = case_when(site_name %in% c("Carmon Creek", "Carmon Creek 2")~ "Treed bog",
                                site_name %in% "IPAD"~"Treed poor fen")) %>%
  mutate(veg_detail= paste(veg_detail2, Veg_type), veg_detail2 = NULL, Veg_type= NULL)
davidson.21.monthly <- davidson.21.monthly %>% 
  mutate(ev_shrub = case_when(veg_detail %in% c("Treed bog Lichen/Rhododendron", "Treed bog Sphagnum/Rhododendron",
                              "Treed poor fen Feathermoss/Rhododendron","Treed bog Feathermoss/Rhododendron",
                              "Treed bog Rhododendron")~ "Present"))
davidson.21.monthly <- davidson.21.monthly %>% 
  mutate(sphagnum_cover = case_when(veg_detail %in% c("Treed bog Sphagnum/Rhododendron","Treed bog Sphagnum")~ "Present"))
davidson.21.monthly <- davidson.21.monthly %>% 
  mutate(other_moss_cover = case_when(veg_detail %in% c("Treed poor fen Feathermoss/Rhododendron","Treed bog Feathermoss/Rhododendron",
                                                        "Treed poor fen Feathermoss", "Treed poor fen Feathermoss/Salix")~ "Present"))
davidson.21.monthly <- davidson.21.monthly %>% 
  mutate(sedge = case_when(veg_detail %in% c("Treed poor fen Carex/Salix","Treed poor fen Carex" )~ "Present"))
davidson.21.monthly <- davidson.21.monthly %>% 
  mutate(dec_broad_tree = case_when(veg_detail %in% c("Treed poor fen Carex/Salix", "Treed poor fen Feathermoss/Salix")~ "Present"))

#####Processing Dobosy_Deadhorse_NOAA-ATDD_tower_1_Kochnendorfer#####---------------------------------------------------
dobosy.all <- read_csv("original files/CO2flux_ADC_Synthesis_Metadata_TOWER_201809_20181005_timeData.csv")
dobosy <- dobosy.all %>% dplyr::filter(Study_ID %in% "Dobosy_Deadhorse_NOAA-ATDD_tower_1_Kochnendorfer") %>%
  group_by(Meas_year, End_month_meas) %>%
  reframe(nee= sum(as.numeric(NEE_gC_m2), na.rm = TRUE),
                   gap_fill_perc_nee= mean(as.numeric(`Gap_%`), na.rm = TRUE),
                   tair= mean(as.numeric(Tair_int_C), na.rm = TRUE),
                   tsoil_surface= mean(as.numeric(Tsoil_C), na.rm = TRUE),
                   soil_moisture= mean(as.numeric(`Soil_moisture_%`), na.rm = TRUE),
                   ppfd= mean(as.numeric(PAR_W_m2), na.rm = TRUE))

dobosy <- dobosy %>% dplyr::rename("year"= "Meas_year", "month"= "End_month_meas")
dobosy$diurnal_coverage <- "Day and Night"
dobosy$gap_fill <- "MDS"
dobosy$citation <- "Dobosy; R.; D. et al.: Estimating Random Uncertainty in Airborne Flux Measurements over Alaskan Tundra: Update on the Flux Fragment Method. https://doi.org/10.1175/JTECH-D-16-0187.1"
dobosy$site_name <- "NOAA-ATDD; Deadhorse"
dobosy$data_contributor_or_author <- "John Kochendorfer"
dobosy$email <- "john.kochendorfer@noaa.gov"
dobosy$country <- "USA"
dobosy$biome <- "Tundra"
dobosy$latitude <- "70.086"
dobosy$longitude <- "-148.57"
dobosy$flux_method <- "EC"
dobosy$flux_method_detail <- "EC_closed"
dobosy$veg_detail <- "Wet sedge"
dobosy$thaw_depth <- "20 - 50 cm"


#####Torbern Tagesson####
#tower data
sheet <- excel_sheets("Torbern Tagesson/Flux_data_tower_2008_2009.xlsx") 
tag.ec  <- bind_rows(lapply(setNames(sheet, sheet),  
                    function(x) read_excel("Torbern Tagesson/Flux_data_tower_2008_2009.xlsx", sheet=x, na=c("NA","9999"))) )
tag.ec.monthly <- tag.ec %>%
  mutate(year= year(as.Date(Time) ), month= month(as.Date(Time)), day= day(as.Date(Time))) %>%
  group_by(year, month) %>%
  dplyr::summarise( ch4_flux_total= mean(`GAP FILLED CH4 FLUX (mg CH4 m-2 h-1)`, na.rm= T),
                    gpp= mean(`GAPfilled GPP (umol CO2 m-2 s-1)`, na.rm= T),
                    reco= mean(`GAPFILLED ER (umol CO2 m-2 s-1)`, na.rm= T))
#convert units
tag.ec.monthly$gpp <- tag.ec.monthly$gpp*1.0368*days_in_month(as.yearmon(paste(tag.ec.monthly$year, tag.ec.monthly$month,sep = '-')))
tag.ec.monthly$reco <- tag.ec.monthly$reco*1.0368*days_in_month(as.yearmon(paste(tag.ec.monthly$year, tag.ec.monthly$month,sep = '-')))
tag.ec.monthly$ch4_flux_total <- tag.ec.monthly$ch4_flux_total/1000/16.04*12.01*24*days_in_month(as.yearmon(paste(tag.ec.monthly$year, tag.ec.monthly$month,sep = '-')))
#calc nee
tag.ec.monthly$nee <- tag.ec.monthly$reco + tag.ec.monthly$gpp
#meteo data
tag.meteo.07 <- read_excel("Torbern Tagesson/Zackenberg_klimatdatabase.xlsx", sheet = 1) %>%
  dplyr::rename("year"= "Year", "month"= "Month", "tair"= "AirTemp(200cm)(C)...9", "precip"= "Precipitation(150cm)(mm)...26",
                "par"= "PAR(200cm)(my-mol*m-2s-1)...32", "snow_depth"= "Snow depth") %>%
  dplyr::select(year, month, tair, precip, par, snow_depth)
tag.meteo.08 <- read_excel("Torbern Tagesson/Zackenberg_klimatdatabase.xlsx", sheet = 2) %>%
  dplyr::rename("year"= "Year", "month"= "Month", "tair"= "AirTemp(200cm)(C)...9", "precip"= "Precipitation(150cm)(mm)...26",
                "par"= "PAR(200cm)(my-mol*m-2s-1)...32", "snow_depth"= "Snow depth") %>%
  dplyr::select(year, month, tair, precip, par, snow_depth)
tag.meteo.09 <- read_excel("Torbern Tagesson/Zackenberg_klimatdatabase.xlsx", sheet = 3)  %>%
  mutate(month= month(as.Date(DateTime)), year= year(as.Date(DateTime)))%>%
  dplyr::rename("tsoil_0"= "Soil temp 0cm_°C", "tsoil_5" = "Soil temp 5cm_°C", "tsoil_20"= "Soil temp 20cm_°C",
                "tsoil_60" = "Soil temp 60cm_°C", "tsoil_100" = "Soil temp 100cm_°C", "tair"= "Air temp_°C", "precip"= "Precipitation Pluvio") %>%
  dplyr::select(year, month, tsoil_0, tsoil_5, tsoil_20, tsoil_60, tsoil_100, tair, precip)
tag.meteo <- full_join(tag.meteo.07, tag.meteo.08) %>% full_join(tag.meteo.09)
tag.meteo.monthly <- tag.meteo %>% group_by(year, month) %>%
  dplyr::summarise(tair= mean(tair, na.rm = T),
                   precip= max(precip, na.rm = T),
                   ppfd= mean(par, na.rm = T),
                   snow_depth= mean(snow_depth, na.rm = T),
                   tsoil_surface= mean(c(tsoil_0, tsoil_5), na.rm = T),
                   tsoil_deep= mean(c(tsoil_20, tsoil_60, tsoil_100), na.rm = T))
tagesson.ec <- left_join(tag.ec.monthly, tag.meteo.monthly)
tagesson.ec$tsoil_surface_depth <- "2.5"
tagesson.ec$tsoil_deep_depth <- "60"
tagesson.ec$site_name <- "Rylekaerene"
tagesson.ec$data_contributor_or_author <- "Torbern Tagesson"
tagesson.ec$email <- "torbern.tagesson@nateko.lu.se"
tagesson.ec$citation <- "Tagesson, T., Mölder, M., Mastepanov, M., Sigsgaard, C., Tamstorf, M.P., Lund, M., . . . Ström, L. (2012) Land-atmosphere exchange of methane from soil thawing to soil freezing in a high-Arctic wet tundra ecosystem. Global Change Biology, 18, 1928–1940."
tagesson.ec$country <- "Greenland"
tagesson.ec$latitude <- "74.481511"
tagesson.ec$longitude <- "-20.555689"
tagesson.ec$veg_detail <- "High-Arctic heterogeneous wetland area. It is a patterned fen characterized by alternating high, dry heath areas, and low, wet fen areas"
tagesson.ec$flux_method <- "EC"
tagesson.ec$flux_method_detail <- "CO2: EC_open CH4: Combination of the gradient and eddy covariance methods"
tagesson.ec$flux_method_description <- "3-axis sonic anemometer (Metek, Gmbh, Elmshorn,Germany), and an open-path CO2/H2O infrared gas analyzer was installed at 3.3 m above the surface. The gas analyzer was tilted 32°from vertical next to the sonic anemometer. CH4 fluxes were thus estimated by combining gradient and EC methods. The CH4 concentrations were measured at two levels (0.70 and 2.75 m) on the tower at 1 Hz rate. The system consisted of a laser off-axis integrated cavity output spectroscopy analyzer (LGR; DLT200,Fast Methane Analyzer, repeatability 1 ppb at 0.1 Hz, Los Gatos Research, Mountain View, CA, USA"
tagesson.ec$instrumentation <- "LI-7500"
tagesson.ec$permafrost <- "Yes"
tagesson.ec$biome <- "Tundra"
tagesson.ec$non_sedge_herbaceous <- "Present"
tagesson.ec$ev_shrub <- "Present"
tagesson.ec$dec_shrub <- "Present"
tagesson.ec$sedge <- "Present"
tagesson.ec$other_moss_cover <- "Present"
tagesson.ec$alt <- "40-80"
tagesson.ec$soil_depth <- "20-30"
tagesson.ec$soil_ph <- "6.9"
tagesson.ec$tair_height <- "3"
tagesson.ec$notes <- "NEE values calculated from gapfilled ER and GPP measurements (NEE= GPP+RECO)"
tagesson.ec$land_cover <- "180"
tagesson.ec$land_cover_bawld <- "Wet Tundra"
  
#chamber 
tag.ch <- dplyr::bind_rows(import_list("Torbern Tagesson/Chamber_fluxes_Tagesson_2007.xls"))
tag.ch <- tag.ch %>% mutate(year= year(as.Date(Time, format= "%d.%m.%Y %H:%M:%S")),
                            month= month(as.Date(Time, format= "%d.%m.%Y %H:%M:%S")),
                            day= day(as.Date(Time, format= "%d.%m.%Y %H:%M:%S")))
#remove empty rows
tag.ch <- tag.ch %>% dplyr::filter(!if_all("year", ~ is.na(.)))
#need to convert coordinate units
library(sf)
utm_data <- st_as_sf(tag.ch, coords = c("East Coordiante (UTM27x)", "North Coordiante (UTM27x)"), crs = 32627)  # Assuming UTM zone 27
decimal_degrees <- st_transform(utm_data, 4326)  # EPSG code for WGS84 (decimal degrees)
# Add the result to the original data.frame
tag.ch$latitude <- st_coordinates(decimal_degrees)[, 2]
tag.ch$longitude <- st_coordinates(decimal_degrees)[, 1]
#fix chamber names
tag.ch <- tag.ch %>%
  mutate(Chamber = gsub("CF", "Continuous fen ", Chamber)) %>%
  mutate(Chamber = gsub("HF", "Hummocky fen ", Chamber)) %>%
  mutate(Chamber = gsub("G", "Grassland ", Chamber)) %>%
  mutate(Chamber = gsub("CAS", "Cassiope heath ", Chamber)) %>%
  mutate(Chamber = gsub("Dry", "Dryas heath ", Chamber)) %>%
  mutate(Chamber = gsub("Sax", "Salix snowbed ", Chamber)) %>%
  mutate(Chamber = gsub("Vac", "Vaccinium heath ", Chamber))
tag.ch <- tag.ch %>% dplyr::rename("site_reference"= "Chamber")
  
## summarize by month
tag.ch.monthly <- tag.ch %>% 
  group_by(site_reference, latitude, longitude, year, month) %>%
  dplyr::summarise(tsoil_surface= mean(`Soil temp 10 cm`, na.rm = T),
                   alt= mean(`Active layer`, na.rm = T),
                   water_table_depth= mean(`Wt depth`, na.rm = T),
                   ppfd= mean(PAR, na.rm = T),
                   ch4_flux_total= mean(`CH4 flux dark (mg CH4 m−2 h−1)`, na.rm = T),
                   nee= mean(`NEE (mg CO2 m−2 h−1)`, na.rm = T),
                   reco= mean(`RESP (mg CO2 m−2 h−1)`, na.rm = T),
                   gpp= mean(`GPP (mg CO2 m−2 h−1)`, na.rm = T),
                   tair= mean(`AirTemp(200cm)(C)`, na.rm = T),
                   chamber_nr_measurement_days= n_distinct(day)) %>%
  mutate(chamber_nr_measurement_days_ch4= chamber_nr_measurement_days) %>%
  dplyr::rename("chamber_nr_measurement_days_co2"= "chamber_nr_measurement_days")

tag.ch.monthly$ch4_flux_total<- tag.ch.monthly$ch4_flux_total/1000/16.04*12.01*24*days_in_month(as.yearmon(paste(tag.ch.monthly$year, tag.ch.monthly$month,sep = '-')))
tag.ch.monthly$nee<- tag.ch.monthly$nee/1000/44.01*12.01*24*days_in_month(as.yearmon(paste(tag.ch.monthly$year, tag.ch.monthly$month,sep = '-')))
tag.ch.monthly$gpp<- tag.ch.monthly$gpp/1000/44.01*12.01*24*days_in_month(as.yearmon(paste(tag.ch.monthly$year, tag.ch.monthly$month,sep = '-')))
tag.ch.monthly$reco<- tag.ch.monthly$reco/1000/44.01*12.01*24*days_in_month(as.yearmon(paste(tag.ch.monthly$year, tag.ch.monthly$month,sep = '-')))
#Adding in static info
tag.ch.monthly$site_name <- "Rylekaerene"
tag.ch.monthly$site_id <- paste("Tagesson_Rylekaerene_", tag.ch.monthly$site_reference, sep = "")
tag.ch.monthly$data_contributor_or_author <- "Torbern Tagesson; Department of Physical Geography and Ecosystems Science, Lund University, Sölvegatan 12, 223 62, Lund, Sweden,"
tag.ch.monthly$email <- "torbern.tagesson@nateko.lu.se"
tag.ch.monthly$citation <- "CO2: Torbern Tagesson et al. (2012) High-resolution satellite data reveal an increase in peak growing season gross primary production in a high-Arctic wet tundra ecosystem 1992-2008. International Journal of Applied Earth Observation and Geoinformation, 18, 407-416. CH4: Torbern Tagesson et al. (2013) Modelling of growing season methane fluxes in a high-Arctic wet tundra ecosystem 1997–2010 using in situ and high-resolution satellite data, Tellus B: Chemical and Physical Meteorology, 65:1, DOI: 10.3402/tellusb.v65i0.19722"
tag.ch.monthly$veg_detail <- "High-Arctic heterogeneous wetland area. It is a patterned fen characterized by alternating high, dry heath areas, and low, wet fen areas"
tag.ch.monthly$permafrost <- "Yes"
tag.ch.monthly$country <- "Greenland"
tag.ch.monthly$biome <- "Tundra"
tag.ch.monthly$alt <- "50- 100"
tag.ch.monthly$tsoil_surface_depth <- "10"
tag.ch.monthly$flux_method <- "Chamber"
tag.ch.monthly$flux_method_detail <- "Closed chamber"
tag.ch.monthly$flux_method_description <- "The chamber was a transparent Plexiglas cube of 0.3 m height and a ground area of 0.34 m2. The outlet and inlet for gases were located on one of the chamber sides, 0.15 and 0.25 m above the ground, respectively. Two small fans were located in the upper part of the chamber to ensure proper mixing of the chamber headspace and representative sampling"
tag.ch.monthly$instrumentation <- "CO2: EGM-4, PP-systems, Hitchin, Hertfordshire, UK CH4: LGR, DLT200 Fast Methane Analyzer, Los Gatos Research,USA"
tag.ch.monthly$diurnal_coverage <- "Day"
tag.ch.monthly$gap_fill <- "Average"
tag.ch.monthly$land_cover_bawld <- "Wet Tundra"
tag.ch.monthly <- tag.ch.monthly %>%
  mutate(land_cover= case_when( startsWith(site_reference, "Grassland") ~ "130",
                                startsWith(site_reference, "Continuous fen")~ "180",
                                startsWith(site_reference, "Hummocky fen")~ "180",
                                startsWith(site_reference, "Cassiope heath")~ "120",
                                startsWith(site_reference, "Dryas heath")~"120",
                                startsWith(site_reference,"Salix snowbed")~"120",
                                startsWith(site_reference,"Vaccinium heath")~"120"))
tag.ch.monthly <- tag.ch.monthly %>%
  mutate(sedge= case_when(startsWith(site_reference, "Continuous fen")~ "Dominant",
                          startsWith(site_reference, "Hummocky fen")~ "Dominant"))
tag.ch.monthly <- tag.ch.monthly %>%
  mutate(non_sedge_herbaceous= case_when(startsWith(site_reference, "Continuous fen")~ "Dominant",
                          startsWith(site_reference, "Hummocky fen")~ "Dominant",
                          startsWith(site_reference, "Grassland") ~ "Dominant"))
tag.ch.monthly <- tag.ch.monthly %>%
  mutate(ev_shrub= case_when(startsWith(site_reference, "Cassiope heath")~ "Dominant",
                             startsWith(site_reference, "Dryas heath")~"Dominant"))
tag.ch.monthly <- tag.ch.monthly %>%
  mutate(dec_shrub= case_when(startsWith(site_reference, "Hummocky fen")~ "Dominant",
                              startsWith(site_reference,"Salix snowbed")~"Dominant",
                              startsWith(site_reference,"Vaccinium heath")~"Dominant"))
tag.ch.monthly$other_moss_cover <- "Present"


tagesson.ch <- tag.ch.monthly
#####Margaret Torn/ Sigrid Dengel NGEE####
#Soil CO2 and CH4 Chamber Fluxes in Tussock Tundra, Council Road Mile Marker 71, Seward Peninsula, Alaska, 2016-2019
chafe.meteo <- read_csv("NGEEdata/CN_MM71_ chamber_soil_temp_moist_thaw_20210120.csv", na=c("NA","-9999", "NaN"))
chafe.flux.lgr <- read_csv("NGEEdata/CN_MM71_soil_chamber_fluxes_LGR_20210120.csv", na=c("NA","-9999", "NaN"))
chafe.flux.pic <- read_csv("NGEEdata/CN_MM71_soil_chamber_fluxes_Picarro_20230227.csv", na=c("NA","-9999", "NaN"))
#merge data
chafe.flux  <- chafe.flux.lgr %>% full_join(chafe.flux.pic)
chafe.meteo <- chafe.meteo %>% mutate(measurement_date= as.Date(measurement_date, format= "%Y-%m-%d"),
                                    time= as.POSIXct(time, format= "%H:%M"))
chafe.flux <- chafe.flux %>% mutate(measurement_date= as.Date(measurement_date, format= "%Y-%m-%d"),
                                    time= as.POSIXct(time, format= "%H:%M"))
chafe <- chafe.flux %>% full_join(chafe.meteo)
#add year and month
chafe <- chafe %>% mutate(year= year(measurement_date), month= month(measurement_date), day= day(measurement_date))
#rename
chafe <- chafe %>% dplyr::rename("site_reference"= "plot_ID")
#If opaque chamber, flux= reco, if clear flux = nee
chafe <- chafe %>% mutate(reco= case_when(chamber_type== "Opq"~ flux_CO2)) %>%
                 mutate(nee= case_when(chamber_type== "Trns"~ flux_CO2)) 
#summary by month
chafe.monthly <- chafe %>% group_by(year, month, site_reference, latitude, longitude, landscape_position) %>%
  reframe(nee= mean(as.numeric(nee), na.rm = TRUE),
          reco= mean(as.numeric(reco), na.rm = TRUE),
          ch4_flux_total= mean(as.numeric(flux_CH4), na.rm = TRUE),
          water_table_depth= -mean(as.numeric(standing_water_depth), na.rm = TRUE),
          tsoil_surface= mean(as.numeric(soil_temp_10_cm), na.rm = TRUE),
          tsoil_deep= mean(as.numeric(c(soil_temp_15_cm, soil_temp_20_cm)), na.rm = TRUE),
          tair= mean(as.numeric(air_temp), na.rm = TRUE),
          thaw_depth= mean(as.numeric(thawdepth), na.rm = TRUE),
          soil_moisture= mean(as.numeric(VWC), na.rm = TRUE),
          chamber_nr_measurement_days= n_distinct(day)) %>%
  mutate(chamber_nr_measurement_days_ch4= chamber_nr_measurement_days) %>%
  dplyr::rename("chamber_nr_measurement_days_co2"= "chamber_nr_measurement_days")

#convert units
chafe.monthly$nee<- chafe.monthly$nee*1.0368*days_in_month(as.yearmon(paste(chafe.monthly$year, chafe.monthly$month,sep = '-')))
chafe.monthly$reco<- chafe.monthly$reco*1.0368*days_in_month(as.yearmon(paste(chafe.monthly$year, chafe.monthly$month,sep = '-')))
chafe.monthly$gpp<- chafe.monthly$nee- chafe.monthly$reco
chafe.monthly$ch4_flux_total<- chafe.monthly$ch4_flux_total*0.0010368*days_in_month(as.yearmon(paste(chafe.monthly$year, chafe.monthly$month,sep = '-')))


#adding static info
chafe.monthly$moisture_depth <- "20"
chafe.monthly$site_name <- "Council Road Mile Marker 71, Seward Peninsula"
chafe.monthly$site_id <- paste("Chafe_SewardPeninsula_",chafe.monthly$site_reference, sep= "")
chafe.monthly$country <- "USA"
chafe.monthly$biome <- "Tundra"
chafe.monthly$data_contributor_or_author <- "Oriana Chafe, Ian Shirley, Stan Wullschleger, and Margaret Torn"
chafe.monthly$citation <-"Oriana Chafe, Ian Shirley, Stan Wullschleger, and Margaret chafe. 2023. Soil CO2 and CH4 Chamber Fluxes in Tussock Tundra, Council Road Mile Marker 71, Seward Peninsula, Alaska, 2016-2019. Next Generation Ecosystem Experiments Arctic Data Collection, Oak Ridge National Laboratory, U.S. Department of Energy, Oak Ridge, Tennessee, USA. Dataset accessed on 11/20/2023 at https://doi.org/10.5440/1765733."
chafe.monthly$gap_fill <- "Average"
chafe.monthly$partition_method <- "GPP= NEE- ER"
chafe.monthly$flux_method <- "Chamber"
chafe.monthly$flux_method_detail <- "Closed-loop chambers"
chafe.monthly$flux_method_description <- "Chambers (25 cm diameter, 15-20 cm height) were tall enough to enclose vegetation and were vented according to Xu et al. (2006), to minimize pressure excursions due to the Venturi effect. In all plots, chambers were seated on PVC bases extending ~15 cm below the soil surface. For each flux measurement, the chamber was seated in a 3 cm-deep, water-filled trench in the base's top rim to create an airtight seal."
chafe.monthly <- chafe.monthly %>% 
  mutate(instrumentation= case_when(year %in% c("2016", "2017")~ "Los Gatos Research, Inc. (LGR) Ultraportable Greenhouse Gas Analyzer",
                                    year %in% c("2018", "2019")~"Picarro G4301 Mobile Gas Concentration Analyzer"))
chafe.monthly <- chafe.monthly %>%
  mutate(land_cover= case_when(landscape_position %in% c("upland", "slope")~ "130",
                               landscape_position %in% "lowland"~ "180") )
chafe.monthly <- chafe.monthly %>%
  mutate(land_cover_bawld= case_when(landscape_position %in% c("upland", "slope")~ "Dry Tundra",
                               landscape_position== "lowland"~ "Tundra Wetland") )
chafe.monthly$landscape_position <- NULL
chafe.ch <- chafe.monthly

#CO2 and CH4 surface flux, soil profile concentrations, and stable isotope composition, Barrow, Alaska, 2012-2013
vaughn.flux <- read_csv("NGEEdata/flux_CO2_CH4_Barrow_2012_2013.csv") %>%
  dplyr::select(UTM_northing, UTM_easting, plot_ID, chamber_type, date, flux_CO2, flux_CH4)
vaughn.soil <- read_csv("NGEEdata/soil_moisture_Barrow_2012_2013.csv") %>%
  dplyr::select(UTM_northing, UTM_easting, plot_ID, date, VWC)
vaughn.temp <- read_csv("NGEEdata/temperature_profiles_Barrow_2012_2013.csv") %>%
  dplyr::select(UTM_northing, UTM_easting, plot_ID, date, soil_temp, air_temp, depth_probe)
vaughn.isotopes <- read_csv("NGEEdata/isotopes_concentratons_Barrow_2012_2013.csv") %>%
  dplyr::select(UTM_northing, UTM_easting, area, plot_ID, sample, sampletype, date, thawdepth)          
#merge dfs
vaughn <- vaughn.flux %>% full_join(vaughn.soil, by= c("UTM_northing", "UTM_easting", "plot_ID","date"))
vaughn <- vaughn %>% full_join(vaughn.temp, by= c("UTM_northing", "UTM_easting", "plot_ID","date"))
vaughn <- vaughn %>% full_join(vaughn.isotopes, by= c("UTM_northing", "UTM_easting", "plot_ID","date"))
vaughn <- vaughn %>% dplyr::filter(!is.na(UTM_northing))#removing empty rows
#add year and month
vaughn$date <- as.POSIXct(vaughn$date, format= "%Y-%m-%d")
vaughn <- vaughn %>% mutate(year= year(as.POSIXct(vaughn$date, format= "%Y-%m-%d")), 
                            month= month(as.POSIXct(vaughn$date, format= "%Y-%m-%d")), 
                            day= day(as.POSIXct(vaughn$date, format= "%Y-%m-%d")))
#rename
vaughn <- vaughn %>% dplyr::rename("site_reference"= "plot_ID")
#If opaque chamber, flux= reco, if clear flux = nee
vaughn <- vaughn %>% mutate(reco= case_when(chamber_type== "Opq"~ flux_CO2)) %>%
  mutate(nee= case_when(chamber_type== "Trns"~ flux_CO2)) 
#separate soil temps
vaughn <- vaughn %>% mutate(depth_probe= as.numeric(depth_probe)) %>%
                     mutate(tsoil_surface= case_when(depth_probe < 10 ~ soil_temp)) %>%
                     mutate(tsoil_deep= case_when(depth_probe > 10 ~ soil_temp)) %>%
                     mutate(tsoil_surface_depth= case_when(depth_probe < 10 ~ depth_probe)) %>%
                     mutate(tsoil_deep_depth= case_when(depth_probe > 10 ~ depth_probe)) 
#need to convert coordinate units
library(sf)
utm_data <- st_as_sf(vaughn, coords = c("UTM_easting", "UTM_northing"), crs = 32604)  # Assuming UTM zone 27
decimal_degrees <- st_transform(utm_data, 4326)  # EPSG code for WGS84 (decimal degrees)
# Add the result to the original data.frame
vaughn$latitude <- st_coordinates(decimal_degrees)[, 2]
vaughn$longitude <- st_coordinates(decimal_degrees)[, 1]
#summary by month
vaughn.monthly <- vaughn %>% group_by(year, month, site_reference, latitude, longitude) %>%
  reframe(nee= mean(as.numeric(nee), na.rm = TRUE),
          reco= mean(as.numeric(reco), na.rm = TRUE),
          ch4_flux_total= mean(as.numeric(flux_CH4), na.rm = TRUE),
          tsoil_surface= mean(as.numeric(tsoil_surface), na.rm = TRUE),
          tsoil_surface_depth = mean(as.numeric(tsoil_surface_depth), na.rm = TRUE),
          tsoil_deep= mean(as.numeric(tsoil_deep), na.rm = TRUE),
          tsoil_deep_depth= mean(as.numeric(tsoil_deep_depth), na.rm = TRUE),
          soil_moisture= mean(as.numeric(VWC), na.rm = TRUE),
          tair= mean(as.numeric(air_temp), na.rm = TRUE),
          thaw_depth= mean(as.numeric(thawdepth), na.rm = TRUE),
          chamber_nr_measurement_days= n_distinct(day)) %>%
  mutate(chamber_nr_measurement_days_ch4= chamber_nr_measurement_days) %>%
  dplyr::rename("chamber_nr_measurement_days_co2"= "chamber_nr_measurement_days")

#remove rows without flux data
vaughn.monthly <- vaughn.monthly %>% dplyr::filter(!if_all(c(nee, reco, ch4_flux_total), ~ is.na(.)))
#convert units
vaughn.monthly$nee<- vaughn.monthly$nee*1.0368*days_in_month(as.yearmon(paste(vaughn.monthly$year, vaughn.monthly$month,sep = '-')))
vaughn.monthly$reco<- vaughn.monthly$reco*1.0368*days_in_month(as.yearmon(paste(vaughn.monthly$year, vaughn.monthly$month,sep = '-')))
vaughn.monthly$gpp<- vaughn.monthly$nee- vaughn.monthly$reco
vaughn.monthly$ch4_flux_total<- vaughn.monthly$ch4_flux_total*0.0010368*days_in_month(as.yearmon(paste(vaughn.monthly$year, vaughn.monthly$month,sep = '-')))
#Adding in static vars
vaughn.monthly$site_name <- "Barrow, Alaska"
vaughn.monthly$site_id <- paste("Vaughn_Barrow_", vaughn.monthly$site_reference, sep= "")
vaughn.monthly$country <- "USA"
vaughn.monthly$biome <- "Tundra"
vaughn.monthly$data_contributor_or_author <- "Lydia Vaughn, Mark Conrad, Margaret Torn, Markus Bill, Bryan Curtis, Oriana Chafe"
vaughn.monthly$citation <-"Vaughn, L.S., Conrad, M.S., Torn, M.S., Bill, M., Curtis, J.B., Chafe, O. 2015. CO2 and CH4 surface fluxes, soil profile concentrations, and stable isotope composition, Barrow, Alaska, 20122013. Next Generation Ecosystem Experiments Arctic Data Collection, Oak Ridge National Laboratory, U.S. Department of Energy, Oak Ridge, Tennessee, USA. Data set accessed at DOI:10.5440/1227684."
vaughn.monthly$gap_fill <- "Average"
vaughn.monthly$partition_method <- "GPP= NEE- ER"
vaughn.monthly$flux_method <- "Chamber"
vaughn.monthly$flux_method_detail <- "Static manual chambers"
vaughn.monthly$flux_method_description <- "Opaque or transparent static chambers (25 cm diameter, 15-20 cm height). In inundated plots, a floating chamber was used whose base extended 4 cm below the water surface. In all other plots, chambers were seated on PVC bases extending ~15 cm below the soil surface."
vaughn.monthly$instrumentation <- "Los Gatos Research, Inc. (LGR) portable Greenhouse Gas Analyzer"
vaughn.monthly$permafrost <- "Yes"
vaughn.monthly$landform <- "Polygonal features"

vaughn.ch <- vaughn.monthly
########------COMBINE ALL----------------------------------------------------------------------------------------
PIdat.ec <- rbindlist(list(sonnentag, pirk.finse, pirk.iskoras, masa, boike, jong, sabrekov.ec, dyukarev.ec, roy, 
                           lopezblanco.ec, hung.ec, rautakoski, aurela.lett.ec, aurela.terv,
                           dobosy, tallidart.ec, tagesson.ec, sullivan.ec, hensgens.ec,
                           carey.ec, jansen.ec, seco.ec, pal.bar.ec, pal.oli.ec, 
                           peichl.hlf.ec, peichl.hlm.ec, peichl.stj.ec, kasak.ec, anthony.ec,
                           arndt.ec.cf, arndt.ec.iq , schulze.lut.ec, schulze.str.ec, prokushkin.ec, goeckede.1.ec,
                           goeckede.2.ec), 
                      fill = TRUE)
PIdat.ec$extraction_source <- "User-contributed"
PIdat.ec$dataentry_person <- "Wargowsky"
#remove rows that do not contain flux data
PIdat.ec <- PIdat.ec %>%
  dplyr::filter(!if_all(c(nee, gpp, reco, ch4_flux_total, nee_seasonal, ch4_flux_seasonal), ~ is.na(.)))

PIdat.ch <- rbindlist(list(peacock, jung, althuizen, schulze.ch, jassey, sabrekov.ch, dyukarev.ch, hung.ch.cbawo,
                           hung.ch.ykd, heffernan, davidson.16.monthly, davidson.19.monthly, davidson.21.monthly,
                           aurela.lett.ch, martikainen, tallidart.ch, tagesson.ch, chafe.ch, koster.ch,
                           sjogersten.ch, lopezblanco.ch, ilyasov.ch, voigt.ch, glagolev.ch, peichl.deg.ch,
                           treat.ch, tuittila.ch, anthony.ch, vaughn.ch, bjorkman.ch, salazar.ch, nykanen.ch,
                           marushchak.ch, webb.ch, johannesson.ch, olefeldt.ch, prokushkin.ch, stpierre.ch,
                           blancbetes.ch, rasanen.ch, virkkala.2017.ch, virkkala.2018.ch), 
                      fill = TRUE)
PIdat.ch$extraction_source <- "User-contributed"
PIdat.ch$dataentry_person <- "Wargowsky"
#remove rows that do not contain flux data
PIdat.ch <- PIdat.ch %>%
  dplyr::filter(!if_all(c(nee, gpp, reco, ch4_flux_total, nee_seasonal, ch4_flux_seasonal), ~ is.na(.)))

dupes.ec <- PIdat.ec %>% get_dupes(site_name, site_reference,  year, month, partition_method) 
dupes.ch <- PIdat.ch %>% get_dupes(site_name, site_reference,  year, month)  

#clarifying extraction source
PIdat.ch <- PIdat.ch %>%
  mutate(extraction_source= ifelse(data_contributor_or_author %in%  "Scott Davidson" , "User-contributed/Publication", extraction_source)) %>%
  mutate(extraction_source= ifelse(data_contributor_or_author %in%  "Torbern Tagesson" , "User-contributed/Publication", extraction_source)) %>%
  mutate(extraction_source= ifelse(data_contributor_or_author %in%  c("Oriana Chafe, Ian Shirley, Stan Wullschleger, and Margaret Torn" ,
                                                                       "Lydia Vaughn, Mark Conrad, Margaret Torn, Markus Bill, Bryan Curtis, Oriana Chafe"), 
                                    "User-contributed/NGEE", extraction_source))
  
PIdat.ec <- PIdat.ec %>%
  mutate(extraction_source= ifelse(data_contributor_or_author %in%  "Torbern Tagesson" , "User-contributed/Publication", extraction_source)) 
  
  

PIdat.ec2 <- PIdat.ec %>% 
  mutate(citation_ch4 = ifelse(!is.na(ch4_flux_total)| !is.na(ch4_flux_seasonal), citation, NA)) %>%
  mutate(citation_co2 = ifelse(!is.na(nee) | !is.na(gpp) | !is.na(reco)| !is.na(nee_seasonal),  citation, NA)) %>%
  mutate(extraction_source_ch4 = ifelse(!is.na(ch4_flux_total)| !is.na(ch4_flux_seasonal), extraction_source, NA)) %>%
  mutate(extraction_source_co2 = ifelse(!is.na(nee) | !is.na(gpp) | !is.na(reco)| !is.na(nee_seasonal), extraction_source, NA)) %>%
  mutate(citation = NULL, extraction_source= NULL)

PIdat.ch2 <- PIdat.ch %>% 
  mutate(citation_ch4 = ifelse(!is.na(ch4_flux_total)| !is.na(ch4_flux_seasonal), citation, NA)) %>%
  mutate(citation_co2 = ifelse(!is.na(nee) | !is.na(gpp) | !is.na(reco)| !is.na(nee_seasonal),  citation, NA)) %>%
  mutate(extraction_source_ch4 = ifelse(!is.na(ch4_flux_total)| !is.na(ch4_flux_seasonal), extraction_source, NA)) %>%
  mutate(extraction_source_co2 = ifelse(!is.na(nee) | !is.na(gpp) | !is.na(reco)| !is.na(nee_seasonal), extraction_source, NA)) %>%
  mutate(citation = NULL, extraction_source= NULL)

#check to make sure chamber_nr_measurement_days and gap_fill_perc were fixed
unique(PIdat.ch2$chamber_nr_measurement_days) #should be NA
PIdat.ch2$chamber_nr_measurement_days <- NULL

unique(PIdat.ec$gap_fill_perc) #should be NA
PIdat.ec$gap_fill_perc <- NULL


setwd("/Users/iwargowsky/Desktop/ABCFlux v2")
static <- read_csv("static.towersites.csv") %>% 
  dplyr::select(-c("tsoil_surface_depth", "tsoil_deep_depth", "tair_height", "moisture_depth", "instrumentation" ))
#join to fill NAs
library(rquery)
PIdat.ec2.static <- natural_join(PIdat.ec2, static, by= "site_reference", jointype= "FULL")
#x<- dplyr::anti_join(PIdat.ec2.static, PIdat.ec2, by= c("site_reference")) #checking to see if any data was removed




setwd("/Users/iwargowsky/Desktop/ABCFlux v2")
write_csv(PIdat.ec2.static, "PI.data.ec.csv")
write_csv(PIdat.ch2, "PI.data.ch.csv")



