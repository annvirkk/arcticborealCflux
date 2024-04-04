###Creating the final database
library(dplyr)
library(readr)
library(readxl)
library(janitor)
library(data.table)
library(zoo)
setwd("/Users/iwargowsky/Desktop/ABCFlux v2") 
abcflux.v1 <- read.csv("ABCfluxv1.v2format.csv") #ABCflux v1 in v2 format
#separate EC and chamber measurements
abcflux.v1.EC <- abcflux.v1 %>% filter(flux_method=="EC")
abcflux.v1.Ch <- abcflux.v1 %>% filter(!flux_method=="EC")

###Eddy covariance tower data####---------------------------------------------------
setwd("/Users/iwargowsky/Desktop/ABCFlux v2") 
PIdat.ec <- read_csv("PI.data.ec.csv") # data from PIs
extracted.ec.dat <- read_csv("extracted.ec.dat.csv")
flux.repository <- read_csv("towerrepositorydata.static.csv") #data from major flux repositories 
#merging 
ABC.ec.wdupes <- rbindlist(list(PIdat.ec ,flux.repository, abcflux.v1.EC, extracted.ec.dat), fill = TRUE)
ABC.ec.wdupes <- ABC.ec.wdupes %>% mutate(year= as.integer(ABC.ec.wdupes$year)) %>% 
                                   mutate(month= as.integer(ABC.ec.wdupes$month))

#remove rows that do not contain flux data
ABC.ec.wdupes <- ABC.ec.wdupes %>% filter(!if_all(c(nee, gpp, reco, ch4_flux_total, nee_seasonal, ch4_flux_seasonal,
                                                    ch4_flux_diffusion,ch4_flux_ebullition, ch4_flux_storage,co2_flux_storage, 
                                                    ch4_flux_storage_bubble, co2_flux_storage_bubble), ~ is.na(.)))

#unifying names for partition methods 
ABC.ec.wdupes <- ABC.ec.wdupes %>%
  mutate(partition_method = ifelse(ABC.ec.wdupes$partition_method %in% c("Reichstein et al. (2005)", "Reichstein (night time=Reco partitioning)", "Reichstein, Nighttime"),"Reichstein",
                                   ifelse(ABC.ec.wdupes$partition_method %in% c("DT", "Lasslop (bulk/day-time partitioning)", "Day Time Partitioning in REddyProc"), "Lasslop", ABC.ec.wdupes$partition_method)))
#find number of  duplicates
dupes<- ABC.ec.wdupes %>% get_dupes(site_name, site_reference, year, month, partition_method) 
dupes$na_count <- rowSums(is.na(dupes))
#setwd("/Users/iwargowsky/Desktop/ABCFlux v2") 
#write.csv(dupes, "PI.repository.v1.dupes.csv")


#remove duplicates
ABC.ec  <- ABC.ec.wdupes  %>% 
  arrange(desc(extraction_source_co2)) %>% #this step ensures "User-contributed" data is kept
  distinct(site_name, site_reference, partition_method, year, month, .keep_all = TRUE)


# ####reformatting based on partitioning methods ####
# ABC.ec <- ABC.ec %>%
#  mutate(gpp.nt = ifelse(partition_method %in% "Reichstein", gpp, NA),
#        gpp.dt = ifelse(partition_method %in% "Lasslop", gpp, NA),
#       reco.nt = ifelse(partition_method %in% "Reichstein", reco, NA),
#      reco.dt = ifelse(partition_method %in% "Lasslop", reco, NA),
#     gpp = ifelse(!(partition_method %in% c("Reichstein", "Lasslop")), gpp, NA),
#     reco = ifelse(!(partition_method %in% c("Reichstein", "Lasslop")), reco, NA),
#     nee.dt.nt = ifelse(partition_method %in% c("Reichstein","Lasslop"), nee, NA),
#     nee = ifelse(!(partition_method %in% c("Reichstein", "Lasslop")), nee, NA))
# 
# ##condense rows
# ABC.ec <- ABC.ec %>% group_by(year, month, site_name, site_reference, extraction_source_co2, extraction_source_ch4) %>% 
#   summarise_all(list(unique = ~toString(unique(.[!is.na(.)])))) %>% #summarise by unique values
#   rename_with(~gsub("_unique", "", .), everything()) #remove "_unique" from column names
# 
# persistant.dupes <- ABC.ec %>% get_dupes(site_name, site_reference, year, month) 
# #write.csv(persistant.dupes, "persistant.dupes.csv")


#fix Ameriflux BASE name
ABC.ec <- ABC.ec %>% 
  mutate(extraction_source_co2= ifelse(extraction_source_co2== "AAmeriflux BASE","Ameriflux BASE", extraction_source_co2))%>%
  mutate(extraction_source_ch4= ifelse(extraction_source_ch4== "AAmeriflux BASE","Ameriflux BASE", extraction_source_ch4))


#save
#setwd("/Users/iwargowsky/Desktop/ABCFlux v2")
#towersites <- as.data.frame(unique(ABC.ec$site_reference) )
#write_csv(towersites, "towersites.csv")
#write_csv(ABC.ec, "ABC.v2.ec.csv")
###Chamber data####-------------------------------------------------------------------------------------------------
setwd("/Users/iwargowsky/Desktop/ABCFlux v2") 
PIdat.ch <- read_csv("PI.data.ch.csv")
ADC.ch <- read_csv("ADC.ch.csv")
Zenodo.ch <- read_csv("Zenodo.ch.csv")
bawld.ch.dat <- read_csv("bawld.ch.dat.csv")
ikw.co2.ch <- read_csv("IKW.CO2.dataextractions.ch.csv")

ABC.ch.wdupes <- rbindlist(list(PIdat.ch, ADC.ch, Zenodo.ch, abcflux.v1.Ch, bawld.ch.dat, ikw.co2.ch), fill = TRUE)
ABC.ch.wdupes <- ABC.ch.wdupes %>% mutate(year= as.integer(ABC.ch.wdupes$year)) %>% 
  mutate(month= as.integer(ABC.ch.wdupes$month))

#unifying names for partition methods 
unique(ABC.ch.wdupes$partition_method) #see if any chamber measurements specify a partition method
ABC.ch.wdupes <- ABC.ch.wdupes %>%
  mutate(partition_method = ifelse(ABC.ch.wdupes$partition_method %in% "Reichstein (night time=Reco partitioning)","Reichstein", ABC.ch.wdupes$partition_method))
# ####reformatting based on partitioning methods ####
# ABC.ch.wdupes <- ABC.ch.wdupes %>%
#   mutate(gpp.nt = ifelse(partition_method %in% "Reichstein", gpp, NA),
#          gpp.dt = ifelse(partition_method %in% "Lasslop", gpp, NA),
#          reco.nt = ifelse(partition_method %in% "Reichstein", reco, NA),
#          reco.dt = ifelse(partition_method %in% "Lasslop", reco, NA),
#          gpp = ifelse(!(partition_method %in% c("Reichstein", "Lasslop")), gpp, NA),
#          reco = ifelse(!(partition_method %in% c("Reichstein", "Lasslop")), reco, NA))

#remove rows that do not contain flux data
ABC.ch.wdupes <- ABC.ch.wdupes %>%
  filter(!if_all(c(nee, gpp, reco, ch4_flux_total, nee_seasonal, ch4_flux_seasonal,
                   ch4_flux_diffusion,ch4_flux_ebullition, ch4_flux_storage,co2_flux_storage, 
                   ch4_flux_storage_bubble, co2_flux_storage_bubble), ~ is.na(.)))

#find number of  duplicates
dupes<- ABC.ch.wdupes %>% get_dupes(site_name, site_reference, site_id, year, month) 
dupes$na_count <- rowSums(is.na(dupes))
#dupes are okay for now



setwd("/Users/iwargowsky/Desktop/ABCFlux v2") 
#write_csv(ABC.ch.wdupes, "ABCv2.ch.csv")



  
####################Combining EC and chamber data ################################

ABC.v2.apr24 <- rbindlist(list(ABC.ch.wdupes, ABC.ec), fill = TRUE)
ABC.v2.apr24<- ABC.v2.apr24 %>% select(-starts_with("...")) %>% filter(!site_name== "")

setwd("/Users/iwargowsky/Desktop/arcticborealCflux") 
write_csv(ABC.v2.apr24, "ABC.v2.apr24.csv")



  
####extract list of sites and dates covered##
ECsites.datescovered <- ABC.v2.mar24 %>% 
  filter(flux_method== "EC" ) %>%
  mutate(ts= as.yearmon(paste(year, month,sep = '-'))) %>%
  group_by(site_name, site_reference) %>% 
  dplyr::summarise (start= first(ts), end= last(ts), num_months= n()) 


#write_csv(ECsites.datescovered, "ABCv2.EC.datescovered.csv")

CHsites.datescovered <- ABC.v2.mar24 %>% 
  filter(!flux_method== "EC" ) %>%
  mutate(ts= as.yearmon(paste(year, month,sep = '-'))) %>%
  group_by(site_name, site_reference) %>% 
  dplyr::summarise (start= first(ts), end= last(ts), num_months= n()) 


#write_csv(CHsites.datescovered, "ABCv2.Ch.datescovered.csv")



