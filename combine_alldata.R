###Creating the final database
library(dplyr)
library(readr)
library(readxl)
library(janitor)
library(data.table)
setwd("/Users/iwargowsky/Desktop/ABCFlux v2") 
abcflux.v1 <- read.csv("ABCfluxv1.v2format.csv") #ABCflux v1 in v2 format
abcflux.v1$extraction_source <- paste("ABCflux v1-", abcflux.v1$extraction_source)
#separate EC and chamber measurements
abcflux.v1.EC <- abcflux.v1 %>% filter(flux_method=="EC")
abcflux.v1.Ch <- abcflux.v1 %>% filter(!flux_method=="EC")

###Eddy covariance tower data####---------------------------------------------------
setwd("/Users/iwargowsky/Desktop/ABCFlux v2") 
flux.repository <- read_csv("towerrepositorydata.static.csv") #data from major flux repositories 
PIdat.ec <- read_csv("PI.data.ec.csv") # data from PIs
anna.bawld.ec <- read_excel("chamber data extractions/ABCfluxv2.vars_AV_171023.xlsx", sheet= 5)[-(1:3),-1]
anna.bawld.ec <- anna.bawld.ec %>% select(-`...78`) # remove blank column
anna.bawld.ec$extraction_source <- "BAWLD-CH4-Publication"
#ADC.ec <- read.csv("ADC.ec.csv")
#merging
ABC.ec.wdupes <- rbindlist(list(PIdat.ec ,flux.repository, abcflux.v1.EC, anna.bawld.ec), fill = TRUE)
#remove rows that do not contain flux data
ABC.ec.wdupes <- ABC.ec.wdupes %>% filter(!if_all(c(nee, gpp, reco, ch4_flux_total, nee_seasonal,
                                                    ch4_flux_seasonal,ch4_flux_diffusion,ch4_flux_ebullition,
                                                    ch4_flux_storage,co2_flux_storage, ch4_flux_storage_bubble,
                                                    co2_flux_storage_bubble), ~ is.na(.)))
#unifying names for partition methods 
ABC.ec.wdupes <- ABC.ec.wdupes %>%
  mutate(partition_method = ifelse(ABC.ec.wdupes$partition_method %in% c("Reichstein et al. (2005)", "Reichstein (night time=Reco partitioning)"),"Reichstein",
                                   ifelse(ABC.ec.wdupes$partition_method %in% c("DT", "Lasslop (bulk/day-time partitioning)"), "Lasslop", ABC.ec.wdupes$partition_method)))
####reformatting based on partitioning methods ####
#ABC.ec.wdupes <- ABC.ec.wdupes %>%
#  mutate(gpp.nt = ifelse(partition_method %in% "Reichstein", gpp, NA),
 #        gpp.dt = ifelse(partition_method %in% "Lasslop", gpp, NA),
  #       reco.nt = ifelse(partition_method %in% "Reichstein", reco, NA),
   #      reco.dt = ifelse(partition_method %in% "Lasslop", reco, NA),
   #      gpp = ifelse(!(partition_method %in% c("Reichstein", "Lasslop")), gpp, NA),
    #     reco = ifelse(!(partition_method %in% c("Reichstein", "Lasslop")), reco, NA))

#find number of  duplicates
dupes<- ABC.ec.wdupes %>% get_dupes(site_name, site_reference, year, month, partition_method)  
#remove duplicates
ABC.ec  <- ABC.ec.wdupes  %>% 
  arrange(desc(extraction_source)) %>% #this step ensures "User-contributed" data is kept
  distinct(site_name, site_reference, year, month, partition_method, .keep_all = TRUE)

#save
setwd("/Users/iwargowsky/Desktop/ABCFlux v2")
#towersites <- as.data.frame(unique(ABC.ec$site_reference) )
#write_csv(towersites, "towersites.csv")
#write_csv(ABC.ec, "ABC.v2.ec.csv")
###Chamber data####---------------------------------------------------
setwd("/Users/iwargowsky/Desktop/ABCFlux v2") 
PIdat.ch <- read_csv("PI.data.ch.csv")
ADC.ch <- read_csv("ADC.ch.csv")
Zenodo.ch <- read_csv("Zenodo.ch.csv")
bawld.dat <- read_csv("bawld.dat.csv")

ABC.ch.wdupes <- rbindlist(list(PIdat.ch, ADC.ch, Zenodo.ch, abcflux.v1.Ch, bawld.dat), fill = TRUE)
ABC.ch.wdupes  <- ABC.ch.wdupes  %>% select(-`...78`) # remove blank column
#remove rows that do not contain flux data
ABC.ch.wdupes <- ABC.ch.wdupes %>%
  filter(!if_all(c(nee, gpp, reco, ch4_flux_total, nee_seasonal,
                   ch4_flux_seasonal,ch4_flux_diffusion,ch4_flux_ebullition,
                   ch4_flux_storage,co2_flux_storage, ch4_flux_storage_bubble,
                   co2_flux_storage_bubble ), ~ is.na(.)))
#ABC.ch.wdupes.ch4 <- ABC.ch.wdupes %>%
 # filter(!if_all("ch4_flux_total", ~ is.na(.)))
#find number of  duplicates
dupes<- ABC.ch.wdupes %>% get_dupes(site_id, site_name, site_reference, site_id, year, month)  
#dupes are okay for now


setwd("/Users/iwargowsky/Desktop/ABCFlux v2") 
#write_csv(ABC.ch.wdupes, "ABCv2.ch.csv")



  
####################Combining EC and chamber data ################################

ABC.v2.jan24 <- rbindlist(list(ABC.ch.wdupes, ABC.ec), fill = TRUE)
#fixing month column
ABC.v2.jan24$month <- as.integer(ABC.v2.jan24$month)

setwd("/Users/iwargowsky/Desktop/arcticborealCflux") 
#write_csv(ABC.v2.jan24, "ABC.v2.jan24.csv")



  
####extract list of sites and dates covered##
sites.datescovered <- ABC.ec %>% 
  distinct(site_reference, year, month, .keep_all = TRUE) %>%
  mutate(ts= as.yearmon(paste(year, month,sep = '-'))) %>%
  group_by(site_reference, longitude, latitude) %>% 
  dplyr::summarise (start= first(ts), end= last(ts), num_months= n()) 


write_csv(sites.datescovered, "ABCv2.EC.datescovered.csv")

