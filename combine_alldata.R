###Creating the final database
library(dplyr)
library(janitor)
setwd("/Users/iwargowsky/Desktop/ABCFlux v2") 
abcflux.v1 <- read.csv("ABCfluxv1.v2format.csv") #ABCflux v1 in v2 format
abcflux.v1$extraction_source <- paste("ABCFlux v1-", abcflux.v1$extraction_source)
#separate EC and chamber measurements
abcflux.v1.EC <- abcflux.v1 %>% filter(flux_method=="EC")
abcflux.v1.Ch <- abcflux.v1 %>% filter(!flux_method=="EC")

###Eddy covariance tower data####---------------------------------------------------
setwd("/Users/iwargowsky/Desktop/ABCFlux v2") 
flux.repository <- read_csv("towerrepositorydata.static.csv") #data from major flux repositories 
PIdat.ec <- read_csv("PI.data.ec.csv") # data from PIs
#ADC.ec <- read.csv("ADC.ec.csv")
#merging
ABC.ec.wdupes <- rbindlist(list(PIdat.ec ,flux.repository, abcflux.v1.EC), fill = TRUE)
#remove rows that do not contain flux data
ABC.ec.wdupes <- ABC.ec.wdupes %>% filter(!if_all(c("nee", "gpp", "reco","ch4_flux_total"), ~ is.na(.)))
#reformatting based on partitioning methods
#ABC.ec.wdupes <- ABC.ec.wdupes %>%
 # mutate(gpp.nt = ifelse(partition_method %in% c("Reichstein", "Reichstein (night time=Reco partitioning)"), gpp, NA),
  #       gpp.dt = ifelse(partition_method == "Lasslop", gpp, NA),
   #      reco.nt = ifelse(partition_method %in% c("Reichstein","Reichstein (night time=Reco partitioning)"), reco, NA),
    #     reco.dt = ifelse(partition_method == "Lasslop", reco, NA),
     #    gpp = ifelse(partition_method %in% c("Reichstein","Reichstein (night time=Reco partitioning)", "dt"), NA, gpp),
      #   reco = ifelse(partition_method %in% c("Reichstein","Reichstein (night time=Reco partitioning)", "dt"), NA, reco))
#find number of  duplicates
dupes<- ABC.ec.wdupes %>% get_dupes(site_reference, year, month, flux_method_detail, partition_method)  
#remove duplicates
ABC.ec <- ABC.ec.wdupes  %>% 
  arrange(desc(extraction_source)) %>% #this step ensures "User-contributed" data is kept
  distinct(site_reference, year, month, partition_method, flux_method_detail, .keep_all = TRUE)



#save
#setwd("/Users/iwargowsky/Desktop/ABCFlux v2")
#towersites <- as.data.frame(unique(ABC.ec$site_reference) )
#write_csv(towersites, "towersites.csv")
#write_csv(ABC.ec, "ABCv2.ec.9.5.csv")
###Chamber data####---------------------------------------------------
setwd("/Users/iwargowsky/Desktop/ABCFlux v2") 
PIdat.ch <- read_csv("PI.data.ch.csv")
ADC.ch <- read_csv("ADC.ch.csv")
Zenodo.ch <- read_csv("Zenodo.ch.csv")
isabel.bawld <- read_csv("isabel.bawld.data.csv")

ABC.ch.wdupes <- rbindlist(list(PIdat.ch, ADC.ch, Zenodo.ch, abcflux.v1.Ch, isabel.bawld), fill = TRUE)
#remove rows that do not contain flux data
ABC.ch.wdupes <- ABC.ch.wdupes %>%
  filter(!if_all(c("nee", "gpp", "reco", "ch4_flux_total"), ~ is.na(.)))
ABC.ch.wdupes.ch4 <- ABC.ch.wdupes %>%
  filter(!if_all("ch4_flux_total", ~ is.na(.)))
#find number of  duplicates
dupes<- ABC.ch.wdupes %>% get_dupes(site_id, site_name, site_reference, site_id, year, month)  
#dupes are okay for now


setwd("/Users/iwargowsky/Desktop/ABCFlux v2") 
write_csv(ABC.ch.wdupes, "ABCv2.ch.csv")



  
####################Combining EC and chamber data ################################

ABC.v2 <- rbindlist(list(ABC.ch.wdupes, ABC.ec), fill = TRUE)
  
  
####extract list of sites and dates covered##
sites.datescovered <- ABC.ec %>% 
  distinct(site_reference, year, month, .keep_all = TRUE) %>%
  mutate(ts= as.yearmon(paste(year, month,sep = '-'))) %>%
  group_by(site_reference, longitude, latitude) %>% 
  dplyr::summarise (start= first(ts), end= last(ts), num_months= n()) 


write_csv(sites.datescovered, "ABCv2.EC.datescovered.csv")

