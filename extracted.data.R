##Combining data from data extraction 
library(dplyr)
library(readr)
library(readxl)

setwd("/Users/iwargowsky/Desktop/ABCFlux v2/chamber data extractions")
#Data from intentsive data extraction
isabel.bawld <- read_excel("Isabel.bawld.data.xlsx", sheet= 1)[-(1:3),]
isabel.bawld$dataentry_person <- "Isabel"
isabel.bawld$extraction_source <- "BAWLD-CH4-Publication"
anna.bawld <- read_excel("ABCfluxv2.vars_AV_0502_2024.xlsx", sheet= 4)[-(1:3),-1]
anna.bawld$dataentry_person <- "Anna"
anna.bawld$extraction_source <- paste("BAWLD-CH4-", anna.bawld$extraction_source, sep="")

#Kenzies files
path <- "/Users/iwargowsky/Desktop/ABCFlux v2/chamber data extractions/Kenzie/"
terrestrial_files <- list.files(file.path(path, "Terrestrial"), pattern = "\\.xlsx$", full.names = TRUE)
kenzie.bawld <- data.frame()
for (file in terrestrial_files) { 
  sheet <- readxl::read_excel(file, sheet = 5)[-(1:3),-1]
  kenzie.bawld <- rbind(kenzie.bawld, sheet)}
kenzie.bawld$dataentry_person <- "McKenzie"


#Combine all 
bawld.dat <- rbindlist(list( isabel.bawld, anna.bawld, kenzie.bawld), fill = TRUE)
bawld.dat <- bawld.dat %>% dplyr::filter(!site_name=="") #removing blank rows

bawld.dat <- bawld.dat %>% 
  mutate(citation_ch4 = ifelse(!is.na(ch4_flux_total), citation, NA)) %>%
  mutate(citation_co2 = ifelse(!is.na(nee) | !is.na(gpp) | !is.na(reco), citation, NA)) %>%
  mutate(extraction_source_ch4 = ifelse(!is.na(ch4_flux_total), extraction_source, NA)) %>%
  mutate(extraction_source_co2 = ifelse(!is.na(nee) | !is.na(gpp) | !is.na(reco), extraction_source, NA)) %>%
  mutate(chamber_nr_measurement_days_ch4 = ifelse(!is.na(ch4_flux_total), chamber_nr_measurement_days, NA)) %>%
  mutate(chamber_nr_measurement_days_co2 = ifelse(!is.na(nee) | !is.na(gpp) | !is.na(reco), chamber_nr_measurement_days, NA)) %>%
  mutate(citation = NULL, extraction_source= NULL, chamber_nr_measurement_days= NULL)


#remove extra columns 
# setwd("/Users/iwargowsky/Desktop/ABCFlux v2")
# v2.vars <- read_csv("ABCfluxv2.variables.csv")
# bawld.dat <- left_join(bawld.dat, v2.vars) %>% select(!colnames(v2.vars))
# #Keep only columns that exist in ABC V2
# bawld.dat <- bawld.dat %>% select(colnames(v2.vars))


setwd("/Users/iwargowsky/Desktop/ABCFlux v2")
write_csv(bawld.dat, "bawld.ch.dat.csv")

#distribution of data in bawld sites
table(bawld.dat$land_cover_bawld)
#find percent of nas in each column then reverse it to find percent of data present
percentna <- data.frame(colMeans(is.na(bawld.dat)))
percentna$Percent.data.present <- 100- percentna$colMeans.is.na.bawld.dat..*100
percentna$variable <- row.names(percentna)
dat <- percentna[c(21,22,23,24,35,36,39,48,49,51,53,54,57),]
#graphing
ggplot(data=dat, aes(x= variable, y=Percent.data.present , fill= variable)) +
  geom_bar(stat = "identity") + coord_flip() +  scale_fill_hue(c = 40) +
  theme(legend.position="none")


#------------------------------EC DATA--------------------------------------------
setwd("/Users/iwargowsky/Desktop/ABCFlux v2")
anna.bawld.ec <- read_excel("chamber data extractions/ABCfluxv2.vars_AV_0502_2024.xlsx", sheet= 5)[-(1:3),-1] %>% select(-`...78`) # remove blank column
anna.bawld.ec$extraction_source <- "BAWLD-CH4-Publication"
anna.bawld.ec$dataentry_person <- "Anna"

ikw.co2.ec <- read_csv("chamber data extractions/IKW.CO2.dataextractions.tower.csv") %>% select(-`...77`) # remove blank column
ikw.co2.ec $extraction_source <- "Publication"
ikw.co2.ec$dataentry_person <- "Isabel"


#Combine all 
bawld.ec.dat <- rbindlist(list( ikw.co2.ec, anna.bawld.ec ), fill = TRUE)
bawld.ec.dat <- bawld.ec.dat %>% dplyr::filter(!site_name=="") 
bawld.ec.dat <- bawld.ec.dat %>% 
  mutate(citation_ch4 = ifelse(!is.na(ch4_flux_total), citation, NA)) %>%
  mutate(citation_co2 = ifelse(!is.na(nee) | !is.na(gpp) | !is.na(reco), citation, NA)) %>%
  mutate(extraction_source_ch4 = ifelse(!is.na(ch4_flux_total), extraction_source, NA)) %>%
  mutate(extraction_source_co2 = ifelse(!is.na(nee) | !is.na(gpp) | !is.na(reco), extraction_source, NA)) %>%
    mutate(citation = NULL, extraction_source= NULL)


setwd("/Users/iwargowsky/Desktop/ABCFlux v2")
write_csv(bawld.ec.dat, "extracted.ec.dat.csv")






#------------CO2 chamber data extraction----------------------------------------
setwd("/Users/iwargowsky/Desktop/ABCFlux v2/chamber data extractions")
ikw.co2.ch <- read_xlsx("ABCfluxv2.IKW.CO2.xlsx", sheet= "terrestrial chamber") 
ikw.co2.ch  <- ikw.co2.ch %>%
  mutate(citation_ch4 = ifelse(!is.na(ch4_flux_total), citation, NA)) %>%
  mutate(citation_co2 = ifelse(!is.na(nee) | !is.na(gpp) | !is.na(reco), citation, NA)) %>%
  mutate(extraction_source_ch4 = ifelse(!is.na(ch4_flux_total), extraction_source, NA)) %>%
  mutate(extraction_source_co2 = ifelse(!is.na(nee) | !is.na(gpp) | !is.na(reco), extraction_source, NA)) %>%
  mutate(chamber_nr_measurement_days_ch4 = ifelse(!is.na(ch4_flux_total), chamber_nr_measurement_days, NA)) %>%
  mutate(chamber_nr_measurement_days_co2 = ifelse(!is.na(nee) | !is.na(gpp) | !is.na(reco), chamber_nr_measurement_days, NA)) %>%
  mutate(citation = NULL, extraction_source= NULL, chamber_nr_measurement_days= NULL)



setwd("/Users/iwargowsky/Desktop/ABCFlux v2") 
write_csv(ikw.co2.ch, "IKW.CO2.dataextractions.ch.csv")





