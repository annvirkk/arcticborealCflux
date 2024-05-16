### Investigating CO2 ####
library(dplyr)
library(ggplot2)
library(readr)
library(zoo)
library(ggplot2)


setwd("/Users/iwargowsky/Desktop/arcticborealCflux")  
abc <- read_csv("ABC.v2.may24.cleanish.nodupes.csv") %>%
  mutate(nee= as.numeric(nee),
         gpp= as.numeric(gpp),
         reco= as.numeric(reco))
abc$extraction_source <- paste("CO2:", abc$extraction_source_co2, "CH4:", abc$extraction_source_ch4, sep= " ")

###looking at just CO2
abc.co2 <- abc %>% 
  dplyr::filter(!if_all(c(nee, gpp, reco, nee_seasonal, co2_flux_storage, co2_flux_storage_bubble), ~ is.na(.)))
# 
# 
# ###Land cover by site_name and site_reference
# abc.co2.condense <- abc.co2 %>%
#   group_by(site_name, site_reference,  flux_method, land_cover_bawld, latitude, longitude, veg_detail, landform, citation_co2, permafrost, nee_seasonal_interval) %>%
#   dplyr::summarise(nee_max= max(nee), nee_min= min(nee),
#                    reco_max= max(reco), reco_min= min(reco),
#                    gpp_max= max(gpp), gpp_min= min(gpp),
#                    nee_season_max= max(nee_seasonal), nee_season_min= min(nee_seasonal),
#                    co2_storage_max= max(co2_flux_storage), co2_storage_min= min(co2_flux_storage),
#                    co2_bubble_max= max(co2_flux_storage_bubble), co2_bubble_min= min(co2_flux_storage_bubble))
# 
# setwd("/Users/iwargowsky/Desktop/ABCFlux v2")
# write_csv(abc.co2.condense, "abc.co2.landcover.csv")


###CO2 fluxes figures by each site

abc.co2$ts <- as.yearmon(paste(abc.co2$year, abc.co2$month,sep = '-')) #add timestamp

abc.co2.ec <- abc.co2 %>% dplyr::filter(flux_method== "EC") #filter for EC towers
abc.co2.ch <- abc.co2 %>% dplyr::filter(flux_method== "Chamber") #filter for chambers

#find chamber sites with multiple site_reference points (used for graphing)
abc.co2.ch.siteref <- abc.co2.ch %>% group_by(site_name) %>%
  dplyr::summarise(n= n_distinct(site_reference))

abc.co2.ch <- abc.co2.ch %>%full_join(abc.co2.ch.siteref)

abc.co2.ch.multi <- abc.co2.ch %>% dplyr::filter(n>1)
abc.co2.ch.solo <- abc.co2.ch %>% dplyr::filter(n==1)



# Plotting CO2 EC and saving each plot
setwd("/Users/iwargowsky/Desktop/ABCFlux v2")
lapply(unique(abc.co2.ec$site_name), function(site) {
  p <- ggplot(subset(abc.co2.ec, site_name == site)) +
    geom_line( aes(x = ts, y = nee, color = extraction_source_co2)) +
    geom_point(aes(x = ts, y = nee, color = extraction_source_co2))+
    geom_line( aes(x = ts, y = gpp, color= extraction_source_co2), linetype= "dashed") +
    geom_point(aes(x = ts, y = gpp, color= extraction_source_co2), shape= 2)+
    geom_line( aes(x = ts, y = reco, color= extraction_source_co2), linetype= "dashed") +
    geom_point(aes(x = ts, y = reco, color= extraction_source_co2), shape= 0)+
    theme(legend.position = "bottom") +
    geom_hline(yintercept = 0)+   
    labs(title = paste("EC ", site),
         x = "Date",
         y = "g C m-2 month-1")
  
  #Save the plot to a file
  ggsave(filename = paste("CO2_EC2/EC_", site, ".jpeg"),
         plot = p, width = 10, height = 6)

  return(p)
})  



abc.co2.ec.x <- abc.co2.ec %>% group_by(site_name, month) %>%
  dplyr::summarise(nee= mean(as.numeric(nee), na.rm= T))

ggplot(abc.co2.ec.x )+
  geom_point(aes(month, nee))

abc.co2.ec.x <- abc.co2.ec %>% group_by(site_name, month) %>%
  dplyr::summarise(gpp= mean(as.numeric(gpp), na.rm= T))

ggplot(abc.co2.ec.x )+
  geom_point(aes(month, gpp))

abc.co2.ec.x <- abc.co2.ec %>% group_by(site_name, month) %>%
  dplyr::summarise(reco= mean(as.numeric(reco), na.rm= T))

ggplot(abc.co2.ec.x )+
  geom_point(aes(month, reco))




# Plotting NEE Chamber and saving each plot
setwd("/Users/iwargowsky/Desktop/ABCFlux v2")
abc.co2.ch.multi.x <- abc.co2.ch.multi %>% dplyr::filter(!is.na(nee))
lapply(unique(abc.co2.ch.multi.x$site_name), function(site) {
  p <- ggplot(subset(abc.co2.ch.multi.x, site_name == site), aes(x = ts, y = nee, color = site_reference )) +
    geom_line() +
    geom_point()+
    labs(title = paste("Chamber NEE", site),
         x = "Date",
         y = "NEE g C m-2 month-1") +
    theme_minimal()

  # Save the plot to a file
  ggsave(filename = paste("NEE Chamber/NEE_chamber_", site, ".jpeg"),
         plot = p, width = 10, height = 6)

  return(p)
})

setwd("/Users/iwargowsky/Desktop/ABCFlux v2")
abc.co2.ch.solo.x <- abc.co2.ch.solo %>% dplyr::filter(!is.na(nee))
lapply(unique(abc.co2.ch.solo.x$site_name), function(site) {
  p <- ggplot(subset(abc.co2.ch.solo.x, site_name == site), aes(x = ts, y = nee)) +
    geom_line() +
    geom_point()+
    labs(title = paste("Chamber NEE", site),
         x = "Date",
         y = "NEE g C m-2 month-1") +
    theme_minimal()

  # Save the plot to a file
  ggsave(filename = paste("NEE Chamber/NEE_chamber_", site, ".jpeg"),
         plot = p, width = 10, height = 6)

  return(p)
})





# Plotting GPP Chamber and saving each plot
setwd("/Users/iwargowsky/Desktop/ABCFlux v2")
abc.co2.ch.multi.x <- abc.co2.ch.multi %>% filter(!is.na(gpp))
lapply(unique(abc.co2.ch.multi.x$site_name), function(site) {
  p <- ggplot(subset(abc.co2.ch.multi.x, site_name == site), aes(x = ts, y = gpp, color = site_reference )) +
    geom_line() +
    geom_point()+
    labs(title = paste("Chamber GPP", site),
         x = "Date",
         y = "GPP g C m-2 month-1") +
    theme_minimal()

  # Save the plot to a file
  ggsave(filename = paste("GPP Chamber/GPP_chamber_", site, ".jpeg"),
         plot = p, width = 10, height = 6)

  return(p)
})

setwd("/Users/iwargowsky/Desktop/ABCFlux v2")
abc.co2.ch.solo.x <- abc.co2.ch.solo %>% filter(!is.na(gpp))
lapply(unique(abc.co2.ch.solo.x$site_name), function(site) {
  p <- ggplot(subset(abc.co2.ch.solo.x, site_name == site), aes(x = ts, y = gpp)) +
    geom_line() +
    geom_point()+
    labs(title = paste("Chamber GPP", site),
         x = "Date",
         y = "GPP g C m-2 month-1") +
    theme_minimal()

  # Save the plot to a file
  ggsave(filename = paste("GPP Chamber/GPP_chamber_", site, ".jpeg"),
         plot = p, width = 10, height = 6)

  return(p)
})




# Plotting RECO Chamber and saving each plot
setwd("/Users/iwargowsky/Desktop/ABCFlux v2")
abc.co2.ch.multi.x <- abc.co2.ch.multi %>% filter(!is.na(reco))
lapply(unique(abc.co2.ch.multi.x$site_name), function(site) {
  p <- ggplot(subset(abc.co2.ch.multi.x, site_name == site), aes(x = ts, y = reco, color = site_reference )) +
    geom_line() +
    geom_point()+
    labs(title = paste("Chamber RECO", site),
         x = "Date",
         y = "RECO g C m-2 month-1") +
    theme_minimal()

  # Save the plot to a file
  ggsave(filename = paste("RECO Chamber/RECO_chamber_", site, ".jpeg"),
         plot = p, width = 10, height = 6)

  return(p)
})

setwd("/Users/iwargowsky/Desktop/ABCFlux v2")
abc.co2.ch.solo.x <- abc.co2.ch.solo %>% filter(!is.na(reco))
lapply(unique(abc.co2.ch.solo.x$site_name), function(site) {
  p <- ggplot(subset(abc.co2.ch.solo.x, site_name == site), aes(x = ts, y = reco)) +
    geom_line() +
    geom_point()+
    labs(title = paste("Chamber RECO", site),
         x = "Date",
         y = "RECO g C m-2 month-1") +
    theme_minimal()

  # Save the plot to a file
  ggsave(filename = paste("RECO Chamber/RECO_chamber_", site, ".jpeg"),
         plot = p, width = 10, height = 6)

  return(p)
})
