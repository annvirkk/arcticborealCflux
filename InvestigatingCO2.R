### Investigating CO2 ####
library(dplyr)
library(ggplot2)
library(readr)
library(zoo)
library(ggplot2)


setwd("/Users/iwargowsky/Desktop/arcticborealCflux")  
abc <- read_csv("ABC.v2.jul24.cleanish.nodupes.csv") %>%
  mutate(nee= as.numeric(nee),
         gpp= as.numeric(gpp),
         reco= as.numeric(reco))

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



# Plotting NEE EC and saving each plot
setwd("/Users/iwargowsky/Desktop/ABCFlux v2")
abc.co2.ec.x <- abc.co2.ec %>% dplyr::filter(!is.na(nee))
lapply(unique(abc.co2.ec.x$site_name), function(site) {
  p <- ggplot(subset(abc.co2.ec.x, site_name == site)) +
    geom_line( aes(x = ts, y = nee, color = gap_fill_perc_nee)) +
    geom_point(aes(x = ts, y = nee, color = gap_fill_perc_nee))+
    theme(legend.position = "bottom") +
    geom_hline(yintercept = 0)+   
    labs(title = paste("EC ", site),
         x = "Date",
         y = "g C m-2 month-1")
  
  #Save the plot to a file
  ggsave(filename = paste("CO2_EC.gapfillperc/NEE", site, ".jpeg"),
         plot = p, width = 10, height = 6)

  return(p)
})  



# Plotting GPP EC and saving each plot
setwd("/Users/iwargowsky/Desktop/ABCFlux v2")
abc.co2.ec.x <- abc.co2.ec %>% dplyr::filter(!is.na(gpp))
lapply(unique(abc.co2.ec.x$site_name), function(site) {
  p <- ggplot(subset(abc.co2.ec.x, site_name == site)) +
    geom_line( aes(x = ts, y = gpp, color = gap_fill_perc_gpp)) +
    geom_point(aes(x = ts, y = gpp, color = gap_fill_perc_gpp))+
    theme(legend.position = "bottom") +
    geom_hline(yintercept = 0)+   
    labs(title = paste("EC GPP ", site),
         x = "Date",
         y = "g C m-2 month-1")
  
  #Save the plot to a file
  ggsave(filename = paste("CO2_EC.gapfillperc/GPP", site, ".jpeg"),
         plot = p, width = 10, height = 6)
  
  return(p)
})  

# Plotting RECO EC and saving each plot
setwd("/Users/iwargowsky/Desktop/ABCFlux v2")
abc.co2.ec.x <- abc.co2.ec %>% dplyr::filter(!is.na(reco))
lapply(unique(abc.co2.ec.x$site_name), function(site) {
  p <- ggplot(subset(abc.co2.ec.x, site_name == site)) +
    geom_line( aes(x = ts, y = reco, color = gap_fill_perc_reco)) +
    geom_point(aes(x = ts, y = reco, color = gap_fill_perc_reco))+
    theme(legend.position = "bottom") +
    geom_hline(yintercept = 0)+   
    labs(title = paste("EC RECO ", site),
         x = "Date",
         y = "g C m-2 month-1")
  
  #Save the plot to a file
  ggsave(filename = paste("CO2_EC.gapfillperc/RECO", site, ".jpeg"),
         plot = p, width = 10, height = 6)
  
  return(p)
})  



abc.co2.ec.x <- abc.co2.ec %>% group_by(site_name, month) %>%
  dplyr::summarise(nee= mean(as.numeric(nee), na.rm= T))

ggplot(abc.co2.ec.x )+
  geom_point(aes(month, nee))+   
  labs(title = "Average monthly EC NEE ",
       x = "Month",
       y = "g C m-2 month-1")

abc.co2.ec.x <- abc.co2.ec %>% group_by(site_name, month) %>%
  dplyr::summarise(gpp= mean(as.numeric(gpp), na.rm= T))

ggplot(abc.co2.ec.x )+
  geom_point(aes(month, gpp))+   
  labs(title = "Average monthly EC GPP ",
       x = "Month",
       y = "g C m-2 month-1")

abc.co2.ec.x <- abc.co2.ec %>% group_by(site_name, month) %>%
  dplyr::summarise(reco= mean(as.numeric(reco), na.rm= T))

ggplot(abc.co2.ec.x )+
  geom_point(aes(month, reco))+   
  labs(title = "Average monthly EC RECO ",
       x = "Month",
       y = "g C m-2 month-1")




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


abc.co2.ch <- abc.co2.ch %>%
  dplyr::filter(!is.na(biome))%>%
  dplyr::filter(!biome=="Temperate")%>%
  mutate(region= case_when(country %in% "USA"~ "Alaskan",
                             country %in% "Canada" ~ "Canadian",
                             country %in% c("Finland","Greenland", "Iceland", "Norway",
                                            "Sweden", "Estonia")~ "Northern European",
                             country %in% c("Russia", "Mongolia") ~ "Russian")) %>% 
  mutate( Region.Biome = paste(region, biome))




ggplot(abc.co2.ch #%>% dplyr::filter(chamber_nr_measurement_days_co2> 4 | is.na(chamber_nr_measurement_days_co2)) %>% 
         #dplyr::filter(chamber_nr_measurement_days> 4 | is.na(chamber_nr_measurement_days))
       ) + 
  geom_point(aes(ts, nee, color= Region.Biome), size=3)+
  ggtitle("Chamber NEE by Region and Biome") +
  labs(x= NULL, y = expression(paste("NEE (gC m"^"-2", "month"^"-1",")")))+
  geom_hline(yintercept = 0)+ scale_color_brewer(palette = "Paired")+
  theme_bw(base_size = 12) + theme(legend.position = "bottom")+
  scale_y_continuous(expand= c(0,0), limits = c(-500,300))  





x <- abc.co2.ch %>% select(site_name, year, month, nee, Region.Biome)




 

setwd("/Users/iwargowsky/Desktop/arcticborealCflux")  
abc.nogaps <- read_csv("ABC.v2.may24.cleanish.nodupes.nogapfilled.csv") %>%
  mutate(nee= as.numeric(nee),
         gpp= as.numeric(gpp),
         reco= as.numeric(reco))
abc.nogaps$extraction_source <- paste("CO2:", abc.nogaps$extraction_source_co2, "CH4:", abc.nogaps$extraction_source_ch4, sep= " ")

###looking at just CO2
abc.co2.nogaps <- abc.nogaps %>% 
  dplyr::filter(!if_all(c(nee, gpp, reco, nee_seasonal, co2_flux_storage, co2_flux_storage_bubble), ~ is.na(.)))

abc.co2.nogaps$ts <- as.yearmon(paste(abc.co2.nogaps$year, abc.co2.nogaps$month,sep = '-')) #add timestamp

abc.co2.ec.nogaps <- abc.co2.nogaps %>% dplyr::filter(flux_method== "EC") #filter for EC towers





abc.co2.ec.x <- gdata::combine(abc.co2.ec , abc.co2.ec.nogaps)



setwd("/Users/iwargowsky/Desktop/ABCFlux v2")
lapply(unique(abc.co2.ec$site_name), function(site) {
  # Subset the data for the current site
  site_data <- abc.co2.ec.x %>% dplyr::filter(site_name == site)
  
  # Create the plot
  p <- ggplot(site_data) +
    geom_line(data = site_data %>% dplyr::filter(source == 'abc.co2.ec.nogaps'), aes(x = ts, y = nee), color= "blue") +
    geom_point(data = site_data %>% dplyr::filter(source == 'abc.co2.ec.nogaps'), aes(x = ts, y = nee)) +
    geom_line(data = site_data %>% dplyr::filter(source == 'abc.co2.ec'), aes(x = ts, y = nee), linetype= "dashed") +
    geom_point(data = site_data %>% dplyr::filter(source == 'abc.co2.ec'), aes(x = ts, y = nee, color = gap_fill_perc_nee)) +
    theme(legend.position = "bottom") +
    scale_color_gradient(low="green", high="red")+
    geom_hline(yintercept = 0) +
    labs(title = paste("EC ", site),
         x = "Date",
         y = "g C m-2 month-1")
  
  # Save the plot to a file
  ggsave(filename = paste0("CO2_EC2gapfill.eitheror/EC_", site, ".jpeg"), plot = p, width = 10, height = 6)
  
  return(p)
})

#look at specific sites
ggplot(data = subset(abc.co2.ec, abc.co2.ec$site_name=='Tombstone_slavin'))+theme_bw()+ggtitle('Tombstone_slavin')+
  geom_hline(yintercept = 0)+
  geom_line(aes(ts,nee, group= site_name))+
  geom_point(aes(ts,nee, color= gap_fill_perc_nee))+
  scale_color_gradient(low="green", high="red")




abc.co2.ch <- abc.co2.ch %>% 
  dplyr::filter(!is.na(nee)) 


x<- abc.co2.ch %>%
  dplyr::filter(as.numeric(chamber_nr_measurement_days_co2) > 3 | is.na(chamber_nr_measurement_days_co2)|chamber_nr_measurement_days_co2 %in% "Continuous" ) %>% 
  dplyr::filter(as.numeric(chamber_nr_measurement_days)> 3 | is.na(chamber_nr_measurement_days))


  



