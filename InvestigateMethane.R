###investigating methane#####
library(dplyr)
library(ggplot2)
library(readr)
library(zoo)
library(ggplot2)

setwd("/Users/iwargowsky/Desktop/ABCFlux v2")  
abc <- read_csv("ABC.v2.apr24.full.csv")

abc.ch4 <- abc %>% filter(!is.na(ch4_flux_total)) #filter for rows with methane fluxs

abc.ch4$ts <- as.yearmon(paste(abc.ch4$year, abc.ch4$month,sep = '-')) #add timestamp

abc.ch4.ec <- abc.ch4 %>% filter(flux_method== "EC") #filter for EC towers
abc.ch4.ch <- abc.ch4 %>% filter(flux_method== "Chamber") #filter for chambers

#find chamber sites with multiple site_reference points (used for graphing)
abc.ch4.ch.siteref <- abc.ch4.ch %>% group_by(site_name) %>%
  dplyr::summarise(n= n_distinct(site_reference))

abc.ch4.ch <- abc.ch4.ch %>%full_join(abc.ch4.ch.siteref)

abc.ch4.ch.multi <- abc.ch4.ch %>% filter(n>1)
abc.ch4.ch.solo <- abc.ch4.ch %>% filter(n==1)




setwd("/Users/iwargowsky/Desktop/ABCFlux v2")
# Plotting CH4 EC and saving each plot
lapply(unique(abc.ch4.ec$site_name), function(site) {
  p <- ggplot(subset(abc.ch4.ec, site_name == site), aes(x = ts, y = ch4_flux_total, color= extraction_source_ch4)) +
    geom_line() +
    geom_point()+
    labs(title = paste("EC CH4", site),
         x = "Date",
         y = "CH4 g C m-2 month-1") +
    theme_minimal()
  
  # Save the plot to a file
  ggsave(filename = paste("CH4 EC2/CH4_EC_", site, ".jpeg"),
         plot = p, width = 10, height = 6)

  return(p)
})


# Plotting CH4 Chamber and saving each plot
lapply(unique(abc.ch4.ch.multi$site_name), function(site) {
  p <- ggplot(subset(abc.ch4.ch.multi, site_name == site), aes(x = ts, y = ch4_flux_total, color = site_reference )) +
    geom_line(group = 1) +
    geom_point()+
    labs(title = paste("Chamber CH4", site),
         x = "Date",
         y = "CH4 g C m-2 month-1") +
    theme_minimal()
  
  # Save the plot to a file
  ggsave(filename = paste("CH4 Chamber/CH4_chamber_", site, ".jpeg"),
         plot = p, width = 10, height = 6)
  
  return(p)
})


lapply(unique(abc.ch4.ch.solo$site_name), function(site) {
  p <- ggplot(subset(abc.ch4.ch.solo, site_name == site), aes(x = ts, y = ch4_flux_total)) +
    geom_line() +
    geom_point()+
    labs(title = paste("Chamber CH4", site),
         x = "Date",
         y = "CH4 g C m-2 month-1") +
    theme_minimal()
  
  # Save the plot to a file
  ggsave(filename = paste("CH4 Chamber/CH4_chamber_", site, ".jpeg"),
         plot = p, width = 10, height = 6)
  
  return(p)
})











abc.ch4.condense <- abc.ch4 %>%
  group_by(site_name, site_reference,  flux_method, land_cover_bawld, latitude, longitude, veg_detail, landform, citation_ch4, permafrost) %>%
  dplyr::summarise(ch4_max= max(ch4_flux_total), ch4_min= min(ch4_flux_total))

setwd("/Users/iwargowsky/Desktop/ABCFlux v2")
write_csv(abc.ch4.condense, "abc.ch4.landcover.csv")


############ SEASONAL ###########

abc.ch4.seasonal <- abc %>%  filter(!if_all(c(ch4_flux_seasonal,
                                     ch4_flux_diffusion,ch4_flux_ebullition, ch4_flux_storage,
                                     ch4_flux_storage_bubble), ~ is.na(.)))
abc.ch4.condense.seasonal <- abc.ch4.seasonal %>%
  group_by(site_name, site_reference,  flux_method, land_cover_bawld, latitude, longitude, veg_detail, landform, citation_ch4, permafrost, ch4_flux_seasonal_interval) %>%
  dplyr::summarise(ch4_season_max= max(ch4_flux_seasonal), ch4_season_min= min(ch4_flux_seasonal),
                   ch4_diff_max= max(ch4_flux_diffusion), ch4_diff_min= min(ch4_flux_diffusion),
                   ch4_eb_max= max(ch4_flux_ebullition), ch4_eb_min= min(ch4_flux_ebullition),
                   ch4_storage_max= max(ch4_flux_storage), ch4_storage_min= min(ch4_flux_storage),
                   ch4_bubble_max= max( ch4_flux_storage_bubble), ch4_bubble_min= min(ch4_flux_storage_bubble))

setwd("/Users/iwargowsky/Desktop/ABCFlux v2")
write_csv(abc.ch4.condense.seasonal, "abc.ch4.seasonal.landcover.csv")


