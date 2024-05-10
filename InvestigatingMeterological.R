##Investigating met variables
library(dplyr)
library(ggplot2)
library(readr)
library(zoo)
library(ggplot2)


setwd("/Users/iwargowsky/Desktop/arcticborealCflux")  
abc <- read_csv("ABC.v2.apr24.csv") %>%
  mutate(tair= as.numeric(tair))

abc$ts <- as.yearmon(paste(abc$year, abc$month,sep = '-')) #add timestamp

abc$extraction_source <- paste("CO2:", abc$extraction_source_co2, "CH4:", abc$extraction_source_ch4, sep= " ")




### TAIR
setwd("/Users/iwargowsky/Desktop/ABCFlux v2")

abc.x <- abc %>% dplyr::filter(!is.na(tair))
                               
lapply(unique(abc.x$site_name), function(site) {
  p <- ggplot(subset(abc.x, site_name == site), aes(x = ts, y = as.numeric(tair), color= extraction_source)) +
    geom_line() +
    geom_point()+
    labs(title = paste("Tair", site),
         x = "Date",
         y = "Air Temperature") +
    theme_minimal()
  
  # Save the plot to a file
  ggsave(filename = paste("Met variables/Tair_", site, ".jpeg"),
         plot = p, width = 10, height = 6)
  
  return(p)
})


abc.x <- abc.x %>% group_by(site_name, month) %>%
  dplyr::summarise(tair= mean(tair, na.rm= T))

ggplot(abc.x)+
  geom_point(aes(month, tair))



### Precip
setwd("/Users/iwargowsky/Desktop/ABCFlux v2")

abc.x <- abc %>% dplyr::filter(!is.na(precip))

lapply(unique(abc.x$site_name), function(site) {
  p <- ggplot(subset(abc.x, site_name == site), aes(x = ts, y = as.numeric(precip), color= extraction_source)) +
    geom_line() +
    geom_point()+
    labs(title = paste("precip", site),
         x = "Date",
         y = "precip") +
    theme_minimal()
  
  # Save the plot to a file
  ggsave(filename = paste("Met variables/precip_", site, ".jpeg"),
         plot = p, width = 10, height = 6)
  
  return(p)
})

abc.x <- abc.x %>% group_by(site_name, month) %>%
  dplyr::summarise(precip= mean(precip, na.rm= T))

ggplot(abc.x)+
  geom_point(aes(month, precip))




### soil temps
setwd("/Users/iwargowsky/Desktop/ABCFlux v2")

abc.x <- abc %>% dplyr::filter(!(is.na(tsoil_surface)& is.na(tsoil_deep)))

lapply(unique(abc.x$site_name), function(site) {
  p <- ggplot(subset(abc.x, site_name == site)) +
    geom_line( aes(x = ts, y = as.numeric(tsoil_surface), color= extraction_source)) +
    geom_point( aes(x = ts, y = as.numeric(tsoil_surface), color= extraction_source))+
    geom_line( aes(x = ts, y = as.numeric(tsoil_deep), color= extraction_source), linetype= "dashed") +
    geom_point( aes(x = ts, y = as.numeric(tsoil_deep), color= extraction_source))+
    labs(title = paste("Soil temps", site),
         x = "Date",
         y = "soil temps") +
    theme_minimal()
  
  # Save the plot to a file
  ggsave(filename = paste("Met variables/soiltemps_", site, ".jpeg"),
         plot = p, width = 10, height = 6)
  
  return(p)
})

abc.x <- abc.x %>% group_by(site_name, month) %>%
  dplyr::summarise(tsoil_surface= mean(as.numeric(tsoil_surface), na.rm= T),
                   tsoil_deep= mean(as.numeric(tsoil_deep), na.rm= T))

ggplot(abc.x)+
  geom_point(aes(month, tsoil_surface))

ggplot(abc.x)+
  geom_point(aes(month, tsoil_deep))


### water table
setwd("/Users/iwargowsky/Desktop/ABCFlux v2")

abc.x <- abc %>% dplyr::filter(!(is.na(water_table_depth)))

lapply(unique(abc.x$site_name), function(site) {
  p <- ggplot(subset(abc.x, site_name == site)) +
    geom_line( aes(x = ts, y = as.numeric(water_table_depth), color= extraction_source)) +
    geom_point( aes(x = ts, y = as.numeric(water_table_depth), color= extraction_source))+
    labs(title = paste("Water table depth", site),
         x = "Date",
         y = "Water table depth") +
    theme_minimal()
  
  # Save the plot to a file
  ggsave(filename = paste("Met variables/watertable_", site, ".jpeg"),
         plot = p, width = 10, height = 6)
  
  return(p)
})

abc.x <- abc.x %>% group_by(site_name, month) %>%
  dplyr::summarise(water_table_depth= mean(as.numeric(water_table_depth), na.rm= T))

ggplot(abc.x)+
  geom_point(aes(month, water_table_depth))




### Soil moisture
setwd("/Users/iwargowsky/Desktop/ABCFlux v2")

abc.x <- abc %>% dplyr::filter(!(is.na(soil_moisture)))

lapply(unique(abc.x$site_name), function(site) {
  p <- ggplot(subset(abc.x, site_name == site)) +
    geom_line( aes(x = ts, y = as.numeric(soil_moisture), color= extraction_source)) +
    geom_point( aes(x = ts, y = as.numeric(soil_moisture), color= extraction_source))+
    labs(title = paste("soil_moisture", site),
         x = "Date",
         y = "soil_moisture") +
    theme_minimal()
  
  # Save the plot to a file
  ggsave(filename = paste("Met variables/soilmoisture_", site, ".jpeg"),
         plot = p, width = 10, height = 6)
  
  return(p)
})

abc.x <- abc.x %>% group_by(site_name, month) %>%
  dplyr::summarise(soil_moisture= mean(as.numeric(soil_moisture), na.rm= T))

ggplot(abc.x)+
  geom_point(aes(month, soil_moisture))



### active layer thickness
setwd("/Users/iwargowsky/Desktop/ABCFlux v2")

abc.x <- abc %>% dplyr::filter(!(is.na(alt)))

lapply(unique(abc.x$site_name), function(site) {
  p <- ggplot(subset(abc.x, site_name == site)) +
    geom_line( aes(x = ts, y = as.numeric(alt), color= extraction_source)) +
    geom_point( aes(x = ts, y = as.numeric(alt), color= extraction_source))+
    labs(title = paste("alt", site),
         x = "Date",
         y = "alt (cm)") +
    theme_minimal()

  # Save the plot to a file
  ggsave(filename = paste("Met variables/alt_", site, ".jpeg"),
         plot = p, width = 10, height = 6)

  return(p)
})

abc.x <- abc.x %>% group_by(site_name, month) %>%
  dplyr::summarise(alt= mean(as.numeric(alt), na.rm= T))

ggplot(abc.x)+
  geom_point(aes(month, alt))


### Thaw depth
setwd("/Users/iwargowsky/Desktop/ABCFlux v2")

abc.x <- abc %>% dplyr::filter(!(is.na(thaw_depth)))

lapply(unique(abc.x$site_name), function(site) {
  p <- ggplot(subset(abc.x, site_name == site)) +
    geom_line( aes(x = ts, y = as.numeric(thaw_depth), color= extraction_source)) +
    geom_point( aes(x = ts, y = as.numeric(thaw_depth), color= extraction_source))+
    labs(title = paste("thaw_depth", site),
         x = "Date",
         y = "thaw_depth(cm)") +
    theme_minimal()
  
  # Save the plot to a file
  ggsave(filename = paste("Met variables/thaw_depth_", site, ".jpeg"),
         plot = p, width = 10, height = 6)
  
  return(p)
})

abc.x <- abc.x %>% group_by(site_name, month) %>%
  dplyr::summarise(thaw_depth= mean(as.numeric(thaw_depth), na.rm= T))

ggplot(abc.x)+
  geom_point(aes(month, thaw_depth))



### snow depth
setwd("/Users/iwargowsky/Desktop/ABCFlux v2")

abc.x <- abc %>% dplyr::filter(!(is.na(snow_depth)))

lapply(unique(abc.x$site_name), function(site) {
  p <- ggplot(subset(abc.x, site_name == site)) +
    geom_line( aes(x = ts, y = as.numeric(snow_depth), color= extraction_source)) +
    geom_point( aes(x = ts, y = as.numeric(snow_depth), color= extraction_source))+
    labs(title = paste("snow_depth", site),
         x = "Date",
         y = "snow_depth(cm)") +
    theme_minimal()
  
  # Save the plot to a file
  ggsave(filename = paste("Met variables/snow_depth_", site, ".jpeg"),
         plot = p, width = 10, height = 6)
  
  return(p)
})

abc.x <- abc.x %>% group_by(site_name, month) %>%
  dplyr::summarise(snow_depth= mean(as.numeric(snow_depth), na.rm= T))

ggplot(abc.x)+
  geom_point(aes(month, snow_depth))


### ppfd
setwd("/Users/iwargowsky/Desktop/ABCFlux v2")

abc.x <- abc %>% dplyr::filter(!(is.na(ppfd)))

lapply(unique(abc.x$site_name), function(site) {
  p <- ggplot(subset(abc.x, site_name == site)) +
    geom_line( aes(x = ts, y = as.numeric(ppfd), color= extraction_source)) +
    geom_point( aes(x = ts, y = as.numeric(ppfd), color= extraction_source))+
    labs(title = paste("ppfd", site),
         x = "Date",
         y = "ppfd") +
    theme_minimal()
  
  # Save the plot to a file
  ggsave(filename = paste("Met variables/ppfd_", site, ".jpeg"),
         plot = p, width = 10, height = 6)
  
  return(p)
})

abc.x <- abc.x %>% group_by(site_name, month) %>%
  dplyr::summarise(ppfd= mean(as.numeric(ppfd), na.rm= T))

ggplot(abc.x)+
  geom_point(aes(month, ppfd))

setwd("/Users/iwargowsky/Desktop/ABCFlux v2")

abc.x <- abc %>% dplyr::filter(!(is.na(ppfd)))

