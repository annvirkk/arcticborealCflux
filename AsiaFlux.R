###Asia Flux data
library(dplyr)
library(purrr)
library(readr)
library(lubridate)
library(data.table)
library(zoo)
setwd("/Users/iwargowsky/Desktop/AsiaFlux")
path <- "/Users/iwargowsky/Desktop/AsiaFlux"
list_of_files <- list.files(path = path,pattern = '*.csv',all.files = T,recursive = T)
# Initialize an empty list to store the data frames
asiadatlist <- list()
# Loop through the list of files
for (i in list_of_files) {
  df <- read_csv(i, na = c("NA", "-99999", "9999"))
  df$site_id <- substr(i, 1, 6)
  df$year <- substr(i, 17, 20)
  asiadatlist[[i]] <- df
}
#condense to single df
asiadat <- rbindlist(asiadatlist, fill = TRUE)
#Average by time to have consistent column names
asiadat <- asiadat %>% group_by(Year, DOY, TIME, site_id) %>%
  dplyr::summarise(tair = mean(as.numeric(c(Ta, Ta_31.4, Ta_24.6, Ta_5.7, Ta_1.8, Ta_17.2, Ta_13.4, 
                                            `Ta _18`, Ta_6, Ta_24, Ta_32)), na.rm = TRUE),
                   precip= mean(as.numeric(PPT)),
                   tsoil_surface= mean(as.numeric(c(Ts, Ts_10)), na.rm = TRUE ),
                   tsoil_deep= mean(as.numeric(Ts_20), na.rm = TRUE),
                   soil_moisture= mean(as.numeric(c(SWC, SWC_10)), na.rm = TRUE),
                   Fc = mean(as.numeric(Fc), na.rm = TRUE))
#add column for year and month
asiadat$year <- asiadat$Year
asiadat$month <- month(as.Date(paste(asiadat$year, asiadat$DOY, sep = "-"), format = "%Y-%j"))

#Average by month
asiadat.monthly <- asiadat %>% group_by(year, month, site_id) %>%
  dplyr::summarise(precip= sum(precip),
            percent_na_tsoil_surface  = (sum(is.na(tsoil_surface))/n()*100),
            tsoil_surface= mean(tsoil_surface, na.rm = TRUE ),
            percent_na_tsoil_deep  = (sum(is.na(tsoil_deep))/n()*100),
            tsoil_deep= mean(tsoil_deep, na.rm = TRUE),
            percent_na_soil_moisture  = (sum(is.na(soil_moisture))/n()*100),
            soil_moisture= mean(soil_moisture, na.rm = TRUE),
            gap_fill_perc_nee = (sum(is.na(Fc))/n()*100))

#remove soil vars with less than 70% percent of data
asiadat.monthly <- asiadat.monthly %>% mutate(tsoil_surface = ifelse(percent_na_tsoil_surface< 30, tsoil_surface, NA)) %>%
  mutate(tsoil_deep = ifelse(percent_na_tsoil_deep< 30, tsoil_deep, NA)) %>%
  mutate(soil_moisture = ifelse(percent_na_soil_moisture< 30, soil_moisture, NA))

#remove unwanted columns 
asiadat.monthly$percent_na_tsoil_surface <- NULL
asiadat.monthly$percent_na_tsoil_deep <- NULL
asiadat.monthly$percent_na_soil_moisture <- NULL

asiadat.monthly <- asiadat.monthly %>% dplyr::filter(!site_id== "RU-TUR")

#### 1/17/2023 load gapfilled data#####-------------------------------------------------------

setwd("/Users/iwargowsky/Desktop/gapfill and partitioning/data")
path <- "/Users/iwargowsky/Desktop/gapfill and partitioning/data"
files <- list.files(path = path,pattern = '*gapfilled_clean',all.files = T,recursive = T)
df <- files %>%
  setNames(nm = .) %>% 
  map_df(~read_csv(.x, col_types = cols(), col_names = TRUE, na=c("NA","-9999")), .id = "site_id")  

#add site names
df$site_id <- substr(df$site_id, 1,3)
df <- df %>% mutate(site_id= case_when(site_id %in% "kbu"~ "MO-KBU",
                                       site_id %in% "skt"~ "MO-SKT",
                                       site_id %in% "ypf"~ "RU-YPF",
                                       site_id %in% "ylf"~ "RU-YLF"))
#add month column
df <- df %>% mutate(month = month(timestamp),
                    year= year(timestamp))
#summarize by month 
df.monthly <- df %>% group_by(site_id, year, month) %>%
  dplyr::summarise(nee= mean(NEE_f, na.rm= FALSE),
            reco= mean(Reco, na.rm= FALSE),
            gpp= mean(GPP_f, na.rm = FALSE),
            tair = mean(Tair_f, na.rm=TRUE))
df.monthly$nee <- df.monthly$nee *1.0368 *days_in_month(as.yearmon(paste(df.monthly$year, df.monthly$month,sep = '-')))
df.monthly$reco <- df.monthly$reco *1.0368 *days_in_month(as.yearmon(paste(df.monthly$year, df.monthly$month,sep = '-')))
df.monthly$gpp <- df.monthly$gpp *-1.0368 *days_in_month(as.yearmon(paste(df.monthly$year, df.monthly$month,sep = '-')))


#merge with meteo data
asiadat.monthly$year <- as.numeric(asiadat.monthly$year)
all.monthly <- df.monthly %>% full_join(asiadat.monthly, by= c("year", "month", "site_id"))
#remove rows without flux data

all.monthly <- all.monthly %>% dplyr::filter(!is.na(nee))

all.monthly$extraction_source <- "AsiaFlux"
all.monthly$notes <- "tsoil_surface, tsoil_deep, soil_moisture data comes from non-gapfilled data where 70% of data was present for that month"
all.monthly$gap_fill <- "MDS ReddyProc"
all.monthly$partition_method <- "Reichstein"


#Renaming RU-YLF to RU-SkP because they are the same site
all.monthly <- all.monthly %>%
  mutate(site_id= ifelse(site_id %in% "RU-YLF", "RU-SkP", site_id)) %>%
  mutate(notes = ifelse(site_id %in% "RU-SkP", paste(notes, " AsiaFlux site code is YLF", sep = ";"), notes))


#save off
setwd("/Users/iwargowsky/Desktop/ABCFlux v2")
write_csv(all.monthly, "asia.monthly.csv")

#------------------------------------------------------------------------------------------------------------
#plot to compare tair and tair_f
all.monthly$ts <- as.yearmon(paste(all.monthly$year, all.monthly$month, sep="-"))
ggplot(data = subset(all.monthly,all.monthly$site_id=='MO-KBU'))+theme_bw()+ggtitle('MO-KBU')+
  geom_hline(yintercept = 0)+
  geom_line(aes(ts,tair_f), color= "red", size= 2)+
  geom_line(aes(ts,tair), color= "blue", size= 1)
ggplot(data = subset(all.monthly,all.monthly$site_id=='MO-SKT'))+theme_bw()+ggtitle('MO-SKT')+
  geom_hline(yintercept = 0)+
  geom_line(aes(ts,tair_f), color= "red", size= 2)+
  geom_line(aes(ts,tair), color= "blue", size= 1)
ggplot(data = subset(all.monthly,all.monthly$site_id=='RU-YPF'))+theme_bw()+ggtitle('RU-YPF')+
  geom_hline(yintercept = 0)+
  geom_line(aes(ts,tair_f), color= "red", size= 2)+
  geom_line(aes(ts,tair), color= "blue", size= 1)
ggplot(data = subset(all.monthly,all.monthly$site_id=='RU-YLF'))+theme_bw()+ggtitle('RU-YLF')+
  geom_hline(yintercept = 0)+
  geom_line(aes(ts,tair_f), color= "red", size= 2)+
  geom_line(aes(ts,tair), color= "blue", size= 1)



ggplot(data = all.monthly)+theme_bw()+ggtitle('Soil temp surface')+
  geom_hline(yintercept = 0)+
  geom_line(aes(ts,tsoil_surface , group= site_id, color= site_id))

ggplot(data = all.monthly)+theme_bw()+ggtitle('Soil temp deep')+
  geom_hline(yintercept = 0)+
  geom_line(aes(ts,tsoil_deep , group= site_id, color= site_id))

ggplot(data = all.monthly)+theme_bw()+ggtitle('Soil moisture')+
  geom_hline(yintercept = 0)+
  geom_line(aes(ts,soil_moisture , group= site_id, color= site_id))
