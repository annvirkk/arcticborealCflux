##Calculating QC from half-hourly vs daily measurements with ICOS data
library(dplyr)
library(ggplot2)
library(purrr)
library(readr)
library(tidyverse)
#load in a DAILY the files
setwd("/Users/iwargowsky/Desktop/ICOS/Warm Winters/FLX_FI-Hyy_FLUXNET2015_FULLSET_1996-2020_beta-3")
#loading DAILY files
DDicos <-   read_csv("FLX_FI-Hyy_FLUXNET2015_FULLSET_DD_1996-2020_beta-3.csv", na=c("NA","-9999"))
HHicos <-   read_csv("FLX_FI-Hyy_FLUXNET2015_FULLSET_HH_1996-2020_beta-3.csv", na=c("NA","-9999"))
##Subset for just some QC variables
DDicos <- DDicos %>% dplyr::select(TIMESTAMP, NEE_CUT_REF, NEE_CUT_REF_QC)
HHicos <- HHicos %>% dplyr::select(TIMESTAMP_START, NEE_CUT_REF, NEE_CUT_REF_QC)
### According to https://fluxnet.org/data/fluxnet2015-dataset/variables-quick-start-guide/
#DD QC values are 0-1 indicating percentage of measured (*_QC=0) or good quality gap-filled
#(*_QC=1) records aggregated from finer temporal resolutions.
 
#Whereas at the half-hourly (HH) or hourly resolution (HH), the _QC variable 
#indicates if the corresponding record is a measured value (*_QC=0), or the quality level
#of the gap-filling that was used for that record (*_QC=1 better, *_QC=3 worse quality)

#Convert HH QC (0,1,2,3) to DD QC (0-1)
#add year,month, day columns
HHicos$year <- substr(HHicos$TIMESTAMP_START, 1,4)
HHicos$month <- substr(HHicos$TIMESTAMP_START, 5,6)
HHicos$day <- substr(HHicos$TIMESTAMP_START, 7,8)

#function to get QC 0-1 
HHtoDD  <- HHicos  %>%  
  mutate(QualScore = case_when(
    NEE_CUT_REF_QC %in% c(0,1) ~ 1,
    NEE_CUT_REF_QC %in% c(2,3) ~ 0)) %>%
  group_by(year,month,day) %>% 
  summarise(NEE_CUT_REF_QC = (sum(QualScore))/(n()),
            NEE_CUT_REF= mean(NEE_CUT_REF))


#quick plot to visual compare DD QC to our calculated QC
HHtoDD$ts = as.Date(paste(HHtoDD$year,HHtoDD$month, HHtoDD$day, sep = '-'))
DDicos$year <- substr(DDicos$TIMESTAMP, 1,4)
DDicos$month <- substr(DDicos$TIMESTAMP, 5,6)
DDicos$day <- substr(DDicos$TIMESTAMP, 7,8)
DDicos$ts = as.Date(paste(DDicos$year,DDicos$month, DDicos$day, sep = '-'))

ggplot()+theme_bw()+ggtitle('FI-Hyy')+
  geom_point(data=DDicos, aes(ts,NEE_CUT_REF_QC, color="DD"))+
  geom_point(data=HHtoDD, aes(ts,NEE_CUT_REF_QC, color= "HHtoDD"))

#check R-squared
model <- lm(DDicos$NEE_CUT_REF_QC~HHtoDD$NEE_CUT_REF_QC)
summary(model)

####one flux code https://github.com/fluxnet/ONEFlux/blob/main/oneflux/pipeline/site_data_product.py





####get % gap fill from HH data ####
setwd("/Users/iwargowsky/Desktop/ICOS/ICOSETC")
path <- "/Users/iwargowsky/Desktop/ICOS/ICOSETC"
files <- list.files(path = path,pattern = '*_HH_',all.files = T,recursive = T)
#load in files as a list of df
icosetc.dat <- lapply(files,function(i){
  fread(i, na.strings =c("NA","-9999"), header = TRUE)
})
names(icosetc.dat)<- files #name each df
#add year, month, day columns
icosetc.dat2 <- lapply(icosetc.dat, function(df) df %>%
                         mutate( year = substr(df$TIMESTAMP_START, 1,4),
                                 month = substr(df$TIMESTAMP_START, 5,6),
                                 day = substr(df$TIMESTAMP_START, 7,8) ))
#replace 1,2,3 with 1, sum and divide by 48 to get gapfill percentage per day
icosetc.dat3 <- lapply(icosetc.dat2, function(df) df %>%
                         mutate( year = substr(df$TIMESTAMP_START, 1,4),
                                 month = substr(df$TIMESTAMP_START, 5,6),
                                 day = substr(df$TIMESTAMP_START, 7,8) ) %>%
                         mutate(gapfill = case_when(NEE_VUT_REF_QC %in% c(1,2,3) ~ 1))%>%
                         dplyr::select(year, month, day, gapfill, NEE_VUT_REF_QC) %>%
                         group_by(year,month,day) %>% 
                         dplyr::summarise(gapfillpercent = sum(gapfill, na.rm=TRUE)/n()))
                         
                         
                         
                         
                         
                     