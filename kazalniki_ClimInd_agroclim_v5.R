setwd("C:/Users/ZalaZn/OneDrive - Univerza v Ljubljani/projekti/CRP kazalniki 2024") # služba
library(graphics)
library(ismev)
library(readxl)
library(ie2misc)
library(ggplot2)
library(openxlsx)
library(dplyr)
library(pollen)
library(tidyr)
library(chillR)
library(ChillModels)
library(tidyverse)
library(viridis)
library(dplyr)

library(ggspatial)
library(sf)
library(ncdf4)
library(ClimInd)
library(haven)
library(lubridate)
library(fruclimadapt)

###################################################################
# Open a connection to the first file in our list
# path <- "C:/Users/zalaz/OneDrive - Univerza v Ljubljani/DR/podatki_hist/OPSI/" # doma
path <- "C:/Users/ZalaZn/OneDrive - Univerza v Ljubljani/DR/podatki_hist/OPSI/" # sluzba
# nc_data <- nc_open(paste0(path,"tg","_ens_mean_0.1deg_reg_v27.0e.nc"))


# nuts3_mapdata <- st_read("C:/Users/zalaz/Work Folders/ZALA BF delo/MR/DR/NUTS3_ID.gpkg")
nuts3_mapdata <- st_read("C:/Users/ZalaZn/OneDrive - Univerza v Ljubljani/DR/NUTS3_ID.gpkg")
slovenia_nuts3_mapdata <- filter(nuts3_mapdata, cntr_code=="SI") # filtriram samo slovenske NUTS3 regije

nc_data <- nc_open(paste0(path,"evspsblpot","_12km_ARSO_v5_day_19810101_20101231.nc"))
names(nc_data$var)
print(nc_data)
lon <- ncvar_get(nc_data, "X") # drugi parameter je ime spremenljivke v datoteki
lat <- ncvar_get(nc_data, "Y")
start_lon <- 13.2 # meje https://sl.wikipedia.org/wiki/Geografija_Slovenije
end_lon <- 16.8

start_lat<-45.3 #+ 25/60 +18.34/3600
end_lat<-47 #+ 54/60 + 37.52/3600

time <- ncvar_get(nc_data, "time")
nx = length(lon)
ny = length(lat)
start1<-c(1,1,1)
count1<-c(nx,ny, 10957)
############################################################# 
# ET
data <- ncvar_get(nc_data,"evspsblpot",start=start1, count=count1)
data_et <- data
#############################################################
library(pracma)
library(spData)
library(sf)

pretvorba_sek_v_dan1 = 60*60*24 #?eprav imam dnevne podatke moram to dat, ker as.positxct meri v sekundah

########### 
# povpre?na dnevna T
datumi0 <- seq(0,count1[3]-1,by = 1)
datumi <- as.POSIXct((start1[3]+datumi0)*pretvorba_sek_v_dan1,origin="1980-12-31 00:00:00")
leta <- format(datumi,format="%Y")
meseci <- format(datumi,format="%m")
dnevi <- format(datumi,format="%d")

nc_data <- nc_open(paste0(path,"tas","_12km_ARSO_v5_day_19810101_20101231.nc"))
data3 <- ncvar_get(nc_data,"tas",start=start1, count=count1)
# d3 <- flipdim(dataPOVP, 2)
datum2 = format(as.POSIXct(datumi, format="%Y-%m-%d"), "%m/%d/%Y")
# dataPOVP<-structure(data3, .Names = datum2)
# data3 <- data.frame(datum = datum2, leto = leta, tg = data3[,,1:10957])
nc_close(nc_data) # konec branja

# minimalna dnevna T
nc_data_min <- nc_open(paste0(path,"tasmin","_12km_ARSO_v5_day_19810101_20101231.nc"))
data_min <- ncvar_get(nc_data_min, "tasmin", start=start1, count=count1)
dataMIN<-structure(data_min, .Names = datum2)
# data_min <- data.frame(datum = datumi, leto = leta, tn = data_min)
nc_close(nc_data_min) # konec branja

# maksimalna dnevna T
nc_data_max<- nc_open(paste0(path,"tasmax","_12km_ARSO_v5_day_19810101_20101231.nc"))
data_max <- ncvar_get(nc_data_max, "tasmax", start=start1, count=count1)
# dataMAX<-structure(data_max, .Names = datum2)
# data_max <- data.frame(datum = datumi, leto = leta, tx = data_max)
nc_close(nc_data_max) # konec branja

# padavine dnevne
nc_data_rr<- nc_open(paste0(path,"pr","_12km_ARSO_v5_day_19810101_20101231.nc"))
data_rr <- ncvar_get(nc_data_rr, "pr", start=start1, count=count1)
# dataRR<-structure(data_rr, .Names = datum2)
# data_rr <- data.frame(datum = datumi, leto = leta, rr = data_rr)
nc_close(nc_data_rr) # konec branja

# evapotranspiracija
nc_data_et<- nc_open(paste0(path,"evspsblpot","_12km_ARSO_v5_day_19810101_20101231.nc"))
data_et <- ncvar_get(nc_data_et, "evspsblpot", start=start1, count=count1)
nc_close(nc_data_et) # konec branja

# celotna bli?nja okolica Slovenije, letne vrednosti agroklimatskih kazalnikov za vseh 70 let izra?unamo

lats1 <- seq(1,24,by=1)
lons1 <- seq(1,40,by=1)


########################## 
j = 12
i = 20

podatki_percentil_90_tmin<-data.frame(latsi=1,lonsi=1,T90p=1)
podatki_percentil_99_tmax<-data.frame(latsi=1,lonsi=1,Tx99p=1)
podatki_percentil_90_tmax<-data.frame(latsi=1,lonsi=1,Tx90p=1)
podatki_percentil_75_tmean<-data.frame(latsi=1,lonsi=1,T75p=1)
podatki_percentil_75_rr<-data.frame(latsi=1,lonsi=1,rr75p=1)


vsi_podatki <- data.frame()
for(i in lons1){
  for(j in lats1){
    print(i)
    print(j)
    if(all(is.na(data3[i,j,1:10957]))){
      next
    }
    else{
      data_temp <- data.frame(Date = datum2, meseci = as.numeric(format(datumi,format="%m")), Temp = data3[i,j,1:10957]-273)
      data_temp <- data_temp %>% filter(meseci >= 4, meseci <= 10)
      
      data_tmax <- data.frame(Date = datum2, meseci = as.numeric(format(datumi,format="%m")), Tmax = data_max[i,j,1:10957]-273)
      data_tmax <- data_tmax %>% filter(meseci >= 4, meseci <= 10)
      skupni_prag_tmax99p <- as.numeric(quantile(data_tmax$Tmax, 0.99))
      skupni_prag_tmax90p <- as.numeric(quantile(data_tmax$Tmax, 0.90))
      
      data_tmin <- data.frame(Date = datum2, meseci = as.numeric(format(datumi,format="%m")), Tmin = data_min[i,j,1:10957]-273)
      data_tmin <- data_tmin %>% filter(meseci >= 4, meseci <= 10)
      skupni_prag_tmin90p <- as.numeric(quantile(data_tmin$Tmin, 0.9))
      
      # percentil90<-data.frame(latsi=lat[j],lonsi=lon[i],T90p=as.numeric(quantile(data_tmin$Tmin,0.9))) # tega ne rabim za proj.
      #podatki_percentil_90_tmin = readrRDS("tn90p_referencno_obdobje.rds")
      # podatki_percentil_90_tmin<-rbind(podatki_percentil_90_tmin,percentil90) # tega ne rabim za proj.
      # podatki_percentil_90_tmin2<- subset(podatki_percentil_90_tmin, as.numeric(latsi)==lat[j]  & podatki_percentil_90_tmin$lon==as.numeric(lon[i]))
      # tn90p = podatki_percentil_90_tmin2$T90p
      data_tmin_90p<-data_tmin
      # data_tmin_90p$Datum <-as.Date(data_tmin_90p$Date)
      data_tmin_90p$leto <-format(as.POSIXct(data_tmin_90p$Date, format="%m/%d/%Y"), "%Y")
      t90_test <- data_tmin_90p %>% 
        mutate(tmin90 = ifelse(Tmin >= skupni_prag_tmin90p,1,0),ind1=1)
      t90_test <- t90_test %>%
        group_by(leto) %>% summarise(sum=sum(tmin90),
                  n=sum(ind1), tn90p = sum/n *100)
      
      # percentil99x<-data.frame(latsi=lat[j],lonsi=lon[i],Tx99p=as.numeric(quantile(data_tmax$Tmax,0.99))) # tega ne rabim za proj.
      # #podatki_percentil_99_tmax = readrRDS("tx99p_referencno_obdobje.rds")
      # podatki_percentil_99_tmax<-rbind(podatki_percentil_99_tmax,percentil99x) # tega ne rabim za proj.
      # podatki_percentil_99_tmax2<- subset(podatki_percentil_99_tmax, as.numeric(latsi)==lat[j]  & podatki_percentil_99_tmax$lon==as.numeric(lon[i]))
      # tx99p = podatki_percentil_99_tmax2$Tx99p
      data_tmax_99p<-data_tmax
      # data_tmax_99p$Datum <-as.Date(data_tmax_99p$Date)
      data_tmax_99p$leto <-format(as.POSIXct(data_tmax_99p$Date, format="%m/%d/%Y"), "%Y")
      tx99_test <- data_tmax_99p %>% 
        mutate(tmax99 = ifelse(Tmax >= skupni_prag_tmax99p,1,0),ind1=1)
      tx99_test <- tx99_test %>%
        group_by(leto) %>% summarise(sum=sum(tmax99),
                                     n=sum(ind1), tx99p = sum)
      
      # percentil90x<-data.frame(latsi=lat[j],lonsi=lon[i],Tx90p=as.numeric(quantile(data_tmax$Tmax,0.90))) # tega ne rabim za proj.
      # #podatki_percentil_90_tmax = readrRDS("tx90p_referencno_obdobje.rds")
      # podatki_percentil_90_tmax<-rbind(podatki_percentil_90_tmax,percentil90x) # tega ne rabim za proj.
      # podatki_percentil_90_tmin2<- subset(podatki_percentil_90_tmax, as.numeric(latsi)==lat[j]  & podatki_percentil_90_tmax$lon==as.numeric(lon[i]))
      # tx90p = podatki_percentil_90_tmin2$Tx90p
      # data_tmax_90p<-data_tmax
      # data_tmax_90p$leto <-format(as.POSIXct(data_tmax_90p$Date, format="%m/%d/%Y"), "%Y")
      # tx90_test <- data_tmax_90p %>% 
      #   mutate(tmax90 = ifelse(Tmax >= percentil90x$Tx90p,1,0),ind1=1)
      # tx90_test <- tx90_test %>%
      #   group_by(leto) %>% summarise(sum=sum(tmax90),
      #                                n=sum(ind1), tx90p = sum)
      data_tmax_90p <- data_tmax %>%
        mutate(leto = format(as.POSIXct(Date, format = "%m/%d/%Y"), "%Y"),
          tmax90 = ifelse(Tmax >= skupni_prag_tmax90p, 1, 0),
          ind1 = 1)
      consecutive_days_list <- data_tmax_90p %>%
        group_by(leto) %>%
        group_split() %>%
        lapply(function(year_data) {above_threshold <- year_data$tmax90
          consecutive_groups <- rle(above_threshold)
          consecutive_days <- sum(consecutive_groups$lengths[consecutive_groups$values == 1 & consecutive_groups$lengths >= 6])
          return(consecutive_days)
        })
      consecutive_days_df <- data.frame(leto = unique(data_tmax_90p$leto), consecutive_days = unlist(consecutive_days_list))
      summary_df <- data_tmax_90p %>%
        group_by(leto) %>%
        summarise(sum = sum(tmax90),n = sum(ind1))
      result_df <- left_join(summary_df, consecutive_days_df, by = "leto")
      
        
      data_pad0 <- data.frame(Date = datum2, meseci = as.numeric(format(datumi,format="%m")), pad = data_rr[i,j,1:10957]*24*60*60)
      data_pad <- data_pad0 %>% filter(meseci >= 4, meseci <= 10)
      
      data_et0 <- data.frame(Date = datum2, meseci = as.numeric(format(datumi,format="%m")), et = data_et[i,j,1:10957]*24*60*60)
      data_etp <- data_et0 %>% filter(meseci >= 4, meseci <= 10)
      
      growing_degree_days <- gd4(structure(data_temp$Temp, .Names = data_temp$Date))
      Growing_season_length <- gsl(structure(data_temp$Temp, .Names = data_temp$Date)) 
      End_growing_season <- ogs6(structure(data_temp$Temp, .Names = data_temp$Date)) + Growing_season_length + 90
      Sums_Tmax32 <- stx32(structure(data_tmax$Tmax, .Names = data_tmax$Date)) ###
      TG_of_warmest_quarter <- bio10(structure(data_temp$Temp, .Names = data_temp$Date)) #Kelvini
      dtr <- (data_tmax$Tmax+273)/(data_temp$Temp+273)
      dtr1 <- (data_tmin$Tmin+273)/(data_temp$Temp+273)
      Diurnal_temp_rangeN <-	dtr(structure(dtr, .Names = data_temp$Date),structure(dtr1, .Names = data_temp$Date))
      # Days_Tmax32	<- d32(structure(data_tmax$Tmax, .Names = data_tmax$Date)) ###
      cons_summer_days <- csd(structure(data_tmax$Tmax, .Names = data_tmax$Date))
      # Warm_spell_duration <- wsdi(structure(data_tmax$Tmax, .Names = data_tmax$Date))

      df_spei_spi <- data.frame(Date = data_etp$Date, ET = data_etp$et, RR = data_pad$pad)
      df_spei_spi <- df_spei_spi %>%
        mutate(Year = format(as.POSIXct(Date, format="%m/%d/%Y"), "%Y"),
               Month = format(as.POSIXct(Date, format="%m/%d/%Y"), "%m"),
               Day = format(as.POSIXct(Date, format="%m/%d/%Y"), "%d")) %>%
        group_by(Year, Month) %>%  # Monthly aggregation
        summarize(ET_monthly = sum(ET), RR_monthly = sum(RR)) %>%
        ungroup()
      df_spei_spi$WB <- df_spei_spi$RR_monthly - df_spei_spi$ET_monthly # monthly climatic water balance (precipitation - potential evapotranspiration)
      spei_3 <- spei(df_spei_spi$WB, scale = 3)
      df_spei_spi$SPEI_3 <- spei_3$fitted
      spei_6 <- spei(df_spei_spi$WB, scale = 6)
      df_spei_spi$SPEI_6 <- spei_6$fitted
      rr_monthly <- data.frame(Date = data_etp$Date, ET = data_etp$et, RR = data_pad$pad) %>%
        mutate(Year = format(as.POSIXct(Date, format="%m/%d/%Y"), "%Y"),
               Month = format(as.POSIXct(Date, format="%m/%d/%Y"), "%m"),
               Day = format(as.POSIXct(Date, format="%m/%d/%Y"), "%d")) %>%
        group_by(Year, Month) %>%
        summarize(RR_monthly = sum(RR)) %>%
        ungroup()
      spi_3 <- spi(rr_monthly$RR_monthly, scale = 3)
      df_spei_spi$SPI_3 <- spi_3$fitted
      spi_6 <- spi(rr_monthly$RR_monthly, scale = 6)
      df_spei_spi$SPI_6 <- spi_6$fitted
      
      # Dry_days <- dd(structure(data_pad$pad, .Names = data_pad$Date)) 
      # Prec_deficit	<- -1*ep(structure(data_etp$et, .Names = data_etp$Date), structure(data_pad$pad, .Names = data_pad$Date))
      Max_consecutive_dry_days <- cdd(structure(data_pad$pad, .Names = data_pad$Date))
      longest_wet_period <- cwd(structure(data_pad$pad, .Names = data_pad$Date))    
      # r10mm <- r10mm(structure(data_pad$pad, .Names = data_pad$Date)) 
      r20mm <- r20mm(structure(data_pad$pad, .Names = data_pad$Date))
      Heavy_prec_days <- d50mm(structure(data_pad$pad, .Names = data_pad$Date)) 
      # wet_days <- dr1mm(structure(data_pad$pad, .Names = data_pad$Date)) 
      SDII <- sdii(structure(data_pad$pad, .Names = data_pad$Date))
      Prec_wettest_month	<- bio13(structure(data_pad$pad, .Names = data_pad$Date)) 
      # Prec_warmest_quarter	<- bio18(structure(data_pad$pad, .Names = data_pad$Date),structure(data_temp$Temp, .Names = data_temp$Date)) 
      # Prec_coldest_quarter	<- bio19(structure(data_pad$pad, .Names = data_pad$Date),structure(data_temp$Temp, .Names = data_temp$Date)) 
      frost_days <- fd(structure(data_tmin$Tmin, .Names = data_tmin$Date))
      Cold_spell_duration	<- csdi(structure(data_tmin$Tmin, .Names = data_tmin$Date))
      cons_frost_days <- cfd(structure(data_tmin$Tmin, .Names = data_tmin$Date))
      ice_days <- id(structure(data_tmax$Tmax, .Names = data_tmax$Date))    
      # Very_wet_days	<- d95p(structure(data_pad$pad, .Names = data_pad$Date)) 
      Effective_prec	<- ep(structure(data_etp$et, .Names = data_etp$Date), structure(data_pad$pad, .Names = data_pad$Date))
      Growing_season_prec	<- gsr(structure(data_pad$pad, .Names = data_pad$Date)) ###
      # Nongrowing_season_prec <- ngsr(structure(data_pad$pad, .Names = data_pad$Date))
      # precip_total <- rti(structure(data_pad$pad, .Names = data_pad$Date))
  
      su <- su(structure(data_tmax$Tmax, .Names = data_tmax$Date))
      tr <- tr(structure(data_tmin$Tmin, .Names = data_tmin$Date))
      # tn90p <- tn90p(structure(data_tmin$Tmin, .Names = data_tmin$Date))
      # vwd <- vwd(data = structure(data_tmax$Tmax, .Names = data_tmax$Date))
      # tn90p
      
      
      percentil75T<-data.frame(latsi=lat[j],lonsi=lon[i],T75p=as.numeric(quantile(data_temp$Temp,0.75))) # tega ne rabim za proj.   OD TUKAJ
      podatki_percentil_75_tmean<-rbind(podatki_percentil_75_tmean,percentil75T) # tega ne rabim za proj.
      podatki_percentil_75_tmean2<- subset(podatki_percentil_75_tmean, as.numeric(latsi)==lat[j]  & podatki_percentil_75_tmean$lon==as.numeric(lon[i]))
      
      percentil75rr<-data.frame(latsi=lat[j],lonsi=lon[i],rr75p=as.numeric(quantile(data_pad$pad,0.75))) # tega ne rabim za proj.
      podatki_percentil_75_rr<-rbind(podatki_percentil_75_rr,percentil75rr) # tega ne rabim za proj.
      podatki_percentil_75_rr2<- subset(podatki_percentil_75_rr, as.numeric(latsi)==lat[j]  & podatki_percentil_75_rr$lon==as.numeric(lon[i]))
      
      data_wwd <- data.frame(date = data_tmax$Date, Tmean = data_temp$Temp, RR = data_pad$pad)
      warm_wet_days <- function(data) {
        data <- data %>% mutate(warm_wet = (Tmean > podatki_percentil_75_tmean2$T75p) & (RR > podatki_percentil_75_rr2$rr75p))
        data$year <- format(as.POSIXct(data$date, format="%m/%d/%Y"), "%Y")
        warm_wet_days_count <- data %>%
          filter(warm_wet) %>%
          group_by(year) %>%
          summarize(warm_wet_days = n(),
                    .groups = "drop")
        return(warm_wet_days_count)
      }
      WWD <- warm_wet_days(data_wwd)
      temp_day1 <- data.frame(Year = format(as.POSIXct(data_temp$Date, format="%m/%d/%Y"), "%Y"), 
                              Month = format(as.POSIXct(data_temp$Date, format="%m/%d/%Y"), "%m"), 
                              Day = format(as.POSIXct(data_temp$Date, format="%m/%d/%Y"), "%d"),  
                              temp = data_temp$Temp)
      x<-aggregate(temp ~  Year + Month, temp_day1, FUN = mean, na.rm=TRUE, na.action=na.pass)
      warmest_m_temp <- x %>% group_by(Year) %>%
        summarise(max = max(temp))
      
      WWD$Year <- WWD$year
      WWD <- left_join(warmest_m_temp,WWD,by="Year") %>%
        mutate_if(is.numeric,coalesce,0)
      
      df <- data.frame(Date = data_temp$Date, Tmean = data_temp$Temp, Tmin = data_tmin$Tmin)
      # df$Date <- as.Date(df$Date)
      calculate_late_frost <- function(df) {
        df <- df %>% arrange(Date) # Ensure data is sorted by date
        df$Year <- format(as.POSIXct(df$Date, format="%m/%d/%Y"), "%Y")
        df$LateFrost <- FALSE
        years <- unique(df$Year)
        results_frost <- data.frame(Year = integer(), LateFrostCount = integer())
        for (current_year in years) {
          year_data <- df %>% filter(Year == current_year)
          start_date <- NA        # Find start of 10°C period
          for (m in 1:(nrow(year_data) - 4)) {
            mean_temp_5days <- mean(year_data$Tmean[m:(m + 4)])
            if (mean_temp_5days >= 10) {
              start_date <- year_data$Date[m + 4] # Use the last day of the 5-day period
              break  }  }
          if (!is.na(start_date)) {
            # Check for late frost after the start date
            late_frost_days <- year_data %>%
              filter(Date > start_date, Tmin <= 0)
            if(nrow(late_frost_days) > 0){
              df$LateFrost[df$Date %in% late_frost_days$Date] <- TRUE          }
            late_frost_count <- nrow(late_frost_days)
            results_frost <- rbind(results_frost, data.frame(Year = current_year, LateFrostCount = late_frost_count))
          } else { 
            results_frost <- rbind(results_frost, data.frame(Year = current_year, LateFrostCount = 0))}  #No 5 day period with temp > 10
        }
        return(list(df = df, results_frost = results_frost))
      }
      late_frost_analysis <- calculate_late_frost(df) # df <- late_frost_analysis$df
      results_frost <- late_frost_analysis$results_frost
      
  
      flowering_heat_sum <- 703.42
      maturity_heat_sum <- 1616.8
  
        
      df_heat_stress <- data.frame(Date = data_temp$Date, Tmax = data_tmax$Tmax, temp = data_temp$Temp)
      calculate_heat_sum <- function(tmax_data) {
        gdd_daily <- pmax(0, tmax_data$temp - 10)
        return(cumsum(gdd_daily))
      }
      is_heat_stress <- function(tmax_window) {# Function to check for heat stress (2-day period above 35°C)
        all(tmax_window > 35)
      }
      df_heat_stress$Year <- format(as.POSIXct(df_heat_stress$Date, format="%m/%d/%Y"), "%Y")
      results <- data.frame(Year = unique(df_heat_stress$Year), HeatStressDaysFL = NA, HeatStressDaysMT = NA)
      for (current_year in unique(df_heat_stress$Year)) {
        year_data <- subset(df_heat_stress, Year == current_year)
        year_data <- subset(year_data, temp >=10)
        year_data$HeatSum <- calculate_heat_sum(year_data)
        start_dateFL <- min(year_data$Date)
        end_dateFL <- year_data$Date[which.min(abs(year_data$HeatSum - flowering_heat_sum))]
  
        end_date <- year_data$Date[which.min(abs(year_data$HeatSum - maturity_heat_sum))]
        start_date <- end_dateFL
        # print(c(start_date,end_date,start_dateFL,end_dateFL))
        if (!is.na(start_date) && !is.na(end_date)) {  # Only proceed if both dates are found
          flowering_data <- subset(year_data, Date >= start_dateFL & Date <= end_dateFL) 
          maturity_data <- subset(year_data, Date >= start_date & Date <= end_date) 
          if (nrow(flowering_data) >= 2){ # Check if there are at least 2 days of flowering data
            heat_stress_eventsFL <- zoo::rollapply(flowering_data$Tmax, width = 2, FUN = is_heat_stress, fill = NA, align = "left")
            heat_stress_daysFL <- sum(heat_stress_eventsFL, na.rm = TRUE) * 2
          }
          if (nrow(maturity_data) >= 2){ # Check if there are at least 2 days of flowering data
            heat_stress_events <- zoo::rollapply(maturity_data$Tmax, width = 2, FUN = is_heat_stress, fill = NA, align = "left")
            heat_stress_days <- sum(heat_stress_events, na.rm = TRUE) * 2
          }
          else {
            heat_stress_days <- 0
            heat_stress_daysFL <- 0# No heat stress days if flowering period is less than 2 days
          }
        } else {
          heat_stress_days <- 0
          heat_stress_daysFL <- 0# Or NA, if you prefer to indicate that flowering period couldn't be determined
          cat("Flowering period not found for year", current_year, "\n")
        }
        results$HeatStressDaysFL[results$Year == current_year] <- heat_stress_daysFL
        results$HeatStressDaysMT[results$Year == current_year] <- heat_stress_days
      }
      
      #T_growing_season <- ta_o(structure(data_temp$Temp, .Names = data_temp$Date)) #Kelvini
      # TG_of_coldest_quarter	<- bio11(structure(data_temp$Temp, .Names = data_temp$Date))  
      # Prec_driest_month	<- bio14(structure(data_pad$pad, .Names = data_pad$Date)) 
      # Onset_growing_season <- ogs6(structure(data_temp$Temp, .Names = data_temp$Date))
      rx1day <- rx1day(structure(data_pad$pad, .Names = data_pad$Date)) 
      rx5d <- rx5d(structure(data_pad$pad, .Names = data_pad$Date)) 
      r95tot <- r95tot(structure(data_pad$pad, .Names = data_pad$Date)) 
      
      temp_day <- data.frame(Year = format(as.POSIXct(data_temp$Date, format="%m/%d/%Y"), "%Y"),
                             Month = format(as.POSIXct(data_temp$Date, format="%m/%d/%Y"), "%m"), 
                             Day = format(as.POSIXct(data_temp$Date, format="%m/%d/%Y"), "%d"),
                             Tmin = data_tmin$Tmin, Tmax = data_tmax$Tmax)
      climdata <- hourly_temps(temp_day, latitude = lat[i])
      # chill_portions0 <- chill_portions(climdata, Start = 214)
      # chill_portions <- aggregate(Chill ~  Year, chill_portions0, FUN = sum, na.rm=TRUE, na.action=na.pass)[,2]
      # chill_units0 <- chill_units(climdata, Start = 214)
      # chill_units <- aggregate(Chill ~  Year, chill_units0, FUN = sum, na.rm=TRUE, na.action=na.pass)[,2]
      BEDD0 <- head(GDD_linear(temp_day, Tb = 10, Tu = 30),-1)
      BEDD <- aggregate(GDD ~  Year, BEDD0, FUN = sum, na.rm=TRUE, na.action=na.pass)[,2]
  
      pod_celi <- data.frame(datum = data_temp$Date, 
                             leto = format(as.POSIXct(data_temp$Date, format="%m/%d/%Y"), "%Y"), 
                             mesec = format(as.POSIXct(data_temp$Date, format="%m/%d/%Y"), "%m"), 
                             tg = data_tmax$Tmax)
      st_dni0 <- pod_celi %>% filter(tg >= 30)
      st_dni30 <- st_dni0 %>% group_by(leto) %>% summarise(tg = sum(tg-30))
      warmest_m_temp$leto <- warmest_m_temp$Year
      Dnevi_30max <- left_join(warmest_m_temp,st_dni30,by="leto") %>%
        mutate_if(is.numeric,coalesce,0)
      Dnevi_30max <- Dnevi_30max[,3:4] #Plant heat stress = accumulated daily maximum temperature values above 30?C 
      
      st_dni32_anthesis <- pod_celi %>% filter(tg >= 32, mesec >= 5, mesec <= 6) 
      Dnevi_32max_anthesis <- st_dni32_anthesis %>% group_by(leto) %>% count()
      Dnevi_32max_anthesis <- left_join(warmest_m_temp[,3],Dnevi_32max_anthesis,by="leto") %>%
        mutate_if(is.numeric,coalesce,0) # Days with Tmax above 32 ?C
  
      podatki_tocka <- data.frame(lats = rep(lat[j], length(growing_degree_days)), 
                                  lons = rep(lon[i], length(growing_degree_days)), 
                                  leto = unique(WWD$Year),
                                  growing_degree_days, 
                                  # Dnevi_30max = Dnevi_30max$tg, 
                                  GSL = Growing_season_length, 
                                  End_growing_season, 
                                  BEDD, 
                                  Sums_Tmax32, 
                                  T_warmest_m = warmest_m_temp$max, 
                                  TG_of_warmest_quarter, 
                                  Diurnal_temp_rangeN, 
                                  #Days_Tmax32, # Dnevi_32max_anthesis = Dnevi_32max_anthesis$n, 
                                  Heat_stress_fl = results$HeatStressDaysFL, 
                                  Heat_stress_mat = results$HeatStressDaysMT,
                                  cons_summer_days, 
                                  WWD = WWD$warm_wet_days, 
                                  WSDI = result_df$consecutive_days,
                                  # SPEI6, SPI6, SPEI3, SPI3,
                                  CDD = Max_consecutive_dry_days,#DD = Dry_days, Prec_deficit, 
                                  CWD = longest_wet_period, 
                                  r20mm, 
                                  Heavy_prec_days,# r10mm, 
                                  SDII, 
                                  Prec_wettest_month, #Prec_warmest_quarter, Prec_coldest_quarter,wet_days, 
                                  FD = frost_days, 
                                  late_frost_days = results_frost$LateFrostCount,
                                  CSDI = Cold_spell_duration, 
                                  CFD = cons_frost_days, 
                                  ice_days, 
                                  Effective_prec, 
                                  Growing_season_prec,#Very_wet_days,
                                  # Nongrowing_season_prec, 
                                  # precip_total,
                                  su, 
                                  tr, 
                                  tn90p = t90_test$tn90p, 
                                  vwd = tx99_test$tx99p, 
                                  rx5d)
      vsi_podatki <- rbind(vsi_podatki, podatki_tocka)
      }
  }
}
saveRDS(skupni_prag_tmin90p, file = "tn90p_referencno_obdobje.rds")
saveRDS(skupni_prag_tmax99p, file = "tx99p_referencno_obdobje.rds")
saveRDS(skupni_prag_tmax90p, file = "tx90p_referencno_obdobje.rds")
saveRDS(skupni_prag_t75p, file = "t75p_referencno_obdobje.rds")
saveRDS(skupni_prag_rr75pp, file = "rr75p_referencno_obdobje.rds")

# vsi_podatki = subset(vsi_podatki, select = -c(Year,SPEI_3,year) ) 
saveRDS(vsi_podatki, file = "kazalniki_OPSI_historical_GS.rds")

vsi_podatki$ID<-seq(1,nrow(vsi_podatki))

write_sav(vsi_podatki, "kazalniki_OPSI_historical_GS.sav")


############## KAZALNIKI ZA OBDOBJE 1950-2024, postaji RAKI?AN in JABLJE
# Postaja 8=Letali??e J.P. Ljubljana, 192=Lj-Be?igrad, 355=Raki?an-Murska Sobota 
lat_Jablje <-	46.1875 #46.1414	
lat_Rakican <-	46.6875 #46.6504
lon_Jablje <- 14.5625	#14.5561 
lon_Rakican <-	16.1875 #16.1966 

temp_Jablje_10_23 <- read_excel("podatki_8.xlsx", sheet = "Temperatura")
pad_Jablje_10_23 <- read_excel("podatki_8.xlsx", sheet = "Padavine")
et_Jablje_10_23 <- read_excel("podatki_8.xlsx", sheet = "ETP")
et_Jablje_10_23[is.na(et_Jablje_10_23)] <- 0

temp_Rakican_10_23 <- read_excel("podatki_355.xlsx", sheet = "Temperatura")
pad_Rakican_10_23 <- read_excel("podatki_355.xlsx", sheet = "Padavine")
et_Rakican_10_23 <- read_excel("podatki_355.xlsx", sheet = "ETP")
et_Rakican_10_23[is.na(et_Rakican_10_23)] <- 0

temp_Jablje_10_23$leto <- format(as.Date(temp_Jablje_10_23$datum),"%Y")
temp_Rakican_10_23$leto <- format(as.Date(temp_Rakican_10_23$datum),"%Y")
temp_Jablje_10_23 <- temp_Jablje_10_23 %>% filter(leto >= 2011, leto <= 2022)
temp_Rakican_10_23 <- temp_Rakican_10_23 %>% filter(leto >= 2011, leto <= 2022)

pad_Jablje_10_23$leto <- format(as.Date(pad_Jablje_10_23$datum),"%Y")
pad_Rakican_10_23$leto <- format(as.Date(pad_Rakican_10_23$datum),"%Y")
pad_Jablje_10_23 <- pad_Jablje_10_23 %>% filter(leto >= 2011, leto <= 2022)
pad_Rakican_10_23 <- pad_Rakican_10_23 %>% filter(leto >= 2011, leto <= 2022)

et_Jablje_10_23$leto <- format(as.Date(et_Jablje_10_23$datum),"%Y")
et_Rakican_10_23$leto <- format(as.Date(et_Rakican_10_23$datum),"%Y")
et_Jablje_10_23 <- et_Jablje_10_23 %>% filter(leto >= 2011, leto <= 2022)
et_Rakican_10_23 <- et_Rakican_10_23 %>% filter(leto >= 2011, leto <= 2022)

data_max_min_J <- data.frame(lat = rep(lat_Jablje,length(temp_Jablje_10_23$tmin)), lon = rep(lon_Jablje,length(temp_Jablje_10_23$tmin)),
                             tmin = temp_Jablje_10_23$tmin, tmax = temp_Jablje_10_23$tmax, tpov = temp_Jablje_10_23$tpov)
data_max_min_R <- data.frame(lat = rep(lat_Rakican,length(temp_Rakican_10_23$tmin)), lon = rep(lon_Rakican,length(temp_Rakican_10_23$tmin)),
                             tmin = temp_Rakican_10_23$tmin, tmax = temp_Rakican_10_23$tmax, tpov = temp_Rakican_10_23$tpov)
data_max_min <- rbind(data_max_min_J,data_max_min_R)
datum2 = format(as.POSIXct(temp_Jablje_10_23$datum, format="%Y-%m-%d"), "%m/%d/%Y")
data_max_min$meseci <- as.numeric(format(as.Date(temp_Rakican_10_23$datum),"%m"))
data_max_min$datum <- datum2

data_pad_J <- data.frame(lat = rep(lat_Jablje,length(pad_Jablje_10_23$pad)), lon = rep(lon_Jablje,length(pad_Jablje_10_23$pad)), datum = pad_Jablje_10_23$datum, pad = pad_Jablje_10_23$pad)
data_pad_R <- data.frame(lat = rep(lat_Rakican,length(pad_Rakican_10_23$pad)), lon = rep(lon_Rakican,length(pad_Rakican_10_23$pad)), datum = pad_Rakican_10_23$datum, pad = pad_Rakican_10_23$pad)
data_pad <- rbind(data_pad_J, data_pad_R)
data_pad$meseci <- as.numeric(format(as.POSIXct(data_pad$datum, format="%m/%d/%Y"), "%m"))

data_et_J <- data.frame(lat = rep(lat_Jablje,length(et_Jablje_10_23$etp)), lon = rep(lon_Jablje,length(et_Jablje_10_23$etp)), datum = et_Jablje_10_23$datum, etp = et_Jablje_10_23$etp)
data_et_R <- data.frame(lat = rep(lat_Rakican,length(et_Rakican_10_23$etp)), lon = rep(lon_Rakican,length(et_Rakican_10_23$etp)), datum = et_Rakican_10_23$datum, etp = et_Rakican_10_23$etp)
data_et <- rbind(data_et_J, data_et_R)
data_et$meseci <- as.numeric(format(as.POSIXct(data_et$datum, format="%m/%d/%Y"), "%m"))
###############################

vsi_podatki_1 <- data.frame()

# lat_izbr = lat_Jablje
# lon_izbr = lon_Jablje
lat_izbr = lat_Rakican
lon_izbr = lon_Rakican

data_max_min = subset(data_max_min, lat == lat_izbr)
data_pad = subset(data_pad, lat == lat_izbr)
data_et = subset(data_et, lat == lat_izbr)

data_max_min <- data_max_min %>% filter(meseci >= 4, meseci <= 10)
data_pad <- data_pad %>% filter(meseci >= 4, meseci <= 10)
data_et <- data_et %>% filter(meseci >= 4, meseci <= 10)
    leta <- format(as.POSIXct(data_max_min$datum, format = "%m/%d/%Y"),"%Y")
    meseci <- as.numeric(format(as.Date(data_max_min$datum),"%m"))
    dnevi <- as.numeric(format(as.POSIXct(data_max_min$datum, format = "%m/%d/%Y"),"%d"))
    datum2 = data_max_min$datum
    dETP = format(as.POSIXct(data_et$datum, format="%Y-%m-%d"), "%m/%d/%Y")

    growing_degree_days <- gd4(structure(data_max_min$tpov, .Names = datum2))
    Growing_season_length <- gsl(structure(data_max_min$tpov, .Names = datum2)) 
    End_growing_season <- ogs6(structure(data_max_min$tpov, .Names = datum2)) + Growing_season_length + 90
    Sums_Tmax32 <- stx32(structure(data_max_min$tmax, .Names = datum2)) 
    TG_of_warmest_quarter <- bio10(structure(data_max_min$tpov, .Names = datum2)) #Kelvini
    
    dtr_max <- (data_max_min$tmax+273)/(data_max_min$tpov+273)
    dtr_min <- (data_max_min$tmin+273)/(data_max_min$tpov+273)
    
    Diurnal_temp_rangeN <-	dtr(structure(dtr_max, .Names = datum2),structure(dtr_min, .Names = datum2))
    cons_summer_days <- csd(structure(data_max_min$tmax, .Names = datum2))
    # Warm_spell_duration <- wsdi(structure(data_max_min$tmin, .Names = datum2))
    Effective_prec	<- ep(structure(data_et$etp, .Names = dETP), structure(data_pad$pad, .Names = datum2))
    Max_consecutive_dry_days <- cdd(structure(data_pad$pad, .Names = datum2))
    longest_wet_period <- cwd(structure(data_pad$pad, .Names = datum2))
    r20mm <- r20mm(structure(data_pad$pad, .Names = datum2)) 
    Heavy_prec_days <- d50mm(structure(data_pad$pad, .Names = datum2))
    SDII <- sdii(structure(data_pad$pad, .Names = datum2))
    Prec_wettest_month	<- bio13(structure(data_pad$pad, .Names = datum2)) 
    frost_days <- fd(structure(data_max_min$tmin, .Names = datum2))
    Cold_spell_duration	<- csdi(structure(data_max_min$tmin, .Names = datum2))
    cons_frost_days <- cfd(structure(data_max_min$tmin, .Names = datum2))
    ice_days <- id(structure(data_max_min$tmin, .Names = datum2))
    Growing_season_prec	<- gsr(structure(data_pad$pad, .Names = datum2)) ###
    # Nongrowing_season_prec <- ngsr(structure(data_pad$pad, .Names = datum2)) 
    precip_total <- rti(structure(data_pad$pad, .Names = datum2))
    su <- su(structure(data_max_min$tmax, .Names = datum2))
    tr <- tr(structure(data_max_min$tmin, .Names = datum2))
    # tn90p <- tn90p(structure(data_max_min$tmin, .Names = datum2))
    # vwd <- vwd(structure(data_max_min$tmax, .Names = datum2))    
    
    # podatki_percentil_90_tmin = readRDS("tn90p_referencno_obdobje.rds")
    # podatki_percentil_90_tmin2<- subset(podatki_percentil_90_tmin, as.numeric(latsi)==lat_izbr  & podatki_percentil_90_tmin$lon==as.numeric(lon_izbr))
    # tn90p = podatki_percentil_90_tmin2$T90p
    data_tmin_90p<-data_max_min
    # data_tmin_90p$Datum <-as.Date(data_tmin_90p$Date)
    data_tmin_90p$leto <-format(as.POSIXct(data_max_min$datum, format="%m/%d/%Y"), "%Y")
    t90_test <- data_tmin_90p %>% 
      mutate(tmin90 = ifelse(tmin >= skupni_prag_tmin90p,1,0),ind1=1)
    t90_test <- t90_test %>%
      group_by(leto) %>% summarise(sum=sum(tmin90),
                                   n=sum(ind1), tn90p = sum/n *100)
    
    # podatki_percentil_99_tmax = readRDS("tx99p_referencno_obdobje.rds")
    # podatki_percentil_99_tmax2<- subset(podatki_percentil_99_tmax, as.numeric(latsi)==lat_izbr  & podatki_percentil_99_tmax$lon==as.numeric(lon_izbr))
    # tx99p = podatki_percentil_99_tmax2$Tx99p
    data_tmax_99p<-data_max_min
    # data_tmax_99p$Datum <-as.Date(data_tmax_99p$Date)
    data_tmax_99p$leto <-format(as.POSIXct(data_max_min$datum, format="%m/%d/%Y"), "%Y")
    tx99_test <- data_tmax_99p %>% 
      mutate(tmax99 = ifelse(tmax >= skupni_prag_tmax99p,1,0),ind1=1)
    tx99_test <- tx99_test %>%
      group_by(leto) %>% summarise(sum=sum(tmax99),
                                   n=sum(ind1), tx99p = sum)
    
    # podatki_percentil_90_tmax = readRDS("tx90p_referencno_obdobje.rds")
    # podatki_percentil_90_tmax2<- subset(podatki_percentil_90_tmax, as.numeric(latsi)==lat_izbr  & podatki_percentil_90_tmax$lon==as.numeric(lon_izbr))
    # tx90p = podatki_percentil_90_tmax2$Tx90p
    data_tmax_90p <- data_max_min %>%
      mutate(leto = format(as.POSIXct(data_max_min$datum, format = "%m/%d/%Y"), "%Y"),
             tmax90 = ifelse(tmax >= skupni_prag_tmax90p, 1, 0),
             ind1 = 1)
    consecutive_days_list <- data_tmax_90p %>%
      group_by(leto) %>%
      group_split() %>%
      lapply(function(year_data) {above_threshold <- year_data$tmax90
      consecutive_groups <- rle(above_threshold)
      consecutive_days <- sum(consecutive_groups$lengths[consecutive_groups$values == 1 & consecutive_groups$lengths >= 6])
      return(consecutive_days)
      })
    consecutive_days_df <- data.frame(leto = unique(data_tmax_90p$leto), consecutive_days = unlist(consecutive_days_list))
    summary_df <- data_tmax_90p %>%
      group_by(leto) %>%
      summarise(sum = sum(tmax90),n = sum(ind1))
    result_df <- left_join(summary_df, consecutive_days_df, by = "leto")
    
    
    rx5d <- rx5d(structure(data_pad$pad, .Names = datum2)) 
    Very_wet_days	<- d95p(structure(data_pad$pad, .Names = datum2)) 
    Prec_warmest_quarter	<- bio18(structure(data_pad$pad, .Names = datum2),structure(data_max_min$tpov, .Names = datum2)) 
    Prec_coldest_quarter	<- bio19(structure(data_pad$pad, .Names = datum2),structure(data_max_min$tpov, .Names = datum2)) 

    temp_day <- data.frame(Year = leta, Month = data_max_min$meseci, Day = dnevi, 
                           Tmin = data_max_min$tmin, Tmax = data_max_min$tmax)
    climdata <- hourly_temps(temp_day, latitude = lat_izbr)
    BEDD0 <- head(GDD_linear(temp_day, Tb = 10, Tu = 30),-1)
    BEDD <- aggregate(GDD ~  Year, BEDD0, FUN = sum, na.rm=TRUE, na.action=na.pass)[,2]
    
    temp_day1 <- data.frame(Year = leta, Month = meseci, Day = dnevi, 
                            temp = data_max_min$tpov)
    x<-aggregate(temp ~  Year + Month, temp_day1, FUN = mean, na.rm=TRUE, na.action=na.pass)
    warmest_m_temp <- x %>% group_by(Year) %>%
      summarise(max = max(temp))
    
    coldest_m_temp <- x %>% group_by(Year) %>%
      summarise(min = min(temp))
    
    pod_celi <- data.frame(datum = datum2, leto = leta, mesec = as.numeric(meseci), tg = data_max_min$tmax)
    st_dni0 <- pod_celi %>% filter(tg >= 30)
    st_dni30 <- st_dni0 %>% group_by(leto) %>% count()
    coldest_m_temp$leto <- coldest_m_temp$Year
    Dnevi_30max <- left_join(coldest_m_temp,st_dni30,by="leto") %>%
      mutate_if(is.numeric,coalesce,0)
    Dnevi_30max <- Dnevi_30max[,3:4]
    
    st_dni32_anthesis <- pod_celi %>% filter(tg >= 32, mesec >= 5, mesec <= 6) 
    Dnevi_32max_anthesis <- st_dni32_anthesis %>% group_by(leto) %>% count()
    Dnevi_32max_anthesis <- left_join(coldest_m_temp[,3],Dnevi_32max_anthesis,by="leto") %>%
      mutate_if(is.numeric,coalesce,0)
    
    podatki_percentil_75_tmean = readRDS("t75p_referencno_obdobje.rds")
    podatki_percentil_75_tmean2<- subset(podatki_percentil_75_tmean, as.numeric(latsi)==lat_izbr  & podatki_percentil_75_tmean$lon==as.numeric(lon_izbr))
    podatki_percentil_75_rr = readRDS("rr75p_referencno_obdobje.rds")
    podatki_percentil_75_rr2<- subset(podatki_percentil_75_rr, as.numeric(latsi)==lat_izbr  & podatki_percentil_75_rr$lon==as.numeric(lon_izbr))
    
    data_wwd <- data.frame(date = datum2, Tmean = data_max_min$tpov, RR = data_pad$pad)
    warm_wet_days <- function(data) {
      data <- data %>% mutate(warm_wet = (Tmean > podatki_percentil_75_tmean2$T75p) & (RR > podatki_percentil_75_rr2$rr75p))
      data$year <- format(as.POSIXct(data$date, format="%m/%d/%Y"), "%Y")
      warm_wet_days_count <- data %>%
        filter(warm_wet) %>%
        group_by(year) %>%
        summarize(warm_wet_days = n(),
                  .groups = "drop")
      return(warm_wet_days_count)
    }   
    WWD <- warm_wet_days(data_wwd)
    temp_day1 <- data.frame(Year = leta, Month = meseci, Day = dnevi, temp = data_max_min$tpov)
    x<-aggregate(temp ~  Year + Month, temp_day1, FUN = mean, na.rm=TRUE, na.action=na.pass)
    warmest_m_temp <- x %>% group_by(Year) %>%
      summarise(max = max(temp))
    
    WWD$Year <- WWD$year
    WWD <- left_join(warmest_m_temp,WWD,by="Year") %>%
      mutate_if(is.numeric,coalesce,0)
    
    df <- data.frame(Date = datum2, Tmean = data_max_min$tpov, Tmin = data_max_min$tmin)
    df$Date <- format(as.POSIXct(datum2, format="%m/%d/%Y"), "%Y-%m-%d")
    calculate_late_frost <- function(df) {
      df <- df %>% arrange(Date) # Ensure data is sorted by date
      df$Year <- year(df$Date)
      df$LateFrost <- FALSE
      years <- unique(df$Year)
      results_frost <- data.frame(Year = integer(), LateFrostCount = integer())
      for (current_year in years) {
        year_data <- df %>% filter(Year == current_year)
        start_date <- NA        # Find start of 10°C period
        for (m in 1:(nrow(year_data) - 4)) {
          mean_temp_5days <- mean(year_data$Tmean[m:(m + 4)])
          if (mean_temp_5days >= 10) {
            start_date <- year_data$Date[m + 4] # Use the last day of the 5-day period
            break  }  }
        if (!is.na(start_date)) {
          # Check for late frost after the start date
          late_frost_days <- year_data %>%
            filter(Date > start_date, Tmin <= 0)
          if(nrow(late_frost_days) > 0){
            df$LateFrost[df$Date %in% late_frost_days$Date] <- TRUE          }
          late_frost_count <- nrow(late_frost_days)
          results_frost <- rbind(results_frost, data.frame(Year = current_year, LateFrostCount = late_frost_count))
        } else { 
          results_frost <- rbind(results_frost, data.frame(Year = current_year, LateFrostCount = 0))}  #No 5 day period with temp > 10
      }
      return(list(df = df, results_frost = results_frost))
    }
    late_frost_analysis <- calculate_late_frost(df) # df <- late_frost_analysis$df
    results_frost <- late_frost_analysis$results_frost
    
    
    flowering_heat_sum <- 703.42
    maturity_heat_sum <- 1616.8
    
    
    df_heat_stress <- data.frame(Date = df$Date, Tmax = data_max_min$tmax, temp = data_max_min$tpov)
    calculate_heat_sum <- function(tmax_data) {
      gdd_daily <- pmax(0, tmax_data$temp - 10)
      return(cumsum(gdd_daily))
    }
    is_heat_stress <- function(tmax_window) {# Function to check for heat stress (2-day period above 35°C)
      all(tmax_window > 35)
    }
    df_heat_stress$Year <- format(as.POSIXct(datum2, format="%m/%d/%Y"), "%Y")
    results <- data.frame(Year = unique(df_heat_stress$Year), HeatStressDaysFL = NA, HeatStressDaysMT = NA)
    for (current_year in unique(df_heat_stress$Year)) {
      year_data <- subset(df_heat_stress, Year == current_year)
      year_data <- subset(year_data, temp >=10)
      year_data$HeatSum <- calculate_heat_sum(year_data)
      start_dateFL <- min(year_data$Date)
      end_dateFL <- year_data$Date[which.min(abs(year_data$HeatSum - flowering_heat_sum))]
      
      end_date <- year_data$Date[which.min(abs(year_data$HeatSum - maturity_heat_sum))]
      start_date <- end_dateFL
      # print(c(start_date,end_date,start_dateFL,end_dateFL))
      if (!is.na(start_date) && !is.na(end_date)) {  # Only proceed if both dates are found
        flowering_data <- subset(year_data, Date >= start_dateFL & Date <= end_dateFL) 
        maturity_data <- subset(year_data, Date >= start_date & Date <= end_date) 
        if (nrow(flowering_data) >= 2){ # Check if there are at least 2 days of flowering data
          heat_stress_eventsFL <- zoo::rollapply(flowering_data$Tmax, width = 2, FUN = is_heat_stress, fill = NA, align = "left")
          heat_stress_daysFL <- sum(heat_stress_eventsFL, na.rm = TRUE) * 2
        }
        if (nrow(maturity_data) >= 2){ # Check if there are at least 2 days of flowering data
          heat_stress_events <- zoo::rollapply(maturity_data$Tmax, width = 2, FUN = is_heat_stress, fill = NA, align = "left")
          heat_stress_days <- sum(heat_stress_events, na.rm = TRUE) * 2
        }
        else {
          heat_stress_days <- 0
          heat_stress_daysFL <- 0# No heat stress days if flowering period is less than 2 days
        }
      } else {
        heat_stress_days <- 0
        heat_stress_daysFL <- 0# Or NA, if you prefer to indicate that flowering period couldn't be determined
        cat("Flowering period not found for year", current_year, "\n")
      }
      results$HeatStressDaysFL[results$Year == current_year] <- heat_stress_daysFL
      results$HeatStressDaysMT[results$Year == current_year] <- heat_stress_days
    }
    
    
    podatki_tocka <- data.frame(lats = rep(lat_izbr, length(growing_degree_days)), 
                                lons = rep(lon_izbr, length(growing_degree_days)), 
                                leto = unique(WWD$Year), ######## leto
                                growing_degree_days, 
                                GSL = Growing_season_length, 
                                End_growing_season,
                                BEDD, 
                                Sums_Tmax32, 
                                T_warmest_m = warmest_m_temp$max, 
                                TG_of_warmest_quarter, 
                                Diurnal_temp_rangeN, 
                                Heat_stress_fl = results$HeatStressDaysFL, 
                                Heat_stress_mat = results$HeatStressDaysMT,
                                cons_summer_days, 
                                WWD = WWD$warm_wet_days, 
                                WSDI = result_df$consecutive_days,
                                # SPEI6, SPI6, SPEI3, SPI3,
                                CDD = Max_consecutive_dry_days,#DD = Dry_days, Prec_deficit, 
                                CWD = longest_wet_period, 
                                r20mm, 
                                Heavy_prec_days,# r10mm, 
                                SDII, 
                                Prec_wettest_month, #Prec_warmest_quarter, Prec_coldest_quarter,wet_days, 
                                FD = frost_days, 
                                late_frost_days = results_frost$LateFrostCount,
                                CSDI = Cold_spell_duration, 
                                CFD = cons_frost_days, 
                                ice_days, 
                                Effective_prec, 
                                Growing_season_prec,#Very_wet_days,
                                # Nongrowing_season_prec, 
                                # precip_total,
                                su, 
                                tr, 
                                tn90p = t90_test$tn90p, 
                                vwd = tx99_test$tx99p, 
                                rx5d)
    
    
    vsi_podatki_1 <- rbind(vsi_podatki_1, podatki_tocka)

saveRDS(vsi_podatki_1, file = "kazalniki_Jablje_Rakican_2011-2022_GS.rds")



































































########## fruclimadapt
# library(fruclimadapt)
# 
# vsi_podatki <- data.frame()
# for(i in lons1){
#   for(j in lats1){
#     print(i)
#     print(j)
#     if(all(is.na(data3[i,j,1:10957]))){
#       next
#     }
#     else{
#       temp_day <- data.frame(Year = leta, Month = meseci, Day = dnevi,
#                               Tmin = data_min[i,j,1:10957]-273, Tmax = data_max[i,j,1:10957]-273)
# 
#       climdata <- hourly_temps(temp_day, latitude = lat[i])
#       chill_portions <- chill_portions(climdata, Start = 214)
#       chill_units <- chill_units(climdata, Start = 214)
#       BEDD0 <- head(GDD_linear(temp_day, Tb = 10, Tu = 30),-1)
#       BEDD <- aggregate(GDD ~  Year, BEDD0, FUN = sum, na.rm=TRUE, na.action=na.pass)
#       podatki_tocka <- data.frame(lats = rep(lat[j], length(TG_of_warmest_quarter)), lons = rep(lon[i], length(TG_of_warmest_quarter)),
#                                   leto = unique(years(datum2)),
#                                   climdata,
#                                   chill_portions, chill_units,
#                                   BEDD)
#       vsi_podatki <- rbind(vsi_podatki, podatki_tocka)
#     }
#   }
# }
# saveRDS(vsi_podatki, file = "kazalniki_OPSI_historical.rds")













# Risanje kazalnikov
# vsi_podatki0 <- vsi_podatki
vsi_podatki <- readRDS(file = "kazalniki_OPSI_historical.rds")
yrs <- unique(years(datumi))
kaz <- names(vsi_podatki[4:41])
kaz_SI <- c("T rastne dobe", "T najtoplej?e ?etrtine leta","T najhladnej?e ?etrtine leta",
         "Vsota Tmin pod -10 ?C","Vsota Tmin pod -15 ?C","?tevilo dni s Tmax nad 32 ?C",
         "Vsota Tmax nad 32 ?C","Dnevni T razpon", "?tevilo suhih dni",
         "Padavine najbolj mokrega meseca","Padavine najbolj suhega meseca","?tevilo dni s koli?ino padavin nad 50 mm",
         "95. percentil mokrih dni","Padavine v obd. rastne dobe","Padavine izven rastne dobe",
         "Padavine najtoplej?e ?etrtine leta", "Padavine najhladnej?e ?etrtine leta",
         "Efektivne padavine","Temperaturna vsota",
         "Dol?ina rastne dobe","WSDI","CSDI",
         "Za?etek rastne dobe","Konec rastne dobe","Maks. ?t. zap. suhih dni",
         "Maks. ?t. zap. mokrih dni","Maks. ?t. zap. poletnih dni","SDII",
         "Mokri dnevi","Dnevi s pozebo","Maks. ?t. zap. dni s pozebo",
         "Ledeni dnevi","Skupna koli?ina padavin",
         "BEDD",
         "T najtoplej?ega meseca", "T najhladnej?ega meseca",
         "Dnevi s Tmax nad 30 ?C", "Dnevi s Tmax nad 32 ?C")

kaz_enote <- c("?C", "?C", "?C",
               "?C dnevi", "?C dnevi", "?t. dni",
               "?C dnevi", "?C", "?t. dni", 
               "mm", "mm","?t. dni",
               "?t. dni", "mm", "mm", 
               "mm", "mm", 
               "mm","?C",
               "?t. dni","?t. dni","?t. dni",
               "ZD","ZD","?t. dni",
               "?t. dni","?t. dni","mm/dan",
               "?t. dni","?t. dni","?t. dni",
               "?t. dni","mm",
               "?C dnevi", 
               "?C","?C",
               "?t. dni","?t. dni")
kaz
# kaz_min0 <- c(10, -5,    0,   0,  0,  0,  5, 200,   50,   0,  0,  5,  200,  150,   50,   0, -3, -3)
# kaz_max0 <- c(25, 10, 1000, 600, 30, 70, 15, 300, 1000, 110, 20, 30, 1900, 2300,  800, 100,  3,  3) 

# for(i in yrs){
#   for(k in seq(1,length(kaz),by=1)){
#     data <- vsi_podatki %>% filter(leto == i) 
#     data=na.omit(data)
#     min = min(na.omit(vsi_podatki[[kaz[k]]]))
#     max = max(na.omit(vsi_podatki[[kaz[k]]]))
#     ggplot() +
#       geom_tile(data=data,aes(x=lons,y=lats,fill=data[[kaz[k]]])) +
#       geom_sf(data=slovenia_nuts3_mapdata, color=alpha("black",0.4),fill = NA) +
#       scale_fill_distiller(kaz_enote[k], palette = "Spectral",  limits = c(min, max)) +
#       # scale_fill_distiller(kaz_enote[k], palette = "Spectral",  limits = c(kaz_min0[k], kaz_max0[k])) +
#       labs(x="Geografska dol?ina",y="Geografska ?irina",title=paste0(kaz_SI[k],", leto ",i)) +
#       coord_sf(crs = st_crs(4326)) +
#       theme_light(base_size = 17)
#     ggsave(paste0("historicni/yearly/yearly_",kaz[k],"_",i,".png"),width = 10, height = 7)
#   }
# }
 
# kaz_min <- c(7, 10, -5,   0,   0,  0,  0,  6, 180, 100,   5,  0,  15,  400,  200, 200,   0, 600)
# kaz_max <- c(20, 23,  8, 600, 300, 30, 60, 12, 280, 700, 100, 20, 22, 1900, 1900, 800, 800, 3000)

zac = 1981
kon = 2010

for(k in seq(1,length(kaz),by=1)){
  min = min(na.omit(vsi_podatki[[kaz[k]]]))
  max = max(na.omit(vsi_podatki[[kaz[k]]]))
  
  data <- data.frame(vsi_podatki$lons, vsi_podatki$lats, vsi_podatki$leto, vsi_podatki[[kaz[k]]])
  colnames(data) <- c("lons","lats","leto","agroclim_ind")
  data <- data %>% filter(leto >= zac, leto <= kon)
  data=na.omit(data)
  
  povp <- data %>% group_by(lats,lons) %>%
    summarise(period_mean = mean(agroclim_ind))
  
  povp1 <- aggregate(agroclim_ind ~  lats + lons, data, FUN = mean, na.rm=TRUE, na.action=na.pass)
  
  ggplot() +
    geom_tile(data=povp,aes(x=lons,y=lats,fill=period_mean)) +
    geom_sf(data=slovenia_nuts3_mapdata, color=alpha("black",0.4),fill = NA) +
    scale_fill_distiller(kaz_enote[k], palette = "RdBu", limits = c(min[k], max[k])) +
    labs(x="Geografska dol?ina",y="Geografska ?irina",title=paste0(kaz_SI[k],", obdobje ",zac,"-",kon)) +
    coord_sf(crs = st_crs(4326)) +
    theme_light(base_size = 17)
  ggsave(paste0("kaz_hist/period_mean_",kaz[k],"_",zac,"-",kon,".png"),width = 10, height = 7)
}

lat_LJ <- 46.0625
lon_LJ <- 14.5625
data_LJ <- vsi_podatki %>% filter(lats == lat_LJ, lons == lon_LJ)
# WARMEST COLDEST QUARTER
data_lja <- data.frame(leto = data_LJ$leto, ime = rep("T najtoplej?e ?etrtine leta", length(data_LJ$leto)),
                       TG = data_LJ$TG_of_warmest_quarter)
data_ljb <- data.frame(leto = data_LJ$leto, ime = rep("T najhladnej?e ?etrtine leta", length(data_LJ$leto)),
                       TG = data_LJ$TG_of_coldest_quarter)
data_LJ <- rbind(data_lja,data_ljb)
cols <- c("T najtoplej?e ?etrtine leta"="sienna3","T najhladnej?e ?etrtine leta"="deepskyblue3")
aa<-ggplot(data_LJ, aes(x=leto, y = TG, group = ime, colour = ime)) +
  theme_light(base_size = 14)+ 
  geom_point(size=2)+geom_line(lty=3,lwd = 1)+
  labs(title = paste("Ljubljana - Be?igrad",sep="")) +  ylab("T [?C]") + xlab("leto") + 
  scale_colour_manual(name="Legenda",values=cols)+ theme(legend.text=element_text(size=15),
                                                         axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
                                                         legend.position='bottom') 
aa# scale_x_continuous(limits=c(70,170))
ggsave(paste("poro?ilo ministrstvo/T_warmest_coldest_quarter.png",sep=""),aa, dpi=500, height=6, width=9, units="in")

# lat_MB <- 46.5625
# lon_MB <- 15.6875
# data_MB <- vsi_podatki %>% filter(lats == lat_MB, lons == lon_MB) # 46.562483, 15.643975
# data_MB$lok <- rep("MB", length(data_MB$lats))
# 
# lat_RA <- 46.4375
# lon_RA <- 13.8125
# data_RA <- vsi_podatki %>% filter(lats == lat_RA, lons == lon_RA) # 46.496503, 13.713147
# data_RA$lok <- rep("RA", length(data_RA$lats))
# data_LJ$lok <- rep("LJ", length(data_LJ$lats))
# 
# data <- rbind(data_LJ, data_MB, data_RA)
# data <- data %>% filter(leto >= 1991, leto <= 2010)
# data <- data.frame(lok = data$lok, leto = data$leto, 
#                    TG_of_coldest_quarter = data$TG_of_coldest_quarter,
#                    TG_of_warmest_quarter = data$TG_of_warmest_quarter, Dry_days = data$Dry_days,
#                    Prec_wettest_month = data$Prec_wettest_month, Prec_driest_month = data$Prec_driest_month)
# write.csv(as.matrix(data),file="LJ_MB_RA.csv")




# GS PRECIPITATION 
data_LJ <- vsi_podatki %>% filter(lats == lat_LJ, lons == lon_LJ)
data_lja <- data.frame(leto = data_LJ$leto, ime = rep("Padavine v obd. rastne dobe", length(data_LJ$leto)),
                       TG = data_LJ$Growing_season_prec)
data_ljb <- data.frame(leto = data_LJ$leto, ime = rep("Padavine izven rastne dobe", length(data_LJ$leto)),
                       TG = data_LJ$Nongrowing_season_prec)
data_LJ <- rbind(data_lja,data_ljb)


cols <- c("Padavine v obd. rastne dobe"="olivedrab3","Padavine izven rastne dobe"="burlywood4")
aa<-ggplot(data_LJ, aes(x=leto, y = TG, group = ime, colour = ime)) +
  theme_light(base_size = 14)+ 
  geom_point(size=2)+geom_line(lty=3,lwd = 1)+
  labs(title = paste("Ljubljana - Be?igrad",sep="")) +  ylab("Koli?ina padavine [mm]") + xlab("leto") + 
  scale_colour_manual(name="Legenda",values=cols)+ theme(legend.text=element_text(size=15),
                                                         axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
                                                         legend.position='bottom') 
aa# scale_x_continuous(limits=c(70,170))
ggsave(paste("poro?ilo ministrstvo/Growing_nongrowing_season_prec.png",sep=""),aa, dpi=500, height=6, width=9, units="in")

# EFFECTIVE PRECIPITATION 
data_LJ <- vsi_podatki %>% filter(lats == lat_LJ, lons == lon_LJ)
data_lja <- data.frame(leto = data_LJ$leto, ime = rep("Efektivne padavine", length(data_LJ$leto)),
                       TG = data_LJ$Effective_prec)
data_LJ <- rbind(data_lja)
cols <- c("Efektivne padavine"="steelblue3")
aa<-ggplot(data_LJ, aes(x=leto, y = TG, group = ime, colour = ime)) +
  theme_light(base_size = 14)+ 
  geom_point(size=2)+geom_line(lty=3,lwd = 1)+
  labs(title = paste("Ljubljana - Be?igrad",sep="")) +  ylab("Efektivne padavine [mm]") + xlab("leto") + 
  scale_colour_manual(name="Legenda",values=cols)+ theme(legend.text=element_text(size=15),
                                                         axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
                                                         legend.position='bottom') 
aa# scale_x_continuous(limits=c(70,170))
ggsave(paste("poro?ilo ministrstvo/Eff_prec.png",sep=""),aa, dpi=500, height=6, width=9, units="in")

# T growing season 
data_LJ <- vsi_podatki %>% filter(lats == lat_LJ, lons == lon_LJ)
data_lja <- data.frame(leto = data_LJ$leto, ime = rep("T rastne dobe", length(data_LJ$leto)),
                       TG = data_LJ$T_growing_season)
data_LJ <- rbind(data_lja)
cols <- c("T rastne dobe"="darkred")
aa<-ggplot(data_LJ, aes(x=leto, y = TG, group = ime, colour = ime)) +
  theme_light(base_size = 14)+ 
  geom_point(size=2)+geom_line(lty=3,lwd = 1)+
  labs(title = paste("Ljubljana - Be?igrad",sep="")) +  ylab("T rastne dobe [?C]") + xlab("leto") + 
  scale_colour_manual(name="Legenda",values=cols)+ theme(legend.text=element_text(size=15),
                                                         axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
                                                         legend.position='bottom') 
aa# scale_x_continuous(limits=c(70,170))
ggsave(paste("poro?ilo ministrstvo/T_gs.png",sep=""),aa, dpi=500, height=6, width=9, units="in")


############### AGROCLIM knji?nica
# # library(agroclim) ## ne dela
# Biologically effective degree-days (BEDD)
# Extreme Heat Exposure (EHE)
# Growing Degree Day (GDD) or Winkler index. Useful as a zoning tool to differentiate between grape varieties and climate (Winkler et al. 1974)
# Huglin Heliothermal Index (HI). Useful as a zoning tool (Huglin 1978)
install.packages("ClimClass")
library(ClimClass)
library(fruclimadapt)
data(Trent_climate)

vsi_podatki_fca <- data.frame()
for(i in lons1){
  for(j in lats1){
    print(i)
    print(j)
    
    year <- format(as.POSIXct(datum2, format="%m/%d/%Y"), "%Y")
    month <- as.numeric(format(as.POSIXct(datum2, format="%m/%d/%Y"), "%m")) #brez nicel 
    day <- as.numeric(format(as.POSIXct(datum2, format="%m/%d/%Y"), "%d")) #brez nicel
    d_Tn <- data.frame(year, month, day, SI1 = data_min[i,j,1:10957]-273, SI2 = data_min[i,j+1,1:10957]-273)
    d_Tx <- data.frame(year, month, day, SI1 = data_max[i,j,1:10957]-273, SI2 = data_max[i,j+1,1:10957]-273)
    d_Tm <- data.frame(year, month, day, SI1 = data3[i,j,1:10957]-273, SI2 = data3[i,j+1,1:10957]-273)
    d_P <- data.frame(year, month, day, SI1 = data_rr[i,j,1:10957]*24*60*60, SI2 = data_rr[i,j+1,1:10957]*24*60*60)

    oivji <- oiv_ind(daily_Tn=d_Tn, daily_Tx=d_Tx, daily_Tm = d_Tm, daily_P=d_P, first.yr=1981, last.yr=2010, subs_missing=FALSE)
    oiv_ind(daily_Tn=Tn,daily_Tx=Tx, daily_P=P, first.yr=1981, last.yr=2010, subs_missing=FALSE)

    climdata <- data.frame(Year = year, Month = month, Day = day, 
                           Tmax = data_max[i,j,1:10957]-273, Tmin = data_min[i,j,1:10957]-273)
    res <- bioclim_thermal(climdata, lat = lats1[j])
    # zz=dmrgrid[,3]
    
    #Tveganje za o?ig jabolk
    # sunburn(climdata, first_d = rep(1,length(unique(year))), last_d = rep(250,length(unique(year))))
    # bioclim_hydrotherm(climdata, lat = lats1[j], elev = )
    
    podatki_tocka <- data.frame(lats = rep(lat[j], length(res$Year)), lons = rep(lon[i], length(res$Year)), 
                                leto = res$Year,
                                CI = res$CI, GST = res$GST, BEDD = res$BEDD,
                                HI = res$HI, WI = res$WI)
    vsi_podatki_fca <- rbind(vsi_podatki_fca, podatki_tocka)
    
  }
}

saveRDS(vsi_podatki_fca, file = "kazalniki_fruclimadapt_historical.rds")
vsi_podatki_fca <- readRDS(file = "kazalniki_fruclimadapt_historical.rds")
yrs <- unique(years(datumi))
kaz <- names(vsi_podatki_fca[4:8])
kaz_SI <- c("CI", "GST","BEDD",
            "Hughlinov indeks","Winklerjev indeks")
kaz_enote <- c("?C", "?C dnevi", "?C dnevi",
               "?C dnevi", "?C dnevi")
kaz

kaz_min <- c( 5,  8,  150,  400, 200)
kaz_max <- c(15, 22, 1500, 2300, 2100)
zac = 2001
kon = 2010

for(k in seq(1,length(kaz),by=1)){
  data <- data.frame(vsi_podatki_fca$lons, vsi_podatki_fca$lats, vsi_podatki_fca$leto, vsi_podatki_fca[[kaz[k]]])
  colnames(data) <- c("lons","lats","leto","agroclim_ind")
  data <- data %>% filter(leto >= zac, leto <= kon)
  data=na.omit(data)
  
  povp <- data %>% group_by(lats,lons) %>%
    summarise(period_mean = mean(agroclim_ind))
  ggplot() +
    geom_tile(data=povp,aes(x=lons,y=lats,fill=period_mean)) +
    geom_sf(data=slovenia_nuts3_mapdata, color=alpha("black",0.4),fill = NA) +
    scale_fill_distiller(kaz_enote[k], palette = "Spectral", limits = c(kaz_min[k], kaz_max[k])) +
    # scale_fill_distiller(kaz_enote[k], palette = "Spectral") +
    labs(x="Geografska dol?ina",y="Geografska ?irina",title=paste0(kaz_SI[k],", obdobje ",zac,"-",kon)) +
    coord_sf(crs = st_crs(4326)) +
    theme_light(base_size = 17)
  ggsave(paste0("historicni/period_mean_",kaz[k],"_",zac,"-",kon,".png"),width = 10, height = 7)
}

zac = 1981
kon = 1990

for(k in seq(1,length(kaz),by=1)){
  data <- data.frame(vsi_podatki_fca$lons, vsi_podatki_fca$lats, vsi_podatki_fca$leto, vsi_podatki_fca[[kaz[k]]])
  colnames(data) <- c("lons","lats","leto","agroclim_ind")
  data <- data %>% filter(leto >= zac, leto <= kon)
  data=na.omit(data)
  
  povp <- data %>% group_by(lats,lons) %>%
    summarise(period_mean = mean(agroclim_ind))
  ggplot() +
    geom_tile(data=povp,aes(x=lons,y=lats,fill=period_mean)) +
    geom_sf(data=slovenia_nuts3_mapdata, color=alpha("black",0.4),fill = NA) +
    scale_fill_distiller(kaz_enote[k], palette = "Spectral", limits = c(kaz_min[k], kaz_max[k])) +
    # scale_fill_distiller(kaz_enote[k], palette = "Spectral") +
    labs(x="Geografska dol?ina",y="Geografska ?irina",title=paste0(kaz_SI[k],", obdobje ",zac,"-",kon)) +
    coord_sf(crs = st_crs(4326)) +
    theme_light(base_size = 17)
  ggsave(paste0("historicni/period_mean_",kaz[k],"_",zac,"-",kon,".png"),width = 10, height = 7)
}

zac = 1981
kon = 2010

for(k in seq(1,length(kaz),by=1)){
  data <- data.frame(vsi_podatki_fca$lons, vsi_podatki_fca$lats, vsi_podatki_fca$leto, vsi_podatki_fca[[kaz[k]]])
  colnames(data) <- c("lons","lats","leto","agroclim_ind")
  data <- data %>% filter(leto >= zac, leto <= kon)
  data=na.omit(data)
  
  povp <- data %>% group_by(lats,lons) %>%
    summarise(period_mean = mean(agroclim_ind))
  ggplot() +
    geom_tile(data=povp,aes(x=lons,y=lats,fill=period_mean)) +
    geom_sf(data=slovenia_nuts3_mapdata, color=alpha("black",0.4),fill = NA) +
    scale_fill_distiller(kaz_enote[k], palette = "Spectral", limits = c(kaz_min[k], kaz_max[k])) +
    # scale_fill_distiller(kaz_enote[k], palette = "Spectral") +
    labs(x="Geografska dol?ina",y="Geografska ?irina",title=paste0("Kazalnik ",kaz_SI[k],", obdobje ",zac,"-",kon)) +
    coord_sf(crs = st_crs(4326)) +
    theme_light(base_size = 17)
  ggsave(paste0("historicni/period_mean_",kaz[k],"_",zac,"-",kon,".png"),width = 10, height = 7)
}




lat_LJ <- 46.0625
lon_LJ <- 14.5625
data_LJ <- vsi_podatki_fca %>% filter(lats == lat_LJ, lons == lon_LJ)
# HUGHLIN
data_lja <- data.frame(leto = data_LJ$leto, ime = rep("Huglinov indeks", length(data_LJ$leto)), TG = data_LJ$HI)
data_LJ <- rbind(data_lja)
cols <- c("Huglinov indeks"="steelblue3")
aa<-ggplot(data_LJ, aes(x=leto, y = TG, group = ime, colour = ime)) +
  theme_light(base_size = 14)+ 
  geom_point(size=2)+geom_line(lty=3,lwd = 1)+
  labs(title = paste("Ljubljana - Be?igrad",sep="")) +  ylab("HI [?C dnevi]") + xlab("leto") + 
  scale_colour_manual(name="Legenda",values=cols)+ theme(legend.text=element_text(size=15),
                                                         axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
                                                         legend.position='bottom') 
aa# scale_x_continuous(limits=c(70,170))
ggsave(paste("poro?ilo ministrstvo/HI.png",sep=""),aa, dpi=500, height=6, width=9, units="in")

data_LJ <- vsi_podatki_fca %>% filter(lats == lat_LJ, lons == lon_LJ)
# COLD NIGHT
data_lja <- data.frame(leto = data_LJ$leto, ime = rep("Indeks hladnih no?i", length(data_LJ$leto)), TG = data_LJ$CI)
data_LJ <- rbind(data_lja)
cols <- c("Indeks hladnih no?i"="steelblue3")
aa<-ggplot(data_LJ, aes(x=leto, y = TG, group = ime, colour = ime)) +
  theme_light(base_size = 14)+ 
  geom_point(size=2)+geom_line(lty=3,lwd = 1)+
  labs(title = paste("Ljubljana - Be?igrad",sep="")) +  ylab("CI [?C]") + xlab("leto") + 
  scale_colour_manual(name="Legenda",values=cols)+ theme(legend.text=element_text(size=15),
                                                         axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
                                                         legend.position='bottom') 
aa# scale_x_continuous(limits=c(70,170))
ggsave(paste("poro?ilo ministrstvo/CI.png",sep=""),aa, dpi=500, height=6, width=9, units="in")

# HI, WI, BEDD  
data_LJ <- vsi_podatki_fca %>% filter(lats == lat_LJ, lons == lon_LJ)
data_lja <- data.frame(leto = data_LJ$leto, ime = rep("Huglinov indeks", length(data_LJ$leto)),
                       TG = data_LJ$HI)
data_ljb <- data.frame(leto = data_LJ$leto, ime = rep("Biolo?ka temperaturna vsota", length(data_LJ$leto)),
                       TG = data_LJ$BEDD)
data_ljc <- data.frame(leto = data_LJ$leto, ime = rep("Winklerjev indeks", length(data_LJ$leto)),
                       TG = data_LJ$WI)
data_LJ <- rbind(data_lja,data_ljb,data_ljc)
cols <- c("Huglinov indeks"="olivedrab3","Winklerjev indeks"="forestgreen","Biolo?ka temperaturna vsota"="burlywood4")
aa<-ggplot(data_LJ, aes(x=leto, y = TG, group = ime, colour = ime)) +
  theme_light(base_size = 14)+ 
  geom_point(size=2)+geom_line(lty=3,lwd = 1)+
  labs(title = paste("Ljubljana - Be?igrad",sep="")) +  ylab("[?C dnevi]") + xlab("leto") + 
  scale_colour_manual(name="Legenda",values=cols)+ theme(legend.text=element_text(size=15),
                                                         axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
                                                         legend.position='bottom') 
aa# scale_x_continuous(limits=c(70,170))
ggsave(paste("poro?ilo ministrstvo/HI_WI_BEDD.png",sep=""),aa, dpi=500, height=6, width=9, units="in")







# 
# 
# 
# 
# # celotna bli?nja okolica Slovenije, letne povpre?ne vrednosti za vseh 70 let izra?unamo
# 
# start_lon <- 13 + 23/60 + 47.81/3600 # meje https://sl.wikipedia.org/wiki/Geografija_Slovenije
# end_lon <- 16 + 36/60 + 7.69/3600
# 
# start_lat<-45 + 25/60 +18.34/3600
# end_lat<-46 + 54/60 + 37.52/3600
# 
# df <- shapename %>%
#   # 1. Project to lon/lat
#   st_transform(4326) %>%
#   # 2 Extract coordinates
#   st_coordinates() %>%
#   # 3 to table /tibble
#   as.data.frame()
# lats <- df$Y
# lons <- df$X
# 
# 
# 
# vsi_podatki <- data.frame()
# for(i in lons){
#   for(j in lats){
#     print(i)
#     print(j)
#     
#     idx_start_lon2 <- which(abs(lon - i)== min(abs(lon - i)))
#     idx_start_lat2 <- which(abs(lat - j)== min(abs(lat - j)))
#     
#     start3<-c(idx_start_lon2,idx_start_lat2, 10956)
#     count3<-c(1,1,15707)
#     datumi0 <- seq(1,count3[3],by = 1)
#     datumi <- as.POSIXct((start3[3]+datumi0)*pretvorba_sek_v_dan1,origin="1950-01-01 00:00:00")
#     leta <- format(datumi,format="%Y")
#     
#     # povpre?na dnevna T
#     nc_data <- nc_open(paste0(path,"tg","_ens_mean_0.1deg_reg_v27.0e.nc"))
#     data3 <- ncvar_get(nc_data,"tg",start=start3, count=count3)
#     datum2 = format(as.POSIXct(datumi, format="%Y-%m-%d"), "%m/%d/%Y")
#     dataPOVP<-structure(data3, .Names = datum2)
#     data3 <- data.frame(datum = datumi, leto = leta, tg = data3)
#     
#     TG_of_warmest_quarter <- bio10(dataPOVP) 
#     TG_of_coldest_quarter	<- bio11(dataPOVP) 
#     povp_tg <- aggregate(tg ~ leto , data3 , "mean")
#     nc_close(nc_data) # konec branja
#     
#     # minimalna dnevna T
#     nc_data_min <- nc_open(paste0(path,"tn","_ens_mean_0.1deg_reg_v27.0e.nc"))
#     data_min <- ncvar_get(nc_data_min, "tn", start=start3, count=count3)
#     dataMIN<-structure(data_min, .Names = datum2)
#     data_min <- data.frame(datum = datumi, leto = leta, tn = data_min)
#     Sums_Tmin10 <- stn10(dataMIN) 
#     Sums_Tmin15	<- stn15(dataMIN)    
#     povp_min <- aggregate(tn ~ leto, data_min , "mean")
#     nc_close(nc_data_min) # konec branja
#     
#     # maksimalna dnevna T
#     nc_data_max<- nc_open(paste0(path,"tx","_ens_mean_0.1deg_reg_v27.0e.nc"))
#     data_max <- ncvar_get(nc_data_max, "tx", start=start3, count=count3)
#     dataMAX<-structure(data_max, .Names = datum2)
#     data_max <- data.frame(datum = datumi, leto = leta, tx = data_max)
#     Days_Tmax32	<- d32(dataMAX)
#     Sums_Tmax32 <- stx32(dataMAX) 
#     povp_max <- aggregate(tx ~ leto, data_max , "mean")
#     nc_close(nc_data_max) # konec branja
#     
#     # padavine dnevne
#     nc_data_rr<- nc_open(paste0(path,"rr","_ens_mean_0.1deg_reg_v27.0e.nc"))
#     data_rr <- ncvar_get(nc_data_rr, "rr", start=start3, count=count3)
#     dataRR<-structure(data_rr, .Names = datum2)
#     data_rr <- data.frame(datum = datumi, leto = leta, rr = data_rr)
#     povp_rr <- aggregate(rr ~ leto, data_rr , "mean")    
#     Dry_days <- dd(dataRR)
#     Prec_wettest_month	<- bio13(dataRR) 
#     Prec_driest_month	<- bio14(dataRR)
#     Heavy_prec_days <- d50mm(dataRR) 
#     Very_wet_days	<- d95p(dataRR)
#     Growing_season_prec	<- gsr(dataRR)
#     Nongrowing_season_prec <- ngsr(dataRR)
#     nc_close(nc_data_rr) # konec branja
#     
#     Prec_warmest_quarter	<- bio18(dataRR,dataPOVP)
#     Prec_coldest_quarter	<- bio19(dataRR,dataPOVP)
#     
#     podatki_tocka <- data.frame(lats = rep(j, length(povp_tg$leto)), lons = rep(i, length(povp_tg$leto)), 
#                                 leto = povp_tg$leto,
#                                 tpovp = povp_tg$tg, tmin = povp_min$tn, 
#                                 tmax = povp_max$tx, rr = povp_rr$rr, TG_of_warmest_quarter, TG_of_coldest_quarter,
#                                 Sums_Tmin10, Sums_Tmin15, Days_Tmax32, Sums_Tmax32,
#                                 Dry_days, Prec_wettest_month, Prec_driest_month,
#                                 Heavy_prec_days, Very_wet_days, Growing_season_prec,
#                                 Nongrowing_season_prec, Prec_warmest_quarter, Prec_coldest_quarter)
#     vsi_podatki <- rbind(vsi_podatki, podatki_tocka)
#     
#   }
# }
# 
# 
# ############################################################
# # TEMPERATURE based indicators
# 
# datum2 = format(as.POSIXct(datumi, format="%Y-%m-%d"), "%m/%d/%Y")
# dataPOVP<-structure(data2, .Names = datum2)
# dataMIN<-structure(data_min, .Names = datum2)
# dataMAX<-structure(data_max, .Names = datum2)
# dataRR<-structure(data_rr, .Names = datum2)
# 
# TG_of_warmest_quarter <- bio10(dataPOVP) 
# TG_of_coldest_quarter	<- bio11(dataPOVP) 
# Days_Tmax32	<- d32(dataMAX)
# Sums_Tmin10 <- stn10(dataMIN) 
# Sums_Tmin15	<- stn15(dataMIN)
# Sums_Tmax32 <- stx32(dataMAX) 
# 
# # PRECIPITATION based indicators
# Dry_days <- dd(dataRR)
# Prec_wettest_month	<- bio13(dataRR) 
# Prec_driest_month	<- bio14(dataRR)
# Heavy_prec_days <- d50mm(dataRR) 
# Very_wet_days	<- d95p(dataRR)
# Growing_season_prec	<- gsr(dataRR)
# Nongrowing_season_prec <- ngsr(dataRR)
# 
# # Tmin Tmax
# Diurnal_temp_range <-	dtr(dataMAX,dataMIN)
# 
# # PRECIPITATION and ET based indicators
# Effective_prec	<- ep
# SPEI12 <- spei12
# SPI12 <- spi12
# 
# # PRECIPITATION and T based indicators
# Prec_warmest_quarter	<- bio18(dataRR,dataPOVP)
# Prec_coldest_quarter	<- bio19(dataRR,dataPOVP)
# 
# # SNOW DEPTH
# Date_first_perm_snow_cover <-	fpsc 
# Date_first_snow_cover <-	fsc 
# Date_last_perm_snow_cover <-	lpsc
#    
#    # narejeno na ARSO

# SPEI1 <- 	spei1
# SPEI3 <- spei3
# SPEI6 <- spei6
# SPI1 <- spi1
# SPI12 <- spi12 
# SPI3 <- spi3 
# SPI6 <- spi6
# Number_snow_cov_days <-	scd 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# # Preoblikovat za Slovenijo
# # ena to?ka v Sloveniji ob enem ?asovnem koraku
# start_lat1 = 45
# start_lon1 = 14.5
# idx_start_lon1 <- which(abs(lon - start_lon1)==min (abs(lon - start_lon1)))
# idx_start_lat1 <- which(abs(lat - start_lat1)==min (abs(lat - start_lat1)))
# 
# start1<-c(idx_start_lon1,idx_start_lat1, 10000)
# count1<-c(1,1,1000)
# data1 <- ncvar_get(nc_data,  "tg",start=start1, count=count1)
# data1
# 
# ##########
# # ena to?ka v Sloveniji celoten ?asovni niz
# start_lat1 = 45
# start_lon1 = 14.5
# idx_start_lon1 <- which(abs(lon - start_lon1)== min(abs(lon - start_lon1)))
# idx_start_lat1 <- which(abs(lat - start_lat1)== min(abs(lat - start_lat1)))
# 
# start2<-c(idx_start_lon1,idx_start_lat1, 10956) #10956 = 1980-01-01
# count2<-c(1,1,15707)
# 
# # povpre?na dnevna T
# data2 <- ncvar_get(nc_data,"tg",start=start2, count=count2)
# nc_close(nc_data) # konec branja
# 
# # minimalna dnevna T
# nc_data_min <- nc_open(paste0(path,"tn","_ens_mean_0.1deg_reg_v27.0e.nc"))
# data_min <- ncvar_get(nc_data_min, "tn", start=start2, count=count2)
# nc_close(nc_data_min) # konec branja
# 
# # maksimalna dnevna T
# nc_data_max<- nc_open(paste0(path,"tx","_ens_mean_0.1deg_reg_v27.0e.nc"))
# data_max <- ncvar_get(nc_data_max, "tx", start=start2, count=count2)
# nc_close(nc_data_max) # konec branja
# 
# # padavine dnevne
# nc_data_rr<- nc_open(paste0(path,"rr","_ens_mean_0.1deg_reg_v27.0e.nc"))
# data_rr <- ncvar_get(nc_data_rr, "rr", start=start2, count=count2)
# nc_close(nc_data_rr) # konec branja
# 
# data2
# datumi0 <- seq(1,count2[3],by = 1)
# datumi <- as.POSIXct((start2[3]+datumi0)*pretvorba_sek_v_dan1,origin="1950-01-01 00:00:00")  
# 
# data_ena_lokacija <- data.frame(datum = datumi, tpovp = data2, tmin = data_min, tmax = data_max, rr = data_rr)
