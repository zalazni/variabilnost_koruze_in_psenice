setwd("C:/Users/ZalaZn/OneDrive - Univerza v Ljubljani/CRP kazalniki 2024/delo") # služba
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

library(ggspatial)
library(sf)
library(ncdf4)
library(ClimInd)
library(pracma)
library(spData)

library(maxnet)
library(dismo) 
library(lubridate)
library(fruclimadapt)
library(data.table)
###################################################################
path <- "C:/Users/ZalaZn/OneDrive - Univerza v Ljubljani/DR/podatki_hist/OPSI/" # sluzba
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
# start1<-c(1,1,1)
# count1<-c(nx,ny, 10957)
# ############################################################# 
# # ET
# data <- ncvar_get(nc_data,"evspsblpot",start=start1, count=count1)
# data_et <- data
# #############################################################
# pretvorba_sek_v_dan1 = 60*60*24 #?eprav imam dnevne podatke moram to dat, ker as.positxct meri v sekundah
# # d<-data[1:nx,1:ny,10957]
# # dan<-as.POSIXct((start1[3]+count1[3])*pretvorba_sek_v_dan1,origin="1980-12-31 00:00:00", tz = "GMT")  
# # d <- flipdim(d, 2) ########################
# # image(d)
# # title(dan)
# # lonlattime <- as.matrix(expand.grid(lon,rev(lat))) ############################# this might take several seconds #reshape whole lswt_array
# # d_vec_long <- as.vector(d)
# # length(d_vec_long) # by now it should be 1447344 #Create data.frame
# # d_data <- data.frame(cbind(lonlattime, d_vec_long))
# # colnames(d_data) <- c("longitude","latitude","ET")
# # 
# # ggplot() +
# #   geom_tile(data=na.omit(d_data),aes(x=longitude,y=latitude,fill=ET)) +
# #   geom_sf(data=slovenia_nuts3_mapdata, color=alpha("black",0.4),fill = NA) +
# #   geom_point(aes(x=14.516,y=46.06))+
# #   scale_fill_distiller("ET", palette = "Spectral") +
# #   labs(x="Longitude",y="Latitude",title=paste("Evapotranspiration,",dan)) +
# #   coord_sf(crs = st_crs(4326)) + xlim(c(start_lon,end_lon)) + ylim(c(start_lat,end_lat)) +
# #   theme_light() 
# # ggsave(paste0("primerSLO_ET",dan,".png"),width = 10, height = 7)
# 
# ########### 
# # povpre?na dnevna T
# datumi0 <- seq(0,count1[3]-1,by = 1)
# datumi <- as.POSIXct((start1[3]+datumi0)*pretvorba_sek_v_dan1,origin="1980-12-31 00:00:00")
# leta <- format(datumi,format="%Y")
# nc_data <- nc_open(paste0(path,"tas","_12km_ARSO_v5_day_19810101_20101231.nc"))
# data3 <- ncvar_get(nc_data,"tas",start=start1, count=count1)
# # d3 <- flipdim(dataPOVP, 2)
# datum2 = format(as.POSIXct(datumi, format="%Y-%m-%d"), "%m/%d/%Y")
# # dataPOVP<-structure(data3, .Names = datum2)
# # data3 <- data.frame(datum = datum2, leto = leta, tg = data3[,,1:10957])
# nc_close(nc_data) # konec branja
# 
# # minimalna dnevna T
# nc_data_min <- nc_open(paste0(path,"tasmin","_12km_ARSO_v5_day_19810101_20101231.nc"))
# data_min <- ncvar_get(nc_data_min, "tasmin", start=start1, count=count1)
# dataMIN<-structure(data_min, .Names = datum2)
# # data_min <- data.frame(datum = datumi, leto = leta, tn = data_min)
# nc_close(nc_data_min) # konec branja
# 
# # maksimalna dnevna T
# nc_data_max<- nc_open(paste0(path,"tasmax","_12km_ARSO_v5_day_19810101_20101231.nc"))
# data_max <- ncvar_get(nc_data_max, "tasmax", start=start1, count=count1)
# # dataMAX<-structure(data_max, .Names = datum2)
# # data_max <- data.frame(datum = datumi, leto = leta, tx = data_max)
# nc_close(nc_data_max) # konec branja
# 
# # padavine dnevne
# nc_data_rr<- nc_open(paste0(path,"pr","_12km_ARSO_v5_day_19810101_20101231.nc"))
# data_rr <- ncvar_get(nc_data_rr, "pr", start=start1, count=count1)
# # dataRR<-structure(data_rr, .Names = datum2)
# # data_rr <- data.frame(datum = datumi, leto = leta, rr = data_rr)
# nc_close(nc_data_rr) # konec branja
# 
# # celotna bli?nja okolica Slovenije, letne vrednosti agroklimatskih kazalnikov za vseh 70 let izra?unamo
# 
# lats1 <- seq(1,24,by=1)
# lons1 <- seq(1,40,by=1)
# 
# ######################### HIST


# Risanje kazalnikov
# vsi_podatki0 <- vsi_podatki
vsi_podatki <- readRDS(file = "C:/Users/ZalaZn/OneDrive - Univerza v Ljubljani/CRP kazalniki 2024/kazalniki_1981-2010_psenica.rds")

# yrs <- unique(years(datumi))
# vsi_podatki$ID<-seq(1,nrow(vsi_podatki))

kaz <- names(vsi_podatki)[4:36]
kaz


# for(i in yrs){
#   for(k in seq(4,length(kaz)-1,by=1)){
#     data <- vsi_podatki %>% filter(leto == i)
#     data=na.omit(data)
#     min = min(na.omit(vsi_podatki[[kaz[k]]]))
#     max = max(na.omit(vsi_podatki[[kaz[k]]]))
#     ggplot() +
#       geom_tile(data=data,aes(x=lons,y=lats,fill=data[[kaz[k]]])) +
#       geom_sf(data=slovenia_nuts3_mapdata, color=alpha("black",0.4),fill = NA) +
#       scale_fill_distiller(kaz_enote[k], palette = "Spectral",  limits = c(min, max)) +
#       # scale_fill_distiller(kaz_enote[k], palette = "Spectral",  limits = c(kaz_min0[k], kaz_max0[k])) +
#       labs(x="Geografska dolzina",y="Geografska sirina",title=paste0(kaz_SI[k],", leto ",i)) +
#       coord_sf(crs = st_crs(4326)) +
#       theme_light(base_size = 17)
#     ggsave(paste0("rezultati/karte_psenica/hist/yearly_",kaz[k],"_",i,".png"),width = 10, height = 7)
#   }
# }


zac = 1981
kon = 2010
# path1 <- "C:/Users/ZalaZn/OneDrive - Univerza v Ljubljani/CRP kazalniki 2024/delo/" # sluzba


for(k in seq(1,length(kaz),by=1)){
  data <- data.frame(vsi_podatki$lons, vsi_podatki$lats, vsi_podatki$leto, vsi_podatki[[kaz[k]]])
  colnames(data) <- c("lons","lats","leto","agroclim_ind")
  data <- data %>% filter(leto >= zac, leto <= kon)
  data=na.omit(data)
  min = min(na.omit(vsi_podatki[[kaz[k]]]))
  max = max(na.omit(vsi_podatki[[kaz[k]]]))

  povp <- data %>% group_by(lats,lons) %>%
    summarise(period_mean = mean(agroclim_ind))
  ggplot() +
    geom_tile(data=povp,aes(x=lons,y=lats,fill=period_mean)) +
    geom_sf(data=slovenia_nuts3_mapdata, color=alpha("black",0.4),fill = NA) +
    scale_fill_distiller("povp_obd",palette = "Spectral", limits = c(min,max)) +
    # scale_fill_distiller(kaz_enote[k], palette = "Spectral") +
    labs(x="Geografska dolzina",y="Geografska sirina",title=paste0("Kazalnik ",kaz[k],", obdobje ",zac,"-",kon)) +
    coord_sf(crs = st_crs(4326)) +
    theme_light(base_size = 17)
  ggsave(paste0("karte_psenica/hist_period_mean_",kaz[k],"_",zac,"-",kon,".png"),width = 10, height = 7)
}

########## PROJEKCIJE

######################################### PROJEKCIJE MEDIANA MODELOV 2071 - 2100

model <- c("CNRM-CERFACS-CNRM-CM5", 
           "ICHEC-EC-EARTH",
           "IPSL-IPSL-CM5A-MR",
           "MOHC-HadGEM2-ES", # samo do 30. 11. 2099 pri padavinah, zato tudi pri Tmin in Tmax .nc fajl spremenim, da gre le do 30.11.
           "MPI-M-MPI-ESM-LR",
           "MPI-M-MPI-ESM-LR")
drugo <- c("_r1i1p1_CLMcom-CCLM4-8-17_v1",
           "_r3i1p1_DMI-HIRHAM5_v1",
           "_r1i1p1_IPSL-INERIS-WRF331F_v1",
           "_r1i1p1_KNMI-RACMO22E_v2",
           "_r1i1p1_CLMcom-CCLM4-8-17_v1",
           "_r1i1p1_SMHI-RCA4_v1a")

get_nc_data <- function(path, model1, rcp, drugo1, leto_zac, leto_kon1, leto_kon_rr1, nx, ny, z){

  nc_data_max <- nc_open(paste0(path,"tasmax","_12km_",model1,"_",rcp,drugo1,"_day_",leto_zac,"0101_",leto_kon1,".nc"))
  time <- ncvar_get(nc_data_max, "time")
  count_len <- length(time)  
  start1<-c(1,1,1)
  count1<-c(nx,ny, count_len)  
  data_max <- ncvar_get(nc_data_max, "tasmax", start=start1, count=count1)
  lon <- ncvar_get(nc_data_max, "lon")
  lat <- ncvar_get(nc_data_max, "lat")
  nc_close(nc_data_max) # konec branja

  nc_data_min <- nc_open(paste0(path,"tasmin","_12km_",model1,"_",rcp,drugo1,"_day_",leto_zac,"0101_",leto_kon1,".nc"))
  data_min <- ncvar_get(nc_data_min, "tasmin", start=start1, count=count1)
  nc_close(nc_data_min) 

  nc_data <- nc_open(paste0(path,"tas","_12km_",model1,"_",rcp,drugo1,"_day_",leto_zac,"0101_",leto_kon1,".nc"))
  data3 <- ncvar_get(nc_data,"tas",start=start1, count=count1)
  nc_close(nc_data) # konec branja

  nc_data_rr<- nc_open(paste0(path,"pr","_12km_",model1,"_",rcp,drugo1,"_day_",leto_zac,"0101_",leto_kon_rr1,".nc"))
  time1 <- ncvar_get(nc_data_rr, "time")
  count_len1 <- length(time1)  
  count2<-c(nx,ny, count_len1) 
  data_rr <- ncvar_get(nc_data_rr, "pr", start=start1, count=count2)
  nc_close(nc_data_rr)

  nc_data_et<- nc_open(paste0(path,"evspsblpot","_12km_",model1,"_",rcp,drugo1,"_day_",leto_zac,"0101_",leto_kon1,".nc"))
  data_et <- ncvar_get(nc_data_et, "evspsblpot", start=start1, count=count1)
  nc_close(nc_data_et)
  # nc_data_time <- nc_open(paste0(path, "pr", "_12km_MOHC-HadGEM2-ES_", rcp, "_r1i1p1_KNMI-RACMO22E_v2", "_day_", leto_zac, "0101_", "20991130", ".nc")) # Use a representative file for time
  # 
  # nc_close(nc_data_time)
  
  return(list(time = time1, lon = lon, lat = lat, data_max = data_max, 
              data_min = data_min, data3 = data3, data_rr = data_rr, data_et = data_et))
  
}


drugo <- c("_r1i1p1_CLMcom-CCLM4-8-17_v1",
           "_r3i1p1_DMI-HIRHAM5_v1",
           "_r1i1p1_IPSL-INERIS-WRF331F_v1",
           "_r1i1p1_KNMI-RACMO22E_v2",
           "_r1i1p1_CLMcom-CCLM4-8-17_v1",
           "_r1i1p1_SMHI-RCA4_v1a")
leto_zac <- "2071"
leto_kon <- c("21001231","21001231","21001231","20991231","21001231","21001231")
leto_kon_rr <- c("21001231","21001231","21001231","20991130","21001231","21001231")

en_model0 <- function(rcp, leto_zac, obd,z){
  povp_obd <- data.table()
  print(c("zacetek za: ",model[z]))    
  nc_data_info <- get_nc_data(path, model[z], rcp, drugo[z], leto_zac, leto_kon[z], leto_kon_rr[z], nx, ny)
  time <- nc_data_info$time
  lon <- nc_data_info$lon
  lat <- nc_data_info$lat
  data_max <- nc_data_info$data_max
  data_min <- nc_data_info$data_min
  data3 <- nc_data_info$data3
  data_rr <- nc_data_info$data_rr
  data_et <- nc_data_info$data_et
  rm(nc_data_info) 
  
  datumi <- as.POSIXct((time) * 60 * 60 * 24, origin = paste0("1949-12-31 00:00:00")) #Directly use time
  leta <- format(datumi, format = "%Y")
  datum2 <- format(datumi, format = "%Y-%m-%d")
  
  leto = format(datumi, format = "%Y")
  leta1 <- seq(min(leto),max(leto),by=1)
  lats1 <- seq(1,ny,by=1)
  lons1 <- seq(1,nx,by=1)
  n_rows <- 24
  n_cols <- 40
  n_layers <- 19
  vsi_podatki <- data.table()
  
  for(i in lons1){
    for(j in lats1){
      if(all(is.na(data3[i,j,1:length(time)]))){
        next
      }
      else{
        data_temp <- data.table(Date = format(as.Date(datum2),"%m/%d/%Y"), meseci = as.numeric(format(datumi,format="%m")), Temp = data3[i,j,1:length(time)]-273)
        # data_temp <- data_temp %>% filter(meseci >= 4, meseci <= 10)
        
        data_tmax <- data.table(Date = format(as.Date(datum2),"%m/%d/%Y"), meseci = as.numeric(format(datumi,format="%m")), Tmax = data_max[i,j,1:length(time)]-273)
        # data_tmax <- data_tmax %>% filter(meseci >= 4, meseci <= 10)
        
        data_tmin <- data.table(Date = format(as.Date(datum2),"%m/%d/%Y"), meseci = as.numeric(format(datumi,format="%m")), Tmin = data_min[i,j,1:length(time)]-273)
        # data_tmin <- data_tmin %>% filter(meseci >= 4, meseci <= 10)
        
        data_pad <- data.table(Date = format(as.Date(datum2),"%m/%d/%Y"), meseci = as.numeric(format(datumi,format="%m")), pad = data_rr[i,j,1:length(time)]*24*60*60)
        # data_pad <- data_pad0 %>% filter(meseci >= 4, meseci <= 10)
        
        data_etp <- data.table(Date = format(as.Date(datum2),"%m/%d/%Y"), meseci = as.numeric(format(datumi,format="%m")), et = data_et[i,j,1:length(time)]*24*60*60)
        # data_etp <- data_et0 %>% filter(meseci >= 4, meseci <= 10)
        
        growing_degree_days <- gd4(structure(data_temp$Temp, .Names = data_temp$Date))
        Growing_season_length <- gsl(structure(data_temp$Temp, .Names = data_temp$Date)) 
        End_growing_season <- ogs6(structure(data_temp$Temp, .Names = data_temp$Date)) + Growing_season_length + 90
        Sums_Tmax32 <- stx32(structure(data_tmax$Tmax, .Names = data_tmax$Date)) ###
        TG_of_warmest_quarter <- bio10(structure(data_temp$Temp, .Names = data_temp$Date)) #Kelvini
        dtr <- (data_tmax$Tmax+273)/(data_temp$Temp+273)
        dtr1 <- (data_tmin$Tmin+273)/(data_temp$Temp+273)
        Diurnal_temp_rangeN <-	dtr(structure(dtr, .Names = data_temp$Date),structure(dtr1, .Names = data_temp$Date))
        cons_summer_days <- csd(structure(data_tmax$Tmax, .Names = data_tmax$Date))
        # Warm_spell_duration <- wsdi(structure(data_tmax$Tmax, .Names = data_tmax$Date))
        
        Max_consecutive_dry_days <- cdd(structure(data_pad$pad, .Names = data_pad$Date))
        longest_wet_period <- cwd(structure(data_pad$pad, .Names = data_pad$Date))    
        r20mm <- r20mm(structure(data_pad$pad, .Names = data_pad$Date))
        Heavy_prec_days <- d50mm(structure(data_pad$pad, .Names = data_pad$Date)) 
        SDII <- sdii(structure(data_pad$pad, .Names = data_pad$Date))
        Prec_wettest_month	<- bio13(structure(data_pad$pad, .Names = data_pad$Date)) 
        frost_days <- fd(structure(data_tmin$Tmin, .Names = data_tmin$Date))

        podatki_percentil_10_tmin = readRDS("../tn10p_referencno_obdobje_psenica+koruza.rds")
        podatki_percentil_10_tmin2<- subset(podatki_percentil_10_tmin, as.numeric(latsi)==lat[j]  & podatki_percentil_10_tmin$lon==as.numeric(lon[i]))
        
        CSDI_optimized <- function(tmin_data) {
          below_threshold <- tmin_data < podatki_percentil_10_tmin2$Tn10p[1]  # Uporaba `rle` (Run Length Encoding) za iskanje zaporednih nizov
          rle_result <- rle(below_threshold)
          cold_spell_lengths <- rle_result$lengths[rle_result$values == TRUE & rle_result$lengths >= 6]
          csdi_days_count <- sum(cold_spell_lengths)
          return(csdi_days_count)
        }
        Cold_spell_duration <- function(df) {
          df$year <- format(as.POSIXct(df$Date, format="%m/%d/%Y"), "%Y")
          annual_csdi <- df %>%
            group_by(year) %>%
            summarise(csdi_annual_sum = CSDI_optimized(Tmin)) %>%
            ungroup() 
          return(annual_csdi)
        }
        CSDI = Cold_spell_duration(data_tmin)
        
        cons_frost_days <- cfd(structure(data_tmin$Tmin, .Names = data_tmin$Date))
        ice_days <- id(structure(data_tmax$Tmax, .Names = data_tmax$Date))    
        Effective_prec	<- ep(structure(data_etp$et, .Names = data_etp$Date), structure(data_pad$pad, .Names = data_pad$Date))
        Growing_season_prec	<- gsr(structure(data_pad$pad, .Names = data_pad$Date)) ###
        Nongrowing_season_prec <- ngsr(structure(data_pad$pad, .Names = data_pad$Date))
        su <- su(structure(data_tmax$Tmax, .Names = data_tmax$Date))
        tr <- tr(structure(data_tmin$Tmin, .Names = data_tmin$Date))
        tn90p <- tn90p(structure(data_tmin$Tmin, .Names = data_tmin$Date))
        # vwd <- vwd(data = structure(data_tmax$Tmax, .Names = data_tmax$Date))
        Sums_Tmin10 <- stn10(structure(data_tmin$Tmin, .Names = data_tmin$Date))
        Sums_Tmin15 <- stn15(structure(data_tmin$Tmin, .Names = data_tmin$Date))

        # percentil90<-data.frame(latsi=lat[j],lonsi=lon[i],T90p=as.numeric(quantile(data_tmin$Tmin,0.9)))
        podatki_percentil_90_tmin = readRDS("../tn90p_referencno_obdobje_psenica+koruza.rds")
        podatki_percentil_90_tmin2<- subset(podatki_percentil_90_tmin, as.numeric(latsi)==lat[j]  & podatki_percentil_90_tmin$lon==as.numeric(lon[i]))
        tn90p = podatki_percentil_90_tmin2$T90p[1]
        data_tmin_90p<-data_tmin
        # data_tmin_90p$Datum <-as.Date(data_tmin_90p$Date)
        data_tmin_90p$leto <-format(as.POSIXct(data_tmin_90p$Date, format="%m/%d/%Y"), "%Y")
        t90_test <- data_tmin_90p %>% 
          mutate(tmin90 = ifelse(Tmin >= podatki_percentil_90_tmin2$T90p,1,0),ind1=1)
        t90_test <- t90_test %>%
          group_by(leto) %>% summarise(sum=sum(tmin90),
                                       n=sum(ind1), tn90p = sum/n *100)
        
        podatki_percentil_99_tmax = readRDS("../tx99p_referencno_obdobje_psenica+koruza.rds")
        podatki_percentil_99_tmax2<- subset(podatki_percentil_99_tmax, as.numeric(latsi)==lat[j]  & podatki_percentil_99_tmax$lon==as.numeric(lon[i]))
        tx99p = podatki_percentil_99_tmax2$Tx99p[1]
        data_tmax_99p<-data_tmax
        # data_tmax_99p$Datum <-as.Date(data_tmax_99p$Date)
        data_tmax_99p$leto <-format(as.POSIXct(data_tmax_99p$Date, format="%m/%d/%Y"), "%Y")
        tx99_test <- data_tmax_99p %>% 
          mutate(tmax99 = ifelse(Tmax >= podatki_percentil_99_tmax2$Tx99p,1,0),ind1=1)
        tx99_test <- tx99_test %>%
          group_by(leto) %>% summarise(sum=sum(tmax99),
                                       n=sum(ind1), tx99p = sum)
        
        podatki_percentil_90_tmax = readRDS("../tx90p_referencno_obdobje_psenica+koruza.rds")
        podatki_percentil_90_tmax2<- subset(podatki_percentil_90_tmax, as.numeric(latsi)==lat[j]  & podatki_percentil_90_tmax$lon==as.numeric(lon[i]))
        tx90p = podatki_percentil_90_tmax2$Tx90p[1]
        data_tmax_90p <- data_tmax %>%
          mutate(leto = format(as.POSIXct(Date, format = "%m/%d/%Y"), "%Y"),
                 tmax90 = ifelse(Tmax >= tx90p, 1, 0),
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
        

        podatki_percentil_75_tmean = readRDS("../t75p_referencno_obdobje_psenica+koruza.rds")
        podatki_percentil_75_tmean2<- subset(podatki_percentil_75_tmean, as.numeric(latsi)==lat[j]  & podatki_percentil_75_tmean$lon==as.numeric(lon[i]))
        
        podatki_percentil_75_rr = readRDS("../rr75p_referencno_obdobje_psenica+koruza.rds")
        podatki_percentil_75_rr2<- subset(podatki_percentil_75_rr, as.numeric(latsi)==lat[j]  & podatki_percentil_75_rr$lon==as.numeric(lon[i]))
        
        data_wwd <- data.frame(date = data_tmax$Date, Tmean = data_temp$Temp, RR = data_pad$pad)
        warm_wet_days <- function(data) {
          data <- data %>% mutate(warm_wet = (Tmean > podatki_percentil_75_tmean2$T75p[1]) & (RR > podatki_percentil_75_rr2$rr75p[1]))
          data$year <- format(as.POSIXct(data$date, format="%m/%d/%Y"), "%Y")
          warm_wet_days_count <- data %>%
            filter(warm_wet) %>%
            group_by(year) %>%
            summarize(warm_wet_days = n(),
                      .groups = "drop")
          return(warm_wet_days_count)
        }            

        WWD <- warm_wet_days(data_wwd)
        temp_day1 <- data.table(Year = format(as.POSIXct(data_temp$Date, format="%m/%d/%Y"), "%Y"), 
                                Month = format(as.POSIXct(data_temp$Date, format="%m/%d/%Y"), "%m"), 
                                Day = format(as.POSIXct(data_temp$Date, format="%m/%d/%Y"), "%d"),  
                                temp = data_temp$Temp)
        x<-aggregate(temp ~  Year + Month, temp_day1, FUN = mean, na.rm=TRUE, na.action=na.pass)
        warmest_m_temp <- x %>% group_by(Year) %>%
          summarise(max = max(temp))
        
        WWD$Year <- WWD$year
        WWD <- left_join(warmest_m_temp,WWD,by="Year") %>%
          mutate_if(is.numeric,coalesce,0)
        
        df <- data.table(Date = data_temp$Date, Tmean = data_temp$Temp, Tmin = data_tmin$Tmin)
        # df$Date <- as.Date(df$Date)
        calculate_late_frost <- function(df) {
          df <- df %>% arrange(Date) # Ensure data is sorted by date
          df$Year <- format(as.POSIXct(df$Date, format="%m/%d/%Y"), "%Y")
          df$LateFrost <- FALSE
          years <- unique(df$Year)
          results_frost <- data.table(Year = integer(), LateFrostCount = integer())
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
              results_frost <- rbind(results_frost, data.table(Year = current_year, LateFrostCount = late_frost_count))
            } else { 
              results_frost <- rbind(results_frost, data.table(Year = current_year, LateFrostCount = 0))}  #No 5 day period with temp > 10
          }
          return(list(df = df, results_frost = results_frost))
        }
        late_frost_analysis <- calculate_late_frost(df) # df <- late_frost_analysis$df
        results_frost <- late_frost_analysis$results_frost
        
        TG_of_coldest_quarter	<- bio11(structure(data_temp$Temp, .Names = data_temp$Date))
        rx1day <- rx1day(structure(data_pad$pad, .Names = data_pad$Date)) 
        rx5d <- rx5d(structure(data_pad$pad, .Names = data_pad$Date)) 
        r95tot <- r95tot(structure(data_pad$pad, .Names = data_pad$Date)) 
        
        temp_day <- data.table(Year = format(as.POSIXct(data_temp$Date, format="%m/%d/%Y"), "%Y"),
                               Month = format(as.POSIXct(data_temp$Date, format="%m/%d/%Y"), "%m"), 
                               Day = format(as.POSIXct(data_temp$Date, format="%m/%d/%Y"), "%d"),
                               Tmin = data_tmin$Tmin, Tmax = data_tmax$Tmax)
        # climdata <- hourly_temps(temp_day, latitude = lat[i])
        # chill_portions0 <- chill_portions(climdata, Start = 214)
        # chill_portions <- aggregate(Chill ~  Year, chill_portions0, FUN = sum, na.rm=TRUE, na.action=na.pass)[,2]
        BEDD0 <- head(GDD_linear(temp_day, Tb = 10, Tu = 30),-1)
        BEDD <- aggregate(GDD ~  Year, BEDD0, FUN = sum, na.rm=TRUE, na.action=na.pass)[,2]
        
        pod_celi <- data.table(datum = data_temp$Date, 
                               leto = format(as.POSIXct(data_temp$Date, format="%m/%d/%Y"),format="%Y"), 
                               mesec = format(as.POSIXct(data_temp$Date, format="%m/%d/%Y"),format="%m"), 
                               tg = data_tmax$Tmax)
        st_dni0 <- pod_celi %>% filter(tg >= 30)
        st_dni30 <- st_dni0 %>% group_by(leto) %>% summarise(tg = sum(tg-30))
        warmest_m_temp$leto <- warmest_m_temp$Year
        Dnevi_30max <- left_join(warmest_m_temp,st_dni30,by="leto") %>%
          mutate_if(is.numeric,coalesce,0)
        Dnevi_30max <- Dnevi_30max[,3:4] #Plant heat stress = accumulated daily maximum temperature values above 30?C 
        
        pod_celi$mesec <- as.integer(pod_celi$mesec)
        st_dni32_anthesis <- pod_celi %>% filter(tg >= 32, mesec >= 5, mesec <= 6) 
        Dnevi_32max_anthesis <- st_dni32_anthesis %>% group_by(leto) %>% count()
        Dnevi_32max_anthesis <- left_join(warmest_m_temp[,3],Dnevi_32max_anthesis,by="leto") %>%
          mutate_if(is.numeric,coalesce,0) # Days with Tmax above 32 ?C
        
        # flowering_heat_sum <- 703.42
        # maturity_heat_sum <- 1616.8
        # 
        # data_temp <- data_temp %>% filter(meseci >= 4, meseci <= 10)
        # data_tmax <- data_tmax %>% filter(meseci >= 4, meseci <= 10)
        # data_tmin <- data_tmin %>% filter(meseci >= 4, meseci <= 10)
        # df_heat_stress <- data.table(Date = data_temp$Date, Tmax = data_tmax$Tmax, temp = data_temp$Temp)
        # calculate_heat_sum <- function(tmax_data) {
        #   gdd_daily <- pmax(0, tmax_data$temp - 10)
        #   return(cumsum(gdd_daily))
        # }
        # is_heat_stress <- function(tmax_window) {# Function to check for heat stress (2-day period above 35°C)
        #   all(tmax_window > 35)
        # }
        # df_heat_stress$Year <- format(as.POSIXct(df_heat_stress$Date, format="%m/%d/%Y"), "%Y")
        # results <- data.table(Year = unique(df_heat_stress$Year), HeatStressDaysFL = NA, HeatStressDaysMT = NA)
        # for (current_year in unique(df_heat_stress$Year)) {
        #   year_data <- subset(df_heat_stress, Year == current_year)
        #   year_data <- subset(year_data, temp >=10)
        #   year_data$HeatSum <- calculate_heat_sum(year_data)
        #   start_dateFL <- min(year_data$Date)
        #   end_dateFL <- year_data$Date[which.min(abs(year_data$HeatSum - flowering_heat_sum))]
        #   
        #   end_date <- year_data$Date[which.min(abs(year_data$HeatSum - maturity_heat_sum))]
        #   start_date <- end_dateFL
        #   # print(c(start_date,end_date,start_dateFL,end_dateFL))
        #   if (!is.na(start_date) && !is.na(end_date)) {  # Only proceed if both dates are found
        #     flowering_data <- subset(year_data, Date >= start_dateFL & Date <= end_dateFL)
        #     maturity_data <- subset(year_data, Date >= start_date & Date <= end_date)
        #     if (nrow(flowering_data) >= 2){ # Check if there are at least 2 days of flowering data
        #       heat_stress_eventsFL <- zoo::rollapply(flowering_data$Tmax, width = 2, FUN = is_heat_stress, fill = NA, align = "left")
        #       heat_stress_daysFL <- sum(heat_stress_eventsFL, na.rm = TRUE) * 2
        #     }
        #     if (nrow(maturity_data) >= 2){ # Check if there are at least 2 days of flowering data
        #       heat_stress_events <- zoo::rollapply(maturity_data$Tmax, width = 2, FUN = is_heat_stress, fill = NA, align = "left")
        #       heat_stress_days <- sum(heat_stress_events, na.rm = TRUE) * 2
        #     }
        #     else {
        #       heat_stress_days <- 0
        #       heat_stress_daysFL <- 0# No heat stress days if flowering period is less than 2 days
        #     }
        #   } else {
        #     heat_stress_days <- 0
        #     heat_stress_daysFL <- 0# Or NA, if you prefer to indicate that flowering period couldn't be determined
        #     cat("Flowering period not found for year", current_year, "\n")
        #   }
        #   results$HeatStressDaysFL[results$Year == current_year] <- heat_stress_daysFL
        #   results$HeatStressDaysMT[results$Year == current_year] <- heat_stress_days
        # }
        podatki_tocka <- data.table(lats = rep(lat[j], length(growing_degree_days)), 
                                    lons = rep(lon[i], length(growing_degree_days)), 
                                    leto = leta1,
                                    model = rep(model[z], length(growing_degree_days)),
                                    growing_degree_days, 
                                    GSL = Growing_season_length, 
                                    # End_growing_season,
                                    BEDD, 
                                    Sums_Tmax32, 
                                    T_warmest_m = warmest_m_temp$max, 
                                    TG_of_warmest_quarter, 
                                    TG_of_coldest_quarter,
                                    Diurnal_temp_rangeN, 
                                    # chill_portions,
                                    Dnevi_32max_anthesis = Dnevi_32max_anthesis$n,
                                    # Heat_stress_fl = results$HeatStressDaysFL,
                                    # Heat_stress_mat = results$HeatStressDaysMT,
                                    cons_summer_days, 
                                    WWD = WWD$warm_wet_days, 
                                    WSDI = result_df$consecutive_days,
                                    CDD = Max_consecutive_dry_days,#DD = Dry_days, 
                                    CWD = longest_wet_period, 
                                    r20mm, 
                                    Heavy_prec_days,# r10mm, 
                                    SDII, 
                                    Prec_wettest_month, #Prec_warmest_quarter, Prec_coldest_quarter,wet_days, 
                                    FD = frost_days, 
                                    late_frost_days = results_frost$LateFrostCount,
                                    CSDI, 
                                    CFD = cons_frost_days, 
                                    ice_days, 
                                    Sums_Tmin10, Sums_Tmin15,
                                    Effective_prec,
                                    Growing_season_prec,#Very_wet_days,
                                    Nongrowing_season_prec,
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
  saveRDS(vsi_podatki, file = paste0("psenica/period_mean_",rcp,"_",obd,"_",model[z],"_",drugo[z],".rds"))
  return(vsi_podatki)
}
   
# posamezen_model <- en_model0(rcp = "rcp45", leto_zac = "2071", obd = "2071-2100", z = 1)
# posam_model2 <- en_model0(rcp = "rcp45", leto_zac = "2071", obd = "2071-2100", z = 2)
# posam_model3 <- en_model0(rcp = "rcp45", leto_zac = "2071", obd = "2071-2100", z = 3)
# posam_model4 <- en_model0(rcp = "rcp45", leto_zac = "2071", obd = "2071-2100", z = 4)
# posam_model5 <- en_model0(rcp = "rcp45", leto_zac = "2071", obd = "2071-2100", z = 5)
# posam_model6 <- en_model0(rcp = "rcp45", leto_zac = "2071", obd = "2071-2100", z = 6)

# posamezen_model <- en_model0(rcp = "rcp85", leto_zac = "2071", obd = "2071-2100", z = 1)
# posam_model2 <- en_model0(rcp = "rcp85", leto_zac = "2071", obd = "2071-2100", z = 2)
# posam_model3 <- en_model0(rcp = "rcp85", leto_zac = "2071", obd = "2071-2100", z = 3)
# posam_model4 <- en_model0(rcp = "rcp85", leto_zac = "2071", obd = "2071-2100", z = 4) #ne se
# posam_model5 <- en_model0(rcp = "rcp85", leto_zac = "2071", obd = "2071-2100", z = 5)
# posam_model6 <- en_model0(rcp = "rcp85", leto_zac = "2071", obd = "2071-2100", z = 6)


posamezen_model0 <- readRDS(paste0("psenica/period_mean_rcp45_2071-2100_", model[1],"_",drugo[1],".rds"))# psenica/ pove da gledam celoletne kazalnike
posamezen_model1 <- readRDS(paste0("psenica/period_mean_rcp85_2071-2100_", model[1],"_",drugo[1],".rds"))
posam_model02 <- readRDS(paste0("psenica/period_mean_rcp45_2071-2100_", model[2],"_",drugo[2],".rds"))
posam_model12 <- readRDS(paste0("psenica/period_mean_rcp85_2071-2100_", model[2],"_",drugo[2],".rds"))
posam_model03 <- readRDS(paste0("psenica/period_mean_rcp45_2071-2100_", model[3],"_",drugo[3],".rds"))
posam_model13 <- readRDS(paste0("psenica/period_mean_rcp85_2071-2100_", model[3],"_",drugo[3],".rds"))
posam_model04 <- readRDS(paste0("psenica/period_mean_rcp45_2071-2100_", model[4],"_",drugo[4],".rds"))
posam_model14 <- readRDS(paste0("psenica/period_mean_rcp85_2071-2100_", model[4],"_",drugo[4],".rds"))
posam_model05 <- readRDS(paste0("psenica/period_mean_rcp45_2071-2100_", model[5],"_",drugo[5],".rds"))
posam_model15 <- readRDS(paste0("psenica/period_mean_rcp85_2071-2100_", model[5],"_",drugo[5],".rds"))
posam_model06 <- readRDS(paste0("psenica/period_mean_rcp45_2071-2100_", model[6],"_",drugo[6],".rds"))
posam_model16 <- readRDS(paste0("psenica/period_mean_rcp85_2071-2100_", model[6],"_",drugo[6],".rds"))

povp_obd <- data.table() 
yrs <- posamezen_model0$leto
# kaz <- names(posamezen_model0)[5:35]
zac = 2071
kon = 2100
rcp="rcp45"
l = list(posamezen_model0, posam_model02, posam_model03,
         posam_model04, posam_model05, posam_model06)
vsi_podatki_70_10_rcp45 <- rbindlist(l)
vsi_podatki_70_10_rcp45 <- vsi_podatki_70_10_rcp45[,c(1:24,26:38)]
vsi_podatki_70_10_rcp45 <- vsi_podatki_70_10_rcp45 %>% filter(leto <= 2100, leto >= 2071)
saveRDS(vsi_podatki_70_10_rcp45, file = "psenica/kazalniki_71-00_rcp45.rds")


rcp="rcp85"
l = list(posamezen_model1, posam_model12, posam_model13,
         posam_model14, posam_model15, posam_model16)
vsi_podatki_70_10_rcp85 <- rbindlist(l)
vsi_podatki_70_10_rcp85 <- vsi_podatki_70_10_rcp85[,c(1:24,26:38)]
vsi_podatki_70_10_rcp85 <- vsi_podatki_70_10_rcp85 %>% filter(leto <= 2100, leto >= 2071)
saveRDS(vsi_podatki_70_10_rcp85, file = "psenica/kazalniki_71-00_rcp85.rds")

# vsi_podatki <- vsi_podatki_70_10_rcp45
# data <- data.table(vsi_podatki$lons,vsi_podatki$lats,vsi_podatki$leto,vsi_podatki$model,vsi_podatki[[kaz[1]]])
# colnames(data) <- c("lons","lats","leto","model","agroclim_ind")
# data=na.omit(data)
# povp01 <- data %>% group_by(lats,lons) %>% # POVPRECJE MODELOV ZA CELOTNO OBDOBJE
#   summarise(period_mean = mean(agroclim_ind))
# 
# # for(z in 1:6){
#   for(k in seq(2,length(kaz),by=1)){
#     print(c(z,k))
#     data <- data.table(vsi_podatki$lons,vsi_podatki$lats,vsi_podatki$leto,vsi_podatki$model,vsi_podatki[[kaz[k]]])
#     colnames(data) <- c("lons","lats","leto","model","agroclim_ind")
#     data=na.omit(data)
#     min = min(na.omit(vsi_podatki[[kaz[k]]]))
#     max = max(na.omit(vsi_podatki[[kaz[k]]]))
#     
#     # povp <- data %>% group_by(lats,lons,model) %>% # POVPRECJE OBDOBJA ZA POSAMEZEN MODEL
#     #   summarise(period_mean = mean(agroclim_ind))
#     # povp$kaz <- rep(kaz[k],length(povp$period_mean))
#     # povp$model <- rep(model[z],length(povp$period_mean))
#     # 
#     # plot_1 <- ggplot() +
#     #   geom_tile(data=povp,aes(x=lons,y=lats,fill=period_mean)) +
#     #   geom_sf(data=slovenia_nuts3_mapdata, color=alpha("black",0.4),fill = NA) +
#     #   scale_fill_distiller(palette = "RdBu", limits = c(min,max)) +
#     #   labs(x="Geografska dolzina",y="Geografska sirina",title=paste0(kaz[k],", obdobje ",zac,"-",kon)) +
#     #   coord_sf(crs = st_crs(4326)) +
#     #   theme_light(base_size = 17)
#     # print(plot_1)
#     # ggsave(paste0("karte_psenica/",rcp,"/period_mean_",kaz[k],"_",model[z],"_",zac,"-",kon,".png"),width = 10, height = 7)
#     # povp_obd<-rbind(povp_obd,povp)
#     
#     povp1 <- data %>% group_by(lats,lons) %>% # POVPRECJE MODELOV ZA CELOTNO OBDOBJE
#       summarise(period_mean = mean(agroclim_ind))
#     povp1$kaz <- rep(kaz[k],length(povp$period_mean))
# 
#     plot_1 <- ggplot() +
#       geom_tile(data=povp1,aes(x=lons,y=lats,fill=period_mean)) +
#       geom_sf(data=slovenia_nuts3_mapdata, color=alpha("black",0.4),fill = NA) +
#       scale_fill_distiller(palette = "RdBu", limits = c(min,max)) +
#       labs(x="Geografska dolzina",y="Geografska sirina",title=paste0(kaz[k],", obdobje ",zac,"-",kon)) +
#       coord_sf(crs = st_crs(4326)) +
#       theme_light(base_size = 17)
#     print(plot_1)
#     ggsave(paste0("karte_psenica/",rcp,"/period_mean_",kaz[k],"_",zac,"-",kon,".png"),width = 10, height = 7)
#   
#   povp01<-cbind(povp01,povp1[3])
#   }
# # }
# 
# saveRDS(povp_obd, file = paste0("period_mean_",rcp,"_",obd,".rds"))
# # return(povp_obd)


################# 2041-2070 #######################

leto_zac <- "2041"
leto_kon <- c("20701231","20701231","20701231","20701231","20701231","20701231")
leto_kon_rr <- c("20701231","20701231","20701231","20701231","20701231","20701231")
leto_zac1 <- "2040"


# posamezen_model0 <- en_model0(rcp = "rcp45", leto_zac = "2041", obd = "2041-2070", z = 1)
# posam_model02 <- en_model0(rcp = "rcp45", leto_zac = "2041", obd = "2041-2070", z = 2)
# posam_model03 <- en_model0(rcp = "rcp45", leto_zac = "2041", obd = "2041-2070", z = 3)
# posam_model04 <- en_model0(rcp = "rcp45", leto_zac = "2041", obd = "2041-2070", z = 4)
# posam_model05 <- en_model0(rcp = "rcp45", leto_zac = "2041", obd = "2041-2070", z = 5)
# posam_model06 <- en_model0(rcp = "rcp45", leto_zac = "2041", obd = "2041-2070", z = 6)
# 
# 
# posamezen_model <- en_model0(rcp = "rcp85", leto_zac = "2041", obd = "2041-2070", z = 1)
# posam_model2 <- en_model0(rcp = "rcp85", leto_zac = "2041", obd = "2041-2070", z = 2)
# posam_model3 <- en_model0(rcp = "rcp85", leto_zac = "2041", obd = "2041-2070", z = 3)
# posam_model4 <- en_model0(rcp = "rcp85", leto_zac = "2041", obd = "2041-2070", z = 4)
# posam_model5 <- en_model0(rcp = "rcp85", leto_zac = "2041", obd = "2041-2070", z = 5)
# posam_model6 <- en_model0(rcp = "rcp85", leto_zac = "2041", obd = "2041-2070", z = 6)

posamezen_model1 <- readRDS(paste0("psenica/period_mean_rcp85_2041-2070_", model[1],"_",drugo[1],".rds"))# psenica/ pove da gledam celoletne kazalnike
posam_model12 <- readRDS(paste0("psenica/period_mean_rcp85_2041-2070_", model[2],"_",drugo[2],".rds"))
posam_model13 <- readRDS(paste0("psenica/period_mean_rcp85_2041-2070_", model[3],"_",drugo[3],".rds"))
posam_model14 <- readRDS(paste0("psenica/period_mean_rcp85_2041-2070_", model[4],"_",drugo[4],".rds"))
posam_model15 <- readRDS(paste0("psenica/period_mean_rcp85_2041-2070_", model[5],"_",drugo[5],".rds"))
posam_model16 <- readRDS(paste0("psenica/period_mean_rcp85_2041-2070_", model[6],"_",drugo[6],".rds"))

posamezen_model0 <- readRDS(paste0("psenica/period_mean_rcp45_2041-2070_", model[1],"_",drugo[1],".rds"))
posam_model02 <- readRDS(paste0("psenica/period_mean_rcp45_2041-2070_", model[2],"_",drugo[2],".rds"))
posam_model03 <- readRDS(paste0("psenica/period_mean_rcp45_2041-2070_", model[3],"_",drugo[3],".rds"))
posam_model04 <- readRDS(paste0("psenica/period_mean_rcp45_2041-2070_", model[4],"_",drugo[4],".rds"))
posam_model05 <- readRDS(paste0("psenica/period_mean_rcp45_2041-2070_", model[5],"_",drugo[5],".rds"))
posam_model06 <- readRDS(paste0("psenica/period_mean_rcp45_2041-2070_", model[6],"_",drugo[6],".rds"))

# posamezen_model0 <- en_model0(rcp = "rcp45", leto_zac = "2041", obd = "2041-2070", z = 1)
# posamezen_model1 <- en_model0(rcp = "rcp85", leto_zac = "2041", obd = "2041-2070", z = 1)
# posam_model02 <- en_model0(rcp = "rcp45", leto_zac = "2041", obd = "2041-2070", z = 2)
# posam_model12 <- en_model0(rcp = "rcp85", leto_zac = "2041", obd = "2041-2070", z = 2)
# posam_model03 <- en_model0(rcp = "rcp45", leto_zac = "2041", obd = "2041-2070", z = 3)
# posam_model13 <- en_model0(rcp = "rcp85", leto_zac = "2041", obd = "2041-2070", z = 3)
# posam_model04 <- en_model0(rcp = "rcp45", leto_zac = "2041", obd = "2041-2070", z = 4)
# posam_model14 <- en_model0(rcp = "rcp85", leto_zac = "2041", obd = "2041-2070", z = 4)
# posam_model05 <- en_model0(rcp = "rcp45", leto_zac = "2041", obd = "2041-2070", z = 5)
# posam_model15 <- en_model0(rcp = "rcp85", leto_zac = "2041", obd = "2041-2070", z = 5)
# posam_model06 <- en_model0(rcp = "rcp45", leto_zac = "2041", obd = "2041-2070", z = 6)
# posam_model16 <- en_model0(rcp = "rcp85", leto_zac = "2041", obd = "2041-2070", z = 6)

povp_obd <- data.table()
yrs <- posamezen_model0$leto
zac = 2041
kon = 2070
rcp="rcp45"
l = list(posamezen_model0, posam_model02, posam_model03,
         posam_model04, posam_model05, posam_model06)
vsi_podatki_41_70_rcp45 <- rbindlist(l)
vsi_podatki_41_70_rcp45 <- vsi_podatki_41_70_rcp45[,c(1:24,26:38)]
saveRDS(vsi_podatki_41_70_rcp45, file = "psenica/kazalniki_41-70_rcp45.rds")
kaz <- names(vsi_podatki_41_70_rcp45)[5:37]

rcp="rcp85"
l = list(posamezen_model1, posam_model12, posam_model13,
         posam_model14, posam_model15, posam_model16)
vsi_podatki_41_70_rcp85 <- rbindlist(l)
vsi_podatki_41_70_rcp85 <- vsi_podatki_41_70_rcp85[,c(1:24,26:38)]
saveRDS(vsi_podatki_41_70_rcp85, file = "psenica/kazalniki_41-70_rcp85.rds")


################# 2011-2040 #######################

leto_zac <- "2011"
leto_kon <- c("20401231","20401231","20401231","20401231","20401231","20401231")
leto_kon_rr <- c("20401231","20401231","20401231","20401231","20401231","20401231")
leto_zac1 <- "2010"


# posamezen_model0 <- en_model0(rcp = "rcp45", leto_zac = "2011", obd = "2011-2040", z = 1)
# posam_model02 <- en_model0(rcp = "rcp45", leto_zac = "2011", obd = "2011-2040", z = 2)
# posam_model03 <- en_model0(rcp = "rcp45", leto_zac = "2011", obd = "2011-2040", z = 3)
# posam_model04 <- en_model0(rcp = "rcp45", leto_zac = "2011", obd = "2011-2040", z = 4)
# posam_model05 <- en_model0(rcp = "rcp45", leto_zac = "2011", obd = "2011-2040", z = 5)
# posam_model06 <- en_model0(rcp = "rcp45", leto_zac = "2011", obd = "2011-2040", z = 6)
# 
# 
# posamezen_model1 <- en_model0(rcp = "rcp85", leto_zac = "2011", obd = "2011-2040", z = 1)
# posam_model12 <- en_model0(rcp = "rcp85", leto_zac = "2011", obd = "2011-2040", z = 2)
# posam_model13 <- en_model0(rcp = "rcp85", leto_zac = "2011", obd = "2011-2040", z = 3)
# posam_model14 <- en_model0(rcp = "rcp85", leto_zac = "2011", obd = "2011-2040", z = 4)
# posam_model15 <- en_model0(rcp = "rcp85", leto_zac = "2011", obd = "2011-2040", z = 5)
# posam_model16 <- en_model0(rcp = "rcp85", leto_zac = "2011", obd = "2011-2040", z = 6)

posamezen_model1 <- readRDS(paste0("psenica/period_mean_rcp85_2011-2040_", model[1],"_",drugo[1],".rds")) # psenica/ pove da gledam celoletne kazalnike
posam_model12 <- readRDS(paste0("psenica/period_mean_rcp85_2011-2040_", model[2],"_",drugo[2],".rds"))
posam_model13 <- readRDS(paste0("psenica/period_mean_rcp85_2011-2040_", model[3],"_",drugo[3],".rds"))
posam_model14 <- readRDS(paste0("psenica/period_mean_rcp85_2011-2040_", model[4],"_",drugo[4],".rds"))
posam_model15 <- readRDS(paste0("psenica/period_mean_rcp85_2011-2040_", model[5],"_",drugo[5],".rds"))
posam_model16 <- readRDS(paste0("psenica/period_mean_rcp85_2011-2040_", model[6],"_",drugo[6],".rds"))

posamezen_model0 <- readRDS(paste0("psenica/period_mean_rcp45_2011-2040_", model[1],"_",drugo[1],".rds"))
posam_model02 <- readRDS(paste0("psenica/period_mean_rcp45_2011-2040_", model[2],"_",drugo[2],".rds"))
posam_model03 <- readRDS(paste0("psenica/period_mean_rcp45_2011-2040_", model[3],"_",drugo[3],".rds"))
posam_model04 <- readRDS(paste0("psenica/period_mean_rcp45_2011-2040_", model[4],"_",drugo[4],".rds"))
posam_model05 <- readRDS(paste0("psenica/period_mean_rcp45_2011-2040_", model[5],"_",drugo[5],".rds"))
posam_model06 <- readRDS(paste0("psenica/period_mean_rcp45_2011-2040_", model[6],"_",drugo[6],".rds"))

povp_obd <- data.table()
yrs <- posamezen_model0$leto
zac = 2011
kon = 2040
rcp="rcp45"
l = list(posamezen_model0, posam_model02, posam_model03,
         posam_model04, posam_model05, posam_model06)
vsi_podatki_11_40_rcp45 <- rbindlist(l)
vsi_podatki_11_40_rcp45 <- vsi_podatki_11_40_rcp45[,c(1:24,26:38)]
vsi_podatki_11_40_rcp45 <- vsi_podatki_11_40_rcp45 %>% filter(leto <= 2040, leto >= 2011)
saveRDS(vsi_podatki_11_40_rcp45, file = "psenica/kazalniki_11-40_rcp45.rds")

rcp="rcp85"
l = list(posamezen_model1, posam_model12, posam_model13,
         posam_model14, posam_model15, posam_model16)
vsi_podatki_11_40_rcp85 <- rbindlist(l)
vsi_podatki_11_40_rcp85 <- vsi_podatki_11_40_rcp85[,c(1:24,26:38)]
vsi_podatki_11_40_rcp85 <- vsi_podatki_11_40_rcp85 %>% filter(leto <= 2040, leto >= 2011)
saveRDS(vsi_podatki_11_40_rcp85, file = "psenica/kazalniki_11-40_rcp85.rds")



####### MEDIANA, MIN, MAX podatkov

library(sf)

grafi_SLO <- function(rattler.vsi, rcp, obd, kaz1){
  rattler.vsi <- data.table(rattler.vsi$lons,rattler.vsi$lats,rattler.vsi$leto,rattler.vsi$model,rattler.vsi[[kaz[k]]])
  colnames(rattler.vsi) <- c("lons","lats","leto","model","agroclim_ind")
  rattler.vsi=na.omit(rattler.vsi)
  min = min(na.omit(rattler.vsi$agroclim_ind))
  max = max(na.omit(rattler.vsi$agroclim_ind))
  
  povp <- aggregate(agroclim_ind ~  lats + lons + model + leto, rattler.vsi, FUN = mean , na.rm=TRUE, na.action=na.pass)
  # povp$kaz <- rep(kaz[k],length(povp$period_mean))
  # povp$model <- rep(model[z],length(povp$period_mean))
  
  rattler.mediana <- aggregate(agroclim_ind ~  lats + lons + leto, rattler.vsi, FUN = median , na.rm=TRUE, na.action=na.pass)
  rattler.min <- aggregate(agroclim_ind ~  lats + lons + leto, rattler.vsi, FUN = "min" , na.rm=TRUE, na.action=na.pass)
  rattler.max <- aggregate(agroclim_ind ~  lats + lons + leto, rattler.vsi, FUN = "max" , na.rm=TRUE, na.action=na.pass)

  sf_object <- st_as_sf(rattler.mediana, coords = c("lons", "lats"), crs = 4326)
  # # print(sf_object)
  # sf_object$agroclim_ind <- rattler.mediana$agroclim_ind
  # st_write(sf_object, paste0("mediana_",rcp,"_",obd,".shp"))

  sf_object1 <- st_as_sf(rattler.min, coords = c("lons", "lats"), crs = 4326)
  # print(sf_object1)
  # sf_object1$agroclim_ind <- rattler.min$agroclim_ind
  # st_write(sf_object1, paste0("min_",rcp,"_",obd,".shp"))

  sf_object2 <- st_as_sf(rattler.max, coords = c("lons", "lats"), crs = 4326)
  # # print(sf_object2)
  # sf_object2$agroclim_ind <- rattler.max$agroclim_ind
  # st_write(sf_object2, paste0("max_",rcp,"_",obd,".shp"))
  # 
  # ggplot(data=rattler.mediana) +
  #   geom_raster(aes(x = lons, y = lats, fill = agroclim_ind)) +
  #   scale_fill_distiller("dnevi", palette = "RdBu", limits=c(min,max)) +
  #   geom_sf(data = slovenia_nuts3_mapdata, color=alpha("black",0.4),fill = NA) +
  #   labs(x="G. dolzina",y="G. sirina",title=paste0(kaz1,", obdobje ",obd,", Mediana, ",rcp)) +
  #   coord_sf(crs = st_crs(4326)) + xlim(c(start_lon,end_lon)) + ylim(c(start_lat,end_lat)) +
  #   theme_minimal() + theme(legend.position = c(.9, .3), legend.title = element_text(face = "bold", size=16),
  #                         text = element_text(size=18),
  #                         plot.title = element_text(size=18),
  #                         axis.title.x=element_blank(),
  #                         axis.title.y=element_blank())
  #   ggsave(paste0("karte_psenica/",rcp,"_mediana/",kaz1,"_",obd,"_median.png"),width = 10, height = 7)
  # 
  # ggplot(data=rattler.min) +
  #   geom_raster(aes(x = lons, y = lats, fill = agroclim_ind)) +
  #   scale_fill_distiller("dnevi", palette = "RdBu", limits=c(min,max)) +
  #   geom_sf(data=slovenia_nuts3_mapdata, color=alpha("black",0.4),fill = NA) +
  #   labs(x="G. dolzina",y="G. sirina",title=paste0(kaz1,", obdobje ",obd,", Min, ",rcp)) +
  #   coord_sf(crs = st_crs(4326)) + xlim(c(start_lon,end_lon)) + ylim(c(start_lat,end_lat)) +
  #   theme_minimal() + theme(legend.position = c(.9, .3), legend.title = element_text(face = "bold", size=16),
  #                         text = element_text(size=18),
  #                         plot.title = element_text(size=18),
  #                         axis.title.x=element_blank(),
  #                         axis.title.y=element_blank())
  # ggsave(paste0("karte_psenica/",rcp,"_mediana/",kaz1,"_",obd,"_min.png"),width = 10, height = 7)
  # 
  # ggplot(data=rattler.max) +
  #   geom_raster(aes(x = lons, y = lats, fill = agroclim_ind)) +
  #   scale_fill_distiller("dnevi", palette = "RdBu", limits=c(min,max)) +
  #   geom_sf(data = slovenia_nuts3_mapdata, color=alpha("black",0.4),fill = NA) +
  #   labs(x="G. dolzina",y="G. sirina",title=paste0(kaz1,", obdobje ",obd,", Max, ",rcp)) +
  #   coord_sf(crs = st_crs(4326)) + xlim(c(start_lon,end_lon)) + ylim(c(start_lat,end_lat)) +
  #   theme_minimal() + theme(legend.position = c(.9, .3), legend.title = element_text(face = "bold", size=16),
  #                         text = element_text(size=18),
  #                         plot.title = element_text(size=18),
  #                         axis.title.x=element_blank(),
  #                         axis.title.y=element_blank())
  # ggsave(paste0("karte_psenica/",rcp,"_mediana/",kaz1,"_",obd,"_max.png"),width = 10, height = 7)
  xdata <- data.frame(lats = rattler.mediana$lats, lons = rattler.mediana$lons, 
                      leto = rattler.mediana$leto, kaz1 = rattler.mediana$agroclim_ind, 
                      min = rattler.min$agroclim_ind, max = rattler.max$agroclim_ind)
  colnames(xdata)[4] <- kaz1
  colnames(xdata)[5] <- kaz1
  colnames(xdata)[6] <- kaz1
  return(xdata)
}

# k=1
# med1 <- grafi_SLO(rattler.vsi = na.omit(vsi_podatki_41_70_rcp45), rcp = "rcp45", obd = "2041-2070", kaz1 = kaz[1])[1:4]
# med2 <- grafi_SLO(rattler.vsi = na.omit(vsi_podatki_41_70_rcp85), rcp = "rcp85", obd = "2041-2070", kaz1 = kaz[1])[1:4]
# med3 <- grafi_SLO(rattler.vsi = na.omit(vsi_podatki_70_10_rcp45), rcp = "rcp45", obd = "2071-2100", kaz1 = kaz[1])[1:4]
# med4 <- grafi_SLO(rattler.vsi = na.omit(vsi_podatki_70_10_rcp85), rcp = "rcp85", obd = "2071-2100", kaz1 = kaz[1])[1:4]
# med5 <- grafi_SLO(rattler.vsi = na.omit(vsi_podatki_11_40_rcp45), rcp = "rcp45", obd = "2011-2040", kaz1 = kaz[1])[1:4]
# med6 <- grafi_SLO(rattler.vsi = na.omit(vsi_podatki_11_40_rcp85), rcp = "rcp85", obd = "2011-2040", kaz1 = kaz[1])[1:4]
# 
# min1 <- grafi_SLO(rattler.vsi = na.omit(vsi_podatki_41_70_rcp45), rcp = "rcp45", obd = "2041-2070", kaz1 = kaz[1])[c(1:3,5)]
# min2 <- grafi_SLO(rattler.vsi = na.omit(vsi_podatki_41_70_rcp85), rcp = "rcp85", obd = "2041-2070", kaz1 = kaz[1])[c(1:3,5)]
# min3 <- grafi_SLO(rattler.vsi = na.omit(vsi_podatki_70_10_rcp45), rcp = "rcp45", obd = "2071-2100", kaz1 = kaz[1])[c(1:3,5)]
# min4 <- grafi_SLO(rattler.vsi = na.omit(vsi_podatki_70_10_rcp85), rcp = "rcp85", obd = "2071-2100", kaz1 = kaz[1])[c(1:3,5)]
# min5 <- grafi_SLO(rattler.vsi = na.omit(vsi_podatki_11_40_rcp45), rcp = "rcp45", obd = "2011-2040", kaz1 = kaz[1])[c(1:3,5)]
# min6 <- grafi_SLO(rattler.vsi = na.omit(vsi_podatki_11_40_rcp85), rcp = "rcp85", obd = "2011-2040", kaz1 = kaz[1])[c(1:3,5)]
# 
# max1 <- grafi_SLO(rattler.vsi = na.omit(vsi_podatki_41_70_rcp45), rcp = "rcp45", obd = "2041-2070", kaz1 = kaz[1])[c(1:3,6)]
# max2 <- grafi_SLO(rattler.vsi = na.omit(vsi_podatki_41_70_rcp85), rcp = "rcp85", obd = "2041-2070", kaz1 = kaz[1])[c(1:3,6)]
# max3 <- grafi_SLO(rattler.vsi = na.omit(vsi_podatki_70_10_rcp45), rcp = "rcp45", obd = "2071-2100", kaz1 = kaz[1])[c(1:3,6)]
# max4 <- grafi_SLO(rattler.vsi = na.omit(vsi_podatki_70_10_rcp85), rcp = "rcp85", obd = "2071-2100", kaz1 = kaz[1])[c(1:3,6)]
# max5 <- grafi_SLO(rattler.vsi = na.omit(vsi_podatki_11_40_rcp45), rcp = "rcp45", obd = "2011-2040", kaz1 = kaz[1])[c(1:3,6)]
# max6 <- grafi_SLO(rattler.vsi = na.omit(vsi_podatki_11_40_rcp85), rcp = "rcp85", obd = "2011-2040", kaz1 = kaz[1])[c(1:3,6)]
# 
# for(k in (2:33)){
#   print(c(k, kaz[k]))
#   med01 <- grafi_SLO(rattler.vsi = na.omit(vsi_podatki_41_70_rcp45), rcp = "rcp45", obd = "2041-2070", kaz1 = kaz[k])
#   med02 <- grafi_SLO(rattler.vsi = na.omit(vsi_podatki_41_70_rcp85), rcp = "rcp85", obd = "2041-2070", kaz1 = kaz[k])
#   med03 <- grafi_SLO(rattler.vsi = na.omit(vsi_podatki_70_10_rcp45), rcp = "rcp45", obd = "2071-2100", kaz1 = kaz[k])
#   med04 <- grafi_SLO(rattler.vsi = na.omit(vsi_podatki_70_10_rcp85), rcp = "rcp85", obd = "2071-2100", kaz1 = kaz[k])
#   med05 <- grafi_SLO(rattler.vsi = na.omit(vsi_podatki_11_40_rcp45), rcp = "rcp45", obd = "2011-2040", kaz1 = kaz[k])
#   med06 <- grafi_SLO(rattler.vsi = na.omit(vsi_podatki_11_40_rcp85), rcp = "rcp85", obd = "2011-2040", kaz1 = kaz[k])
# 
#   med1 <- cbind(med1,med01[4])
#   med2 <- cbind(med2,med02[4])
#   med3 <- cbind(med3,med03[4])
#   med4 <- cbind(med4,med04[4])
#   med5 <- cbind(med5,med05[4])
#   med6 <- cbind(med6,med06[4])
# 
#   min1 <- cbind(min1,med01[5])
#   min2 <- cbind(min2,med02[5])
#   min3 <- cbind(min3,med03[5])
#   min4 <- cbind(min4,med04[5])
#   min5 <- cbind(min5,med05[5])
#   min6 <- cbind(min6,med06[5])
# 
#   max1 <- cbind(max1,med01[6])
#   max2 <- cbind(max2,med02[6])
#   max3 <- cbind(max3,med03[6])
#   max4 <- cbind(max4,med04[6])
#   max5 <- cbind(max5,med05[6])
#   max6 <- cbind(max6,med06[6])
# 
#   }
# 
# # med4 <- subset(med4, med4$leto != 2101)
# 
# saveRDS(med1, file = "psenica/mediana_41-70_rcp45.rds")
# saveRDS(med2, file = "psenica/mediana_41-70_rcp85.rds")
# saveRDS(med3, file = "psenica/mediana_71-00_rcp45.rds")
# saveRDS(med4, file = "psenica/mediana_71-00_rcp85.rds")
# saveRDS(med5, file = "psenica/mediana_11-40_rcp45.rds")
# saveRDS(med6, file = "psenica/mediana_11-40_rcp85.rds")
# 
# saveRDS(min1, file = "psenica/min_41-70_rcp45.rds")
# saveRDS(min2, file = "psenica/min_41-70_rcp85.rds")
# saveRDS(min3, file = "psenica/min_71-00_rcp45.rds")
# saveRDS(min4, file = "psenica/min_71-00_rcp85.rds")
# saveRDS(min5, file = "psenica/min_11-40_rcp45.rds")
# saveRDS(min6, file = "psenica/min_11-40_rcp85.rds")
# 
# saveRDS(max1, file = "psenica/max_41-70_rcp45.rds")
# saveRDS(max2, file = "psenica/max_41-70_rcp85.rds")
# saveRDS(max3, file = "psenica/max_71-00_rcp45.rds")
# saveRDS(max4, file = "psenica/max_71-00_rcp85.rds")
# saveRDS(max5, file = "psenica/max_11-40_rcp45.rds")
# saveRDS(max6, file = "psenica/max_11-40_rcp85.rds")
# #
# # # sf_object <- read_sf(paste0("max_",rcp,"_",obd,".shp"))
# # # sf_object1 <- read_sf(paste0("min_",rcp,"_",obd,".shp"))
# # # sf_object2 <- read_sf(paste0("mediana_",rcp,"_",obd,".shp"))
# 
# 

























###############################################################################################################################################

# DODAJANJE z in faktorjev za obdobja projekcij in rcp4.5 ter 8.5

# me_podatki_71_00_rcp45 = readRDS(file = "psenica/mediana_71-00_rcp45.rds")
# me_podatki_71_00_rcp85 = readRDS(file = "psenica/mediana_71-00_rcp85.rds")
# me_podatki_41_70_rcp45 = readRDS(file = "psenica/mediana_41-70_rcp45.rds")
# me_podatki_41_70_rcp85 = readRDS(file = "psenica/mediana_41-70_rcp85.rds")
# me_podatki_11_40_rcp45 = readRDS(file = "psenica/mediana_11-40_rcp45.rds")
# me_podatki_11_40_rcp85 = readRDS(file = "psenica/mediana_11-40_rcp85.rds")

vsi_podatki_71_00_rcp85 = readRDS(file = "psenica/kazalniki_71-00_rcp85.rds")
vsi_podatki_71_00_rcp45 = readRDS(file = "psenica/kazalniki_71-00_rcp45.rds")
vsi_podatki_41_70_rcp85 = readRDS(file = "psenica/kazalniki_41-70_rcp85.rds")
vsi_podatki_41_70_rcp45 = readRDS(file = "psenica/kazalniki_41-70_rcp45.rds")
vsi_podatki_11_40_rcp85 = readRDS(file = "psenica/kazalniki_11-40_rcp85.rds")
vsi_podatki_11_40_rcp45 = readRDS(file = "psenica/kazalniki_11-40_rcp45.rds")

dodaj_z_in_faktorje <- function(vsi_podatki,vsi_podatki_1){

  M_vrednosti <- as.numeric(vsi_podatki[1, 70:102])
  SD_vrednosti <- as.numeric(vsi_podatki[1, 103:135])
  
  z_vrednosti <- sweep(vsi_podatki_1[, 5:37], 2, M_vrednosti, FUN = "-")
  z_vrednosti <- sweep(z_vrednosti, 2, SD_vrednosti, FUN = "/")
  vsi_podatki_2 <- cbind(vsi_podatki_1, z_vrednosti)
  imena_z <- paste0("z", colnames(vsi_podatki_1)[5:37])
  colnames(vsi_podatki_2)[(ncol(vsi_podatki_1) + 1):ncol(vsi_podatki_2)] <- imena_z
  
  vsi_podatki_3 <- vsi_podatki_2
  
  vsi_podatki_3$zpozeba<-(vsi_podatki_3$zCFD + vsi_podatki_3$zFD + vsi_podatki_3$zice_days + 
                            vsi_podatki_3$zlate_frost_days)/4
  
  vsi_podatki_3$zvrocinski_stres<-(vsi_podatki_3$zSums_Tmax32 + vsi_podatki_3$zDnevi_32max_anthesis + 
                                     vsi_podatki_3$zvwd + vsi_podatki_3$zWSDI)/4
  
  vsi_podatki_3$zrastna<-(vsi_podatki_3$zgrowing_degree_days + vsi_podatki_3$zGSL + 
                            vsi_podatki_3$zTG_of_warmest_quarter + vsi_podatki_3$zBEDD +
                            vsi_podatki_3$zcons_summer_days + vsi_podatki_3$zsu + vsi_podatki_3$zTG_of_coldest_quarter + 
                            vsi_podatki_3$zT_warmest_m + vsi_podatki_3$zDiurnal_temp_rangeN)/9
  
  vsi_podatki_3$zvrocinski_stres_Vlaga_noc<-(vsi_podatki_3$zWWD + vsi_podatki_3$ztn90p)/2
  
  vsi_podatki_3$zmin_padavine <- vsi_podatki_3$zCDD
  
  # vsi_podatki_3$zvisoke_padavine <- (vsi_podatki_3$zPrec_wettest_month + vsi_podatki_3$zr20mm + 
  #                                    vsi_podatki_3$zrx5d + vsi_podatki_3$zHeavy_prec_days + 
  #                                    vsi_podatki_3$zCWD + vsi_podatki_3$zSDII)/6
  
  vsi_podatki_3$zvisoke_padavine <- (vsi_podatki_3$zPrec_wettest_month + vsi_podatki_3$zr20mm + 
                                       vsi_podatki_3$zrx5d + vsi_podatki_3$zHeavy_prec_days + 
                                       vsi_podatki_3$zCWD + vsi_podatki_3$zSDII +
                                       vsi_podatki_3$zEffective_prec + vsi_podatki_3$zGrowing_season_prec + 
                                       vsi_podatki_3$zNongrowing_season_prec)/9
  
  # vsi_podatki_3$zkonec_rastne<-(vsi_podatki_3$zGSL+vsi_podatki_3$zEnd_growing_season)/2
  
  vsi_podatki_3$zprezimovanje_minT<-(vsi_podatki_3$zcsdi_annual_sum +
                                       vsi_podatki_3$zSums_Tmin10 + vsi_podatki_3$zSums_Tmin15)/3
  
  
  return(vsi_podatki_3)
}
vsi_podatki <- readRDS(file = "../kazalniki_1981-2010_psenica.rds")
vsi_podatki_1 = vsi_podatki_71_00_rcp45

faktorji_71_00_rcp45 <- dodaj_z_in_faktorje(vsi_podatki,vsi_podatki_71_00_rcp45)
faktorji_71_00_rcp85 <- dodaj_z_in_faktorje(vsi_podatki,vsi_podatki_71_00_rcp85)
faktorji_41_70_rcp45 <- dodaj_z_in_faktorje(vsi_podatki,vsi_podatki_41_70_rcp45)
faktorji_41_70_rcp85 <- dodaj_z_in_faktorje(vsi_podatki,vsi_podatki_41_70_rcp85)
faktorji_11_40_rcp45 <- dodaj_z_in_faktorje(vsi_podatki,vsi_podatki_11_40_rcp45)
faktorji_11_40_rcp85 <- dodaj_z_in_faktorje(vsi_podatki,vsi_podatki_11_40_rcp85)

library(ggthemes)

lat_Jablje <-	46.1875 #46.1414	
lat_Rakican <-	46.6875 #46.6504
lon_Jablje <- 14.5625	#14.5561 
lon_Rakican <-	16.1875 #16.1966 
latsi <- c(lat_Jablje, lat_Rakican)
lonsi <- c(lon_Jablje, lon_Rakican)
ime <- c("Jablje", "Rakican")
pts <- data.frame(lat = latsi, lon = lonsi, Location = ime)

grafi_lepi <- function(rattler.vsi, rcp, obd, kaz1, n){
  

  min_n <- c(-3,-7, -4, -3,-4, -1, -2)
  max_n <- c( 3, 7,  4,  3, 4,  1,  2)
  rattler.vsi <- data.table(rattler.vsi$lons,rattler.vsi$lats,rattler.vsi$leto,rattler.vsi$model,rattler.vsi[[kaz1]])
  colnames(rattler.vsi) <- c("lons","lats","leto","model","agroclim_ind")
  rattler.vsi=na.omit(rattler.vsi)
  min = min(na.omit(rattler.vsi$agroclim_ind))
  max = max(na.omit(rattler.vsi$agroclim_ind))
  
  povp <- aggregate(agroclim_ind ~  lats + lons + model, rattler.vsi, FUN = mean , na.rm=TRUE, na.action=na.pass) # najprej naredimo povpre?je obdobja
  rattler.mediana <- aggregate(agroclim_ind ~  lats + lons, povp, FUN = median , na.rm=TRUE, na.action=na.pass) # nato naredimo me min max modelov
  rattler.min <- aggregate(agroclim_ind ~  lats + lons, povp, FUN = "min" , na.rm=TRUE, na.action=na.pass)
  rattler.max <- aggregate(agroclim_ind ~  lats + lons, povp, FUN = "max" , na.rm=TRUE, na.action=na.pass)

  pts$Location <- as.factor(pts$Location)
  
  # a1 <- ggplot(data=rattler.mediana) +
  #   geom_raster(aes(x = lons, y = lats, fill = agroclim_ind)) +
  #   scale_fill_distiller("z", palette = "RdBu", limits=c(min_n[n],max_n[n])) +
  #   geom_sf(data = slovenia_nuts3_mapdata, color=alpha("black",0.4),fill = NA) +
  #   labs(x="G. dolzina",y="G. sirina",title=paste0(kaz1,", obdobje ",obd,", Mediana, ",rcp)) +
  #   geom_point(data = pts, aes(x = lon, y = lat, shape = Location), size = 3) + # Map 'point_type' to shape
  #   scale_shape_manual(name = "Location", # Set the title for the shape legend
  #                      values = c("Jablje" = 17, "Rakican" = 16)) + # Specify shapes (24=triangle, 22=square)
  #   coord_sf(crs = st_crs(4326)) + xlim(c(start_lon,end_lon)) + ylim(c(start_lat,end_lat)) +
  #   theme_map() + theme(legend.position = c(.9, .3), legend.title = element_text(face = "bold", size=16),
  #                         text = element_text(size=18),
  #                         plot.title = element_text(size=18),
  #                         axis.title.x=element_blank(),
  #                         axis.title.y=element_blank())
  # a1
  #   ggsave(paste0("karte_psenica/",rcp,"_mediana/faktor",kaz1,"_",obd,".png"),a1,width = 10, height = 7)
  # 
  # ggplot(data=rattler.min) +
  #   geom_raster(aes(x = lons, y = lats, fill = agroclim_ind)) +
  #   scale_fill_distiller("dnevi", palette = "RdBu", limits=c(min,max)) +
  #   geom_sf(data=slovenia_nuts3_mapdata, color=alpha("black",0.4),fill = NA) +
  #   labs(x="G. dolzina",y="G. sirina",title=paste0(kaz1,", obdobje ",obd,", Min, ",rcp)) +
  #   geom_point(data = pts, aes(x = lon, y = lat, shape = Location), size = 3) + # Map 'point_type' to shape
  #   scale_shape_manual(name = "Location", # Set the title for the shape legend
  #                      values = c("Jablje" = 17, "Rakican" = 16)) + # Specify shapes (24=triangle, 22=square)
  #   coord_sf(crs = st_crs(4326)) + xlim(c(start_lon,end_lon)) + ylim(c(start_lat,end_lat)) +
  #   theme_map() + theme(legend.position = c(.9, .3), legend.title = element_text(face = "bold", size=16),
  #                       text = element_text(size=18),
  #                       plot.title = element_text(size=18),
  #                       axis.title.x=element_blank(),
  #                       axis.title.y=element_blank())
  # 
  # ggsave(paste0("karte_psenica/",rcp,"_mediana/faktor",kaz1,"_",obd,"_min.png"),width = 10, height = 7)
  # 
  # ggplot(data=rattler.max) +
  #   geom_raster(aes(x = lons, y = lats, fill = agroclim_ind)) +
  #   scale_fill_distiller("dnevi", palette = "RdBu", limits=c(min,max)) +
  #   geom_sf(data = slovenia_nuts3_mapdata, color=alpha("black",0.4),fill = NA) +
  #   labs(x="G. dolzina",y="G. sirina",title=paste0(kaz1,", obdobje ",obd,", Max, ",rcp)) +
  #   geom_point(data = pts, aes(x = lon, y = lat, shape = Location), size = 3) + # Map 'point_type' to shape
  #   scale_shape_manual(name = "Location", # Set the title for the shape legend
  #                      values = c("Jablje" = 17, "Rakican" = 16)) + # Specify shapes (24=triangle, 22=square)
  #   coord_sf(crs = st_crs(4326)) + xlim(c(start_lon,end_lon)) + ylim(c(start_lat,end_lat)) +
  #   theme_map() + theme(legend.position = c(.9, .3), legend.title = element_text(face = "bold", size=16),
  #                       text = element_text(size=18),
  #                       plot.title = element_text(size=18),
  #                       axis.title.x=element_blank(),
  #                       axis.title.y=element_blank())
  # 
  # ggsave(paste0("karte_psenica/",rcp,"_mediana/faktor",kaz1,"_",obd,"_max.png"),width = 10, height = 7)
  
  # povp_hist <- aggregate(. ~ lats + lons, vsi_podatki, FUN = mean , na.rm=TRUE, na.action=na.pass)
  # povp_hist1 <- data.table(lons = povp_hist$lons,lats = povp_hist$lats,kaz1 = povp_hist[[kaz1]])
  # povp_proj <- aggregate(. ~  lats + lons + model, rattler.vsi, FUN = mean , na.rm=TRUE, na.action=na.pass) # najprej naredimo povprecje obdobja, nato anomalijo, nato me, min, max
  # anomalija_proj<- merge(povp_hist1,povp_proj,by=c("lats","lons"))
  # anomalija_proj$ano <- anomalija_proj$agroclim_ind - anomalija_proj$kaz1 
  # 
  min_ano <- c(-3,-7, -3.5, -5,-1, -1, -2)
  max_ano <- c( 3, 7,  3.5,  5, 1,  1,  2)
  
  data1 <- data.frame(vsi_podatki$lons, vsi_podatki$lats, vsi_podatki$leto, vsi_podatki[[kaz1]])
  colnames(data1) <- c("lons","lats","leto","agroclim_ind")
  data1 <- data1 %>% filter(leto >= 1981, leto <= 2010)
  data1=na.omit(data1)
  
  povp_hist <- data1 %>% group_by(lats,lons) %>%
    summarise(period_mean = mean(agroclim_ind))
  
  ano <- merge(rattler.mediana,povp_hist,by=c("lats","lons"))
  ano$anomalije <- ano$agroclim_ind - ano$period_mean
  ano_min <- merge(rattler.min,povp_hist,by=c("lats","lons"))
  ano_min$anomalije <- ano_min$agroclim_ind - ano_min$period_mean
  ano_max <- merge(rattler.max,povp_hist,by=c("lats","lons"))
  ano_max$anomalije <- ano_max$agroclim_ind - ano_max$period_mean
  
  # ano1<-ggplot() +
  #   geom_tile(data=anomalija_proj,aes(x=lons,y=lats,fill=ano)) +
  #   geom_sf(data=slovenia_nuts3_mapdata, fill = NA) +
  #   scale_fill_distiller("z", palette = "RdBu",limits=c(min_ano[n],max_ano[n])) + # 
  #   coord_sf(crs = st_crs(4326)) +
  #   theme_map() + labs(x="G. dolzina",y="G. sirina",title=paste0("Anomalije - ",kaz1," RCP4.5, ",obd)) +
  #   geom_point(data = pts, aes(x = lon, y = lat, shape = Location), size = 3) + # Map 'point_type' to shape
  #   scale_shape_manual(name = "Location", # Set the title for the shape legend
  #                      values = c("Jablje" = 17, "Rakican" = 16)) + # Specify shapes (24=triangle, 22=square)
  #   theme(legend.position = c(.9, .3),legend.title = element_text(face = "bold", size=16),
  #         text = element_text(size=18),
  #         plot.title = element_text(size=18),
  #         axis.title.x=element_blank(),
  #         axis.title.y=element_blank())
  # ano1
  # ggsave(paste0("karte_psenica/",rcp,"_mediana/anomalije_",kaz1,"_",obd,".png"),ano1,width = 10, height = 7)
  
  ggplot() +
    geom_tile(data=ano,aes(x=lons,y=lats,fill=anomalije)) +
    geom_sf(data=slovenia_nuts3_mapdata, color=alpha("black",0.4),fill = NA) +
    scale_fill_distiller(palette = "RdBu", limits = c(min_ano[n],max_ano[n])) +
    theme_map() + labs(x="G. dolzina",y="G. sirina",title=paste0("Anomalije - ",kaz1,", mediana modelov, ",rcp,", ",obd)) +
    coord_sf(crs = st_crs(4326)) +
    geom_point(data = pts, aes(x = lon, y = lat, shape = Location), size = 3) + # Map 'point_type' to shape
    scale_shape_manual(name = "Location", # Set the title for the shape legend
                       values = c("Jablje" = 17, "Rakican" = 16)) + # Specify shapes (24=triangle, 22=square)
    theme(legend.position = c(.9, .3),legend.title = element_text(face = "bold", size=16),
          text = element_text(size=18),
          plot.title = element_text(size=18),
          axis.title.x=element_blank(),
          axis.title.y=element_blank())
  
  ggsave(paste0("karte_psenica/",rcp,"_mediana/ano_mediana_",kaz1,"_",obd,".png"),width = 10, height = 7)
  
  ggplot() +
    geom_tile(data=ano_min,aes(x=lons,y=lats,fill=anomalije)) +
    geom_sf(data=slovenia_nuts3_mapdata, color=alpha("black",0.4),fill = NA) +
    scale_fill_distiller(palette = "RdBu", limits = c(min_ano[n],max_ano[n])) +
    theme_map() + labs(x="G. dolzina",y="G. sirina",title=paste0("Anomalije: ",kaz1,", min modelov, ",rcp,", ",obd)) +
    coord_sf(crs = st_crs(4326)) +
    geom_point(data = pts, aes(x = lon, y = lat, shape = Location), size = 3) + # Map 'point_type' to shape
    scale_shape_manual(name = "Location", # Set the title for the shape legend
                       values = c("Jablje" = 17, "Rakican" = 16)) + # Specify shapes (24=triangle, 22=square)
    theme(legend.position = c(.9, .3),legend.title = element_text(face = "bold", size=16),
          text = element_text(size=18),
          plot.title = element_text(size=18),
          axis.title.x=element_blank(),
          axis.title.y=element_blank())
  ggsave(paste0("karte_psenica/",rcp,"_mediana/ano_min_",kaz1,"_",obd,".png"),width = 10, height = 7)
  
  ggplot() +
    geom_tile(data=ano_max,aes(x=lons,y=lats,fill=anomalije)) +
    geom_sf(data=slovenia_nuts3_mapdata, color=alpha("black",0.4),fill = NA) +
    scale_fill_distiller(palette = "RdBu", limits = c(min_ano[n],max_ano[n])) +
    theme_map() + labs(x="G. dolzina",y="G. sirina",title=paste0("Anomalije: ",kaz1,", max modelov, ",rcp,", ",obd)) +
    coord_sf(crs = st_crs(4326)) +
    geom_point(data = pts, aes(x = lon, y = lat, shape = Location), size = 3) + # Map 'point_type' to shape
    scale_shape_manual(name = "Location", # Set the title for the shape legend
                       values = c("Jablje" = 17, "Rakican" = 16)) + # Specify shapes (24=triangle, 22=square)
    theme(legend.position = c(.9, .3),legend.title = element_text(face = "bold", size=16),
          text = element_text(size=18),
          plot.title = element_text(size=18),
          axis.title.x=element_blank(),
          axis.title.y=element_blank())
  ggsave(paste0("karte_psenica/",rcp,"_mediana/ano_max_",kaz1,"_",obd,".png"),width = 10, height = 7)
  
  lok1 <- subset(povp_hist, lats == lat_Jablje & lons == lon_Jablje)
  lok2 <- subset(povp_hist, lats == lat_Rakican & lons == lon_Rakican)
  
  hist1<-ggplot() +
    geom_tile(data=povp_hist,aes(x=lons,y=lats,fill=period_mean)) +
    geom_sf(data=slovenia_nuts3_mapdata, fill = NA) +
    scale_fill_distiller("z", palette = "RdBu", limits = c(min_n[n],max_n[n])) + # Use gradientn for custom palette
    coord_sf(crs = st_crs(4326)) + 
    theme_map() +#labs(x="G. dolzina",y="G. sirina",title="Hist 1981-2010") + 
    geom_point(data = pts, aes(x = lon, y = lat, shape = Location), size = 3) + # Map 'point_type' to shape
    scale_shape_manual(name = "Location", # Set the title for the shape legend
                       values = c("Jablje" = 17, "Rakican" = 16)) + # Specify shapes (24=triangle, 22=square)
    theme(legend.position = c(.9, .3),legend.title = element_text(face = "bold", size=16),
          text = element_text(size=18),
          plot.title = element_text(size=18),
          axis.title.x=element_blank(),
          axis.title.y=element_blank())
  
  hist1
  ggsave(paste0("karte_psenica/faktor",kaz1,"1981-2010.png"),hist1,width = 10, height = 7)

    return(povp)
}


me_71_00_rcp45 <- grafi_lepi(faktorji_71_00_rcp45, rcp = "rcp45", obd = "2071-2100", kaz1 = "zpozeba", n=1)
me_71_00_rcp45 <- grafi_lepi(faktorji_71_00_rcp45, rcp = "rcp45", obd = "2071-2100", kaz1 = "zvrocinski_stres", n=2)
me_71_00_rcp45 <- grafi_lepi(faktorji_71_00_rcp45, rcp = "rcp45", obd = "2071-2100", kaz1 = "zrastna", n=3)
me_71_00_rcp45 <- grafi_lepi(faktorji_71_00_rcp45, rcp = "rcp45", obd = "2071-2100", kaz1 = "zvrocinski_stres_Vlaga_noc", n=4)
me_71_00_rcp45 <- grafi_lepi(faktorji_71_00_rcp45, rcp = "rcp45", obd = "2071-2100", kaz1 = "zvisoke_padavine", n=5)
me_71_00_rcp45 <- grafi_lepi(faktorji_71_00_rcp45, rcp = "rcp45", obd = "2071-2100", kaz1 = "zmin_padavine", n=6)
me_71_00_rcp45 <- grafi_lepi(faktorji_71_00_rcp45, rcp = "rcp45", obd = "2071-2100", kaz1 = "zprezimovanje_minT", n=7)

me_71_00_rcp85 <- grafi_lepi(faktorji_71_00_rcp85, rcp = "rcp85", obd = "2071-2100", kaz1 = "zpozeba", n=1)
me_71_00_rcp85 <- grafi_lepi(faktorji_71_00_rcp85, rcp = "rcp85", obd = "2071-2100", kaz1 = "zvrocinski_stres", n=2)
me_71_00_rcp85 <- grafi_lepi(faktorji_71_00_rcp85, rcp = "rcp85", obd = "2071-2100", kaz1 = "zrastna", n=3)
me_71_00_rcp85 <- grafi_lepi(faktorji_71_00_rcp85, rcp = "rcp85", obd = "2071-2100", kaz1 = "zvrocinski_stres_Vlaga_noc", n=4)
me_71_00_rcp85 <- grafi_lepi(faktorji_71_00_rcp85, rcp = "rcp85", obd = "2071-2100", kaz1 = "zvisoke_padavine", n=5)
me_71_00_rcp85 <- grafi_lepi(faktorji_71_00_rcp85, rcp = "rcp85", obd = "2071-2100", kaz1 = "zmin_padavine", n=6)
me_71_00_rcp85 <- grafi_lepi(faktorji_71_00_rcp85, rcp = "rcp85", obd = "2071-2100", kaz1 = "zprezimovanje_minT", n=7)

me_41_70_rcp45 <- grafi_lepi(faktorji_41_70_rcp45, rcp = "rcp45", obd = "2041-2070", kaz1 = "zpozeba", n=1)
me_41_70_rcp45 <- grafi_lepi(faktorji_41_70_rcp45, rcp = "rcp45", obd = "2041-2070", kaz1 = "zvrocinski_stres", n=2)
me_41_70_rcp45 <- grafi_lepi(faktorji_41_70_rcp45, rcp = "rcp45", obd = "2041-2070", kaz1 = "zrastna", n=3)
me_41_70_rcp45 <- grafi_lepi(faktorji_41_70_rcp45, rcp = "rcp45", obd = "2041-2070", kaz1 = "zvrocinski_stres_Vlaga_noc", n=4)
me_41_70_rcp45 <- grafi_lepi(faktorji_41_70_rcp45, rcp = "rcp45", obd = "2041-2070", kaz1 = "zvisoke_padavine", n=5)
me_41_70_rcp45 <- grafi_lepi(faktorji_41_70_rcp45, rcp = "rcp45", obd = "2041-2070", kaz1 = "zmin_padavine", n=6)
me_41_70_rcp45 <- grafi_lepi(faktorji_41_70_rcp45, rcp = "rcp45", obd = "2041-2070", kaz1 = "zprezimovanje_minT", n=7)

me_41_70_rcp85 <- grafi_lepi(faktorji_41_70_rcp85, rcp = "rcp85", obd = "2041-2070", kaz1 = "zpozeba", n=1)
me_41_70_rcp85 <- grafi_lepi(faktorji_41_70_rcp85, rcp = "rcp85", obd = "2041-2070", kaz1 = "zvrocinski_stres", n=2)
me_41_70_rcp85 <- grafi_lepi(faktorji_41_70_rcp85, rcp = "rcp85", obd = "2041-2070", kaz1 = "zrastna", n=3)
me_41_70_rcp85 <- grafi_lepi(faktorji_41_70_rcp85, rcp = "rcp85", obd = "2041-2070", kaz1 = "zvrocinski_stres_Vlaga_noc", n=4)
me_41_70_rcp85 <- grafi_lepi(faktorji_41_70_rcp85, rcp = "rcp85", obd = "2041-2070", kaz1 = "zvisoke_padavine", n=5)
me_41_70_rcp85 <- grafi_lepi(faktorji_41_70_rcp85, rcp = "rcp85", obd = "2041-2070", kaz1 = "zmin_padavine", n=6)
me_41_70_rcp85 <- grafi_lepi(faktorji_41_70_rcp85, rcp = "rcp85", obd = "2041-2070", kaz1 = "zprezimovanje_minT", n=7)

me_11_40_rcp45 <- grafi_lepi(faktorji_11_40_rcp45, rcp = "rcp45", obd = "2011-2040", kaz1 = "zpozeba", n=1)
me_11_40_rcp45 <- grafi_lepi(faktorji_11_40_rcp45, rcp = "rcp45", obd = "2011-2040", kaz1 = "zvrocinski_stres", n=2)
me_11_40_rcp45 <- grafi_lepi(faktorji_11_40_rcp45, rcp = "rcp45", obd = "2011-2040", kaz1 = "zrastna", n=3)
me_11_40_rcp45 <- grafi_lepi(faktorji_11_40_rcp45, rcp = "rcp45", obd = "2011-2040", kaz1 = "zvrocinski_stres_Vlaga_noc", n=4)
me_11_40_rcp45 <- grafi_lepi(faktorji_11_40_rcp45, rcp = "rcp45", obd = "2011-2040", kaz1 = "zvisoke_padavine", n=5)
me_11_40_rcp45 <- grafi_lepi(faktorji_11_40_rcp45, rcp = "rcp45", obd = "2011-2040", kaz1 = "zmin_padavine", n=6)
me_11_40_rcp45 <- grafi_lepi(faktorji_11_40_rcp45, rcp = "rcp45", obd = "2011-2040", kaz1 = "zprezimovanje_minT", n=7)

me_11_40_rcp85 <- grafi_lepi(faktorji_11_40_rcp85, rcp = "rcp85", obd = "2011-2040", kaz1 = "zpozeba", n=1)
me_11_40_rcp85 <- grafi_lepi(faktorji_11_40_rcp85, rcp = "rcp85", obd = "2011-2040", kaz1 = "zvrocinski_stres", n=2)
me_11_40_rcp85 <- grafi_lepi(faktorji_11_40_rcp85, rcp = "rcp85", obd = "2011-2040", kaz1 = "zrastna", n=3)
me_11_40_rcp85 <- grafi_lepi(faktorji_11_40_rcp85, rcp = "rcp85", obd = "2011-2040", kaz1 = "zvrocinski_stres_Vlaga_noc", n=4)
me_11_40_rcp85 <- grafi_lepi(faktorji_11_40_rcp85, rcp = "rcp85", obd = "2011-2040", kaz1 = "zvisoke_padavine", n=5)
me_11_40_rcp85 <- grafi_lepi(faktorji_11_40_rcp85, rcp = "rcp85", obd = "2011-2040", kaz1 = "zmin_padavine", n=6)
me_11_40_rcp85 <- grafi_lepi(faktorji_11_40_rcp85, rcp = "rcp85", obd = "2011-2040", kaz1 = "zprezimovanje_minT", n=7)


# 
# # povp_hist <- readRDS(file = "faktorji_hist_1981-2010.rds") # to je ostalo od koruze
# povp_hist <- aggregate(. ~ lats + lons, vsi_podatki, FUN = mean , na.rm=TRUE, na.action=na.pass)
# 
# rattler.vsi <- faktorji_41_70_rcp45 # datoteka s kazalniki, z vrednostmi in vsemi 7 faktorji
# rattler.vsi=na.omit(rattler.vsi)
# povp_proj <- aggregate(. ~  lats + lons + model, rattler.vsi, FUN = mean , na.rm=TRUE, na.action=na.pass) # najprej naredimo povprecje obdobja, nato anomalijo, nato me, min, max
# anomalija_rastna <- povp_proj
# 
# colnames(anomalija_rastna)[3] <- "proj"
# povp_hist_zrastna<-data.frame(lats=povp_hist$lats, lons = povp_hist$lons, hist=povp_hist$zrastna,
#                               BEDD_hist=povp_hist$BEDD, su_hist=povp_hist$su,
#                               GSL_hist = povp_hist$GSL, csu = povp_hist$cons_summer_days,
#                               TG_of_coldest_quarter_hist = povp_hist$TG_of_coldest_quarter,
#                               Diurnal_temp_rangeN_hist = povp_hist$Diurnal_temp_rangeN,
#                               growing_degree_day_hist=povp_hist$growing_degree_days,
#                               TG_of_warmest_quarte_hist= povp_hist$TG_of_warmest_quarter,
#                               T_warmest_m_hist=povp_hist$T_warmest_m)
# 
# anomalija_rastna_proj<-data.frame(lats=anomalija_rastna$lats, lons = anomalija_rastna$lons, 
#                                   proj=anomalija_rastna$zrastna,
#                                   BEDD_proj=anomalija_rastna$BEDD, 
#                                   su_proj=anomalija_rastna$su,
#                                   GSL_proj = anomalija_rastna$GSL,
#                                   csu_proj = anomalija_rastna$cons_summer_days,
#                                   TG_of_coldest_quarte_proj= anomalija_rastna$TG_of_coldest_quarter,
#                                   Diurnal_temp_rangeN_proj = anomalija_rastna$Diurnal_temp_rangeN,
#                                   growing_degree_day_proj=anomalija_rastna$growing_degree_days,
#                                   TG_of_warmest_quarte_proj= anomalija_rastna$TG_of_warmest_quarter,
#                                   T_warmest_m_proj=anomalija_rastna$T_warmest_m)
# anomalija_rastna_proj<- merge(povp_hist_zrastna,anomalija_rastna_proj,by=c("lats","lons"))
# anomalija_rastna_proj$zrastna <- anomalija_rastna_proj$proj - anomalija_rastna_proj$hist
# povprecje_slovenije_hist_zrastna<- mean(anomalija_rastna_proj$hist)
# povprecje_slovenije_proj_zrastna<-mean(anomalija_rastna_proj$proj)
# 
# ano1<-ggplot() +
#   geom_tile(data=anomalija_rastna_proj,aes(x=lons,y=lats,fill=zrastna)) +
#   geom_sf(data=slovenia_nuts3_mapdata, fill = NA) +
#   scale_fill_distiller("z", palette = "RdBu",limits=c(-4,4)) + # 
#   coord_sf(crs = st_crs(4326)) +
#   theme_map() + labs(x="G. dolzina",y="G. sirina",title="Anomalije - rastna doba RCP4.5 2041-2070") +
#   geom_point(data = pts, aes(x = lon, y = lat, shape = Location), size = 3) + # Map 'point_type' to shape
#   scale_shape_manual(name = "Location", # Set the title for the shape legend
#                      values = c("Jablje" = 17, "Rakican" = 16)) + # Specify shapes (24=triangle, 22=square)
#   theme(legend.position = c(.9, .3),legend.title = element_text(face = "bold", size=16),
#         text = element_text(size=18),
#         plot.title = element_text(size=18),
#         axis.title.x=element_blank(),
#         axis.title.y=element_blank())
# ano1
# ggsave(paste0("karte_psenica/rcp45_mediana/anomalije_rastna_2041-2070.png"),ano1,width = 10, height = 7)
# lok1 <- subset(anomalija_rastna_proj, lats == lat_Jablje & lons == lon_Jablje)
# lok2 <- subset(anomalija_rastna_proj, lats == lat_Rakican & lons == lon_Rakican)
# print(c(lok1$hist,lok1$zrastna))
# print(c(lok2$hist,lok2$zrastna))
# 
# hist1<-ggplot() +
#   geom_tile(data=anomalija_rastna_proj,aes(x=lons,y=lats,fill=hist)) +
#   geom_sf(data=slovenia_nuts3_mapdata, fill = NA) +
#   scale_fill_distiller("z", palette = "RdBu", limits = c(-4,4)) + # Use gradientn for custom palette
#   coord_sf(crs = st_crs(4326)) + 
#   theme_map() +#labs(x="G. dolzina",y="G. sirina",title="Hist 1981-2010") + 
#   geom_point(data = pts, aes(x = lon, y = lat, shape = Location), size = 3) + # Map 'point_type' to shape
#   scale_shape_manual(name = "Location", # Set the title for the shape legend
#                      values = c("Jablje" = 17, "Rakican" = 16)) + # Specify shapes (24=triangle, 22=square)
#   theme(legend.position = c(.9, .3),legend.title = element_text(face = "bold", size=16),
#         text = element_text(size=18),
#         plot.title = element_text(size=18),
#         axis.title.x=element_blank(),
#         axis.title.y=element_blank())
# 
# hist1
# ggsave(paste0("karte_psenica/faktorrastna1981-2010.png"),hist1,width = 10, height = 7)
# 
# proj1<-ggplot() +
#   geom_tile(data=anomalija_rastna_proj,aes(x=lons,y=lats,fill=proj)) +
#   geom_sf(data=slovenia_nuts3_mapdata, fill = NA) +
#   scale_fill_distiller("z", palette = "RdBu", limits = c(-4,4)) + # Use gradientn for custom palette
#   coord_sf(crs = st_crs(4326)) +
#   theme_map() +labs(x="G. dolzina",y="G. sirina",title="Projekcije RCP4.5 2041-2070") + 
#   geom_point(data = pts, aes(x = lon, y = lat, shape = Location), size = 3) + # Map 'point_type' to shape
#   scale_shape_manual(name = "Location", # Set the title for the shape legend
#                      values = c("Jablje" = 17, "Rakican" = 16)) + # Specify shapes (24=triangle, 22=square)  
#   theme(legend.position = c(.9, .3),legend.title = element_text(face = "bold", size=18),
#         text = element_text(size=18),
#         plot.title = element_text(size=18),
#         axis.title.x=element_blank(),
#         axis.title.y=element_blank())
# proj1
# ggsave(paste0("karte_psenica/rcp45_mediana/faktorzrastna_2041-2070.png"),proj1,width = 10, height = 7)
# 
# # RCP4.5 2071-2100
# rattler.vsi <- faktorji_71_00_rcp45
# rattler.vsi=na.omit(rattler.vsi)
# povp_proj <- aggregate(. ~  lats + lons, rattler.vsi, FUN = mean , na.rm=TRUE, na.action=na.pass)
# anomalija_rastna <- povp_proj
# colnames(anomalija_rastna)[3] <- "proj"
# povp_hist_zrastna<-data.frame(lats=povp_hist$lats, lons = povp_hist$lons, hist=povp_hist$zrastna)
# anomalija_rastna_proj<-data.frame(lats=anomalija_rastna$lats, lons = anomalija_rastna$lons,proj=anomalija_rastna$zrastna)
# anomalija_rastna_proj<- merge(povp_hist_zrastna,anomalija_rastna_proj,by=c("lats","lons"))
# anomalija_rastna_proj$zrastna <- anomalija_rastna_proj$proj - anomalija_rastna_proj$hist
# lok1 <- subset(anomalija_rastna_proj, lats == lat_Jablje & lons == lon_Jablje)
# lok2 <- subset(anomalija_rastna_proj, lats == lat_Rakican & lons == lon_Rakican)
# print(c("Jablje rcp45_71_00",lok1$hist,lok1$zrastna))
# print(c("Rakican rcp45_71_00",lok2$hist,lok2$zrastna))
# ano1<-ggplot() +
#   geom_tile(data=anomalija_rastna_proj,aes(x=lons,y=lats,fill=zrastna)) +
#   geom_sf(data=slovenia_nuts3_mapdata, fill = NA) +
#   scale_fill_distiller("z", palette = "RdBu",limits=c(-4,4)) + # 
#   coord_sf(crs = st_crs(4326)) +
#   theme_map() + labs(x="G. dolzina",y="G. sirina",title="Anomalije - rastna doba RCP4.5 2071-2100") +
#   geom_point(data = pts, aes(x = lon, y = lat, shape = Location), size = 3) + # Map 'point_type' to shape
#   scale_shape_manual(name = "Location", # Set the title for the shape legend
#                      values = c("Jablje" = 17, "Rakican" = 16)) + # Specify shapes (24=triangle, 22=square)
#   theme(legend.position = c(.9, .3),legend.title = element_text(face = "bold", size=16),
#         text = element_text(size=18),
#         plot.title = element_text(size=18),
#         axis.title.x=element_blank(),
#         axis.title.y=element_blank())
# ano1
# ggsave(paste0("karte_psenica/rcp45_mediana/anomalije_rastna_2071-2100.png"),ano1,width = 10, height = 7)
# 
# # RCP8.5 2071-2100
# rattler.vsi <- faktorji_71_00_rcp85
# rattler.vsi=na.omit(rattler.vsi)
# povp_proj <- aggregate(. ~  lats + lons, rattler.vsi, FUN = mean , na.rm=TRUE, na.action=na.pass)
# anomalija_rastna <- povp_proj
# colnames(anomalija_rastna)[3] <- "proj"
# povp_hist_zrastna<-data.frame(lats=povp_hist$lats, lons = povp_hist$lons, hist=povp_hist$zrastna)
# anomalija_rastna_proj<-data.frame(lats=anomalija_rastna$lats, lons = anomalija_rastna$lons,proj=anomalija_rastna$zrastna)
# anomalija_rastna_proj<- merge(povp_hist_zrastna,anomalija_rastna_proj,by=c("lats","lons"))
# anomalija_rastna_proj$zrastna <- anomalija_rastna_proj$proj - anomalija_rastna_proj$hist
# lok1 <- subset(anomalija_rastna_proj, lats == lat_Jablje & lons == lon_Jablje)
# lok2 <- subset(anomalija_rastna_proj, lats == lat_Rakican & lons == lon_Rakican)
# print(c("Jablje rcp85_71_00",lok1$hist,lok1$zrastna))
# print(c("Rakican rcp85_71_00",lok2$hist,lok2$zrastna))
# ano1<-ggplot() +
#   geom_tile(data=anomalija_rastna_proj,aes(x=lons,y=lats,fill=zrastna)) +
#   geom_sf(data=slovenia_nuts3_mapdata, fill = NA) +
#   scale_fill_distiller("z", palette = "RdBu",limits=c(-4,4)) + # 
#   coord_sf(crs = st_crs(4326)) +
#   theme_map() + labs(x="G. dolzina",y="G. sirina",title="Anomalije - rastna doba RCP8.5 2071-2100") + 
#   geom_point(data = pts, aes(x = lon, y = lat, shape = Location), size = 3) + # Map 'point_type' to shape
#   scale_shape_manual(name = "Location", # Set the title for the shape legend
#                      values = c("Jablje" = 17, "Rakican" = 16)) + # Specify shapes (24=triangle, 22=square)
#   theme(legend.position = c(.9, .3),legend.title = element_text(face = "bold", size=16),
#         text = element_text(size=18),
#         plot.title = element_text(size=18),
#         axis.title.x=element_blank(),
#         axis.title.y=element_blank())
# ano1
# ggsave(paste0("karte_psenica/rcp85_mediana/anomalije_rastna_2071-2100.png"),ano1,width = 10, height = 7)
# 
# 
# ## VISOKE PADAVINE
# 
# me_41_70_rcp45 <- grafi_lepi(faktorji_41_70_rcp45, rcp = "rcp45", obd = "2041-2070", kaz1 = "zvisoke_padavine", n=5)
# anomalija_pad <- me_41_70_rcp45
# colnames(anomalija_pad)[3] <- "proj"
# povp_hist_zpad<-data.frame(lats=povp_hist$lats, lons = povp_hist$lons, hist=povp_hist$zvisoke_padavine)
# anomalija_pad_proj<-data.frame(lats=anomalija_pad$lats, lons = anomalija_pad$lons, 
#                                   proj=anomalija_pad$proj)
# anomalija_pad_proj<- merge(povp_hist_zpad,anomalija_pad_proj,by=c("lats","lons"))
# anomalija_pad_proj$zvisoke_padavine <- anomalija_pad_proj$proj - anomalija_pad_proj$hist
# 
# ggplot() +
#   geom_tile(data=povp_hist,aes(x=lons,y=lats,fill=zvisoke_padavine)) +
#   geom_sf(data=slovenia_nuts3_mapdata, fill = NA) +
#   scale_fill_distiller("z", palette = "RdBu", limits = c(-10, 10)) + # Use gradientn for custom palette
#   coord_sf(crs = st_crs(4326)) +
#   theme_map() +#labs(x="G. dolzina",y="G. sirina",title="Hist 1981-2010") + 
#   geom_point(data = pts, aes(x = lon, y = lat, shape = Location), size = 3) + # Map 'point_type' to shape
#   scale_shape_manual(name = "Location", # Set the title for the shape legend
#                      values = c("Jablje" = 17, "Rakican" = 16)) + # Specify shapes (24=triangle, 22=square)
#   theme_map() + theme(legend.position = c(.9, .3),legend.title = element_text(face = "bold", size=18),
#                       text = element_text(size=18),
#                       plot.title = element_text(size=18),
#                       axis.title.x=element_blank(),
#                       axis.title.y=element_blank())
# ggsave(paste0("karte_psenica/faktorvisoke_padavine1981-2010.png"),width = 10, height = 7)
# lok1 <- subset(anomalija_pad_proj, lats == lat_Jablje & lons == lon_Jablje)
# lok2 <- subset(anomalija_pad_proj, lats == lat_Rakican & lons == lon_Rakican)
# print(c(lok1$hist,lok1$zvisoke_padavine))
# print(c(lok2$hist,lok2$zvisoke_padavine))
# 
# ggplot() +
#   geom_tile(data=anomalija_pad_proj,aes(x=lons,y=lats,fill=proj)) +
#   geom_sf(data=slovenia_nuts3_mapdata, fill = NA) +
#   scale_fill_distiller("z", palette = "RdBu", limits = c(-25, 25)) + # Use gradientn for custom palette
#   coord_sf(crs = st_crs(4326)) +
#   theme_map() +labs(x="G. dolzina",y="G. sirina",title="Proj 2041-2070") + 
#   geom_point(data = pts, aes(x = lon, y = lat, shape = Location), size = 3) + # Map 'point_type' to shape
#   scale_shape_manual(name = "Location", # Set the title for the shape legend
#                      values = c("Jablje" = 17, "Rakican" = 16)) + # Specify shapes (24=triangle, 22=square)
#   theme_map() + theme(legend.position = c(.9, .3),legend.title = element_text(face = "bold", size=18),
#                       text = element_text(size=18),
#                       plot.title = element_text(size=18),
#                       axis.title.x=element_blank(),
#                       axis.title.y=element_blank())
# ano2<-ggplot() +
#   geom_tile(data=anomalija_pad_proj,aes(x=lons,y=lats,fill=zvisoke_padavine)) +
#   geom_sf(data=slovenia_nuts3_mapdata, fill = NA) +
#   scale_fill_distiller("z", palette = "RdBu", limits = c(-25, 25)) + # Use gradientn for custom palette
#   coord_sf(crs = st_crs(4326)) +
#   theme_map() + labs(x="G. dolzina",y="G. sirina",title="Anomalije - visoke padavine RCP4.5 2041-2070") + 
#   geom_point(data = pts, aes(x = lon, y = lat, shape = Location), size = 3) + # Map 'point_type' to shape
#   scale_shape_manual(name = "Location", # Set the title for the shape legend
#                      values = c("Jablje" = 17, "Rakican" = 16)) + # Specify shapes (24=triangle, 22=square)  
#   theme(legend.position = c(.9, .3),legend.title = element_text(face = "bold", size=18),
#         text = element_text(size=18),
#         plot.title = element_text(size=18),
#         axis.title.x=element_blank(),
#         axis.title.y=element_blank())
# ano2
# ggsave(paste0("karte_psenica/rcp45_mediana/anomalije_visoke_padavine_2041-2070.png"),ano2,width = 10, height = 7)
# 
# # RCP4.5 2071-2100
# me_71_00_rcp45 <- grafi_lepi(faktorji_71_00_rcp45, rcp = "rcp45", obd = "2071-2100", kaz1 = "zvisoke_padavine", n=5)
# anomalija_pad <- me_71_00_rcp45
# colnames(anomalija_pad)[3] <- "proj"
# povp_hist_zpad<-data.frame(lats=povp_hist$lats, lons = povp_hist$lons, hist=povp_hist$zvisoke_padavine)
# anomalija_pad_proj<-data.frame(lats=anomalija_pad$lats, lons = anomalija_pad$lons, 
#                                proj=anomalija_pad$proj)
# anomalija_pad_proj<- merge(povp_hist_zpad,anomalija_pad_proj,by=c("lats","lons"))
# anomalija_pad_proj$zvisoke_padavine <- anomalija_pad_proj$proj - anomalija_pad_proj$hist
# lok1 <- subset(anomalija_pad_proj, lats == lat_Jablje & lons == lon_Jablje)
# lok2 <- subset(anomalija_pad_proj, lats == lat_Rakican & lons == lon_Rakican)
# print(c("Jablje rcp45_71_00",lok1$hist,lok1$zvisoke_padavine))
# print(c("Rakican rcp45_71_00",lok2$hist,lok2$zvisoke_padavine))
# ano2<-ggplot() +
#   geom_tile(data=anomalija_pad_proj,aes(x=lons,y=lats,fill=zvisoke_padavine)) +
#   geom_sf(data=slovenia_nuts3_mapdata, fill = NA) +
#   scale_fill_distiller("z", palette = "RdBu", limits = c(-25, 25)) + # Use gradientn for custom palette
#   coord_sf(crs = st_crs(4326)) +
#   theme_map() + labs(x="G. dolzina",y="G. sirina",title="Anomalije - visoke padavine RCP4.5 2071-2100") + 
#   geom_point(data = pts, aes(x = lon, y = lat, shape = Location), size = 3) + # Map 'point_type' to shape
#   scale_shape_manual(name = "Location", # Set the title for the shape legend
#                      values = c("Jablje" = 17, "Rakican" = 16)) + # Specify shapes (24=triangle, 22=square)  
#   theme(legend.position = c(.9, .3),legend.title = element_text(face = "bold", size=18),
#         text = element_text(size=18),
#         plot.title = element_text(size=18),
#         axis.title.x=element_blank(),
#         axis.title.y=element_blank())
# ano2
# ggsave(paste0("karte_psenica/rcp45_mediana/anomalije_visoke_padavine_2071-2100.png"),ano2,width = 10, height = 7)
# 
# # RCP8.5
# me_71_00_rcp85 <- grafi_lepi(faktorji_71_00_rcp85, rcp = "rcp85", obd = "2071-2100", kaz1 = "zvisoke_padavine", n=5)
# anomalija_pad <- me_71_00_rcp85
# colnames(anomalija_pad)[3] <- "proj"
# povp_hist_zpad<-data.frame(lats=povp_hist$lats, lons = povp_hist$lons, hist=povp_hist$zvisoke_padavine)
# anomalija_pad_proj<-data.frame(lats=anomalija_pad$lats, lons = anomalija_pad$lons, 
#                                proj=anomalija_pad$proj)
# anomalija_pad_proj<- merge(povp_hist_zpad,anomalija_pad_proj,by=c("lats","lons"))
# anomalija_pad_proj$zvisoke_padavine <- anomalija_pad_proj$proj - anomalija_pad_proj$hist
# lok1 <- subset(anomalija_pad_proj, lats == lat_Jablje & lons == lon_Jablje)
# lok2 <- subset(anomalija_pad_proj, lats == lat_Rakican & lons == lon_Rakican)
# print(c("Jablje rcp85_71_00",lok1$hist,lok1$zvisoke_padavine))
# print(c("Rakican rcp85_71_00",lok2$hist,lok2$zvisoke_padavine))
# ano2<-ggplot() +
#   geom_tile(data=anomalija_pad_proj,aes(x=lons,y=lats,fill=zvisoke_padavine)) +
#   geom_sf(data=slovenia_nuts3_mapdata, fill = NA) +
#   scale_fill_distiller("z", palette = "RdBu", limits = c(-25, 25)) + # Use gradientn for custom palette
#   coord_sf(crs = st_crs(4326)) +
#   theme_map() + labs(x="G. dolzina",y="G. sirina",title="Anomalije - visoke padavine RCP8.5 2071-2100") + 
#   geom_point(data = pts, aes(x = lon, y = lat, shape = Location), size = 3) + # Map 'point_type' to shape
#   scale_shape_manual(name = "Location", # Set the title for the shape legend
#                      values = c("Jablje" = 17, "Rakican" = 16)) + # Specify shapes (24=triangle, 22=square)  
#   theme(legend.position = c(.9, .3),legend.title = element_text(face = "bold", size=18),
#         text = element_text(size=18),
#         plot.title = element_text(size=18),
#         axis.title.x=element_blank(),
#         axis.title.y=element_blank())
# ano2
# ggsave(paste0("karte_psenica/rcp85_mediana/anomalije_visoke_padavine_2071-2100.png"),ano2,width = 10, height = 7)
# 
# 
# 
# # VROCINSKI STRES
# 
# me_41_70_rcp45 <- grafi_lepi(faktorji_41_70_rcp45, rcp = "rcp45", obd = "2041-2070", kaz1 = "zvrocinski_stres", n=2)
# anomalija_vroc <- me_41_70_rcp45
# colnames(anomalija_vroc)[3] <- "proj"
# povp_hist_zpad<-data.frame(lats=povp_hist$lats, lons = povp_hist$lons, hist=povp_hist$zvrocinski_stres)
# anomalija_vroc_proj<-data.frame(lats=anomalija_vroc$lats, lons = anomalija_vroc$lons, 
#                                proj=anomalija_vroc$proj)
# anomalija_vroc_proj<- merge(povp_hist_zpad,anomalija_vroc_proj,by=c("lats","lons"))
# anomalija_vroc_proj$zvrocinski_stres <- anomalija_vroc_proj$proj - anomalija_vroc_proj$hist
# 
# b1 <- ggplot() +
#   geom_tile(data=povp_hist,aes(x=lons,y=lats,fill=zvrocinski_stres)) +
#   geom_sf(data=slovenia_nuts3_mapdata, fill = NA) +
#   scale_fill_distiller("z", palette = "RdBu", limits = c(-10, 10)) + # Use gradientn for custom palette
#   coord_sf(crs = st_crs(4326)) +
#   theme_map() + #labs(x="G. dolzina",y="G. sirina",title="Hist 1981-2010") + 
#   geom_point(data = pts, aes(x = lon, y = lat, shape = Location), size = 3) + # Map 'point_type' to shape
#   scale_shape_manual(name = "Location", # Set the title for the shape legend
#                      values = c("Jablje" = 17, "Rakican" = 16)) + # Specify shapes (24=triangle, 22=square)
#   theme_map() + theme(legend.position = c(.9, .3),legend.title = element_text(face = "bold", size=18),
#                       text = element_text(size=18),
#                       plot.title = element_text(size=18),
#                       axis.title.x=element_blank(),
#                       axis.title.y=element_blank())
# b1
# ggsave(paste0("karte_psenica/faktorvrocinski_stres1981-2010.png"),b1,width = 10, height = 7)
# lok1 <- subset(anomalija_vroc_proj, lats == lat_Jablje & lons == lon_Jablje)
# lok2 <- subset(anomalija_vroc_proj, lats == lat_Rakican & lons == lon_Rakican)
# print(c(lok1$hist,lok1$zvrocinski_stres))
# print(c(lok2$hist,lok2$zvrocinski_stres))
# 
# ggplot() +
#   geom_tile(data=anomalija_vroc_proj,aes(x=lons,y=lats,fill=proj)) +
#   geom_sf(data=slovenia_nuts3_mapdata, fill = NA) +
#   scale_fill_distiller("z", palette = "RdBu", limits = c(-20,20)) + # Use gradientn for custom palette
#   coord_sf(crs = st_crs(4326)) +
#   theme_map() +labs(x="G. dolzina",y="G. sirina",title="Proj 2041-2070") + 
#   geom_point(data = pts, aes(x = lon, y = lat, shape = Location), size = 3) + # Map 'point_type' to shape
#   scale_shape_manual(name = "Location", # Set the title for the shape legend
#                      values = c("Jablje" = 17, "Rakican" = 16)) + # Specify shapes (24=triangle, 22=square)
#   theme_map() + theme(legend.position = c(.9, .3),legend.title = element_text(face = "bold", size=18),
#                       text = element_text(size=18),
#                       plot.title = element_text(size=18),
#                       axis.title.x=element_blank(),
#                       axis.title.y=element_blank())
# ano2<-ggplot() +
#   geom_tile(data=anomalija_vroc_proj,aes(x=lons,y=lats,fill=zvrocinski_stres)) +
#   geom_sf(data=slovenia_nuts3_mapdata, fill = NA) +
#   scale_fill_distiller("z", palette = "RdBu", limits = c(-22,22)) + # Use gradientn for custom palette
#   coord_sf(crs = st_crs(4326)) +
#   theme_map() + labs(x="G. dolzina",y="G. sirina",title="Anomalije - vrocinski stres RCP4.5 2041-2070") + 
#   geom_point(data = pts, aes(x = lon, y = lat, shape = Location), size = 3) + # Map 'point_type' to shape
#   scale_shape_manual(name = "Location", # Set the title for the shape legend
#                      values = c("Jablje" = 17, "Rakican" = 16)) + # Specify shapes (24=triangle, 22=square)  
#   theme(legend.position = c(.9, .3),legend.title = element_text(face = "bold", size=18),
#         text = element_text(size=18),
#         plot.title = element_text(size=18),
#         axis.title.x=element_blank(),
#         axis.title.y=element_blank())
# ano2
# ggsave(paste0("karte_psenica/rcp45_mediana/anomalije_vrocinski_stres_2041-2070.png"),ano2,width = 10, height = 7)
# 
# # RCP4.5 2071-2100
# me_71_00_rcp45 <- grafi_lepi(faktorji_71_00_rcp45, rcp = "rcp45", obd = "2071-2100", kaz1 = "zvrocinski_stres", n=2)
# anomalija_vroc <- me_71_00_rcp45
# colnames(anomalija_vroc)[3] <- "proj"
# povp_hist_zpad<-data.frame(lats=povp_hist$lats, lons = povp_hist$lons, hist=povp_hist$zvrocinski_stres)
# anomalija_vroc_proj<-data.frame(lats=anomalija_vroc$lats, lons = anomalija_vroc$lons, 
#                                 proj=anomalija_vroc$proj)
# anomalija_vroc_proj<- merge(povp_hist_zpad,anomalija_vroc_proj,by=c("lats","lons"))
# anomalija_vroc_proj$zvrocinski_stres <- anomalija_vroc_proj$proj - anomalija_vroc_proj$hist
# lok1 <- subset(anomalija_vroc_proj, lats == lat_Jablje & lons == lon_Jablje)
# lok2 <- subset(anomalija_vroc_proj, lats == lat_Rakican & lons == lon_Rakican)
# print(c(lok1$hist,lok1$zvrocinski_stres))
# print(c(lok2$hist,lok2$zvrocinski_stres))
# ano2<-ggplot() +
#   geom_tile(data=anomalija_vroc_proj,aes(x=lons,y=lats,fill=zvrocinski_stres)) +
#   geom_sf(data=slovenia_nuts3_mapdata, fill = NA) +
#   scale_fill_distiller("z", palette = "RdBu", limits = c(-22,22)) + # Use gradientn for custom palette
#   coord_sf(crs = st_crs(4326)) +
#   theme_map() + labs(x="G. dolzina",y="G. sirina",title="Anomalije - vrocinski stres RCP4.5 2071-2100") + 
#   geom_point(data = pts, aes(x = lon, y = lat, shape = Location), size = 3) + # Map 'point_type' to shape
#   scale_shape_manual(name = "Location", # Set the title for the shape legend
#                      values = c("Jablje" = 17, "Rakican" = 16)) + # Specify shapes (24=triangle, 22=square)  
#   theme(legend.position = c(.9, .3),legend.title = element_text(face = "bold", size=18),
#         text = element_text(size=18),
#         plot.title = element_text(size=18),
#         axis.title.x=element_blank(),
#         axis.title.y=element_blank())
# ano2
# ggsave(paste0("karte_psenica/rcp45_mediana/anomalije_vrocinski_stres_2071-2100.png"),ano2,width = 10, height = 7)
# 
# 
# # RCP8.5
# me_71_00_rcp85 <- grafi_lepi(faktorji_71_00_rcp85, rcp = "rcp85", obd = "2071-2100", kaz1 = "zvrocinski_stres", n=2)
# anomalija_vroc <- me_71_00_rcp85
# colnames(anomalija_vroc)[3] <- "proj"
# povp_hist_zpad<-data.frame(lats=povp_hist$lats, lons = povp_hist$lons, hist=povp_hist$zvrocinski_stres)
# anomalija_vroc_proj<-data.frame(lats=anomalija_vroc$lats, lons = anomalija_vroc$lons, 
#                                 proj=anomalija_vroc$proj)
# anomalija_vroc_proj<- merge(povp_hist_zpad,anomalija_vroc_proj,by=c("lats","lons"))
# anomalija_vroc_proj$zvrocinski_stres <- anomalija_vroc_proj$proj - anomalija_vroc_proj$hist
# lok1 <- subset(anomalija_vroc_proj, lats == lat_Jablje & lons == lon_Jablje)
# lok2 <- subset(anomalija_vroc_proj, lats == lat_Rakican & lons == lon_Rakican)
# print(c(lok1$hist,lok1$zvrocinski_stres))
# print(c(lok2$hist,lok2$zvrocinski_stres))
# ano2<-ggplot() +
#   geom_tile(data=anomalija_vroc_proj,aes(x=lons,y=lats,fill=zvrocinski_stres)) +
#   geom_sf(data=slovenia_nuts3_mapdata, fill = NA) +
#   scale_fill_distiller("z", palette = "RdBu", limits = c(-22,22)) + # Use gradientn for custom palette
#   coord_sf(crs = st_crs(4326)) +
#   theme_map() + labs(x="G. dolzina",y="G. sirina",title="Anomalije - vrocinski stres RCP8.5 2071-2100") + 
#   geom_point(data = pts, aes(x = lon, y = lat, shape = Location), size = 3) + # Map 'point_type' to shape
#   scale_shape_manual(name = "Location", # Set the title for the shape legend
#                      values = c("Jablje" = 17, "Rakican" = 16)) + # Specify shapes (24=triangle, 22=square)  
#   theme(legend.position = c(.9, .3),legend.title = element_text(face = "bold", size=18),
#         text = element_text(size=18),
#         plot.title = element_text(size=18),
#         axis.title.x=element_blank(),
#         axis.title.y=element_blank())
# ano2
# ggsave(paste0("karte_psenica/rcp85_mediana/anomalije_vrocinski_stres_2071-2100.png"),ano2,width = 10, height = 7)
# 
# 
# ######### VROCINSKI STRES Z VLAGO IN VISOKIMI T PONOCI
# 
# rattler.vsi <- faktorji_41_70_rcp45
# #colnames(rattler.vsi) <- c("lons","lats","leto","agroclim_ind")
# rattler.vsi=na.omit(rattler.vsi)
# povp_proj <- aggregate(. ~  lats + lons, rattler.vsi, FUN = mean , na.rm=TRUE, na.action=na.pass)
# anomalija_vroc1 <- povp_proj
# 
# colnames(anomalija_vroc1)[3] <- "proj"
# povp_hist_zvrocinski_stres_Vlaga_noc<-data.frame(lats=povp_hist$lats, lons = povp_hist$lons, hist=povp_hist$zvrocinski_stres_Vlaga_noc,
#                               WSDI_hist=povp_hist$WSDI, tn90p_hist=povp_hist$tn90p,
#                               vwd_hist=povp_hist$vwd)
# anomalija_vroc1_proj<-data.frame(lats=anomalija_vroc1$lats, lons = anomalija_vroc1$lons, 
#                                   proj=anomalija_vroc1$zvrocinski_stres_Vlaga_noc,
#                                   WSDI_proj=anomalija_vroc1$WSDI, 
#                                   tn90p_proj=anomalija_vroc1$tn90p,
#                                   vwd_proj=anomalija_vroc1$vwd)
# anomalija_vroc1_proj<- merge(povp_hist_zvrocinski_stres_Vlaga_noc,anomalija_vroc1_proj,by=c("lats","lons"))
# anomalija_vroc1_proj$zvrocinski_stres_Vlaga_noc <- anomalija_vroc1_proj$proj - anomalija_vroc1_proj$hist
# povprecje_slovenije_hist_zvrocinski_stres_Vlaga_noc<- mean(anomalija_vroc1_proj$hist)
# povprecje_slovenije_proj_zvrocinski_stres_Vlaga_noc<-mean(anomalija_vroc1_proj$proj)
# 
# 
# me_41_70_rcp45 <- grafi_lepi(faktorji_41_70_rcp45, rcp = "rcp45", obd = "2041-2070", kaz1 = "zvrocinski_stres_Vlaga_noc", n=4)
# anomalija_vroc1 <- me_41_70_rcp45
# colnames(anomalija_vroc1)[3] <- "proj"
# povp_hist_zvroc1<-data.frame(lats=povp_hist$lats, lons = povp_hist$lons, hist=povp_hist$zvrocinski_stres_Vlaga_noc)
# anomalija_vroc1_proj<-data.frame(lats=anomalija_vroc1$lats, lons = anomalija_vroc1$lons, 
#                                 proj=anomalija_vroc1$proj)
# anomalija_vroc1_proj<- merge(povp_hist_zvroc1,anomalija_vroc1_proj,by=c("lats","lons"))
# anomalija_vroc1_proj$zvrocinski_stres_Vlaga_noc <- anomalija_vroc1_proj$proj - anomalija_vroc1_proj$hist
# 
# b1 <- ggplot() +
#   geom_tile(data=povp_hist,aes(x=lons,y=lats,fill=zvrocinski_stres_Vlaga_noc)) +
#   geom_sf(data=slovenia_nuts3_mapdata, fill = NA) +
#   scale_fill_distiller("z", palette = "RdBu", limits = c(-4,4)) + # Use gradientn for custom palette
#   coord_sf(crs = st_crs(4326)) +
#   theme_map() + #labs(x="G. dolzina",y="G. sirina",title="Hist 1981-2010") + 
#   geom_point(data = pts, aes(x = lon, y = lat, shape = Location), size = 3) + # Map 'point_type' to shape
#   scale_shape_manual(name = "Location", # Set the title for the shape legend
#                      values = c("Jablje" = 17, "Rakican" = 16)) + # Specify shapes (24=triangle, 22=square)
#   theme_map() + theme(legend.position = c(.9, .3),legend.title = element_text(face = "bold", size=18),
#                       text = element_text(size=18),
#                       plot.title = element_text(size=18),
#                       axis.title.x=element_blank(),
#                       axis.title.y=element_blank())
# b1
# ggsave(paste0("karte_psenica/faktorvrocinski_stres_Vlaga_noc1981-2010.png"),b1,width = 10, height = 7)
# lok1 <- subset(anomalija_vroc1_proj, lats == lat_Jablje & lons == lon_Jablje)
# lok2 <- subset(anomalija_vroc1_proj, lats == lat_Rakican & lons == lon_Rakican)
# print(c(lok1$hist,lok1$zvrocinski_stres_Vlaga_noc))
# print(c(lok2$hist,lok2$zvrocinski_stres_Vlaga_noc))
# 
# ggplot() +
#   geom_tile(data=anomalija_vroc1_proj,aes(x=lons,y=lats,fill=proj)) +
#   geom_sf(data=slovenia_nuts3_mapdata, fill = NA) +
#   scale_fill_distiller("z", palette = "RdBu", limits = c(-4,4))+ #, limits = c(-5,5)) + # Use gradientn for custom palette
#   coord_sf(crs = st_crs(4326)) +
#   theme_map() +labs(x="G. dolzina",y="G. sirina",title="Proj 2041-2070") + 
#   geom_point(data = pts, aes(x = lon, y = lat, shape = Location), size = 3) + # Map 'point_type' to shape
#   scale_shape_manual(name = "Location", # Set the title for the shape legend
#                      values = c("Jablje" = 17, "Rakican" = 16)) + # Specify shapes (24=triangle, 22=square)
#   theme_map() + theme(legend.position = c(.9, .3),legend.title = element_text(face = "bold", size=18),
#                       text = element_text(size=18),
#                       plot.title = element_text(size=18),
#                       axis.title.x=element_blank(),
#                       axis.title.y=element_blank())
# 
# ano2<-ggplot() +
#   geom_tile(data=anomalija_vroc1_proj,aes(x=lons,y=lats,fill=zvrocinski_stres_Vlaga_noc)) +
#   geom_sf(data=slovenia_nuts3_mapdata, fill = NA) +
#   scale_fill_distiller("z", palette = "RdBu", limits = c(-10,10)) + # Use gradientn for custom palette
#   coord_sf(crs = st_crs(4326)) +
#   theme_map() + labs(x="G. dolzina",y="G. sirina",title="Anomalije - vrocina ponoci in vlaga RCP4.5 2041-2070") + 
#   geom_point(data = pts, aes(x = lon, y = lat, shape = Location), size = 3) + # Map 'point_type' to shape
#   scale_shape_manual(name = "Location", # Set the title for the shape legend
#                      values = c("Jablje" = 17, "Rakican" = 16)) + # Specify shapes (24=triangle, 22=square)  
#   theme(legend.position = c(.9, .3),legend.title = element_text(face = "bold", size=18),
#         text = element_text(size=18),
#         plot.title = element_text(size=18),
#         axis.title.x=element_blank(),
#         axis.title.y=element_blank())
# ano2
# ggsave(paste0("karte_psenica/rcp45_mediana/anomalije_vrocinski_stres_Vlaga_noc_2041-2070.png"),ano2,width = 10, height = 7)
# 
# # RCP4.5 2071-2100
# me_71_00_rcp45 <- grafi_lepi(faktorji_71_00_rcp45, rcp = "rcp45", obd = "2071-2100", kaz1 = "zvrocinski_stres_Vlaga_noc", n=4)
# anomalija_vroc1 <- me_71_00_rcp45
# colnames(anomalija_vroc1)[3] <- "proj"
# povp_hist_zvroc1<-data.frame(lats=povp_hist$lats, lons = povp_hist$lons, hist=povp_hist$zvrocinski_stres_Vlaga_noc)
# anomalija_vroc1_proj<-data.frame(lats=anomalija_vroc1$lats, lons = anomalija_vroc1$lons, 
#                                  proj=anomalija_vroc1$proj)
# anomalija_vroc1_proj<- merge(povp_hist_zvroc1,anomalija_vroc1_proj,by=c("lats","lons"))
# anomalija_vroc1_proj$zvrocinski_stres_Vlaga_noc <- anomalija_vroc1_proj$proj - anomalija_vroc1_proj$hist
# lok1 <- subset(anomalija_vroc1_proj, lats == lat_Jablje & lons == lon_Jablje)
# lok2 <- subset(anomalija_vroc1_proj, lats == lat_Rakican & lons == lon_Rakican)
# print(c(lok1$hist,lok1$zvrocinski_stres_Vlaga_noc))
# print(c(lok2$hist,lok2$zvrocinski_stres_Vlaga_noc))
# ano2<-ggplot() +
#   geom_tile(data=anomalija_vroc1_proj,aes(x=lons,y=lats,fill=zvrocinski_stres_Vlaga_noc)) +
#   geom_sf(data=slovenia_nuts3_mapdata, fill = NA) +
#   scale_fill_distiller("z", palette = "RdBu", limits = c(-10,10)) + # Use gradientn for custom palette
#   coord_sf(crs = st_crs(4326)) +
#   theme_map() + labs(x="G. dolzina",y="G. sirina",title="Anomalije - vrocina ponoci in vlaga RCP4.5 2071-2100") + 
#   geom_point(data = pts, aes(x = lon, y = lat, shape = Location), size = 3) + # Map 'point_type' to shape
#   scale_shape_manual(name = "Location", # Set the title for the shape legend
#                      values = c("Jablje" = 17, "Rakican" = 16)) + # Specify shapes (24=triangle, 22=square)  
#   theme(legend.position = c(.9, .3),legend.title = element_text(face = "bold", size=18),
#         text = element_text(size=18),
#         plot.title = element_text(size=18),
#         axis.title.x=element_blank(),
#         axis.title.y=element_blank())
# ano2
# ggsave(paste0("karte_psenica/rcp45_mediana/anomalije_vrocinski_stres_Vlaga_noc_2071-2100.png"),ano2,width = 10, height = 7)
# 
# # RCP8.5 2071-2100
# me_71_00_rcp85 <- grafi_lepi(faktorji_71_00_rcp85, rcp = "rcp85", obd = "2071-2100", kaz1 = "zvrocinski_stres_Vlaga_noc", n=4)
# anomalija_vroc1 <- me_71_00_rcp85
# colnames(anomalija_vroc1)[3] <- "proj"
# povp_hist_zvroc1<-data.frame(lats=povp_hist$lats, lons = povp_hist$lons, hist=povp_hist$zvrocinski_stres_Vlaga_noc)
# anomalija_vroc1_proj<-data.frame(lats=anomalija_vroc1$lats, lons = anomalija_vroc1$lons, 
#                                  proj=anomalija_vroc1$proj)
# anomalija_vroc1_proj<- merge(povp_hist_zvroc1,anomalija_vroc1_proj,by=c("lats","lons"))
# anomalija_vroc1_proj$zvrocinski_stres_Vlaga_noc <- anomalija_vroc1_proj$proj - anomalija_vroc1_proj$hist
# lok1 <- subset(anomalija_vroc1_proj, lats == lat_Jablje & lons == lon_Jablje)
# lok2 <- subset(anomalija_vroc1_proj, lats == lat_Rakican & lons == lon_Rakican)
# print(c(lok1$hist,lok1$zvrocinski_stres_Vlaga_noc))
# print(c(lok2$hist,lok2$zvrocinski_stres_Vlaga_noc))
# ano2<-ggplot() +
#   geom_tile(data=anomalija_vroc1_proj,aes(x=lons,y=lats,fill=zvrocinski_stres_Vlaga_noc)) +
#   geom_sf(data=slovenia_nuts3_mapdata, fill = NA) +
#   scale_fill_distiller("z", palette = "RdBu", limits = c(-10,10)) + # Use gradientn for custom palette
#   coord_sf(crs = st_crs(4326)) +
#   theme_map() + labs(x="G. dolzina",y="G. sirina",title="Anomalije - vrocina ponoci in vlaga RCP8.5 2071-2100") + 
#   geom_point(data = pts, aes(x = lon, y = lat, shape = Location), size = 3) + # Map 'point_type' to shape
#   scale_shape_manual(name = "Location", # Set the title for the shape legend
#                      values = c("Jablje" = 17, "Rakican" = 16)) + # Specify shapes (24=triangle, 22=square)  
#   theme(legend.position = c(.9, .3),legend.title = element_text(face = "bold", size=18),
#         text = element_text(size=18),
#         plot.title = element_text(size=18),
#         axis.title.x=element_blank(),
#         axis.title.y=element_blank())
# ano2
# ggsave(paste0("karte_psenica/rcp85_mediana/anomalije_vrocinski_stres_Vlaga_noc_2071-2100.png"),ano2,width = 10, height = 7)
# 
# 
# # PREZIMOVALNE RAZMERE
# 
# me_41_70_rcp45 <- grafi_lepi(faktorji_41_70_rcp45, rcp = "rcp45", obd = "2041-2070", kaz1 = "zprezimovanje_minT", n=7)
# anomalija_prezim <- me_41_70_rcp45
# colnames(anomalija_prezim)[3] <- "proj"
# povp_hist_zpad<-data.frame(lats=povp_hist$lats, lons = povp_hist$lons, hist=povp_hist$zprezimovanje_minT)
# anomalija_prezim_proj<-data.frame(lats=anomalija_prezim$lats, lons = anomalija_prezim$lons, 
#                                 proj=anomalija_prezim$proj)
# anomalija_prezim_proj<- merge(povp_hist_zpad,anomalija_prezim_proj,by=c("lats","lons"))
# anomalija_prezim_proj$zprezimovanje_minT <- anomalija_prezim_proj$proj - anomalija_prezim_proj$hist
# 
# b1 <- ggplot() +
#   geom_tile(data=povp_hist,aes(x=lons,y=lats,fill=zprezimovanje_minT)) +
#   geom_sf(data=slovenia_nuts3_mapdata, fill = NA) +
#   scale_fill_distiller("z", palette = "RdBu", limits = c(-2,2)) + # Use gradientn for custom palette
#   coord_sf(crs = st_crs(4326)) +
#   theme_map() + #labs(x="G. dolzina",y="G. sirina",title="Hist 1981-2010") + 
#   geom_point(data = pts, aes(x = lon, y = lat, shape = Location), size = 3) + # Map 'point_type' to shape
#   scale_shape_manual(name = "Location", # Set the title for the shape legend
#                      values = c("Jablje" = 17, "Rakican" = 16)) + # Specify shapes (24=triangle, 22=square)
#   theme_map() + theme(legend.position = c(.9, .3),legend.title = element_text(face = "bold", size=18),
#                       text = element_text(size=18),
#                       plot.title = element_text(size=18),
#                       axis.title.x=element_blank(),
#                       axis.title.y=element_blank())
# b1
# ggsave(paste0("karte_psenica/faktorprezimovanje_minT1981-2010.png"),b1,width = 10, height = 7)
# lok1 <- subset(anomalija_prezim_proj, lats == lat_Jablje & lons == lon_Jablje)
# lok2 <- subset(anomalija_prezim_proj, lats == lat_Rakican & lons == lon_Rakican)
# print(c(lok1$hist,lok1$zprezimovanje_minT))
# print(c(lok2$hist,lok2$zprezimovanje_minT))
# 
# ggplot() +
#   geom_tile(data=anomalija_prezim_proj,aes(x=lons,y=lats,fill=proj)) +
#   geom_sf(data=slovenia_nuts3_mapdata, fill = NA) +
#   scale_fill_distiller("z", palette = "RdBu", limits = c(-10,10)) + # Use gradientn for custom palette
#   coord_sf(crs = st_crs(4326)) +
#   theme_map() +labs(x="G. dolzina",y="G. sirina",title="Proj. 2041-2070") + 
#   geom_point(data = pts, aes(x = lon, y = lat, shape = Location), size = 3) + # Map 'point_type' to shape
#   scale_shape_manual(name = "Location", # Set the title for the shape legend
#                      values = c("Jablje" = 17, "Rakican" = 16)) + # Specify shapes (24=triangle, 22=square)
#   theme_map() + theme(legend.position = c(.9, .3),legend.title = element_text(face = "bold", size=18),
#                       text = element_text(size=18),
#                       plot.title = element_text(size=18),
#                       axis.title.x=element_blank(),
#                       axis.title.y=element_blank())
# ano2<-ggplot() +
#   geom_tile(data=anomalija_prezim_proj,aes(x=lons,y=lats,fill=zprezimovanje_minT)) +
#   geom_sf(data=slovenia_nuts3_mapdata, fill = NA) +
#   scale_fill_distiller("z", palette = "RdBu", limits = c(-10,10)) + # Use gradientn for custom palette
#   coord_sf(crs = st_crs(4326)) +
#   theme_map() + labs(x="G. dolzina",y="G. sirina",title="Anomalije - prezimovanje RCP4.5 2041-2070") + 
#   geom_point(data = pts, aes(x = lon, y = lat, shape = Location), size = 3) + # Map 'point_type' to shape
#   scale_shape_manual(name = "Location", # Set the title for the shape legend
#                      values = c("Jablje" = 17, "Rakican" = 16)) + # Specify shapes (24=triangle, 22=square)  
#   theme(legend.position = c(.9, .3),legend.title = element_text(face = "bold", size=18),
#         text = element_text(size=18),
#         plot.title = element_text(size=18),
#         axis.title.x=element_blank(),
#         axis.title.y=element_blank())
# ano2
# ggsave(paste0("karte_psenica/rcp45_mediana/anomalije_prezimovanje_2041-2070.png"),ano2,width = 10, height = 7)
# 
# # RCP4.5 2071-2100
# me_71_00_rcp45 <- grafi_lepi(faktorji_71_00_rcp45, rcp = "rcp45", obd = "2071-2100", kaz1 = "zprezimovanje_minT", n=7)
# anomalija_prezim <- me_71_00_rcp45
# colnames(anomalija_prezim)[3] <- "proj"
# anomalija_prezim_proj<-data.frame(lats=anomalija_prezim$lats, lons = anomalija_prezim$lons, 
#                                   proj=anomalija_prezim$proj)
# anomalija_prezim_proj<- merge(povp_hist_zpad,anomalija_prezim_proj,by=c("lats","lons"))
# anomalija_prezim_proj$zprezimovanje_minT <- anomalija_prezim_proj$proj - anomalija_prezim_proj$hist
# 
# lok1 <- subset(anomalija_prezim_proj, lats == lat_Jablje & lons == lon_Jablje)
# lok2 <- subset(anomalija_prezim_proj, lats == lat_Rakican & lons == lon_Rakican)
# print(c(lok1$hist,lok1$zprezimovanje_minT))
# print(c(lok2$hist,lok2$zprezimovanje_minT))
# ano2<-ggplot() +
#   geom_tile(data=anomalija_prezim_proj,aes(x=lons,y=lats,fill=zprezimovanje_minT)) +
#   geom_sf(data=slovenia_nuts3_mapdata, fill = NA) +
#   scale_fill_distiller("z", palette = "RdBu", limits = c(-10,10)) + # Use gradientn for custom palette
#   coord_sf(crs = st_crs(4326)) +
#   theme_map() + labs(x="G. dolzina",y="G. sirina",title="Anomalije - prezimovanje RCP4.5 2071-2100") + 
#   geom_point(data = pts, aes(x = lon, y = lat, shape = Location), size = 3) + # Map 'point_type' to shape
#   scale_shape_manual(name = "Location", # Set the title for the shape legend
#                      values = c("Jablje" = 17, "Rakican" = 16)) + # Specify shapes (24=triangle, 22=square)  
#   theme(legend.position = c(.9, .3),legend.title = element_text(face = "bold", size=18),
#         text = element_text(size=18),
#         plot.title = element_text(size=18),
#         axis.title.x=element_blank(),
#         axis.title.y=element_blank())
# ano2
# ggsave(paste0("karte_psenica/rcp45_mediana/anomalije_prezimovanje_2071-2100.png"),ano2,width = 10, height = 7)
# 
# 
# # RCP8.5
# me_71_00_rcp85 <- grafi_lepi(faktorji_71_00_rcp85, rcp = "rcp85", obd = "2071-2100", kaz1 = "zprezimovanje_minT", n=7)
# anomalija_prezim <- me_71_00_rcp85
# colnames(anomalija_prezim)[3] <- "proj"
# anomalija_prezim_proj<-data.frame(lats=anomalija_prezim$lats, lons = anomalija_prezim$lons, 
#                                 proj=anomalija_prezim$proj)
# anomalija_prezim_proj<- merge(povp_hist_zpad,anomalija_prezim_proj,by=c("lats","lons"))
# anomalija_prezim_proj$zprezimovanje_minT <- anomalija_prezim_proj$proj - anomalija_prezim_proj$hist
# lok1 <- subset(anomalija_prezim_proj, lats == lat_Jablje & lons == lon_Jablje)
# lok2 <- subset(anomalija_prezim_proj, lats == lat_Rakican & lons == lon_Rakican)
# print(c(lok1$hist,lok1$zprezimovanje_minT))
# print(c(lok2$hist,lok2$zprezimovanje_minT))
# ano2<-ggplot() +
#   geom_tile(data=anomalija_prezim_proj,aes(x=lons,y=lats,fill=zprezimovanje_minT)) +
#   geom_sf(data=slovenia_nuts3_mapdata, fill = NA) +
#   scale_fill_distiller("z", palette = "RdBu", limits = c(-10,10)) + # Use gradientn for custom palette
#   coord_sf(crs = st_crs(4326)) +
#   theme_map() + labs(x="G. dolzina",y="G. sirina",title="Anomalije - prezimovanje RCP8.5 2071-2100") + 
#   geom_point(data = pts, aes(x = lon, y = lat, shape = Location), size = 3) + # Map 'point_type' to shape
#   scale_shape_manual(name = "Location", # Set the title for the shape legend
#                      values = c("Jablje" = 17, "Rakican" = 16)) + # Specify shapes (24=triangle, 22=square)  
#   theme(legend.position = c(.9, .3),legend.title = element_text(face = "bold", size=18),
#         text = element_text(size=18),
#         plot.title = element_text(size=18),
#         axis.title.x=element_blank(),
#         axis.title.y=element_blank())
# ano2
# ggsave(paste0("karte_psenica/rcp85_mediana/anomalije_prezimovanje_2071-2100.png"),ano2,width = 10, height = 7)
# 
# 
# 
# # POZEBA
# 
# me_41_70_rcp45 <- grafi_lepi(faktorji_41_70_rcp45, rcp = "rcp45", obd = "2041-2070", kaz1 = "zpozeba", n=1)
# anomalija_pozeba <- me_41_70_rcp45
# colnames(anomalija_pozeba)[3] <- "proj"
# povp_hist_zpad<-data.frame(lats=povp_hist$lats, lons = povp_hist$lons, hist=povp_hist$zpozeba)
# anomalija_pozeba_proj<-data.frame(lats=anomalija_pozeba$lats, lons = anomalija_pozeba$lons, 
#                                   proj=anomalija_pozeba$proj)
# anomalija_pozeba_proj<- merge(povp_hist_zpad,anomalija_pozeba_proj,by=c("lats","lons"))
# anomalija_pozeba_proj$zpozeba <- anomalija_pozeba_proj$proj - anomalija_pozeba_proj$hist
# 
# b1 <- ggplot() +
#   geom_tile(data=povp_hist,aes(x=lons,y=lats,fill=zpozeba)) +
#   geom_sf(data=slovenia_nuts3_mapdata, fill = NA) +
#   scale_fill_distiller("z", palette = "RdBu", limits = c(-5,5)) + # Use gradientn for custom palette
#   coord_sf(crs = st_crs(4326)) +
#   theme_map() + #labs(x="G. dolzina",y="G. sirina",title="Hist 1981-2010") + 
#   geom_point(data = pts, aes(x = lon, y = lat, shape = Location), size = 3) + # Map 'point_type' to shape
#   scale_shape_manual(name = "Location", # Set the title for the shape legend
#                      values = c("Jablje" = 17, "Rakican" = 16)) + # Specify shapes (24=triangle, 22=square)
#   theme_map() + theme(legend.position = c(.9, .3),legend.title = element_text(face = "bold", size=18),
#                       text = element_text(size=18),
#                       plot.title = element_text(size=18),
#                       axis.title.x=element_blank(),
#                       axis.title.y=element_blank())
# b1
# ggsave(paste0("karte_psenica/faktorpozeba1981-2010.png"),b1,width = 10, height = 7)
# lok1 <- subset(anomalija_pozeba_proj, lats == lat_Jablje & lons == lon_Jablje)
# lok2 <- subset(anomalija_pozeba_proj, lats == lat_Rakican & lons == lon_Rakican)
# print(c(lok1$hist,lok1$zpozeba))
# print(c(lok2$hist,lok2$zpozeba))
# 
# ggplot() +
#   geom_tile(data=anomalija_pozeba_proj,aes(x=lons,y=lats,fill=proj)) +
#   geom_sf(data=slovenia_nuts3_mapdata, fill = NA) +
#   scale_fill_distiller("z", palette = "RdBu", limits = c(-10,10)) + # Use gradientn for custom palette
#   coord_sf(crs = st_crs(4326)) +
#   theme_map() +labs(x="G. dolzina",y="G. sirina",title="Proj. 2041-2070") + 
#   geom_point(data = pts, aes(x = lon, y = lat, shape = Location), size = 3) + # Map 'point_type' to shape
#   scale_shape_manual(name = "Location", # Set the title for the shape legend
#                      values = c("Jablje" = 17, "Rakican" = 16)) + # Specify shapes (24=triangle, 22=square)
#   theme_map() + theme(legend.position = c(.9, .3),legend.title = element_text(face = "bold", size=18),
#                       text = element_text(size=18),
#                       plot.title = element_text(size=18),
#                       axis.title.x=element_blank(),
#                       axis.title.y=element_blank())
# ano2<-ggplot() +
#   geom_tile(data=anomalija_pozeba_proj,aes(x=lons,y=lats,fill=zpozeba)) +
#   geom_sf(data=slovenia_nuts3_mapdata, fill = NA) +
#   scale_fill_distiller("z", palette = "RdBu", limits = c(-5,5)) + # Use gradientn for custom palette
#   coord_sf(crs = st_crs(4326)) +
#   theme_map() + labs(x="G. dolzina",y="G. sirina",title="Anomalije - pozeba RCP4.5 2041-2070") + 
#   geom_point(data = pts, aes(x = lon, y = lat, shape = Location), size = 3) + # Map 'point_type' to shape
#   scale_shape_manual(name = "Location", # Set the title for the shape legend
#                      values = c("Jablje" = 17, "Rakican" = 16)) + # Specify shapes (24=triangle, 22=square)  
#   theme(legend.position = c(.9, .3),legend.title = element_text(face = "bold", size=18),
#         text = element_text(size=18),
#         plot.title = element_text(size=18),
#         axis.title.x=element_blank(),
#         axis.title.y=element_blank())
# ano2
# ggsave(paste0("karte_psenica/rcp45_mediana/anomalije_pozeba_2041-2070.png"),ano2,width = 10, height = 7)
# 
# # RCP4.5 2071-2100
# me_71_00_rcp45 <- grafi_lepi(faktorji_71_00_rcp45, rcp = "rcp45", obd = "2071-2100", kaz1 = "zpozeba", n=1)
# anomalija_pozeba <- me_71_00_rcp45
# colnames(anomalija_pozeba)[3] <- "proj"
# anomalija_pozeba_proj<-data.frame(lats=anomalija_pozeba$lats, lons = anomalija_pozeba$lons, 
#                                   proj=anomalija_pozeba$proj)
# anomalija_pozeba_proj<- merge(povp_hist_zpad,anomalija_pozeba_proj,by=c("lats","lons"))
# anomalija_pozeba_proj$zpozeba <- anomalija_pozeba_proj$proj - anomalija_pozeba_proj$hist
# 
# lok1 <- subset(anomalija_pozeba_proj, lats == lat_Jablje & lons == lon_Jablje)
# lok2 <- subset(anomalija_pozeba_proj, lats == lat_Rakican & lons == lon_Rakican)
# print(c(lok1$hist,lok1$zpozeba))
# print(c(lok2$hist,lok2$zpozeba))
# ano2<-ggplot() +
#   geom_tile(data=anomalija_pozeba_proj,aes(x=lons,y=lats,fill=zpozeba)) +
#   geom_sf(data=slovenia_nuts3_mapdata, fill = NA) +
#   scale_fill_distiller("z", palette = "RdBu", limits = c(-5,5)) + # Use gradientn for custom palette
#   coord_sf(crs = st_crs(4326)) +
#   theme_map() + labs(x="G. dolzina",y="G. sirina",title="Anomalije - pozeba RCP4.5 2071-2100") + 
#   geom_point(data = pts, aes(x = lon, y = lat, shape = Location), size = 3) + # Map 'point_type' to shape
#   scale_shape_manual(name = "Location", # Set the title for the shape legend
#                      values = c("Jablje" = 17, "Rakican" = 16)) + # Specify shapes (24=triangle, 22=square)  
#   theme(legend.position = c(.9, .3),legend.title = element_text(face = "bold", size=18),
#         text = element_text(size=18),
#         plot.title = element_text(size=18),
#         axis.title.x=element_blank(),
#         axis.title.y=element_blank())
# ano2
# ggsave(paste0("karte_psenica/rcp45_mediana/anomalije_pozeba_2071-2100.png"),ano2,width = 10, height = 7)
# 
# 
# # RCP8.5
# me_71_00_rcp85 <- grafi_lepi(faktorji_71_00_rcp85, rcp = "rcp85", obd = "2071-2100", kaz1 = "zpozeba", n=1)
# anomalija_pozeba <- me_71_00_rcp85
# colnames(anomalija_pozeba)[3] <- "proj"
# anomalija_pozeba_proj<-data.frame(lats=anomalija_pozeba$lats, lons = anomalija_pozeba$lons, 
#                                   proj=anomalija_pozeba$proj)
# anomalija_pozeba_proj<- merge(povp_hist_zpad,anomalija_pozeba_proj,by=c("lats","lons"))
# anomalija_pozeba_proj$zpozeba <- anomalija_pozeba_proj$proj - anomalija_pozeba_proj$hist
# lok1 <- subset(anomalija_pozeba_proj, lats == lat_Jablje & lons == lon_Jablje)
# lok2 <- subset(anomalija_pozeba_proj, lats == lat_Rakican & lons == lon_Rakican)
# print(c(lok1$hist,lok1$zpozeba))
# print(c(lok2$hist,lok2$zpozeba))
# ano2<-ggplot() +
#   geom_tile(data=anomalija_pozeba_proj,aes(x=lons,y=lats,fill=zpozeba)) +
#   geom_sf(data=slovenia_nuts3_mapdata, fill = NA) +
#   scale_fill_distiller("z", palette = "RdBu", limits = c(-5,5)) + # Use gradientn for custom palette
#   coord_sf(crs = st_crs(4326)) +
#   theme_map() + labs(x="G. dolzina",y="G. sirina",title="Anomalije - pozeba RCP8.5 2071-2100") + 
#   geom_point(data = pts, aes(x = lon, y = lat, shape = Location), size = 3) + # Map 'point_type' to shape
#   scale_shape_manual(name = "Location", # Set the title for the shape legend
#                      values = c("Jablje" = 17, "Rakican" = 16)) + # Specify shapes (24=triangle, 22=square)  
#   theme(legend.position = c(.9, .3),legend.title = element_text(face = "bold", size=18),
#         text = element_text(size=18),
#         plot.title = element_text(size=18),
#         axis.title.x=element_blank(),
#         axis.title.y=element_blank())
# ano2
# ggsave(paste0("karte_psenica/rcp85_mediana/anomalije_pozeba_2071-2100.png"),ano2,width = 10, height = 7)









# 
# risi1 <- function(rcp, leto_zac, leto_zac1, obd){
#   model <- c("CNRM-CERFACS-CNRM-CM5", 
#              "ICHEC-EC-EARTH",
#              "IPSL-IPSL-CM5A-MR",
#              "MOHC-HadGEM2-ES", # samo do 30. 11. 2099 pri padavinah, zato tudi pri Tmin in Tmax .nc fajl spremenim, da gre le do 30.11.
#              "MPI-M-MPI-ESM-LR",
#              "MPI-M-MPI-ESM-LR")
#   drugo <- c("_r1i1p1_CLMcom-CCLM4-8-17_v1",
#              "_r3i1p1_DMI-HIRHAM5_v1",
#              "_r1i1p1_IPSL-INERIS-WRF331F_v1",
#              "_r1i1p1_KNMI-RACMO22E_v2",
#              "_r1i1p1_CLMcom-CCLM4-8-17_v1",
#              "_r1i1p1_SMHI-RCA4_v1a")
#   leto_zac <- "2041"
#   leto_kon <- c("20701231","20701231","20701231","20701231","20701231","20701231")
#   leto_zac1 <- "2040"
#   
#   nc_data_rr<- nc_open(paste0(path,"pr","_12km_MOHC-HadGEM2-ES_",rcp,drugo[4],"_day_",leto_zac,"0101_",leto_kon[4],".nc"))
#   time <- ncvar_get(nc_data_rr, "time")
#   lon <- ncvar_get(nc_data_rr, "lon") # drugi parameter je ime spremenljivke v datoteki
#   lat <- ncvar_get(nc_data_rr, "lat")
#   start1<-c(1,1,1)
#   count1<-c(nx,ny, length(time))
#   data_rr <- ncvar_get(nc_data_rr, "pr", start=start1, count=count1)
#   datumi0 <- seq(0,length(time)-1,by = 1)
#   pretvorba_sek_v_dan1 = 60*60*24 #?eprav imam dnevne podatke moram to dat, ker as.positxct meri v sekundah
#   datumi <- as.POSIXct((start1[3]+datumi0)*pretvorba_sek_v_dan1,origin=paste0(leto_zac1,"-12-31 00:00:00"))
#   leta <- format(datumi,format="%Y")
#   datum2 = format(as.POSIXct(datumi, format="%Y-%m-%d"), "%m/%d/%Y")
#   nc_close(nc_data_rr) # konec branja
#    
#   povp_obd <- data.table()
#   
#   for(z in seq(1,6,by=1)){
#     nc_data_info <- get_nc_data(path, model[z], rcp, drugo[z], leto_zac, leto_kon[z], leto_kon[z], nx, ny, count1[3])
#     time <- nc_data_info$time
#     lon <- nc_data_info$lon
#     lat <- nc_data_info$lat
#     data_max <- nc_data_info$data_max
#     data_min <- nc_data_info$data_min
#     data3 <- nc_data_info$data3
#     data_rr <- nc_data_info$data_rr
#     data_et <- nc_data_info$data_et
#     rm(nc_data_info) # Free memory immediately
#     
#     datumi <- as.POSIXct((time) * 60 * 60 * 24, origin = paste0(leto_zac1, "-12-31 00:00:00")) #Directly use time
#     leta <- format(datumi, format = "%Y")
#     datum2 <- format(datumi, format = "%Y-%m-%d")
#     
#     
#     leto = format(datumi, format = "%Y")
#     leta1 <- seq(min(leto),max(leto),by=1)
#     lats1 <- seq(1,ny,by=1)
#     lons1 <- seq(1,nx,by=1)
#     n_rows <- 24
#     n_cols <- 40
#     n_layers <- 19
#     vsi_podatki <- data.frame()
#   
#     nc_data_max <- nc_open(paste0(path,"tasmax","_12km_",model[z],"_",rcp,drugo[z],"_day_",leto_zac,"0101_",leto_kon[z],".nc"))
#     data_max <- ncvar_get(nc_data_max, "tasmax", start=start1, count=count1)
#     nc_close(nc_data_max) # konec branja
#     
#     nc_data_min <- nc_open(paste0(path,"tasmin","_12km_",model[z],"_",rcp,drugo[z],"_day_",leto_zac,"0101_",leto_kon[z],".nc"))
#     data_min <- ncvar_get(nc_data_min, "tasmin", start=start1, count=count1)
#     nc_close(nc_data_min) # konec branja
#     
#     nc_data <- nc_open(paste0(path,"tas","_12km_",model[z],"_",rcp,drugo[z],"_day_",leto_zac,"0101_",leto_kon[z],".nc"))
#     data3 <- ncvar_get(nc_data,"tas",start=start1, count=count1)
#     nc_close(nc_data) # konec branja
#     
#     nc_data_rr<- nc_open(paste0(path,"pr","_12km_",model[z],"_",rcp,drugo[z],"_day_",leto_zac,"0101_",leto_kon[z],".nc"))
#     data_rr <- ncvar_get(nc_data_rr, "pr", start=start1, count=count1)
#     nc_close(nc_data_rr)
#     
#     nc_data_et<- nc_open(paste0(path,"evspsblpot","_12km_",model[z],"_",rcp,drugo[z],"_day_",leto_zac,"0101_",leto_kon[z],".nc"))
#     data_et <- ncvar_get(nc_data_et, "evspsblpot", start=start1, count=count1)
#     nc_close(nc_data_et)
#     
#     leto = format(datumi, format = "%Y")
#     leta1 <- seq(min(leto),max(leto),by=1)
#     lats1 <- seq(1,24,by=1)
#     lons1 <- seq(1,40,by=1)
#     n_rows <- 24
#     n_cols <- 40
#     n_layers <- 19
#     vsi_podatki <- data.frame()
#     
#     for(i in lons1){
#       for(j in lats1){
#         print(i)
#         print(j)
#         if(all(is.na(data3[i,j,1:count1[3]]))){
#           next
#         }
#         else{
#           data_temp <- data.frame(Date = datum2, meseci = as.numeric(format(datumi,format="%m")), Temp = data3[i,j,1:count1[3]]-273)
#           data_temp <- data_temp %>% filter(meseci >= 4, meseci <= 10)
#           
#           data_tmax <- data.frame(Date = datum2, meseci = as.numeric(format(datumi,format="%m")), Tmax = data_max[i,j,1:count1[3]]-273)
#           data_tmax <- data_tmax %>% filter(meseci >= 4, meseci <= 10)
#           
#           data_tmin <- data.frame(Date = datum2, meseci = as.numeric(format(datumi,format="%m")), Tmin = data_min[i,j,1:count1[3]]-273)
#           data_tmin <- data_tmin %>% filter(meseci >= 4, meseci <= 10)
#           
#           data_pad0 <- data.frame(Date = datum2, meseci = as.numeric(format(datumi,format="%m")), pad = data_rr[i,j,1:count1[3]]*24*60*60)
#           data_pad <- data_pad0 %>% filter(meseci >= 4, meseci <= 10)
#           
#           data_et0 <- data.frame(Date = datum2, meseci = as.numeric(format(datumi,format="%m")), et = data_et[i,j,1:count1[3]]*24*60*60)
#           data_etp <- data_et0 %>% filter(meseci >= 4, meseci <= 10)
#           
#           growing_degree_days <- gd4(structure(data_temp$Temp, .Names = data_temp$Date))
#           Growing_season_length <- gsl(structure(data_temp$Temp, .Names = data_temp$Date)) 
#           End_growing_season <- ogs6(structure(data_temp$Temp, .Names = data_temp$Date)) + Growing_season_length + 90
#           Sums_Tmax32 <- stx32(structure(data_tmax$Tmax, .Names = data_tmax$Date)) ###
#           TG_of_warmest_quarter <- bio10(structure(data_temp$Temp, .Names = data_temp$Date)) #Kelvini
#           dtr <- (data_tmax$Tmax+273)/(data_temp$Temp+273)
#           dtr1 <- (data_tmin$Tmin+273)/(data_temp$Temp+273)
#           Diurnal_temp_rangeN <-	dtr(structure(dtr, .Names = data_temp$Date),structure(dtr1, .Names = data_temp$Date))
#           cons_summer_days <- csd(structure(data_tmax$Tmax, .Names = data_tmax$Date))
#           Warm_spell_duration <- wsdi(structure(data_tmax$Tmax, .Names = data_tmax$Date)) ## UREDI
#           Max_consecutive_dry_days <- cdd(structure(data_pad$pad, .Names = data_pad$Date))
#           longest_wet_period <- cwd(structure(data_pad$pad, .Names = data_pad$Date))    
#           r20mm <- r20mm(structure(data_pad$pad, .Names = data_pad$Date))
#           Heavy_prec_days <- d50mm(structure(data_pad$pad, .Names = data_pad$Date)) 
#           SDII <- sdii(structure(data_pad$pad, .Names = data_pad$Date))
#           Prec_wettest_month	<- bio13(structure(data_pad$pad, .Names = data_pad$Date)) 
#           frost_days <- fd(structure(data_tmin$Tmin, .Names = data_tmin$Date))
#           Cold_spell_duration	<- csdi(structure(data_tmin$Tmin, .Names = data_tmin$Date))
#           cons_frost_days <- cfd(structure(data_tmin$Tmin, .Names = data_tmin$Date))
#           ice_days <- id(structure(data_tmax$Tmax, .Names = data_tmax$Date))    
#           Effective_prec	<- ep(structure(data_etp$et, .Names = data_etp$Date), structure(data_pad$pad, .Names = data_pad$Date))
#           Growing_season_prec	<- gsr(structure(data_pad$pad, .Names = data_pad$Date)) ###
#           su <- su(structure(data_tmax$Tmax, .Names = data_tmax$Date))
#           tr <- tr(structure(data_tmin$Tmin, .Names = data_tmin$Date))
#           tn90p <- tn90p(structure(data_tmin$Tmin, .Names = data_tmin$Date)) ## UREDI
#           vwd <- vwd(data = structure(data_tmax$Tmax, .Names = data_tmax$Date)) ## UREDI
#           r95tot <- r95tot(structure(data_pad$pad, .Names = data_pad$Date)) ## UREDI
#           
#           data_wwd <- data.frame(date = data_tmax$Date, Tmean = data_temp$Temp, RR = data_pad$pad)
#           warm_wet_days <- function(data) {
#             data <- data %>% mutate(day_of_year = yday(as.Date(data$date)))
#             percentiles <- data %>%
#               group_by(day_of_year) %>%
#               summarize(Tmean75th = quantile(Tmean, 0.75, na.rm = TRUE),
#                         RR75th = quantile(RR[RR > 0], 0.75, na.rm = TRUE), .groups = "drop")
#             data <- data %>% left_join(percentiles, by = "day_of_year")
#             data <- data %>% mutate(warm_wet = (Tmean > Tmean75th) & (RR > RR75th))
#             data$year <- format(as.POSIXct(data$date, format="%m/%d/%Y"), "%Y")
#             warm_wet_days_count <- data %>%
#               filter(warm_wet) %>%
#               group_by(year) %>%
#               summarize(warm_wet_days = n(),
#                         .groups = "drop")
#             return(warm_wet_days_count)
#           }
#           WWD <- warm_wet_days(data_wwd)
#           temp_day1 <- data.frame(Year = format(as.POSIXct(data_temp$Date, format="%m/%d/%Y"), "%Y"), 
#                                   Month = format(as.POSIXct(data_temp$Date, format="%m/%d/%Y"), "%m"), 
#                                   Day = format(as.POSIXct(data_temp$Date, format="%m/%d/%Y"), "%d"),  
#                                   temp = data_temp$Temp)
#           x<-aggregate(temp ~  Year + Month, temp_day1, FUN = mean, na.rm=TRUE, na.action=na.pass)
#           warmest_m_temp <- x %>% group_by(Year) %>%
#             summarise(max = max(temp))
#           
#           WWD$Year <- WWD$year
#           WWD <- left_join(warmest_m_temp,WWD,by="Year") %>%
#             mutate_if(is.numeric,coalesce,0)
#           
#           df <- data.frame(Date = data_temp$Date, Tmean = data_temp$Temp, Tmin = data_tmin$Tmin)
#           # df$Date <- as.Date(df$Date)
#           calculate_late_frost <- function(df) {
#             df <- df %>% arrange(Date) # Ensure data is sorted by date
#             df$Year <- format(as.POSIXct(df$Date, format="%m/%d/%Y"), "%Y")
#             df$LateFrost <- FALSE
#             years <- unique(df$Year)
#             results_frost <- data.frame(Year = integer(), LateFrostCount = integer())
#             for (current_year in years) {
#               year_data <- df %>% filter(Year == current_year)
#               start_date <- NA        # Find start of 10°C period
#               for (m in 1:(nrow(year_data) - 4)) {
#                 mean_temp_5days <- mean(year_data$Tmean[m:(m + 4)])
#                 if (mean_temp_5days >= 10) {
#                   start_date <- year_data$Date[m + 4] # Use the last day of the 5-day period
#                   break  }  }
#               if (!is.na(start_date)) {
#                 # Check for late frost after the start date
#                 late_frost_days <- year_data %>%
#                   filter(Date > start_date, Tmin <= 0)
#                 if(nrow(late_frost_days) > 0){
#                   df$LateFrost[df$Date %in% late_frost_days$Date] <- TRUE          }
#                 late_frost_count <- nrow(late_frost_days)
#                 results_frost <- rbind(results_frost, data.frame(Year = current_year, LateFrostCount = late_frost_count))
#               } else { 
#                 results_frost <- rbind(results_frost, data.frame(Year = current_year, LateFrostCount = 0))}  #No 5 day period with temp > 10
#             }
#             return(list(df = df, results_frost = results_frost))
#           }
#           late_frost_analysis <- calculate_late_frost(df) # df <- late_frost_analysis$df
#           results_frost <- late_frost_analysis$results_frost
#           
#           
#           flowering_heat_sum <- 703.42
#           maturity_heat_sum <- 1616.8
#           
#           
#           df_heat_stress <- data.frame(Date = data_temp$Date, Tmax = data_tmax$Tmax, temp = data_temp$Temp)
#           calculate_heat_sum <- function(tmax_data) {
#             gdd_daily <- pmax(0, tmax_data$temp - 10)
#             return(cumsum(gdd_daily))
#           }
#           is_heat_stress <- function(tmax_window) {# Function to check for heat stress (2-day period above 35°C)
#             all(tmax_window > 35)
#           }
#           df_heat_stress$Year <- format(as.POSIXct(df_heat_stress$Date, format="%m/%d/%Y"), "%Y")
#           results <- data.frame(Year = unique(df_heat_stress$Year), HeatStressDaysFL = NA, HeatStressDaysMT = NA)
#           for (current_year in unique(df_heat_stress$Year)) {
#             year_data <- subset(df_heat_stress, Year == current_year)
#             year_data <- subset(year_data, temp >=10)
#             year_data$HeatSum <- calculate_heat_sum(year_data)
#             start_dateFL <- min(year_data$Date)
#             end_dateFL <- year_data$Date[which.min(abs(year_data$HeatSum - flowering_heat_sum))]
#             
#             end_date <- year_data$Date[which.min(abs(year_data$HeatSum - maturity_heat_sum))]
#             start_date <- end_dateFL
#             # print(c(start_date,end_date,start_dateFL,end_dateFL))
#             if (!is.na(start_date) && !is.na(end_date)) {  # Only proceed if both dates are found
#               flowering_data <- subset(year_data, Date >= start_dateFL & Date <= end_dateFL) 
#               maturity_data <- subset(year_data, Date >= start_date & Date <= end_date) 
#               if (nrow(flowering_data) >= 2){ # Check if there are at least 2 days of flowering data
#                 heat_stress_eventsFL <- zoo::rollapply(flowering_data$Tmax, width = 2, FUN = is_heat_stress, fill = NA, align = "left")
#                 heat_stress_daysFL <- sum(heat_stress_eventsFL, na.rm = TRUE) * 2
#               }
#               if (nrow(maturity_data) >= 2){ # Check if there are at least 2 days of flowering data
#                 heat_stress_events <- zoo::rollapply(maturity_data$Tmax, width = 2, FUN = is_heat_stress, fill = NA, align = "left")
#                 heat_stress_days <- sum(heat_stress_events, na.rm = TRUE) * 2
#               }
#               else {
#                 heat_stress_days <- 0
#                 heat_stress_daysFL <- 0# No heat stress days if flowering period is less than 2 days
#               }
#             } else {
#               heat_stress_days <- 0
#               heat_stress_daysFL <- 0# Or NA, if you prefer to indicate that flowering period couldn't be determined
#               cat("Flowering period not found for year", current_year, "\n")
#             }
#             results$HeatStressDaysFL[results$Year == current_year] <- heat_stress_daysFL
#             results$HeatStressDaysMT[results$Year == current_year] <- heat_stress_days
#           }
#           
#           rx1day <- rx1day(structure(data_pad$pad, .Names = data_pad$Date)) 
#           rx5d <- rx5d(structure(data_pad$pad, .Names = data_pad$Date)) 
#           
#           temp_day <- data.frame(Year = format(as.POSIXct(data_temp$Date, format="%m/%d/%Y"), "%Y"),
#                                  Month = format(as.POSIXct(data_temp$Date, format="%m/%d/%Y"), "%m"), 
#                                  Day = format(as.POSIXct(data_temp$Date, format="%m/%d/%Y"), "%d"),
#                                  Tmin = data_tmin$Tmin, Tmax = data_tmax$Tmax)
#           climdata <- hourly_temps(temp_day, latitude = lat[i])
#           BEDD0 <- head(GDD_linear(temp_day, Tb = 10, Tu = 30),-1)
#           BEDD <- aggregate(GDD ~  Year, BEDD0, FUN = sum, na.rm=TRUE, na.action=na.pass)[,2]
#           
#           pod_celi <- data.frame(datum = data_temp$Date, 
#                                  leto = format(as.POSIXct(data_temp$Date, format="%m/%d/%Y"), "%Y"), 
#                                  mesec = format(as.POSIXct(data_temp$Date, format="%m/%d/%Y"), "%m"), 
#                                  tg = data_tmax$Tmax)
#           st_dni0 <- pod_celi %>% filter(tg >= 30)
#           st_dni30 <- st_dni0 %>% group_by(leto) %>% summarise(tg = sum(tg-30))
#           warmest_m_temp$leto <- warmest_m_temp$Year
#           Dnevi_30max <- left_join(warmest_m_temp,st_dni30,by="leto") %>%
#             mutate_if(is.numeric,coalesce,0)
#           Dnevi_30max <- Dnevi_30max[,3:4] #Plant heat stress = accumulated daily maximum temperature values above 30?C 
#           
#           st_dni32_anthesis <- pod_celi %>% filter(tg >= 32, mesec >= 5, mesec <= 6) 
#           Dnevi_32max_anthesis <- st_dni32_anthesis %>% group_by(leto) %>% count()
#           Dnevi_32max_anthesis <- left_join(warmest_m_temp[,3],Dnevi_32max_anthesis,by="leto") %>%
#             mutate_if(is.numeric,coalesce,0) # Days with Tmax above 32 ?C
# 
#           podatki_tocka <- data.frame(lats = rep(lat[j], length(growing_degree_days)), 
#                                       lons = rep(lon[i], length(growing_degree_days)), 
#                                       leto = leta1,
#                                       model = rep(model[z], length(growing_degree_days)),
#                                       growing_degree_days, 
#                                       GSL = Growing_season_length, 
#                                       End_growing_season,
#                                       BEDD, 
#                                       Sums_Tmax32, 
#                                       T_warmest_m = warmest_m_temp$max, 
#                                       TG_of_warmest_quarter, 
#                                       Diurnal_temp_rangeN, 
#                                       Heat_stress_fl = results$HeatStressDaysFL, 
#                                       Heat_stress_mat = results$HeatStressDaysMT,
#                                       cons_summer_days, 
#                                       WWD = WWD$warm_wet_days, 
#                                       WSDI = Warm_spell_duration,
#                                       CDD = Max_consecutive_dry_days,#DD = Dry_days, 
#                                       CWD = longest_wet_period, 
#                                       r20mm, 
#                                       Heavy_prec_days,# r10mm, 
#                                       SDII, 
#                                       Prec_wettest_month, #Prec_warmest_quarter, Prec_coldest_quarter,wet_days, 
#                                       FD = frost_days, 
#                                       late_frost_days = results_frost$LateFrostCount,
#                                       CSDI = Cold_spell_duration, 
#                                       CFD = cons_frost_days, 
#                                       ice_days, 
#                                       Effective_prec,
#                                       Growing_season_prec,#Very_wet_days,
#                                       # Nongrowing_season_prec, 
#                                       # precip_total,
#                                       su, 
#                                       tr, 
#                                       tn90p, 
#                                       vwd, 
#                                       rx5d)
#           vsi_podatki <- rbind(vsi_podatki, podatki_tocka)
#         }
#       }    
#       
#     }
#   }
#   saveRDS(povp_obd, file = paste0("period_mean_",rcp,"_",obd,".rds"))
#   return(povp_obd)    
# }
# 
# vsi0 <- risi1(rcp = "rcp45", leto_zac = "2041", leto_zac1 = "2040", obd = "2041-2070")
# vsi1 <- risi1(rcp = "rcp85", leto_zac = "2041", leto_zac1 = "2040", obd = "2041-2070")
# 
# vsi2 <- risi0(rcp = "rcp45", leto_zac = "2071", leto_zac1 = "2070", obd = "2071-2100")
# vsi3 <- risi0(rcp = "rcp85", leto_zac = "2071", leto_zac1 = "2070", obd = "2071-2100")
# 
# # vsi0 <- readRDS(file = paste0("period_mean_",rcp = "rcp45","_",obd = "2041-2070",".rds"))
# # vsi1 <- readRDS(file = paste0("period_mean_",rcp = "rcp85","_",obd = "2041-2070",".rds"))
# # vsi2 <- readRDS(file = paste0("period_mean_",rcp = "rcp45","_",obd = "2071-2100",".rds"))
# # vsi3 <- readRDS(file = paste0("period_mean_",rcp = "rcp85","_",obd = "2071-2100",".rds"))


test <- subset(faktorji_41_70_rcp45, faktorji_41_70_rcp45$lats == lat_Rakican)
test <- subset(test, test$lons == lon_Rakican)
plot(test$leto, test$tn90p)
plot(test$leto, test$WSDI)
plot(test$leto, test$vwd)
# plot(test$leto, test$BEDD)
# plot(test$leto, test$GSL)
plot(test$leto, test$zvrocinski_stres_Vlaga_noc)
plot(test$leto, test$zrastna)

test <- subset(faktorji_71_00_rcp85, faktorji_71_00_rcp85$lats == lat_Rakican)
test <- subset(test, test$lons == lon_Rakican)
plot(test$leto, test$tn90p)
plot(test$leto, test$WSDI)
plot(test$leto, test$vwd)
# plot(test$leto, test$BEDD)
# plot(test$leto, test$GSL)
plot(test$leto, test$zvrocinski_stres_Vlaga_noc)
plot(test$leto, test$zrastna)



test <- subset(vsi_podatki, vsi_podatki$lats == lat_Rakican)
test <- subset(test, test$lons == lon_Rakican)
plot(test$leto, test$tn90p)
plot(test$leto, test$WSDI)
plot(test$leto, test$vwd)
# plot(test$leto, test$BEDD)
# plot(test$leto, test$GSL)
plot(test$leto, test$zvrocinski_stres_Vlaga_noc)
plot(test$leto, test$zrastna)



















