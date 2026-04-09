setwd("C:/Users/ZalaZn/OneDrive - Univerza v Ljubljani/CRP kazalniki 2024") #sluzba
library(ggplot2)
library(ggfortify)
library(gridExtra)
library(carData)
library(car)
library(factoextra)
library(tidyr)
library(corrplot)
library(readxl)
library(randomForest)
library(rpart)
library(haven)
library(ggplot2) 
library(dplyr)
library(psych)
library(scales)

library(ggspatial)
library(sf)
library(viridis)
library(ncdf4)
library(ClimInd)

library(Kendall)
library(patchwork)
library(tidyverse)

# vsi_podatki <- readRDS(file = "kazalniki_OPSI_historical_psenica+koruza.rds")
vsi_podatki <- na.omit(readRDS(file = "kazalniki_OPSI_historical_psenica+koruza.rds")) # tole je za NON GROWING SEASON PREC
id <- seq(1,nrow(vsi_podatki),by=1)
head(vsi_podatki)
summary(vsi_podatki)
vsi_podatki = subset(vsi_podatki, select = -c(End_growing_season,Heat_stress_fl,Heat_stress_mat,chill_portions,year) )

data0 <- data.frame(id, vsi_podatki[,4:36])
write_sav(data0, "delo/kazalniki_psenica.sav")

nuts3_mapdata <- st_read("C:/Users/ZalaZn/OneDrive - Univerza v Ljubljani/DR/NUTS3_ID.gpkg")
slovenia_nuts3_mapdata <- filter(nuts3_mapdata, cntr_code=="SI") # filtriram samo slovenske NUTS3 regije




# vsi_podatki.temp <- subset(vsi_podatki, select = c(growing_degree_days,
#                                                    GSL,
#                                                    BEDD,
#                                                    Sums_Tmax32,
#                                                    T_warmest_m,
#                                                    TG_of_warmest_quarter,
#                                                    TG_of_coldest_quarter,
#                                                    Diurnal_temp_rangeN,
#                                                    Dnevi_32max_anthesis,
#                                                    cons_summer_days,
#                                                    WWD,
#                                                    WSDI,
#                                                    su,
#                                                    tr,
#                                                    tn90p,
#                                                    vwd))
# 
# 
# n_faktorjev <- 3
# 
# # install.packages("GPArotation")
# library(GPArotation)
# # Izvedite faktorsko analizo z rotacijo Oblimin
# fit_fa <- fa(vsi_podatki.temp,
#              nfactors = n_faktorjev,
#              rotate = "oblimin",
#              fm = "pa",
#              scores = "Bartlett") 
# 
# res.pca <- prcomp(vsi_podatki.temp, scale = TRUE)
# print(res.pca)
# summary(res.pca)
# eig.val<-get_eigenvalue(res.pca)
# eig.val
# fviz_eig(res.pca, col.var="blue")
# 
# var <- get_pca_var(res.pca)
# var
# head(var$cos2)
# corrplot(var$cos2, is.corr=FALSE)
# fviz_pca_var(res.pca,
#              col.var = "cos2", # Color by the quality of representation
#              gradient.cols = c("darkorchid4", "gold", "darkorange"),
#              repel = TRUE
# )
# 
# 
# pca_rotated <- principal(r = vsi_podatki.temp,
#                          nfactors = 3,
#                          rotate = "varimax")
# 
# # Prika?ite rezultate
# print(pca_rotated)
# # Matrika Component matrix je shranjena v `$loadings`
# component_matrix <- pca_rotated$loadings
# print(component_matrix)



kaz <- names(vsi_podatki[4:36])
kaz_enote <- c("?C dnevi", "st. dni",
               "st. dni",
               "?C dnevi","?C dnevi",
               "?C", "?C", "",
               "st. dni","st. dni",
               "st. dni","st. dni",
               "st. dni","st. dni",
               "st. dni","st. dni",
               "st. dni","mm","mm",
               "st. dni","st. dni",
               "st. dni","st. dni",
               "st. dni",
               "mm", "mm", #"mm","mm",
               "st. dni","st. dni",
               "% dni","st. dni","mm") # PREVERI ENOTE



calc_z_score <- function(data) {
  mean_data <- mean(data)
  std_dev_data <- sd(data)
  z_score <- (data - mean_data) / std_dev_data
  return(z_score)
}

#DODAMO KAZALNIKE V OSNOVNE PODATKE#
vsi_podatki_org<-vsi_podatki
zac = 1981
kon = 2010
povprecje <- c()
data_z <- vsi_podatki_org
data_z1 <- data.frame()

for(k in seq(1,length(kaz),by=1)){
  data <- data.frame(vsi_podatki$lons, vsi_podatki$lats, vsi_podatki$leto, vsi_podatki[[kaz[k]]])
  colnames(data) <- c("lons","lats","leto","agroclim_ind")
  data$leto <- as.numeric(as.character(data$leto))
  # data <- data %>% filter(leto >= zac, leto <= kon)
  data=na.omit(data)
  min = min(na.omit(vsi_podatki[[kaz[k]]]))
  max = max(na.omit(vsi_podatki[[kaz[k]]]))
  
  kaz_z <- calc_z_score(data$agroclim_ind)
  data$z_score <- kaz_z
  data_z[[kaz[k]]] <- kaz_z
  
  vmesni <- data.frame(lons = data$lons, lats = data$lats, leto = data$leto, kaz_z, kaz = rep(kaz[k], by = length(kaz_z)))
  data_z1 <- rbind(data_z1, vmesni)

}
colnames(data_z)<-paste0("z",colnames(data_z))
colnames(data_z)[c(1,2,3)]<-c("lats","lons","leto")
vsi_podatki<-left_join(vsi_podatki,data_z, by=c("lats","lons","leto"))


###DODAVA POVPRECJA #########
data_z <- vsi_podatki_org
data_z1 <- data.frame()

for(k in seq(1,length(kaz),by=1)){
  data <- data.frame(vsi_podatki$lons, vsi_podatki$lats, vsi_podatki$leto, vsi_podatki[[kaz[k]]])
  colnames(data) <- c("lons","lats","leto","agroclim_ind")
  data$leto <- as.numeric(as.character(data$leto))
  # data <- data %>% filter(leto >= zac, leto <= kon)
  data=na.omit(data)
  min = min(na.omit(vsi_podatki[[kaz[k]]]))
  max = max(na.omit(vsi_podatki[[kaz[k]]]))
  
  kaz_z <- mean(data$agroclim_ind)
  data$z_score <- kaz_z
  data_z[[kaz[k]]] <- kaz_z
  
  vmesni <- data.frame(lons = data$lons, lats = data$lats, leto = data$leto, kaz_z, kaz = rep(kaz[k], by = length(kaz_z)))
  data_z1 <- rbind(data_z1, vmesni)
  
}
colnames(data_z)<-paste0("M",colnames(data_z))
colnames(data_z)[c(1,2,3)]<-c("lats","lons","leto")
vsi_podatki<-left_join(vsi_podatki,data_z, by=c("lats","lons","leto"))

###DODAVA SD #########
data_z <- vsi_podatki_org
data_z1 <- data.frame()

for(k in seq(1,length(kaz),by=1)){
  data <- data.frame(vsi_podatki$lons, vsi_podatki$lats, vsi_podatki$leto, vsi_podatki[[kaz[k]]])
  colnames(data) <- c("lons","lats","leto","agroclim_ind")
  data$leto <- as.numeric(as.character(data$leto))
  # data <- data %>% filter(leto >= zac, leto <= kon)
  data=na.omit(data)
  min = min(na.omit(vsi_podatki[[kaz[k]]]))
  max = max(na.omit(vsi_podatki[[kaz[k]]]))
  
  kaz_z <- sd(data$agroclim_ind)
  data$z_score <- kaz_z
  data_z[[kaz[k]]] <- kaz_z
  
  vmesni <- data.frame(lons = data$lons, lats = data$lats, leto = data$leto, kaz_z, kaz = rep(kaz[k], by = length(kaz_z)))
  data_z1 <- rbind(data_z1, vmesni)
  
}
colnames(data_z)<-paste0("SD",colnames(data_z))
colnames(data_z)[c(1,2,3)]<-c("lats","lons","leto")
vsi_podatki<-left_join(vsi_podatki,data_z, by=c("lats","lons","leto"))

vsi_podatki$leto<-as.numeric(as.character(vsi_podatki$leto))


###IZRACUN FAKTORJEV

vsi_podatki$zprezimovanje_minT<-(vsi_podatki$zcsdi_annual_sum+vsi_podatki$zSums_Tmin10+vsi_podatki$zSums_Tmin15)/3

vsi_podatki$zpozeba<-(vsi_podatki$zCFD + vsi_podatki$zFD + vsi_podatki$zice_days + 
                         vsi_podatki$zlate_frost_days)/4

vsi_podatki$zvrocinski_stres_Vlaga_noc<-(vsi_podatki$zWWD + vsi_podatki$ztn90p)/2

vsi_podatki$zrastna<-(vsi_podatki$zgrowing_degree_days + vsi_podatki$zGSL + vsi_podatki$zBEDD +
                                 vsi_podatki$zTG_of_warmest_quarter + vsi_podatki$zcons_summer_days + 
                                 vsi_podatki$zsu + vsi_podatki$zTG_of_coldest_quarter+ 
                                 vsi_podatki$zT_warmest_m + vsi_podatki$zDiurnal_temp_rangeN)/9

# vsi_podatki$zkonec_rastne<-(vsi_podatki$zGSL+vsi_podatki$zEnd_growing_season)/2
##                           vsi_podatki$zTG_of_coldest_quarter+vsi_podatki$zT_coldest_m)/4

vsi_podatki$zvrocinski_stres <- (vsi_podatki$zSums_Tmax32 + vsi_podatki$zDnevi_32max_anthesis + vsi_podatki$zvwd + vsi_podatki$zWSDI)/4

vsi_podatki$zmin_padavine <- vsi_podatki$zCDD

# vsi_podatki$zvisoke_padavine <- (vsi_podatki$zPrec_wettest_month + vsi_podatki$zr20mm + 
#                                    vsi_podatki$zrx5d + vsi_podatki$zHeavy_prec_days + 
#                                    vsi_podatki$zCWD + vsi_podatki$zSDII)/6 # +
#                                    # vsi_podatki$zEffective_prec + vsi_podatki$zGrowing_season_prec)/8

vsi_podatki$zvisoke_padavine <- (vsi_podatki$zPrec_wettest_month + vsi_podatki$zr20mm + 
                                   vsi_podatki$zrx5d + vsi_podatki$zHeavy_prec_days + 
                                   vsi_podatki$zCWD + vsi_podatki$zSDII +
                                   vsi_podatki$zEffective_prec + vsi_podatki$zGrowing_season_prec + 
                                   vsi_podatki$zNongrowing_season_prec)/9

saveRDS(vsi_podatki, file = "kazalniki_1981-2010_psenica.rds")

# Effective_prec
# Growing_season_prec
# Nongrowing_season_prec
# precip_total
# GSL
# End_growing_season
# TA DVA STA V 4 faktorju temperaturne faktorske, zato ju spustimo

##################################################################################################################################################

# PREVERJANJE KORELACIJ MED KAZALNIKI

data0_cor <- cor(data0)
data0_cor # korelacije izra?unam


pairs(data0[, c(2:4,6:9,11,30)], main = "Temp rastna PC1")
pairs(data0[, c(5,10,13,33)], main = "Temp vro?ina PC2")
pairs(data0[, c(12,32)], main = "Temp vlaga Noc PC3")
pairs(data0[, c(20:21,23:24)], main = "Pozeba PC1")
pairs(data0[, c(22,25:26)], main = "Prezimovanje PC2")
pairs(data0[, c(15:19,27:29,34)], main = "Visoke padavine PC2")


# RISANJE GRAFOV ZA OBE POSTAJI - ?ASOVNE VRSTE FAKTORJEV IN NJIHOVIH KOMPONENT

lat_Jablje <-	46.1875 #46.1414
lat_Rakican <-	46.6875 #46.6504
lon_Jablje <- 14.5625	#14.5561
lon_Rakican <-	16.1875 #16.1966
data_Jablje <- vsi_podatki %>% filter(lats == lat_Jablje, lons == lon_Jablje)
data_Jablje$lok <- rep("Jablje",length(data_Jablje$lats))
data_Rakican <- vsi_podatki %>% filter(lats == lat_Rakican, lons == lon_Rakican)
data_Rakican$lok <- rep("Rakican",length(data_Rakican$lats))

library(boot)
library(patchwork)
acf(data_Jablje$zpozeba, plot = TRUE) # https://medium.com/@kis.andras.nandor/understanding-autocorrelation-and-partial-autocorrelation-functions-acf-and-pacf-2998e7e1bcb5
pacf(data_Jablje$zpozeba, plot = TRUE)
acf(data_Jablje$zvrocinski_stres, plot = TRUE)
pacf(data_Jablje$zvrocinski_stres, plot = TRUE)
acf(data_Jablje$zrastna, plot = TRUE)
pacf(data_Jablje$zrastna, plot = TRUE)
acf(data_Jablje$zvrocinski_stres_Vlaga_noc, plot = TRUE)
pacf(data_Jablje$zvrocinski_stres_Vlaga_noc, plot = TRUE)
acf(data_Jablje$zmin_padavine, plot = TRUE)
pacf(data_Jablje$zmin_padavine, plot = TRUE)
acf(data_Jablje$zvisoke_padavine, plot = TRUE)
pacf(data_Jablje$zvisoke_padavine, plot = TRUE)
acf(data_Rakican$zpozeba, plot = TRUE)
pacf(data_Rakican$zpozeba, plot = TRUE)
acf(data_Rakican$zvrocinski_stres, plot = TRUE)
pacf(data_Rakican$zvrocinski_stres, plot = TRUE)
acf(data_Rakican$zrastna, plot = TRUE)
pacf(data_Rakican$zrastna, plot = TRUE)
acf(data_Rakican$zvrocinski_stres_Vlaga_noc, plot = TRUE)
pacf(data_Rakican$zvrocinski_stres_Vlaga_noc, plot = TRUE)
acf(data_Rakican$zprezimovanje_minT, plot = TRUE)
pacf(data_Rakican$zprezimovanje_minT, plot = TRUE)
acf(data_Rakican$zvisoke_padavine, plot = TRUE)
pacf(data_Rakican$zvisoke_padavine, plot = TRUE)
# ugotovimo, da ni znatnih sezonskih vzorcev in lahko normalno uporabimo MK

MK_pozebaJ <- MannKendall(data_Jablje$zpozeba)
MK_vrocinaJ <- MannKendall(data_Jablje$zvrocinski_stres)
MK_rastnaJ <- MannKendall(data_Jablje$zrastna)
MannKendall(data_Jablje$zprezimovanje_minT)
MK_vrocina_vlaga_nocJ <- MannKendall(data_Jablje$zvrocinski_stres_Vlaga_noc)
MannKendall(data_Jablje$zmin_padavine)
MannKendall(data_Jablje$zvisoke_padavine)

MannKendall(data_Rakican$zpozeba)
MK_vrocinskiR <- MannKendall(data_Rakican$zvrocinski_stres)
MK_rastnaR <- MannKendall(data_Rakican$zrastna)
MannKendall(data_Rakican$zprezimovanje_minT)
MK_vrocina_vlaga_nocR <- MannKendall(data_Rakican$zvrocinski_stres_Vlaga_noc)
MannKendall(data_Rakican$zmin_padavine)
MannKendall(data_Rakican$zvisoke_padavine)

barve = c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2","black", "#999999")

# POZEBA
Jaa2<-ggplot() +
  theme_light(base_size = 17)+
  geom_smooth(data = data_Jablje, aes(x = leto, y = zpozeba, color = "PC"),
              method = "lm",
              se = FALSE, # Plot the line only (without confidence bands)
              fullrange = TRUE # The fit spans the full range of the horizontal axis
  ) +
  geom_point(data = data_Jablje,
             aes(x = leto, y = zice_days,colour= paste("ice days")),size=2)+
  geom_line(data = data_Jablje,
            aes(x = leto, y = zice_days,colour= paste("ice days")),linewidth=1.1)+
  geom_point(data = data_Jablje,
             aes(x = leto, y = zFD, colour=paste("FD")),size=2)+
  geom_line(data = data_Jablje,
            aes(x = leto, y = zFD, colour= paste("FD")),linewidth=1.1)+
  geom_point(data = data_Jablje,
             aes(x = leto, y = zCFD, colour=paste("CFD")),size=2)+
  geom_line(data = data_Jablje,
            aes(x = leto, y = zCFD, colour=paste("CFD")),linewidth=1.1)+
  geom_point(data = data_Jablje,
             aes(x = leto, y = zlate_frost_days, colour=paste("late frost")),size=2)+
  geom_line(data = data_Jablje,
            aes(x = leto, y = zlate_frost_days, colour=paste("late frost")),linewidth=1.1)+
  geom_point(data = data_Jablje,
             aes(x = leto, y = zpozeba, colour= "PC"),size=2)+
  geom_line(data = data_Jablje,
            aes(x = leto, y = zpozeba, colour= "PC"),linewidth=1.2,linetype = "dashed")+
  labs(colour="",title = paste("Jablje",sep="")) +  ylab("z value") + xlab("year") +
  annotate(geom="text", x = 2007, y = 2.8, size = 5, label = paste("tau ==", as.numeric(round(MK_pozebaJ$tau, 3))),
           parse = TRUE, vjust = 0, color ="black") +
  annotate(geom="text", x = 2007, y = 2.8, size = 5, label = paste("p == ",as.numeric(round(MK_pozebaJ$sl,3))),
           parse = TRUE, vjust = 1.5, color ="black") +
  # scale_color_manual(values = barve) +
  scale_color_manual(values = c("light blue","deepskyblue3","darkseagreen3",
                                "forest green","black","navy blue","dark green","light green")) +
  geom_hline(aes(yintercept=0), color="lightgrey", linetype="dashed")+
  scale_y_continuous(breaks=pretty_breaks(4)) + ylim(c(-2,3)) +
  scale_x_continuous(breaks=seq(1980,2010,by=5))+
  # theme_ipsum() +
  guides(colour=guide_legend(nrow=1,byrow=TRUE))+
  theme_minimal()+
  theme(legend.text=element_text(size=17),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 17),
        axis.text.y = element_text(size = 17),
        axis.title.x = element_text(size = 17), axis.title.y = element_text(size = 17),
        legend.position='top')
Jaa2
ggsave(paste("delo/faktorji_psenica/Jablje_pozeba.png"),width = 10, height = 6)

# VROCINSKI STRES GRAF

Jaa3<-ggplot() +
  theme_light(base_size = 17)+
  geom_smooth(data = data_Jablje, aes(x = leto, y = zvrocinski_stres, color = "PC"),
              method = "lm",
              se = FALSE, # Plot the line only (without confidence bands)
              fullrange = TRUE # The fit spans the full range of the horizontal axis
  ) +
  geom_point(data = data_Jablje,
             aes(x = leto, y = zSums_Tmax32, colour=paste("Sums_Tmax32")),size=2)+
  geom_line(data = data_Jablje,
            aes(x = leto, y = zSums_Tmax32, colour=paste("Sums_Tmax32")),linewidth=1.1)+
  geom_point(data = data_Jablje,
             aes(x = leto, y = zDnevi_32max_anthesis,colour= paste("Days_Tmax32_anthesis")),size=2)+
  geom_line(data = data_Jablje,
            aes(x = leto, y = zDnevi_32max_anthesis,colour= paste("Days_Tmax32_anthesis")),linewidth=1.1)+
  geom_point(data = data_Jablje,
             aes(x = leto, y = zvwd, colour=paste("VWD")),size=2)+
  geom_line(data = data_Jablje,
            aes(x = leto, y = zvwd, colour=paste("VWD")),linewidth=1.1)+
  geom_point(data = data_Jablje,
             aes(x = leto, y = zWSDI, colour=paste("WSDI")),size=2)+
  geom_line(data = data_Jablje,
            aes(x = leto, y = zWSDI, colour=paste("WSDI")),linewidth=1.1)+
  geom_point(data = data_Jablje,aes(x = leto, y = zvrocinski_stres, colour= "PC"),size=2)+
  geom_line(data = data_Jablje,aes(x = leto, y = zvrocinski_stres, colour= "PC"),linewidth=1.2,linetype = "dashed")+
  annotate(geom="text", x = 2007, y = 2.8, size = 5, label = paste("tau ==", as.numeric(round(MK_vrocinaJ$tau, 3))),
           parse = TRUE, vjust = 0, color ="black") +
  annotate(geom="text", x = 2007, y = 2.8, size = 5, label = paste("p == ",as.numeric(round(MK_vrocinaJ$sl,3))),
           parse = TRUE, vjust = 1.5, color ="black") +
  labs(colour="",title = paste("Jablje",sep="")) +  ylab("z value") + xlab("year") +
  scale_color_manual(values = c("light blue","black","deepskyblue3","dark green",
                                "darkseagreen3","forest green","navy blue")) +
  geom_hline(aes(yintercept=0), color="lightgrey", linetype="dashed")+
  scale_y_continuous(breaks=pretty_breaks(4),limits = c(-2,5)) +
  scale_x_continuous(breaks=seq(1980,2010,by=5))+
  # theme_ipsum() +
  guides(colour=guide_legend(nrow=2,byrow=TRUE))+
  theme_minimal()+
  theme(legend.text=element_text(size=17),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 17),
        axis.text.y = element_text(size = 17),
        axis.title.x = element_text(size = 17), axis.title.y = element_text(size = 17),
        legend.position='top')
Jaa3
ggsave(paste("delo/faktorji_psenica/Jablje_vrocinski_stres.png"),Jaa3,width = 10, height = 6)


# RASTNA DOBA

Jaa4<-ggplot() +
  theme_light(base_size = 17)+
  geom_smooth(data = data_Jablje, aes(x = leto, y = zrastna, color = "PC"),
    method = "lm",
    se = FALSE, # Plot the line only (without confidence bands)
    fullrange = TRUE # The fit spans the full range of the horizontal axis
  ) +
  geom_point(data = data_Jablje,
             aes(x = leto, y = zgrowing_degree_days, colour=paste("GDD")),size=2)+
  geom_line(data = data_Jablje,
            aes(x = leto, y = zgrowing_degree_days, colour=paste("GDD")),linewidth=1.1)+
  geom_point(data = data_Jablje,
             aes(x = leto, y = zTG_of_warmest_quarter, colour=paste("warmest 3-month T")),size=2)+
  geom_line(data = data_Jablje,
            aes(x = leto, y = zTG_of_warmest_quarter, colour=paste("warmest 3-month T")),linewidth=1.1)+
  geom_point(data = data_Jablje,
             aes(x = leto, y = zBEDD, colour=paste("BEDD")),size=2)+
  geom_line(data = data_Jablje,
            aes(x = leto, y = zBEDD, colour=paste("BEDD")),linewidth=1.1)+
  geom_point(data = data_Jablje,
             aes(x = leto, y = zsu, colour=paste("SU")),size=2)+
  geom_line(data = data_Jablje,
            aes(x = leto, y = zsu, colour=paste("SU")),linewidth=1.1)+
  geom_point(data = data_Jablje,
             aes(x = leto, y = zGSL, colour=paste("GSL")),size=2)+
  geom_line(data = data_Jablje,
            aes(x = leto, y = zGSL, colour=paste("GSL")),linewidth=1.1)+
  geom_point(data = data_Jablje,
             aes(x = leto, y = zTG_of_coldest_quarter, colour=paste("coldest 3-month T")),size=2)+
  geom_line(data = data_Jablje,
            aes(x = leto, y = zTG_of_coldest_quarter, colour=paste("coldest 3-month T")),linewidth=1.1)+
  geom_point(data = data_Jablje,
             aes(x = leto, y = zcons_summer_days, colour=paste("CSU")),size=2)+
  geom_line(data = data_Jablje,
            aes(x = leto, y = zcons_summer_days, colour=paste("CSU")),linewidth=1.1)+
  geom_point(data = data_Jablje,
             aes(x = leto, y = zT_warmest_m, colour=paste("warmest month T")),size=2)+
  geom_line(data = data_Jablje,
            aes(x = leto, y = zT_warmest_m, colour=paste("warmest month T")),linewidth=1.1)+
  geom_point(data = data_Jablje,
             aes(x = leto, y = zDiurnal_temp_rangeN, colour=paste("NDTR")),size=2)+
  geom_line(data = data_Jablje,
            aes(x = leto, y = zDiurnal_temp_rangeN, colour=paste("NDTR")),linewidth=1.1)+
  geom_point(data = data_Jablje,
             aes(x = leto, y = zrastna, colour= "PC"),size=2)+
  geom_line(data = data_Jablje,
            aes(x = leto, y = zrastna, colour= "PC"),linewidth=1.2,linetype = "dashed")+
  annotate(geom="text", x = 2007, y = 2.8, size = 5, label = paste("tau ==", as.numeric(round(MK_rastnaJ$tau, 3))),
           parse = TRUE, vjust = 0, color ="black") +
  annotate(geom="text", x = 2007, y = 2.8, size = 5, label = paste("p == ",as.numeric(round(MK_rastnaJ$sl,3))),
           parse = TRUE, vjust = 1.5, color ="black") +
  labs(colour="",title = paste("Jablje",sep="")) +  ylab("z value") + xlab("year") +
  scale_color_manual(values = c("light green","light blue","deepskyblue3","darkseagreen",
                                "darkseagreen3","dark green","black","cyan4",
                                "yellow","bisque2","darkred")) +
  geom_hline(aes(yintercept=0), color="lightgrey", linetype="dashed")+
  scale_y_continuous(breaks=pretty_breaks(4),limits = c(-2,3)) + 
  scale_x_continuous(breaks=seq(1980,2010,by=5))+
  guides(colour=guide_legend(nrow=2,byrow=TRUE))+
  theme_minimal()+
  theme(legend.text=element_text(size=17),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 17),
        axis.text.y = element_text(size = 17),
        axis.title.x = element_text(size = 17), axis.title.y = element_text(size = 17),
        legend.position='top')
Jaa4
ggsave(paste("delo/faktorji_psenica/Jablje_rastna.png"),width = 10, height = 6)


# PREZIMOVALNE RAZMERE

Jaa5<-ggplot() +
  theme_light(base_size = 17)+
  # geom_smooth(data = data_Jablje, aes(x = leto, y = zprezimovanje_minT, color = "PC"),
  #             method = "lm",
  #             se = FALSE, # Plot the line only (without confidence bands)
  #             fullrange = TRUE # The fit spans the full range of the horizontal axis
  # ) +
  geom_point(data = data_Jablje,
             aes(x = leto, y = zcsdi_annual_sum, colour=paste("CSDI")),size=2)+
  geom_line(data = data_Jablje,
            aes(x = leto, y = zcsdi_annual_sum, colour=paste("CSDI")),linewidth=1.1)+
  geom_point(data = data_Jablje,
             aes(x = leto, y = zSums_Tmin10, colour=paste("Sums_Tmin10")),size=2)+
  geom_line(data = data_Jablje,
            aes(x = leto, y = zSums_Tmin10, colour=paste("Sums_Tmin10")),linewidth=1.1)+
  geom_point(data = data_Jablje,
             aes(x = leto, y = zSums_Tmin15, colour=paste("Sums_Tmin15")),size=2)+
  geom_line(data = data_Jablje,
            aes(x = leto, y = zSums_Tmin15, colour=paste("Sums_Tmin15")),linewidth=1.1)+
  geom_point(data = data_Jablje,
             aes(x = leto, y = zrastna, colour= "PC"),size=2)+
  geom_line(data = data_Jablje,
            aes(x = leto, y = zrastna, colour= "PC"),linewidth=1.2,linetype="dashed")+
  labs(colour="",title = paste("Jablje",sep="")) + ylab("") +
  xlab("year") +  scale_color_manual(values = c("light green","black","light blue","deepskyblue3","darkseagreen",
                                                "darkseagreen3","dark green","cyan4",
                                                "yellow","bisque2","darkred")) +
  geom_hline(aes(yintercept=0), color="lightgrey", linetype="dashed")+
  scale_y_continuous(breaks=pretty_breaks(4), limits=c(-2,6)) + 
  scale_x_continuous(breaks=seq(1980,2010,by=5))+
  # guides(colour=guide_legend(nrow=2,byrow=TRUE))+
  theme_minimal()+
  theme(legend.text=element_text(size=17),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 17),
        axis.text.y = element_text(size = 17),
        axis.title.x = element_text(size = 17), axis.title.y = element_text(size = 17),
        legend.position='top')
Jaa5
ggsave(paste("delo/faktorji_psenica/Jablje_prezimovalne.png"),width = 10, height = 6)

# padavinski minimumi
Jaa6<-ggplot() +
  theme_light(base_size = 17)+
  geom_point(data = data_Jablje,
             aes(x = leto, y = zCDD, colour=paste("CDD")),size=1.75)+
  geom_line(data = data_Jablje,
            aes(x = leto, y = zCDD, colour=paste("CDD")),linewidth=1.5)+
  geom_point(data = data_Jablje,
             aes(x = leto, y = zmin_padavine, colour= "PC"),size=2)+
  geom_line(data = data_Jablje,
            aes(x = leto, y = zmin_padavine, colour= "PC"),linewidth=1.2,linetype = "dashed")+
  labs(colour="",title = paste("Jablje",sep="")) +  ylab("z value") + xlab("year") +
  scale_color_manual(values = c("light blue","black","deepskyblue3",
                                "darkseagreen3","dark green")) +
  geom_hline(aes(yintercept=0), color="lightgrey", linetype="dashed")+
  scale_y_continuous(breaks=pretty_breaks(4),limits = c(-2,4)) +
  scale_x_continuous(breaks=seq(1980,2010,by=5))+
  guides(colour=guide_legend(nrow=1,byrow=TRUE))+
  theme_minimal()+
  theme(legend.text=element_text(size=17),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 17),
        axis.text.y = element_text(size = 17),
        axis.title.x = element_text(size = 17), axis.title.y = element_text(size = 17),
        legend.position='top')
Jaa6
ggsave(paste("delo/faktorji_psenica/Jablje_min_padavine.png"),width = 10, height = 6)

# vrocinski_stres_Vlaga_noc
Jaa7<-ggplot() +
  geom_smooth(data = data_Jablje, aes(x = leto, y = zvrocinski_stres_Vlaga_noc, color = "PC"),
              method = "lm",
              se = FALSE, # Plot the line only (without confidence bands)
              fullrange = TRUE # The fit spans the full range of the horizontal axis
  ) +
  theme_light(base_size = 17)+
  geom_point(data = data_Jablje,
             aes(x = leto, y = ztn90p,colour= paste("warm nights")),size=2)+
  geom_line(data = data_Jablje,
            aes(x = leto, y = ztn90p,colour= paste("warm nights")),linewidth=1.1)+
  geom_line(data = data_Jablje,
            aes(x = leto, y = zWWD, colour=paste("WWD")),linewidth=1.1)+
  geom_point(data = data_Jablje,
             aes(x = leto, y = zWWD,colour= paste("WWD")),size=2)+
  geom_point(data = data_Jablje,
             aes(x = leto, y = zvrocinski_stres_Vlaga_noc, colour= "PC"),size=2)+
  geom_line(data = data_Jablje,
            aes(x = leto, y = zvrocinski_stres_Vlaga_noc, colour= "PC"),linewidth=1.2,linetype = "dashed")+
  annotate(geom="text", x = 2007, y = 3.5, size = 5, label = paste("tau ==", as.numeric(round(MK_vrocina_vlaga_nocJ$tau, 3))),
           parse = TRUE, vjust = 0, color ="black") +
  annotate(geom="text", x = 2007, y = 3.5, size = 5, label = paste("p == ",as.numeric(round(MK_vrocina_vlaga_nocJ$sl,3))),
           parse = TRUE, vjust = 1.5, color ="black") +
  labs(colour="",title = paste("Jablje",sep="")) +  ylab("z value") + xlab("year") +
  scale_color_manual(values = c("black","light blue","deepskyblue3",
                                "darkseagreen3","dark green")) +
  geom_hline(aes(yintercept=0), color="lightgrey", linetype="dashed")+
  scale_y_continuous(breaks=pretty_breaks(4),limits = c(-2,4)) +
  scale_x_continuous(breaks=seq(1980,2010,by=5))+
  guides(colour=guide_legend(nrow=1,byrow=TRUE))+
  theme_minimal()+
  theme(legend.text=element_text(size=17),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 17),
        axis.text.y = element_text(size = 17),
        axis.title.x = element_text(size = 17), axis.title.y = element_text(size = 17),
        legend.position='top')
Jaa7
ggsave(paste("delo/faktorji_psenica/Jablje_vrocinski_stres_Vlaga_noc.png"),width = 10, height = 6)



# visoke padavine

Jaa8<-ggplot() +
  theme_light(base_size = 17)+
  geom_point(data = data_Jablje,
             aes(x = leto, y = zPrec_wettest_month,colour= paste("wettest month precip.")),size=2)+
  geom_line(data = data_Jablje,
            aes(x = leto, y = zPrec_wettest_month,colour= paste("wettest month precip.")),linewidth=1.1)+
  geom_point(data = data_Jablje,
             aes(x = leto, y = zr20mm,colour= paste("R20mm")),size=2)+
  geom_line(data = data_Jablje,
            aes(x = leto, y = zr20mm,colour= paste("R20mm")),linewidth=1.1)+
  geom_point(data = data_Jablje,
             aes(x = leto, y = zrx5d,colour= paste("Rx5day")),size=2)+
  geom_line(data = data_Jablje,
            aes(x = leto, y = zrx5d,colour= paste("Rx5day")),linewidth=1.1)+
  geom_point(data = data_Jablje,
             aes(x = leto, y = zHeavy_prec_days, colour=paste("R50mm")),size=2)+
  geom_line(data = data_Jablje,
            aes(x = leto, y = zHeavy_prec_days, colour=paste("R50mm")),linewidth=1.1)+
  geom_point(data = data_Jablje,
             aes(x = leto, y = zCWD, colour=paste("CWD")),size=2)+
  geom_line(data = data_Jablje,
            aes(x = leto, y = zCWD, colour=paste("CWD")),linewidth=1.1)+
  geom_point(data = data_Jablje,
             aes(x = leto, y = zSDII,colour= paste("SDII")),size=2)+
  geom_line(data = data_Jablje,
            aes(x = leto, y = zSDII,colour= paste("SDII")),linewidth=1.1)+
  geom_point(data = data_Jablje,
             aes(x = leto, y = zEffective_prec,colour= paste("EP")),size=2)+
  geom_line(data = data_Jablje,
            aes(x = leto, y = zEffective_prec,colour= paste("EP")),linewidth=1.1)+
  geom_point(data = data_Jablje,
             aes(x = leto, y = zGrowing_season_prec,colour= paste("GS precip.")),size=2)+
  geom_line(data = data_Jablje,
            aes(x = leto, y = zGrowing_season_prec,colour= paste("GS precip.")),linewidth=1.1)+
  geom_point(data = data_Jablje,
             aes(x = leto, y = zNongrowing_season_prec,colour= paste("Non-GS precip.")),size=2)+
  geom_line(data = data_Jablje,
            aes(x = leto, y = zNongrowing_season_prec,colour= paste("Non-GS precip.")),linewidth=1.1)+
  geom_point(data = data_Jablje,
             aes(x = leto, y = zvisoke_padavine, colour= "PC"),size=2)+
  geom_line(data = data_Jablje,
            aes(x = leto, y = zvisoke_padavine, colour= "PC"),linewidth=1.2,linetype = "dashed")+
  labs(colour="",title = paste("Jablje",sep="")) + ylab("z value") + xlab("year") +
  scale_color_manual(values = c("light blue","deepskyblue3","dark green",
                                "darkseagreen3","black","forest green",
                                "darkseagreen","skyblue4","yellow","bisque2")) +
  geom_hline(aes(yintercept=0), color="lightgrey", linetype="dashed")+
  scale_y_continuous(breaks=pretty_breaks(4),limits = c(-2,3)) +
  scale_x_continuous(breaks=seq(1980,2010,by=5))+
  # theme_ipsum() +
  guides(colour=guide_legend(nrow=2,byrow=TRUE))+
  theme_minimal()+
  theme(legend.text=element_text(size=17),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 17),
        axis.text.y = element_text(size = 17),
        axis.title.x = element_text(size = 17), axis.title.y = element_text(size = 17),
        legend.position='top')
Jaa8
ggsave(paste("delo/faktorji_psenica/Jablje_visoke_padavine.png"),width = 10, height = 6)



#### RAKICAN

# POZEBA

Raa2<-ggplot() +
  theme_light(base_size = 17)+
  geom_point(data = data_Rakican,
             aes(x = leto, y = zice_days,colour= paste("ice days")),size=2)+
  geom_line(data = data_Rakican,
            aes(x = leto, y = zice_days,colour= paste("ice days")),linewidth=1.1)+
  geom_point(data = data_Rakican,
             aes(x = leto, y = zFD, colour=paste("FD")),size=2)+
  geom_line(data = data_Rakican,
            aes(x = leto, y = zFD, colour= paste("FD")),linewidth=1.1)+
  geom_point(data = data_Rakican,
             aes(x = leto, y = zCFD, colour=paste("CFD")),size=2)+
  geom_line(data = data_Rakican,
            aes(x = leto, y = zCFD, colour=paste("CFD")),linewidth=1.1)+
  geom_point(data = data_Rakican,
             aes(x = leto, y = zlate_frost_days, colour=paste("late frost")),size=2)+
  geom_line(data = data_Rakican,
            aes(x = leto, y = zlate_frost_days, colour=paste("late frost")),linewidth=1.1)+
  geom_point(data = data_Rakican,
             aes(x = leto, y = zpozeba, colour= "PC"),size=2)+
  geom_line(data = data_Rakican,
            aes(x = leto, y = zpozeba, colour= "PC"),linewidth=1.2,linetype="dashed")+
  labs(colour="",title = paste("Rakičan",sep="")) +  ylab("") +
  xlab("year") +  scale_color_manual(values = c("light blue","deepskyblue3","darkseagreen3",
                                "forest green","black","navy blue","dark green","light green")) +
  geom_hline(aes(yintercept=0), color="lightgrey", linetype="dashed")+
  scale_y_continuous(breaks=pretty_breaks(4)) + ylim(c(-2,3)) +
  scale_x_continuous(breaks=seq(1980,2010,by=5))+
  # theme_ipsum() +
  guides(colour=guide_legend(nrow=1,byrow=TRUE))+
  theme_minimal()+
  theme(legend.text=element_text(size=17),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 17),
        axis.text.y = element_text(size = 17),
        axis.title.x = element_text(size = 17), axis.title.y = element_text(size = 17),
        legend.position='top')
Raa2
ggsave(paste("delo/faktorji_psenica/Rakican_pozeba.png"),width = 10, height = 6)

# VROCINSKI STRES GRAF

Raa3<-ggplot() +
  theme_light(base_size = 17)+
  geom_smooth(data = data_Rakican, aes(x = leto, y = zvrocinski_stres, color = "PC"),
              method = "lm",
              se = FALSE, # Plot the line only (without confidence bands)
              fullrange = TRUE # The fit spans the full range of the horizontal axis
  ) +
  geom_point(data = data_Rakican,
             aes(x = leto, y = zSums_Tmax32, colour=paste("Sums_Tmax32")),size=2)+
  geom_line(data = data_Rakican,
            aes(x = leto, y = zSums_Tmax32, colour=paste("Sums_Tmax32")),linewidth=1.1)+
  geom_point(data = data_Rakican,
             aes(x = leto, y = zDnevi_32max_anthesis,colour= paste("Days_Tmax32_anthesis")),size=2)+
  geom_line(data = data_Rakican,
            aes(x = leto, y = zDnevi_32max_anthesis,colour= paste("Days_Tmax32_anthesis")),linewidth=1.1)+
  geom_point(data = data_Rakican,
             aes(x = leto, y = zvwd, colour=paste("VWD")),size=2)+
  geom_line(data = data_Rakican,
            aes(x = leto, y = zvwd, colour=paste("VWD")),linewidth=1.1)+
  geom_point(data = data_Rakican,
             aes(x = leto, y = zWSDI, colour=paste("WSDI")),size=2)+
  geom_line(data = data_Rakican,
            aes(x = leto, y = zWSDI, colour=paste("WSDI")),linewidth=1.1)+
  geom_point(data = data_Rakican,aes(x = leto, y = zvrocinski_stres, colour= "PC"),size=2)+
  geom_line(data = data_Rakican,aes(x = leto, y = zvrocinski_stres, colour= "PC"),linewidth=1.2,linetype="dashed")+
  annotate(geom="text", x = 1983, y = 4, size = 5, label = paste("tau ==", as.numeric(round(MK_vrocinskiR$tau, 3))),
           parse = TRUE, vjust = 0, color ="black") +
  annotate(geom="text", x = 1983, y = 4, size = 5, label = paste("p == ",as.numeric(round(MK_vrocinskiR$sl,3))),
           parse = TRUE, vjust = 1.5, color ="black") +
  labs(colour="",title = paste("Rakičan",sep="")) + ylab("") +
  xlab("year") +  scale_color_manual(values = c("light blue","black","deepskyblue3","dark green",
                                "darkseagreen3","forest green","navy blue")) +
  geom_hline(aes(yintercept=0), color="lightgrey", linetype="dashed")+
  scale_y_continuous(breaks=pretty_breaks(4),limits = c(-2,5)) +
  scale_x_continuous(breaks=seq(1980,2010,by=5))+
  # theme_ipsum() +
  # guides(colour=guide_legend(nrow=2,byrow=TRUE))+
  theme_minimal()+
  theme(legend.text=element_text(size=17),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 17),
        axis.text.y = element_text(size = 17),
        axis.title.x = element_text(size = 17), axis.title.y = element_text(size = 17),
        legend.position='top')
Raa3
ggsave(paste("delo/faktorji_psenica/Rakican_vrocinski_stres.png"),Raa3,width = 10, height = 6)


# RASTNA DOBA

Raa4<-ggplot() +
  theme_light(base_size = 17)+
  geom_smooth(data = data_Rakican, aes(x = leto, y = zrastna, color = "PC"),
              method = "lm",
              se = FALSE, # Plot the line only (without confidence bands)
              fullrange = TRUE # The fit spans the full range of the horizontal axis
  ) +
  geom_point(data = data_Rakican,
             aes(x = leto, y = zgrowing_degree_days, colour=paste("GDD")),size=2)+
  geom_line(data = data_Rakican,
            aes(x = leto, y = zgrowing_degree_days, colour=paste("GDD")),linewidth=1.1)+
  geom_point(data = data_Rakican,
             aes(x = leto, y = zTG_of_warmest_quarter, colour=paste("warmest 3-month T")),size=2)+
  geom_line(data = data_Rakican,
            aes(x = leto, y = zTG_of_warmest_quarter, colour=paste("warmest 3-month T")),linewidth=1.1)+
  geom_point(data = data_Rakican,
             aes(x = leto, y = zTG_of_coldest_quarter, colour=paste("coldest 3-month T")),size=2)+
  geom_line(data = data_Rakican,
            aes(x = leto, y = zTG_of_coldest_quarter, colour=paste("coldest 3-month T")),linewidth=1.1)+
  geom_point(data = data_Rakican,
             aes(x = leto, y = zBEDD, colour=paste("BEDD")),size=2)+
  geom_line(data = data_Rakican,
            aes(x = leto, y = zBEDD, colour=paste("BEDD")),linewidth=1.1)+
  geom_point(data = data_Rakican,
             aes(x = leto, y = zsu, colour=paste("SU")),size=2)+
  geom_line(data = data_Rakican,
            aes(x = leto, y = zsu, colour=paste("SU")),linewidth=1.1)+
  geom_point(data = data_Rakican,
             aes(x = leto, y = zGSL, colour=paste("GSL")),size=2)+
  geom_line(data = data_Rakican,
            aes(x = leto, y = zGSL, colour=paste("GSL")),linewidth=1.1)+
  geom_point(data = data_Rakican,
             aes(x = leto, y = zcons_summer_days, colour=paste("CSU")),size=2)+
  geom_line(data = data_Rakican,
            aes(x = leto, y = zcons_summer_days, colour=paste("CSU")),linewidth=1.1)+
  geom_point(data = data_Rakican,
             aes(x = leto, y = zT_warmest_m, colour=paste("warmest month T")),size=2)+
  geom_line(data = data_Rakican,
            aes(x = leto, y = zT_warmest_m, colour=paste("warmest month T")),linewidth=1.1)+
  geom_point(data = data_Rakican,
             aes(x = leto, y = zDiurnal_temp_rangeN, colour=paste("NDTR")),size=2)+
  geom_line(data = data_Rakican,
            aes(x = leto, y = zDiurnal_temp_rangeN, colour=paste("NDTR")),linewidth=1.1)+
  geom_point(data = data_Rakican,
             aes(x = leto, y = zrastna, colour= "PC"),size=2)+
  geom_line(data = data_Rakican,
            aes(x = leto, y = zrastna, colour= "PC"),linewidth=1.2,linetype="dashed")+
  annotate(geom="text", x = 2007, y = 2.7, size = 5, label = paste("tau ==", as.numeric(round(MK_rastnaR$tau, 3))),
           parse = TRUE, vjust = 0, color ="black") +
  annotate(geom="text", x = 2007, y = 2.7, size = 5, label = paste("p == ",as.numeric(round(MK_rastnaR$sl,3))),
           parse = TRUE, vjust = 1.5, color ="black") +
  labs(colour="",title = paste("Rakičan",sep="")) + ylab("") +
  xlab("year") +  scale_color_manual(values = c("light green","light blue","deepskyblue3","darkseagreen",
                                "darkseagreen3","dark green","black","cyan4",
                                "yellow","bisque2","darkred")) +
  geom_hline(aes(yintercept=0), color="lightgrey", linetype="dashed")+
  scale_y_continuous(breaks=pretty_breaks(4), limits=c(-2,3)) + 
  scale_x_continuous(breaks=seq(1980,2010,by=5))+
  # guides(colour=guide_legend(nrow=2,byrow=TRUE))+
  theme_minimal()+
  theme(legend.text=element_text(size=17),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 17),
        axis.text.y = element_text(size = 17),
        axis.title.x = element_text(size = 17), axis.title.y = element_text(size = 17),
        legend.position='top')
Raa4
ggsave(paste("delo/faktorji_psenica/Rakican_rastna.png"),width = 10, height = 6)

# PREZIMOVALNE RAZMERE

Raa5<-ggplot() +
  theme_light(base_size = 17)+
  # geom_smooth(data = data_Rakican, aes(x = leto, y = zprezimovanje_minT, color = "PC"),
  #             method = "lm",
  #             se = FALSE, # Plot the line only (without confidence bands)
  #             fullrange = TRUE # The fit spans the full range of the horizontal axis
  # ) +
  geom_point(data = data_Rakican,
             aes(x = leto, y = zcsdi_annual_sum, colour=paste("CSDI")),size=2)+
  geom_line(data = data_Rakican,
            aes(x = leto, y = zcsdi_annual_sum, colour=paste("CSDI")),linewidth=1.1)+
  geom_point(data = data_Rakican,
             aes(x = leto, y = zSums_Tmin10, colour=paste("Sums_Tmin10")),size=2)+
  geom_line(data = data_Rakican,
            aes(x = leto, y = zSums_Tmin10, colour=paste("Sums_Tmin10")),linewidth=1.1)+
  geom_point(data = data_Rakican,
             aes(x = leto, y = zSums_Tmin15, colour=paste("Sums_Tmin15")),size=2)+
  geom_line(data = data_Rakican,
            aes(x = leto, y = zSums_Tmin15, colour=paste("Sums_Tmin15")),linewidth=1.1)+
  geom_point(data = data_Rakican,
             aes(x = leto, y = zrastna, colour= "PC"),size=2)+
  geom_line(data = data_Rakican,
            aes(x = leto, y = zrastna, colour= "PC"),linewidth=1.2,linetype="dashed")+
  labs(colour="",title = paste("Rakičan",sep="")) + ylab("") +
  xlab("year") +  scale_color_manual(values = c("light green","black","light blue","deepskyblue3","darkseagreen",
                                                "darkseagreen3","dark green","cyan4",
                                                "yellow","bisque2","darkred")) +
  geom_hline(aes(yintercept=0), color="lightgrey", linetype="dashed")+
  scale_y_continuous(breaks=pretty_breaks(4), limits=c(-2,6)) + 
  scale_x_continuous(breaks=seq(1980,2010,by=5))+
  # guides(colour=guide_legend(nrow=2,byrow=TRUE))+
  theme_minimal()+
  theme(legend.text=element_text(size=17),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 17),
        axis.text.y = element_text(size = 17),
        axis.title.x = element_text(size = 17), axis.title.y = element_text(size = 17),
        legend.position='top')
Raa5
ggsave(paste("delo/faktorji_psenica/Rakican_prezimovalne.png"),width = 10, height = 6)

# padavinski minimumi
Raa6<-ggplot() +
  theme_light(base_size = 17)+
  geom_point(data = data_Rakican,
             aes(x = leto, y = zCDD, colour=paste("CDD")),size=1.75)+
  geom_line(data = data_Rakican,
            aes(x = leto, y = zCDD, colour=paste("CDD")),linewidth=1.5)+
  geom_point(data = data_Rakican,
             aes(x = leto, y = zmin_padavine, colour= "PC"),size=2)+
  geom_line(data = data_Rakican,
            aes(x = leto, y = zmin_padavine, colour= "PC"),linewidth=1.2,linetype="dashed")+
  labs(colour="",title = paste("Rakičan",sep="")) + ylab("") +
  xlab("year") +
  scale_color_manual(values = c("light blue","black","deepskyblue3",
                                "darkseagreen3","dark green")) +
  geom_hline(aes(yintercept=0), color="lightgrey", linetype="dashed")+
  scale_y_continuous(breaks=pretty_breaks(4),limits = c(-2,4)) +
  scale_x_continuous(breaks=seq(1980,2010,by=5))+
  # guides(colour=guide_legend(nrow=1,byrow=TRUE))+
  theme_minimal()+
  theme(legend.text=element_text(size=17),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 17),
        axis.text.y = element_text(size = 17),
        axis.title.x = element_text(size = 17), axis.title.y = element_text(size = 17),
        legend.position='top')
Raa6
ggsave(paste("delo/faktorji_psenica/Rakican_min_padavine.png"),width = 10, height = 6)

# Vrocina vlaga Noc
Raa7<-ggplot() +
  theme_light(base_size = 17)+
  geom_smooth(data = data_Rakican, aes(x = leto, y = zvrocinski_stres_Vlaga_noc, color = "PC"),
              method = "lm",
              se = FALSE, # Plot the line only (without confidence bands)
              fullrange = TRUE # The fit spans the full range of the horizontal axis
  ) +
  geom_point(data = data_Rakican,
             aes(x = leto, y = ztn90p,colour= paste("warm nights")),size=2)+
  geom_line(data = data_Rakican,
            aes(x = leto, y = ztn90p,colour= paste("warm nights")),linewidth=1.1)+
  geom_point(data = data_Rakican,
             aes(x = leto, y = zWWD, colour=paste("WWD")),size=2)+
  geom_line(data = data_Rakican,
            aes(x = leto, y = zWWD, colour=paste("WWD")),linewidth=1.1)+
  geom_point(data = data_Rakican,
             aes(x = leto, y = zvrocinski_stres_Vlaga_noc, colour= "PC"),size=2)+
  geom_line(data = data_Rakican,
            aes(x = leto, y = zvrocinski_stres_Vlaga_noc, colour= "PC"),linewidth=1.2,linetype="dashed")+
  annotate(geom="text", x = 2007, y = 3.5, size = 5, label = paste("tau ==", as.numeric(round(MK_vrocina_vlaga_nocR$tau, 3))),
           parse = TRUE, vjust = 0, color ="black") +
  annotate(geom="text", x = 2007, y = 3.5, size = 5, label = paste("p == ",as.numeric(round(MK_vrocina_vlaga_nocR$sl,3))),
           parse = TRUE, vjust = 1.5, color ="black") +
  labs(colour="",title = paste("Rakičan",sep="")) + ylab("") +
  xlab("year") +
  scale_color_manual(values = c("black","light blue","deepskyblue3",
                                "darkseagreen3","dark green")) +
  geom_hline(aes(yintercept=0), color="lightgrey", linetype="dashed")+
  scale_y_continuous(breaks=pretty_breaks(4),limits = c(-2,4)) +
  scale_x_continuous(breaks=seq(1980,2010,by=5))+
  # guides(colour=guide_legend(nrow=1,byrow=TRUE))+
  theme_minimal()+
  theme(legend.text=element_text(size=17),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 17),
        axis.text.y = element_text(size = 17),
        axis.title.x = element_text(size = 17), axis.title.y = element_text(size = 17),
        legend.position='top')
Raa7
ggsave(paste("delo/faktorji_psenica/Rakican_vrocinski_stres_Vlaga_noc.png"),width = 10, height = 6)



# visoke padavine
Raa8<-ggplot() +
  theme_light(base_size = 17)+
  geom_point(data = data_Rakican,
             aes(x = leto, y = zPrec_wettest_month,colour= paste("wettest month precip.")),size=2)+
  geom_line(data = data_Rakican,
            aes(x = leto, y = zPrec_wettest_month,colour= paste("wettest month precip.")),linewidth=1.1)+
  geom_point(data = data_Rakican,
             aes(x = leto, y = zr20mm,colour= paste("R20mm")),size=2)+
  geom_line(data = data_Rakican,
            aes(x = leto, y = zr20mm,colour= paste("R20mm")),linewidth=1.1)+
  geom_point(data = data_Rakican,
             aes(x = leto, y = zrx5d,colour= paste("Rx5day")),size=2)+
  geom_line(data = data_Rakican,
            aes(x = leto, y = zrx5d,colour= paste("Rx5day")),linewidth=1.1)+
  geom_point(data = data_Rakican,
             aes(x = leto, y = zHeavy_prec_days, colour=paste("R50mm")),size=2)+
  geom_line(data = data_Rakican,
            aes(x = leto, y = zHeavy_prec_days, colour=paste("R50mm")),linewidth=1.1)+
  geom_point(data = data_Rakican,
             aes(x = leto, y = zCWD, colour=paste("CWD")),size=2)+
  geom_line(data = data_Rakican,
            aes(x = leto, y = zCWD, colour=paste("CWD")),linewidth=1.1)+
  geom_point(data = data_Rakican,
             aes(x = leto, y = zSDII,colour= paste("SDII")),size=2)+
  geom_line(data = data_Rakican,
            aes(x = leto, y = zSDII,colour= paste("SDII")),linewidth=1.1)+
  geom_point(data = data_Rakican,
             aes(x = leto, y = zEffective_prec,colour= paste("EP")),size=2)+
  geom_line(data = data_Rakican,
            aes(x = leto, y = zEffective_prec,colour= paste("EP")),linewidth=1.1)+
  geom_point(data = data_Rakican,
             aes(x = leto, y = zGrowing_season_prec,colour= paste("GS precip.")),size=2)+
  geom_line(data = data_Rakican,
            aes(x = leto, y = zGrowing_season_prec,colour= paste("GS precip.")),linewidth=1.1)+
  geom_point(data = data_Rakican,
             aes(x = leto, y = zNongrowing_season_prec,colour= paste("Non-GS precip.")),size=2)+
  geom_line(data = data_Rakican,
            aes(x = leto, y = zNongrowing_season_prec,colour= paste("Non-GS precip.")),linewidth=1.1)+
  geom_point(data = data_Rakican,
             aes(x = leto, y = zvisoke_padavine, colour= "PC"),size=2)+
  geom_line(data = data_Rakican,
            aes(x = leto, y = zvisoke_padavine, colour= "PC"),linewidth=1.2,linetype="dashed")+
  labs(colour="",title = paste("Rakičan",sep="")) + ylab("") +
  xlab("year") +
  scale_color_manual(values = c("light blue","deepskyblue3","dark green",
                                "darkseagreen3","black","forest green","darkseagreen","skyblue4","yellow","bisque2")) +
  geom_hline(aes(yintercept=0), color="lightgrey", linetype="dashed")+
  scale_y_continuous(breaks=pretty_breaks(5),limits = c(-2,3)) +
  scale_x_continuous(breaks=seq(1980,2010,by=5))+
  # theme_ipsum() +
  # guides(colour=guide_legend(nrow=2,byrow=TRUE))+
  theme_minimal()+
  theme(legend.text=element_text(size=17),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 17),
        axis.text.y = element_text(size = 17),
        axis.title.x = element_text(size = 17), axis.title.y = element_text(size = 17),
        legend.position='top')
Raa8
ggsave(paste("delo/faktorji_psenica/Rakican_visoke_padavine.png"),width = 10, height = 6)


# my_colors <- c("light blue", "deepskyblue3", "darkseagreen3", "forest green", "black", "navy blue", "dark green", "light green")
# my_labels <- c("ice days", "FD", "CFD", "late frost", "PC")
# data_Jablje_long <- data_Jablje %>%
#   pivot_longer(cols = c(zice_days, zFD, zCFD, zlate_frost_days, zpozeba),
#     names_to = "faktor", values_to = "z_value")
# data_Rakican_long <- data_Rakican %>%
#   pivot_longer(cols = c(zice_days, zFD, zCFD, zlate_frost_days, zpozeba),
#     names_to = "faktor", values_to = "z_value")
# factor_levels <- c("zice_days", "zFD", "zCFD", "zlate_frost_days", "zpozeba")
# data_Jablje_long$faktor <- factor(data_Jablje_long$faktor, levels = factor_levels)
# data_Rakican_long$faktor <- factor(data_Rakican_long$faktor, levels = factor_levels)
# 
# Jaa2 <- ggplot(data_Jablje_long, aes(x = leto, y = z_value, color = faktor)) +
#   geom_point(size = 2) +
#   geom_line(size = 1.1) +
#   geom_smooth(data = data_Jablje %>% select(leto, zpozeba) %>% drop_na(),
#               aes(x = leto, y = zpozeba, color = "zpozeba"), 
#               method = "lm", se = FALSE, fullrange = TRUE) +
#   labs(colour = "", title = "Jablje", y = "z value", x = "year") +
#   annotate(geom="text", x = 2007, y = 2.8, size = 5, label = "tau", parse = TRUE, vjust = 0, color ="black") +
#   annotate(geom="text", x = 2007, y = 2.8, size = 5, label = "p", parse = TRUE, vjust = 1.5, color ="black") +
#   scale_color_manual(values = my_colors, labels = my_labels) +
#   geom_hline(yintercept = 0, color = "lightgrey", linetype = "dashed") +
#   scale_y_continuous(breaks = scales::pretty_breaks(4), limits = c(-2, 3)) +
#   scale_x_continuous(breaks = seq(1980, 2010, by = 5)) +
#   guides(colour = guide_legend(nrow = 1, byrow = TRUE)) +
#   theme_minimal(base_size = 17) +
#   theme(legend.position = 'top',
#         axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 17))
# 
# 
# Raa2 <- ggplot(data_Rakican_long, aes(x = leto, y = z_value, color = faktor)) +
#   geom_point(size = 2) +
#   geom_line(size = 1.1) +
#   labs(colour = "", title = "Rakičan", y = "", x = "year") +
#   scale_color_manual(values = my_colors, labels = my_labels) +
#   geom_hline(yintercept = 0, color = "lightgrey", linetype = "dashed") +
#   scale_y_continuous(breaks = scales::pretty_breaks(4), limits = c(-2, 3)) +
#   scale_x_continuous(breaks = seq(1980, 2010, by = 5)) +
#   guides(colour = guide_legend(nrow = 1, byrow = TRUE)) +
#   theme_minimal(base_size = 17) +
#   theme(legend.position = 'top',
#         axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 17))
# # Combine the two plots and save
# Jaa2 + Raa2 + 
#   plot_layout(guides = 'collect') & 
#   plot_annotation('Pozeba') &
#   theme(legend.position = 'bottom')

# ggsave("delo/faktorji_psenica/pozeba.png", width = 17, height = 8)
Jaa2 + Raa2 # + plot_layout(guides='collect') & plot_annotation('Pozeba') &
  theme(legend.position='bottom')
ggsave(paste("delo/faktorji_psenica/pozeba.png"),width = 17, height = 8)

Jaa3 + Raa3 + plot_layout(guides='collect') & plot_annotation('Vrocinski stres')&
  theme(legend.position='bottom')
ggsave(paste("delo/faktorji_psenica/vrocinski_stres.png"),width = 17, height = 8)

Jaa4 + Raa4 + plot_layout(guides='collect') & plot_annotation('Rastna doba')&
  theme(legend.position='bottom')
ggsave(paste("delo/faktorji_psenica/rastna.png"),width = 17, height = 8)

Jaa6 + Raa6 + plot_layout(guides='collect') & plot_annotation('Minimalne padavine')&
  theme(legend.position='bottom')
ggsave(paste("delo/faktorji_psenica/min_padavine.png"),width = 17, height = 8)

Jaa7 + Raa7 + plot_layout(guides='collect') &
  theme(legend.position='bottom')&  plot_annotation('Nocni Vrocinski stres in visoka vlaga')
ggsave(paste("delo/faktorji_psenica/vrocinski_stres_Vlaga_noc.png"),width = 17, height = 8)

Jaa8 + Raa8 + plot_layout(guides='collect') &
  theme(legend.position='bottom') &  plot_annotation('Visoke padavine')
ggsave(paste("delo/faktorji_psenica/visoke_padavine.png"),width = 17, height = 8)

# POVPRECJE OBDOBJA ZA FAKTORJE - karte_psenica

vsi_podatki_povp <- vsi_podatki %>% group_by(lats,lons) %>% summarise(across(everything(), list(mean)))
saveRDS(vsi_podatki_povp, file = "faktorji_hist_1981-2010_psenica.rds")


vsi_podatki_povppovp <- vsi_podatki %>% summarise(across(everything(), list(mean)))
# najprej povprecje na 8178 tockah cez 20 letih in 282 lokacij -> povprecje faktorjev pride okoli 0 za vse faktorje
# povprecje kazalnikov pa je enako kot ce najprej povprecis obdobje in nato povprecim se po 282 lokacijah

vsi_podatki_povppovp2 <- vsi_podatki_povp %>% group_by() %>% summarise(across(everything(), list(mean)))
# preverila povprecje za vse lat lon, en rezultato za 1981-2010 pride okoli 0
colnames(vsi_podatki_povp)<-colnames(vsi_podatki)

library(ggsci)

my_palette <- c(
  "#28788E", "#72B9C2", "#D4CE91", "#E6D18C", 
  "#51A0A8", "#87C0B0",  "#E3D390", "#E8D89A", "#B77B52"
)
my_palette <- c("#D73027", "#FC8D59", "#FEE08B", "#E0F3F8", "#91BFDB", "#4575B4")
# my_palette <- c("light green","light blue","deepskyblue3","aquamarine","dark green","darkseagreen")
library(ggthemes)
library(grDevices)
library(colorspace)
my_palette <- hcl.colors(10, palette = "Earth") # https://bookdown.org/hneth/ds4psy/D.3-apx-colors-basics.html

ggplot() +
  geom_tile(data=vsi_podatki_povp,aes(x=lons,y=lats,fill=zpozeba)) +
  geom_sf(data=slovenia_nuts3_mapdata, fill = NA) +
  scale_fill_distiller("z", palette = "RdBu", 
                       breaks = seq(-0.5,2,0.5)) + # Use gradientn for custom palette
  coord_sf(crs = st_crs(4326)) +
  theme_map(base_size = 17) + #theme_minimal
  theme(legend.position = c(.9, .3),
        axis.title.x=element_blank(),
        axis.title.y=element_blank())
ggsave(paste0("delo/karte_psenica/faktorpozeba1981-2010.png"),width = 10, height = 7)

ggplot() +
  geom_tile(data=vsi_podatki_povp,aes(x=lons,y=lats,fill=zprezimovanje_minT)) +
  geom_sf(data=slovenia_nuts3_mapdata, fill = NA) +
  scale_fill_distiller("z", palette = "RdBu", 
                       breaks = seq(-0.5,2,0.5)) + # Use gradientn for custom palette
  coord_sf(crs = st_crs(4326)) +
  theme_map(base_size = 17) + #theme_minimal
  theme(legend.position = c(.9, .3),
        axis.title.x=element_blank(),
        axis.title.y=element_blank())
ggsave(paste0("delo/karte_psenica/faktorprezimovanje_minT1981-2010.png"),width = 10, height = 7)

ggplot() +
  geom_tile(data=vsi_podatki_povp,aes(x=lons,y=lats,fill=zrastna)) +
  geom_sf(data=slovenia_nuts3_mapdata, fill = NA) +
  scale_fill_distiller("z", palette = "RdBu", 
                       breaks = seq(-3,3, 1), limits = c(-3, 3)) + # Use gradientn for custom palette
  coord_sf(crs = st_crs(4326)) +
  theme_map() +
  theme(legend.position = c(.9, .3),
      axis.title.x=element_blank(),
      axis.title.y=element_blank())
ggsave(paste0("delo/karte_psenica/faktorrastna1981-2010.png"),width = 10, height = 7)

ggplot() +
  geom_tile(data=vsi_podatki_povp,aes(x=lons,y=lats,fill=zvrocinski_stres)) +
  geom_sf(data=slovenia_nuts3_mapdata, fill = NA) +
  scale_fill_distiller("z", palette = "RdBu", 
                       breaks = seq(0, 12, 2)) + # Use gradientn for custom palette
  coord_sf(crs = st_crs(4326)) +
  theme_map(base_size = 17) +
  theme(legend.position = c(.9, .3),
      axis.title.x=element_blank(),
      axis.title.y=element_blank())
ggsave(paste0("delo/karte_psenica/faktorvrocinski_stres1981-2010.png"),width = 10, height = 7)

ggplot() +
  geom_tile(data=vsi_podatki_povp,aes(x=lons,y=lats,fill=zvrocinski_stres_Vlaga_noc)) +
  geom_sf(data=slovenia_nuts3_mapdata, fill = NA) +
  scale_fill_distiller("z", palette = "RdBu", 
                       breaks = seq(-2,2, 0.5)) + # Use gradientn for custom palette
  coord_sf(crs = st_crs(4326)) +
  theme_map(base_size = 17) +
  theme(legend.position = c(.9, .3),
      axis.title.x=element_blank(),
      axis.title.y=element_blank())
ggsave(paste0("delo/karte_psenica/faktor_vrocina_vlaga_noc1981-2010.png"),width = 10, height = 7)

ggplot() +
  geom_tile(data=vsi_podatki_povp,aes(x=lons,y=lats,fill=zvisoke_padavine)) +
  geom_sf(data=slovenia_nuts3_mapdata, fill = NA) +
  scale_fill_distiller("z", palette = "RdBu", 
                       breaks = seq(-1, 6, 1)) + # Use gradientn for custom palette
  coord_sf(crs = st_crs(4326)) +
  theme_map(base_size = 17) + theme(legend.position = c(.9, .3),
      axis.title.x=element_blank(),
      axis.title.y=element_blank())
ggsave(paste0("delo/karte_psenica/faktorvisoke_padavine1981-2010.png"),width = 10, height = 7)

ggplot() +
  geom_tile(data=vsi_podatki_povp,aes(x=lons,y=lats,fill=zmin_padavine)) +
  geom_sf(data=slovenia_nuts3_mapdata, fill = NA) +
  scale_fill_distiller("z", palette = "RdBu", 
                       breaks = seq(-0.3,1.8,0.3)) + # Use gradientn for custom palette
  coord_sf(crs = st_crs(4326)) +
  theme_map(base_size = 17) +
  theme(legend.position = c(.9, .3),
      axis.title.x=element_blank(),
      axis.title.y=element_blank())
ggsave(paste0("delo/karte_psenica/faktormin_padavine1981-2010.png"),width = 10, height = 7)


# ###VSI PODATKI 2004
# 
# vsi_podatki_2004 <- subset(vsi_podatki,vsi_podatki$leto==2004)
# ggplot() +
#   geom_tile(data=vsi_podatki_2004,aes(x=lons,y=lats,fill=zpozeba)) +
#   geom_sf(data=slovenia_nuts3_mapdata, fill = NA) +
#   scale_fill_distiller(palette = "Spectral", limits=c(-3,3)) +
#   # scale_fill_distiller(kaz_enote[k], palette = "Spectral") +
#   labs(x="Geografska dolzina",y="Geografska sirina",title=paste0("pozeba 2004")) +
#   coord_sf(crs = st_crs(4326)) +
#   theme_light(base_size = 17)
# ggsave(paste0("delo/karte_psenicatest/faktorpozeba2004.png"),width = 10, height = 7)


###### KORELACIJE ###
corr.test(vsi_podatki$zpozeba,vsi_podatki$zrastna)

 a1<-ggplot() +
   theme_light(base_size = 14) +
   geom_point(data =vsi_podatki,
              aes(x = zvrocinski_stres, y = zrastna))
 a1
 ggsave(paste("delo/faktorji_psenica/korelacija vrocinskega stresa in rastne dobe.png"),a1,width = 12, height = 7)
 
 a2<-ggplot() +
   theme_light(base_size = 14) +
   geom_point(data =vsi_podatki,
              aes(x = zrastna, y = zpozeba))
 a2
 ggsave(paste("delo/faktorji_psenica/korelacija rastne dobe in pozebe.png"),a2,width = 12, height = 7)
 
 a3<-ggplot() +
   theme_light(base_size = 14) +
   geom_point(data =vsi_podatki,
              aes(x = zvisoke_padavine, y = zmin_padavine))
 a3
 ggsave(paste("delo/faktorji_psenica/korelacija ekstr padavin in min padavin.png"),a3,width = 12, height = 7)
 
 a4<-ggplot() +
   theme_light(base_size = 14) +
   geom_point(data =vsi_podatki,
              aes(x = zvrocinski_stres_Vlaga_noc, y = zvrocinski_stres))
 a4
 ggsave(paste("delo/faktorji_psenica/korelacija vrocinskega stresa in vlage ter vrocine ponoci.png"),a4,width = 12, height = 7)
 
 # a5<-ggplot() +
 #   theme_light(base_size = 14) +
 #   geom_point(data =vsi_podatki,
 #              aes(x = zprezimovanje_minT, y = zpozeba))
 # a5
 # ggsave(paste("delo/faktorji_psenica/korelacija prezimovanja in pozebe.png"),a5,width = 12, height = 7)
 # 
 a6<-ggplot() +
   theme_light(base_size = 14) +
   geom_point(data =vsi_podatki,
              aes(x = zrastna, y = zvrocinski_stres_Vlaga_noc))
 a6
 ggsave(paste("delo/faktorji_psenica/korelacija rastne dobe in vlage ter vrocine ponoci.png"),a6,width = 12, height = 7)

 

 ###############################################################################################################################################
 
 # DODAJANJE kazalnikov za 2011-2024, Jablje in Rakican

 vsi_podatki_1 = readRDS(file = "kazalniki_Jablje_Rakican_2011-2024_psenica.rds") #TOLE PREVERI; DO KDAJ IMAMO PODATKEEE O PRIDELKU
 vsi_podatki_1 = subset(vsi_podatki_1, select = -c(End_growing_season,Heat_stress_fl,Heat_stress_mat,chill_portions,year) )
 
 vsi_podatki_M_SD = vsi_podatki[1:28,70:135] # kopiram M-je in SD-je za 1981-2010, 
 # da lahko izra?unam z vrednosti obdobja 2011-2024
 vsi_podatki_2 <- cbind(vsi_podatki_1, vsi_podatki_M_SD)
 
 for(i in seq(0,32,by=1)){ # za 31 kazalnikov smo imeli od 0 do 30, sedaj za 33 pa od 0 do 32
   z <- (vsi_podatki_2[,4+i] - vsi_podatki_2[,37+i])/vsi_podatki_2[,70+i] 
   vsi_podatki_2 <- cbind(vsi_podatki_2,z)
   print(i)
 }
 
 colnames(vsi_podatki_2) <- c(colnames(vsi_podatki_2)[1:102], colnames(vsi_podatki[37:69]))
 vsi_podatki_3 <- cbind(vsi_podatki_2[,1:36],vsi_podatki_2[,103:135], vsi_podatki_2[,37:102])
 
 
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

 vsi_podatki_3$zprezimovanje_minT<-(vsi_podatki_3$zcsdi_annual_sum+vsi_podatki_3$zSums_Tmin10+vsi_podatki_3$zSums_Tmin15)/3

 
 
 vsi_podatki <- rbind(vsi_podatki,vsi_podatki_3) #### POMEMBNO, KONCNA SPREMEMBA ORIGINALNEGA
 # NIZA 1981-2010, DODAMO 2011-2024 ZA JABLJE IN RAKICAN
 
 saveRDS(vsi_podatki, file = "kazalniki_1981-2024_psenica.rds")
 
 ################################################################################################################################################
 
 library(readxl)
 
 vsi_podatki <- readRDS("kazalniki_1981-2024_psenica.rds")
 
 podatki_psenica <- read_excel("Podatki IOSDV - ozimna pšenica.xlsx", sheet = "yield")
 
 podatki_psenica <- podatki_psenica[,c(1,2,3,6,7)]
 
 
 lat_LJ <- 46.0625
 lon_LJ <- 14.5625
 
 lat_Jablje <-	46.1875 #46.1414	
 lat_Rakican <-	46.6875 #46.6504
 lon_Jablje <- 14.5625	#14.5561 
 lon_Rakican <-	16.1875 #16.1966 
 
 data_LJ <- vsi_podatki %>% filter(lats == lat_LJ, lons == lon_LJ)
 data_Jablje <- vsi_podatki %>% filter(lats == lat_Jablje, lons == lon_Jablje)
 data_Jablje$lok <- rep("Jablje",length(data_Jablje$lats))
 data_Rakican <- vsi_podatki %>% filter(lats == lat_Rakican, lons == lon_Rakican)
 data_Rakican$lok <- rep("Rakican",length(data_Rakican$lats))
 podatki_meteo <- rbind(data_Jablje,data_Rakican)
 podatki_meteo$leto <- as.numeric(podatki_meteo$leto)
 leto_zac <- max(min(podatki_psenica$leto),min(data_Jablje$leto))
 leto_kon <- min(max(podatki_psenica$leto),max(data_Jablje$leto))
 
 data_rf <- left_join(podatki_meteo,podatki_psenica,by=c("leto","lok")) %>%
   mutate_if(is.numeric,coalesce,NA)
 
 data_Jablje <- data_rf %>% filter(lok == "Jablje")
 data_Rakican <- data_rf %>% filter(lok == "Rakican")
 
 ################################ KORELACIJE FAKTORJEV S PRIDELKOM, V FAKTORJE SEM DODALA ?E Z VREDNOSTI ZA JABLJE IN Rakičan ZA 2011-2022
 
 # data_Jablje$kolicina_norm<-(data_Jablje$kolicina - mean(na.omit(data_Jablje$kolicina)) ) / 
 #   (sd(na.omit(data_Jablje$kolicina)))
 # data_Rakican$kolicina_norm<-(data_Rakican$kolicina - mean(na.omit(data_Rakican$kolicina)) ) / 
 #   (sd(na.omit(data_Rakican$kolicina)))
 # data_rf <- rbind(data_Jablje,data_Rakican)
 
 cor.test(data_Jablje$zpozeba,data_Jablje$kolicina, method = "spearman")
 # test_rf <- data_rf %>% filter(
 #   zpozeba < quantile(zpozeba, 0.95)
 # )
 
 library(cowplot)
 library(colorspace)
 library(ggrepel)
 
 cor_pozebaR <- cor.test(data_Rakican$zpozeba,data_Rakican$kolicina, method = "spearman")
 
 aa2<-ggplot() +
   theme_light(base_size = 20)+
   geom_point(data = na.omit(data_rf),
              aes(x = zpozeba, y = kolicina, colour = lok), size = 5, alpha = 0.5, shape = 20) + 
   geom_smooth(data = na.omit(data_rf),
               aes(x = zpozeba, y = kolicina, colour = lok), method ="lm", alpha = 0.1) +  
   geom_text(data = na.omit(data_rf),
             aes(x = zpozeba, y = kolicina - 120, colour = lok, label = leto, ... = ), size = 5) +
   annotate(geom="text", x = 2.5, y = 3500, size = 6, label = paste("rho ==", round(cor_pozebaR$estimate, 3)),
            parse = TRUE, vjust = 0, color ="darkseagreen") +
   annotate(geom="text", x = 2.5, y = 3500, size = 6, label = paste("p == ",round(cor_pozebaR$p.value,3)), parse = TRUE, vjust = 2, color ="darkseagreen") +
   scale_color_manual("Pridelek psenice - lokacija",values = c("deepskyblue3","darkseagreen","light green")) + 
   theme_minimal_hgrid() +
   theme(
     legend.position = "top",
     legend.text = element_text(size = 18),legend.title = element_text(size = 18),
     legend.box.spacing = unit(0, "pt"),
     axis.text.x = element_text(size = 18),
     axis.text.y = element_text(size = 18),
     axis.title.x = element_text(size = 18), axis.title.y = element_text(size = 18)   )+
     background_grid(major = "y", minor = "y", size.minor = 0.5)+
     xlab("spring frost conditions (z)") + ylab("yield")
 print(aa2)
 ggsave(paste("delo/faktorji_psenica/korelacija pozebe in pridelka.png"),aa2,width = 10, height = 9)
 
 # cor_prezimJ <- cor.test(data_Jablje$zprezimovanje_minT,data_Jablje$kolicina, method = "spearman")
 cor_prezimR <- cor.test(data_Rakican$zprezimovanje_minT,data_Rakican$kolicina, method = "spearman")
 
 aa2<-ggplot() +
   theme_light(base_size = 20)+
   geom_point(data = na.omit(data_rf),
              aes(x = zprezimovanje_minT, y = kolicina, colour = lok), size = 5, alpha = 0.5, shape = 20) + 
   geom_smooth(data = na.omit(data_rf),
               aes(x = zprezimovanje_minT, y = kolicina, colour = lok), method ="lm", alpha = 0.1) +  
   geom_text(data = na.omit(data_rf),
             aes(x = zprezimovanje_minT, y = kolicina - 120, colour = lok, label = leto, ... = ), size = 5) +
   annotate(geom="text", x = 6.5, y = 3200, size = 6, label = paste("rho ==", round(cor_prezimR$estimate, 3)),
            parse = TRUE, vjust = 0, color ="darkseagreen") +
   annotate(geom="text", x = 6.5, y = 3200, size = 6, label = paste("p == ",round(cor_prezimR$p.value,3)), parse = TRUE, vjust = 2, color ="darkseagreen") +
   scale_color_manual("Pridelek psenice - lokacija",values = c("deepskyblue3","darkseagreen","light green")) + 
   theme_minimal_hgrid() +
   theme(
     legend.position = "top",
     legend.text = element_text(size = 18),legend.title = element_text(size = 18),
     legend.box.spacing = unit(0, "pt"),
     axis.text.x = element_text(size = 18),
     axis.text.y = element_text(size = 18),
     axis.title.x = element_text(size = 18), axis.title.y = element_text(size = 18)   )+
   background_grid(major = "y", minor = "y", size.minor = 0.5)+
   xlab("dormancy period (z)") + ylab("yield")
 print(aa2)
 ggsave(paste("delo/faktorji_psenica/korelacija prezimov razmer in pridelka.png"),aa2,width = 10, height = 9)
 
 cor.test(data_Jablje$zmin_padavine,data_Jablje$kolicina, method = "spearman")
 cor.test(data_Rakican$zmin_padavine,data_Rakican$kolicina, method = "spearman")
 a1<-ggplot() +
   theme_light(base_size = 20) +
   geom_point(data = na.omit(data_rf),
              aes(x = zmin_padavine, y = kolicina, colour = lok), size = 5, alpha = 0.5, shape = 20) +  
   geom_smooth(data = na.omit(data_rf),
               aes(x = zmin_padavine, y = kolicina, colour = lok), method ="lm", span = 1.5, alpha = 0.1) +  
   geom_text(data = na.omit(data_rf),
             aes(x = zmin_padavine, y = kolicina - 120, colour = lok, label = leto, ... = ), size = 5) +
   # annotate(geom="text", x = 2.8, y = 4, size = 6, label = paste("rho ==", round(cor_min_pad$estimate, 3)), 
   #          parse = TRUE, vjust = 0, color ="azure4") +
   # annotate(geom="text", x = 2.8, y = 4, size = 6, label = paste("p == ",round(cor_min_pad$p.value,3)), parse = TRUE, vjust = 2, color ="azure4") +
   scale_color_manual("Pridelek psenice - lokacija",values = c("deepskyblue3","darkseagreen","light green")) + 
   theme_minimal_hgrid() +
   theme(
     legend.position = "top",
     legend.text = element_text(size = 18),legend.title = element_text(size = 18),
     legend.box.spacing = unit(0, "pt"),
     axis.text.x = element_text(size = 18),
     axis.text.y = element_text(size = 18),
     axis.title.x = element_text(size = 18), axis.title.y = element_text(size = 18)   )+
   background_grid(major = "y", minor = "y", size.minor = 0.5)+
   xlab("low precipitation conditions (z)") + ylab("yield")
 a1
 ggsave(paste("delo/faktorji_psenica/korelacija min padavin in pridelka.png"),a1,width = 10, height = 9)
 
 # test_rf <- data_rf %>% filter(
 #   zrastna < quantile(zrastna, 0.95)
 # )
 cor.test(data_Jablje$zrastna,data_Jablje$kolicina, method = "spearman")
 cor.test(data_Rakican$zrastna,data_Rakican$kolicina, method = "spearman")
 a2<-ggplot() +
   theme_light(base_size = 20) +
   geom_point(data = na.omit(data_rf),
              aes(x = zrastna, y = kolicina, colour = lok), size = 5, alpha = 0.5, shape = 20) + 
   geom_smooth(data = na.omit(data_rf),
               aes(x = zrastna, y = kolicina, colour = lok), method ="lm", span = 1.5, alpha = 0.1) +  
   geom_text(data = na.omit(data_rf),
             aes(x = zrastna, y = kolicina - 120, colour = lok, label = leto, ... = ), size = 5) +
   # annotate(geom="text", x = 1.6, y = 4, size = 6, label = paste("rho ==", round(cor_rastna$estimate, 3)), 
   #          parse = TRUE, vjust = 0, color ="azure4") +
   # annotate(geom="text", x = 1.6, y = 4, size = 6, label = paste("p == ",round(cor_rastna$p.value,3)), parse = TRUE, vjust = 2, color ="azure4") +
   scale_color_manual("Pridelek psenice - lokacija", values = c("deepskyblue3","darkseagreen","light green")) + 
   theme_minimal_hgrid() +
   theme(
     legend.position = "top",
     legend.text = element_text(size = 18),legend.title = element_text(size = 18),
     legend.box.spacing = unit(0, "pt"),
     axis.text.x = element_text(size = 18),
     axis.text.y = element_text(size = 18),
     axis.title.x = element_text(size = 18), axis.title.y = element_text(size = 18)   )+
   background_grid(major = "y", minor = "y", size.minor = 0.5) +
   xlab("growing season temperature conditions (z)") + ylab("yield")
 a2
 ggsave(paste("delo/faktorji_psenica/korelacija rastne dobe in pridelka.png"),a2,width = 10, height = 9)
 
 # test_rf <- data_rf %>% filter(
   # zvisoke_padavine < quantile(zvisoke_padavine, 0.95)
 # )
 cor_visoke_padR <- cor.test(data_Rakican$zvisoke_padavine,data_Rakican$kolicina, method = "spearman")
 cor_visoke_padJ <- cor.test(data_Jablje$zvisoke_padavine,data_Jablje$kolicina, method = "spearman")
 a3<-ggplot() +
   theme_light(base_size = 20) +
   geom_point(data = na.omit(data_rf),
              aes(x = zvisoke_padavine, y = kolicina, colour = lok), size = 5, alpha = 0.5, shape = 20) + 
   geom_smooth(data = na.omit(data_rf),
               aes(x = zvisoke_padavine, y = kolicina, colour = lok), method ="lm", alpha = 0.1) +  
   geom_text(data = na.omit(data_rf),
             aes(x = zvisoke_padavine, y = kolicina - 120, colour = lok, label = leto, ... = ), size = 5) +
   annotate(geom="text", x = 0.5, y = 3200, size = 6, label = paste("rho ==", round(cor_visoke_padR$estimate, 3)),
            parse = TRUE, vjust = 0, color ="darkseagreen") +
   annotate(geom="text", x = 0.5, y = 3200, size = 6, label = paste("p == ",round(cor_visoke_padR$p.value,3)), parse = TRUE, vjust = 2, color ="darkseagreen") +
   scale_color_manual("Pridelek psenice - lokacija",values = c("deepskyblue3","darkseagreen","light green")) + 
   theme_minimal_hgrid() +
   theme(
     legend.position = "top",
     legend.text = element_text(size = 18),legend.title = element_text(size = 18),
     legend.box.spacing = unit(0, "pt"),
     axis.text.x = element_text(size = 18),
     axis.text.y = element_text(size = 18),
     axis.title.x = element_text(size = 18), axis.title.y = element_text(size = 18)   )+
   background_grid(major = "y", minor = "y", size.minor = 0.5) +
   xlab("high precipitation conditions (z)") + ylab("yield")
 a3
 ggsave(paste("delo/faktorji_psenica/korelacija ekstr padavin in pridelka.png"),a3,width = 10, height = 9)
 
 # test_rf <- data_rf %>% filter(
 #   zvrocinski_stres_Vlaga_noc < quantile(zvrocinski_stres_Vlaga_noc, 0.95)
 # )
 cor.test(data_Rakican$zvrocinski_stres_Vlaga_noc,data_Rakican$kolicina, method = "spearman")
 cor.test(data_Jablje$zvrocinski_stres_Vlaga_noc,data_Jablje$kolicina, method = "spearman")
 a4<-ggplot() +
   theme_light(base_size = 20) +
   geom_point(data = na.omit(data_rf),
              aes(x = zvrocinski_stres_Vlaga_noc, y = kolicina, colour = lok), size = 5, alpha = 0.5, shape = 20) +
   geom_smooth(data = na.omit(data_rf),
               aes(x = zvrocinski_stres_Vlaga_noc, y = kolicina, colour = lok), method ="lm", alpha = 0.1) +
   geom_text(data = na.omit(data_rf),
             aes(x = zvrocinski_stres_Vlaga_noc, y = kolicina - 120, colour = lok, label = leto, ... = ), size = 5) +
   # annotate(geom="text", x = 2.8, y = 0, size = 6, label = paste("rho ==", round(cor_vrocinski_stres_Vlaga_noc$estimate, 3)),
   #          parse = TRUE, vjust = 0, color ="azure4") +
   # annotate(geom="text", x = 2.8, y = 0, size = 6, label = paste("p == ",round(cor_vrocinski_stres_Vlaga_noc$p.value,3)), parse = TRUE, vjust = 2, color ="azure4") +
   scale_color_manual("Pridelek psenice - lokacija", values = c("deepskyblue3","darkseagreen","light green")) +
   theme_minimal_hgrid() +
   theme(
     legend.position = "top",
     legend.text = element_text(size = 18),legend.title = element_text(size = 18),
     legend.box.spacing = unit(0, "pt"),
     axis.text.x = element_text(size = 18),
     axis.text.y = element_text(size = 18),
     axis.title.x = element_text(size = 18), axis.title.y = element_text(size = 18)   )+
   background_grid(major = "y", minor = "y", size.minor = 0.5) +
   xlab("night heat and high humidity conditions (z)") + ylab("yield")
 a4
 ggsave(paste("delo/faktorji_psenica/korelacija vrocine ponoci vlage in pridelka.png"),a4,width = 10, height = 9)
 
 # test_rf <- data_rf %>% filter(
 #   zvrocinski_stres < quantile(zvrocinski_stres, 0.95)
 # )
 cor.test(data_Jablje$zvrocinski_stres,data_Jablje$kolicina, method = "spearman")
 cor.test(data_Rakican$zvrocinski_stres,data_Rakican$kolicina, method = "spearman")
 
  a6<-ggplot() +
   geom_point(data = na.omit(data_rf),
              aes(x = zvrocinski_stres, y = kolicina, colour = lok), size = 5, alpha = 0.5, shape = 20) + 
   geom_smooth(data = na.omit(data_rf),
               aes(x = zvrocinski_stres, y = kolicina, colour = lok), method ="lm", alpha = 0.1) +  
   geom_text(data = na.omit(data_rf),
              aes(x = zvrocinski_stres, y = kolicina - 120, colour = lok, label = leto, ... = ), size = 5) +
    # annotate(geom="text", x = 2.8, y = 4, size = 6, label = paste("rho ==", round(cor_moderate$estimate, 3)), 
    #          parse = TRUE, vjust = 0, color ="azure4") +
    # annotate(geom="text", x = 2.8, y = 4, size = 6, label = paste("p == ",round(cor_moderate$p.value,3)), parse = TRUE, vjust = 2, color ="azure4") +
   theme_light(base_size = 20) +
    scale_color_manual("Pridelek psenice - lokacija",values = c("deepskyblue3","darkseagreen","light green")) + 
    theme_minimal_hgrid() +
    theme(
      legend.position = "top",
      legend.text = element_text(size = 18),legend.title = element_text(size = 18),
      legend.box.spacing = unit(0, "pt"),
      axis.text.x = element_text(size = 18),
      axis.text.y = element_text(size = 18),
      axis.title.x = element_text(size = 18), axis.title.y = element_text(size = 18)   )+
    background_grid(major = "y", minor = "y", size.minor = 0.5) +
    xlab("moderate heat stress conditions (z)") + ylab("yield")
  
  a6
 ggsave(paste("delo/faktorji_psenica/korelacija vrocinskega stresa in pridelka.png"),a6,width = 10, height = 9)
 
 
 #tvoj model
 mod<- lm( kolicina ~  zrastna + zvisoke_padavine + 
             zekstremne_T + zvrocinski_stres, data=data_rf)
 plot(mod)
 summary(mod)
 library(effects)
 plot(allEffects(mod))
 library(lme4)
 data_rf$lok<-as.factor(data_rf$lok)
 mod_lme<- lmer(kolicina ~ zrastna + zvisoke_padavine + 
                  zekstremne_T + zvrocinski_stres + 
                  (zrastna|lok) + (zvisoke_padavine|lok) +
                  (zvrocinski_stres|lok) + (zekstremne_T|lok), 
                data=data_rf)
 mod_lme<- lmer(kolicina ~ zvisoke_padavine + (zvisoke_padavine|lok), 
                data=data_rf)
 plot(mod_lme)
 summary(mod_lme)
 plot(allEffects(mod_lme))
 

##########################################################
library(ggpubr)

# fig5<-ggplot() + theme_light(base_size = 20) +
#   geom_point(data = na.omit(data_rf), aes(x = BEDD, y = kolicina, colour = lok), size = 2) + 
#   geom_smooth(data = na.omit(data_rf), aes(x = BEDD, y = kolicina, colour = lok), method ="lm", alpha = 0.1) +
#   scale_color_manual(values = c("deepskyblue3","light blue","light green","aquamarine","dark green","navy")) 
# 
# fig6<-ggplot() + theme_light(base_size = 20) +
#   geom_point(data = na.omit(data_rf), aes(x = DTR, y = kolicina, colour = lok), size = 2) + 
#   geom_smooth(data = na.omit(data_rf), aes(x = DTR, y = kolicina, colour = lok), method ="lm", alpha = 0.1) +
#   scale_color_manual(values = c("deepskyblue3","light blue","light green","aquamarine","dark green","navy")) 
# 
# fig7<-ggplot() + theme_light(base_size = 20) +
#   geom_point(data = na.omit(data_rf), aes(x = TGS, y = kolicina, colour = lok), size = 2) + 
#   geom_smooth(data = na.omit(data_rf), aes(x = TGS, y = kolicina, colour = lok), method ="lm", alpha = 0.1) +
#   scale_color_manual(values = c("deepskyblue3","light blue","light green","aquamarine","dark green","navy")) 
# 
# fig8<-ggplot() + theme_light(base_size = 20) +
#   geom_point(data = na.omit(data_rf), aes(x = T_warmest_m, y = kolicina, colour = lok), size = 2) + 
#   geom_smooth(data = na.omit(data_rf), aes(x = T_warmest_m, y = kolicina, colour = lok), method ="lm", alpha = 0.1) +
#   scale_color_manual(values = c("deepskyblue3","light blue","light green","aquamarine","dark green","navy")) 
# 
# fig9<-ggplot() + theme_light(base_size = 20) +
#   geom_point(data = na.omit(data_rf), aes(x = TG_of_warmest_quarter, y = kolicina, colour = lok), size = 2) + 
#   geom_smooth(data = na.omit(data_rf), aes(x = TG_of_warmest_quarter, y = kolicina, colour = lok), method ="lm", alpha = 0.1) +
#   scale_color_manual(values = c("deepskyblue3","light blue","light green","aquamarine","dark green","navy"))
# 
# fig10<-ggplot() + theme_light(base_size = 20) +
#   geom_point(data = na.omit(data_rf), aes(x = growing_degree_days, y = kolicina, colour = lok), size = 2) + 
#   geom_smooth(data = na.omit(data_rf), aes(x = growing_degree_days, y = kolicina, colour = lok), method ="lm", alpha = 0.1) +
#   scale_color_manual(values = c("deepskyblue3","light blue","light green","aquamarine","dark green","navy"))
# fig_skupni1<-ggarrange(fig5,fig6,fig7,fig8,fig9,fig10,nrow = 2, ncol = 3,common.legend = TRUE,legend = "bottom",font.label = c(size=20))
# fig_skupni1
# ggsave(paste("delo/faktorji_psenica/test - posamezni kazalniki/kazalniki rastne dobe in pridelka.png"),fig_skupni1,width = 24, height = 12)




fig11<-ggplot() + theme_light(base_size = 20) +
  geom_point(data = na.omit(data_rf), aes(x = zr20mm, y = kolicina, colour = lok), size = 2) + 
  geom_smooth(data = na.omit(data_rf), aes(x = zr20mm, y = kolicina, colour = lok), method ="lm", alpha = 0.1) +
  geom_text(data = na.omit(data_rf),
            aes(x = zr20mm, y = kolicina, colour = lok, label = leto, ... = )) +
  scale_color_manual(values = c("deepskyblue3","light blue","light green","aquamarine","dark green","navy")) 

fig12<-ggplot() + theme_light(base_size = 20) +
  geom_point(data = na.omit(data_rf), aes(x = zHeavy_prec_days, y = kolicina, colour = lok), size = 2) + 
  geom_smooth(data = na.omit(data_rf), aes(x = zHeavy_prec_days, y = kolicina, colour = lok), method ="lm", alpha = 0.1) +
  geom_text(data = na.omit(data_rf),
            aes(x = zHeavy_prec_days, y = kolicina, colour = lok, label = leto, ... = )) +
  scale_color_manual(values = c("deepskyblue3","light blue","light green","aquamarine","dark green","navy")) 

fig1<-ggplot() + theme_light(base_size = 20) +
  geom_point(data = na.omit(data_rf), aes(x = zPrec_wettest_month, y = kolicina, colour = lok), size = 2) +
  geom_smooth(data = na.omit(data_rf), aes(x = zPrec_wettest_month, y = kolicina, colour = lok), method ="lm", alpha = 0.1) +
  geom_text(data = na.omit(data_rf),
            aes(x = zPrec_wettest_month, y = kolicina, colour = lok, label = leto, ... = )) +
  scale_color_manual(values = c("deepskyblue3","light blue","light green"))

fig2<-ggplot() + theme_light(base_size = 20) +
  geom_point(data = na.omit(data_rf), aes(x = zrx5d, y = kolicina, colour = lok), size = 2) +
  geom_smooth(data = na.omit(data_rf), aes(x = zrx5d, y = kolicina, colour = lok), method ="lm", alpha = 0.1) +
  geom_text(data = na.omit(data_rf),
            aes(x = zrx5d, y = kolicina, colour = lok, label = leto, ... = )) +
  scale_color_manual(values = c("deepskyblue3","light blue","light green"))

fig3<-ggplot() + theme_light(base_size = 20) +
  geom_point(data = na.omit(data_rf), aes(x = zCWD, y = kolicina, colour = lok), size = 2) +
  geom_smooth(data = na.omit(data_rf), aes(x = zCWD, y = kolicina, colour = lok), method ="lm", alpha = 0.1) +
  geom_text(data = na.omit(data_rf),
            aes(x = zCWD, y = kolicina, colour = lok, label = leto, ... = )) +
  scale_color_manual(values = c("deepskyblue3","light blue","light green"))

fig4<-ggplot() + theme_light(base_size = 20) +
  geom_point(data = na.omit(data_rf), aes(x = zSDII, y = kolicina, colour = lok), size = 2) +
  geom_smooth(data = na.omit(data_rf), aes(x = zSDII, y = kolicina, colour = lok), method ="lm", alpha = 0.1) +
  geom_text(data = na.omit(data_rf),
            aes(x = zSDII, y = kolicina, colour = lok, label = leto, ... = )) +
  scale_color_manual(values = c("deepskyblue3","light blue","light green"))

fig5<-ggplot() + theme_light(base_size = 20) +
  geom_point(data = na.omit(data_rf), aes(x = zEffective_prec, y = kolicina, colour = lok), size = 2) +
  geom_smooth(data = na.omit(data_rf), aes(x = zEffective_prec, y = kolicina, colour = lok), method ="lm", alpha = 0.1) +
  geom_text(data = na.omit(data_rf),
            aes(x = zEffective_prec, y = kolicina, colour = lok, label = leto, ... = )) +
  scale_color_manual(values = c("deepskyblue3","light blue","light green"))

fig6<-ggplot() + theme_light(base_size = 20) +
  geom_point(data = na.omit(data_rf), aes(x = zGrowing_season_prec, y = kolicina, colour = lok), size = 2) +
  geom_smooth(data = na.omit(data_rf), aes(x = zGrowing_season_prec, y = kolicina, colour = lok), method ="lm", alpha = 0.1) +
  geom_text(data = na.omit(data_rf),
            aes(x = zGrowing_season_prec, y = kolicina, colour = lok, label = leto, ... = )) +
  scale_color_manual(values = c("deepskyblue3","light blue","light green"))

fig7<-ggplot() + theme_light(base_size = 20) +
  geom_point(data = na.omit(data_rf), aes(x = zNongrowing_season_prec, y = kolicina, colour = lok), size = 2) +
  geom_smooth(data = na.omit(data_rf), aes(x = zNongrowing_season_prec, y = kolicina, colour = lok), method ="lm", alpha = 0.1) +
  geom_text(data = na.omit(data_rf),
            aes(x = zNongrowing_season_prec, y = kolicina, colour = lok, label = leto, ... = )) +
  scale_color_manual(values = c("deepskyblue3","light blue","light green"))

fig8<-ggplot() + theme_light(base_size = 20) +
  geom_point(data = na.omit(data_rf), aes(x = zprecip_total, y = kolicina, colour = lok), size = 2) +
  geom_smooth(data = na.omit(data_rf), aes(x = zprecip_total, y = kolicina, colour = lok), method ="lm", alpha = 0.1) +
  geom_text(data = na.omit(data_rf),
            aes(x = zprecip_total, y = kolicina, colour = lok, label = leto, ... = )) +
  scale_color_manual(values = c("deepskyblue3","light blue","light green"))

fig_skupni2<-ggarrange(fig11,fig12,fig1,fig2,fig3,fig4,fig5,fig6,fig7, 
                       nrow = 3, ncol = 4,common.legend = TRUE,legend = "bottom",font.label = c(size=20))
fig_skupni2
ggsave(paste("delo/faktorji_psenica/kazalniki visokih padavin in pridelka.png"),fig_skupni2,width = 16, height = 6)
# 
# 
# fig11<-ggplot() + theme_light(base_size = 20) +
#   geom_point(data = na.omit(data_rf), aes(x = zr20mm, y = log(kolicina), colour = lok), size = 2) + 
#   geom_smooth(data = na.omit(data_rf), aes(x = zr20mm, y = log(kolicina), colour = lok), method ="lm", alpha = 0.1) +
#   scale_color_manual(values = c("deepskyblue3","light blue","light green","aquamarine","dark green","navy")) 
# 
# fig12<-ggplot() + theme_light(base_size = 20) +
#   geom_point(data = na.omit(data_rf), aes(x = zHeavy_prec_days, y = log(kolicina), colour = lok), size = 2) + 
#   geom_smooth(data = na.omit(data_rf), aes(x = zHeavy_prec_days, y = log(kolicina), colour = lok), method ="lm", alpha = 0.1) +
#   scale_color_manual(values = c("deepskyblue3","light blue","light green","aquamarine","dark green","navy")) 
# 
# fig1<-ggplot() + theme_light(base_size = 20) +
#   geom_point(data = na.omit(data_rf), aes(x = zPrec_wettest_month, y = log(kolicina), colour = lok), size = 2) +
#   geom_smooth(data = na.omit(data_rf), aes(x = zPrec_wettest_month, y = log(kolicina), colour = lok), method ="lm", alpha = 0.1) +
#   scale_color_manual(values = c("deepskyblue3","light blue","light green"))
# 
# fig2<-ggplot() + theme_light(base_size = 20) +
#   geom_point(data = na.omit(data_rf), aes(x = zrx5d, y = log(kolicina), colour = lok), size = 2) +
#   geom_smooth(data = na.omit(data_rf), aes(x = zrx5d, y = log(kolicina), colour = lok), method ="lm", alpha = 0.1) +
#   scale_color_manual(values = c("deepskyblue3","light blue","light green"))
# 
# fig3<-ggplot() + theme_light(base_size = 20) +
#   geom_point(data = na.omit(data_rf), aes(x = zCWD, y = log(kolicina), colour = lok), size = 2) +
#   geom_smooth(data = na.omit(data_rf), aes(x = zCWD, y = log(kolicina), colour = lok), method ="lm", alpha = 0.1) +
#   scale_color_manual(values = c("deepskyblue3","light blue","light green"))
# 
# fig4<-ggplot() + theme_light(base_size = 20) +
#   geom_point(data = na.omit(data_rf), aes(x = zSDII, y = log(kolicina), colour = lok), size = 2) +
#   geom_smooth(data = na.omit(data_rf), aes(x = zSDII, y = log(kolicina), colour = lok), method ="lm", alpha = 0.1) +
#   scale_color_manual(values = c("deepskyblue3","light blue","light green"))
# 
# fig_skupni2<-ggarrange(fig11,fig12,fig1,fig2,fig3,fig4,
#                        nrow = 2, ncol = 3,common.legend = TRUE,legend = "bottom",font.label = c(size=20))
# fig_skupni2
# ggsave(paste("delo/faktorji_psenica/test - posamezni kazalniki/kazalniki visokih padavin in pridelka.png"),fig_skupni2,width = 16, height = 6)












for(k in seq(1,length(kaz),by=1)){
  data <- data.frame(vsi_podatki$lons, vsi_podatki$lats, vsi_podatki$leto, vsi_podatki[[kaz[k]]])
  colnames(data) <- c("lons","lats","leto","agroclim_ind")
  data$leto <- as.numeric(as.character(data$leto))
  # data <- data %>% filter(leto >= zac, leto <= kon)
  data=na.omit(data)
  min = min(na.omit(vsi_podatki[[kaz[k]]]))
  max = max(na.omit(vsi_podatki[[kaz[k]]]))
  
  kaz_z <- calc_z_score(data$agroclim_ind)
  data$z_score <- kaz_z
  data_z[[kaz[k]]] <- kaz_z
  
  vmesni <- data.frame(lons = data$lons, lats = data$lats, leto = data$leto, kaz_z, kaz = rep(kaz[k], by = length(kaz_z)))
  data_z1 <- rbind(data_z1, vmesni)

  
  povp <- data %>% group_by(lats,lons) %>%
    summarise(period_mean = mean(agroclim_ind))
  ggplot() +
    geom_tile(data=povp,aes(x=lons,y=lats,fill=period_mean)) +
    geom_sf(data=slovenia_nuts3_mapdata, fill = NA) +
    scale_fill_distiller(kaz_enote[k], palette = "Spectral", c(min,max)) +
    # scale_fill_distiller(kaz_enote[k], palette = "Spectral") +
    labs(x="Geografska dolzina",y="Geografska sirina",title=paste0("Kazalnik ",kaz[k],", ",zac,"-",kon)) +
    coord_sf(crs = st_crs(4326)) +
    theme_light(base_size = 17)
  ggsave(paste0("delo/karte_psenica/hist_period_mean_",kaz[k],"_",zac,"-",kon,".png"),width = 10, height = 7)
  
  min1 = min(data$z_score)
  max1 = max(data$z_score)
  povp <- data %>% group_by(lats,lons) %>%
    summarise(period_mean = mean(z_score))
  ggplot() +
    geom_tile(data=povp,aes(x=lons,y=lats,fill=period_mean)) +
    geom_sf(data=slovenia_nuts3_mapdata, fill = NA) +
    scale_fill_distiller(palette = "Spectral", limits = c(min1,max1)) +
    # scale_fill_distiller(kaz_enote[k], palette = "Spectral") +
    labs(x="Geografska dolzina",y="Geografska sirina",title=paste0("Z vrednost - ",kaz[k],", ",zac,"-",kon)) +
    coord_sf(crs = st_crs(4326)) +
    theme_light(base_size = 17)
  ggsave(paste0("delo/karte_psenica/z_hist_period_mean_",kaz[k],"_",zac,"-",kon,".png"),width = 10, height = 7)
}

# 
# ####### RISANJE FAKTORJEV
# # install.packages(c("gfonts","hrbrthemes"))
# library(hrbrthemes)
# library(ggplot2)
# # RASTNA DOBA IN DORMANCA
# data_RDD <- data_z1 %>% filter(
#   kaz == "TGS" | kaz == "TG_of_warmest_quarter" | kaz == "TG_of_coldest_quarter" | kaz == "DTR" |
#   kaz == "growing_degree_days" | kaz == "GSL" | kaz == "BEDD" | kaz == "T_warmest_m" |
#   kaz == "T_coldest_m")
# rastna_doba_dormanca <- aggregate(kaz_z ~ lons + lats + leto, data_RDD, FUN = mean, na.rm=TRUE, na.action=na.pass)
# 
# lat_LJ <- 46.0625
# lon_LJ <- 14.5625
# data_LJ <- rastna_doba_dormanca %>% filter(lats == lat_LJ, lons == lon_LJ)
# data_LJ$kaz <- rep("faktor", by = length(data_LJ[,1]))
# data_LJ_kaz <- data_RDD %>% filter(lats == lat_LJ, lons == lon_LJ)
# data_LJ <- rbind(data_LJ, data_LJ_kaz)
# 
# # cols <- c("T najtoplej?e ?etrtine leta"="sienna3","T najhladnej?e ?etrtine leta"="deepskyblue3",
# #   "DTR" ="steelblue3", "growing_degree_days" ="darkblue", "GSL" = "light green", "BEDD" = "dark green", "T_warmest_m" = "aquamarine",
# #    "T_coldest_m" = "yellow","faktor"="darkred")
# aa<-ggplot(data_LJ, aes(x=leto, y = kaz_z, group = kaz, colour = kaz)) +
#   theme_light(base_size = 14)+ 
#   geom_point(size=1.75)+geom_line(lty=1,lwd = 0.5)+
#   labs(title = paste("Rastna doba in dormanca",sep="")) +  ylab("") + xlab("leto") + 
#   scale_color_viridis(name="Legenda",discrete = TRUE) + 
#   scale_y_continuous(limits=c(-2.5,2.5)) + 
#   # theme_ipsum() +
#   theme(legend.text=element_text(size=15), 
#         axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
#         legend.position='bottom') 
# aa
# ggsave(paste("delo/faktorji_psenica/rastna_doba_dormanca_casovni.png"),width = 12, height = 7)
# ggsave(paste("delo/faktorji_psenica/rastna_doba_dormanca_casovni.pdf"))
# 
# povp_rastna_doba_dormanca <- rastna_doba_dormanca %>% group_by(lats,lons) %>%
#   summarise(period_mean = mean(kaz_z))
# ggplot() +
#   geom_tile(data=povp_rastna_doba_dormanca,aes(x=lons,y=lats,fill=period_mean)) +
#   geom_sf(data=slovenia_nuts3_mapdata, fill = NA) +
#   labs(x="Geografska dolzina",y="Geografska sirina",title=paste0("faktor Rastna doba in dormanca")) +
#   coord_sf(crs = st_crs(4326)) +
#   scale_fill_distiller(palette = "Spectral") +
#   theme_light(base_size = 17)
# ggsave(paste0("delo/faktorji_psenica/rastna_doba_dormanca_",zac,"-",kon,".png"),width = 10, height = 7)
# 
# 
# 
# # POZEBA IN LEDENI DNEVI
# data_PLD  <- data_z1 %>% filter(
#   kaz == "FD" | kaz == "CFD" | kaz == "ice_days")
# pozeba_ledeni_dnevi <- aggregate(kaz_z ~ lons + lats + leto, data_PLD, FUN = mean, na.rm=TRUE, na.action=na.pass)
# data_LJ1 <- pozeba_ledeni_dnevi %>% filter(lats == lat_LJ, lons == lon_LJ)
# data_LJ1$kaz <- rep("faktor", by = length(data_LJ1[,1]))
# data_LJ1_kaz <- data_PLD %>% filter(lats == lat_LJ, lons == lon_LJ)
# data_LJ1 <- rbind(data_LJ1, data_LJ1_kaz)
# 
# aa<-ggplot(data_LJ1, aes(x=leto, y = kaz_z, group = kaz, colour = kaz)) +
#   theme_light(base_size = 14)+ 
#   geom_point(size=1.75)+geom_line(lty=1,lwd = 0.5)+
#   labs(title = paste("Pozeba in ledeni dnevi",sep="")) +  ylab("") + xlab("leto") + 
#   scale_color_viridis(name="Legenda",discrete = TRUE) + 
#   scale_y_continuous(limits=c(-2.5,2.5)) + 
#   theme(legend.text=element_text(size=15), 
#         axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
#         legend.position='bottom') 
# aa
# ggsave(paste("delo/faktorji_psenica/pozeba_ledeni_dnevi_casovni.png"),width = 12, height = 7)
# 
# povp_pozeba_ledeni_dnevi <- pozeba_ledeni_dnevi %>% group_by(lats,lons) %>%
#   summarise(period_mean = mean(kaz_z))
# ggplot() +
#   geom_tile(data=povp_pozeba_ledeni_dnevi,aes(x=lons,y=lats,fill=period_mean)) +
#   geom_sf(data=slovenia_nuts3_mapdata, fill = NA) +
#   scale_fill_distiller(palette = "Spectral") +
#   labs(x="Geografska dolzina",y="Geografska sirina",title=paste0("faktor Pozeba in ledeni dnevi")) +
#   coord_sf(crs = st_crs(4326)) +
#   theme_light(base_size = 17)
# ggsave(paste0("delo/faktorji_psenica/pozeba_ledeni_dnevi_",zac,"-",kon,".png"),width = 10, height = 7)
# 
# 
# 
# # VISOKE TEMPERATURE
# data_VT <- data_z1 %>% filter(
#   kaz == "Days_Tmax32" | kaz == "Sums_Tmax32" | kaz == "su" | kaz == "tr" |
#   kaz == "cons_summer_days" | kaz == "Dnevi_30max" | kaz == "Dnevi_32max_anthesis")
# visoke_T <- aggregate(kaz_z ~ lons + lats + leto, data_VT, FUN = mean, na.rm=TRUE, na.action=na.pass)
# data_LJ2 <- visoke_T %>% filter(lats == lat_LJ, lons == lon_LJ)
# data_LJ2$kaz <- rep("faktor", by = length(data_LJ2[,1]))
# data_LJ2_kaz <- data_VT %>% filter(lats == lat_LJ, lons == lon_LJ)
# data_LJ2 <- rbind(data_LJ2, data_LJ2_kaz)
# 
# aa<-ggplot(data_LJ2, aes(x=leto, y = kaz_z, group = kaz, colour = kaz)) +
#   theme_light(base_size = 14)+ 
#   geom_point(size=1.75)+geom_line(lty=1,lwd = 0.5)+
#   labs(title = paste("Visoke temperature",sep="")) +  ylab("") + xlab("leto") + 
#   scale_color_viridis(name="Legenda",discrete = TRUE) + 
#   # scale_y_continuous(limits=c(-2.5,2.5)) + 
#   theme(legend.text=element_text(size=15), 
#         axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
#         legend.position='bottom') 
# aa
# ggsave(paste("delo/faktorji_psenica/visoke_T_casovni.png"),width = 12, height = 7)
# 
# povp_visoke_T <- visoke_T %>% group_by(lats,lons) %>%
#   summarise(period_mean = mean(kaz_z))
# ggplot() +
#   geom_tile(data=povp_visoke_T,aes(x=lons,y=lats,fill=period_mean)) +
#   geom_sf(data=slovenia_nuts3_mapdata, fill = NA) +
#   scale_fill_distiller(palette = "Spectral") +
#   labs(x="Geografska dolzina",y="Geografska sirina",title=paste0("faktor Visoke temperature")) +
#   coord_sf(crs = st_crs(4326)) +
#   theme_light(base_size = 17)
# ggsave(paste0("delo/faktorji_psenica/visoke_T_",zac,"-",kon,".png"),width = 10, height = 7)
# 
# 
# 
# # Vrocinski EKSTREMI
# data_VE <- data_z1 %>% filter(
#   kaz == "tn90p" | kaz == "tx90p" | kaz == "tg90p" | kaz == "WSDI")
# vrocinski_ekstremi <- aggregate(kaz_z ~ lons + lats + leto, data_VE, FUN = mean, na.rm=TRUE, na.action=na.pass)
# data_LJ3 <- vrocinski_ekstremi %>% filter(lats == lat_LJ, lons == lon_LJ)
# data_LJ3$kaz <- rep("faktor", by = length(data_LJ3[,1]))
# data_LJ3_kaz <- data_VE %>% filter(lats == lat_LJ, lons == lon_LJ)
# data_LJ3 <- rbind(data_LJ3, data_LJ3_kaz)
# 
# aa<-ggplot(data_LJ3, aes(x=leto, y = kaz_z, group = kaz, colour = kaz)) +
#   theme_light(base_size = 14)+ 
#   geom_point(size=1.75)+geom_line(lty=1,lwd = 0.5)+
#   labs(title = paste("Vrocinski ekstremi",sep="")) +  ylab("") + xlab("leto") + 
#   scale_color_viridis(name="Legenda",discrete = TRUE) + 
#   scale_y_continuous(limits=c(-4.5,4.5)) +
#   theme(legend.text=element_text(size=15), 
#         axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
#         legend.position='bottom') 
# aa
# ggsave(paste("delo/faktorji_psenica/Vrocinski_ekstremi_casovni.png"),width = 12, height = 7)
# 
# povp_vrocinski_ekstremi <- vrocinski_ekstremi %>% group_by(lats,lons) %>%
#   summarise(period_mean = mean(kaz_z))
# ggplot() +
#   geom_tile(data=povp_vrocinski_ekstremi,aes(x=lons,y=lats,fill=period_mean)) +
#   geom_sf(data=slovenia_nuts3_mapdata, fill = NA) +
#   scale_fill_distiller(palette = "Spectral") +
#   labs(x="Geografska dolzina",y="Geografska sirina",title=paste0("faktor Vrocinski ekstremi")) +
#   coord_sf(crs = st_crs(4326)) +
#   theme_light(base_size = 17)
# ggsave(paste0("delo/faktorji_psenica/Vrocinski_ekstremi_",zac,"-",kon,".png"),width = 10, height = 7)
# 
# 
# 
# # PREZIMOVANJE IN MINIMALNE TEMPERATURE
# data_PMT <- data_z1 %>% filter(
#   kaz == "Sums_Tmin10" | kaz == "Sums_Tmin15" | kaz == "CSDI")
# prezimovanje_minT <- aggregate(kaz_z ~ lons + lats + leto, data_PMT, FUN = mean, na.rm=TRUE, na.action=na.pass)
# data_LJ4 <- prezimovanje_minT %>% filter(lats == lat_LJ, lons == lon_LJ)
# data_LJ4$kaz <- rep("faktor", by = length(data_LJ4[,1]))
# data_LJ4_kaz <- data_PMT %>% filter(lats == lat_LJ, lons == lon_LJ)
# data_LJ4 <- rbind(data_LJ4, data_LJ4_kaz)
# 
# aa<-ggplot(data_LJ4, aes(x=leto, y = kaz_z, group = kaz, colour = kaz)) +
#   theme_light(base_size = 14)+ 
#   geom_point(size=1.75)+geom_line(lty=1,lwd = 0.5)+
#   labs(title = paste("Prezimovanje in minimalne T",sep="")) +  ylab("") + xlab("leto") + 
#   scale_color_viridis(name="Legenda",discrete = TRUE) + 
#   scale_y_continuous(limits=c(-4.5,4.5)) +
#   theme(legend.text=element_text(size=15), 
#         axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
#         legend.position='bottom') 
# aa
# ggsave(paste("delo/faktorji_psenica/prezimovanje_minT_casovni.png"),width = 12, height = 7)
# 
# povp_prezimovanje_minT <- prezimovanje_minT %>% group_by(lats,lons) %>%
#   summarise(period_mean = mean(kaz_z))
# ggplot() +
#   geom_tile(data=povp_prezimovanje_minT,aes(x=lons,y=lats,fill=period_mean)) +
#   geom_sf(data=slovenia_nuts3_mapdata, fill = NA) +
#   scale_fill_distiller(palette = "Spectral") +
#   labs(x="Geografska dolzina",y="Geografska sirina",title=paste0("faktor Prezimovanje in minimalne T")) +
#   coord_sf(crs = st_crs(4326)) +
#   theme_light(base_size = 17)
# ggsave(paste0("delo/faktorji_psenica/prezimovanje_minT_",zac,"-",kon,".png"),width = 10, height = 7)
# 
# 
# 
# # VISOKE PADAVINE
# data_VP <- data_z1 %>% filter(
#     kaz == "Prec_wettest_month" | kaz == "r10mm" | kaz == "r20mm" | 
#     kaz == "rx1day" | kaz == "rx5d" | kaz == "Heavy_prec_days" | 
#     kaz == "Growing_season_prec" |  kaz == "Nongrowing_season_prec" | 
#     kaz == "Prec_warmest_quarter" | kaz == "Prec_coldest_quarter" | 
#     kaz == "Effective_prec" | kaz == "longest_wet_period" |
#     kaz == "SDII" | kaz == "wet_days" | kaz == "precip_total")
# visoke_padavine <- aggregate(kaz_z ~ lons + lats + leto, data_VP, FUN = mean, na.rm=TRUE, na.action=na.pass)
# data_LJ5 <- visoke_padavine %>% filter(lats == lat_LJ, lons == lon_LJ)
# data_LJ5$kaz <- rep("faktor", by = length(data_LJ5[,1]))
# data_LJ5_kaz <- data_VP %>% filter(lats == lat_LJ, lons == lon_LJ)
# data_LJ5 <- rbind(data_LJ5, data_LJ5_kaz)
# 
# aa<-ggplot(data_LJ5, aes(x=leto, y = kaz_z, group = kaz, colour = kaz)) +
#   theme_light(base_size = 14)+ 
#   geom_point(size=1.75)+geom_line(lty=1,lwd = 0.5)+
#   labs(title = paste("Visoke padavine",sep="")) +  ylab("") + xlab("leto") + 
#   scale_color_viridis(name="Legenda",discrete = TRUE) + 
#   scale_y_continuous(limits=c(-3,3)) +
#   theme(legend.text=element_text(size=15), 
#         axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
#         legend.position='bottom') 
# aa
# ggsave(paste("delo/faktorji_psenica/visoke_padavine_casovni.png"),width = 12, height = 7)
# 
# povp_visoke_padavine <- visoke_padavine %>% group_by(lats,lons) %>%
#   summarise(period_mean = mean(kaz_z))
# ggplot() +
#   geom_tile(data=povp_visoke_padavine,aes(x=lons,y=lats,fill=period_mean)) +
#   geom_sf(data=slovenia_nuts3_mapdata, fill = NA) +
#   scale_fill_distiller(palette = "Spectral") +
#   labs(x="Geografska dolzina",y="Geografska sirina",title=paste0("faktor Visoke padavine")) +
#   coord_sf(crs = st_crs(4326)) +
#   theme_light(base_size = 17)
# ggsave(paste0("delo/faktorji_psenica/visoke_padavine_",zac,"-",kon,".png"),width = 10, height = 7)
# 
# 
# 
# # PADAVINSKI EKSTREMI
# data_PE <- data_z1 %>% filter(
#   kaz == "r95tot" | kaz == "Very_wet_days")
# padavinski_ekstremi <- aggregate(kaz_z ~ lons + lats + leto, data_PE, FUN = mean, na.rm=TRUE, na.action=na.pass)
# data_LJ6 <- padavinski_ekstremi %>% filter(lats == lat_LJ, lons == lon_LJ)
# data_LJ6$kaz <- rep("faktor", by = length(data_LJ6[,1]))
# data_LJ6_kaz <- data_PE %>% filter(lats == lat_LJ, lons == lon_LJ)
# data_LJ6 <- rbind(data_LJ6, data_LJ6_kaz)
# 
# aa<-ggplot(data_LJ6, aes(x=leto, y = kaz_z, group = kaz, colour = kaz)) +
#   theme_light(base_size = 14)+ 
#   geom_point(size=1.75)+geom_line(lty=1,lwd = 0.5)+
#   labs(title = paste("Padavinski ekstremi",sep="")) +  ylab("") + xlab("leto") + 
#   scale_color_viridis(name="Legenda",discrete = TRUE) + 
#   scale_y_continuous(limits=c(-2.5,2.5)) +
#   theme(legend.text=element_text(size=15), 
#         axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
#         legend.position='bottom') 
# aa
# ggsave(paste("delo/faktorji_psenica/padavinski_ekstremi_casovni.png"),width = 12, height = 7)
# 
# povp_padavinski_ekstremi <- padavinski_ekstremi %>% group_by(lats,lons) %>%
#   summarise(period_mean = mean(kaz_z))
# ggplot() +
#   geom_tile(data=povp_padavinski_ekstremi,aes(x=lons,y=lats,fill=period_mean)) +
#   geom_sf(data=slovenia_nuts3_mapdata, fill = NA) +
#   scale_fill_distiller(palette = "Spectral") +
#   labs(x="Geografska dolzina",y="Geografska sirina",title=paste0("faktor Padavinski ekstremi")) +
#   coord_sf(crs = st_crs(4326)) +
#   theme_light(base_size = 17)
# ggsave(paste0("delo/faktorji_psenica/padavinski_ekstremi_",zac,"-",kon,".png"),width = 10, height = 7)
































# vsi_podatki_test<-vsi_podatki%>%
#   #group_by(leto,lats,lons) %>%
#   mutate( 
#           meanTGS = mean(na.omit(TGS)),
#           sdTGS = sd(na.omit(TGS)),
#           zTGS = (TGS-meanTGS)/sdTGS)%>%
#   relocate(c("meanTGS","sdTGS","zTGS"), .after = TGS) %>%
#           
# mutate(       
#           meanTG_of_warmest_quarter = mean(na.omit(TG_of_warmest_quarter)),
#           sdTG_of_warmest_quarter = sd(na.omit(TG_of_warmest_quarter)),
#           zTG_of_warmest_quarter =   (TG_of_warmest_quarter-meanTG_of_warmest_quarter)/sdTG_of_warmest_quarter ) %>%
#   relocate(c("meanTG_of_warmest_quarter","sdTG_of_warmest_quarter","zTG_of_warmest_quarter"), .after = TG_of_warmest_quarter)
# 
# 
# vsi_podatki_1985 <- subset(vsi_podatki_test,vsi_podatki_test$leto==1985)
# library(dplyr)
# 
# ggplot() +
#   geom_tile(data=vsi_podatki_1985,aes(x=lons,y=lats,fill=TGS)) +
#   geom_sf(data=slovenia_nuts3_mapdata, fill = NA) +
#  scale_fill_distiller(palette = "Spectral") +
#   # scale_fill_distiller(kaz_enote[k], palette = "Spectral") +
#   labs(x="Geografska dolzina",y="Geografska sirina",title=paste0("TGS 1985")) +
#   coord_sf(crs = st_crs(4326)) +
#   theme_light(base_size = 17)
# ggsave(paste0("delo/karte_psenicatest/TGS.png"),width = 10, height = 7)
#           
# ggplot() +
#   geom_tile(data=vsi_podatki_1985,aes(x=lons,y=lats,fill=zTGS)) +
#   geom_sf(data=slovenia_nuts3_mapdata, fill = NA) +
#   scale_fill_distiller(palette = "Spectral", limits=c(-3,3)) +
#   # scale_fill_distiller(kaz_enote[k], palette = "Spectral") +
#   labs(x="Geografska dolzina",y="Geografska sirina",title=paste0("TGS 1985")) +
#   coord_sf(crs = st_crs(4326)) +
#   theme_light(base_size = 17)
# ggsave(paste0("delo/karte_psenicatest/zTGS.png"),width = 10, height = 7)
# 
# 
# vsi_podatki_1985<- vsi_podatki_1985%>%
#   #group_by(leto,lats,lons) %>%
#   mutate( 
#     meanTGS1985 = mean(na.omit(TGS)),
#     sdTGS1985 = sd(na.omit(TGS)),
#     zTGS1985= (TGS- meanTGS1985)/sdTGS1985)%>%
#   relocate(c("meanTGS1985","sdTGS1985","zTGS1985"), .after = TGS)
# 
# ggplot() +
#   geom_tile(data=vsi_podatki_1985,aes(x=lons,y=lats,fill=zTGS1985)) +
#   geom_sf(data=slovenia_nuts3_mapdata, fill = NA) +
#   scale_fill_distiller(palette = "Spectral", limits=c(-3,3)) +
#   # scale_fill_distiller(kaz_enote[k], palette = "Spectral") +
#   labs(x="Geografska dolzina",y="Geografska sirina",title=paste0("TGS 1985")) +
#   coord_sf(crs = st_crs(4326)) +
#   theme_light(base_size = 17)
# ggsave(paste0("delo/karte_psenicatest/zTGS1985_z_vrednost.png"),width = 10, height = 7)
# 
# 
# ggplot() +
#   geom_tile(data=vsi_podatki_1985,aes(x=lons,y=lats,fill=TG_of_warmest_quarter)) +
#   geom_sf(data=slovenia_nuts3_mapdata, fill = NA) +
#   scale_fill_distiller(palette = "Spectral") +
#   # scale_fill_distiller(kaz_enote[k], palette = "Spectral") +
#   labs(x="Geografska dolzina",y="Geografska sirina",title=paste0("TTG_of_warmest_quarter1985")) +
#   coord_sf(crs = st_crs(4326)) +
#   theme_light(base_size = 17)
# ggsave(paste0("delo/karte_psenicatest/TG_of_warmest_quarter.png"),width = 10, height = 7)
# 
# ggplot() +
#   geom_tile(data=vsi_podatki_1985,aes(x=lons,y=lats,fill=zTG_of_warmest_quarter)) +
#   geom_sf(data=slovenia_nuts3_mapdata, fill = NA) +
#   scale_fill_distiller(palette = "Spectral") +
#   # scale_fill_distiller(kaz_enote[k], palette = "Spectral") +
#   labs(x="Geografska dolzina",y="Geografska sirina",title=paste0("TG_of_warmest_quarter1985")) +
#   coord_sf(crs = st_crs(4326)) +
#   theme_light(base_size = 17)
# ggsave(paste0("delo/karte_psenicatest/zTG_of_warmest_quarter.png"),width = 10, height = 7)
# 
# 
# 
# 
# vsi_podatki_povp<-vsi_podatki%>%
#   group_by(lats,lons) %>%
#   summarise(TGS1981_2010=mean(TGS),
#             z1981_2010 = mean(zTGS)) %>%
#   group_by()%>%
#   mutate(meanTGS=mean(TGS1981_2010),
#          sdTGS = sd(na.omit(TGS1981_2010)) )
#   
# ggplot() +
#   geom_tile(data=vsi_podatki_povp,aes(x=lons,y=lats,fill=TGS1981_2010)) +
#   geom_sf(data=slovenia_nuts3_mapdata, fill = NA) +
#   scale_fill_distiller(palette = "Spectral", limits = c(7,19)) +
#   # scale_fill_distiller(kaz_enote[k], palette = "Spectral") +
#   labs(x="Geografska dolzina",y="Geografska sirina",title=paste0("TGS1981_2010")) +
#   coord_sf(crs = st_crs(4326)) +
#   theme_light(base_size = 17)
# ggsave(paste0("delo/karte_psenicatest/TGS_1981_2010.png"),width = 10, height = 7)
# 

#vsi_podatki_povp2<-vsi_podatki_povp%>%
# 
# 
# 
# mutate( 
#     meanTGS = mean(na.omit(TGS)),
#     sdTGS = sd(na.omit(TGS)),
#     zTGS = (TGS-meanTGS)/sdTGS)%>%
#   relocate(c("meanTGS","sdTGS","zTGS"), .after = TGS) %>%
# 
# 
# 
#   











data0_cor <- cor(data0)
data0_cor # korelacije izra?unam


pairs(data0[, c(2:4,6:9,11,30)], main = "Temp rastna PC1")
pairs(data0[, c(5,10,13,33)], main = "Temp vro?ina PC2")
pairs(data0[, c(12,32)], main = "Temp vlaga Noc PC3")
pairs(data0[, c(20:21,23:24)], main = "Pozeba PC1")
pairs(data0[, c(22,25:26)], main = "Prezimovanje PC2")
pairs(data0[, c(15:19,27:29,34)], main = "Visoke padavine PC2")

# data(mtcars) 
# Scatter_Matrix <- ggpairs(data0,columns = c(1, 4:7), 
#                           title = "Scatter Plot Matrix", 
#                           axisLabels = "show") 
# ggsave("Scatter plot matrix.png", Scatter_Matrix, width = 7, 
#        height = 7, units = "in") 
# Scatter_Matrix

res.pca <- prcomp(data0[,c(2:49)], scale = TRUE)
print(res.pca)
summary(res.pca)
eig.val<-get_eig(res.pca)
eig.val<-get_eigenvalue(res.pca)
eig.val
fviz_eig(res.pca, col.var="blue")

loadings <- res.pca$rotation
rotated <- principal(data0[,c(2:49)], nfactors = 9, rotate = "varimax", scores = TRUE)
rot_load <- as.matrix(rotated$loadings)
str(rot_load)
print(rotated$scores[1:5,])  # Scores returned by principal()


# Visualize the rotated PCA loadings
fviz_pca_var(rotated, 
             col.var = "cos2", # Color by contribution
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE, # Avoid text overlapping
             title = "PCA with Varimax Rotation")

# REZULTATI PCA ZA SPREMENLJIVKE
# PCA results can be assesed with regard to variables (sport disciplines) and individuals (athletes). 
# Firstly, I will conduct extraction of results for variables. 
# For that purpose get_pca_var() is used to provide a list of matrices containing all the results for the active variables 
# (coordinates, correlation between variables and axes, squared cosine, and contributions).

var <- get_pca_var(res.pca)
var
# Cos2 is called square cosine (squared coordinates) and corresponds to the quality of representation of variables. 
# Cos2 of variables on all the dimensions using the corrplot package is displayed below, as well as bar plot of variables cos2 using the function fviz_cos2().

head(var$cos2)
corrplot(var$cos2, is.corr=FALSE)

fviz_cos2(res.pca, choice = "var", axes = 1:2)

fviz_pca_var(res.pca,
             col.var = "cos2", # Color by the quality of representation
             gradient.cols = c("darkorchid4", "gold", "darkorange"),
             repel = TRUE
)

# Effective_prec, TG_of_warmest_quarter have very high cos2, which implies a good representation of the variable on the principal component. 
# ==> variables are positioned close to the circumference of the correlation circle. 

# Very_wet_days has the lowest cos2, which indicates that the variable is not perfectly represented by the PCs. 
# ==> In this case the variable is close to the center of the circle - it is less important for the first components.

# Contributions of variables to PC1
a<-fviz_contrib(res.pca, choice = "var", axes = 1)
# Contributions of variables to PC2
b<-fviz_contrib(res.pca, choice = "var", axes = 2)
c<-fviz_contrib(res.pca, choice = "var", axes = 3)
d<-fviz_contrib(res.pca, choice = "var", axes = 4)
grid.arrange(a,b, ncol=2, top='Contribution of the variables to the first two PCs')
grid.arrange(c,d, ncol=2, top='Contribution of the variables to the 3rd and 4th two PCs')
# Contrib is a contribution of variables. The function fviz_contrib() is used to draw a bar plot of variable contributions for the most significant dimensions, therefore PC1 and PC2.
# The red dashed line on the graph above indicates the expected average contribution. 
# For a certain component, a variable with a contribution exceeding this benchmark is considered as important in contributing to the component. 
# It can be seen that the variables effective_prec and sums_Tmin10 contribute the most to the first two dimensions.
# Sums_Tmax32 and Prec_driest_month contribute the most to the 3rd and 4th dimension.

# Considering the calculated PCs, I will summarize them in clusters via k-means clustering method. 
# For that purpose, I will use eclust() function with 4 clusters as an assumption and autoplot() for 2D observations.
kmeans<-eclust(data0, k=4)
autoplot(res.pca, data=kmeans, colour="cluster")




