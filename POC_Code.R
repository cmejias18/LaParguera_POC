
#### 1. Import libraries to be used. #####
library(naniar)
library(tidyverse)
library(dplyr)
library(lubridate)
library(patchwork)
library(rstudioapi)
library(ggmap)
library(rgdal)
library(raster)
library(ggplot2)
library(ggbiplot)
library(sf)
library(ggrepel)
library(ggcorrplot)
library(ggbeeswarm)
library(ggsignif)
library(ggpubr)
library(factoextra)
library(FSA)
library(dunn.test)
library(gt)
library(plotly)
library(rstatix)
#library(ggstatplot)
library(devtools)


#### 2. Insert Data set, Convert Sampling Date column from character to Date format, Rename Columns, Filter sites of interest #############

data <-  read_csv("DB_AnalysisR.csv")
data$`Sampling Date` = as_date(data$'Sampling Date',format="%m/%d/%Y")
# names(POC)
# dim(POC)
# class(POC)
# head(POC)
# tail(POC)
# summary(data)


data2 <- data %>%  
  dplyr::rename(
    Date = "Sampling Date", 
    Site = "Sample Site", 
    Lon = "Long", 
    FVol = "Filt.Vol (L)", 
    CIR = "13C", 
    NIR = "15N", 
    POC = "POC (mg/m3)", 
    PON = "PON (mg/m3)", 
    Temp = "Temp C (ITS90)", 
    DIC = "DIC (UMOL/KG)", 
    TA = "TA (UMOL/KG)",
    Chl = "Chl (ug/L)") %>%
  dplyr::filter(Site =="AB"|
           Site == "BB"|
           Site == "NQ"|
           Site == "VL") %>% 
  subset(Date > "2018-01-01" & Date < "2019-12-31") %>% 
  replace_with_na_all(condition=~.x==-999)


#### 3. Figure 1: Map & Stations ########


register_google(key = "AIzaSyBAkWwL25eBEQFJf4n9Rc2sKQbxSAqu7To")

cols <- c("VL" = '#023858', "AB" = '#0570b0', "NQ" = '#74a9cf', "BB" = '#74c476') #blue
# cols2 <- c("VL" = '#fef0d9', "AB" = '#fdcc8a', "NQ" = '#fc8d59', "BB" = '#d7301f') #orange
# cols3 <- c("VL" = '#016571', "AB" = '#80cdc1', "NQ" = '#dfc27d', "BB" = '#a6611a') #terra
# cols4 <- c("VL" = '#081d58', "AB" = '#225ea8', "NQ" = '#41b6c4', "BB" = '#c7e9b4') #yellow
cols5 <- c("VL" = '#053061', "AB" = '#4393c3', "NQ" = '#f4a582', "BB" = '#b2162b') #diverging
shapesx <- c("VL" = '21', "AB" = '22', "NQ" = '23', "BB" = '24')



dataMap <- data2 %>% 
  dplyr::filter(Site =="VL") %>% 
  subset(Date > "2018-01-01" & Date < "2019-12-31") %>% 
  replace_with_na_all(condition=~.x==-999)

LPmap <- get_googlemap(c(lon = -67.04, lat = 17.92),
                       zoom = 12, 
                       maptype = "satellite", 
                       force = FALSE)

dataMapZoom <- data2 %>% 
  dplyr::filter(Site =="AB"|
                  Site == "BB"|
                  Site == "NQ") %>% 
  subset(Date > "2018-01-01" & Date < "2019-12-31") %>% 
  replace_with_na_all(condition=~.x==-999)

LPmapzoom <- get_googlemap(c(lon = -67.030399, lat = 17.962708),
                       zoom = 14, 
                       maptype = "satellite", 
                       force = FALSE)


ggmap(LPmapzoom) +
  theme_classic()+
  theme(
    axis.title.x = element_text(size = 22),
    axis.text.x = element_text(size = 22),
    axis.title.y = element_text(size = 22), 
    axis.text.y = element_text(size = 22), 
    #legend.position = "right", 
    legend.title = element_blank(), 
    legend.text = element_text(size=15))+
  scale_y_continuous(limits = c(17.95, 17.98), breaks = seq(17.95, 17.98, by = 0.01))+
  scale_x_continuous(limits = c(-67.055, -67.010), breaks = seq(-67.055, -67.010, by = 0.015)) +
  ylab("Latitude")+
  xlab("Longitude")+
  geom_point(dataMapZoom, mapping=aes(x= Lon, y = Lat, shape=Site, fill=Site, size=5, stroke = 2), size = 10, color="black", show.legend = FALSE)+
  scale_shape_manual(values=c(21,24,23), 
                     breaks = c("BB","NQ", "AB"), 
                     labels = c("Acidification Buoy", "Enrique", "Bio Bay"))+ 
  scale_fill_manual(values=cols5, 
                    breaks = c("Veril","Acidification Buoy", "Enrique", "Bio Bay"))+
  
  geom_text_repel(aes(x = -67.0510, y = 17.9540),
                  label = "AB", fill = "#636363", color = "white", fontface = "bold",
                  min.segment.length = 0, segment.size = 1, segment.colour= "gray", alpha = 1, 
                  size = 10, direction = "both", force= 2, nudge_x = -0.01, nudge_y = 0, stat = "unique")+
  
  geom_text_repel(aes(x = -67.0504, y = 17.9550),
                  label = "NQ", fill = "#636363", color = "white", fontface = "bold",
                  min.segment.length = 0, segment.size = 1, segment.colour= "gray", alpha = 1, 
                  size = 10, direction = "both", force= 2, nudge_x = 0, nudge_y = 0.01, stat = "unique")+
  
  geom_text_repel(aes(x = -67.0142, y = 17.9720),
                  label = "BB", fill = "#636363", color = "white", fontface = "bold",
                  min.segment.length = 0, segment.size = 1, segment.colour= "gray", alpha = 1,
                  size = 10, direction = "both", force= 2, nudge_x = 0, nudge_y = -0.01, stat = "unique")

ggmap(LPmap) +
  theme_classic()+
  theme(
    axis.title.x = element_text(size = 22),
    axis.text.x = element_text(size = 22),
    axis.title.y = element_text(size = 22), 
    axis.text.y = element_text(size = 22), 
    #legend.position = "right", 
    legend.title = element_blank(), 
    legend.text = element_text(size=15))+
  scale_y_continuous(limits = c(17.85, 17.98), breaks = seq(17.85, 17.98, by = 0.04))+
  scale_x_continuous(limits = c(-67.12, -66.95), breaks = seq(-67.10, -66.95, by = 0.04)) +
  ylab("Latitude")+
  xlab("Longitude")+
  geom_point(dataMap, mapping=aes(x= Lon, y = Lat, shape=Site,  fill=Site, stroke = 1), size= 20, color="white", show.legend = FALSE)+
  scale_shape_manual(values=22, 
                     breaks = "VL", 
                     labels = "Veril")+
  scale_fill_manual(values="#053061")+
  #breaks = "Veril"
  geom_text_repel(aes(x = -67.0213, y = 17.8702), 
                  label = "VL",color = "white", fontface = "bold", 
                  segment.size = 1, segment.colour = "gray", alpha = 1,
                  size = 20, direction = "both", force= 10, nudge_x = 0, nudge_y = -0.01, stat = "unique")
  
  # geom_text_repel(aes(x = -67.0510, y = 17.9540),
  #                 label = "AB", fill = "#636363", color = "white", fontface = "bold",
  #                 segment.size = 1, segment.colour= "gray", alpha = 1, 
  #                 size = 10, direction = "both", force= 10, nudge_x = -0.01, nudge_y = 0, stat = "unique")+
  # 
  # geom_text_repel(aes(x = -67.0504, y = 17.9550),
  #                 label = "NQ", fill = "#636363", color = "white", fontface = "bold",
  #                 segment.size = 1, segment.colour= "gray", alpha = 1, 
  #                 size = 10, direction = "both", force= 10, nudge_x = 0, nudge_y = 0.01, stat = "unique")+
  # 
  # geom_text_repel(aes(x = -67.0142, y = 17.9720),
  #                 label = "BB", fill = "#636363", color = "white", fontface = "bold",
  #                 segment.size = 1, segment.colour= "gray", alpha = 1, 
  #                 size = 10, direction = "both", force= 10, nudge_x = 0, nudge_y = -0.01, stat = "unique") 
  
  #guides(shape=guide_legend(override.aes=list(shape=c(22,25,24,21), fill=cols5, size = 5)), size = "none")
  


   
#### 4. Group by Site and Date, Calculate Average and Std.Dev ########


data_mean <- data2 %>% 
  group_by(Site,Date) %>% 
  dplyr::summarize(
            d13C_m=mean(CIR),
            d13C_std=sd(CIR),
            d15N_m=mean(NIR),
            d15N_std=sd(NIR),
            POC_m=mean(POC),
            POC_std=sd(POC),
            PON_m=mean(PON),
            PON_std=sd(PON), 
            Temp_m = mean(Temp),
            Temp_std = sd(Temp),
            Sal_m = mean(Sal), 
            Sal_std = sd(Sal), 
            DIC_m = mean(DIC), 
            DIC_std = sd(DIC), 
            TA_m = mean(TA), 
            TA_std = sd(TA),  
            pH_m = mean(pH), 
            pH_std = sd(pH),
            Chl_m = mean(Chl), 
            Chl_std = sd(Chl), 
            NTU_m = mean(NTU), 
            NTU_std = sd(NTU),
            Lat_m = mean(Lat), 
            Lon_m = mean(Lon))



#### 5. Figure 2: Timeseries Plot : Graph Mean & STDEV ########

TempGraph2 <- ggplot(data_mean, aes(x = Date , y = Temp_m, colour=Site, shape=Site)) + 
  ggtitle("A")+
  geom_point(size = 4) + 
  scale_shape_manual(values=c(25, 21, 24, 22))+
  scale_colour_manual(values=cols5)+
  scale_fill_manual(values=cols5)+
  geom_line()+
  geom_errorbar(aes(ymin = Temp_m, ymax = Temp_m))+
  theme_classic()+
  labs(x = NULL, y = "Temperature (°C)") +
  scale_x_date(limits = as.Date(c("2018-07-01", "2019-08-01"))) +
  theme(
    axis.text.x = element_blank(),
    axis.title.x = element_blank(), 
    axis.title.y = element_text(size=18),
    axis.text.y = element_text(size = 18), 
    axis.line.x = element_blank(),
    axis.ticks.x = element_blank())+
  #guides(shape=guide_legend(override.aes=list(fill=cols5)), colour="none")
  guides(colour=guide_legend(override.aes=list(shape=c(25, 21, 24, 22))), shape="none")

TempGraph2

SalGraph2 <- ggplot(data_mean, aes(x = Date , y = Sal_m, colour=Site, shape = Site)) +
  ggtitle("B")+
  geom_point(size=4) + 
  geom_line()+
  geom_errorbar(aes(ymin = Sal_m, ymax = Sal_m))+
  scale_shape_manual(values=c(25, 21, 24, 22))+
  scale_colour_manual(values=cols5)+
  theme_classic()+
  labs(x = NULL, y = "Salinity (PSU)") +
  scale_x_date(limits = as.Date(c("2018-07-01", "2019-08-01"))) +
  theme(
    #axis.text.x = element_text(size=12), 
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size=18),
    axis.text.y = element_text(size=18), 
    axis.line.x = element_blank(),
    axis.ticks.x = element_blank())+
  guides(shape=guide_legend(override.aes=list(fill=cols5)), colour="none")

#guides(colour=guide_legend(override.aes=list(shape=c(25, 21, 24, 22))), shape="none")

pHGraph2 <- ggplot(data_mean, aes(x = Date , y = pH_m, colour=Site, shape = Site)) + 
  ggtitle("C")+
  geom_point(size=4) + 
  geom_line()+
  geom_errorbar(aes(ymin = pH_m, ymax = pH_m))+
  scale_shape_manual(values=c(25, 21, 24, 22))+
  scale_colour_manual(values=cols5)+
  theme_classic()+
  labs(x = NULL, y = expression(paste("pH "[T]))) + 
  scale_x_date(limits = as.Date(c("2018-07-01", "2019-08-01"))) +
  #scale_y_continuous(limits = c(7.4, 8.4), breaks = seq(7.4, 8.4, by = 0.2))+
  theme(
    axis.text.x = element_text(size=16),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size=18),
    axis.text.y = element_text(size =18))+
  guides(shape=guide_legend(override.aes=list(fill=cols5)), colour="none")
  #guides(colour=guide_legend(override.aes=list(shape=shapes)), shape="none")

POCGraph2 <- ggplot(data_mean, aes(x = Date , y = POC_m, colour=Site, shape = Site)) + 
  ggtitle("D")+
  geom_point(size=4) + 
  geom_line()+
  scale_shape_manual(values=c(25, 21, 24, 22))+
  scale_colour_manual(values=cols5)+
  geom_errorbar(aes(ymin = POC_m - POC_std, ymax = POC_m + POC_std)) +
  theme_classic() +
  labs(x = NULL, y = expression(paste("POC ", (mg/m^3)))) +
  scale_x_date(limits = as.Date(c("2018-07-01", "2019-08-01")))+
  theme(
    axis.text.x = element_blank(),
    axis.title.x = element_blank(), 
    axis.title.y = element_text(size=18),
    axis.text.y = element_text(size=18), 
    axis.line.x = element_blank(),
    axis.ticks.x = element_blank())+
  guides(shape=guide_legend(override.aes=list(fill=cols5)), colour="none")

PONGraph2 <- ggplot(data_mean, aes(x = Date , y = PON_m, colour=Site, shape=Site)) + 
  ggtitle("E")+
  geom_point(size=4) +
  geom_line()+
  scale_shape_manual(values=c(25, 21, 24, 22))+
  scale_colour_manual(values=cols5)+
  geom_errorbar(aes(ymin = PON_m - PON_std, ymax =PON_m + PON_std)) + 
  theme_classic() +
  labs(x = NULL, y = expression(paste("PON ", (mg/m^3)))) +
  scale_x_date(limits = as.Date(c("2018-07-01 ", "2019-08-01"))) +
  theme(
    #axis.text.x = element_text(size=12), 
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size=18),
    axis.text.y = element_text(size = 18), 
    axis.line.x = element_blank(),
    axis.ticks.x = element_blank())+
  guides(shape=guide_legend(override.aes=list(fill=cols5)), colour="none")

CIRGraph2 <- ggplot(data_mean, aes(x = Date , y = d13C_m , colour=Site, shape=Site)) +   
  ggtitle("F")+
  geom_point(size = 4) + 
  geom_line() +
  scale_shape_manual(values=c(25, 21, 24, 22))+
  scale_colour_manual(values=cols5) +
  geom_errorbar(aes(ymin = d13C_m - d13C_std, ymax = d13C_m + d13C_std)) + 
  theme_classic() + 
  labs(x = NULL, y = expression(paste(delta^{13}, "C (\u2030)"))) +
  scale_x_date(limits = as.Date(c("2018-07-01 ", "2019-08-01")))+
  theme(
    #axis.text.x = element_text(size=12), 
    axis.text.x = element_blank(),
    axis.title.x = element_blank(), 
    axis.title.y = element_text(size=18), 
    axis.text.y = element_text(size = 18), 
    axis.line.x = element_blank(),
    axis.ticks.x = element_blank())+
  guides(shape=guide_legend(override.aes=list(fill=cols5)), colour="none")
 
NIRGraph2 <- ggplot(data_mean, aes(x = Date , y = d15N_m, colour=Site, shape=Site)) + 
  ggtitle("G")+
  geom_point(size=4) + 
  geom_line()+
  scale_shape_manual(values=c(25, 21, 24, 22))+
  scale_colour_manual(values=cols5) +
  geom_errorbar(aes(ymin = d15N_m - d15N_std, ymax = d15N_m + d15N_std)) + 
  theme_classic() +
  labs(x = NULL, y = expression(paste(delta^{15}, "N (\u2030)"))) +
  scale_x_date(limits = as.Date(c("2018-07-01 ", "2019-08-01"))) +
  theme(
    axis.text.x = element_text(size=16), 
    axis.title.x = element_blank(), 
    axis.title.y = element_text(size=18),
    axis.text.y = element_text(size =18))+
  guides(shape=guide_legend(override.aes=list(fill=cols5)), colour="none")



TimeseriesPlot<-((TempGraph2/SalGraph2/pHGraph2)|(POCGraph2/PONGraph2/CIRGraph2/NIRGraph2)) +
  plot_layout(guides='collect') &
  theme(legend.position = "bottom",
        legend.title = element_text(size=16), 
        legend.text = element_text(size=16))
  
  ggsave("ParameterTimeSeries_Final.pdf", TimeseriesPlot, width = 12, height = 7)

TimeseriesPlot



#### 6. Figure 3: Parameter Gradient (BoxPlot) Box Plot ("O" in Odata_mean corresponds to "Ordered" for the boxplot station order) ########

#To remove outliers: geom_box....,outlier.shape = NA

Odata_mean <- data_mean
Odata_mean$Site <- factor(Odata_mean$Site, c("BB", "NQ", "AB", "VL"))

TempBox<-ggplot(Odata_mean, mapping = aes(Site, Temp_m)) +  
  ggtitle("A")+
  geom_boxplot(aes(fill = Site))+
  geom_jitter(width = 0.1)+
  scale_fill_manual(values=cols5)+
  theme_classic()+
  labs(x = NULL, y = "Temperature (°C)")+
  #scale_y_continuous(limits = c(25,32))+
  theme(
    axis.text.x = element_blank(),
    axis.title.y = element_text(size=16),
    axis.text.y = element_text(size = 16),
    axis.line.x = element_blank(),
    axis.ticks.x = element_blank(),
    legend.position = "none")

SalBox<-ggplot(Odata_mean, mapping = aes(Site, Sal_m)) +  
  ggtitle("B")+
  geom_boxplot(aes(fill = Site))+
  geom_jitter(width = 0.1)+
  scale_fill_manual(values=cols5)+
  theme_classic()+
  labs(x = NULL, y = "Salinity (PSU)")+
  #scale_y_continuous(limits = c(32,40))+
  theme(
    axis.text.x = element_blank(),
    axis.title.y = element_text(size=16),
    axis.text.y = element_text(size = 16),
    axis.line.x = element_blank(),
    axis.ticks.x = element_blank(),
    legend.position = "none")

pHBox<-ggplot(Odata_mean, mapping = aes(Site, pH_m)) +  
  ggtitle("C")+
  geom_boxplot(aes(fill = Site))+
  geom_jitter(width = 0.1)+
  scale_fill_manual(values=cols5)+
  theme_classic()+
  labs(x = NULL, y = expression(paste("pH "[T])))+
  theme(
    axis.title.y = element_text(size=16),
    axis.text.y = element_text(size = 16),
    axis.text.x = element_text(size = 16), 
    legend.position = "none")

POCBox<-ggplot(Odata_mean, aes(Site, POC_m)) +  
  ggtitle("D")+
  geom_boxplot(aes(fill = Site))+
  geom_jitter(width = 0.1)+
  scale_fill_manual(values=cols5)+
  theme_classic()+
  labs(x = NULL, y = expression(paste("POC ", (mg/m^3)))) +
  #scale_y_continuous(limits = c(0,800))+
  theme(
    axis.text.x = element_blank(),
    axis.title.y = element_text(size=16),
    axis.text.y = element_text(size = 16),
    axis.line.x = element_blank(),
    axis.ticks.x = element_blank(),
    legend.position = "none")

PONBox<-ggplot(Odata_mean, mapping = aes(Site, PON_m)) +  
  ggtitle("E")+
  geom_boxplot(aes(fill = Site))+
  geom_jitter(width = 0.1)+
  scale_fill_manual(values=cols5)+
  theme_classic()+
  labs(x = NULL, y = expression(paste("PON ", (mg/m^3))))+
  theme(
    axis.text.x = element_blank(),
    axis.title.y = element_text(size=16),
    axis.text.y = element_text(size = 16),
    axis.line.x = element_blank(),
    axis.ticks.x = element_blank(),
    legend.position = "none")

CIRBox<-ggplot(Odata_mean, mapping = aes(Site, d13C_m)) +  
  ggtitle("F")+
  geom_boxplot(aes(fill = Site))+
  geom_jitter(width = 0.1)+
  scale_fill_manual(values=cols5)+
  theme_classic()+
  labs(x = NULL, y = expression(paste(delta^{13}, "C (\u2030)")))+
  #scale_y_continuous(limits = c(-60,0))+
  theme(
    axis.text.x = element_blank(),
    axis.title.y = element_text(size=16),
    axis.text.y = element_text(size = 16),
    axis.line.x = element_blank(),
    axis.ticks.x = element_blank(),
    legend.position = "none")

NIRBox<-ggplot(Odata_mean, mapping = aes(Site, d15N_m)) +  
  ggtitle("G")+
  geom_boxplot(aes(fill = Site))+
  geom_jitter(width = 0.1)+
  scale_fill_manual(values=cols5)+
  theme_classic()+
  labs(x = NULL, y = expression(paste(delta^{15}, "N (\u2030)")))+
  #scale_y_continuous(limits = c(0,15))+
  theme(
    axis.title.y = element_text(size=16),
    axis.text.y = element_text(size = 16),
    axis.text.x = element_text(size = 16), 
    legend.position = "none")


BoxPlot <- ((TempBox/SalBox/pHBox)|(POCBox/PONBox/CIRBox/NIRBox)) +
  plot_layout(guides = "collect") 

ggsave("ParameterGradient_Final.pdf", BoxPlot, width = 12, height = 7)

BoxPlot



#### 7. Kruskal & Dunn Test ####
#KT = tests for detectable differences in POC between sites (i.e., POC detectably varies between sites)


kruskal.test(POC_m~as.factor(Site),data=data_mean) 
kruskal.test(PON_m~as.factor(Site),data=data_mean)
kruskal.test(d13C_m~as.factor(Site),data=data_mean)
kruskal.test(d15N_m~as.factor(Site),data=data_mean)
kruskal.test(Temp_m~as.factor(Site),data=data_mean)
kruskal.test(Sal_m~as.factor(Site),data=data_mean)
kruskal.test(pH_m~as.factor(Site),data=data_mean)


#DT = if there are differences in POC between sites, this tests for detectable differences in POC between every site vs site comparison (i.e., POC_biobay > POC_Enrique, POC_biobay>POC_AB, POC_biobay>POC_veril, and etc.)

TempDT<-dunnTest(Temp_m~as.factor(Site),data=data_mean,method="bonferroni") 
SalDT<-dunnTest(Sal_m~as.factor(Site),data=data_mean,method="bonferroni") 
pHDT<-dunnTest(pH_m~as.factor(Site),data=data_mean,method="bonferroni") 
POCDT<-dunnTest(POC_m~as.factor(Site),data=data_mean,method="bonferroni") 
PONDT<-dunnTest(PON_m~as.factor(Site),data=data_mean,method="bonferroni") 
CIRDT<-dunnTest(d13C_m~as.factor(Site),data=data_mean,method="bonferroni") 
NIRDT<-dunnTest(d15N_m~as.factor(Site),data=data_mean,method="bonferroni") 

#Individual Dunn Test Results Table (rounding and organization)

options(scipen=999)
Temp_p=bind_cols("Comparison"=TempDT$res$Comparison,"Temperature"=round(TempDT$res$P.adj,3))
Sal_p=bind_cols("Comparison"=SalDT$res$Comparison,"Salinity"=round(SalDT$res$P.adj,3))
pH_p=bind_cols("Comparison"=pHDT$res$Comparison,"pH"=round(pHDT$res$P.adj,3))
POC_p=bind_cols("Comparison"=POCDT$res$Comparison,"POC"=round(POCDT$res$P.adj,3))
PON_p=bind_cols("Comparison"=PONDT$res$Comparison,"PON"=round(PONDT$res$P.adj,3))
CIR_p=bind_cols("Comparison"=CIRDT$res$Comparison,"13C"=round(CIRDT$res$P.adj,3))
NIR_p=bind_cols("Comparison"=NIRDT$res$Comparison,"15N"=round(NIRDT$res$P.adj,3))



#Merging above data frames
DN_list <- list(Temp_p, Sal_p, pH_p, POC_p, PON_p, CIR_p, NIR_p) %>% 
  reduce(full_join, by='Comparison')

write.table(DN_list, file = "DunnTestStats2.csv", sep = ",", quote = FALSE, row.names = F)

DN_list %>% 
  gt() %>% 
  #tab_header(
    #title= ('TITLE')) %>% 
  # fmt_number(
  #   columns = 2:8,
  #   1.000 == ">0.999", 
  #   0.000 == "<0.001") %>% 
  opt_align_table_header(align = "left") %>%  
  cols_label(Comparison = "Sites") %>% 
  cols_width(
    Comparison ~ px(100), 
    Temperature ~ px(100), 
    Salinity ~ px(100), 
    pH ~ px(100), 
    POC ~ px(100), 
    PON ~ px(100), 
    '13C' ~ px(100), 
    '15N' ~ px(100)) %>% 
  tab_style(
    style =  
      cell_text(weight = "bold"),
    locations = 
      cells_body(
      columns = 2:8,
      rows =  Temperature <0.05)) 
     

  




#### 8. Figure 4: PCA ########

data_pca <- data2 %>% 
  group_by(Site,Date) %>% 
  dplyr::summarize(
    d13C = mean(CIR),
    d15N = mean(NIR),
    POC = mean(POC),
    PON = mean(PON),
    Temp = mean(Temp),
    Sal = mean(Sal), 
    pH = mean(pH))


 #Testpca <- prcomp(data_pca[, unlist(lapply(data_pca, is.numeric))])

data_pca = na.omit(data_pca)
pca <- prcomp(data_pca[c(3:9)], center = TRUE, scale. = TRUE)
pca
summary(pca)

PCAPlot<-ggbiplot(pca, obs.scale = 1, var.scale = 1, size=10, 
                  group = data_pca$Site,
                  varname.size=5,
                  labels.size=5, 
                  ellipse = TRUE, 
                  circle = FALSE, 
                  label.repel = TRUE, 
                  loadings.colour = "black")+
  #scale_shape_manual(name="Site", values=c(21,24,25,22))+
  scale_shape_manual(name="Site", values=c(23,21,24, 22))+
  scale_color_manual(name="Site", values=cols5) +
  geom_point(aes(colour=data_pca$Site, shape=data_pca$Site))+
  labs(
    x = "PC1 (45.5%)", 
    y = "PC2 (18.1%)")+
  theme_classic()+
  theme(
    legend.direction = 'horizontal',
    legend.position = 'top',
    legend.text = element_text(size=18),
    legend.title = element_text(size = 18), 
    axis.title.y = element_text(size=22),
    axis.text.y = element_text(size = 22, color = "black"),
    axis.title.x = element_text(size=22),
    axis.text.x = element_text(size = 22, color = "black"))+
  guides(shape=guide_legend(override.aes=list(fill=cols5)), colour="none", size="none")
#guides(colours=guide_legend(override.aes=list(shape=c(25,21,24,22))), colour="none", size="none")
PCAPlot




ggsave("PrincipalComponentAnalysis.pdf", PCAPlot, width = 12, height = 7)


#Scree Plot
ggscreeplot(pca)
fviz_eig(pca)


rm(list = setdiff(ls(), "LPmap"))

