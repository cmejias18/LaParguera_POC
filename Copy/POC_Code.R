
#### 1. Import libraries #####

library(rstudioapi)
library(ggmap)
library(tidyverse)
library(dplyr)
library(runner)
library(lubridate)
library(naniar)
library(dunn.test)
library(ggpubr)
library(ggbiplot)
library(patchwork)
library(gt)
library(factoextra)
library(performance)
library(see)
library(ggspatial)
library(ggsn)
library(ggplot2)


#### 2. Insert Datasets (.csv) ####

data <- read_csv("DB_AnalysisR_OutlierRemoved.csv", col_types = cols(`Sampling Date` = col_date(format = "%m/%d/%Y")))%>%   
  dplyr::rename(
    Date = "Sampling Date", 
    Site = "Sample Site", 
    Depth = "Total Depth (m)",
    Lon = "Long", 
    FVol = "Filt.Vol (L)", 
    CIR = "13C", 
    NIR = "15N", 
    POC = "POC (mg/m3)", 
    PON = "PON (mg/m3)", 
    Temp = "Temp C (ITS90)", 
    DIC = "DIC (UMOL/KG)",
    TA = "TA (UMOL/KG)") %>%
  dplyr::mutate(
    "CN" = POC/PON) %>% 
  dplyr::filter(Site =="AB"|
           Site == "BB"|
           Site == "NQ"|
           Site == "VL") %>% 
  dplyr::select(-c(TA, DIC)) %>%
  subset(Date > "2018-01-01" & Date < "2019-12-31") %>% 
  replace_with_na_all(condition=~.x==-999)

#Precipitation Data and Cumulative Sum Calculation (14 days)

precip <- read_csv("Precip2018_2019.csv") 

precip14days <-
  precip %>% 
  mutate(Date = as.Date(Date, "%m/%d/%Y"))%>% 
  complete(Date = seq.Date(min(Date), max(Date), by="day")) %>% 
  mutate(Rain_cm_14days = sum_run(x = Rain_cm, k = 14, idx = as.Date(Date, format = "%m/%d/%Y"))) 


data2 <- merge(data, precip14days, by.x = "Date", all.x = TRUE)%>% 
  dplyr::select(-c(Month, MonthNo, Rain_in, Rain_cm))



#### 3. Figure 1: Map & Stations ########

register_google(key = ...)

cols5 <- c("BB" = '#b2162b', "NQ" = '#f4a582', "AB" = '#92c5de', "VL" = '#2166ac')


##Zoom Out Map = Figure 1A##

dataMap <- data %>% 
  dplyr::filter(Site =="VL"|
                  Site =="AB"|
                  Site == "BB"|
                  Site == "NQ") %>% 
  subset(Date > "2018-01-01" & Date < "2019-12-31") %>% 
  replace_with_na_all(condition=~.x==-999)


LPmap <- get_googlemap(c(lon = -67.04, lat = 17.92),
                       zoom = 12, 
                       maptype = "satellite", 
                       force = FALSE)


F1<-ggmap(LPmap) +
  geom_rect(aes(xmin = -66.9810, xmax = -66.9625, ymin = 17.8640, ymax = 17.8652), colour = "white", fill = "white", linewidth = 0.5)+ 
  annotate("text", x = -66.9705, y = 17.86, label= "200 m", colour="white", fontface="bold", size=5)+
  theme_classic()+
  ggtitle("A")+
  theme(
    plot.title=element_text(size=20),
    axis.title.x = element_text(size = 22),
    axis.text.x = element_text(size = 22),
    axis.title.y = element_text(size = 22), 
    axis.text.y = element_text(size = 22))+
  scale_y_continuous(limits = c(17.86, 17.99), breaks = seq(17.86, 17.99, by = 0.04))+
  scale_x_continuous(limits = c(-67.10, -66.96), breaks = seq(-67.10, -66.96, by = 0.04)) +
  ylab("Latitude")+
  xlab("Longitude")+
  geom_point(dataMap, mapping=aes(x= Lon, y = Lat, shape=Site, fill=Site, stroke = 1), size= 4, color="white", show.legend = FALSE)+
  scale_shape_manual(values=c(21,22,24,23), 
                     breaks = c("BB","VL","NQ","AB"), 
                     labels = c("Bio Bay","Veril","Enrique","Acidification Buoy"))+ 
  scale_fill_manual(values=cols5, 
                    breaks = c("Veril","Acidification Buoy","Enrique","Bio Bay"))+
  annotate("text", x = -67.0492, y = 17.9488, label= "AB", colour="white", fontface="bold", size=7)+
  annotate("text", x = -67.0492, y = 17.9605, label= "NQ", colour="white", fontface="bold", size=7)+
  annotate("text", x = -67.0142, y = 17.9680, label= "BB", colour="white", fontface="bold", size=7)+
  annotate("text", x = -67.0213, y = 17.8635, label= "VL", colour="white", fontface="bold", size=9)+
  annotation_north_arrow(location="tl", style = north_arrow_minimal(line_width = 2, line_col = "white", text_col = "white", text_size = 15))

  
F1
ggsave("LPMapRev.pdf", F1, width = 12, height = 7)


##Zoom In Map = Figure 1B##
 
dataMapZoom <- data %>% 
  dplyr::filter(Site =="AB"|
                Site == "NQ") %>% 
  subset(Date > "2018-01-01" & Date < "2019-12-31") %>% 
  replace_with_na_all(condition=~.x==-999)


LPmapzoom <- get_googlemap(c(lon = -67.030399, lat = 17.962708),
                       zoom = 14, 
                       maptype = "satellite", 
                       force = FALSE)


F2<-ggmap(LPmapzoom) +
  geom_rect(aes(xmin = -67.0439, xmax = -67.042, ymin = 17.945, ymax = 17.9452), fill = "white", colour = "white", linewidth = 0.5)+
  annotate("text", x = -67.0428, y = 17.9446, label= "200 m", colour="white", fontface="bold", size=5)+
  theme_classic()+
  ggtitle("B") +
  theme(
    plot.title=element_text(size=20),
    axis.title.x = element_text(size = 22),
    axis.text.x = element_text(size = 22),
    axis.title.y = element_text(size = 22), 
    axis.text.y = element_text(size = 22))+ 
  scale_y_continuous(limits = c(17.9445, 17.9645), breaks = seq(17.94, 17.965, by = 0.008))+
  scale_x_continuous(limits = c(-67.056, -67.041), breaks = seq(-67.055, -67.04, by = 0.006))+
  ylab("Latitude")+
  xlab("Longitude")+
  geom_point(dataMapZoom, mapping=aes(x= Lon, y = Lat, shape=Site, fill=Site, size=5, stroke = 2), size = 8, color="white", show.legend = FALSE)+
  scale_shape_manual(values=c(21,24,23), 
                     breaks = c("BB","NQ","AB"), 
                     labels = c("Bio Bay", "Enrique", "Acidification Buoy"))+ 
  scale_fill_manual(values=cols5, 
                    breaks = c("Veril","Acidification Buoy", "Enrique", "Bio Bay"))+
  annotate("text", x = -67.0510, y = 17.9516, label= "AB", colour="white", fontface="bold", size=10)+
  annotate("text", x = -67.0504, y = 17.9576, label= "NQ", colour="white", fontface="bold", size=10)+
  annotation_north_arrow(location="tl", style = north_arrow_minimal(line_width = 2, line_col = "white", text_col = "white", text_size = 15))

F2
ggsave("LPMapZoomRev.pdf", F2, width = 12, height = 7)


##BB Zoom In Map = Figure 1C##

dataMapZoomBB <- data %>% 
  dplyr::filter(Site =="BB") %>% 
  subset(Date > "2018-01-01" & Date < "2019-12-31") %>% 
  replace_with_na_all(condition=~.x==-999)


LPmapzoomBB <- get_googlemap(c(lon = -67.014, lat = 17.971),
                           zoom = 16, 
                           maptype = "satellite", 
                           force = FALSE)


F3<-ggmap(LPmapzoomBB) +
  geom_rect(aes(xmin = -67.010, xmax = -67.0091, ymin = 17.9663, ymax = 17.9664), fill = "white", colour = "white", linewidth = 0.5)+ 
  annotate("text", x = -67.0096, y = 17.9660, label= "100 m", colour="white", fontface="bold", size=5)+
  theme_classic()+
  ggtitle("C") +
  theme(
    plot.title=element_text(size=20),
    axis.title.x = element_text(size = 22),
    axis.text.x = element_text(size = 22),
    axis.title.y = element_text(size = 22), 
    axis.text.y = element_text(size = 22))+ 
  scale_y_continuous(limits = c(17.9660, 17.9760), breaks = seq(17.9660, 17.9760, by = 0.004))+
  scale_x_continuous(limits = c(-67.018, -67.009), breaks = seq(-67.018, -67.009, by = 0.004))+
  ylab("Latitude")+
  xlab("Longitude")+
  geom_point(dataMapZoomBB, mapping=aes(x= Lon, y = Lat, shape=Site, fill=Site, size=5, stroke = 2), size = 8, color="white", show.legend = FALSE)+
  scale_shape_manual(values=21, 
                     breaks = "BB", 
                     labels = "Bio Bay")+ 
  scale_fill_manual(values=cols5, 
                    breaks = "Bio Bay")+
  annotate("text", x = -67.014, y = 17.9715, label= "BB", colour="white", fontface="bold", size=10)+
  annotation_north_arrow(location="tl", style = north_arrow_minimal(line_width = 2, line_col = "white", text_col = "white", text_size = 15))

F3

LPMap_FinalRev<-F1+F2+F3

LPMap_FinalRev[[1]] = LPMap_FinalRev[[1]] + theme(axis.title.x = element_blank())
LPMap_FinalRev[[2]] = LPMap_FinalRev[[2]] + theme(axis.title.y = element_blank(),)
LPMap_FinalRev[[3]] = LPMap_FinalRev[[3]] + theme(axis.title.y = element_blank(),
                                        axis.title.x = element_blank())

LPMap_FinalRev
ggsave("LPMap_FinalRev.pdf", LPMap_FinalRev, width = 20, height = 7)


#### 4. Group by Site and Date, Calculate Average and Std.Dev ########

data_mean <- data2 %>% 
  group_by(Date,Site) %>% 
  dplyr::summarise(
            d13C_M=mean(CIR),
            d13C_SD=sd(CIR),
            d15N_M=mean(NIR),
            d15N_SD=sd(NIR),
            POC_M=mean(POC),
            POC_SD=sd(POC),
            PON_M=mean(PON),
            PON_SD=sd(PON), 
            Temp = mean(Temp),
            Sal = mean(Sal), 
            pH = mean(pH),
            CN_M = mean(CN),
            CN_SD =sd(CN),
            TDepth = mean(Depth),
            Precipitation = mean(Rain_cm_14days),
            Lat = mean(Lat), 
            Lon = mean(Lon))
data_mean$Site <- factor(data_mean$Site, level = c("BB", "NQ", "AB", "VL"))


#### 5. Figure 2: Timeseries Plot ########

TempGraph2 <- ggplot(data_mean, aes(x = Date , y = Temp, fill= Site, shape=Site)) + 
  ggtitle("A")+
  geom_point(size = 4) + 
  scale_shape_manual(values=c(21, 24, 23, 22))+ 
  scale_fill_manual(values=cols5)+              
  geom_line()+
  geom_errorbar(aes(ymin = Temp, ymax = Temp))+
  theme_classic()+
  labs(x = NULL, y = "Temperature (°C)") +
  scale_x_date(limits = as.Date(c("2018-07-01", "2019-08-01"))) +
  theme(
    axis.text.x = element_blank(),
    axis.title.x = element_blank(), 
    axis.title.y = element_text(size=15),
    axis.text.y = element_text(size = 15), 
    axis.line.x = element_blank(),
    axis.ticks.x = element_blank())
  
SalGraph2 <- ggplot(data_mean, aes(x = Date , y = Sal, fill= Site, shape=Site)) +
  ggtitle("B")+
  geom_point(size=4) + 
  geom_line()+
  geom_errorbar(aes(ymin = Sal, ymax = Sal))+
  scale_shape_manual(values=c(21, 24, 23, 22))+
  scale_fill_manual(values=cols5)+ 
  theme_classic()+
  labs(x = NULL, y = "Salinity") +
  scale_x_date(limits = as.Date(c("2018-07-01", "2019-08-01"))) +
  theme(
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size=15),
    axis.text.y = element_text(size=15), 
    axis.line.x = element_blank(),
    axis.ticks.x = element_blank())

pHGraph2 <- ggplot(data_mean, aes(x = Date , y = pH, fill=Site, shape = Site)) + 
  ggtitle("D")+
  geom_point(size=4) + 
  geom_line()+
  geom_errorbar(aes(ymin = pH, ymax = pH))+
  scale_shape_manual(values=c(21, 24, 23, 22))+
  scale_fill_manual(values=cols5)+ 
  theme_classic()+
  labs(x = NULL, y = expression(paste("pH "[T]))) + 
  scale_x_date(limits = as.Date(c("2018-07-01", "2019-08-01"))) +
  #scale_y_continuous(limits = c(7.4, 8.4), breaks = seq(7.4, 8.4, by = 0.2))+
  theme(
    legend.position= "top",
    axis.text.x = element_text(size=16),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size=15),
    axis.text.y = element_text(size = 15))
    #axis.ticks.x = element_blank())

PRCPraph2 <- ggplot(data_mean, aes(x = Date, y = Precipitation)) +
  ggtitle("C")+
  # geom_bar(stat="identity", fill="darkgray", width = 8)+
  geom_point(size = 4, color = "darkgray") +
  geom_line(color = "darkgreen")+
  theme_classic()+
  labs(x = NULL, y = "Precipitation (cm)") +
  #scale_x_date(limits = as.Date(c("2018-07-01", "2019-08-01"))) +
  theme(
    axis.text.x = element_blank(), 
    axis.title.x = element_blank(), 
    axis.title.y = element_text(size=14.5),
    axis.text.y = element_text(size =15), 
    axis.line.x = element_blank(),
    axis.ticks.x = element_blank())

POCGraph2 <- ggplot(data_mean, aes(x = Date , y = POC_M, fill=Site, shape = Site)) + 
  ggtitle("E")+
  geom_point(size=4) + 
  geom_line()+
  scale_shape_manual(values=c(21, 24, 23, 22))+
  scale_fill_manual(values=cols5)+ 
  geom_errorbar(aes(ymin = POC_M - POC_SD, ymax = POC_M + POC_SD)) +
  theme_classic() +
  labs(x = NULL, y = expression(paste("POC ", (mg/m^3)))) +
  scale_x_date(limits = as.Date(c("2018-07-01", "2019-08-01")))+
  theme(
    axis.text.x = element_blank(),
    axis.title.x = element_blank(), 
    axis.title.y = element_text(size=15),
    axis.text.y = element_text(size=15), 
    axis.line.x = element_blank(),
    axis.ticks.x = element_blank())

PONGraph2 <- ggplot(data_mean, aes(x = Date , y = PON_M, fill=Site, shape=Site)) + 
  ggtitle("F")+
  geom_point(size=4) +
  geom_line()+
  scale_shape_manual(values=c(21, 24, 23, 22))+
  scale_fill_manual(values=cols5)+ 
  geom_errorbar(aes(ymin = PON_M - PON_SD, ymax =PON_M + PON_SD)) + 
  theme_classic() +
  labs(x = NULL, y = expression(paste("PON ", (mg/m^3)))) +
  scale_x_date(limits = as.Date(c("2018-07-01 ", "2019-08-01"))) +
  theme(
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size=15),
    axis.text.y = element_text(size = 15), 
    axis.line.x = element_blank(),
    axis.ticks.x = element_blank())

CIRGraph2 <- ggplot(data_mean, aes(x = Date , y = d13C_M , fill=Site, shape=Site)) +   
  ggtitle("G")+
  geom_point(size = 4) + 
  geom_line() +
  scale_shape_manual(values=c(21, 24, 23, 22))+
  scale_fill_manual(values=cols5)+ 
  geom_errorbar(aes(ymin = d13C_M - d13C_SD, ymax = d13C_M + d13C_SD)) + 
  theme_classic() + 
  labs(x = NULL, y = expression(paste(delta^{13}, "C (\u2030)"))) +
  scale_x_date(limits = as.Date(c("2018-07-01 ", "2019-08-01")))+
  theme(
    axis.text.x = element_blank(), 
    axis.title.x = element_blank(), 
    axis.title.y = element_text(size=15),
    axis.text.y = element_text(size =15), 
    axis.line.x = element_blank(),
    axis.ticks.x = element_blank())
 
NIRGraph2 <- ggplot(data_mean, aes(x = Date , y = d15N_M, fill=Site, shape=Site)) + 
  ggtitle("H")+
  geom_point(size=4) + 
  geom_line()+
  scale_shape_manual(values=c(21, 24, 23, 22))+
  scale_fill_manual(values=cols5)+ 
  geom_errorbar(aes(ymin = d15N_M - d15N_SD, ymax = d15N_M + d15N_SD)) + 
  theme_classic() +
  labs(x = NULL, y = expression(paste(delta^{15}, "N (\u2030)"))) +
  scale_x_date(limits = as.Date(c("2018-07-01 ", "2019-08-01"))) +
  theme(
    axis.text.x = element_blank(), 
    axis.title.x = element_blank(), 
    axis.title.y = element_text(size=15),
    axis.text.y = element_text(size =15), 
    axis.line.x = element_blank(),
    axis.ticks.x = element_blank())

CNGraph2 <- ggplot(data_mean, aes(x = Date , y = CN_M, fill= Site, shape=Site)) +
  ggtitle("I")+
  geom_point(size = 4) +
  scale_shape_manual(values=c(21, 24, 23, 22))+
  scale_fill_manual(values=cols5)+
  geom_line()+
  geom_errorbar(aes(ymin = CN_M - CN_SD, ymax = CN_M + CN_SD))+
  theme_classic()+
  labs(x = NULL, y = "C:N") +
  scale_x_date(limits = as.Date(c("2018-07-01", "2019-08-01"))) +
  theme(
    legend.position= "top",
    axis.text.x = element_text(size=16),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size=15),
    axis.text.y = element_text(size = 15))
    #axis.ticks.x = element_blank())


TimeseriesPlot<-((TempGraph2/SalGraph2/PRCPraph2/pHGraph2)|(POCGraph2/PONGraph2/CIRGraph2/NIRGraph2/CNGraph2)) + plot_layout(guides='collect') &
  theme(legend.position = "bottom",
        legend.title = element_text(size=16), 
        legend.text = element_text(size=16))


TimeseriesPlot
  
ggsave("ParameterTimeSeries_FinalRev.pdf", TimeseriesPlot, width = 12, height = 9)


#### 6. Figure 3: Parameter Gradient (BoxPlot)#### 

TempBox<-ggplot(data_mean, mapping = aes(Site, Temp)) +  
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

SalBox<-ggplot(data_mean, mapping = aes(Site, Sal)) +  
  ggtitle("B")+
  geom_boxplot(aes(fill = Site))+
  geom_jitter(width = 0.1)+
  scale_fill_manual(values=cols5)+
  theme_classic()+
  labs(x = NULL, y = "Salinity")+
  #scale_y_continuous(limits = c(32,40))+
  theme(
    axis.text.x = element_blank(),
    axis.title.y = element_text(size=16),
    axis.text.y = element_text(size = 16),
    axis.line.x = element_blank(),
    axis.ticks.x = element_blank(),
    legend.position = "none")

pHBox<-ggplot(data_mean, mapping = aes(Site, pH)) +  
  ggtitle("C")+
  geom_boxplot(aes(fill = Site))+
  geom_jitter(width = 0.1)+
  scale_fill_manual(values=cols5)+
  theme_classic()+
  labs(x = NULL, y = expression(paste("pH "[T])))+
  theme(
    axis.text.x = element_blank(),
    axis.title.y = element_text(size=16),
    axis.text.y = element_text(size = 16),
    axis.line.x = element_blank(),
    axis.ticks.x = element_blank(),
    legend.position = "none")

CNBox<-ggplot(data_mean, mapping = aes(Site, CN_M)) +
  ggtitle("D")+
  geom_boxplot(aes(fill = Site))+
  geom_jitter(width = 0.1)+
  scale_fill_manual(values=cols5)+
  theme_classic()+
  labs(x = NULL, y = expression(paste("C:N")))+
  theme(
    axis.title.y = element_text(size=16),
    axis.text.y = element_text(size = 16),
    axis.text.x = element_text(size = 16),
    legend.position = "none")

POCBox<-ggplot(data_mean, aes(Site, POC_M)) +  
  ggtitle("E")+
  geom_boxplot(aes(fill = Site))+
  #geom_errorbar(aes(ymin = POC_M - POC_SD, ymax = POC_M + POC_SD))+
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

PONBox<-ggplot(data_mean, mapping = aes(Site, PON_M)) +  
  ggtitle("F")+
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

CIRBox<-ggplot(data_mean, mapping = aes(Site, d13C_M)) +  
  ggtitle("G")+
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

NIRBox<-ggplot(data_mean, mapping = aes(Site, d15N_M)) +  
  ggtitle("H")+
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

BoxPlot <- ((TempBox/SalBox/pHBox/CNBox)|(POCBox/PONBox/CIRBox/NIRBox)) +
  plot_layout(guides = "collect") 

BoxPlot

ggsave("ParameterGradient_FinalRev.pdf", BoxPlot, width = 13, height = 10)



#### 7. Figure 4: PCA ########

data_pca <- data2 %>% 
  group_by(Site,Date) %>% 
  dplyr::summarize(
    d13C = mean(CIR),
    d15N = mean(NIR),
    POC = mean(POC),
    PON = mean(PON),
    Temperature = mean(Temp),
    Salinity = mean(Sal), 
    pH = mean(pH),
    "C:N" = mean(CN),
    Depth = mean(Depth),
    Precipitation = Rain_cm_14days)
data_pca$Site<-factor(data_pca$Site, c("BB", "NQ", "AB", "VL"))
data_pca = na.omit(data_pca)
pca <- prcomp(data_pca[c(3:12)], center = TRUE, scale. = TRUE)

pca
summary(pca)
fviz_eig(pca)

PCAPlot<-ggbiplot(pca, obs.scale = 2, var.scale = 2, size= 1.5, 
                  group = data_pca$Site,
                  varname.size= 5.5,
                  varname.adjust = 1.5,
                  varname.dist= 2,
                  label.repel = TRUE,
                  labels.size=5,
                  ellipse = TRUE, 
                  circle = FALSE)+ 
  geom_point(aes(shape=data_pca$Site,fill=data_pca$Site), size = 4.5)+
  scale_shape_manual(name="Site", values=c(21, 24, 23, 22))+
  scale_color_manual(name="Site", values=cols5) +
  scale_fill_manual(name="Site", values=cols5) +
  labs(
    x = "PC1 (35%)",
    y = "PC2 (16%)")+
  theme_classic()+
  theme(
    legend.direction = 'horizontal',
    legend.position = 'top',
    legend.text = element_text(size=20),
    legend.title = element_text(size = 20), 
    axis.title.y = element_text(size=22),
    axis.text.y = element_text(size = 22, color = "black"),
    axis.title.x = element_text(size=22),
    axis.text.x = element_text(size = 22, color = "black"))

seg <- which(sapply(PCAPlot$layers, function(x) class(x$geom)[1] == 'GeomSegment'))
PCAPlot$layers[[seg]]$aes_params$colour <- 'black'

txt <- which(sapply(PCAPlot$layers, function(x) class(x$geom)[1] == 'GeomText'))
PCAPlot$layers[[txt]]$aes_params$colour <- 'black'

PCAPlot

ggsave("PCA_Rev2.pdf", PCAPlot, width = 14, height = 8)


#### 8. Stats: ANOVA & Models ####

data_mean$Month = substr(data_mean$Date,6,7) 

temp0=aov(Temp~1,data=data_mean) #null model
temp1=aov(Temp~Site,data=data_mean) #Site as a predictor
temp2=aov(Temp~Month,data=data_mean) #Month as a predictor
temp3=aov(Temp~Site+Month,data=data_mean) #Site + Month as predictors
temp4=aov(Temp~Site*Month,data=data_mean) #Site + Month + Interaction as predictors
AIC(temp0,temp1,temp2,temp3,temp4) #AIC of all models included, lowest AIC is selected as the best model to fit the data
summary(temp3) #Site and Month are a significant predictor
check_model(temp3) #check selected model
TukeyHSD(temp3, conf.level=.95)

sal0=aov(Sal~1,data=data_mean) #null model
sal1=aov(Sal~Site,data=data_mean) #Site as a predictor 
sal2=aov(Sal~Month,data=data_mean) #Month as a predictor
sal3=aov(Sal~Site+Month,data=data_mean) #Site + Month as predictors
sal4=aov(Sal~Site*Month,data=data_mean) #Site + Month + Interaction as predictors
AIC(sal0,sal1,sal2,sal3,sal4) #AIC of all models included, lowest AIC is selected as the best model to fit the data
summary(sal3) #Site and Month are a significant predictor
check_model(sal3) #check selected model
TukeyHSD(sal3, conf.level=.95)

pH0=aov(pH~1,data=data_mean) #null model
pH1=aov(pH~Site,data=data_mean) #Site as a predictor
pH2=aov(pH~Month,data=data_mean) #Month as a predictor
pH3=aov(pH~Site+Month,data=data_mean) #Site + Month as predictors
pH4=aov(pH~Site*Month,data=data_mean) #Site + Month + Interaction as predictors
AIC(pH0,pH1,pH2,pH3,pH4) #AIC of all models included, pH4 has the lowest AIC but Site:month interaction is not significant so select pH3
summary(pH3)#Site and Month are a significant predictor
check_model(pH3) #check selected model
TukeyHSD(pH3, conf.level=.95) 

CN0=aov(CN_M~1,data=data_mean) #null model
CN1=aov(CN_M~Site,data=data_mean) #Site as a predictor
CN2=aov(CN_M~Month,data=data_mean) #Month as a predictor
CN3=aov(CN_M~Site+Month,data=data_mean) #Site + Month as predictors
CN4=aov(CN_M~Site*Month,data=data_mean) #Site + Month + Interaction as predictors
AIC(CN0,CN1,CN2,CN3,CN4) #AIC of all models included, lowest AIC is selected as the best model to fit the data
summary(CN1) #Site is a significant predictor
check_model(CN1) #check selected model
TukeyHSD(CN1, conf.level=.95)

poc0=aov(POC_M~1,data=data_mean) #null model
poc1=aov(POC_M~Site,data=data_mean) #Site as a predictor
poc2=aov(POC_M~Month,data=data_mean) #Month as a predictor
poc3=aov(POC_M~Site+Month,data=data_mean) #Site + Month as predictors
poc4=aov(POC_M~Site*Month,data=data_mean) #Site + Month + Interaction as predictors
AIC(poc0,poc1,poc2,poc3,poc4) #AIC of all models included, the lowest AIC is selected as the best model to fit the data
summary(poc1) #Site is a significant predictor
check_model(poc1) #check selected model
TukeyHSD(poc1, conf.level=.95) 

PON0=aov(PON_M~1,data=data_mean) #null model
PON1=aov(PON_M~Site,data=data_mean) #Site as a predictor
PON2=aov(PON_M~Month,data=data_mean) #Month as a predictor
PON3=aov(PON_M~Site+Month,data=data_mean) #Site + Month as predictors
PON4=aov(PON_M~Site*Month,data=data_mean) #Site + Month + Interaction as predictors
AIC(PON0,PON1,PON2,PON3,PON4) #AIC of all models included, the lowest AIC is selected as the best model to fit the data
summary(PON1) #Site is a significant predictor
check_model(PON1) #check selected model
TukeyHSD(PON1, conf.level=.95)

d13C0=aov(d13C_M~1,data=data_mean) #null model
d13C1=aov(d13C_M~Site,data=data_mean) #Site as a predictor
d13C2=aov(d13C_M~Month,data=data_mean) #Month as a predictor
d13C3=aov(d13C_M~Site+Month,data=data_mean) #Site + Month as predictors
d13C4=aov(d13C_M~Site*Month,data=data_mean) #Site + Month + Interaction as predictors
AIC(d13C0,d13C1,d13C2,d13C3,d13C4) #AIC of all models included, the lowest AIC is selected as the best model to fit the data
summary(d13C1) #Site is a significant predictor
check_model(d13C1) #check selected model
TukeyHSD(d13C1, conf.level=.95) 

d15N0=aov(d15N_M~1,data=data_mean) #null model
d15N1=aov(d15N_M~Site,data=data_mean) #Site as a predictor
d15N2=aov(d15N_M~Month,data=data_mean) #Month as a predictor
d15N3=aov(d15N_M~Site+Month,data=data_mean) #Site + Month as predictors
d15N4=aov(d15N_M~Site*Month,data=data_mean) #Site + Month + Interaction as predictors
AIC(d15N0,d15N1,d15N2,d15N3,d15N4) #AIC of all models included, the lowest AIC is selected as the best model to fit the data
summary(d15N4) #Site:Month interaction are a significant predictors
check_model(d15N4) #check selected model
TukeyHSD(d15N4, conf.level=.95)

######## Adding code to export table of ANOVA model summaries

library(htmltools)
library(flextable)
library(modelsummary)
#https://vincentarelbundock.github.io/modelsummary/articles/modelsummary.html

#generate a list of best AIC model summaries (Table 1)
models = list(Temperature=(temp3),
              Salinity=(sal3),
              pH=(pH3),
              POC=(poc1),
              PON=(PON1),
              d13C=(d13C1),
              d15N=(d15N4),
              "C:N" = (CN1))

models
#create table of best AIC model summaries
modelsummary(models,estimate="{p.value}",statistic=NULL,output = "Tables3.docx",title = 'p-values and summary statistics are reported for the best AIC model selected for each parameter. All site-level p-values are relative to the Bioluminescent Bay Site and all month p-values are relative to January.')


####9. Data Summary = Table 2 ####

data_sum <- data %>% 
  group_by(Site) %>% 
  dplyr::summarise(
    Depth = mean(Depth,na.rm=TRUE), 
    Tempm = mean(Temp,na.rm=TRUE),
    Temp_sd = sd(Temp,na.rm=TRUE),
    Salm = mean(Sal,na.rm=TRUE), 
    Sal_sd = sd(Sal,na.rm=TRUE), 
    pHm = mean(pH,na.rm=TRUE),
    pH_sd = sd(pH,na.rm=TRUE),
    POCm=mean(POC,na.rm=TRUE),
    POC_SD=sd(POC,na.rm=TRUE),
    PONm=mean(PON,na.rm=TRUE),
    PON_SD=sd(PON,na.rm=TRUE), 
    CNm = mean(CN,na.rm=TRUE),
    CN_SD = sd(CN,na.rm=TRUE),
    d13C=mean(CIR,na.rm=TRUE),
    d13C_SD=sd(CIR,na.rm=TRUE),
    d15N=mean(NIR,na.rm=TRUE),
    d15N_SD=sd(NIR,na.rm=TRUE)) %>% 
  dplyr::rename(Temp = Tempm) %>% 
  dplyr::rename(Sal = Salm) %>% 
  dplyr::rename(pH = pHm) %>% 
  dplyr::rename(POC = POCm) %>% 
  dplyr::rename(PON = PONm) %>% 
  dplyr::rename(CN = CNm) 

write.csv(data_sum,"C:/Users/clmej/OneDrive - University of Puerto Rico/PhD/POC Project/Analysis/LaParguera_POC\\data_sum_AB6.csv", row.names=FALSE)

#### 10. Figure 5. POC SOURCE #### Base figure adapted from Lamb et al. 2006 ####
Source <-
ggplot()+
  geom_rect(aes(xmin=3.6 , xmax=6, ymin=-27, ymax=-12), color="#a6978c", fill="#a6978c",alpha=0.3) + #Bacteria
  geom_rect(aes(xmin=3.9 , xmax=10, ymin=-24, ymax=-18), color="blue", fill="#acdbc6", alpha=0.3) + #Marine POC
  geom_rect(aes(xmin=3.9 , xmax=10, ymin=-33, ymax=-25), color="#9dcc87", fill="#9dcc87",alpha=0.3) + #Freshwater POC
  geom_rect(aes(xmin=5 , xmax=9, ymin=-24.45, ymax=-16), color="black", fill="#c8d7da",alpha=0.4) + #Marine Algae
  geom_rect(aes(xmin=5 , xmax=9, ymin=-30, ymax=-24.55), color="black", fill="#ffffff",alpha=0.3) + #Freshwater Algae
  geom_rect(aes(xmin=12 , xmax=45, ymin=-32, ymax=-21), color="#e8da97", fill="#e8da97",alpha=0.3) + #C4 Terrestrial Plants
  geom_rect(aes(xmin=20 , xmax=45, ymin=-17, ymax=-9), color="#dbe6a8", fill="#dbe6a8",alpha=0.3) + #C3 Terrestrial Plants
  geom_rect(aes(xmin=7 , xmax=27, ymin=-25.1, ymax=-22), color="#c6d4be", fill="#c6d4be",alpha=0.3) + #Freshwater DOC
  geom_rect(aes(xmin=7 , xmax=27, ymin=-32, ymax=-26), color="#c8d7da", fill="#ffffff",alpha=0.3) + #Marine DOC
  annotate("text", x = 40, y = -16, label= "C4 Terrestrial Plants", colour="black", fontface="bold", size=5)+
  annotate("text", x = 40, y = -30, label= "C3 Terrestrial Plants", colour="black", fontface="bold", size=5)+
  annotate("text", x = 23, y = -31, label= "Freshwater DOC", colour="black", fontface="bold", size=5)+
  annotate("text", x = 4.4, y = -30, label= "Freshwater POC", colour="black", fontface="bold", size=5, angle = 90)+
  annotate("text", x = 5.5, y = -27.1, label= "Freshwater Algae", colour="black", fontface="bold", size=5, angle = 90)+
  annotate("text", x = 9.5, y = -20, label= "Marine POC", colour="black", fontface="bold", size=5, angle = 90)+
  annotate("text", x = 7, y = -16.5, label= "Marine Algae", colour="black", fontface="bold", size=5)+
  annotate("text", x = 4.8, y = -12.5, label= "Bacteria", colour="black", fontface="bold", size=5)+
  annotate("text", x = 24, y = -24, label= "Marine DOC", colour="black", fontface="bold", size=5)+
  xlab("C:N")+
  ylab(expression(paste(delta^{13}, "C (\u2030)")))+
  scale_x_continuous(expand=c(0,0),limits = c(0,45),breaks=seq(-100,100,5)) + 
  scale_y_continuous(expand=c(0,0),limits = c(-34,-9),breaks=seq(-100,100,2)) +
  theme_classic()+
  theme(text=element_text(size=20))+
  geom_point(data_mean, mapping = aes(x = CN_M, y = d13C_M, size = 5, alpha = 1, fill=Site, shape=Site)) +   
  scale_shape_manual(values=c(21, 24, 23, 22))+
  scale_fill_manual(values=cols5)+
  guides(size="none", alpha = "none")+
  guides(fill = guide_legend(override.aes = list(size = 5)))

Source
ggsave("Source_Rev.pdf", Source, width = 12, height = 8)
