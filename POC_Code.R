
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

precip <- read_csv("Precip2018_2019.csv") 

precip14days <-
  precip %>% 
  mutate(Date = as.Date(Date, "%m/%d/%Y"))%>% 
  complete(Date = seq.Date(min(Date), max(Date), by="day")) %>% 
  mutate(Rain_in_14days = sum_run(x = Rain_in, k = 14, idx = as.Date(Date, format = "%m/%d/%Y"))) 


data2 <- merge(data, precip14days, by.x = "Date", all.x = TRUE)%>% 
  dplyr::select(-c(Month, MonthNo, Rain_in))

#Plot of Precipitation Rolling SumCum 14 days
Precip_plot <- ggplot(precip14days, aes(x = Date , y = Rain_in_14days)) + 
  ggtitle("A")+
  geom_point(size = 4) + 
  theme_classic()+
  labs(x = "Date", y = "Precipitation (in)")
  scale_x_date(limits = as.Date(c("2018-01-01", "2019-12-31")))

Precip_plot


#### 3. Figure 1: Map & Stations ########

register_google(key = '...')

cols5 <- c("BB" = '#b2162b', "NQ" = '#f4a582', "AB" = '#92c5de', "VL" = '#2166ac')


##Zoom Out Map##

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

LPmap <- ne_tiles(c(longitude = -67.04, latitude = 17.92),
                       zoom = 12, category="osm")



F1<-ggmap(LPmap) +
  geom_rect(aes(xmin = -67.056, xmax = -67.041, ymin = 17.9445, ymax = 17.9645), colour = "white", alpha = 0, linewidth = 1)+ 
  ggsn::scalebar(x.min = -67.00, x.max = -66.97, y.min = 17.86, y.max = 17.89, 
                 location = "bottomleft", 
                 dist = 2000, 
                 dist_unit = "m", 
                 transform = TRUE, 
                 model = "WGS84", 
                 height = 0.15, 
                 st.dist=0.2, 
                 st.bottom= FALSE, 
                 st.size = 4,  
                 st.color = "white", 
                 box.color = "black", 
                 box.fill = c("black", "white"))+
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
  annotation_north_arrow(location="tl", style = north_arrow_fancy_orienteering(line_width = 2, line_col = "white", text_col = "white", text_size = 15))

  
F1
ggsave("LPMapRv.pdf", F1, width = 12, height = 7)


##Zoom In Map##

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
  theme_classic()+
  ggtitle("B") +
  ggsn::scalebar(x.min = -67.043, x.max = -67.050, y.min = 17.945, y.max = 17.948, 
                 location = "bottomright", 
                 dist = 200, 
                 dist_unit = "m", 
                 transform = TRUE, 
                 model = "WGS84", 
                 height = 0.15, 
                 st.dist=0.3, 
                 st.bottom= FALSE, 
                 st.size = 4,  
                 st.color = "white", 
                 box.color = "black", 
                 box.fill = c("black", "white"))+
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
  annotation_north_arrow(location="tl", style = north_arrow_fancy_orienteering(line_width = 2, line_col = "white", text_col = "white", text_size = 15))

F2
ggsave("LPMapZoomRv.pdf", F2, width = 12, height = 7)


##BB Zoom In Map##

dataMapZoomBB <- data %>% 
  dplyr::filter(Site =="BB") %>% 
  subset(Date > "2018-01-01" & Date < "2019-12-31") %>% 
  replace_with_na_all(condition=~.x==-999)


LPmapzoomBB <- get_googlemap(c(lon = -67.014, lat = 17.971),
                           zoom = 16, 
                           maptype = "satellite", 
                           force = FALSE)


F3<-ggmap(LPmapzoomBB) +
  theme_classic()+
  ggtitle("C") +
  ggsn::scalebar(x.min = -67.009, x.max = -67.012, y.min = 17.9662, y.max = 17.9670,
                 location = "bottomright",
                 dist = 100,
                 dist_unit = "m",
                 transform = TRUE,
                 model = "WGS84",
                 height = 0.35,
                 st.dist= 0.5,
                 st.bottom= FALSE,
                 st.size = 4,
                 st.color = "white",
                 box.color = "black",
                 box.fill = c("black", "white"))+
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
  annotation_north_arrow(location="tl", style = north_arrow_fancy_orienteering(line_width = 2, line_col = "white", text_col = "white", text_size = 15))

F3

LPMap_FinalRev<-F1+F2+F3

LPMap_FinalRev[[1]] = LPMap_Rev[[1]] + theme(axis.title.x = element_blank())
LPMap_FinalRev[[2]] = LPMap_Rev[[2]] + theme(axis.title.y = element_blank(), )
LPMap_FinalRev[[3]] = LPMap_Rev[[3]] + theme(axis.title.y = element_blank(),
                                        axis.title.x = element_blank())

LPMap_FinalRev
ggsave("LPMap_FinalRev.pdf", LPMap_FinalRev, width = 20, height = 7)


#### 4. Group by Site and Date, Calculate Average and Std.Dev ########

data_mean <- data %>% 
  group_by(Site,Date) %>% 
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
            Lat = mean(Lat), 
            Lon = mean(Lon))
data_mean$Site <- factor(data_mean$Site, level = c("BB", "NQ", "AB", "VL"))
print(data_mean)

#### 5. Figure 2: Timeseries Plot : Graph Mean & STDEV ########

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
    axis.title.y = element_text(size=18),
    axis.text.y = element_text(size = 18), 
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
  labs(x = NULL, y = "Salinity (PSU)") +
  scale_x_date(limits = as.Date(c("2018-07-01", "2019-08-01"))) +
  theme(
    #axis.text.x = element_text(size=12), 
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size=18),
    axis.text.y = element_text(size=18), 
    axis.line.x = element_blank(),
    axis.ticks.x = element_blank())


pHGraph2 <- ggplot(data_mean, aes(x = Date , y = pH, fill=Site, shape = Site)) + 
  ggtitle("C")+
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
    axis.text.x = element_text(size=16),
    #axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size=18),
    axis.text.y = element_text(size =18))


POCGraph2 <- ggplot(data_mean, aes(x = Date , y = POC_M, fill=Site, shape = Site)) + 
  ggtitle("D")+
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
    axis.title.y = element_text(size=18),
    axis.text.y = element_text(size=18), 
    axis.line.x = element_blank(),
    axis.ticks.x = element_blank())


PONGraph2 <- ggplot(data_mean, aes(x = Date , y = PON_M, fill=Site, shape=Site)) + 
  ggtitle("E")+
  geom_point(size=4) +
  geom_line()+
  scale_shape_manual(values=c(21, 24, 23, 22))+
  scale_fill_manual(values=cols5)+ 
  geom_errorbar(aes(ymin = PON_M - PON_SD, ymax =PON_M + PON_SD)) + 
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
    axis.ticks.x = element_blank())


CIRGraph2 <- ggplot(data_mean, aes(x = Date , y = d13C_M , fill=Site, shape=Site)) +   
  ggtitle("F")+
  geom_point(size = 4) + 
  geom_line() +
  scale_shape_manual(values=c(21, 24, 23, 22))+
  scale_fill_manual(values=cols5)+ 
  geom_errorbar(aes(ymin = d13C_M - d13C_SD, ymax = d13C_M + d13C_SD)) + 
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
    axis.ticks.x = element_blank())
 

NIRGraph2 <- ggplot(data_mean, aes(x = Date , y = d15N_M, fill=Site, shape=Site)) + 
  ggtitle("G")+
  geom_point(size=4) + 
  geom_line()+
  scale_shape_manual(values=c(21, 24, 23, 22))+
  scale_fill_manual(values=cols5)+ 
  geom_errorbar(aes(ymin = d15N_M - d15N_std, ymax = d15N_M + d15N_std)) + 
  theme_classic() +
  labs(x = NULL, y = expression(paste(delta^{15}, "N (\u2030)"))) +
  scale_x_date(limits = as.Date(c("2018-07-01 ", "2019-08-01"))) +
  theme(
    axis.text.x = element_text(size=16), 
    axis.title.x = element_blank(), 
    axis.title.y = element_text(size=18),
    axis.text.y = element_text(size =18))

CNGraph2 <- ggplot(data_mean, aes(x = Date , y = CN_m, fill= Site, shape=Site)) +
  ggtitle("X")+
  geom_point(size = 4) +
  scale_shape_manual(values=c(21, 24, 23, 22))+
  scale_fill_manual(values=cols5)+
  geom_line()+
  geom_errorbar(aes(ymin = CN_m - CN_std, ymax = CN_m + CN_std))+
  theme_classic()+
  labs(x = NULL, y = "C:N") +
  scale_x_date(limits = as.Date(c("2018-07-01", "2019-08-01"))) +
  theme(
    legend.position= "top",
    axis.text.x = element_text(size=16),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size=18),
    axis.text.y = element_text(size = 18),
    #axis.line.x = element_blank(),
    axis.ticks.x = element_blank())


TimeseriesPlot<-((TempGraph2/SalGraph2/pHGraph2)|(POCGraph2/PONGraph2/CIRGraph2/NIRGraph2)) + plot_layout(guides='collect') &
  theme(legend.position = "bottom",
        legend.title = element_text(size=16), 
        legend.text = element_text(size=16))

TimeseriesPlot2<-((TempGraph2/SalGraph2/pHGraph2/CNGraph2)|(POCGraph2/PONGraph2/CIRGraph2/NIRGraph2)) + 
  plot_layout(guides='collect') &
  theme(legend.position = "bottom",
        legend.title = element_text(size=16), 
        legend.text = element_text(size=16))

TimeseriesPlot
TimeseriesPlot2
  
ggsave("ParameterTimeSeries_Final.pdf", TimeseriesPlot, width = 12, height = 9)


#### 6. Figure 3: Parameter Gradient (BoxPlot) Box Plot#### 


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
    axis.title.y = element_text(size=18),
    axis.text.y = element_text(size = 18),
    axis.line.x = element_blank(),
    axis.ticks.x = element_blank(),
    legend.position = "none")


SalBox<-ggplot(data_mean, mapping = aes(Site, Sal)) +  
  ggtitle("B")+
  geom_boxplot(aes(fill = Site))+
  geom_jitter(width = 0.1)+
  scale_fill_manual(values=cols5)+
  theme_classic()+
  labs(x = NULL, y = "Salinity (PSU)")+
  #scale_y_continuous(limits = c(32,40))+
  theme(
    axis.text.x = element_blank(),
    axis.title.y = element_text(size=18),
    axis.text.y = element_text(size = 18),
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
    axis.title.y = element_text(size=18),
    axis.text.y = element_text(size = 18),
    # axis.text.x = element_blank(),
    axis.text.x = element_text(size = 18), 
    legend.position = "none")


POCBox<-ggplot(data_mean, aes(Site, POC_M)) +  
  ggtitle("D")+
  geom_boxplot(aes(fill = Site))+
  #geom_errorbar(aes(ymin = POC_M - POC_SD, ymax = POC_M + POC_SD))+
  geom_jitter(width = 0.1)+
  scale_fill_manual(values=cols5)+
  theme_classic()+
  labs(x = NULL, y = expression(paste("POC ", (mg/m^3)))) +
  #scale_y_continuous(limits = c(0,800))+
  theme(
    axis.text.x = element_blank(),
    axis.title.y = element_text(size=18),
    axis.text.y = element_text(size = 18),
    axis.line.x = element_blank(),
    axis.ticks.x = element_blank(),
    legend.position = "none")


PONBox<-ggplot(data_mean, mapping = aes(Site, PON_M)) +  
  ggtitle("E")+
  geom_boxplot(aes(fill = Site))+
  geom_jitter(width = 0.1)+
  scale_fill_manual(values=cols5)+
  theme_classic()+
  labs(x = NULL, y = expression(paste("PON ", (mg/m^3))))+
  theme(
    axis.text.x = element_blank(),
    axis.title.y = element_text(size=18),
    axis.text.y = element_text(size = 18),
    axis.line.x = element_blank(),
    axis.ticks.x = element_blank(),
    legend.position = "none")


CIRBox<-ggplot(data_mean, mapping = aes(Site, d13C_M)) +  
  ggtitle("F")+
  geom_boxplot(aes(fill = Site))+
  geom_jitter(width = 0.1)+
  scale_fill_manual(values=cols5)+
  theme_classic()+
  labs(x = NULL, y = expression(paste(delta^{13}, "C (\u2030)")))+
  #scale_y_continuous(limits = c(-60,0))+
  theme(
    axis.text.x = element_blank(),
    axis.title.y = element_text(size=18),
    axis.text.y = element_text(size = 18),
    axis.line.x = element_blank(),
    axis.ticks.x = element_blank(),
    legend.position = "none")


NIRBox<-ggplot(data_mean, mapping = aes(Site, d15N_M)) +  
  ggtitle("G")+
  geom_boxplot(aes(fill = Site))+
  geom_jitter(width = 0.1)+
  scale_fill_manual(values=cols5)+
  theme_classic()+
  labs(x = NULL, y = expression(paste(delta^{15}, "N (\u2030)")))+
  #scale_y_continuous(limits = c(0,15))+
  theme(
    axis.title.y = element_text(size=18),
    axis.text.y = element_text(size = 18),
    axis.text.x = element_text(size = 18), 
    legend.position = "none")

CNBox<-ggplot(data_mean, mapping = aes(Site, CN_m)) +
  ggtitle("X")+
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
CNBox

BoxPlot <- ((TempBox/SalBox/pHBox)|(POCBox/PONBox/CIRBox/NIRBox)) +
  plot_layout(guides = "collect") 

# #BoxPlot2 <- ((TempBox/SalBox/pHBox/CNBox)|(POCBox/PONBox/CIRBox/NIRBox)) +
#   plot_layout(guides = "collect") 

BoxPlot
BoxPlot2

ggsave("ParameterGradient_Final.pdf", BoxPlot, width = 12, height = 9)



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
    TDepth = mean(Depth),
    Precipitation = Rain_in_14days)
data_pca$Site<-factor(data_pca$Site, c("BB", "NQ", "AB", "VL"))
data_pca = na.omit(data_pca)
#pca <- prcomp(data_pca[c(3:9)], center = TRUE, scale. = TRUE) #initial PCA
pca <- prcomp(data_pca[c(3:12)], center = TRUE, scale. = TRUE) #to include C:N, Depth, and precipitation

pca
summary(pca)
fviz_eig(pca)

PCAPlot<-ggbiplot(pca, obs.scale = 2.2, var.scale = 1, size=2, 
                  group = data_pca$Site,
                  varname.size=5,
                  labels.size=5,
                  ellipse = TRUE, 
                  circle = FALSE,
                  label.repel = TRUE)+ 
  geom_point(aes(shape=data_pca$Site,fill=data_pca$Site), size = 8)+
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

ggsave("PCA_Rev.pdf", PCAPlot, width = 12, height = 7)


#### 8. Stats: ANOVA & Models ####

data_mean$Month = substr(data_mean$Date,6,7)

temp0=aov(Temp~1,data=data_mean) #null model
temp1=aov(Temp~Site,data=data_mean) #Site as a predictor of Temp
temp2=aov(Temp~Month,data=data_mean) #Month as a predictor of Temp
temp3=aov(Temp~Site+Month,data=data_mean) #Site + Month as predictors of Temp
temp4=aov(Temp~Site*Month,data=data_mean) #Site + Month + Interaction as predictors of Temp
AIC(temp0,temp1,temp2,temp3,temp4) #AIC of all models included, t3 has the lowest AIC and is the best model to fit the data
summary(temp3) #Site and Month are a significant predictor of Temp
check_model(temp3) #model assumptions appear to be mostly okay here
TukeyHSD(temp3, conf.level=.95) #BB Temp > AB Temp, NQ Temp > AB Temp, VL Temp < AB Temp, VL Temp < BB Temp, VL Temp < NQ Temp

sal0=aov(Sal~1,data=data_mean) #null model
sal1=aov(Sal~Site,data=data_mean) #Site as a predictor of sal
sal2=aov(Sal~Month,data=data_mean) #Month as a predictor of sal
sal3=aov(Sal~Site+Month,data=data_mean) #Site + Month as predictors of sal
sal4=aov(Sal~Site*Month,data=data_mean) #Site + Month + Interaction as predictors of sal
AIC(sal0,sal1,sal2,sal3,sal4) #AIC of all models included, t3 has the lowest AIC and is the best model to fit the data
summary(sal3) #Site and Month are a significant predictor of sal
check_model(sal3) #model assumptions appear to be mostly okay here
TukeyHSD(sal3, conf.level=.95) #BB sal > AB sal, NQ sal > AB sal, VL sal < AB sal, VL sal < BB sal, VL sal < NQ sal

pH0=aov(pH~1,data=data_mean) #null model
pH1=aov(pH~Site,data=data_mean) #Site as a predictor of pH
pH2=aov(pH~Month,data=data_mean) #Month as a predictor of pH
pH3=aov(pH~Site+Month,data=data_mean) #Site + Month as predictors of pH
pH4=aov(pH~Site*Month,data=data_mean) #Site + Month + Interaction as predictors of pH
AIC(pH0,pH1,pH2,pH3,pH4) #AIC of all models included, pH4 has the lowest AIC but Site:month is not significant so select pH3
summary(pH3)#Site and Month are a significant predictor of pH
check_model(pH3) #model assumptions appear to be mostly okay here
TukeyHSD(pH3, conf.level=.95) #BB pH > AB pH, NQ pH > AB pH, VL pH < AB pH, VL pH < BB pH, VL pH < NQ pH

poc0=aov(POC_M~1,data=data_mean) #null model
poc1=aov(POC_M~Site,data=data_mean) #Site as a predictor of POC
poc2=aov(POC_M~Month,data=data_mean) #Month as a predictor of POC
poc3=aov(POC_M~Site+Month,data=data_mean) #Site + Month as predictors of POC
poc4=aov(POC_M~Site*Month,data=data_mean) #Site + Month + Interaction as predictors of POC
AIC(poc0,poc1,poc2,poc3,poc4) #AIC of all models included, t1 has the lowest AIC and is the best model to fit the data
summary(poc1) #Site is a significant predictor of POC
check_model(poc1) #model assumptions appear to be mostly okay here
TukeyHSD(poc1, conf.level=.95) #BB POC > AB POC, NQ POC < BB POC, VL POC < BB POC

PON0=aov(PON_M~1,data=data_mean) #null model
PON1=aov(PON_M~Site,data=data_mean) #Site as a predictor of PON
PON2=aov(PON_M~Month,data=data_mean) #Month as a predictor of PON
PON3=aov(PON_M~Site+Month,data=data_mean) #Site + Month as predictors of PON
PON4=aov(PON_M~Site*Month,data=data_mean) #Site + Month + Interaction as predictors of PON
AIC(PON0,PON1,PON2,PON3,PON4) #AIC of all models included, t3 has the lowest AIC and is the best model to fit the data
summary(PON1) #Site and Month are a significant predictor of PON
check_model(PON1) #model assumptions appear to be mostly okay here
TukeyHSD(PON1, conf.level=.95) #BB PON > AB PON, NQ PON > AB PON, VL PON < AB PON, VL PON < BB PON, VL PON < NQ PON

d13C0=aov(d13C_M~1,data=data_mean) #null model
d13C1=aov(d13C_M~Site,data=data_mean) #Site as a predictor of d13C
d13C2=aov(d13C_M~Month,data=data_mean) #Month as a predictor of d13C
d13C3=aov(d13C_M~Site+Month,data=data_mean) #Site + Month as predictors of d13C
d13C4=aov(d13C_M~Site*Month,data=data_mean) #Site + Month + Interaction as predictors of d13C
AIC(d13C0,d13C1,d13C2,d13C3,d13C4) #AIC of all models included, t3 has the lowest AIC and is the best model to fit the data
summary(d13C1) #Site and Month are a significant predictor of d13C
check_model(d13C1) #model assumptions appear to be mostly okay here
TukeyHSD(d13C1, conf.level=.95) #BB d13C > AB d13C, NQ d13C > AB d13C, VL d13C < AB d13C, VL d13C < BB d13C, VL d13C < NQ d13C

d15N0=aov(d15N_M~1,data=data_mean) #null model
d15N1=aov(d15N_M~Site,data=data_mean) #Site as a predictor of d15N
d15N2=aov(d15N_M~Month,data=data_mean) #Month as a predictor of d15N
d15N3=aov(d15N_M~Site+Month,data=data_mean) #Site + Month as predictors of d15N
d15N4=aov(d15N_M~Site*Month,data=data_mean) #Site + Month + Interaction as predictors of d15N
AIC(d15N0,d15N1,d15N2,d15N3,d15N4) #AIC of all models included, t3 has the lowest AIC and is the best model to fit the data
summary(d15N4) #Site and Month are a significant predictor of d15N
check_model(d15N4) #model assumptions appear to be mostly okay here
TukeyHSD(d15N4, conf.level=.95) #BB d15N > AB d15N, NQ d15N > AB d15N, VL d15N < AB d15N, VL d15N < BB d15N, VL d15N < NQ d15N

######## Adding code to export table of ANOVA model summaries
#install packages
install.packages("modelsummary")
install.packages("htmltools")
install.packages("flextable")

#load libraries
library(modelsummary)
#https://vincentarelbundock.github.io/modelsummary/articles/modelsummary.html

#generate a list of best AIC model summaries
models = list(Temperature=(temp3),
              Salinity=(sal3),
              pH=(pH3), #please note, the interaction for model pH4 was not significant so I have removed it
              POC=(poc1),
              PON=(PON1),
              d13C=(d13C1),
              d15N=(d15N4))


#create table of best AIC model summaries
modelsummary(models,estimate="{p.value}",statistic=NULL,output = "TableS1.docx",title = 'p-values and summary statistics are reported for the best AIC model selected for each parameter. All site-level p-values are relative to the Bioluminescent Bay Site and all month p-values are relative to January.')


#### 9. Computing correlation matrix####
correlation_matrix <- round(cor(as.data.frame(as.numeric(data_pca[complete.cases(data_pca[,2:8]),]))),1)

# Computing correlation matrix with p-values
corrp.mat <- cor_pmat(data_pca)

# Visualizing the correlation matrix using
# square and circle methods
ggcorrplot(correlation_matrix, method ="square")
ggcorrplot(correlation_matrix, method ="circle")


####10. Data Summary Table ####

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

