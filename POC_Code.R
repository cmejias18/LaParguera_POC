
#### 1. Import libraries #####

library(rstudioapi)
library(ggmap)
library(tidyverse)
library(lubridate)
library(naniar)
library(dunn.test)
library(ggpubr)
library(ggbiplot)
library(patchwork)
library(gt)
<<<<<<< HEAD
library(devtools)
=======
>>>>>>> 86968c653d5eef0f4a2187d290011dc3e23acdf9

#optional Libraries
library(factoextra)

#Pending to eliminate Libraries
library(rgdal)
library(raster)
library(sf)
library(ggrepel)
library(FSA)
library(plotly)
library(rstatix)
#library(ggstatplot)


<<<<<<< HEAD

#### 2. Insert Dataset (.csv), Convert Sampling Date column from character to Date format, Rename Columns, Filter sites of interest, Eliminate parameters that will not be used in the analysis #############

=======
#### 2. Insert Dataset (.csv), Convert Sampling Date column from character to Date format, Rename Columns, Filter sites of interest, Eliminate parameters that will not be used in the analysis #############

>>>>>>> 86968c653d5eef0f4a2187d290011dc3e23acdf9
data <- read_csv("DB_AnalysisR.csv", col_types = cols(`Sampling Date` = col_date(format = "%m/%d/%Y")))%>%  
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
  dplyr::select(-c(TA, DIC, Chl, NTU, PRCP, Wind,SSH_A)) %>% 
  subset(Date > "2018-01-01" & Date < "2019-12-31") %>% 
  replace_with_na_all(condition=~.x==-999)



#### 3. Figure 1: Map & Stations ########


<<<<<<< HEAD
register_google(key = '...')
=======
register_google(key = "...")
>>>>>>> 86968c653d5eef0f4a2187d290011dc3e23acdf9

cols5 <- c("VL" = '#053061', "AB" = '#4393c3', "NQ" = '#f4a582', "BB" = '#b2162b') #diverging
smbls <- c("VL" = '22', "AB" = '23', "NQ" = '24', "BB" = '21')


#Zoom Out Map

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
  geom_point(dataMap, mapping=aes(x= Lon, y = Lat, shape=Site,  fill=Site, stroke = 1), size= 4, color="white", show.legend = FALSE)+
  scale_shape_manual(values=c(21,22,24,23), 
                     breaks = c("BB","VL", "NQ","AB"), 
                     labels = c("Bio Bay", "Veril", "Enrique", "Acidification Buoy"))+ 
  scale_fill_manual(values=cols5, 
                    breaks = c("Veril","Acidification Buoy", "Enrique", "Bio Bay"))+
  annotate("text", x = -67.0510, y = 17.9510, label= "AB", colour="white", fontface="bold", size=6)+
  annotate("text", x = -67.0504, y = 17.9580, label= "NQ", colour="white", fontface="bold", size=6)+
  annotate("text", x = -67.0142, y = 17.9700, label= "BB", colour="white", fontface="bold", size=6)+
  annotate("text", x = -67.0213, y = 17.8655, label= "VL", colour="white", fontface="bold", size=6)


#Zoom In Map


dataMapZoom <- data %>% 
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
                     breaks = c("BB","NQ","AB"), 
                     labels = c("Bio Bay", "Enrique", "Acidification Buoy"))+ 
  scale_fill_manual(values=cols5, 
                    breaks = c("Veril","Acidification Buoy", "Enrique", "Bio Bay"))+
  annotate("text", x = -67.0510, y = 17.9520, label= "AB", colour="white", fontface="bold", size=8)+
  annotate("text", x = -67.0504, y = 17.9570, label= "NQ", colour="white", fontface="bold", size=8)+
  annotate("text", x = -67.0142, y = 17.9710, label= "BB", colour="white", fontface="bold", size=8)


#Map Inset

LPmap + inset(ggplotGrob(LPmapzoom), xmin = -76.7, xmax = -66.7, ymin = 26, ymax = 35)





#### 4. Group by Site and Date, Calculate Average and Std.Dev ########

data_mean <- data %>% 
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
            Temp = mean(Temp),
            Sal = mean(Sal), 
            pH = mean(pH), 
            Lat = mean(Lat), 
            Lon = mean(Lon))
            

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
    axis.ticks.x = element_blank())+
  guides(shape= guide_legend(override.aes = list(fill=cols5)), fill = "none")
  

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
    axis.ticks.x = element_blank())+
  guides(shape=guide_legend(override.aes=list(fill=cols5)), fill="none")


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
    axis.title.x = element_blank(),
    axis.title.y = element_text(size=18),
    axis.text.y = element_text(size =18))+
  guides(shape=guide_legend(override.aes=list(fill=cols5)), fill="none")


POCGraph2 <- ggplot(data_mean, aes(x = Date , y = POC_m, fill=Site, shape = Site)) + 
  ggtitle("D")+
  geom_point(size=4) + 
  geom_line()+
  scale_shape_manual(values=c(21, 24, 23, 22))+
  scale_fill_manual(values=cols5)+ 
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
  guides(shape=guide_legend(override.aes=list(fill=cols5)), fill="none")


PONGraph2 <- ggplot(data_mean, aes(x = Date , y = PON_m, fill=Site, shape=Site)) + 
  ggtitle("E")+
  geom_point(size=4) +
  geom_line()+
  scale_shape_manual(values=c(21, 24, 23, 22))+
  scale_fill_manual(values=cols5)+ 
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
  guides(shape=guide_legend(override.aes=list(fill=cols5)), fill="none")


CIRGraph2 <- ggplot(data_mean, aes(x = Date , y = d13C_m , fill=Site, shape=Site)) +   
  ggtitle("F")+
  geom_point(size = 4) + 
  geom_line() +
  scale_shape_manual(values=c(21, 24, 23, 22))+
  scale_fill_manual(values=cols5)+ 
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
  guides(shape=guide_legend(override.aes=list(fill=cols5)), fill="none")
 

NIRGraph2 <- ggplot(data_mean, aes(x = Date , y = d15N_m, fill=Site, shape=Site)) + 
  ggtitle("G")+
  geom_point(size=4) + 
  geom_line()+
  scale_shape_manual(values=c(21, 24, 23, 22))+
  scale_fill_manual(values=cols5)+ 
  geom_errorbar(aes(ymin = d15N_m - d15N_std, ymax = d15N_m + d15N_std)) + 
  theme_classic() +
  labs(x = NULL, y = expression(paste(delta^{15}, "N (\u2030)"))) +
  scale_x_date(limits = as.Date(c("2018-07-01 ", "2019-08-01"))) +
  theme(
    axis.text.x = element_text(size=16), 
    axis.title.x = element_blank(), 
    axis.title.y = element_text(size=18),
    axis.text.y = element_text(size =18))+
  guides(shape=guide_legend(override.aes=list(fill=cols5)), fill="none")


TimeseriesPlot<-((TempGraph2/SalGraph2/pHGraph2)|(POCGraph2/PONGraph2/CIRGraph2/NIRGraph2)) + 
  plot_layout(guides='collect') &
  theme(legend.position = "bottom",
        legend.title = element_text(size=16), 
        legend.text = element_text(size=16))

TimeseriesPlot
  
ggsave("ParameterTimeSeries_Final.pdf", TimeseriesPlot, width = 12, height = 7)


#### 6. Figure 3: Parameter Gradient (BoxPlot) Box Plot ("O" in Odata_mean corresponds to "Ordered" for the boxplot station order) ########


data_mean$Site<-factor(data_mean$Site, c("BB", "NQ", "AB", "VL"))

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
  labs(x = NULL, y = "Salinity (PSU)")+
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
    axis.title.y = element_text(size=16),
    axis.text.y = element_text(size = 16),
    axis.text.x = element_text(size = 16), 
    legend.position = "none")


POCBox<-ggplot(data_mean, aes(Site, POC_m)) +  
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


PONBox<-ggplot(data_mean, mapping = aes(Site, PON_m)) +  
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


CIRBox<-ggplot(data_mean, mapping = aes(Site, d13C_m)) +  
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


NIRBox<-ggplot(data_mean, mapping = aes(Site, d15N_m)) +  
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

BoxPlot

ggsave("ParameterGradient_Final.pdf", BoxPlot, width = 12, height = 7)



#### 7. Figure 4: PCA ########

data_pca <- data %>% 
  group_by(Site,Date) %>% 
  dplyr::summarize(
    d13C = mean(CIR),
    d15N = mean(NIR),
    POC = mean(POC),
    PON = mean(PON),
    Temp = mean(Temp),
    Sal = mean(Sal), 
    pH = mean(pH))


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
                  label.repel = TRUE)+ 
  geom_point(aes(colour=data_pca$Site, shape=data_pca$Site))+
  scale_shape_manual(name="Site", values=c(23,21,24, 22))+
  scale_colour_manual(name="Site", values=cols5) +
  labs(
    x = "PC1 (45.5%)", 
    y = "PC2 (18.1%)")+
<<<<<<< HEAD
  theme_gray()+
=======
  theme_classic()+
>>>>>>> 86968c653d5eef0f4a2187d290011dc3e23acdf9
  theme(
    legend.direction = 'horizontal',
    legend.position = 'top',
    legend.text = element_text(size=18),
    legend.title = element_text(size = 18), 
    axis.title.y = element_text(size=22),
    axis.text.y = element_text(size = 22, color = "black"),
    axis.title.x = element_text(size=22),
    axis.text.x = element_text(size = 22, color = "black"))+
  guides(shape=guide_legend(override.aes=list(fill=cols5)), colour = "none")

PCAPlot

ggsave("PrincipalComponentAnalysis.pdf", PCAPlot, width = 12, height = 7)


#Scree Plot
ggscreeplot(pca)
fviz_eig(pca) #factoextra_package


#### 8. Figure 5: Off-set Plots ########

data_mean_wide <- data_mean %>%
  pivot_wider(id_cols = Date, 
              names_from = Site,
              values_from = c(d13C_m, d15N_m, POC_m, PON_m, Temp, Sal, pH)) %>% 
  mutate(
    d13C_VL_AB = d13C_m_VL - d13C_m_AB,
    d13C_VL_NQ = d13C_m_VL - d13C_m_NQ,
    d13C_VL_BB = d13C_m_VL - d13C_m_BB,
    d15N_VL_AB = d15N_m_VL - d15N_m_AB,
    d15N_VL_NQ = d15N_m_VL - d15N_m_NQ,
    d15N_VL_BB = d15N_m_VL - d15N_m_BB,
    POC_VL_AB = POC_m_VL - POC_m_AB,
    POC_VL_NQ = POC_m_VL - POC_m_NQ,
    POC_VL_BB = POC_m_VL - POC_m_BB,
    PON_VL_AB = PON_m_VL - PON_m_AB,
    PON_VL_NQ = PON_m_VL - PON_m_NQ,
    PON_VL_BB = PON_m_VL - PON_m_BB,
    Temp_VL_AB = Temp_VL - Temp_AB,
    Temp_VL_NQ = Temp_VL - Temp_NQ,
    Temp_VL_BB = Temp_VL - Temp_BB,
    Sal_VL_AB = Sal_VL - Sal_AB,
    Sal_VL_NQ = Sal_VL - Sal_NQ,
    Sal_VL_BB = Sal_VL - Sal_BB,
    pH_VL_AB = pH_VL - pH_AB,
    pH_VL_NQ = pH_VL - pH_NQ,
    pH_VL_BB = pH_VL - pH_BB) %>%
  group_by(Date) %>%
  dplyr::summarize(
    d13C_VL_AB = d13C_m_VL - d13C_m_AB,
    d13C_VL_NQ = d13C_m_VL - d13C_m_NQ,
    d13C_VL_BB = d13C_m_VL - d13C_m_BB,
    d15N_VL_AB = d15N_m_VL - d15N_m_AB,
    d15N_VL_NQ = d15N_m_VL - d15N_m_NQ,
    d15N_VL_BB = d15N_m_VL - d15N_m_BB,
    POC_VL_AB = POC_m_VL - POC_m_AB,
    POC_VL_NQ = POC_m_VL - POC_m_NQ,
    POC_VL_BB = POC_m_VL - POC_m_BB,
    PON_VL_AB = PON_m_VL - PON_m_AB,
    PON_VL_NQ = PON_m_VL - PON_m_NQ,
    PON_VL_BB = PON_m_VL - PON_m_BB,
    Temp_VL_AB = Temp_VL - Temp_AB,
    Temp_VL_NQ = Temp_VL - Temp_NQ,
    Temp_VL_BB = Temp_VL - Temp_BB,
    Sal_VL_AB = Sal_VL - Sal_AB,
    Sal_VL_NQ = Sal_VL - Sal_NQ,
    Sal_VL_BB = Sal_VL - Sal_BB,
    pH_VL_AB = pH_VL - pH_AB,
    pH_VL_NQ = pH_VL - pH_NQ,
    pH_VL_BB = pH_VL - pH_BB)


TempGraph_Diff <- ggplot()+
  ggtitle("A")+
  geom_point(data_mean_wide, mapping=aes(x = Date, y = Temp_VL_AB, colour="VL_AB"), size=3) + 
  geom_point(data_mean_wide, mapping=aes(x = Date, y = Temp_VL_NQ, colour="VL_NQ"), size=3) + 
  geom_point(data_mean_wide, mapping=aes(x = Date, y = Temp_VL_BB, colour="VL_BB"), size=3) + 
  geom_hline(yintercept=0) + 
  scale_color_manual(values = c('VL_AB' = '#053061', "VL_NQ" = '#f4a582', "VL_BB" = '#b2162b'))+
  theme_classic() + 
  theme(legend.position = "none") +
  labs(x = NULL, y = "Temperature (°C)") +
  scale_x_date(limits = as.Date(c("2018-07-01", "2019-06-30"))) +
  #scale_y_continuous(limits = c(-2, 2))+
  theme(
    axis.text.x = element_blank(), 
    axis.title.x = element_blank(), 
    axis.ticks.x = element_blank(),
    axis.title.y = element_text(size=18),
    axis.text.y = element_text(size = 18), 
    legend.position = "none")


SalGraph_Diff <- ggplot()+
  ggtitle("B")+
  geom_point(data_mean_wide, mapping=aes(x = Date, y = Sal_VL_AB,colour="VL_AB"), size=3) + 
  geom_point(data_mean_wide, mapping=aes(x = Date, y = Sal_VL_NQ,colour="VL_NQ"), size=3) + 
  geom_point(data_mean_wide, mapping=aes(x = Date, y = Sal_VL_BB,colour="VL_BB"), size=3) + 
  geom_hline(yintercept=0) + 
  scale_color_manual(values = c('VL_AB' = '#053061', "VL_NQ" = '#f4a582', "VL_BB" = '#b2162b'))+
  theme_classic() + 
  theme(legend.position = "none") +
  labs(x = NULL, y = "Salinity (PSU)") +
  scale_x_date(limits = as.Date(c("2018-07-01", "2019-06-30"))) +
  #scale_y_continuous(limits = c(-1, 1))+
  theme(
    axis.text.x = element_blank(), 
    axis.title.x = element_blank(), 
    axis.ticks.x = element_blank(),
    axis.title.y = element_text(size = 18),
    axis.text.y = element_text(size = 18), 
    legend.position = "none")
  #scale_y_continuous(limits = c(-1, 1), breaks = seq(-1,1, by = 1))


pHGraph_Diff <- ggplot()+
  ggtitle("C")+
  geom_point(data_mean_wide, mapping=aes(x = Date, y = pH_VL_AB, colour ="VL_AB"), size=3) + 
  geom_point(data_mean_wide, mapping=aes(x = Date, y = pH_VL_NQ, colour ="VL_NQ"), size=3) + 
  geom_point(data_mean_wide, mapping=aes(x = Date, y = pH_VL_BB, colour ="VL_BB"), size=3) + 
  geom_hline(yintercept=0) + 
  scale_color_manual(values = c('VL_AB' = '#053061', "VL_NQ" = '#f4a582', "VL_BB" = '#b2162b'))+
  theme_classic() + 
  labs(x = NULL, y = expression(paste("pH "[T]))) + 
  scale_x_date(limits = as.Date(c("2018-07-01", "2019-06-30")))+
  #scale_y_continuous(limits = c(-0.5,0.5))+
  theme(
    axis.title.y = element_text(size=18),
    axis.text.y = element_text(size = 18),
    axis.text.x = element_text(size = 13), 
    legend.position = "none")


POCGraph_Diff <- ggplot() +
  ggtitle("D")+
  geom_point(data_mean_wide, mapping=aes(x = Date, y = POC_VL_AB, colour="VL_AB"), size=3) + 
  geom_point(data_mean_wide, mapping=aes(x = Date, y = POC_VL_NQ, colour="VL_NQ"), size=3) + 
  geom_point(data_mean_wide, mapping=aes(x = Date, y = POC_VL_BB, colour="VL_BB"), size=3) + 
  geom_hline(yintercept=0) + 
  scale_color_manual(values = c('VL_AB' = '#053061', "VL_NQ" = '#f4a582', "VL_BB" = '#b2162b'))+
  theme_classic() + 
  theme(legend.position = "none") +
  labs(x = NULL, y = expression(paste("POC ", (mg/m^3)))) +
  scale_x_date(limits = as.Date(c("2018-07-01", "2019-06-30"))) +
  #scale_y_continuous(limits = c(-800, 200), breaks = seq(-800,200, by = 200)) +
  theme(
    axis.text.x = element_blank(), 
    axis.title.x = element_blank(), 
    axis.ticks.x = element_blank(),
    axis.title.y = element_text(size=18),
    axis.text.y = element_text(size = 18), 
    legend.position = "none") 


PONGraph_Diff <- ggplot()+
  ggtitle("E")+
  geom_point(data_mean_wide, mapping=aes(x = Date, y = PON_VL_AB,colour="VL_AB"), size=3) + 
  geom_point(data_mean_wide, mapping=aes(x = Date, y = PON_VL_NQ,colour="VL_NQ"), size=3) + 
  geom_point(data_mean_wide, mapping=aes(x = Date, y = PON_VL_BB,colour="VL_BB"), size=3) + 
  geom_hline(yintercept=0) + 
  scale_color_manual(values = c('VL_AB' = '#053061', "VL_NQ" = '#f4a582', "VL_BB" = '#b2162b'))+
  theme_classic() + 
  theme(legend.position = "none") +
  labs(x = NULL, y = expression(paste("PON ", (mg/m^3)))) +
  scale_x_date(limits = as.Date(c("2018-07-01", "2019-06-30"))) +
  theme(
    axis.text.x = element_blank(), 
    axis.title.x = element_blank(), 
    axis.ticks.x = element_blank(),
    axis.title.y = element_text(size=18),
    axis.text.y = element_text(size = 18), 
<<<<<<< HEAD
    legend.position = "none")
=======
    legend.position = "none")+
>>>>>>> 86968c653d5eef0f4a2187d290011dc3e23acdf9
  #scale_y_continuous(limits = c(-150, 150))


CIRGraph_Diff <- ggplot(data_mean_wide) +
  ggtitle("F")+
  geom_point(aes(x = Date, y = d13C_VL_AB, colour="VL_AB"), size=3) +
  geom_point(aes(x = Date, y = d13C_VL_NQ, colour="VL_NQ"), size=3) + 
  geom_point(aes(x = Date, y = d13C_VL_BB, colour="VL_BB"), size=3) + 
  geom_hline(yintercept=0) +
  scale_color_manual(values = c('VL_AB' = '#053061', "VL_NQ" = '#f4a582', "VL_BB" = '#b2162b'))+
  theme_classic() + 
  labs(x = NULL, y = expression(paste(delta^{13}, "C (\u2030)"))) +
  scale_x_date(limits = as.Date(c("2018-07-01", "2019-06-30"))) +
  #scale_y_continuous(limits = c(-20, 20))+
  theme(
    axis.text.x = element_blank(), 
    axis.title.x = element_blank(), 
    axis.ticks.x = element_blank(),
    axis.title.y = element_text(size=18),
    axis.text.y = element_text(size = 18), 
    legend.position = "none")


NIRGraph_Diff <- ggplot() +
  ggtitle("G")+
  geom_point(data_mean_wide, mapping=aes(x=Date, y = d15N_VL_AB, colour="VL_AB"), size=3) + 
  geom_point(data_mean_wide, mapping=aes(x=Date, y = d15N_VL_NQ, colour="VL_NQ"), size=3) + 
  geom_point(data_mean_wide, mapping=aes(x=Date, y = d15N_VL_BB, colour="VL_BB"), size=3) +
  geom_hline(yintercept=0) + 
  scale_color_manual(values = c('VL_AB' = '#053061', "VL_NQ" = '#f4a582', "VL_BB" = '#b2162b'))+
  theme_classic() + 
  labs(x = NULL, y = expression(paste(delta^{15}, "N (\u2030)"))) +
  scale_x_date(limits = as.Date(c("2018-07-01", "2019-06-30")))+
  #scale_y_continuous(limits = c(-10, 10))+
  theme(
    axis.title.y = element_text(size=18),
    axis.text.y = element_text(size = 18),
    axis.text.x = element_text(size = 13),
    legend.position = "none")


OffSet_Plot<-((TempGraph_Diff/SalGraph_Diff/pHGraph_Diff)|(POCGraph_Diff/PONGraph_Diff/CIRGraph_Diff/NIRGraph_Diff)) + 
  plot_layout(guides='collect') &
  theme(legend.position = "bottom",
        legend.title = element_text(size=16), 
        legend.text = element_text(size=16))
  
OffSet_Plot

ggsave("OffsetAnalysis.pdf", OffSet_Plot, width = 12, height = 7)


#### 9. Stats: Kruskal & Dunn Test ####
#ST = tests for Normality
#KT = tests for detectable differences in parameters between sites (i.e., POC detectably varies between sites)
#DT = #DT = if there are differences in POC between sites, this tests for detectable differences in POC between every site vs site comparison (i.e., POC_biobay > POC_Enrique, POC_biobay>POC_AB, POC_biobay>POC_veril, and etc.)


shapiro.test(data_mean$POC_m)
shapiro.test(data_mean$PON_m)
shapiro.test(data_mean$d13C_m)
shapiro.test(data_mean$d15N_m)
shapiro.test(data_mean$Temp)
shapiro.test(data_mean$Sal)
shapiro.test(data_mean$pH)

ggdensity(data_mean$POC_m)
ggdensity(data_mean$PON_m)
ggdensity(data_mean$d13C_m)
ggdensity(data_mean$d15N_m)
ggqqplot(data_mean$d13C_m)
ggqqplot(data_mean$Temp)

kruskal.test(POC_m~as.factor(Site),data=data_mean) 
kruskal.test(PON_m~as.factor(Site),data=data_mean)
kruskal.test(d13C_m~as.factor(Site),data=data_mean)
kruskal.test(d15N_m~as.factor(Site),data=data_mean)
kruskal.test(Temp~as.factor(Site),data=data_mean)
kruskal.test(Sal~as.factor(Site),data=data_mean)
kruskal.test(pH~as.factor(Site),data=data_mean)

TempDT<-dunnTest(Temp~as.factor(Site),data=data_mean,method="bonferroni") 
SalDT<-dunnTest(Sal~as.factor(Site),data=data_mean,method="bonferroni") 
pHDT<-dunnTest(pH~as.factor(Site),data=data_mean,method="bonferroni") 
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

