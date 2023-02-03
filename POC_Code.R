
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
library(factoextra)
library(performance)
library(see)

#Pending to eliminate Libraries
library(rgdal)
library(raster)
library(sf)
library(ggrepel)
library(FSA)
library(plotly)
library(rstatix)
#library(ggstatplot)



#### 2. Insert Dataset (.csv), Convert Sampling Date column from character to Date format, Rename Columns, Filter sites of interest, Eliminate parameters that will not be used in the analysis #############


data <- read_csv("DB_AnalysisR_OutlierRemoved.csv", col_types = cols(`Sampling Date` = col_date(format = "%m/%d/%Y")))%>%   
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
    TA = "TA (UMOL/KG)") %>%
 mutate(
    CN = POC/PON) %>% 
  dplyr::filter(Site =="AB"|
           Site == "BB"|
           Site == "NQ"|
           Site == "VL") %>% 
  dplyr::select(-c(TA, DIC)) %>%
  subset(Date > "2018-01-01" & Date < "2019-12-31") %>% 
  replace_with_na_all(condition=~.x==-999)



#### 3. Figure 1: Map & Stations ########


register_google(key = '...')


cols5 <- c("BB" = '#b2162b', "NQ" = '#f4a582', "AB" = '#4393c3', "VL" = '#053061') #diverging


####Zoom Out Map####

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
  geom_rect(aes(xmin = -67.056, xmax = -67.041, ymin = 17.9445, ymax = 17.9645), colour = "white", alpha = 0, size = 1)+ 
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
  annotate("text", x = -67.0213, y = 17.8635, label= "VL", colour="white", fontface="bold", size=9)
  
F1

#Zoom In Map


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
  annotate("text", x = -67.0504, y = 17.9576, label= "NQ", colour="white", fontface="bold", size=10)

F1+F2


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
            CN_m = mean(CN),
            CN_std=sd(CN),
            Lat = mean(Lat), 
            Lon = mean(Lon))
data_mean$Site <- factor(data_mean$Site, level = c("BB", "NQ", "AB", "VL"))
  
            

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
    #axis.text.x = element_text(size=16),
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size=18),
    axis.text.y = element_text(size =18))


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
    axis.ticks.x = element_blank())


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
    axis.ticks.x = element_blank())


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
    axis.ticks.x = element_blank())
 

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


TimeseriesPlot<-((TempGraph2/SalGraph2/pHGraph2)|(POCGraph2/PONGraph2/CIRGraph2/NIRGraph2)) + 
  plot_layout(guides='collect') &
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
  
ggsave("ParameterTimeSeries_Final.pdf", TimeseriesPlot, width = 12, height = 7)


###6. Figure 3: Parameter Gradient (BoxPlot) Box Plot### 


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
    axis.text.x = element_blank(),
    #axis.text.x = element_text(size = 16), 
    legend.position = "none")


POCBox<-ggplot(data_mean, aes(Site, POC_m)) +  
  ggtitle("D")+
  geom_boxplot(aes(fill = Site))+
  #geom_errorbar(aes(ymin = POC_m - POC_std, ymax = POC_m + POC_std))+
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

BoxPlot2 <- ((TempBox/SalBox/pHBox/CNBox)|(POCBox/PONBox/CIRBox/NIRBox)) +
  plot_layout(guides = "collect") 

BoxPlot
BoxPlot2

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
    pH = mean(pH), 
    CN = mean(CN))
data_pca$Site<-factor(data_pca$Site, c("BB", "NQ", "AB", "VL"))

data_pca = na.omit(data_pca)
pca <- prcomp(data_pca[c(3:9)], center = TRUE, scale. = TRUE)
pca <- prcomp(data_pca[c(3:10)], center = TRUE, scale. = TRUE) #para incluir C:N

pca
summary(pca)

PCAPlot<-ggbiplot(pca, obs.scale = 1, var.scale = 1, size=10, 
                  group = data_pca$Site,
                  varname.size=5,
                  labels.size=5, 
                  ellipse = TRUE, 
                  circle = FALSE,  
                  label.repel = TRUE)+ 
  geom_point(aes(shape=data_pca$Site,fill=data_pca$Site, color = 'black'), size = 8)+
  scale_shape_manual(name="Site", values=c(21, 24, 23, 22))+
  scale_color_manual(name="Site", values=cols5) +
  scale_fill_manual(name="Site", values=cols5) + 
  labs(
    x = "PC1 (45.5%)", 
    y = "PC2 (18.1%)")+
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


PCAPlot

ggsave("PrincipalComponentAnalysis.pdf", PCAPlot, width = 12, height = 7)

 
#Scree Plot
ggscreeplot(pca)
fviz_eig(pca)


#### 8. Stats: ANOVA ####

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
AIC(pH0,pH1,pH2,pH3,pH4) #AIC of all models included, t3 has the lowest AIC and is the best model to fit the data
summary(pH4)#Site and Month are a significant predictor of pH
check_model(pH4) #model assumptions appear to be mostly okay here
TukeyHSD(pH4, conf.level=.95) #BB pH > AB pH, NQ pH > AB pH, VL pH < AB pH, VL pH < BB pH, VL pH < NQ pH

poc0=aov(POC_m~1,data=data_mean) #null model
poc1=aov(POC_m~Site,data=data_mean) #Site as a predictor of POC
poc2=aov(POC_m~Month,data=data_mean) #Month as a predictor of POC
poc3=aov(POC_m~Site+Month,data=data_mean) #Site + Month as predictors of POC
poc4=aov(POC_m~Site*Month,data=data_mean) #Site + Month + Interaction as predictors of POC
AIC(poc0,poc1,poc2,poc3,poc4) #AIC of all models included, t1 has the lowest AIC and is the best model to fit the data
summary(poc1) #Site is a significant predictor of POC
check_model(poc1) #model assumptions appear to be mostly okay here
TukeyHSD(poc1, conf.level=.95) #BB POC > AB POC, NQ POC < BB POC, VL POC < BB POC

PON0=aov(PON_m~1,data=data_mean) #null model
PON1=aov(PON_m~Site,data=data_mean) #Site as a predictor of PON
PON2=aov(PON_m~Month,data=data_mean) #Month as a predictor of PON
PON3=aov(PON_m~Site+Month,data=data_mean) #Site + Month as predictors of PON
PON4=aov(PON_m~Site*Month,data=data_mean) #Site + Month + Interaction as predictors of PON
AIC(PON0,PON1,PON2,PON3,PON4) #AIC of all models included, t3 has the lowest AIC and is the best model to fit the data
summary(PON1) #Site and Month are a significant predictor of PON
check_model(PON1) #model assumptions appear to be mostly okay here
TukeyHSD(PON1, conf.level=.95) #BB PON > AB PON, NQ PON > AB PON, VL PON < AB PON, VL PON < BB PON, VL PON < NQ PON

d13C0=aov(d13C_m~1,data=data_mean) #null model
d13C1=aov(d13C_m~Site,data=data_mean) #Site as a predictor of d13C
d13C2=aov(d13C_m~Month,data=data_mean) #Month as a predictor of d13C
d13C3=aov(d13C_m~Site+Month,data=data_mean) #Site + Month as predictors of d13C
d13C4=aov(d13C_m~Site*Month,data=data_mean) #Site + Month + Interaction as predictors of d13C
AIC(d13C0,d13C1,d13C2,d13C3,d13C4) #AIC of all models included, t3 has the lowest AIC and is the best model to fit the data
summary(d13C1) #Site and Month are a significant predictor of d13C
check_model(d13C1) #model assumptions appear to be mostly okay here
TukeyHSD(d13C1, conf.level=.95) #BB d13C > AB d13C, NQ d13C > AB d13C, VL d13C < AB d13C, VL d13C < BB d13C, VL d13C < NQ d13C

d15N0=aov(d15N_m~1,data=data_mean) #null model
d15N1=aov(d15N_m~Site,data=data_mean) #Site as a predictor of d15N
d15N2=aov(d15N_m~Month,data=data_mean) #Month as a predictor of d15N
d15N3=aov(d15N_m~Site+Month,data=data_mean) #Site + Month as predictors of d15N
d15N4=aov(d15N_m~Site*Month,data=data_mean) #Site + Month + Interaction as predictors of d15N
AIC(d15N0,d15N1,d15N2,d15N3,d15N4) #AIC of all models included, t3 has the lowest AIC and is the best model to fit the data
summary(d15N4) #Site and Month are a significant predictor of d15N
check_model(d15N4) #model assumptions appear to be mostly okay here
TukeyHSD(d15N4, conf.level=.95) #BB d15N > AB d15N, NQ d15N > AB d15N, VL d15N < AB d15N, VL d15N < BB d15N, VL d15N < NQ d15N


library(sjPlot)
tab_model(temp3,sal3,
          string.pred = "Coeffcient",
          
          string.ci = "Conf. Int (95%)",
          
          string.p = "p-Value",
          
          file="Table_1-Model_Summaries.html")




# Computing correlation matrix
correlation_matrix <- round(cor(as.data.frame(as.numeric(data_pca[complete.cases(data_pca[,2:8]),]))),1)

# Computing correlation matrix with p-values
corrp.mat <- cor_pmat(data_pca)

# Visualizing the correlation matrix using
# square and circle methods
ggcorrplot(correlation_matrix, method ="square")
ggcorrplot(correlation_matrix, method ="circle”))

