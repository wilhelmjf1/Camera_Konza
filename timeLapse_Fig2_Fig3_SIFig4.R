#### Data and statistical analysis and visualization for camera data collected at N04D Konza Prairie in 2023 ####

# Associated with the publication: Quantification of Non-Perennial Streamflow using Time-lapse Photography #

# Load in necessary packages
library(tidyverse) 
library(dplyr)
library(lubridate)
library(ggplot2)
library(scales)
library(grid)
library(gridExtra)

#Read in all the data

#setwd("insert your working directory here")

gc_SFM01=read.csv("results_2025_01_13_all.csv", header=T)
#datetime 05/08/2023 11:15
pt_SFM01=read.csv("gpPrelimQ_KMZ_MayJul2023.csv", header=T)

pt_SFM01_Q=read.csv("DISC_GP_KMZ_SFM01_SW_MayJul2023.csv", header=T)
#Q_Ls
pt_SFM01=read.csv("PRES_GP_KMZ_SFM01_SW_MayJul2023.csv", header=T)
#waterLevel_m
#Time 5/8/2023  12:00:00 AM

rain_SFM01=read.csv("lterPPT_MayJul2023.csv", header=T)
#RecDate 5/8/2023  #PPT_mm

gc_SFM01$datetime <- mdy_hm(as.character(gc_SFM01$datetime)) 

pt_SFM01$Time <- mdy_hm(as.character(pt_SFM01$Time))

pt_SFM01_Q$Time <- mdy_hm(as.character(pt_SFM01_Q$Time))

rain_SFM01$RecDate <- mdy(as.character(rain_SFM01$RecDate)) 

# For each row in gc_SFM01, find the closest time in pt_SFM01

gc_SFM01_with_nearest_time <- gc_SFM01 %>%
  rowwise() %>%
  mutate(nearest_time = pt_SFM01$Time[which.min(abs(difftime(gc_SFM01$datetime, pt_SFM01$Time, units = "secs")))])

# Resulting dataframe with closest times

gc_SFM01_with_nearest_time

#closest dataframe with rain

gc_SFM01_with_nearest_time <- gc_SFM01_with_nearest_time %>%
  rowwise() %>%
  mutate(nearest_time = pt_SFM01$Time[which.min(abs(difftime(gc_SFM01$datetime, pt_SFM01$Time, units = "secs")))])

PrecipDailyBar <- ggplot(rain_SFM01, aes(RecDate, PPT_mm)) +
  geom_bar(stat="identity", colour = "darkblue", fill = "darkblue", na.rm = TRUE) +
  xlab("") + ylab("Precip (mm)") +
  theme_bw() +
  theme(axis.title.y = element_text(size = 10), axis.text.y = element_text(size = 8), axis.text.x=element_blank(), axis.title.x=element_blank())

mmed <- function(waterLevel_clean,n=5){runmed(waterLevel_clean,n)}

###

p <- ggplot(gc_SFM01, aes(x=datetime)) +
  geom_line(aes(y = waterLevel_clean_lerp_med9/100+.13), linetype ="solid", color="blue", size=.5) +
  #theme(legend.position="right", axis.text=element_text(size=20), axis.title=element_text(size=20), axis.text.x=element_blank(), axis.title.x=element_blank()) +
  theme_bw() +
  theme(axis.title.y = element_text(size = 10), axis.text.y = element_text(size = 8), axis.text.x=element_blank(), axis.title.x=element_blank())
  #theme(axis.title.y = element_text(size = 10), axis.text.y = element_text(size = 8), axis.text.x=element_blank(), axis.title.x=element_blank())  +
  coord_cartesian(ylim = c(0, .6)) +
  xlab("") +
  scale_y_continuous(
    "Water level (m)")
p

p1 <- p + 
  geom_line(data=pt_SFM01, aes(x=Time, y=waterLevel_m), color = "orange")
p1

#p <- ggplot(gc_SFM01, aes(x=datetime)) +
  geom_line(aes(y = waterLevel), linetype ="solid", color="blue", size=.5) +
  scale_x_datetime(limits = c(as.POSIXct("2023-05-07"), as.POSIXct("2023-07-10"))) +
  theme_bw() +
  theme(axis.title.y = element_text(size = 8), axis.text.y = element_text(size = 8), axis.text.x=element_blank(), axis.title.x=element_blank()) +
  coord_cartesian(ylim = c(0, .8)) +
  xlab("") +
  scale_y_continuous(
    "Water depth (m)") 

p2 <- ggplot(gc_SFM01, aes(x=datetime)) +
  geom_line(aes(y = Q_Ls_no13), linetype ="solid", color="blue", size=.5) +
  theme_bw() +
  theme(axis.title.y = element_text(size = 10), axis.text.y = element_text(size = 8), axis.text.x = element_text(size = 10), axis.title.x=element_blank()) +
  xlab("") +
  scale_y_continuous(
    "Discharge (L/s)")
p2
p3 <- p2 + 
  geom_line(data=pt_SFM01_Q, aes(x=Time, y=Q_Ls), color = "orange")
p3

####pt_SFM01 #Time
####gc_SFM01 #datetime

pt_SFM01$time <- as.POSIXct(pt_SFM01$Time)
gc_SFM01$time <- as.POSIXct(gc_SFM01$datetime)

# Merge datasets using "nearest" time matching
merged_data <- merge(pt_SFM01, gc_SFM01, by = "time", all = TRUE, tolerance = 60)
#date.x time_
  points <- ggplot(data = merged_data, mapping = aes(x = waterDepth_m, y = (waterLevel_clean_lerp_med9/100)+.13)) +
    geom_point() +
    theme_bw() +
    xlab("Pressure Transducer (m)") +
    ylab("GaugeCam (m)") +
    theme(axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14), axis.text.y = element_text(size = 14), axis.text.x = element_text(size = 14)) +
    ylim(c(0, .6)) +
    xlim(c(0, .6)) +
    geom_abline (slope=1, linetype = "dashed", color="Red")
  points
  
  model1 = lm(waterDepth_m_SW_~(waterLevel_clean_lerp_med9), data = merged_data) 
  summary(model1)
  summary(model1)$r.squared 

points

points + scale_x_continuous(trans='log10') +
  scale_y_continuous(trans='log10')

###

install.packages("cowplot")


g1$widths[2:3] <- maxWidth
g2$widths[2:3] <- maxWidth

grid.arrange(
  arrangeGrob(cowplot::plot_grid(PrecipDailyBar, p1, p3, align = "v", ncol=1, rel_heights = c(1/4, 1/2, 1/2))))

#grid.arrange(PrecipDailyBar, p, align = "v", ncol = 1, heights = c(1, 3))

regression <- ggplot(data = gc_SFM01, mapping = aes(x = Q_Ls_PT_ratingCurve, y = Q_Ls_no13)) +
  geom_point(size=1) +
  theme_bw() +
  xlab("Q with GaugeCam rating curve (L/s)") +
  ylab("Q with Pressure Transducer rating curve (L/s)") +
  theme(axis.title.x = element_text(size = 10), axis.title.y = element_text(size = 10), axis.text.y = element_text(size = 10), axis.text.x = element_text(size = 10)) +
  ylim(c(0, 30)) +
  xlim(c(0, 30))
regression

ratingCurve=read.csv("ratingCurveSummary.csv", header=T)
regression <- ggplot(data = ratingCurve, mapping = aes(x = ManualStage_m, y = GaugeCamStage_m_clean)) +
  geom_point(size=3) +
  theme_bw() +
  xlab("Staff gauge water depth (m)") +
  ylab("GaugeCam water depth (m)") +
  theme(axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 12), axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 12)) +
  ylim(c(0.35, .5)) +
  xlim(c(0.54, .57))
regression

regression2 <- ggplot(data = ratingCurve, mapping = aes(x = ManualStage_m, y = StillingWellStage_m)) +
  geom_point(size=3) +
  theme_bw() +
  xlab("Staff gauge water depth (m)") +
  ylab("Pressure transducer water depth (m)") +
  theme(axis.title.x = element_text(size = 12), axis.title.y = element_text(size = 12), axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 12)) +
  ylim(c(0.52, .58)) +
  xlim(c(0.52, .58))
regression2

model1 = lm(ManualStage_m~GaugeCamStage_m_clean, data = ratingCurve) 
summary(model1)
summary(model1)$r.squared 

model2 = lm(ManualStage_m~StillingWellStage_m, data = ratingCurve) 
summary(model2)
summary(model2)$r.squared 