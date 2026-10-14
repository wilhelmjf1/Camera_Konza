#### Data and statistical analysis and visualization for camera data collected at N04D Konza Prairie in 2023 ####

# Associated with the publication: Quantification of Non-Perennial Streamflow using Time-lapse Photography #

# Load in necessary packages
library(dplyr) 
library(cowplot) 
library(tidyverse) 
library(lubridate) 
library(RColorBrewer) 
library(ggthemes) 
library(readxl) 
library(viridis)
library(patchwork)
library(ggplot2)
library(gridExtra)

#Read in all the data

#setwd("insert your working directory here")

#gaugeCam data

N4D01=read.csv("gaugeCam_Konza_2023_N4D01_1.csv", header=T)
N4D03=read.csv("gaugeCam_Konza_2023_N4D03_1.csv", header=T)
N4D05=read.csv("gaugeCam_Konza_2023_N4D05_1.csv", header=T)
N4D09=read.csv("gaugeCam_Konza_2023_N4D09_1.csv", header=T)

#STIC data

N_04M01=read.csv("04M01_2023_1_.csv", header=T)
N_04M03=read.csv("04M03_2023_1.csv", header=T)
N_04M05=read.csv("04M05_2023_1.csv", header=T)
N_04M09=read.csv("04M09_2023_1.csv", header=T)

#stilling well data

SW_04M01=read.csv("04M01_2023_SW_1.csv", header=T)
SW_04M03=read.csv("04M03_2023_SW_1.csv", header=T)
SW_04M05=read.csv("04M05_2023_SW_1.csv", header=T)
SW_04M09=read.csv("04M09_2023_SW_1.csv", header=T)

#agreement

A_04M01=read.csv("04M01_agreement.csv", header=T)
A_04M03=read.csv("04M03_agreement.csv", header=T)
A_04M05=read.csv("04M05_agreement.csv", header=T)
A_04M09=read.csv("04M09_agreement.csv", header=T)

A_04M01$new_date <- mdy(A_04M01$date)
A_04M03$new_date <- mdy(A_04M03$date)
A_04M05$new_date <- mdy(A_04M05$date)
A_04M09$new_date <- mdy(A_04M09$date)

N4D01$new_date <- mdy(N4D01$date)
N4D03$new_date <- mdy(N4D03$date)
N4D05$new_date <- mdy(N4D05$date)
N4D09$new_date <- mdy(N4D09$date)

N_04M01$Time <- mdy_hm(as.character(N_04M01$Time)) 
N_04M03$Time <- mdy_hm(as.character(N_04M03$Time)) 
N_04M05$Time <- mdy_hm(as.character(N_04M05$Time)) 
N_04M09$Time <- mdy_hm(as.character(N_04M09$Time)) 

SW_04M01$Time <- mdy_hm(as.character(SW_04M01$Time)) 
SW_04M03$Time <- mdy_hm(as.character(SW_04M03$Time)) 
SW_04M05$Time <- mdy_hm(as.character(SW_04M05$Time)) 
SW_04M09$Time <- mdy_hm(as.character(SW_04M09$Time)) 

###

pw <- ggplot(N4D_weir, mapping = aes(x = Date, 
                                     y = weir,
                                     fill = ASD022_N04D_wetdry))
pw
pw <- pw +
  geom_tile()
pw <- pw + scale_fill_manual(breaks = c("wet", "dry"), 
                           values=c("darkblue", "yellow"))
pw
pw <- pw + labs(x = NULL) + 
  ylab("Weir") +
  theme_bw() + 
  theme(legend.position = "none")
pw <- pw + theme(axis.text.y = element_text(color="white"), axis.text.x = element_text(color="white"))

connection <- N4D01$connection
Site <- as.factor(N4D01$Site)

p0 <- ggplot(N4D01, mapping = aes(x = new_date, 
                          y = Site,
                          fill = connection))
p0

p0 <- p0 +
  geom_tile()
p0 <- p0 +
  scale_fill_manual(values = c("darkblue", "cyan4", "yellow"))
p0

p0 <- p0 + scale_x_date(date_breaks = "1 month", date_labels = "%B")
p0

p0 <- p0 + 
  ylab("01") +
  theme_bw() + 
  labs(x = NULL) +
  theme(legend.position = "none")
p0 <- p0 + theme(axis.text.y = element_blank(), axis.text.x = element_blank())

###

p1 <- ggplot(N_04M01, mapping = aes(x = Time, 
                                   y = site,
                                   fill = wetdry))
p1

p1 <- p1 +
  geom_tile()
p1 <- p1 +
  scale_fill_manual(values = c("yellow", "darkblue"))

p1
p1 <- p1 + labs(x = NULL) + theme(legend.position = "none")
p1 <- p1 + 
  ylab("STIC") +
  theme_bw() + 
  labs(x = NULL) +
  theme(legend.position = "none")
p1 <- p1 + theme(axis.text.y = element_blank(), axis.text.x = element_blank())

p2 <- ggplot(SW_04M01, mapping = aes(x = Time, 
                                     y = siteId,
                                     fill = wetdry_waterdepth))
p2
p2 <- p2 +
  geom_tile()
p2 <- p2 +
  scale_fill_manual(values = c("yellow", "darkblue"))

p2
p2 <- p2 + labs(x = NULL) + 
  ylab("Stilling Well") +
  theme_bw() + 
  theme(legend.position = "none")
p2 <- p2 + theme(axis.text.y = element_blank(), axis.text.x = element_blank())

###agreement

pa <- ggplot(A_04M01, mapping = aes(x = new_date, 
                                  y = Site,
                                  fill = Agreement))
pa

pa <- pa +
  geom_tile()
pa <- pa +
  scale_fill_manual(values = c("darkorange", "dodgerblue1"))
pa

pa <- pa + scale_x_date(date_breaks = "1 month", date_labels = "%B")
pa

pa <- pa + 
  ylab("AG") +
  theme_bw() + 
  labs(x = NULL) +
  theme(legend.position = "none")
pa <- pa + theme(axis.text.y = element_text(color="white"))

###04M01

grid.arrange(p1, p2, p0, pa, ncol = 1)

combined_plot <- plot_grid(p1, p2, p0, pa, ncol = 1, align = "v")

###04M03

N4D03$new_date <- mdy(N4D03$date)
connection <- N4D03$connection
Site <- as.factor(N4D03$Site)

p3 <- ggplot(N4D03, mapping = aes(x = new_date, 
                                  y = Site,
                                  fill = connection))
p3

p3 <- p3 +
  geom_tile()
p3 <- p3 +
  scale_fill_manual(values = c("darkblue", "cyan4", "yellow"))
p3
p3 <- p3 + scale_x_date(date_breaks = "1 month", date_labels = "%B")
p3
p3 <- p3 + 
  ylab("03") +
  theme_bw() + 
  labs(x = NULL) +
  theme(legend.position = "none")
p3 <- p3 + theme(axis.text.y = element_text(color="white"))

p3 <- p3 + theme(axis.text.y = element_blank(), axis.text.x = element_blank())

###

p4 <- ggplot(N_04M03, mapping = aes(x = Time, 
                                    y = site,
                                    fill = wetdry))
p4

p4 <- p4 +
  geom_tile()
p4 <- p4 +
  scale_fill_manual(values = c("yellow", "darkblue"))
p4
p4 <- p4 + labs(x = NULL) + theme(legend.position = "none")
p4 <- p4 + 
  ylab("STIC") +
  theme_bw() + 
  labs(x = NULL) +
  theme(legend.position = "none")
p4 <- p4 + theme(axis.text.y = element_blank(), axis.text.x = element_blank())

p5 <- ggplot(SW_04M03, mapping = aes(x = Time, 
                                     y = siteId,
                                     fill = wetdry_waterdepth))
p5
p5 <- p5 +
  geom_tile()
p5 <- p5 +
  scale_fill_manual(values = c("yellow", "darkblue"))
p5
p5 <- p5 + labs(x = NULL) + 
  ylab("Stilling Well") +
  theme_bw() + 
  theme(legend.position = "none")
p5 <- p5 + theme(axis.text.y = element_blank(), axis.text.x = element_blank())

###agreement

pa3 <- ggplot(A_04M03, mapping = aes(x = new_date, 
                                    y = Site,
                                    fill = Agreement))
pa3

pa3 <- pa3 +
  geom_tile()
pa3 <- pa3 +
  scale_fill_manual(values = c("darkorange", "dodgerblue1"))
pa3

pa3 <- pa3 + scale_x_date(date_breaks = "1 month", date_labels = "%B")
pa3

pa3 <- pa3 + 
  ylab("AG") +
  theme_bw() + 
  labs(x = NULL) +
  theme(legend.position = "none")
pa3 <- pa3 + theme(axis.text.y = element_text(color="white"))

combined_plot <- plot_grid(p4, p5, p3, pa3, ncol = 1, align = "v")
combined_plot

###04M05

N4D05$new_date <- mdy(N4D05$date)
connection <- N4D05$connection
Site <- as.factor(N4D05$Site)

p6 <- ggplot(N4D05, mapping = aes(x = new_date, 
                                  y = Site,
                                  fill = connection))
p6

p6 <- p6 +
  geom_tile()
p6 <- p6 +
  scale_fill_manual(values = c("darkblue", "cyan4", "yellow"))
p6
p6 <- p6 + scale_x_date(date_breaks = "1 month", date_labels = "%B")
p6
p6 <- p6 + 
  ylab("05") +
  theme_bw() + 
  labs(x = NULL) +
  theme(legend.position = "none")
p6 <- p6 + theme(axis.text.y = element_text(color="white"))

p6 <- p6 + theme(axis.text.y = element_blank(), axis.text.x = element_blank())

###

p7 <- ggplot(N_04M05, mapping = aes(x = Time, 
                                    y = site,
                                    fill = wetdry))
p7

p7 <- p7 +
  geom_tile()
p7 <- p7 +
  scale_fill_manual(values = c("yellow", "darkblue"))
p7
p7 <- p7 + labs(x = NULL) + theme(legend.position = "none")
p7 <- p7 + 
  ylab("STIC") +
  theme_bw() + 
  labs(x = NULL) +
  theme(legend.position = "none")
p7 <- p7 + theme(axis.text.y = element_blank(), axis.text.x = element_blank())

p8 <- ggplot(SW_04M05, mapping = aes(x = Time, 
                                     y = siteId,
                                     fill = wetdry_waterdepth))
p8
p8 <- p8 +
  geom_tile()
p8 <- p8 +
  scale_fill_manual(values = c("yellow", "darkblue"))
p8
p8 <- p8 + labs(x = NULL) + 
  ylab("Stilling Well") +
  theme_bw() + 
  theme(legend.position = "none")
p8 <- p8 + theme(axis.text.y = element_blank(), axis.text.x = element_blank())

###agreement

pa5 <- ggplot(A_04M05, mapping = aes(x = new_date, 
                                     y = Site,
                                     fill = Agreement))
pa5

pa5 <- pa5 +
  geom_tile()
pa5 <- pa5 +
  scale_fill_manual(values = c("darkorange", "dodgerblue1"))
pa5

pa5 <- pa5 + scale_x_date(date_breaks = "1 month", date_labels = "%B")
pa5

pa5 <- pa5 + 
  ylab("AG") +
  theme_bw() + 
  labs(x = NULL) +
  theme(legend.position = "none")
pa5 <- pa5 + theme(axis.text.y = element_text(color="white"))

combined_plot <- plot_grid(p7, p8, p6, pa5, ncol = 1, align = "v")

grid.arrange(p7, p8, p6, pa5, ncol = 1)

###04M09

N4D09$new_date <- mdy(N4D09$date)
connection <- N4D09$connection
Site <- as.factor(N4D09$Site)

p9 <- ggplot(N4D09, mapping = aes(x = new_date, 
                                  y = Site,
                                  fill = connection))
p9

p9 <- p9 +
  geom_tile()
p9 <- p9 +
  scale_fill_manual(values = c("darkblue", "cyan4", "yellow"))
p9
p9 <- p9 + scale_x_date(date_breaks = "1 month", date_labels = "%B")
p9
p9 <- p9 + 
  ylab("09") +
  theme_bw() + 
  labs(x = NULL) +
  theme(legend.position = "none")
p9 <- p9 + theme(axis.text.y = element_text(color="white"))

p9 <- p9 + theme(axis.text.y = element_blank(), axis.text.x = element_blank())

p10 <- ggplot(N_04M09, mapping = aes(x = Time, 
                                    y = site,
                                    fill = wetdry))
p10

p10 <- p10 +
  geom_tile()
p10 <- p10 +
  scale_fill_manual(values = c("yellow", "darkblue"))
p10
p10 <- p10 + labs(x = NULL) + theme(legend.position = "none")
p10 <- p10 + 
  ylab("STIC") +
  theme_bw() + 
  labs(x = NULL) +
  theme(legend.position = "none")
p10 <- p10 + theme(axis.text.y = element_blank(), axis.text.x = element_blank())

p11 <- ggplot(SW_04M09, mapping = aes(x = Time, 
                                     y = siteId,
                                     fill = wetdry_waterdepth))
p11
p11 <- p11 +
  geom_tile()
p11 <- p11 +
  scale_fill_manual(values = c("yellow", "darkblue"))
p11
p11 <- p11 + labs(x = NULL) + 
  ylab("Stilling Well") +
  theme_bw() + 
  theme(legend.position = "none")
p11 <- p11 + theme(axis.text.y = element_blank(), axis.text.x = element_blank())

###agreement

pa9 <- ggplot(A_04M09, mapping = aes(x = new_date, 
                                     y = Site,
                                     fill = Agreement))
pa9

pa9 <- pa9 +
  geom_tile()
pa9 <- pa9 +
  scale_fill_manual(values = c("darkorange", "dodgerblue1"))
pa9

pa9 <- pa9 + scale_x_date(date_breaks = "1 month", date_labels = "%B")
pa9

pa9 <- pa9 + 
  ylab("AG") +
  theme_bw() + 
  labs(x = NULL) +
  theme(legend.position = "none")
pa9 <- pa9 + theme(axis.text.y = element_text(color="white"))

###

combined_plot <- plot_grid(p11, p9, pa9, ncol = 1, align = "v")

grid.arrange(p10, p11, p9, pa9, ncol = 1)

combined_plot <- plot_grid(pw, p0, p3, p6, p9, ncol = 1, align = "v")

grid.arrange(p9, p6, p3, pw, p0, ncol = 1)

combined_plot <- plot_grid(pw, p9, p6, p3, p0, ncol = 1, align = "v")
