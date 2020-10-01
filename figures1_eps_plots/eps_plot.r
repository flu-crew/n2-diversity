library(reshape2)
library(dplyr)
library(ggplot2)
library(tidyverse)

###########
#LOAD DATA#
###########
n2_1998 <- read.csv("../N2.1998_eps/n2.1998_gmrf_reconstruction1.tsv", sep = "\t", header = TRUE)
n2_1998_2 <- read.csv("../N2.1998_eps/n2.1998_gmrf_reconstruction2.tsv", sep = "\t", header = TRUE)
n2_1998_B <- read.csv("../split_eps_plots/98Bdata.csv", sep = "\t", header = TRUE)
n2_2002 <- read.csv("../N2.2002_eps/n2.2002_gmrf_reconstruction1.tsv", sep = "\t" , header = TRUE)
n2_2002_2 <- read.csv("../N2.2002_eps/n2.2002_gmrf_reconstruction2.tsv", sep = "\t" , header = TRUE)
delta2 <- read.csv("../H1.delta2_eps/h1.delta2_gmrf_reconstruction1.tsv", sep = "\t" , header = TRUE)
delta2_2 <- read.csv("../H1.delta2_eps/h1.delta2_gmrf_reconstruction2.tsv", sep = "\t" , header = TRUE)


###############################
#PRETTY PLOT EPS N2 Figure S1 #
###############################
#using tiff() and dev.off
tiff('test.tiff', units="in", width=13.5, height=9, res=300, compression = 'lzw')

#Data starts in August 2009, or at 2009.66. Scaled at 800x680
(plot1 <- ggplot(n2_2002, aes(x = Time, y = Median))) +
  geom_ribbon(aes(ymin = Lower, ymax = Upper), fill = "red", alpha = 0.5) +
  geom_ribbon(data = n2_1998, aes(ymin = Lower, ymax = Upper), fill = "blue", alpha = 0.5) +
  geom_line(aes(y = Median, color = "N2.2002"), size = 1.5) + 
  geom_line(data = n2_1998, aes(y = Median, color = "N2.1998"), size = 1.5) +
  scale_color_manual(values = c(
    'N2.2002' = 'darkred',
    'N2.1998' = 'darkblue')) + 
  #ggtitle("Effective population size over time") +
  labs(y = "Median EPS", x = "Time", color = "Clade") +
  ylim(0,130) + 
  theme(legend.position = "bottom", 
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(colour = "gray90"),
        panel.grid.minor = element_line(colour = "gray90"),
        title = element_text(size=20,face="bold"),
        axis.title = element_text(size=16,face="bold"),
        axis.text = element_text(size=14,face="bold"),
        legend.title = element_text(size=14,),
        legend.text = element_text(size=14),
        ) + 
  scale_x_continuous(minor_breaks = seq(2008 , 2020, 1), breaks = seq(2008, 2020, 2), limits = c(2009.66,2019))

dev.off()

############################
#PRETTY PLOT TOTAL COMBINED#
############################
n2_total <- n2_2002 + n2_1998  
n2_total$Time = (n2_2002$Time + n2_1998$Time)/2  #Attempt to average the time

plot3 <- ggplot(n2_total, aes(x = Time, y = Median)) +
  geom_ribbon(aes(ymin = Lower, ymax = Upper), fill = "purple", alpha = 0.5) +
  geom_line(aes(y = Median, color = "N2.2002"), size = 1.5) + 
  scale_color_manual(values = c(
    'N2.2002' = 'purple4')) + 
  #ggtitle("Effective population size over time") +
  labs(y = "Median EPS", x = "Time", color = "Clade") +
  ylim(0,130) + 
  theme(legend.position = "bottom", 
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(colour = "gray90"),
        panel.grid.minor = element_line(colour = "gray90"),
        title = element_text(size=20,face="bold"),
        axis.title = element_text(size=16,face="bold"),
        axis.text = element_text(size=14,face="bold"),
        legend.title = element_text(size=14,),
        legend.text = element_text(size=14),
  ) + 
  scale_x_continuous(minor_breaks = seq(2008 , 2020, 1), breaks = seq(2008, 2020, 2), limits = c(2009.66,2019))



####################################
#PRETTY PLOT EPS N2.1998 and Delta2#
####################################
plot2 <- ggplot(delta2, aes(x = Time, y = Median)) +
  geom_ribbon(aes(ymin = Lower, ymax = Upper), fill = "red", alpha = 0.5) +
  geom_ribbon(data = n2_1998_B, aes(ymin = Lower, ymax = Upper), fill = "blue", alpha = 0.5) +
  geom_line(aes(y = Median, color = "H1.Delta2"), size = 1.5) + 
  geom_line(data = n2_1998_B, aes(y = Median, color = "N2.1998B"), size = 1.5) +
  scale_color_manual(values = c(
    'H1.Delta2' = 'darkred',
    'N2.1998B' = 'darkblue')) + 
  #ggtitle("Effective population size over time") +
  labs(y = "Median EPS", x = "Time", color = "Clade") +
  theme(legend.position = "bottom", 
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(colour = "gray90"),
        panel.grid.minor = element_line(colour = "gray90"), 
        title = element_text(size=20,face="bold"),
        axis.title = element_text(size=16,face="bold"),
        axis.text = element_text(size=14,face="bold"),
        legend.title = element_text(size=14,),
        legend.text = element_text(size=14),
        ) + 
  scale_x_continuous(minor_breaks = seq(2008 , 2020, 1), breaks = seq(2008, 2020, 2), limits = c(2009.66,2019))

#Plot first two as subsets
#library(cowplot)
mixplot <- plot_grid(plot1, plot2, labels = "AUTO", label_size = 18)
title <- ggdraw() + draw_label("Effective population size over time", fontface='bold', size = 20)
plot_grid(title, mixplot, ncol=1, rel_heights=c(0.1, 1))

###############################################
#Compare first and 2nd runs 1998, 2002, delta2#
###############################################
#1998
ggplot(n2_1998, aes(x = Time, y = Median)) +
  geom_ribbon(aes(ymin = Lower, ymax = Upper), fill = "red", alpha = 0.5) +
  geom_ribbon(data = n2_1998_2, aes(ymin = Lower, ymax = Upper), fill = "blue", alpha = 0.5) +
  geom_line(aes(y = Median, color = "1998.1"), size = 1.5) + 
  geom_line(data = n2_1998_2, aes(y = Median, color = "1998.2"), size = 1.5) +
  scale_color_manual(values = c(
    '1998.1' = 'darkred',
    '1998.2' = 'darkblue')) + 
  ggtitle("Effective population size over time") +
  labs(y = "Median EPS", x = "Time", color = "N2 Clade") +
  theme(legend.position = "bottom", 
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(colour = "gray90"),
        panel.grid.minor = element_line(colour = "gray90")) + 
  scale_x_continuous(minor_breaks = seq(2007 , 2017, 1), breaks = seq(2007, 2017, 2))

#2002
ggplot(n2_2002, aes(x = Time, y = Median)) +
  geom_ribbon(aes(ymin = Lower, ymax = Upper), fill = "red", alpha = 0.5) +
  geom_ribbon(data = n2_2002_2, aes(ymin = Lower, ymax = Upper), fill = "blue", alpha = 0.5) +
  geom_line(aes(y = Median, color = "2002.1"), size = 1.5) + 
  geom_line(data = n2_2002_2, aes(y = Median, color = "2002.2"), size = 1.5) +
  scale_color_manual(values = c(
    '2002.1' = 'darkred',
    '2002.2' = 'darkblue')) + 
  ggtitle("Effective population size over time") +
  labs(y = "Median EPS", x = "Time", color = "N2 Clade") +
  theme(legend.position = "bottom", 
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(colour = "gray90"),
        panel.grid.minor = element_line(colour = "gray90")) + 
  scale_x_continuous(minor_breaks = seq(2007 , 2017, 1), breaks = seq(2007, 2017, 2))

#delta2
ggplot(delta2, aes(x = Time, y = Median)) +
  geom_ribbon(aes(ymin = Lower, ymax = Upper), fill = "red", alpha = 0.5) +
  geom_ribbon(data = delta2_2, aes(ymin = Lower, ymax = Upper), fill = "blue", alpha = 0.5) +
  geom_line(aes(y = Median, color = "delta2.1"), size = 1.5) + 
  geom_line(data = delta2_2, aes(y = Median, color = "delta2.2"), size = 1.5) +
  scale_color_manual(values = c(
    'delta2.1' = 'darkred',
    'delta2.2' = 'darkblue')) + 
  ggtitle("Effective population size over time") +
  labs(y = "Median EPS", x = "Time", color = "N2 Clade") +
  theme(legend.position = "bottom", 
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(colour = "gray90"),
        panel.grid.minor = element_line(colour = "gray90")) + 
  scale_x_continuous(minor_breaks = seq(2007 , 2017, 1), breaks = seq(2007, 2017, 2))