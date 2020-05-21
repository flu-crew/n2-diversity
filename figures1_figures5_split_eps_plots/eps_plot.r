library(reshape2)
library(dplyr)
library(ggplot2)
library(tidyverse)

###########
#LOAD DATA#
###########
n2_98A <- read.csv("98Adata.csv", sep = "\t", header = TRUE)
n2_98B <- read.csv("98Bdata.csv", sep = "\t", header = TRUE)
n2_02A <- read.csv("02Adata.csv", sep = "\t", header = TRUE)
n2_02B <- read.csv("02Bdata.csv", sep = "\t", header = TRUE)
h1_delta1b <- read.csv("delta1b.csv", sep = "\t", header = TRUE)
h1_delta1b_2 <- read.csv("delta1b_2.csv", sep = "\t", header = TRUE)

####################
#PRETTY PLOT EPS N2#
####################
#n2_2002$Time = as.numeric(n2_2002$Time)
#n2_1998$Time = as.numeric(n2_1998$Time)

#Data starts in August 2009, or at 2009.66. Scaled at 800x680
ggplot(n2_98A, aes(x = Time, y = Median)) +
  geom_ribbon(aes(ymin = Lower, ymax = Upper), fill = "red", alpha = 0.2) +
  geom_ribbon(data = n2_98B, aes(ymin = Lower, ymax = Upper), fill = "blue", alpha = 0.2) +
  geom_ribbon(data = n2_02A, aes(ymin = Lower, ymax = Upper), fill = "gold", alpha = 0.2) +
  geom_ribbon(data = n2_02B, aes(ymin = Lower, ymax = Upper), fill = "magenta", alpha = 0.2) +
  geom_line(aes(y = Median, color = "N2.1998A"), size = 1.5) + 
  geom_line(data = n2_98B, aes(y = Median, color = "N2.1998B"), size = 1.5) +
  geom_line(data = n2_02A, aes(y = Median, color = "N2.2002A"), size = 1.5) +
  geom_line(data = n2_02B, aes(y = Median, color = "N2.2002B"), size = 1.5) +
  scale_color_manual(values = c(
    'N2.1998A' = 'darkred',
    'N2.1998B' = 'darkblue',
    'N2.2002A' = 'darkgoldenrod1',
    'N2.2002B' = 'darkmagenta')) + 
  ggtitle("Effective population size over time") +
  labs(y = "Median EPS", x = "Time", color = "NA Clade") +
  theme(legend.position = "bottom", 
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(colour = "gray90"),
        panel.grid.minor = element_line(colour = "gray90"),
        title = element_text(size=20,face="bold"),
        axis.title = element_text(size=16,face="bold"),
        axis.text = element_text(size=14,face="bold"),
        legend.title = element_text(size=16,),
        legend.text = element_text(size=16),
        ) + 
  scale_x_continuous(minor_breaks = seq(2008 , 2020, 1), breaks = seq(2008, 2020, 2), limits = c(2009.66,2019))

###########################
#TRY TO SPLIT INTO 4 PANEL#
###########################
n2_98A$N2 <- "N2.1998A"
n2_98B$N2 <- "N2.1998B"
n2_02A$N2 <- "N2.2002A"
n2_02B$N2 <- "N2.2002B"
n2_mix <- rbind(n2_98A, n2_98B, n2_02A, n2_02B)

#Data starts in August 2009, or at 2009.66. Scaled at 800x680
ggplot(n2_mix, aes(x = Time, y = Median)) +
  geom_ribbon(aes(ymin = Lower, ymax = Upper), alpha = 0.5) +
  geom_line(aes(y = Median), size = 1.5) + 
  #ggtitle("Effective population size over time") +
  labs(y = "Median EPS", x = "Time") +
  theme(legend.position = "bottom", 
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(colour = "gray90"),
        panel.grid.minor = element_line(colour = "gray90"),
        title = element_text(size=20,face="bold"),
        axis.title = element_text(size=16,face="bold"),
        axis.text = element_text(size=14,face="bold"),
        legend.title = element_text(size=16,),
        legend.text = element_text(size=16),
  ) + 
  scale_x_continuous(minor_breaks = seq(2008 , 2020, 1), breaks = seq(2008, 2020, 2), limits = c(2009.66,2019)) +
  facet_wrap(~N2)

######################
#PLOT WITHOUT 95% HPD#
######################
#n2_2002$Time = as.numeric(n2_2002$Time)
#n2_1998$Time = as.numeric(n2_1998$Time)

#Data starts in August 2009, or at 2009.66. Scaled at 800x680
ggplot(n2_98A, aes(x = Time, y = Median)) +
  geom_line(aes(y = Median, color = "N2.1998A"), size = 2) + 
  geom_line(data = n2_98B, aes(y = Median, color = "N2.1998B"), size = 2) +
  geom_line(data = n2_02A, aes(y = Median, color = "N2.2002A"), size = 2) +
  geom_line(data = n2_02B, aes(y = Median, color = "N2.2002B"), size = 2) +
  scale_color_manual(values = c(
    'N2.1998A' = 'red',
    'N2.1998B' = 'blue',
    'N2.2002A' = 'darkgoldenrod1',
    'N2.2002B' = 'magenta')) + 
  #ggtitle("Effective population size over time") +
  labs(y = "Median EPS", x = "Time", color = "NA Clade") +
  theme(legend.position = "bottom", 
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(colour = "gray90"),
        panel.grid.minor = element_line(colour = "gray90"),
        title = element_text(size=20,face="bold"),
        axis.title = element_text(size=16,face="bold"),
        axis.text = element_text(size=14,face="bold"),
        legend.title = element_text(size=16,),
        legend.text = element_text(size=16),
  ) + 
  scale_x_continuous(minor_breaks = seq(2008 , 2020, 1), breaks = seq(2008, 2020, 2), limits = c(2009.66,2019))

#########
#MONOKAI# Figure S1
#########
#Data starts in August 2009, or at 2009.66. Scaled at 800x680
ggplot(n2_98A, aes(x = Time, y = Median)) +
  geom_ribbon(aes(ymin = Lower, ymax = Upper), fill = "deeppink", alpha = 0.35) +
  geom_ribbon(data = n2_98B, aes(ymin = Lower, ymax = Upper), fill = "olivedrab1", alpha = 0.35) +
  geom_ribbon(data = n2_02A, aes(ymin = Lower, ymax = Upper), fill = "sienna1", alpha = 0.35) +
  geom_ribbon(data = n2_02B, aes(ymin = Lower, ymax = Upper), fill = "turquoise", alpha = 0.35) +
  geom_line(aes(y = Median, color = "N2.1998A"), size = 1.5) + 
  geom_line(data = n2_98B, aes(y = Median, color = "N2.1998B"), size = 1.5) +
  geom_line(data = n2_02A, aes(y = Median, color = "N2.2002A"), size = 1.5) +
  geom_line(data = n2_02B, aes(y = Median, color = "N2.2002B"), size = 1.5) +
  scale_color_manual(values = c(
    'N2.1998A' = 'deeppink',
    'N2.1998B' = 'olivedrab1',
    'N2.2002A' = 'sienna1',
    'N2.2002B' = 'turquoise')) + 
  #ggtitle("Effective population size over time") +
  ylim(0,130) + 
  labs(y = "Median EPS", x = "Time", color = "NA Clade") +
  theme(legend.position = "bottom", 
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(colour = "gray90"),
        panel.grid.minor = element_line(colour = "gray90"),
        title = element_text(size=20,face="bold"),
        axis.title = element_text(size=16,face="bold"),
        axis.text = element_text(size=14,face="bold"),
        legend.title = element_text(size=16,),
        legend.text = element_text(size=16),
  ) + 
  scale_x_continuous(minor_breaks = seq(2008 , 2020, 1), breaks = seq(2008, 2020, 2), limits = c(2009.66,2019))

##########################################
#COMBINE PLOTS AND CHECK AGAINST ORIGINAL#
##########################################
n2_02 <- n2_02A + n2_02B  
n2_02$Time = (n2_02A$Time + n2_02B$Time)/2  #Attempt to average the time
n2_98 <- n2_98A + n2_98B  
n2_98$Time = (n2_98A$Time + n2_98B$Time)/2  #Attempt to average the time

#Load mixed for comparison
n2_1998_orig <- read.csv("../N2.1998_eps/n2.1998_gmrf_reconstruction1.tsv", sep = "\t", header = TRUE)
n2_2002_orig <- read.csv("../N2.2002_eps/n2.2002_gmrf_reconstruction1.tsv", sep = "\t" , header = TRUE)

#Data starts in August 2009, or at 2009.66. Scaled at 800x680
ggplot(n2_98, aes(x = Time, y = Median)) +
  geom_ribbon(aes(ymin = Lower, ymax = Upper), fill = "red", alpha = 0.5) +
  geom_ribbon(data = n2_02, aes(ymin = Lower, ymax = Upper), fill = "blue", alpha = 0.5) +
  geom_ribbon(data = n2_1998_orig, aes(ymin = Lower, ymax = Upper), fill = "gold", alpha = 0.5) +
  geom_ribbon(data = n2_2002_orig, aes(ymin = Lower, ymax = Upper), fill = "magenta", alpha = 0.5) +
  geom_line(aes(y = Median, color = "98"), size = 1.5) + 
  geom_line(data = n2_02, aes(y = Median, color = "02"), size = 1.5) +
  geom_line(data = n2_1998_orig, aes(y = Median, color = "1998_orig"), size = 1.5) +
  geom_line(data = n2_2002_orig, aes(y = Median, color = "2002_orig"), size = 1.5) +
  scale_color_manual(values = c(
    '98' = 'darkred',
    '02' = 'darkblue',
    '1998_orig' = 'darkgoldenrod1',
    '2002_orig' = 'darkmagenta')) + 
  ggtitle("Effective population size over time") +
  labs(y = "Median EPS", x = "Time", color = "N2 Clade") +
  theme(legend.position = "bottom", 
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(colour = "gray90"),
        panel.grid.minor = element_line(colour = "gray90"),
        title = element_text(size=20,face="bold"),
        axis.title = element_text(size=16,face="bold"),
        axis.text = element_text(size=14,face="bold"),
        legend.title = element_text(size=16,),
        legend.text = element_text(size=16),
  ) + 
  scale_x_continuous(minor_breaks = seq(2008 , 2020, 1), breaks = seq(2008, 2020, 2), limits = c(2009.66,2019))

#######################
#CHECK 02A AND DELTA1B#
#######################

#Data starts in August 2009, or at 2009.66. Scaled at 800x680
ggplot(h1_delta1b, aes(x = Time, y = Median)) +
  geom_ribbon(aes(ymin = Lower, ymax = Upper), fill = "red", alpha = 0.5) +
  geom_ribbon(data = n2_02A, aes(ymin = Lower, ymax = Upper), fill = "gold", alpha = 0.5) +
  geom_line(aes(y = Median, color = "H1.Delta1b"), size = 1.5) + 
  geom_line(data = n2_02A, aes(y = Median, color = "N2.2002A"), size = 1.5) +
  scale_color_manual(values = c(
    'H1.Delta1b' = 'darkred',
    'N2.2002A' = 'darkgoldenrod1')) + 
  ggtitle("Effective population size over time") +
  labs(y = "Median EPS", x = "Time", color = "NA Clade") +
  theme(legend.position = "bottom", 
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(colour = "gray90"),
        panel.grid.minor = element_line(colour = "gray90"),
        title = element_text(size=20,face="bold"),
        axis.title = element_text(size=16,face="bold"),
        axis.text = element_text(size=14,face="bold"),
        legend.title = element_text(size=16,),
        legend.text = element_text(size=16),
  ) + 
  scale_x_continuous(minor_breaks = seq(2008 , 2020, 1), breaks = seq(2008, 2020, 2), limits = c(2009.66,2019)) 

#################################
#Split and check 02A and Delta1B#
#################################
h1_delta1b_n202 <- read.csv("delta1b_N202A.csv", sep = "\t", header = TRUE)
n2_02A_delta <- read.csv("N202_delta1b.csv", sep = "\t", header = TRUE)

(plot1 <- ggplot(h1_delta1b_n202, aes(x = Time, y = Median)) +
  geom_ribbon(aes(ymin = Lower, ymax = Upper), fill = "green", alpha = 0.3) +
  geom_ribbon(data = n2_02A_delta, aes(ymin = Lower, ymax = Upper), fill = "purple", alpha = 0.3) +
  geom_line(aes(y = Median, color = "H1.Delta1b"), size = 1.5) + 
  geom_line(data = n2_02A_delta, aes(y = Median, color = "N2.2002A"), size = 1.5) +
  scale_color_manual(values = c(
    'H1.Delta1b' = 'purple3',
    'N2.2002A' = 'green4')) + 
  #ggtitle("Effective population size over time") +
  ylim(0,45) +
  labs(y = "Median EPS", x = "Time", color = "Clade") +
  theme(legend.position = "bottom", 
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(colour = "gray90"),
        panel.grid.minor = element_line(colour = "gray90"),
        title = element_text(size=20,face="bold"),
        axis.title = element_text(size=16,face="bold"),
        axis.text = element_text(size=14,face="bold"),
        legend.title = element_text(size=16,),
        legend.text = element_text(size=16),
  ) + 
  scale_x_continuous(minor_breaks = seq(2008 , 2020, 1), breaks = seq(2008, 2020, 2), limits = c(2009.66,2019)))

####################################
#PRETTY PLOT EPS N2.1998 and Delta2#
####################################
n2_1998_B <- read.csv("../split_eps_plots/98Bdata.csv", sep = "\t", header = TRUE)
delta2 <- read.csv("../H1.delta2_eps/h1.delta2_gmrf_reconstruction1.tsv", sep = "\t" , header = TRUE)
delta2_2 <- read.csv("../H1.delta2_eps/h1.delta2_gmrf_reconstruction2.tsv", sep = "\t" , header = TRUE)

(plot2 <- ggplot(delta2, aes(x = Time, y = Median)) +
  geom_ribbon(aes(ymin = Lower, ymax = Upper), fill = "orange", alpha = 0.3) +
  geom_ribbon(data = n2_1998_B, aes(ymin = Lower, ymax = Upper), fill = "turquoise3", alpha = 0.3) +
  geom_line(aes(y = Median, color = "H1.Delta2"), size = 1.5) + 
  geom_line(data = n2_1998_B, aes(y = Median, color = "N2.1998B"), size = 1.5) +
  scale_color_manual(values = c(
    'H1.Delta2' = 'orange',
    'N2.1998B' = 'turquoise3')) + 
  #ggtitle("Effective population size over time") +
  labs(y = "Median EPS", x = "Time", color = "Clade") +
  ylim(0,45) +
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
  scale_x_continuous(minor_breaks = seq(2008 , 2020, 1), breaks = seq(2008, 2020, 2), limits = c(2009.66,2019)))

#Plot first two as subsets #Figure S5
library(cowplot)
mixplot <- plot_grid(plot2, plot1, labels = "AUTO", label_size = 18)
#title <- ggdraw() + draw_label("Effective population size over time", fontface='bold', size = 20)
plot_grid(mixplot, ncol=1, rel_heights=c(0.1, 1))



###Old
h1_delta1b_n202$corr = h1_delta1b_n202$Median - n2_02A_delta$Median

ggplot(h1_delta1b_n202, aes(x = Time, y = Median)) +
  geom_ribbon(aes(ymin = Lower, ymax = Upper), fill = "red", alpha = 0.3) +
  geom_ribbon(data = n2_02A_delta, aes(ymin = Lower, ymax = Upper), fill = "gold", alpha = 0.3) +
  geom_line(aes(y = Median, color = "H1.Delta1b"), size = 1.5) + 
  geom_line(data = n2_02A_delta, aes(y = Median, color = "N2.2002A"), size = 1.5) +
  geom_line(aes(y = corr, color = "corr"), size = 1.5) +
  scale_color_manual(values = c(
    'H1.Delta1b' = 'darkred',
    'N2.2002A' = 'darkgoldenrod1',
    'corr' = 'blue')) + 
  ggtitle("Effective population size over time") +
  labs(y = "Median EPS", x = "Time", color = "NA Clade") +
  theme(legend.position = "bottom", 
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(colour = "gray90"),
        panel.grid.minor = element_line(colour = "gray90"),
        title = element_text(size=20,face="bold"),
        axis.title = element_text(size=16,face="bold"),
        axis.text = element_text(size=14,face="bold"),
        legend.title = element_text(size=16,),
        legend.text = element_text(size=16),
  ) + 
  scale_x_continuous(minor_breaks = seq(2008 , 2020, 1), breaks = seq(2008, 2020, 2), limits = c(2009.66,2019))


