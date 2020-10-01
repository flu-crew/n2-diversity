#Section for instlaling rerq packages. Uncomment as needed.
#install.packages("dendextend")
#install.packages("ape")
source("https://bioconductor.org/biocLite.R")
biocLite("DECIPHER")

#Load libraries
library(dendextend) # for comparing two dendrograms
library(DECIPHER)   # used for loading in newick format trees
library(ape)        #general purpose
library(reshape2)
library(ggplot2)

#load source data, our HA and N2 trees
dendHA <- ReadDendrogram("ha_prepped2.tre", internalLabels = FALSE)
dendNA <- ReadDendrogram("na_prepped2.tre", internalLabels = FALSE)

#set label colors trees
ha_lab <- labels(dendHA)
ha_lab <- unlist(lapply(ha_lab, function(x) strsplit(x,"\\|")[[1]][4])) #index 4 for HA clade
ha_lab <- as.factor(ha_lab)
levels(ha_lab) <- 1:length(levels(ha_lab))
ha_lab <- as.numeric(ha_lab)
labels_colors(dendHA) <- ha_lab

na_lab <- labels(dendNA)
na_lab <- unlist(lapply(na_lab, function(x) strsplit(x,"\\|")[[1]][3])) #index 6 for HA clade
na_lab <- as.factor(na_lab)
levels(na_lab) <- 1:length(levels(na_lab))
na_lab <- as.numeric(na_lab)
labels_colors(dendNA) <- na_lab

#Create a dendrogram list with colorized trees
#dl <- dendlist(highlight_branches_col(dendHA, viridis(100)), highlight_branches_col(dendNA, rev(magma(100))))
dl <- dendlist(color_branches(dendHA, clusters = ha_lab), color_branches(dendNA, clusters = na_lab))
#tanglegram(dendHA, dendNA, main_left = "HA", main_right = "NA", lwd = 1, type = "r", remove_nodePar = TRUE,
#           highlight_distinct_edges = FALSE, highlight_branches_lwd=FALSE, common_subtrees_color_lines = TRUE, faster = FALSE)
tanglegram(dl, main_left = "HA", main_right = "N2", lwd = 1, type = "r", remove_nodePar = TRUE, 
           highlight_distinct_edges = FALSE, highlight_branches_lwd=FALSE, common_subtrees_color_lines = TRUE, 
           edge.lwd = 2,lab.cex = 1, faster = TRUE)

#Reset colors
test = ha_lab
test = replace(test, test == 1, rgb(206,57,154, max = 255))
test = replace(test, test == 2, rgb(93,132,0, max = 255)) 
test = replace(test, test == 3, rgb(0,148,79, max = 255))
test = replace(test, test == 4, rgb(0,150,129, max = 255))
test = replace(test, test == 5, rgb(0,147,169, max = 255))
test = replace(test, test == 6, rgb(153,88,212, max = 255))
test = replace(test, test == 7, rgb(68,115,215, max = 255))
test = replace(test, test == 8, rgb(0,136,199, max = 255))
test = replace(test, test == 9, rgb(192,63,190, max = 255))
test = replace(test, test == 10, rgb(168,107,0, max = 255))
test = replace(test, test == 11, rgb(190,90,38, max = 255))
test = replace(test, test == 12, rgb(204,71,107, max = 255))
test = replace(test, test == 13, rgb(138,121,0, max = 255)) 
test = replace(test, test == 14, rgb(0,141,0, max = 255)) 

#FIGURE3
tanglegram(dl, 
           main_left = "HA", 
           main_right = "N2",
           color_lines = test,
           lwd = 2, 
           axes = TRUE,
           type = "r", 
           remove_nodePar = TRUE, 
           highlight_distinct_edges = FALSE, 
           highlight_branches_lwd=FALSE, 
           common_subtrees_color_lines = FALSE, 
           edge.lwd = 2,
           lab.cex = 1, 
           faster = FALSE)

#Print entanglement
dl %>% entanglement # lower is better


####################################################
#Do a year plot based on HA clade for a few subsets#
####################################################
pairData <- read.csv("master.csv", header = TRUE)

#Select only Delta1a
delta1a <- subset(pairData, ha_clade == "delta1a")
delta1a_time <- dcast(delta1a, year~na_clade, value.var = "ha_clade")
(p1 <- ggplot(delta1a_time) +
  geom_line(aes(x = year, y = N2.2002A, color = "N2.2002A"), size = 1.5) + 
  geom_line(aes(x = year, y = N2.2002B, color = "N2.2002B"), size = 1.5) +
  scale_color_manual(values = c(
    'N2.2002A' = 'red',
    'N2.2002B' = 'blue')) + 
  ggtitle("Paired Delta1a detections over time") +
  labs(y = "Detections", x = "Time", color = "N2 Clade") +
  theme(legend.position = "bottom", 
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(colour = "gray90"),
        panel.grid.minor = element_line(colour = "gray90"),
        title = element_text(size=20,face="bold"),
        axis.title = element_text(size=16,face="bold"),
        axis.text = element_text(size=14,face="bold"),
        legend.title = element_text(size=14,),
        legend.text = element_text(size=14)
      ) + 
    scale_x_continuous(minor_breaks = seq(2009 , 2017, 1), breaks = seq(2009, 2017, 2), limits = c(2009,2017)))

#Select only Cluster_iva
clusteriva <- subset(pairData, ha_clade == "cluster_iva")
clusteriva_time <- dcast(clusteriva, year~na_clade, value.var = "ha_clade")
(p2 <- ggplot(clusteriva_time) +
  geom_line(aes(x = year, y = N2.2002A, color = "N2.2002A"), size = 1.5) + 
  geom_line(aes(x = year, y = N2.2002B, color = "N2.2002B"), size = 1.5) +
  scale_color_manual(values = c(
    'N2.2002A' = 'red',
    'N2.2002B' = 'blue')) + 
  ggtitle("Paired Cluster IVA detections over time") +
  labs(y = "Detections", x = "Time", color = "N2 Clade") +
  theme(legend.position = "bottom", 
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(colour = "gray90"),
        panel.grid.minor = element_line(colour = "gray90"),
        title = element_text(size=20,face="bold"),
        axis.title = element_text(size=16,face="bold"),
        axis.text = element_text(size=14,face="bold"),
        legend.title = element_text(size=14,),
        legend.text = element_text(size=14)
        ) + 
    xlim(c(2010,2017)))

#Select only 2010.1
h2010 <- subset(pairData, ha_clade == "2010.1")
h2010_time <- dcast(h2010, year~na_clade, value.var = "ha_clade")
(p3 <- ggplot(h2010_time) +
  geom_line(aes(x = year, y = N2.2002A, color = "N2.2002A"), size = 1.5) + 
  geom_line(aes(x = year, y = N2.2002B, color = "N2.2002B"), size = 1.5) + 
  scale_color_manual(values = c(
    'N2.2002A' = 'red',
    'N2.2002B' = 'blue')) + 
  ggtitle("Paired 2010.1 detections over time") +
  labs(y = "Detections", x = "Time", color = "N2 Clade") +
  theme(legend.position = "bottom", 
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(colour = "gray90"),
        panel.grid.minor = element_line(colour = "gray90"),
        title = element_text(size=20,face="bold"),
        axis.title = element_text(size=16,face="bold"),
        axis.text = element_text(size=14,face="bold"),
        legend.title = element_text(size=14,),
        legend.text = element_text(size=14)
  ) +
  xlim(c(2012,2017)))

#Plot together as A, B
#FIGURE3A
library(cowplot)
plot_grid(p1, p2, p3, labels = "AUTO", label_size = 18, nrow = 1, hjust = 0, vjust = 1)

#############
#FOCUS ON NA#
#############

#Select only N2.2002A
NA2002A <- subset(pairData, na_clade == "N2.2002A")
NA2002A_time <- dcast(NA2002A, year~ha_clade, value.var = "na_clade")
NA_plot <- melt(NA2002A_time, id.vars = "year")
ggplot(NA_plot,aes(x = year, y=value, col = variable)) +
  geom_line(size = 1.5) + 
  ggtitle("Paired N2.2002A detections over time") +
  labs(y = "Detections", x = "Time", color = "N2 Clade") +
  theme(legend.position = "bottom", 
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(colour = "gray90"),
        panel.grid.minor = element_line(colour = "gray90"),
        title = element_text(size=20,face="bold"),
        axis.title = element_text(size=16,face="bold"),
        axis.text = element_text(size=14,face="bold"),
        legend.title = element_text(size=14,),
        legend.text = element_text(size=14)
  )


#Select only N2.2002B
NA2002B <- subset(pairData, na_clade == "N2.2002B")
NA2002B_time <- dcast(NA2002B, year~ha_clade, value.var = "na_clade")
NA_plot <- melt(NA2002B_time, id.vars = "year")
ggplot(NA_plot,aes(x = year, y=value, col = variable)) +
  geom_line(size = 1.5) + 
  ggtitle("Paired N2.2002B detections over time") +
  labs(y = "Detections", x = "Time", color = "N2 Clade") +
  theme(legend.position = "bottom", 
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(colour = "gray90"),
        panel.grid.minor = element_line(colour = "gray90"),
        title = element_text(size=20,face="bold"),
        axis.title = element_text(size=16,face="bold"),
        axis.text = element_text(size=14,face="bold"),
        legend.title = element_text(size=14,),
        legend.text = element_text(size=14)
  )


#Select only N2.1998B
NA1998A <- subset(pairData, na_clade == "N2.1998A")
NA1998A_time <- dcast(NA1998A, year~ha_clade, value.var = "na_clade")
NA_plot <- melt(NA1998A_time, id.vars = "year")
ggplot(NA_plot,aes(x = year, y=value, col = variable)) +
  geom_line(size = 1.5) + 
  ggtitle("Paired N2.1998A detections over time") +
  labs(y = "Detections", x = "Time", color = "N2 Clade") +
  theme(legend.position = "bottom", 
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(colour = "gray90"),
        panel.grid.minor = element_line(colour = "gray90"),
        title = element_text(size=20,face="bold"),
        axis.title = element_text(size=16,face="bold"),
        axis.text = element_text(size=14,face="bold"),
        legend.title = element_text(size=14,),
        legend.text = element_text(size=14)
  )

#Select only N2.1998B
NA1998B <- subset(pairData, na_clade == "N2.1998B")
NA1998B_time <- dcast(NA1998B, year~ha_clade, value.var = "na_clade")
NA_plot <- melt(NA1998B_time, id.vars = "year")
ggplot(NA_plot,aes(x = year, y=value, col = variable)) +
  geom_line(size = 1.5) + 
  ggtitle("Paired N2.1998B detections over time") +
  labs(y = "Detections", x = "Time", color = "N2 Clade") +
  theme(legend.position = "bottom", 
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(colour = "gray90"),
        panel.grid.minor = element_line(colour = "gray90"),
        title = element_text(size=20,face="bold"),
        axis.title = element_text(size=16,face="bold"),
        axis.text = element_text(size=14,face="bold"),
        legend.title = element_text(size=14,),
        legend.text = element_text(size=14)
  )

