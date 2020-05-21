#http://seananderson.ca/2013/10/19/reshape/
#http://www.sthda.com/english/wiki/ggplot2-violin-plot-quick-start-guide-r-software-and-data-visualization
#https://groups.google.com/forum/#!topic/beast-users/YiGBZCFNWIc
library(reshape2)
library(ggplot2)

# Load data
substitutionRates <- read.csv("violin_plot.csv")

# Convert to long format
allRates <- melt(substitutionRates)

#######################################
# Plot HA and NAs with Discrete colors#
#######################################
ggplot(data = allRates, aes(x=variable, y=value, fill=variable)) +
  geom_violin() + 
  scale_y_continuous(limits = c(0.0,0.015))  +
  theme(legend.position="bottom") +
  stat_summary(fun.y=median, geom="point", size=2, color="red") +
  labs(title="Nucleotide substitution rate per codon", y = "nucleotide mutations / site / year", x = "Phylogenetic Clade", fill="Clade") +
  guides(fill=FALSE) + 
  theme(legend.position = "bottom", 
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(colour = "gray90"),
        panel.grid.minor = element_line(colour = "gray90"),
        axis.title = element_text(size=12,face="bold"),
        axis.text = element_text(size=11,face="bold"),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5),
        title = element_text(size=16,face="bold"),
        legend.title = element_text(size=12,),
        legend.text = element_text(size=12),
  )

####################
#Summarize averages#
####################
meanRates <- data.frame(
  N2.1998A =  rowMeans(substitutionRates[1:3]),
  N2.1998B =  rowMeans(substitutionRates[4:6]),
  N2.2002A =  rowMeans(substitutionRates[7:9]),
  N2.2002B =  rowMeans(substitutionRates[10:12]),
  H1.gamma =  rowMeans(substitutionRates[13:15]),
  H1.delta2 =  rowMeans(substitutionRates[16:18]),
  H3.2010 =  rowMeans(substitutionRates[19:21]),
  H3.clusteriv =  rowMeans(substitutionRates[22:24])
)

# Convert to long format
rates <- melt(meanRates)

#Figure S6
ggplot(data = rates, aes(x=variable, y=value, fill=variable)) +
  geom_violin() + 
  scale_y_continuous(limits = c(0.0,0.01))  +
  theme(legend.position="bottom") +
  stat_summary(fun.y=median, geom="point", size=2, color="red") +
  labs(y = "nucleotide mutations / site / year", x = "Phylogenetic Clade", fill="Clade") + #title="Mean nucleotide substitution rate",
  guides(fill=FALSE) +
  theme(legend.position = "bottom", 
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(colour = "gray90"),
        panel.grid.minor = element_line(colour = "gray90"),
        axis.title = element_text(size=16,face="bold"),
        axis.text = element_text(size=14,face="bold"),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5),
        title = element_text(size=20,face="bold"),
        legend.title = element_text(size=16,),
        legend.text = element_text(size=16),
  )

###########################
#Focus on codon position 3#
###########################
cp3Rates <- data.frame(
  N2.1998A =  substitutionRates[3],
  N2.1998B =  substitutionRates[6],
  N2.2002A =  substitutionRates[9],
  N2.2002B =  substitutionRates[12],
  H1.gamma =  substitutionRates[15],
  H1.delta2 =  substitutionRates[24],
  H3.clusteriv =  substitutionRates[21],
  H3.2010 =  substitutionRates[18]
)

# Convert to long format
rates <- melt(cp3Rates)

ggplot(data = rates, aes(x=variable, y=value, fill=variable)) +
  geom_violin() + 
  scale_y_continuous(limits = c(0.0,0.02))  +
  theme(legend.position="bottom") +
  stat_summary(fun.y=median, geom="point", size=2, color="red") +
  labs(title="Mean substituion rate for each codon position for assorted N2, H1, and H3") +
  theme(legend.position = "bottom", 
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(colour = "gray90"),
        panel.grid.minor = element_line(colour = "gray90"),
        axis.text.x = element_text(angle = 90, hjust = 1))

