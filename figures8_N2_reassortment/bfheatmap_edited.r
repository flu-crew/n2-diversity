library(ggplot2)
library(reshape2)
library(viridis)

#Load data
d98A <- read.csv("98A.txt", sep="\t")
d98B <- read.csv("98B.txt", sep="\t")
d02A <- read.csv("02A.txt", sep="\t")
d02B <- read.csv("02B.txt", sep="\t")

#Add n2 variable
d98A$N2 <- "N2.1998A"
d98B$N2 <- "N2.1998B"
d02A$N2 <- "N2.2002A"
d02B$N2 <- "N2.2002B"

#Merge the 4 DFs
merged_N2 <- rbind(d98A, d98B, d02A, d02B)
merged_N2$logBF = log(merged_N2$BAYES_FACTOR)

#Remove less significant reassortments from the image
merged_N2 <- merged_N2[!(merged_N2$FROM == "pdmh1"),]
merged_N2 <- merged_N2[!(merged_N2$FROM == "unknown"),]
merged_N2 <- merged_N2[!(merged_N2$FROM == "cluster_ivd"),]
merged_N2 <- merged_N2[!(merged_N2$FROM == "delta-like"),]
merged_N2 <- merged_N2[!(merged_N2$FROM == "cluster_ive"),]
merged_N2 <- merged_N2[!(merged_N2$FROM == "cluster_ivc"),]
merged_N2 <- merged_N2[!(merged_N2$FROM == "location13"),]


merged_N2 <- merged_N2[!(merged_N2$TO == "pdmh1"),]
merged_N2 <- merged_N2[!(merged_N2$TO == "unknown"),]
merged_N2 <- merged_N2[!(merged_N2$TO == "cluster_ivd"),]
merged_N2 <- merged_N2[!(merged_N2$TO == "delta-like"),]
merged_N2 <- merged_N2[!(merged_N2$TO == "cluster_ive"),]
merged_N2 <- merged_N2[!(merged_N2$TO == "cluster_ivc"),]
merged_N2 <- merged_N2[!(merged_N2$TO == "location13"),]

#Replace -Inf with zero for time being
merged_N2$logBF[!is.finite(merged_N2$logBF)] = 0

#############
# FIGURE S8 #
#############
#Plot ln(BF)

tiff('test.tiff', units="in", width=13.5, height=13.5, res=300, compression = 'lzw')

ggplot(data = merged_N2, aes(x=TO, y=FROM, fill=logBF)) + 
  geom_tile(color="white") + 
  coord_equal() +
  ylab(label = "DONOR") + 
  xlab(label = "RECIPIENT") + 
  #scale_fill_viridis(option="inferno") + 
  scale_fill_viridis(name = "lnBF",
                     option="inferno",
    rescaler = function(x, to = c(0, 1), from = NULL) {
    ifelse(x<10, 
           scales::rescale(x,
                           to = to,
                           from = c(min(x, na.rm = TRUE), 10)),
           1)
    }) +
  #ggtitle("N2.2002 Ln Bayes Factor of HA Clade Reassortment") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1,size = 16,face = "bold"),
        strip.text.x = element_text(size = 16),
        plot.title = element_text(size=18,face="bold"),
        axis.text.y = element_text(size = 16,face = "bold"),
        panel.background = element_blank()) + 
  facet_wrap(~N2, ncol = 2)

dev.off()

#FIGURE4
#Plot posterior prob
ggplot(data = merged_N2, aes(x=TO, y=FROM, fill=POSTERIOR.PROBABILITY)) + 
  geom_tile(color="gray80") + 
  coord_equal() +
  scale_fill_viridis(option="inferno") +
  ggtitle("Posterior Probability of N2.2002 HA Clade Reassortment") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1,size = 16,face = "bold"),
        plot.title = element_text(size=18,face="bold"),
        axis.text.y = element_text(size = 16,face = "bold"),
        panel.background = element_blank()) +
  facet_wrap(~N2, ncol = 2)

