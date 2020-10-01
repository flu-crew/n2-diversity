#Libraries
require(ggplot2)
library(BayesTwin)
library(cowplot)

#Functions
plotHistogram <- function(df, x, y) {
  #Calculate
  hiNum <- round(colSums((x-y) > 0)/nrow(x-y),2)
  loNum <- round(colSums((x-y) < 0)/nrow(x-y),2)
  textPercent <- paste(loNum, "% < 0 < ", hiNum, "%", sep="")
  textXAxis <- expression(paste(mu,"Rate1 - ",mu,"Rate2", sep=""))
  df["muDiff"] = x - y
  hpdLimits <- HPD(x-y)
  
  #Plot
  ggplot(df, aes(x=muDiff)) + 
    geom_histogram(bins=50, colour="black", fill="white", alpha=0.6) +                        # Histrogram
    geom_vline(aes(xintercept=mean(x=muDiff)), color="red", linetype="dashed", size=1) +      # Mean lines
    geom_vline(aes(xintercept=0), color="black", linetype="solid", size=2) +                  # Zero line
    geom_vline(aes(xintercept=hpdLimits[1]), color="blue", linetype="dashed", size=1) +       # Low HPD
    geom_vline(aes(xintercept=hpdLimits[2]), color="blue", linetype="dashed", size=1) +       # High HPD
    annotate(geom="text", x=mean(x-y), y=Inf, label=textPercent, color="red", fontface="bold", vjust=1, hjust=0.5) +
    annotate(geom="text", x=hpdLimits[1], y=0, label=hpdLimits[1], color="blue", fontface="bold", vjust=0, hjust=0) +
    annotate(geom="text", x=hpdLimits[2], y=0, label=hpdLimits[2], color="blue", fontface="bold", vjust=0, hjust=1) +
    scale_x_continuous(name =textXAxis)
}

#Load in data
subset1 <- read.csv("results/subset1.log.txt", skip = 3, header = T, sep="\t")
subset4 <- read.csv("results/subset4.log.txt", skip = 3, header = T, sep="\t")
subset5 <- read.csv("results/subset5.log.txt", skip = 3, header = T, sep="\t")
subset6 <- read.csv("results/subset6.log.txt", skip = 3, header = T, sep="\t")
subset7 <- read.csv("results/subset7.log.txt", skip = 3, header = T, sep="\t")
subset8 <- read.csv("results/subset8.log.txt", skip = 3, header = T, sep="\t")
subset9 <- read.csv("results/subset9.log.txt", skip = 3, header = T, sep="\t")

#Burn-in 10% of data
subset1 <- subset1[-c(1:101),]
subset4 <- subset4[-c(1:101),]
subset5 <- subset5[-c(1:101),]
subset6 <- subset6[-c(1:101),]
subset7 <- subset7[-c(1:101),]
subset8 <- subset8[-c(1:101),]
subset9 <- subset9[-c(1:101),]

#Bayesian estimation suspersedes t-test
x <- as.matrix(subset1["civf.meanRate"])
y <- as.matrix(subset1["d1b.meanRate"])
(plot1 <- plotHistogram(subset1,x,y))

x <- as.matrix(subset4["X2010.1.aln.meanRate"])
y <- as.matrix(subset4["delta1b.aln.meanRate"])
(plot4 <- plotHistogram(subset4,x,y))

x <- as.matrix(subset5["alpha.aln.meanRate"])
y <- as.matrix(subset5["cluster_ivb.aln.meanRate"])
(plot5 <- plotHistogram(subset5,x,y))

x <- as.matrix(subset6["X2010.1.aln.meanRate"])
y <- as.matrix(subset6["alpha.aln.meanRate"])
(plot6 <- plotHistogram(subset6,x,y))

x <- as.matrix(subset7["civa.aln.meanRate"])
y <- as.matrix(subset7["civb.aln.meanRate"])
(plot7 <- plotHistogram(subset7,x,y))

x <- as.matrix(subset8["civa.aln.meanRate"])
y <- as.matrix(subset8["delta1a.aln.meanRate"])
(plot8 <- plotHistogram(subset8,x,y))

x <- as.matrix(subset9["X2010.1.aln.meanRate"])
y <- as.matrix(subset9["delta1a.aln.meanRate"])
(plot9 <- plotHistogram(subset9,x,y))

#Pretty plot
tiff('test.tiff', units="in", width=13.5, height=9, res=300, compression = 'lzw')

plot_grid(plot1, plot4, plot5, plot6, plot7, plot8, plot9,
          labels = c("A", "B", "C", "D", "E", "F", "G"),
          ncol = 3, nrow = 3)
dev.off()


###Frequentist analysis, ignore
#Kolmogorov-Smirnov test
x <- as.matrix(subset1["civf.meanRate"])
y <- as.matrix(subset1["d1b.meanRate"])
ks.test(x,y)
t.test(x,y)
wilcox.test(x,y)
kruskal.test(list(x,y)) 
chisq.test(x,y)

x <- as.matrix(subset4["X2010.1.aln.meanRate"])
y <- as.matrix(subset4["delta1b.aln.meanRate"])
hist(x)
hist(y)
hist(x-y)
ks.test(x,y)
t.test(x,y)
wilcox.test(x,y)
kruskal.test(list(x,y))
chisq.test(x,y)

x <- as.matrix(subset5["alpha.aln.meanRate"])
y <- as.matrix(subset5["cluster_ivb.aln.meanRate"])
ks.test(x,y)
t.test(x,y)
wilcox.test(x,y)
kruskal.test(list(x,y))
chisq.test(x,y)

x <- as.matrix(subset6["X2010.1.aln.meanRate"])
y <- as.matrix(subset6["alpha.aln.meanRate"])
ks.test(x,y)
t.test(x,y)
wilcox.test(x,y)
kruskal.test(list(x,y))
chisq.test(x,y)

x <- as.matrix(subset7["civa.aln.meanRate"])
y <- as.matrix(subset7["civb.aln.meanRate"])
ks.test(x,y)
t.test(x,y)
wilcox.test(x,y)
kruskal.test(list(x,y))
chisq.test(x,y)

x <- as.matrix(subset8["civa.aln.meanRate"])
y <- as.matrix(subset8["delta1a.aln.meanRate"])
ks.test(x,y)
t.test(x,y)
wilcox.test(x,y)
kruskal.test(list(x,y))
chisq.test(x,y)

x <- as.matrix(subset9["X2010.1.aln.meanRate"])
y <- as.matrix(subset9["delta1a.aln.meanRate"])
ks.test(x,y)
t.test(x,y)
wilcox.test(x,y)
kruskal.test(list(x,y))
chisq.test(x,y)

# https://rpubs.com/mharris/KSplot
# simulate two distributions - your data goes here!
group <- c(rep("sample1", length(x)), rep("sample2", length(y)))
dat <- data.frame(KSD = c(x,y), group = group)
# create ECDF of data
cdf1 <- ecdf(x) 
cdf2 <- ecdf(y) 
# find min and max statistics to draw line between points of greatest distance
minMax <- seq(min(x, y), max(x, y), length.out=length(x)) 
x0 <- minMax[which( abs(cdf1(minMax) - cdf2(minMax)) == max(abs(cdf1(minMax) - cdf2(minMax))) )] 
y0 <- cdf1(x0) 
y1 <- cdf2(x0) 

ggplot(dat, aes(x = KSD, group = group, color = group))+
  stat_ecdf(size=1) +
  theme_bw(base_size = 28) +
  theme(legend.position ="top") +
  xlab("Sample") +
  ylab("ECDF") +
  #geom_line(size=1) +
  geom_segment(aes(x = x0[1], y = y0[1], xend = x0[1], yend = y1[1]),
               linetype = "dashed", color = "red") +
  geom_point(aes(x = x0[1] , y= y0[1]), color="red", size=8) +
  geom_point(aes(x = x0[1] , y= y1[1]), color="red", size=8) +
  ggtitle("K-S Test: Sample 1 / Sample 2") +
  theme(legend.title=element_blank())



#hiNum <- round(colSums((x-y) > 0)/nrow(x-y),2)
#loNum <- round(colSums((x-y) < 0)/nrow(x-y),2)
#textPercent <- paste(loNum, "% < 0 < ", hiNum, "%", sep="")
#textXAxis <- expression(paste(mu,"Rate1 - ",mu,"Rate2", sep=""))
#subset4["muDiff"] = x - y
#hpdLimits <- HPD(x-y)
#ggplot(subset4, aes(x=muDiff)) + 
#  geom_histogram(bins=50, colour="black", fill="white", alpha=0.6) +                        # Histrogram
#  geom_vline(aes(xintercept=mean(x=muDiff)), color="red", linetype="dashed", size=1) +      # Mean lines
#  geom_vline(aes(xintercept=0), color="black", linetype="solid", size=2) +                  # Zero line
#  geom_vline(aes(xintercept=hpdLimits[1]), color="blue", linetype="dashed", size=1) +       # Low HPD
#  geom_vline(aes(xintercept=hpdLimits[2]), color="blue", linetype="dashed", size=1) +       # High HPD
#  annotate(geom="text", x=mean(x-y), y=Inf, label=textPercent, color="red", fontface="bold", vjust=1, hjust=0.5) +
#  annotate(geom="text", x=hpdLimits[1], y=0, label=hpdLimits[1], color="blue", fontface="bold", vjust=0, hjust=0) +
#  annotate(geom="text", x=hpdLimits[2], y=0, label=hpdLimits[2], color="blue", fontface="bold", vjust=0, hjust=1) +
#  scale_x_continuous(name =textXAxis)
