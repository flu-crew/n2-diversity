#! /usr/bin/env Rscript
# Auth: Michael Zeller 5/28/2019

#Load needed libraries
library(ape)
library(ggplot2)
library(lubridate)
library(gap)
library(cowplot)
library(gridExtra)

#Substring functions
#https://stackoverflow.com/questions/7963898/extracting-the-last-n-characters-from-a-string-in-r
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

getDistFromOldest <- function(t){
  #Grab R from commandline arguments, or print usage
  treeFile1 <- t
  
  #Load user specified tree file
  tree1<-read.tree(treeFile1)
  
  #Get patristic distance matrix
  dist1 <-cophenetic(tree1)
  
  #Get names
  dates1 <- substrRight(colnames(dist1),10)
  dates1 <- as.Date(dates1, format = "%m/%d/%Y")
  if(is.na(dates1[1])) {
    dates1 <- substrRight(colnames(dist1),10)
    dates1 <- as.Date(dates1, tryFormats = c("%m/%d/%Y","%Y-%m-%d"))
  }
  oldestId <- which(dates1 == sort(dates1)[1])[[1]]
  
  #Calculate decimal date
  decdate <- decimal_date(dates1)
  
  #Get clades
  clades <- sapply(strsplit(colnames(dist1), "|", fixed = TRUE), "[", 3)
  
  #Return
  resultFrame <- data.frame("dates" = dates1, "decimalDate" = decdate, "dist" = dist1[oldestId,], "clade" = clades)
  return(resultFrame)
}

getBranchFrames <- function(t){
  #Grab R from commandline arguments, or print usage
  treeFile1 <- t
  
  #Load user specified tree file
  tree1<-read.tree(treeFile1)
  
  #Set up dataframe
  tipLen<-length(tree1$tip.label)
  treeInfo<-setNames(tree1$edge.length[sapply(1:tipLen,function(x,y) which(y==x),y=tree1$edge[,2])],tree1$tip.label)
  resultFrame <- data.frame("branchLengths" = treeInfo)
  clades <- sapply(strsplit(rownames(resultFrame), "|", fixed = TRUE), "[", 3)
  dates <- sapply(strsplit(rownames(resultFrame), "|", fixed = TRUE), "[", 4)
  dates <- as.Date(dates, tryFormats = c("%m/%d/%Y","%Y-%m-%d"))
  resultFrame <- cbind("names" = rownames(resultFrame), resultFrame, "clade" = clades, "date" = dates)
  
  return(resultFrame)
}

plotCophenDist<- function(df){
  (ggplot(df, aes(dates, dist, color = clade)) +
     geom_point() + 
     geom_smooth(method = "lm", se = FALSE) + 
     theme(legend.position="bottom")
  )
}

plotBranchHisto <- function(df){
  (ggplot(df, aes(x = branchLengths, color = clade, fill = clade)) +
     geom_histogram(aes(y=..density..), binwidth = 0.001, alpha=0.6, position="identity", lwd=0.2) +
     theme(legend.position="bottom")
  )
}

plotTimeDist <- function(df){
  (ggplot(df, aes(date, branchLengths, color = clade)) +
     geom_point(aes(size = 2)) 
     #theme(axis.text.x = element_text(angle = 90, hjust = 1))
  )
}

manualChow <- function(lmPooled, lmA, lmB){
  #Get residual sum of squares
  rssA <- sum(residuals(lmA)^2)
  rssB <- sum(residuals(lmB)^2)
  rssPooled <- sum(residuals(lmPooled)^2)
  
  #Assume intercepts = 2
  k = 2
  
  #Calculate f critical chow statistic at the 95% confidence level
  fcrit = qf(0.95, df1 = lmA$df, df2 = lmB$df)
  chowStat = ((rssPooled - (rssA + rssB))/k)/((rssA+rssB)/(lmA$df + lmB$df - (2*k)))
  p <- pf(chowStat, lmA$df, lmB$df, lower.tail=FALSE)
  
  #Return
  result <- c(fcrit, chowStat, p)
  names(result) = c("F-critical", "Chow Statistic", "P-value")
  return(result)
}

#Draw
#reassort234 <- getDistFromOldest("2-3-4/reassortment234.tre")
#reassort56 <- getDistFromOldest("5-6/reassortment56.tre")
#reassort789 <- getDistFromOldest("7-8-9/reassortment789.tre")
reassort1 <- getDistFromOldest("1/short.tre")
reassort4 <- getDistFromOldest("4/redo_reassort4.tre")
reassort5 <- getDistFromOldest("5/redo_reassort5.tre")
reassort6 <- getDistFromOldest("6/redo_reassort6.tre")
reassort7 <- getDistFromOldest("7/redo_reassort7.tre")
reassort9 <- getDistFromOldest("9/redo_reassort9.tre")

#Change the order of factors so the donor HA is on the right, and recpient HA is on right for all legends (red then cyan)
reassort4$clade <- factor(reassort4$clade, levels = rev(levels(reassort4$clade)))
reassort5$clade <- factor(reassort5$clade, levels = rev(levels(reassort5$clade)))
reassort6$clade <- factor(reassort6$clade, levels = rev(levels(reassort6$clade)))
reassort7$clade <- factor(reassort7$clade, levels = rev(levels(reassort7$clade)))
reassort8$clade <- factor(reassort8$clade, levels = rev(levels(reassort8$clade)))
reassort9$clade <- factor(reassort9$clade, levels = rev(levels(reassort9$clade)))

#ggplot
#install.packages("gap") #https://rdrr.io/cran/gap/man/chow.test.html
(plot1 <- plotCophenDist(reassort1))
set1 = reassort1[which(reassort1$clade == "cluster_ivf"),]
set2 = reassort1[which(reassort1$clade == "delta1b"),]
(chow.test(set1$dist, set1$decimalDate, set2$dist, set2$decimalDate))

(plot4 <- plotCophenDist(reassort4))
set1 = reassort4[which(reassort4$clade == "delta1b"),]
set2 = reassort4[which(reassort4$clade == "2010.1"),]
(chow.test(set1$dist, set1$decimalDate, set2$dist, set2$decimalDate))

plot5 <- plotCophenDist(reassort5)
set1 = reassort5[which(reassort5$clade == "alpha"),]
set2 = reassort5[which(reassort5$clade == "cluster_ivb"),]
(chow.test(set1$dist, set1$decimalDate, set2$dist, set2$decimalDate))

(plot6 <- plotCophenDist(reassort6))
set1 = reassort6[which(reassort6$clade == "alpha"),]
set2 = reassort6[which(reassort6$clade == "2010.1"),]
(chow.test(set1$dist, set1$decimalDate, set2$dist, set2$decimalDate))

plot7 <- plotCophenDist(reassort7)
set1 = reassort7[which(reassort7$clade == "cluster_iva"),]
set2 = reassort7[which(reassort7$clade == "cluster_ivb"),]
(chow.test(set1$dist, set1$decimalDate, set2$dist, set2$decimalDate))

plot8 <- plotCophenDist(reassort8)
set1 = reassort8[which(reassort8$clade == "cluster_iva"),]
set2 = reassort8[which(reassort8$clade == "delta1a"),]
(chow.test(set1$dist, set1$decimalDate, set2$dist, set2$decimalDate))

plot9 <- plotCophenDist(reassort9)
set1 = reassort9[which(reassort9$clade == "2010.1"),]
set2 = reassort9[which(reassort9$clade == "delta1a"),]
(chow.test(set1$dist, set1$decimalDate, set2$dist, set2$decimalDate))

######################
#Plotting by N2 clade#
######################
#grid.arrange(plot1, plot4, plot5, plot6, plot7, plot8, plot9, ncol = 3, nrow = 3)
plot_grid(plot1, plot4, plot5, plot6, plot7, plot8, plot9,
          labels = c("A", "B", "C", "D", "E", "F", "G"),
          ncol = 3, nrow = 3)

##########################
#Naihui Zhou's suggestion#
##########################
fitTest = lm(dist ~ decimalDate * clade, reassort1)
summary(fitTest)
anova(fitTest)

fitTest = lm(dist ~ decimalDate * clade, reassort4)
summary(fitTest)
anova(fitTest)

fitTest = lm(dist ~ decimalDate * clade, reassort5)
summary(fitTest)
anova(fitTest)

fitTest = lm(dist ~ decimalDate * clade, reassort6)
summary(fitTest)
anova(fitTest)

fitTest = lm(dist ~ decimalDate * clade, reassort7)
summary(fitTest)
anova(fitTest)

fitTest = lm(dist ~ decimalDate * clade, reassort8)
summary(fitTest)
anova(fitTest)

fitTest = lm(dist ~ decimalDate * clade, reassort9)
summary(fitTest)
anova(fitTest)


#########
#TESTING#
#########
#https://www.youtube.com/watch?v=uUxJk4UGXOg
#http://aoki2.si.gunma-u.ac.jp/R/Chow.html
fita = lm(dist ~ decimalDate, reassort1, subset = clade == "cluster_ivf")
fitb = lm(dist ~ decimalDate, reassort1, subset = clade == "delta1b")
fitp = lm(dist ~ decimalDate, reassort1)
(chow <- manualChow(fitp,fita,fitb))

fita = lm(dist ~ decimalDate, reassort8, subset = clade == "cluster_iva")
fitb = lm(dist ~ decimalDate, reassort8, subset = clade == "delta1a")
fitp = lm(dist ~ decimalDate, reassort8)
(chow <- manualChow(fitp, fita, fitb))


########################
#TERMINAL LEAF PLOTTING#
########################

#http://blog.phytools.org/2013/10/finding-edge-lengths-of-all-terminal.html
#Get tip lengths
#Load user specified tree file
treeInfo1 <- getBranchFrames("1/redo_reassort1.tre")
treeInfo4 <- getBranchFrames("4/redo_reassort4.tre")
treeInfo5 <- getBranchFrames("5/redo_reassort5.tre")
treeInfo6 <- getBranchFrames("6/redo_reassort6.tre")
treeInfo7 <- getBranchFrames("7/redo_reassort7.tre")
treeInfo8 <- getBranchFrames("8/redo_reassort8.tre")
treeInfo9 <- getBranchFrames("9/redo_reassort9.tre")

plotBranchHisto(treeInfo1)
plotBranchHisto(treeInfo4)
plotBranchHisto(treeInfo5) 
plotBranchHisto(treeInfo6)
plotBranchHisto(treeInfo7)
plotBranchHisto(treeInfo8)
plotBranchHisto(treeInfo9)

plotTimeDist(treeInfo1)
plotTimeDist(treeInfo4)
plotTimeDist(treeInfo5)
plotTimeDist(treeInfo6)
plotTimeDist(treeInfo7)
plotTimeDist(treeInfo8)
plotTimeDist(treeInfo9)



