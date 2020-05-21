#! /usr/bin/env Rscript
# Auth: Michael Zeller 5/28/2019

#Load needed libraries
library(ape)
library(ggplot2)
library(lubridate)
library(gap)

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
     geom_point(aes(size = 2)) + 
     geom_smooth(method = "lm", se = FALSE)
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
reassortDelta1a <- getDistFromOldest("ha_delta1a/ha_delta1a.tre")
reassort2010 <- getDistFromOldest("ha_2010.1/ha_2010.1.tre")
reassortClusteriv <- getDistFromOldest("ha_clusteriv/clusteriv.tre")

#ggplot
#install.packages("gap") #https://rdrr.io/cran/gap/man/chow.test.html
plotCophenDist(reassortDelta1a)
set1 = reassortDelta1a[which(reassortDelta1a$clade == "02A"),]
set2 = reassortDelta1a[which(reassortDelta1a$clade == "02B"),]
(chow.test(set1$dist, set1$decimalDate, set2$dist, set2$decimalDate))

plotCophenDist(reassort2010)
set1 = reassort2010[which(reassort2010$clade == "02A"),]
set2 = reassort2010[which(reassort2010$clade == "02B"),]
(chow.test(set1$dist, set1$decimalDate, set2$dist, set2$decimalDate))

plotCophenDist(reassortClusteriv)
set1 = reassort2010[which(reassort2010$clade == "02A"),]
set2 = reassort2010[which(reassort2010$clade == "02B"),]
(chow.test(set1$dist, set1$decimalDate, set2$dist, set2$decimalDate))
