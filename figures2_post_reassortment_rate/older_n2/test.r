#Substring functions
#https://stackoverflow.com/questions/7963898/extracting-the-last-n-characters-from-a-string-in-r
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

#Grab R from commandline arguments, or print usage
treeFile1 <- "older_n2\\mixedevent3.tre"

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
  #dates1 <- as.Date(dates1, "%Y/%m/%d")
}
oldestId <- which(dates1 == sort(dates1)[1])[[1]]

#Get clades
clades <- sapply(strsplit(colnames(dist1), "|", fixed = TRUE), "[", 3)

#Return
resultFrame <- data.frame("dates" = dates1, "dist" = dist1[oldestId,], "clade" = clades)
(plot1 <- ggplot(resultFrame, aes(dates, dist, color = clade)) +
    geom_point(aes(size = 2)) + 
    geom_smooth(method = "lm", se = FALSE)
)
