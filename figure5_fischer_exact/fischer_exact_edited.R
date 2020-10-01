library(reshape2)
library(dplyr)
library(ggplot2)
#library(gridExtra)
#library(cowplot)
#library(ggpubr)

###########
#LOAD DATA#
###########
allData <- read.csv("pair_list.csv")

#Reshape data
haPair <- dcast(allData, na_clade ~ ha_clade, value.var = "ha_clade")
rownames(haPair) <- haPair$na_clade
haPair <- subset(haPair, select = -c(na_clade))
haPair

##############################
#CHI SQUARE & Fischer's Exact#
##############################
(chi <- chisq.test(haPair))
(fet <- fisher.test(haPair, workspace = 2e+5, simulate.p.value = TRUE, B = 10000))

##################
#Retest 02 and 98#
##################
haPair02 <- haPair[c(6,7),]
haPair02 <- subset(haPair02, select = -c(`2010.2`,`gamma2-beta-like`,`cluster_ivf`,`unknown`, `delta-like`)) #remove zero columns
haPair98 <- haPair[c(4,5),]
haPair98 <- subset(haPair98, select = c(`delta2`, `delta1b`, `cluster_ivf`)) #remove columns < 10
(chi02 <- chisq.test(haPair02))
(fet <- fisher.test(haPair02, workspace = 2e+5, simulate.p.value = TRUE, B = 10000))
(chi98 <- chisq.test(haPair98))
(fet <- fisher.test(haPair98, workspace = 2e+5, simulate.p.value = TRUE, B = 10000))

####################################
#Posthoc determination of preference#
####################################
#find standardized residuals 2002
#https://rpubs.com/melike/heatmapTable
na02 <- melt(chi02$stdres)
na02 <- na02[order(na02$value),] #sort
(plot1 <- ggplot(na02, aes(Var1, Var2)) + # x and y axes => Var1 and Var2
  geom_tile(aes(fill = value)) + # background colours are mapped according to the value column
  geom_text(aes(fill = na02$value, label = round(na02$value, 2))) + # write the values
  coord_equal() +
  scale_fill_gradient2(low = "#67adea", 
                       mid = "white", 
                       high = "#ed4e4e", 
                       midpoint = 0) +  # determine the colour
  theme(panel.grid.major.x=element_blank(), #no gridlines
        panel.grid.minor.x=element_blank(), 
        panel.grid.major.y=element_blank(), 
        panel.grid.minor.y=element_blank(),
        panel.background=element_rect(fill="white"), # background=white
        axis.text.x = element_text(angle=90, hjust = 1,vjust=1,size = 18,face = "bold"),
        plot.title = element_text(size=18,face="bold"),
        axis.text.y = element_text(size = 18,face = "bold"),
        legend.position = "none") + # JC: drop a legend
  ggtitle("Post hoc N2.2002 STD Residuals") + 
  theme(legend.title=element_text(face="bold", size=10)) + 
  scale_x_discrete(name="") +
  scale_y_discrete(name="") +
  labs(fill="STD Res"))
#write.csv(chi$stdres,"stdres.csv")

#find standardized residuals 1998
na98 <- melt(chi98$stdres)
na98 <- na98[order(na98$value),] #sort
(plot2 <- ggplot(na98, aes(Var1, Var2)) + # x and y axes => Var1 and Var2
  geom_tile(aes(fill = value, width=1, height=1)) + # background colours are mapped according to the value column
  geom_text(aes(fill = na98$value, label = round(na98$value, 2))) + # write the values
  #geom_text(aes(fill = na98$value, label = round(na98$value, 2)), size=10) + # write the values
  coord_equal() +
  scale_fill_gradient2(low = "#67adea", 
                       mid = "white", 
                       high = "#ed4e4e", 
                       midpoint = 0) +  # determine the colour
  theme(panel.grid.major.x=element_blank(), #no gridlines
        panel.grid.minor.x=element_blank(), 
        panel.grid.major.y=element_blank(), 
        panel.grid.minor.y=element_blank(),
        panel.background=element_rect(fill="white"), # background=white
        axis.text.x = element_text(angle=90, hjust = 1,vjust=1,size = 18,face = "bold"),
        plot.title = element_text(size=18,face="bold"),
        axis.text.y = element_text(size = 18,face = "bold")) + 
  ggtitle("Post Hoc N2.1998 STD Residuals") + 
  theme(legend.title=element_text(face="bold", size=10)) + 
  scale_x_discrete(name="") +
  scale_y_discrete(name="") +
  labs(fill="STD Res"))

#grid.arrange(plot1,plot2,nrow = 1)

#plot_grid(plot1, plot2,labels = "AUTO", align='v')
# Yeah...resizing everything in ggplot2 would be a pain, 
# since benefit of ggplot2 is it makes sizing decisions for you.
# A suggested facet wrap below:

# ===== Jennifer Chang
na98$N2="N2.1998" # Can change it back to 02, 98 if you want the same order as your original
na02$N2="N2.2002"
na98 <- data.frame(lapply(na98, as.character), stringsAsFactors=FALSE)
na98$value=as.numeric(na98$value)
na02 <- data.frame(lapply(na02, as.character), stringsAsFactors=FALSE)
na02$value=as.numeric(na02$value)
bothna=rbind(na98,na02)
str(bothna)
(plot3 <- ggplot(bothna, aes(Var1, Var2)) + # x and y axes => Var1 and Var2
    geom_tile(aes(fill = value, width=1, height=1)) + # background colours are mapped according to the value column
    geom_text(aes(fill = bothna$value, label = round(bothna$value, 2))) + # write the values
    #coord_equal() +
    scale_fill_gradient2(low = "#67adea", 
                         mid = "white", 
                         high = "#ed4e4e", 
                         midpoint = 0) +  # determine the colour
    theme(panel.grid.major.x=element_blank(), #no gridlines
          panel.grid.minor.x=element_blank(), 
          panel.grid.major.y=element_blank(), 
          panel.grid.minor.y=element_blank(),
          panel.background=element_rect(fill="white"), # background=white
          axis.text.x = element_text(angle=90, hjust = 1,vjust=0.5,size = 14,face = "bold"),
          plot.title = element_text(size=18,face="bold"),
          axis.text.y = element_text(size = 14,face = "bold")) + 
    #    ggtitle("Post hoc N2 Std Residuals") + 
    theme(legend.title=element_text(face="bold", size=12)) + 
    #    scale_x_discrete(name="") +
    #    scale_y_discrete(name="") +
    labs(fill="Std Res", x="", y="")+
  facet_grid(~N2,scale="free_x",space="free_x"))


#Raw Counts
obs_na02 <- melt(chi02$observed)
obs_na02 <- obs_na02[order(obs_na02$value),] #sort
obs_na98 <- melt(chi98$observed)
obs_na98 <- obs_na98[order(obs_na98$value),] #sort

obs_na98$N2="N2.1998" # Can change it back to 02, 98 if you want the same order as your original
obs_na02$N2="N2.2002"
obs_na98 <- data.frame(lapply(obs_na98, as.character), stringsAsFactors=FALSE)
obs_na98$value=as.numeric(obs_na98$value)
obs_na02 <- data.frame(lapply(obs_na02, as.character), stringsAsFactors=FALSE)
obs_na02$value=as.numeric(obs_na02$value)
obs_bothna=rbind(obs_na98,obs_na02)
str(obs_bothna)
(plot4 <- ggplot(obs_bothna, aes(Var1, Var2)) + # x and y axes => Var1 and Var2
    geom_tile(aes(fill = value, width=1, height=1)) + # background colours are mapped according to the value column
    geom_text(aes(fill = obs_bothna$value, label = round(obs_bothna$value, 2))) + # write the values
    #coord_equal() +
    scale_fill_gradient2(low = "#67adea", 
                         mid = "white", 
                         high = "#ed4e4e", 
                         midpoint = 0) +  # determine the colour
    theme(panel.grid.major.x=element_blank(), #no gridlines
          panel.grid.minor.x=element_blank(), 
          panel.grid.major.y=element_blank(), 
          panel.grid.minor.y=element_blank(),
          panel.background=element_rect(fill="white"), # background=white
          axis.text.x = element_text(angle=90, hjust = 1,vjust=1,size = 14,face = "bold"),
          plot.title = element_text(size=18,face="bold"),
          axis.text.y = element_text(size = 14,face = "bold")) + 
    #ggtitle("Observed number of detections") + 
    theme(legend.title=element_text(face="bold", size=12)) + 
    scale_x_discrete(name="") +
    scale_y_discrete(name="") +
    labs(fill="Detections")+
  facet_grid(~N2, scale="free",space="free"))

#Plot together
library(cowplot)
(mixplot <- plot_grid(plot4, plot3, labels = "AUTO", label_size = 18))

(mixplot <- plot_grid(plot4, labels = "", label_size = 18))
