library(reshape2)
library(maps)
library(ggplot2)
library(dplyr)
library(mapproj)
library(viridis)

###########
#LOAD DATA#
###########
allData <- read.csv("states_clades.csv")

#Reshape data
haState <- dcast(allData, state ~ HA_clade, value.var = "HA_clade")
rownames(haState) <- haState$state
haState <- subset(haState, select = -c(state))
#haState <- subset(haState, select = -c(pdmh1,gamma,delta_like,beta,cluster_ivf,cluster_ive,cluster_ivd,cluster_ivc))

#Reshape data
naState <- dcast(allData, state ~ NA_clade, value.var = "NA_clade")
rownames(naState) <- naState$state
keeps <- c("N2.2002A","N2.2002B","N2.1998A","N2.1998B")
naState <- subset(naState, select=keeps)

#plot
barplot(as.matrix(t(naState)),beside = TRUE)

##############################
#CHI SQUARE & Fischer's Exact#
##############################
(chi <- chisq.test(naState))
(fet <- fisher.test(naState, workspace = 2e+5, simulate.p.value = TRUE, B = 10000))
#both p-values are significant

##################
#POST HOC TESTING#
##################
#find standardized residuals 
#https://rpubs.com/melike/heatmapTable
naCo <- melt(t(chi$stdres))
naCo <- naCo[order(naCo$value),] #sort
ggplot(naCo, aes(Var1, Var2)) + # x and y axes => Var1 and Var2
  geom_tile(aes(fill = value)) + # background colours are mapped according to the value column
  geom_text(aes(fill = naCo$value, label = round(naCo$value, 2))) + # write the values
  scale_fill_gradient2(low = "#67adea", 
                       mid = "white", 
                       high = "#ed4e4e", 
                       midpoint = 0) +  # determine the colour
  theme(panel.grid.major.x=element_blank(), #no gridlines
        panel.grid.minor.x=element_blank(), 
        panel.grid.major.y=element_blank(), 
        panel.grid.minor.y=element_blank(),
        panel.background=element_rect(fill="white"), # background=white
        axis.text.x = element_text(angle=90, hjust = 1,vjust=1,size = 12,face = "bold"),
        plot.title = element_text(size=20,face="bold"),
        legend.title=element_text(face="bold", size=14),
        axis.text.y = element_text(size = 12,face = "bold")) + 
  ggtitle("Post Hoc Chi Sq STD Residuals") + 
  scale_x_discrete(name="") +
  scale_y_discrete(name="") +
  labs(fill="STD Res")
#write.csv(chi$stdres,"stdres.csv")


##############
#PLOT NA MAPS#
##############
#http://socviz.co/maps.html
##ggplot
us_states <- map_data("state")

#Melt data into usable format
naState <- dcast(allData, state ~ NA_clade, value.var = "NA_clade")
naState = naState[,c(1,4,5,7,8)]
rownames(naState) <- naState$state
naState <- melt(naState)
naState$region <- tolower(naState$state)
us_states <- map_data("state")
us_states_na <- left_join(us_states, naState)
#us_states_na$value[is.na(us_states_na$value)] <- 0

# ==== Jennifer (this is a hack, probably should fix this somewhere earlier)
st_na<-subset(us_states_na, is.na(variable)) # hope NAs are the same across all N2's :P
for(s in c("N2.2002A","N2.2002B","N2.1998A","N2.1998B")){        # add NAs states for each group
  print(s)
  st_na$variable=s
  us_states_na=rbind(us_states_na,st_na)
}
us_states_na=subset(us_states_na, !is.na(variable)) # Remove the NAs
# ==== End Hack


#Plot geographies
(plot1 <- ggplot(data = us_states_na, aes(x = long, y = lat, group = group, fill = value)) + 
  geom_polygon(color = "black", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  #scale_fill_gradient(low = "#EEEEEE", high = "#CB454A") +
  scale_fill_viridis(option="viridis") +
  #labs(title = "N2 detection by state") +
  theme(axis.title.x=element_blank(),
    strip.text.x = element_text(size = 16),
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank(),
    axis.title.y=element_blank(),
    axis.text.y=element_blank(),
    axis.ticks.y=element_blank(),
    plot.title = element_text(size=20,face="bold"),
    legend.title=element_text(face="bold", size=16),
    legend.text=element_text(face="bold", size=16))+
facet_wrap(~variable, ncol = 2)
)

#Load data
bf98A <- read.csv("98A.txt", sep="\t")
bf98B <- read.csv("98B.txt", sep="\t")
bf02A <- read.csv("02A.txt", sep="\t")
bf02B <- read.csv("02B.txt", sep="\t")
bf98A$logBF = log(bf98A$BAYES_FACTOR)
bf98B$logBF = log(bf98B$BAYES_FACTOR)
bf02A$logBF = log(bf02A$BAYES_FACTOR)
bf02B$logBF = log(bf02B$BAYES_FACTOR)

#Plot ln(BF)
bf98A$N2 = "N2.1998A"
bf98B$N2 = "N2.1998B"
bf02A$N2 = "N2.2002A"
bf02B$N2 = "N2.2002B"
allbf = rbind(bf98A,bf98B,bf02A,bf02B)

#Presort
allbf <- allbf[with(allbf, order(allbf$FROM, allbf$TO)),]

#Eliminate some data
allbf <- allbf[!(allbf$FROM == "Colorado"),]
allbf <- allbf[!(allbf$FROM == "Maryland"),]
allbf <- allbf[!(allbf$FROM == "Wyoming"),]
allbf <- allbf[!(allbf$FROM == "Mississippi"),]
allbf <- allbf[!(allbf$FROM == "Alabama"),]
allbf <- allbf[!(allbf$FROM == "Virginia"),]
allbf <- allbf[!(allbf$FROM == "South_Carolina"),]
allbf <- allbf[!(allbf$FROM == "Georgia"),]
allbf <- allbf[!(allbf$FROM == "California"),]
allbf <- allbf[!(allbf$FROM == "Texas"),]
allbf <- allbf[!(allbf$FROM == "Montana"),]
allbf <- allbf[!(allbf$TO == "Colorado"),]
allbf <- allbf[!(allbf$TO == "Maryland"),]
allbf <- allbf[!(allbf$TO == "Wyoming"),]
allbf <- allbf[!(allbf$TO == "Mississippi"),]
allbf <- allbf[!(allbf$TO == "Alabama"),]
allbf <- allbf[!(allbf$TO == "Virginia"),]
allbf <- allbf[!(allbf$TO == "South_Carolina"),]
allbf <- allbf[!(allbf$TO == "Georgia"),]
allbf <- allbf[!(allbf$TO == "California"),]
allbf <- allbf[!(allbf$TO == "Texas"),]
allbf <- allbf[!(allbf$TO == "Montana"),]

#Plot
(plot2 <- ggplot(data = allbf, aes(x=TO, y=FROM, fill=logBF)) + 
  geom_tile(color="white") + 
  coord_equal() +
  scale_fill_viridis(option="inferno",
                     name = "lnBF",
                     #Rescale to account BF between 0 and 10
                     rescaler = function(x, to = c(0, 1), from = NULL) {
                       ifelse(x<10, 
                              scales::rescale(x,
                                              to = to,
                                              from = c(min(x, na.rm = TRUE), 10)),
                              1)
                     }
  ) +
  #scale_fill_gradientn(colors = viridis_pal(option="inferno")(9), limits=c(-5, 10), na.value = "#FDE725FF") +
  #ggtitle("Ln Bayes Factor of N2 Location Transmission") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size=12,vjust=0.5),
        panel.background = element_blank(),
        #axis.title.x=element_blank(),
        strip.text.x = element_text(size = 14),
        #axis.ticks.x=element_blank(),
        #axis.title.y=element_blank(),
        axis.text.y=element_text(size = 12),
        #axis.ticks.y=element_blank(),
        plot.title = element_text(size=24,face="bold"),
        legend.title=element_text(face="bold", size=16),
        legend.text=element_text(face="bold", size = 14)) +
  facet_wrap(~N2,nrow = 2)
)

#Plot together as A, B
library(cowplot)
plot_grid(plot1, plot2, labels = "AUTO", label_size = 18, nrow = 1, hjust = 0, vjust = 1,scale = c(1.0, 0.7)) #Figure 5
 plot_grid(plot1, plot2, labels = "AUTO", align = 'v', scale = c(1.5, 1.0))
plot_grid(plot1, plot2, labels = "AUTO", align = 'v', rel_widths = c(1.8,1))

#mixplot <- plot_grid(plot1, plot2, labels = "AUTO", label_size = 18)
#title <- ggdraw() + draw_label("Effective population size over time", fontface='bold', size = 20)
#plot_grid(mixplot, ncol=1, rel_heights=c(0.1, 1))

