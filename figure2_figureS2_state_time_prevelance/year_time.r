library(reshape2)
library(ggplot2)
library(viridis)
library(dplyr)

# Load data
clades <- read.csv("year_data.csv", na.strings = 0)
clades[is.na(clades)] <- 0

# Convert to long format
longClades <- melt(clades, id = c("year"))


ggplot(data = longClades, aes(x=year, y=value, color=variable, fill=variable))+
  #geom_smooth(se = FALSE)
  geom_line(size=1.5)
  
  
ggplot(data = longClades, aes(x=year, y=value, color=variable, fill=variable, ymin=0, ymax=value, alpha=0.7))+
  geom_ribbon() + 
  geom_line()

ggplot(data = longClades, aes(x=year, y=value, color=variable, fill=variable))+
  geom_area()

#normalize
normClades <- clades/rowSums(clades[2:11])
normClades[1] <- clades[1]
normLongClades <- melt(normClades, id = c("year"))

############
# Figure 2 #
############
tiff('figure2.tiff', units="in", width=13.5, height=6.75, res=300, compression = 'lzw')

ggplot(data = normLongClades, aes(x=year, y=value, color=variable, fill=variable))+
  geom_area(alpha=1, size=.5, colour="white") +
  scale_fill_viridis(discrete = T, option="inferno", name="HA-NA clade pair") + 
  labs(y = "Percent of N2 detection", x = "Year", color="test") +
  scale_x_continuous(limits = c(2009,2018), expand = c(0, 0.2)) +
  scale_y_continuous(limits = c(0,1), expand = c(0, 0)) +
  theme(legend.position = "bottom", 
        panel.background = element_rect(fill = "white"),
        axis.title = element_text(size=12,face="bold"),
        axis.text = element_text(size=11,face="bold"),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5),
        legend.title = element_text(size=12,),
        legend.text = element_text(size=12),
  )

dev.off()

########################
###State based Graphs###
########################
states <- read.csv("state_data.csv")

#Drop out clades not in top 10
topClades <- list("H1.delta1b-N2.2002A","H3.cluster_iva-N2.2002B","H1.delta1a-N2.2002B","H1.delta2-N2.1998B","H3.cluster_ivb-N2.2002B","H1.delta1a-N2.2002A","H3.2010.1-N2.2002B","H3.cluster_iva-N2.2002A","H3.2010.1-N2.2002A","H1.alpha-N2.2002B")
topStates <- list("Iowa","Illinois","North Carolina","Minnesota","Indiana","Nebraska")
states <-states [states$merged_clade %in% topClades,]
states <-states [states$State.Province %in% topStates,]

#State facet
stateList <- dcast(states, State.Province + collection_date ~ merged_clade)
stateItem <- melt(stateList, id = c("collection_date","State.Province"))

ggplot(data = stateItem, aes(x=collection_date, y=value, color=variable, fill=variable))+
  geom_area(alpha=1, size=.5, colour="white") +
  scale_fill_viridis(discrete = T, option="inferno", name="HA-NA clade pair") + 
  labs(y = "Percent of N2 detection", x = "Year", color="test") +
  #scale_x_continuous(limits = c(2009,2018), expand = c(0, 0.2)) +
  theme(legend.position = "bottom", 
        panel.background = element_rect(fill = "white"),
        axis.title = element_text(size=12,face="bold"),
        axis.text = element_text(size=11,face="bold"),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5),
        legend.title = element_text(size=12,),
        legend.text = element_text(size=12),
  ) +
  facet_wrap(~State.Province, ncol = 2)

ggplot(data = stateItem, aes(x=collection_date, y=value, color=variable, fill=variable))+
  geom_line(size = 1.5) +
  scale_fill_viridis(discrete = T, option="inferno", name="HA-NA clade pair") + 
  labs(y = "Percent of N2 detection", x = "Year", color="test") +
  #scale_x_continuous(limits = c(2009,2018), expand = c(0, 0.2)) +
  theme(legend.position = "bottom", 
        panel.background = element_rect(fill = "white"),
        axis.title = element_text(size=12,face="bold"),
        axis.text = element_text(size=11,face="bold"),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5),
        legend.title = element_text(size=12,),
        legend.text = element_text(size=12),
  ) +
  facet_wrap(~State.Province, ncol = 2)


data <- stateItem  %>%
  group_by(State.Province, collection_date, variable) %>%
  summarise(n = sum(value)) %>%
  mutate(percentage = n / sum(n))
         

#############
# Figure S2 #
#############
tiff('figures2.tiff', units="in", width=13.5, height=10, res=300, compression = 'lzw')

ggplot(data = data, aes(x=collection_date, y=percentage, color=variable, fill=variable))+
  geom_area(alpha=1, size=.5, colour="white") +
  scale_fill_viridis(discrete = T, option="inferno", name="HA-NA clade pair") + 
  labs(y = "Percent of N2 detection", x = "Year", color="test") +
  #scale_x_continuous(limits = c(2009,2018), expand = c(0, 0.2)) +
  theme(legend.position = "bottom", 
        panel.background = element_rect(fill = "white"),
        axis.title = element_text(size=12,face="bold"),
        axis.text = element_text(size=11,face="bold"),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5),
        legend.title = element_text(size=12,),
        legend.text = element_text(size=12),
  ) +
  facet_wrap(~State.Province, ncol = 2)
 
dev.off()
