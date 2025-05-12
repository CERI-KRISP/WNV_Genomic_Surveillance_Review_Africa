library(ggtree)
library(ape)
library(ggplot2)
library(colorspace)
library(phytools)
library(treeio)
library(dplyr)
library(readr)
library(tidyr)
library(reshape2)
library(tidytree)
library(writexl)
library(rworldmap)

##read in time tree
tree1 <- read.nexus(
  file = "timetree1.nexus")


viz1 <- ggtree(tree1,mrsd = "2023-09-08", ladderize = TRUE)+
  theme_tree2(axis.text.x = element_text(size = 10, vjust = 0.5))+
  theme(legend.position = 'none')+
  scale_x_continuous(breaks = seq(1860, 2040, by = 20))  # Define breaks every 100 years
                       
viz1

tips_to_remove <- c("AY490240")
pruned <- drop.tip(tree1, tips_to_remove, trim.internal = TRUE)

unique(metadata$country)

###country to continent/region in metadata
world_data<-getMap(resolution='low')@data

country2continent = function(x)
{  
  country_data = subset(world_data, NAME==x)
  
  return (as.character(country_data[['REGION']]))   # returns the continent (7 continent model)
}


country2continent_region = function(x)
{  
  country_data = subset(world_data, NAME==x)
  
  return (as.character(country_data[['IMAGE24']]))  
}


metadata$continent<-lapply(metadata$country,country2continent)
metadata$region<-lapply(metadata$country,country2continent_region)

unique(metadata$continent)
unique(metadata$region)

p <-ggtree(tree1,mrsd = "2023-09-08") %<+% metadata + geom_tiplab(size=2, aes(label = access_country))+
theme_tree2(axis.text.x = element_text(size = 10, vjust = 0.5))
  
p

p2 <-ggtree(pruned,mrsd = "2023-09-08", ladderize = TRUE,size=0.35, colour = 'darkgrey')+
  theme_tree2(axis.text.x = element_text(size = 10, vjust = 0.5))

p2


tips <- p2 %<+% metadata +
  geom_tippoint(aes(subset=(region=='Western Europe')),size=3,fill = 'grey',  color='black',shape=21, stroke=0.1)+
  geom_tippoint(aes(subset=(region=='Central Europe')),size=3,fill = 'grey',  color='black',shape=21, stroke=0.2)+
  geom_tippoint(aes(subset=(continent=='Asia')),size=3,fill = 'oldlace',  color='black',shape=21, stroke=0.2)+
  geom_tippoint(aes(subset=(region=='Russia+')),size=3,fill = 'grey30',  color='black',shape=21, stroke=0.1)+
  geom_tippoint(aes(subset=(region=='Middle East')),size=3,fill = 'red4',  color='black',shape=21, stroke=0.1)+
  geom_tippoint(aes(subset=(continent=='North America')),size=3,fill = 'orange',  color='black',shape=21, stroke=0.1)+
  geom_tippoint(aes(subset=(continent=='South America')),size=3,fill = 'yellow',  color='black',shape=21, stroke=0.1)+
  geom_tippoint(aes(subset=(country=='Turkey')),size=3,fill = 'darkorange3',  color='black',shape=21, stroke=0.1)+
  geom_tippoint(aes(subset=(country=='Egypt')),size=3,fill = 'darkolivegreen2',  color='black',shape=21, stroke=0.1)+
  geom_tippoint(aes(subset=(country=='Zambia')),size=3,fill = 'aquamarine', color='black',shape=21, stroke=0.1)+
  geom_tippoint(aes(subset=(country=='Senegal')),size=3,fill = 'royalblue3', color='black',shape=21, stroke=0.1)+
  geom_tippoint(aes(subset=(country=='Nigeria')),size=3,fill = 'forestgreen', color='black',shape=21, stroke=0.1)+
  geom_tippoint(aes(subset=(country=='Tunisia')),size=3,fill = 'pink', color='black',shape=21, stroke=0.1)+
  geom_tippoint(aes(subset=(country=='Central African Republic')),size=3,fill = 'lightskyblue', color='black',shape=21, stroke=0.1)+
  geom_tippoint(aes(subset=(country=='Ethiopia')),size=3,fill = 'magenta1', color='black',shape=21, stroke=0.1)+
  geom_tippoint(aes(subset=(country=='Kenya')),size=3,fill = 'firebrick1', color='black',shape=21, stroke=0.25)+
  geom_tippoint(aes(subset=(country=='Morocco')),size=3,fill = 'purple3', color='black',shape=21, stroke=0.25)+
  theme(legend.position = "right")+
  scale_x_continuous(breaks = seq(1900, 2020, by = 10))
tips

nodes <- fortify(tree1) %>% ggtree() + geom_text(aes(label=node), size =1.0, hjust=-.3)
nodes
ggsave("tree.pdf", width = 90, height = 150, units= "cm", limitsize = FALSE )

p3 <- tips %>% collapse(node=432) + 
  geom_point2(aes(subset=(node==432)), shape=23, size=5, fill='grey') #lineage 2 italy

p3
