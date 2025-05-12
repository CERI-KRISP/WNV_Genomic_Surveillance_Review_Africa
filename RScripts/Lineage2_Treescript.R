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
tree <- read.nexus(
  file = "timetree.nexus")


viz <- ggtree(tree,mrsd = "2023-10-11", ladderize = TRUE)+
  theme_tree2(axis.text.x = element_text(size = 10, vjust = 0.5))+
  theme(legend.position = 'none')+
  scale_x_continuous(breaks = seq(1680, 2023, by = 20))  # Define breaks every 100 years
                       
viz

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

p <-ggtree(tree,mrsd = "2023-10-11", ladderize = TRUE,size=0.35, colour = 'darkgrey')+
  theme_tree2(axis.text.x = element_text(size = 10, vjust = 0.5))
  
p

tips <- p %<+% metadata +
  #geom_tiplab(aes(label = access_country), size = 2, hjust = -0.2)+
  geom_tippoint(aes(subset=(region=='Western Europe')),size=3,fill = 'grey',  color='black',shape=21, stroke=0.1)+
  geom_tippoint(aes(subset=(region=='Central Europe')),size=3,fill = 'grey30',  color='black',shape=21, stroke=0.2)+
  geom_tippoint(aes(subset=(region=='Russia+')),size=3,fill = '#e9c46a',  color='black',shape=21, stroke=0.1)+
  geom_tippoint(aes(subset=(region=='Ukraine+')),size=3,fill = 'oldlace',  color='black',shape=21, stroke=0.2)+
  geom_tippoint(aes(subset=(country=='Zambia')),size=3,fill = 'aquamarine', color='black',shape=21, stroke=0.1)+
  geom_tippoint(aes(subset=(country=='Senegal')),size=3,fill = 'royalblue4', color='black',shape=21, stroke=0.1)+
  geom_tippoint(aes(subset=(country=='Democratic Republic of the Congo')),size=3,fill = 'pink', color='black',shape=21, stroke=0.1)+
  geom_tippoint(aes(subset=(country=='Namibia')),size=3,fill = 'aquamarine4', color='black',shape=21, stroke=0.1)+
  #geom_tippoint(aes(subset=(country=='Ethiopia')),size=3,fill = 'purple', color='black',shape=21, stroke=0.1)+
  geom_tippoint(aes(subset=(country=='Central African Republic')),size=3,fill = '#3185fc', color='black',shape=21, stroke=0.1)+
  #geom_tippoint(aes(subset=(country=='Egypt')),size=3,fill = 'pink', color='black',shape=21, stroke=0.1)+
 geom_tippoint(aes(subset=(country=='Uganda')),size=3,fill = 'purple', color='black',shape=21, stroke=0.1)+
  geom_tippoint(aes(subset=(country=='South Africa')),size=3,fill = 'firebrick1', color='black',shape=21, stroke=0.25)+
  geom_tippoint(aes(subset=(country=='Madagascar')),size=3,fill = 'purple4', color='black',shape=21, stroke=0.25)+
  theme(legend.position = "right") +
  scale_x_continuous(breaks = seq(1700, 2020, by = 40))
tips

nodes <- fortify(tree) %>% ggtree() + geom_text(aes(label=node), size =1.0, hjust=-.3)
nodes
ggsave("tree.pdf", width = 90, height = 150, units= "cm", limitsize = FALSE )

p2 <- tips %>% collapse(node=831) + 
  geom_point2(aes(subset=(node==831)), shape=23, size=5, fill='grey') #lineage 2 italy

p2

p3 <- p2 %>% collapse(node=772) + 
  geom_point2(aes(subset=(node==772)), shape=23, size=5, fill='grey') #lineage 2 germany

p3

p4 <- p3 %>% collapse(node=687) + 
  geom_point2(aes(subset=(node==687)), shape=23, size=5, fill='grey') #lineage 2 greece

p4

p5 <- p4 %>% collapse(node=545) + 
  geom_point2(aes(subset=(node==545)), shape=23, size=5, fill='#e9c46a') #lineage 2 russia

p5
