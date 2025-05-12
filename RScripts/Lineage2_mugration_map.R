
library(ggplot2)
library("readxl")
library(tidyverse)
library(dplyr)
library(ggpubr)
library(cowplot)
library(ggthemes)
library(viridis)
library(ggrepel)
library('gridExtra')
library('data.table')
library('scales')
library('lubridate')
library("patchwork")
library("zoo")
library(sp)
library(rworldmap)

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


country2lat = function(x)
{  
  country_data = subset(world_data, NAME==x)
  
  return (as.numeric(country_data[['LAT']]))  
}


country2long = function(x)
{  
  country_data = subset(world_data, NAME==x)
  
  return (as.numeric(country_data[['LON']]))  
}




export2type = function(x,y)
{  
  if (x==y) {
    
    return (as.character("Regional"))
  }
  
  else {
    
    return (as.character("Global"))
  }
}

stat_box_data <- function(y, upper_limit = max(iris$Sepal.Length) * 1.15) {
  return( 
    data.frame(
      y = 350,
      label = paste('n=',length(y),' ',
                    'mean delay=',round(mean(y)),'days'
      )
    )
  )
}


#read in data
data<-read.csv('annotated_tree_events.csv', header = TRUE)

#data[data == "USA"] <- "United States"
data$date<-date_decimal(data$EventTime)
data$date<-as.Date(cut(data$date,breaks = "week",start.on.monday = FALSE))


##read in global map
#worldmap <- getMap()
#world_map_data<-worldmap@data
#world_map_data_short<-world_map_data  %>% dplyr::select("NAME","REGION")
world1 <- map_data("world")

data$destination_lat<- lapply(data$Destination,country2lat)
data$destination_long<- lapply(data$Destination,country2long)

data$origin_lat<- lapply(data$Origin,country2lat)
data$origin_long<- lapply(data$Origin,country2long)

# Replace the values for CAR
data[4, 7] <- 6.59212
data[4, 8] <- 20.42526
#DRC
data[3, 7] <- -2.86690
data[3, 8] <- 23.73187
#Czech
data[43, 7] <- 49.69127
data[43, 8] <- 15.17274
data[46, 7] <- 49.69127
data[46, 8] <- 15.17274
data[57, 7] <- 49.69127
data[57, 8] <- 15.17274
data[60, 7] <- 49.69127
data[60, 8] <- 15.17274



#world3<- world1 %>% 
 # left_join(data, by = c("region" = "Destination"))

lab_dates <- pretty(data$date)

lin2_map<-ggplot() +
  theme_void()+
  geom_map(data=world1,map=world1, aes(long, lat,map_id=region), color="gray40", fill='grey94',linewidth=0.2)+
  
  geom_curve(data = data,
             aes(x = as.double(origin_long), 
                 y = as.double(origin_lat), 
                 xend = as.double(destination_long), 
                 yend = as.double(destination_lat), colour=date),size=1,
             alpha=1)+
  
  geom_point(data = data,
             aes(x = as.double(origin_long), y = as.double(origin_lat), size=n),
             fill='grey20', linewidth=1, shape=21)+
  
  scale_color_gradientn(colours = c('red4', '#DB0201',  'darkorange','#A0CBAD','#8FB1BE', 'royalblue4' ), breaks = lab_dates, 
                        labels = lab_dates, name='Inferred date of dispersal')+
  theme(legend.position = 'top')+
  theme(legend.direction = 'horizontal')+
  coord_fixed()+
  guides(colour = guide_colourbar(barwidth = 20, barheight = 0.5,title.position = 'top',title.hjust = 1,ticks.colour = "white",
                                  ticks.linewidth = 2),
         size='none')+
  theme(plot.title = element_text(family="Helvetica"))+
  scale_y_continuous(limits=c(-40,80))+
  scale_x_continuous(limits=c(-20,150))

lin2_map




