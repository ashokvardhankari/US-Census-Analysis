
source("theme_clean.r")
library(ggplot2)
library(maps)


## Run

stateMap <- map_data("state")
colnames(stateMap)
head(stateMap)
is.data.frame(stateMap) # TRUE

ggplot(stateMap,aes(x=long,y=lat,group=group))+
      geom_polygon(fill= "#FFA500",
                   color="black")+coord_map("albers",29.5,45.5)

## End


## Run
census <- read.csv(file="US_Census_Statistics.csv",
                   header=T, as.is=TRUE)
census <- census[-9,]

censusCH <- data.frame(state=tolower(rownames(USArrests)), census)
head(censusCH)

census_map <- merge(stateMap, censusCH, by.x="region",by.y="state")
head(census_map)

## End



## Run

library(plyr)
census_map <- arrange(census_map, group, order) 
head(census_map)

## End



## Run

popQuints <- quantile(censusCH$P_2000, seq(0,1,by=.2))
censusCH$PopulationClass = cut(censusCH$P_2000, popQuints,include.lowest=TRUE)
popColors <- colorRampPalette(c("#CD853F","#FF4500",'#FFD700',"#FF8C00", "#8B4513"))(5)

## End

# First map for population in 2000
## Run

ggplot(censusCH,aes(map_id = state,fill=PopulationClass), dpi = 300)+
geom_map(map=census_map, color=gray(0),size=.3)+ 
scale_fill_manual(values=popColors)+
expand_limits(x=stateMap$long,y=stateMap$lat)+
coord_map("albers",29.5,45.5)+
labs(fill="P_2000")+theme_clean()+ggtitle("Population in 2000")+
      theme(plot.title = element_text(size=20, face="bold", vjust=2))

## End

# Second map for population in 2010

popQuints2 <- quantile(censusCH$P_2010, seq(0,1,by=.2))
censusCH$PopulationClass2 = cut(censusCH$P_2010, popQuints2,include.lowest=TRUE)
popColors <- colorRampPalette(c("#CD853F","#FF4500",'#FFD700',"#FF8C00", "#8B4513"))(5)


ggplot(censusCH,aes(map_id = state,fill=PopulationClass2))+
      geom_map(map=census_map, color=gray(0),size=.3)+ 
      scale_fill_manual(values=popColors)+
      expand_limits(x=stateMap$long,y=stateMap$lat)+
      coord_map("albers",29.5,45.5)+
      labs(fill="P_2010")+theme_clean()+ggtitle("Population in 2010")+
      theme(plot.title = element_text(size=20, face="bold", vjust=2))
