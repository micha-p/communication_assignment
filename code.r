
require(readr)
trainSF <- read_csv("sanfrancisco_incidents_summer_2014.csv")
trainSEA <- read_csv("seattle_incidents_summer_2014.csv")

require(lubridate)
trainSF$hour <- hour(hm(trainSF$Time))
trainSEA$hourS <- hour(mdy_hms(trainSEA$"Occurred Date or Date Range Start"))
trainSEA$hourE <- hour(mdy_hms(trainSEA$"Occurred Date Range End"))
trainSEA$hour <- floor((trainSEA$hourS + pmax(trainSEA$hourE,trainSEA$hourS,na.rm=TRUE)) / 2)

require(dplyr)
byhourSF <- trainSF %>% group_by(hour) %>% summarise(total = n())
byhourSEA <- trainSEA %>% group_by(hour) %>% summarise(total = n())
byhourSF$city="San Francisco"
byhourSEA$city="Seattle"

require(ggplot2)
p <- ggplot(byhourSF, aes(x = hour, y = total, color=city))
p <- p + geom_line(data=byhourSF)
p <- p + geom_line(data=byhourSEA) 
p <- p + xlim(0, 23) + scale_colour_manual(values = c("red","blue"))
p <- p + xlab("Hour of day") + ylab("Total count of incidents") + labs(colour = "City")
# ggsave("hour.png", p, width=14, height=10, units="in") 
p

require(ggmap)
#mapSF<-get_map(location="sanfrancisco",zoom=12,source="osm",maptype = "roadmap", color = "bw")
#mapSEA<-get_map(location="seattle",zoom=12,source="osm",maptype = "roadmap", color = "bw")
#saveRDS(mapSF,"openstreetmapSF.rds")
#saveRDS(mapSEA,"openstreetmapSEA.rds")
mapSF <- readRDS("openstreetmapSF.rds")
mapSEA <- readRDS("openstreetmapSEA.rds")

trainSEA$Category <- trainSEA$"Offense Type"
countsSF <- trainSF %>% group_by(Category) %>% summarise(Counts=length(Category))
countsSF <- countsSF[order(-countsSF$Counts),]
countsSEA <- trainSEA %>% group_by(Category) %>% summarise(Counts=length(Category))
countsSEA <- countsSEA[order(-countsSEA$Counts),]
# This removes the "Other Offenses" category
topSF <- trainSF[trainSF$Category %in% countsSF$Category[c(1,3:21)],]
# This removes the "PROPERTY FOUND" category
topSEA <- trainSEA[trainSEA$Category %in% countsSEA$Category[c(1:8,10:21)],]



pSF <- ggmap(mapSF) +
     geom_point(data=topSF, aes(x=X, y=Y, color=factor(Category)), alpha=0.05) +
     guides(colour = guide_legend(override.aes = list(alpha=1.0)))
# ggsave("mapSF.png", p, width=14, height=10, units="in")
pSEA <- ggmap(mapSEA) +
     geom_point(data=topSEA, aes(x=Longitude, y=Latitude, color=factor(Category)), alpha=0.05) +
     guides(colour = guide_legend(override.aes = list(alpha=1.0)))
# ggsave("mapSEA.png", p, width=14, height=10, units="in")
pSF
pSEA
