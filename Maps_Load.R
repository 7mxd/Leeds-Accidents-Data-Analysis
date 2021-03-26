# Loading the data 

library(tidyverse)

Accidents_2017 <- read_csv("Data/datagov-uk-6efe5505-941f-45bf-b576-4c1e09b579a1/2017-8.csv", 
                           col_names = c("Reference_Number","Easting","Northing","Vehicles_Num","Accident_Date","Time","Road_Class","Road_Surface",
                                         "Lightning_Cond","Weather_Cond","Vehicle_Type","Casualty_Class","Severity","Gender","Age"), skip = 1)

Accidents_Date <- Accidents_2017$Accident_Date
Accidents_Date <- parse_date(Accidents_Date, "%m/%d/%Y")

Time <- Accidents_2017$Time
Time <- parse_time(Time, "%H%M")

Accidents_2017 <- Accidents_2017 %>%
  select(1:4, 7:15) %>%
  mutate(Accidents_Date, Time) 

Accidents_2017 <- Accidents_2017[,c(1:4,14,15,5:13)]

# Loading the Map

library(sf)
library(leaflet)


long_lat <- Accidents_2017 %>%
  st_as_sf(coords = c("Easting", "Northing"), crs = 27700) %>%
  st_transform(4326) %>%
  st_coordinates() %>%
  as_tibble()

long_lat_FULL <- Accidents_2017 %>%
  mutate(long_lat)  %>%
  select(Reference_Number, X, Y, Severity)

pal <- colorFactor(c("yellow", "orange", "red"), domain = c("Slight", "Serious", "Fatal"))

leaflet(data = long_lat_FULL)%>%
  addTiles() %>%
  addCircleMarkers(lng = ~X, lat = ~Y, 
                   color = ~pal(Severity),
                   stroke = F, fillOpacity = 0.75,
                   clusterOptions = markerClusterOptions())  %>%
  addLegend(colors = c("yellow", "orange", "red") , labels = c("Slight", "Serious", "Fatal"), title = "Severity")




             