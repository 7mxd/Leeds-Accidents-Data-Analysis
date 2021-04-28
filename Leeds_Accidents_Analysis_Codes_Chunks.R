# libraries and packages

packages <- c("tidyverse", "leaflet","sf")
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

library(tidyverse)
library(leaflet)
library(sf)




# Loading the data into R 

Accidents_2017 <- read_csv("Data/datagov-uk-6efe5505-941f-45bf-b576-4c1e09b579a1/2017-8.csv", 
                           col_names = c("Reference_Number","Easting","Northing","Vehicles_Num","Accident_Date","Time","Road_Class","Road_Surface",
                                         "Lightning_Cond","Weather_Cond","Vehicle_Type","Casualty_Class","Severity","Gender","Age"), skip = 1)

# Parsing the data
Accidents_Date <- Accidents_2017$Accident_Date
Accidents_Date <- parse_date(Accidents_Date, "%m/%d/%Y")

Time <- Accidents_2017$Time
Time <- parse_time(Time, "%H%M")




# Adding the new columns to the tibble.

Accidents_2017 <- Accidents_2017 %>%
  select(1:4, 7:15) %>%
  mutate(Accidents_Date, Time) 

Accidents_2017 <- Accidents_2017[,c(1:4,14,15,5:13)]

str(Accidents_2017)




# Variance Part

# 1. Variance (Age)

ggplot(data = Accidents_2017, mapping= aes(x=Age)) +
  geom_histogram(binwidth = 1,fill="black", color = "white") +
  ggtitle("Number of Casualties Based on Age") + 
  ylab("Number of Casualties")

Accidents_2017 %>%
  group_by(Age) %>%
  summarise(count = n())

# 2. Variance (Gender)

ggplot(data = Accidents_2017, mapping = aes(x=Gender)) + 
  geom_bar(aes(fill = Gender) , width = 0.5) +
  ggtitle("Number of Casualties Based on Gender") + 
  ylab("Number of Casualties")

Accidents_2017 %>%
  group_by(Gender) %>%
  summarise(count = n())

# 3. Variance (Number of Accidents Per Month)

# Accidents per day

Accidents_2017 %>%
  group_by(Accidents_Date) %>%
  summarise(sum = n())

# Accidents per month

( Accidents_by_month <- 
    Accidents_2017 %>%
    mutate(month = format(Accidents_Date,"%m")) %>%
    group_by(month) %>%
    summarise(accidents_num = n())
)

# Plot of Accidents per month
ggplot(data = Accidents_by_month, mapping = aes(x = month, y = accidents_num)) +
  geom_bar(aes(fill = month) , show.legend = F, stat = "identity") +
  ggtitle("Number of Accidents Per Month") +
  xlab("Month") +
  ylab("Number of Accidents")


# 4. Variance (Road Surface)

ggplot(data = Accidents_2017) +
  geom_bar(mapping = aes(x = Road_Surface,fill = Road_Surface))

Accidents_2017 %>%
  group_by(Road_Surface) %>%
  summarise(count = n())

# 5. Variance (Time)

ggplot(Accidents_2017,mapping=aes(x=Time))+
  geom_histogram(binwidth = 2000)+
  ggtitle("Accidents against Time")+
  ylab("Number of Accidents")+
  xlab("Time")




# Covariance Part

# 1. Covariance (Age against Count, Classified by Gender)

Accidents_2017 %>%
  group_by(Gender, Age) %>%
  summarise(Count = n() ) %>%
  ungroup() %>%
  ggplot(mapping = aes(x=Age , y = Count)) +
  geom_point(alpha = 0.6) + 
  facet_wrap(~Gender) +
  ggtitle("Number of Casualties Per Age For Males and Females") +
  ylab("Number of Casualties")

Accidents_2017 %>%
  group_by(Gender, Age) %>%
  summarise(Count = n() )

# 2. Covariance (Time and Severity)

ggplot(data = Accidents_2017)+ 
  geom_boxplot(mapping = aes(x = reorder(Severity, Time, FUN = median), y = Time))+ coord_flip()+
  ggtitle("Severity against Time")+
  ylab("Severity")+
  xlab("Time")

# 3. Covariance (Age and Severity)

ggplot(data = Accidents_2017)+ 
  geom_boxplot(mapping = aes(x = reorder(Severity, Age, FUN = median), y = Age))+ coord_flip()+
  ggtitle("Severity against Age")+
  ylab("Age")+
  xlab("Severity")

# 4. Covariance (Age against Count, Classified by Casualty Class)

Accidents_2017 %>%
  group_by(Casualty_Class, Age) %>%
  summarise(Count = n() ) %>%
  ungroup() %>%
  ggplot(mapping = aes(x=Age , y = Count)) +
  geom_point(alpha = 0.6) + 
  geom_smooth()+
  facet_wrap(~Casualty_Class) +
  ggtitle("Count against Age based on Casualty class") +
  ylab("Number of Casualties")

Accidents_2017 %>%
  group_by(Casualty_Class,Age) %>%
  summarise(Count = n() )


Accidents_2017 %>%
  group_by(Casualty_Class) %>%
  summarise(Count = n() )




# Extra Plot (MAP): 

# Locations of the Accidents

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
  addLegend(colors = c("yellow", "orange", "red") , 
            labels = c("Slight", "Serious", "Fatal"), title = "Severity")



