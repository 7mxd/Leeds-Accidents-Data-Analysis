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



