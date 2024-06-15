#load necessary libraries
library(dplyr)
library(ggplot2)
library(lubridate)
library(leaflet)
library(sp)
library(readr)

#loading the dataset
Road_accident <- read_csv("~/Road_accident.csv")
print(Road_accident)

#checking for missing values 
Road_accident<-na.omit(Road_accident)

#converting time column to appropriate data type
Road_accident$Time<-hms::as_hms(Road_accident$Time)

#summary statistics for the dataset
summary(Road_accident)

#extracting hours from the time column
Road_accident$Hour<-hour(Road_accident$Time)

#ploting the number of accidents by hour
ggplot(Road_accident,aes(x=Hour))+geom_histogram(binwidth = 1,fill="skyblue",color="red")+labs(title = "Number of accidents by hour of the day",x="Hour of the Day",y="Number of Accidents")

#accidents by weather condition
ggplot(Road_accident,aes(x=Weather_conditions))+geom_bar(fill="lightgreen",color="black")+labs(title = "Number of Accidents by weather conditions",x="weather condition",y="number of accidents")

#accidents by road condition
ggplot(Road_accident,aes(x=Road_surface_type))+geom_bar(fill="lightcoral",color="black")+labs(title = "Number of Accidents by Road Condition",x="Road Condition",y="Number of Accidents")

# Example columns: Latitude and Longitude
if ("Latitude" %in% colnames(Road_accident) & "Longitude" %in% colnames(Road_accident)) {
  leaflet(data = Road_accident) %>%
    addTiles() %>%
    addCircleMarkers(~Longitude, ~Latitude, radius = 3, color = ~Severity,
                     popup = ~paste("Date:", Date, "<br>", "Time:", Time, "<br>", "Severity:", Severity),
                     clusterOptions = markerClusterOptions()) %>%
    addLegend("bottomright", pal = colorFactor(c("blue", "orange", "red"), domain = Road_accident$Severity),
              values = ~Severity, title = "Severity")
} else {
  print("Latitude and Longitude columns are not available in the dataset.")
}

# Analyze the impact of weather, road condition, and time of day on accident severity
ggplot(Road_accident, aes(x=Weather_conditions, fill=Casualty_severity)) +
  geom_bar(position="fill") +
  labs(title="Accident Severity by Weather Condition", x="Weather Condition", y="Proportion")

ggplot(Road_accident, aes(x=Road_surface_type, fill=Casualty_severity)) +
  geom_bar(position="fill") +
  labs(title="Accident Severity by Road Condition", x="Road Condition", y="Proportion")

ggplot(Road_accident, aes(x=Hour, fill=Casualty_severity)) +
  geom_histogram(binwidth=1, position="fill") +
  labs(title="Accident Severity by Hour of the Day", x="Hour of the Day", y="Proportion")

