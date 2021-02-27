library(tidyverse)
library(readxl)
library(patchwork)
library(jsonlite)
library(ggmap)

# Install dplyr package 
install.packages("dplyr")    

# Load dplyr package     
library("dplyr") 

register_google(key = "AIzaSyDKbBOFE1HMdA03NgBnF0w5yxheedYp7Xc", write = TRUE)
httr::set_config(httr::config(http_version = 0))


umd_scooter_data <- read_csv("Scooters_Updated.csv")
names(umd_scooter_data)

write.csv(umd_scooter_data,"/Users/Aayush/Desktop/UMD Data Challenge 2021/Scooters_Updated.csv", row.names = FALSE)


umd_scooter_data$Year <- format(umd_scooter_data$START, format="%Y")
umd_scooter_data$Hours <- format(umd_scooter_data$START, format="%H")


umd_scooter_data <- umd_scooter_data %>%
  mutate(Season = case_when(between(Hours, 0, 5) ~ "Late Night",
                            between(Hours, 6, 11) ~ "Morning",
                            between(Hours, 12, 16) ~ "Noon",
                            between(Hours, 17, 19) ~ "Evening",
                            between(Hours, 20, 23) ~ "Night"),
         Season = as.factor(Season),
         Year = as.factor(Year),
         VEHICLE_TYPE = as.factor(VEHICLE_TYPE))

rides_across_years <- umd_scooter_data %>%
  group_by(Year) %>%
  summarise(Number_of_rides = n(),
            Average_distance = mean(DISTANCE, na.rm = TRUE),
            Average_Time = mean(MINUTES, na.rm = TRUE)) %>%
  ungroup()


(rides_across_years %>%
  ggplot(aes(x=Year, y=Number_of_rides)) +
  geom_col(fill="steelblue", width = 0.2) +
  theme_bw() +
  labs(x="Year", y="Number of rides", title = "Number of Rides for 2019 vs 2020") +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank())) /
(rides_across_years %>%
  ggplot(aes(x=Year, y=Average_distance)) +
  geom_col(fill="red", width = 0.2) +
  theme_bw() +
  labs(x="Year", y="Average Distance", title = "Average Distance for 2019 vs 2020") +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank()))



umd_scooter_2020 <- umd_scooter_data %>%
  filter(Year == "2020")

umd_scooter_2019 <- umd_scooter_data %>%
  filter(Year == "2019")


umd_scooter_2019_seasons <- umd_scooter_2019 %>%
  group_by(Season) %>%
  summarise(Number_of_rides = n(),
            Avg_distance = mean(DISTANCE, na.rm = TRUE))

umd_scooter_2020_seasons <- umd_scooter_2020 %>%
  group_by(Season) %>%
  summarise(Number_of_rides = n(),
            Avg_distance = mean(DISTANCE, na.rm = TRUE))


(umd_scooter_2019_seasons %>%
  ggplot(aes(x=Season, y=Number_of_rides)) +
  geom_col(fill = "red", alpha = 0.8, width = 0.5) +
  labs(title = "Number of rides during different day times for 2019", x="DayTime", y="Number of Rides") +
  theme_bw() +
  theme(panel.grid.major.x = element_blank())) /
(umd_scooter_2020_seasons %>%
  ggplot(aes(x=Season, y=Number_of_rides)) +
  geom_col(fill = "grey50", width = 0.5, alpha=0.8) +
  labs(title = "Number of rides during different day times for 2020", x="DayTime", y="Number of Rides") +
  theme_bw() +
  theme(panel.grid.major.x = element_blank()))

(umd_scooter_2019_seasons %>%
    ggplot(aes(x=Season, y=Avg_distance)) +
    geom_col(fill = "red", alpha = 0.8, width = 0.5) +
    labs(title = "Average Distance during different day times for 2019", x="DayTime", y="Average Distance") +
    theme_bw() +
    theme(panel.grid.major.x = element_blank())) /
  (umd_scooter_2020_seasons %>%
     ggplot(aes(x=Season, y=Avg_distance)) +
     geom_col(fill = "grey50", width = 0.5, alpha=0.8) +
     labs(title = "Average Distance during different day times for 2020", x="DayTime", y="Average Distance") +
     theme_bw() +
     theme(panel.grid.major.x = element_blank()))

umd_scooter_2019_ride_type <- umd_scooter_2019 %>%
  group_by(VEHICLE_TYPE) %>%
  summarise(Number_of_rides = n(),
            Avg_distance = mean(DISTANCE, na.rm = TRUE),
            Avg_time = mean(MINUTES, na.rm = TRUE))

umd_scooter_2020_ride_type <- umd_scooter_2020 %>%
  group_by(VEHICLE_TYPE) %>%
  summarise(Number_of_rides = n(),
            Avg_distance = mean(DISTANCE, na.rm = TRUE),
            Avg_time = mean(MINUTES, na.rm = TRUE))

(umd_scooter_2019_ride_type %>%
    ggplot(aes(x=VEHICLE_TYPE, y=Number_of_rides)) +
    geom_col(fill = "red", alpha = 0.8, width = 0.5) +
    labs(title = "Number of rides by Vehichle Type for 2019", x="Vehichle Type", y="Rides") +
    theme_bw() +
    scale_y_continuous(breaks = seq(0,20000,by=5000), labels = seq(0,20000,by=5000)) +
    ylim(0,20000) +
    theme(panel.grid.major.x = element_blank())) /
  (umd_scooter_2020_ride_type %>%
     ggplot(aes(x=VEHICLE_TYPE, y=Number_of_rides)) +
     geom_col(fill = "grey50", width = 0.5, alpha=0.8) +
     labs(title = "Number of rides by Vehichle Type for 2020", x="Vehichle Type", y="Rides") +
     theme_bw() +
     theme(panel.grid.major.x = element_blank()))

(umd_scooter_2019_ride_type %>%
    ggplot(aes(x=VEHICLE_TYPE, y=Avg_distance)) +
    geom_col(fill = "red", alpha = 0.8, width = 0.5) +
    labs(title = "Average Distance for Vehichle Type in 2019", x="Vehichle Type", y="Average Distance") +
    theme_bw() +
    theme(panel.grid.major.x = element_blank())) /
  (umd_scooter_2020_ride_type %>%
     ggplot(aes(x=VEHICLE_TYPE, y=Avg_distance)) +
     geom_col(fill = "grey50", width = 0.5, alpha=0.8) +
     labs(title = "Average Distance for Vehichle Type in 2020", x="Vehichle Type", y="Average Distance") +
     theme_bw() +
     theme(panel.grid.major.x = element_blank()))

(umd_scooter_2019_ride_type %>%
    ggplot(aes(x=VEHICLE_TYPE, y=Avg_time)) +
    geom_col(fill = "red", alpha = 0.8, width = 0.5) +
    labs(title = "Average Time for Vehichle Type in 2019", x="Vehichle Type", y="Average Time") +
    theme_bw() +
    theme(panel.grid.major.x = element_blank())) /
  (umd_scooter_2020_ride_type %>%
     ggplot(aes(x=VEHICLE_TYPE, y=Avg_time)) +
     geom_col(fill = "grey50", width = 0.5, alpha=0.8) +
     labs(title = "Average Time for Vehichle Type in 2020", x="Vehichle Type", y="Average Time") +
     theme_bw() +
     theme(panel.grid.major.x = element_blank()))

umd_data_scooter_stops <- umd_scooter_2020 %>%
  filter(((START.LONG > -76.915 | START.LONG < -76.977) | (START.LAT > 38.989 | START.LAT < 38.978)) & 
         ((END.LONG < -76.915 & END.LONG > -76.977 ) & (END.LAT > 38.978 & END.LAT < 38.989 )))

average_distance = mean(umd_data_scooter_stops$DISTANCE, na.rm = TRUE)

umd_scooter_2019$Start_Location <- mapply(FUN = function(lon, lat) ifelse((lon < -76.915 & lon > -76.977) & (lat > 38.978 & lat < 38.989),
                                                                          "OnCampus", "OffCampus"), umd_scooter_2019$START.LONG, umd_scooter_2019$START.LAT)

umd_scooter_2020$Start_Location <- mapply(FUN = function(lon, lat) ifelse((lon < -76.915 & lon > -76.977) & (lat > 38.978 & lat < 38.989),
                                                                          "OnCampus", "OffCampus"), umd_scooter_2020$START.LONG, umd_scooter_2020$START.LAT)

top_10_stops_2019 <- umd_scooter_2019 %>%
  mutate(Start_Location = as.factor(Start_Location)) %>%
  group_by(Address) %>%
  summarise(Number_of_rides = n(),
            Average_Distance = mean(DISTANCE, na.rm = TRUE),
            Average_Time = mean(MINUTES, na.rm = TRUE),
            Start_Location = Start_Location)

top_10_stops_2020 <- umd_scooter_2020 %>%
  mutate(Start_Location = as.factor(Start_Location)) %>%
  group_by(Address) %>%
  summarise(Number_of_rides = n(),
            Average_Distance = mean(DISTANCE, na.rm = TRUE),
            Average_Time = mean(MINUTES, na.rm = TRUE),
            Start_Location = Start_Location)

top_10_stops_2019 <- unique(top_10_stops_2019)
top_10_stops_2020 <- unique(top_10_stops_2020)

top_10_stops_2019 <- top_10_stops_2019 %>%
  mutate(Start_Location = as.factor(Start_Location),
         Address = fct_reorder(Address, Number_of_rides, .desc = TRUE))

top_10_stops_2020 <- top_10_stops_2020 %>%
  mutate(Start_Location = as.factor(Start_Location),
         Address = fct_reorder(Address, Number_of_rides, .desc = TRUE))

top_10_2019 <- top_10_stops_2019 %>%
  arrange(desc(Number_of_rides))
top_10_2019 <- top_10_2019[1:10,]

top_10_2020 <- top_10_stops_2020 %>%
  arrange(desc(Number_of_rides))
top_10_2020 <- top_10_2020[1:10,]

top_10_2019 <- top_10_2019 %>%
  separate(Address, c("Address1", "Address2"), sep = ",", extra = "merge") %>%
  mutate(Address1 = fct_reorder(Address1, Number_of_rides, .desc = TRUE))

top_10_2020 <- top_10_2020 %>%
  separate(Address, c("Address1", "Address2"), sep = ",", extra = "merge") %>%
  mutate(Address1 = fct_reorder(Address1, Number_of_rides, .desc = TRUE))


(top_10_2019 %>%
  ggplot(aes(x=Address1, y=Number_of_rides, fill=Start_Location)) +
  geom_col() +
  scale_x_discrete(guide=guide_axis(n.dodge=2)) +
  labs(x="Address", y="Number of Rides", title = "Top 10 locations with highest rides in 2019",
       caption = "Data Reference: Scooter Data") +
  scale_fill_manual(name="",breaks=c("OffCampus","OnCampus"),
                    values=c("darkred", "grey70")) +
  geom_hline(yintercept = 0) +
  theme(panel.background = element_blank(),
        legend.position = "bottom",
        axis.ticks = element_blank(),
        panel.grid.major.y = element_line(color="grey80", linetype = 3)) +
  theme_classic()) /
(top_10_2020 %>%
  ggplot(aes(x=Address1, y=Number_of_rides, fill=Start_Location)) +
  geom_col() +
  scale_x_discrete(guide=guide_axis(n.dodge=2)) +
  labs(x="Address", y="Number of Rides", title = "Top 10 locations with highest rides in 2020",
     caption = "Data Reference: Scooter Data") +
  scale_fill_manual(name="",breaks=c("OffCampus","OnCampus"),
                    values=c("darkred", "grey70")) +
  geom_hline(yintercept = 0) +
  theme(panel.background = element_blank(),
        legend.position = "bottom",
        axis.ticks = element_blank(),
        panel.grid.major.y = element_line(color="grey80", linetype = 3)) +
   scale_y_continuous(breaks = seq(0,1500,by=500)) +
   ylim(0,1500) +
  theme_classic())


address_rides <- umd_scooter_data %>%
  group_by(Address, EndStopAddress) %>%
  summarise(Number_of_Rides = n())

write.csv(address_rides,"/Users/Aayush/Desktop/UMD Data Challenge 2021/Scooters_Updated_Address.csv", row.names = FALSE)
  

umd_scooter_data$Address <- mapply(FUN = function(lon, lat) revgeocode(c(lon, lat)), umd_scooter_data$`START LONG`, umd_scooter_data$`START LAT`)
umd_scooter_data$EndStopAddress <- mapply(FUN = function(lon, lat) revgeocode(c(lon, lat)), umd_scooter_data$END.LONG, umd_scooter_data$END.LAT)




