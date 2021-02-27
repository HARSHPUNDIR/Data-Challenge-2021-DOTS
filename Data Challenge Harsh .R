library(readr)
library(tidyverse)
library(readxl)
library(jsonlite)
library(ggmap)
library(stringr)

veo_data <- read_csv("Data_Lv3_UMDOTS_Escooters.csv")

veo_data<- veo_data %>% select(-c(X1, `Unnamed: 0`, CREATED))

#avearge trip distance
q1 <- mean(veo_data$DISTANCE, na.rm = TRUE)

#most frequent start point
q2 <- veo_data %>% 
  group_by(`START LONG`, `START LAT`) %>%  
  summarise(Count = n())

q2 <- q2[which.max(q2$Count), 1:2]

q2[1,1] # most frequent Start Longitude
q2[1,2] # most frequent Start Latitude

start_point <- c(q2[1,1],q2[1,2])

#register_google()


# most frequent start address
start_address <- revgeocode(as.numeric(start_point), output = "address")
                            
#register_google(key = "AIzaSyAxLa-x349WEsS4vxFCO2ljhNSKeDJ5IDY", write = TRUE)

#most frequent end point
q3 <- veo_data %>% 
  group_by(`END LONG`, `END LAT`) %>%  
  summarise(Count = n())

q3 <- q3[which.max(q3$Count), 1:2]

q3[1,1] # most frequent End Longitude
q3[1,2] # most frequent End Latitude

end_point <- c(q3[1,1],q3[1,2])


# most frequent end address
end_address <- revgeocode(as.numeric(end_point), output = "address")

q4 <- veo_data %>% 
  group_by(PATH) %>% 
  summarise(Cnt = n())


Numextract <- function(string){
  unlist(regmatches(string,gregexpr("[[:digit:]]+\\.*[[:digit:]]*",string)))
}

#This one not working separated using Python

#x <- str_extract_all(strLine,"\\(?[0-9,.]+\\)?")[[1]]

#df <- veo_data %>%  mutate(new_values = str_extract_all(PATH[1],"\\(?[0-9,.]+\\)?")[[1]])

#q4 <- q4[which.max(q4$Cnt),1]

#q4 %>% separate(PATH, into =  c("Long", "Lat"), sep = ",") %>% mutate("Long" = )
