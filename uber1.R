#install necessary packages 
install.packages("ggplot2")
install.packages("ggthemes")
install.packages("lubridate")
install.packages("dplyr")
install.packages("tidyr")
install.packages("DT")
install.packages("scales")

library(ggplot2)
library(ggthemes)
library(lubridate)
library(dplyr)
library(tidyr)
library(DT)
library(scales)


data1<-read.csv("C:\\Users\\DHINAKARAN V\\Desktop\\mini project\\bangalore_hotel.csv")
data2<-read.csv("C:\\Users\\DHINAKARAN V\\Desktop\\mini project\\bangalore__uber_data.csv")
mergedata<-cbind(data1,data2)
head(mergedata)
# Exploring the Data/data pre procesing
str(mergedata) 
summary(mergedata) 
sum(is.na(mergedata))
colSums(is.na(mergedata))
mergedata$Miles[is.na(mergedata$Miles)] <- mean(mergedata$Miles, na.rm = TRUE)
sum(is.na(mergedata))
data_cleaned <- mergedata[!duplicated(mergedata), ]
mdata<- mergedata[ , -16]
mdata$Distance.to.Transport..km.<-NULL
head(mdata)

####Language Known by employees###1
ggplot(mdata, aes(Known.Language)) + 
  geom_bar(fill = "darkred", color = "darkblue") +
  scale_y_continuous(labels = comma) +
  ggtitle("Language known by uber employees")

###AVGERAGE OCCUPANCY RATE VS RATING###2
ggplot(mdata, aes(x = Rating, y =Average.Occupancy.Rate....)) +
  geom_point(color = "blue") +
  theme_minimal() +
  labs(title = "Customized: Hotel Rating vs Occupancy Rate", x = "Hotel Rating", y = "Occupancy Rate")

####AVERAGE DISTANCE TRAVELLED BY DRIVER###3
my_data3<-head(mdata,15)
my_data3 %>%
  group_by(Driver.Name) %>%
  summarise(avg_distance = mean(Distance.to.Railway.Station..km.)) %>%
  ggplot(aes(x = reorder(Driver.Name, avg_distance), y = avg_distance)) +
  geom_bar(stat = "identity", fill = "lightgreen",color="darkgreen") +
  coord_flip() +
  labs(title = "Average Distance Traveled by Driver", x = "Driver Name", y = "Average Distance (Miles)")

###HOTEL MAP LOCATION###4
install.packages("ggmap")
install.packages("leaflet")
install.packages("leaflet.extras")
library(ggmap)
library(leaflet)
library(leaflet.extras)
hotels_data <- data.frame(
  hotel = c("The Oberoi", "ITC Gardenia", "St. Mark's Hotel", "Sterlings Mac Hotel",
            "Hotel Trinity Isle", "Hotel Nandhana Regent", "Hotel T.A.P. Silver Square", "Hotel Janpath"),
  lat = c(12.9738, 12.9666, 12.9710, 12.9609, 12.9938, 12.9536, 12.9745, 12.9720),
  lon = c(77.6200, 77.5958, 77.5990, 77.6467, 77.5736, 77.6419, 77.6100, 77.5950),
  color = c("red", "blue", "green", "purple", "orange", "pink", "yellow", "cyan") 
)
# Use awesomeIcons to assign colors to markers
icons <- awesomeIcons(
  icon = 'flag',     
  iconColor = 'white', 
  markerColor = hotels_data$color,  
  library = 'ion'    
)
# colorful markers
leaflet() %>%
  addTiles() %>%
  addAwesomeMarkers(data = hotels_data, 
                    lng = ~lon, lat = ~lat, 
                    icon = icons, 
                    popup = ~hotel) %>%
  setView(lng = 77.5946, lat = 12.9716, zoom = 12)

###TREE MAP FOR HOTEL VS OCCUPANCY RATE###5
install.packages("treemap")
library(treemap)
treemap(mdata,
        index = "Hotel.Name",              
        vSize = "Average.Occupancy.Rate....",          
        title = "Treemap of Hotels by Occupancy Rate",
        palette = "Set3",                   
        border.col = "white",               
        fontsize.labels = 12               
)

###HEATMAP FOR HOTEL OCCUPANCY VS RATING###6
my_data=head(mdata,147)
install.packages("ggplot2")
library(ggplot2)
ggplot(my_data, aes(x = Hotel.Name, y = Rating)) +
  geom_tile(aes(fill = Average.Occupancy.Rate....), color = "white") +
  scale_fill_gradient(low = "pink", high = "blue") +
  labs(title = "Heatmap of Hotel Rating and Average Occupancy Rate", 
       x = "Hotel Name", 
       y = "Rating",
       fill = "Average Occupancy Rate") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

###DRIVERS PERCENTAGE BY HOTEL###7
install.packages("dplyr")
install.packages("ggplot2")
library(ggplot2)
library(dplyr)
my_data1=head(mdata,10)
driver_by_hotel <- my_data1 %>%
  group_by(Hotel.Name, Driver.Name) %>%
  summarise(Count = n()) %>%  
  ungroup()                   
driver_by_hotel <- driver_by_hotel %>%
  group_by(Hotel.Name) %>%
  mutate(TotalDrivers = sum(Count)) %>%  
  ungroup()
# percentage 
driver_by_hotel$Percentage <- round(driver_by_hotel$Count / driver_by_hotel$TotalDrivers * 100, 1)
driver_by_hotel$Label <- paste0(driver_by_hotel$Driver.Name, " (", driver_by_hotel$Percentage, "%)")
ggplot(driver_by_hotel, aes(x = "", y = Count, fill = Driver.Name)) +
  geom_bar(stat = "identity", width = 1) +            
  coord_polar("y", start = 0) +                       
  facet_wrap(~ Hotel.Name) +                          
  geom_text(aes(label = paste0(Percentage, "%")),    
            position = position_stack(vjust = 0.5),   
            color = "white", size = 4) +              
  scale_fill_brewer(palette = "Set3") +               
  theme_void() +                                      
  labs(title = "Driver Distribution by Hotel with Percentages", fill = "Driver Name") +  
  theme(legend.position = "right",                   
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"), 
        legend.text = element_text(size = 10),       
        legend.title = element_text(size = 12))      


###PEAK DEMAND TIME###8
install.packages("dplyr")
library(dplyr)
install.packages("tidyr")
library(tidyr)
install.packages("ggplot2")
library(ggplot2)
my_data2 <- data.frame(STD.Time1 = c("06:30:00", "12:15:00", "18:45:00", "22:00:00", "03:55:00", "15:00:00"))
my_data2$hour <- as.numeric(format(as.POSIXct(my_data2$STD.Time1, format="%H:%M:%S"), "%H"))
my_data2$time_of_day <- cut(my_data2$hour, 
                            breaks = c(-Inf, 6, 12, 18, 24), 
                            labels = c("Night", "Morning", "Afternoon", "Evening"), 
                            right = FALSE)
time_data <- my_data2 %>%
  group_by(time_of_day) %>%
  summarise(count = n())
print(time_data)  
ggplot(time_data, aes(x = time_of_day, y = count, fill = time_of_day)) +
  geom_bar(stat = "identity", width = 0.7) +
  labs(title = "Distribution of Trips by Time of Day", x = "Time of Day", y = "Count of Trips") +
  scale_fill_manual(values = c("Morning" = "grey", "Afternoon" = "pink", "Evening" = "lightblue", "Night" = "violet")) +
  theme_minimal() +
  theme(legend.position = "none")  

###MOST FREQUENT DROP-OFF LOCATION###9
install.packages("dplyr")
library(dplyr)
install.packages("tidyr")
library(tidyr)
install.packages("ggplot2")
library(ggplot2)
# Group by hotel name and count occurrences
hotel_dropoff_data <- mdata %>%
  group_by(Hotel.Name) %>%     
  summarise(frequency = n())     
hotel_colors <- c("The Ritz-Carlton" = "tomato", "JW Marriott" = "salmon", "The Oberoi" = "lightcoral", 
                  "Vivanta" = "firebrick", "The Leela Palace" = "pink", 
                  "Hotel Janpath" = "black", "Hotel T.A.P. Silver Square" = "brown", 
                  "UG Deluxe Hotel" = "darkred","The Park Bangalore"="darkred","ITC Gardenia"="pink","Taj West End"="pink","Sterlings Mac Hotel"="pink","st.Mark's Hotel"='lightcoral',"Shangri-La"="brown","Radisson Blu Atria"="red","Conrad Bangalore"="salmon","Four Seasons"="firebrick","Hotel Nandhana Regent"="salmon")
ggplot(hotel_dropoff_data, aes(x = Hotel.Name, y = frequency)) +
  geom_point(aes(color = Hotel.Name, fill = Hotel.Name), 
             size = 5, alpha = 0.8, shape = 21, stroke = 1.5) + 
  scale_color_manual(values = hotel_colors) + 
  scale_fill_manual(values = hotel_colors) +  
  #  titles and labels
  labs(title = "Most Frequent Hotel Drop-Off Locations",
       x = "Hotel Name",
       y = "Number of Drop-Offs",
       color = "Hotel", fill = "Hotel") +
  theme_minimal(base_size = 15) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, color = "black", face = "bold"),
        axis.text.y = element_text(color = "black", face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
        legend.position = "right",
        legend.title = element_text(face = "bold", size = 12)) +
  theme(panel.grid.major = element_line(color = "lightgray", size = 0.5),
        panel.grid.minor = element_line(color = "lightgray", size = 0.25))  

###AVERAGE DISTANCE###10
average_distances <- mdata %>%
  summarise(
    AverageBusDistance = mean(`Distance.to.Bus.Stand..km.`, na.rm = TRUE),
    AverageAirportDistance = mean(`Distance.to.Airport..km.`, na.rm = TRUE),
    AverageRailwayDistance = mean(`Distance.to.Railway.Station..km.`, na.rm = TRUE)
  )
# line chart
line_data <- data.frame(
  Category = c("Bus", "Airport", "Railway"),
  AverageDistance = c(average_distances$AverageBusDistance, 
                      average_distances$AverageAirportDistance, 
                      average_distances$AverageRailwayDistance)
)
ggplot(line_data, aes(x = Category, y = AverageDistance, group = 1)) +
  geom_line(size = 1.5, color = "steelblue", linetype = "dashed") +  
  geom_point(size = 5, color = "red", shape = 21, fill = "yellow", stroke = 2) +  
  labs(title = "Average Trip Distances for Transport Categories",
       x = "Transport Category",
       y = "Average Distance (km)") +
  theme_minimal(base_size = 14) +  # Larger base size for better readability
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, color = "darkblue"), # Color x-axis text
        axis.text.y = element_text(color = "darkblue"),  # Color y-axis text
        plot.title = element_text(hjust = 0.5, size = 18, face = "bold", color = "darkred"),  # Title styling
        panel.grid.major = element_line(color = "lightgray", linetype = "dotted"),  # Major grid lines
        panel.grid.minor = element_blank())  # Remove minor grid lines


###Revenue by Hotel Location###11
install.packages("dplyr")
library(dplyr)
install.packages("tidyr")
library(tidyr)
install.packages("ggplot2")
library(ggplot2)
hotel_revenue <- mdata %>%
  group_by(Hotel.Name) %>%
  summarise(TotalRevenue = sum(`Fare.Price..INR.`, na.rm = TRUE))  
top_5_hotels <- hotel_revenue %>%
  arrange(desc(TotalRevenue)) %>% 
  head(5)                            
ggplot(top_5_hotels, aes(x = reorder(Hotel.Name, -TotalRevenue), y = TotalRevenue)) +
  geom_bar(stat = "identity", fill = "#3498db", color = "black", alpha = 0.8) +  
  geom_text(aes(label = scales::comma(TotalRevenue)), vjust = -0.5, size = 5, color = "darkblue") +  
  labs(title = "Top 5 Hotels by Uber Revenue",
       x = "Hotel Name",
       y = "Total Revenue (INR)") +
  theme_minimal(base_size = 14) +  
  theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold"), 
        axis.text.x = element_text(angle = 45, hjust = 1, color = "darkblue"),  
        axis.text.y = element_text(color = "darkblue"),  # Color y-axis text
        panel.grid.major = element_line(color = "lightgray", linetype = "dotted"),  
        panel.grid.minor = element_blank()) +  # Remove minor grid lines
  coord_cartesian(ylim = c(0, max(top_5_hotels$TotalRevenue) * 1.1))  

###DRIVER PERFORMANCE###12
install.packages("dplyr")
library(dplyr)
install.packages("tidyr")
library(tidyr)
install.packages("ggplot2")
library(ggplot2)
driver_performance <- mdata %>%
  group_by(Driver.Name) %>%
  summarise(TotalRating = sum(Rating.1, na.rm = TRUE)) %>%
  arrange(desc(TotalRating)) %>%
  head(5)  # Select top 5 drivers
pie_data <- data.frame(
  Driver = driver_performance$Driver.Name,
  TotalRating = driver_performance$TotalRating
)
pie_data <- pie_data %>%
  mutate(Percentage = TotalRating / sum(TotalRating) * 100,
         Label = paste0(Driver, ": ", round(Percentage, 1), "%"))  
#  pie chart
ggplot(pie_data, aes(x = "", y = TotalRating, fill = Driver)) +
  geom_bar(stat = "identity", width = 1) +                      
  coord_polar("y") +                                             
  geom_text(aes(label = Label), position = position_stack(vjust = 0.5), color = "white") +  
  labs(title = "Top 5 Drivers by Rating",
       fill = "Driver Name") +                                   
  theme_void(base_size = 14) +                                  
  theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),  
        legend.title = element_text(size = 14, face = "bold"),  
        legend.text = element_text(size = 12)) +                
  scale_fill_brewer(palette = "Set3")                           

###Purpose###13
install.packages("dplyr")
library(dplyr)
install.packages("tidyr")
library(tidyr)
install.packages("ggplot2")
library(ggplot2)
purpose_counts <- mdata %>%
  group_by(Purpose) %>%
  summarise(count = n())
ggplot(purpose_counts, aes(x = "", y = count, fill = Purpose)) +
  geom_bar(stat = "identity", width = 1) + 
  coord_polar("y", start = 0) +             
  labs(title = "Distribution of Purpose", fill = "Purpose") +  
  theme_void() +                           
  scale_fill_brewer(palette = "Pastel1") +  
  geom_text(aes(label = paste0(round(count/sum(count) * 100, 1), "%")),  
            position = position_stack(vjust = 0.5),  
            color = "black", size = 4, fontface = "bold") +  
  theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),  
        legend.title = element_text(size = 12, face = "bold"),  
        legend.text = element_text(size = 10),                  
        plot.background = element_rect(fill = "white"),         
        panel.grid = element_blank(),                          
        legend.position = "right") 

### recommendation ###
library(dplyr)
data1 <- read.csv("C:\\Users\\DHINAKARAN V\\Desktop\\project\\bangalore_hotel.csv")
data2 <- read.csv("C:\\Users\\DHINAKARAN V\\Desktop\\project\\uber_data.csv")
# Define function for hotel recommendation
recommend_hotel <- function(hotel_name_input) {
  # Filter hotel data for the specified hotel
  hotel_info <- data1 %>%
    filter(Hotel.Name == hotel_name_input) %>%
    select(Hotel.Name, Star.Category, Average.Occupancy.Rate....)
  
  # Check if hotel exists in data
  if (nrow(hotel_info) == 0) {
    cat("Hotel not found. Please enter a valid hotel name.\n")
    return()
  }
  drivers_info <- data2 %>%
    filter(Hotel.Name == hotel_name_input) %>%
    group_by(Driver.Name) %>%
    summarize(Average_Fare = mean(Fare.Price..INR., na.rm = TRUE), .groups = "drop") %>%
    arrange(Average_Fare)
  cat("=========== HAPPY JOURNEY ===========\n")
  cat("Hotel Name:", hotel_info$Hotel.Name[1], "\n")
  cat("Star Rating:", hotel_info$Star.Category[1], "\n")
  cat("Occupancy Rate (%):", hotel_info$Average.Occupancy.Rate....[1], "\n")
  cat("\nAvailable Drivers:\n")
  
  if (nrow(drivers_info) > 0) {
    for (i in 1:nrow(drivers_info)) {
      cat("  - Driver Name:", drivers_info$Driver.Name[i], "\n")
      cat("    Average Fare (INR):", round(drivers_info$Average_Fare[i], 2), "\n")
    }
  } else {
    cat("  No drivers available for this hotel.\n")
  }
  
  cat("=====================================\n")
}
recommend_hotel("ITC Gardenia")  
# Call the function with the input hotel name
recommend_hotel(hotel_name_input)
###Recommendation with map###2
library(dplyr)
library(leaflet)
data1 <- data.frame(
  Hotel.Name = c("The Oberoi", "ITC Gardenia", "St. Mark's Hotel", "Sterlings Mac Hotel",
                 "Hotel Trinity Isle", "Hotel Nandhana Regent", "Hotel T.A.P. Silver Square", "Hotel Janpath"),
  Star.Category = c(5, 5, 4, 4, 3, 3, 2, 2),
  Average.Occupancy.Rate.... = c(85, 90, 80, 75, 70, 65, 60, 55),
  Latitude = c(12.9738, 12.9666, 12.9710, 12.9609, 12.9938, 12.9536, 12.9745, 12.9720),
  Longitude = c(77.6200, 77.5958, 77.5990, 77.6467, 77.5736, 77.6419, 77.6100, 77.5950)
)
data2 <- data.frame(
  Hotel.Name = c("The Oberoi", "ITC Gardenia", "St. Mark's Hotel", "The Oberoi", 
                 "Hotel Nandhana Regent", "Hotel T.A.P. Silver Square", "Hotel Janpath", "Hotel Janpath"),
  Driver.Name = c("Raj", "Mohan", "Sita", "Arun", "Vinod", "Deepak", "Anil", "Sunil"),
  Fare.Price..INR. = c(250, 300, 220, 200, 150, 180, 170, 160)
)
show_map_and_drivers <- function(hotel_name_input = NULL) {
  color_palette <- colorFactor(c("red", "green", "blue", "purple", "orange"), domain = data1$Star.Category)
  map <- leaflet(data = data1) %>%
    addTiles() %>%
    addCircleMarkers(
      ~Longitude, ~Latitude,
      color = ~color_palette(Star.Category),  
      label = ~paste0("Hotel: ", Hotel.Name, "<br>",
                      "Rating: ", Star.Category, " stars<br>",
                      "Occupancy Rate: ", Average.Occupancy.Rate...., "%"),
      popup = ~paste0("Hotel: ", Hotel.Name, "<br>",
                      "Rating: ", Star.Category, " stars<br>",
                      "Occupancy Rate: ", Average.Occupancy.Rate...., "%"),
      radius = 8,
      fillOpacity = 0.7
    ) %>%
    setView(lng = mean(data1$Longitude), lat = mean(data1$Latitude), zoom = 12)
  print(map)
  if (!is.null(hotel_name_input)) {
    # Filter hotel data for the specified hotel
    hotel_info <- data1 %>%
      filter(Hotel.Name == hotel_name_input)
    if (nrow(hotel_info) == 0) {
      cat("Hotel not found. Please enter a valid hotel name.\n")
      return()
    }
    cat("=========== HAPPY JOURNEY ===========\n")
    cat("Hotel Name:", hotel_info$Hotel.Name[1], "\n")
    cat("Star Rating:", hotel_info$Star.Category[1], "\n")
    cat("Occupancy Rate (%):", hotel_info$Average.Occupancy.Rate....[1], "\n\n")
       drivers_info <- data2 %>%
      filter(Hotel.Name == hotel_name_input) %>%
      group_by(Driver.Name) %>%
      summarize(Average_Fare = mean(Fare.Price..INR., na.rm = TRUE), .groups = "drop") %>%
      arrange(Average_Fare)
    # Display driver information 
    if (nrow(drivers_info) > 0) {
      cat("===========================\n")
      cat("Available Drivers:\n")
      cat("===========================\n")
      for (i in 1:nrow(drivers_info)) {
        cat(paste0("Driver Name: ", drivers_info$Driver.Name[i], "\n"))
        cat(paste0("Average Fare (INR): ", round(drivers_info$Average_Fare[i], 2), "\n"))
        cat("===========================\n")
      }
    } else {
      cat("No drivers available for this hotel.\n")
    }
  }
}
show_map_and_drivers("ITC Gardenia")  #  desired hotel name
