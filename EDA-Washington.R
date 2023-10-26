library(shiny)
library(tidyr)
library(dplyr)
library(readr)
library(ggplot2)
library(lubridate)
library(shiny)
library(leaflet)
library(plotly)
library(viridis)
library(viridisLite)
library(stringr)

rm(list=ls())

setwd("C:/Users/elect/OneDrive/DATA 340")

#read as an rds file
accident2021 <- read.csv("accident2021.csv")
saveRDS(accident2021, file = "accident2021.rds")
accident2021 <- readRDS("accident2021.rds")

accident2020 <- read.csv("accident2020.csv")
saveRDS(accident2020, file = "accident2020.rds")
accident2020 <- readRDS("accident2020.rds")

accident2019 <- read.csv("accident2019.csv")
saveRDS(accident2019, file = "accident2019.rds")
accident2019 <- readRDS("accident2019.rds")

accident2018 <- read.csv("accident2018.csv")
saveRDS(accident2018, file = "accident2018.rds")
accident2018 <- readRDS("accident2018.rds")

accident2021 <- filter(accident2021, STATENAME %in% c("Washington"))
accident2020 <- filter(accident2020, STATENAME %in% c("Washington"))
accident2019 <- filter(accident2019, STATENAME %in% c("Washington"))
accident2018 <- filter(accident2018, STATENAME %in% c("Washington"))

# Convert LATITUDENAME column to character in all datasets
accident2021 <- mutate(accident2021, LATITUDENAME = as.character(LATITUDENAME))
accident2020 <- mutate(accident2020, LATITUDENAME = as.character(LATITUDENAME))
accident2019 <- mutate(accident2019, LATITUDENAME = as.character(LATITUDENAME))
accident2018 <- mutate(accident2018, LATITUDENAME = as.character(LATITUDENAME))



convert_to_character <- function(data) {
  data %>% mutate_if(~ !is.character(.), as.character)
}

# Apply the conversion function to all datasets
accident2021 <- convert_to_character(accident2021)
accident2020 <- convert_to_character(accident2020)
accident2019 <- convert_to_character(accident2019)
accident2018 <- convert_to_character(accident2018)

# Combine the datasets
washington_data <- bind_rows(accident2021, accident2020, accident2019, accident2018)

# Remove rows with null values
washington_data <- na.omit(washington_data)


#Accident count by mont (2021)
ggplot(accident2021, aes(x = factor(MONTHNAME, levels = month.name))) +
  geom_bar(fill = "violet") +
  labs(title = "Accident Count by Month(2021)", x = "Month", y = "Accident Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



#Accident Heatmap by Hour and Day of Week 
ggplot(accident2021, aes(x = HOURNAME, y = DAY_WEEKNAME)) +
  geom_bin2d() +
  labs(title = "Accident Heatmap by Hour and Day of Week", x = "Hour", y = "Day of Week") +
  scale_fill_gradient(low = "white", high = "blue") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels
#accidents count by month in years 2020, 2019, 2018
ggplot(accident2020, aes(x = factor(MONTHNAME, levels = month.name))) +
  geom_bar(fill = "dodgerblue") +
  labs(title = "Accident Count by Month(2020)", x = "Month", y = "Accident Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(accident2019, aes(x = factor(MONTHNAME, levels = month.name))) +
  geom_bar(fill = "firebrick") +
  labs(title = "Accident Count by Month(2019)", x = "Month", y = "Accident Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(accident2018, aes(x = factor(MONTHNAME, levels = month.name))) +
  geom_bar(fill = "forestgreen") +
  labs(title = "Accident Count by Month(2018)", x = "Month", y = "Accident Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


##shiny app for accidents in 2021 in WA

# Convert LATITUDE and LONGITUD to numeric
accident2021$LATITUDE <- as.numeric(as.character(accident2021$LATITUDE))
accident2021$LONGITUD <- as.numeric(as.character(accident2021$LONGITUD))


# Calculate the count of accidents for each ROUTENAME and sort by count (highest to lowest)
route_counts <- accident2021 %>%
  group_by(ROUTENAME) %>%
  summarize(Count = n()) %>%
  arrange(desc(Count))  # Sort by count in descending order

# Get the sorted ROUTENAME options
route_names <- route_counts$ROUTENAME

# Define the UI for the Shiny app
ui <- fluidPage(
  titlePanel("Washington State Accident Map"),
  selectInput(
    inputId = "route_selector",
    label = "Select Route Name:",
    choices = route_names,
    selected = route_names[1]
  ),
  textOutput("total_accidents"),
  leafletOutput("accident_map")
)

# Define the server for the Shiny app
server <- function(input, output) {
  # Filter data based on selected ROUTENAME
  selected_route_data <- reactive({
    filter(accident2021, ROUTENAME == input$route_selector)
  })
  
  # Calculate the total number of accidents for the selected ROUTENAME
  total_accidents <- reactive({
    nrow(selected_route_data())
  })
  
  # Create Leaflet map
  output$accident_map <- renderLeaflet({
    # Filter data for the state of Washington
    washington_map <- leaflet() %>%
      setView(lng = -120.5, lat = 47.3, zoom = 7)  # Center and zoom to Washington State
    
    # Add road map
    washington_map <- addProviderTiles(washington_map, "Esri.WorldStreetMap")
    
    accidents_subset <- selected_route_data()
    
    washington_map <- addCircleMarkers(washington_map, data = accidents_subset,
                                       lng = ~LONGITUD, lat = ~LATITUDE,
                                       radius = 5, popup = ~ROUTENAME)
    
    washington_map
  })
  
  # Display the total count of accidents for the selected ROUTENAME
  output$total_accidents <- renderText({
    route_name <- input$route_selector
    total_accidents_count <- total_accidents()
    paste("There are", total_accidents_count, "accidents in", route_name, "ROUTENAME.")
  })
}

# Run the Shiny app
shinyApp(ui, server)



##Notice that State Highway has most accidents
#filter data to that routename
# Step 1: Filter the dataset for "State Highway" ROUTENAME
state_highway_accidents <- accident2021 %>%
  filter(ROUTENAME == "State Highway")

# Bar chart for accident types (HARM_EVNAME)
accident_types <- state_highway_accidents %>%
  group_by(HARM_EVNAME) %>%
  summarize(Count = n()) %>%
  arrange(desc(Count))

ggplot(accident_types, aes(x = reorder(HARM_EVNAME, -Count), y = Count)) +
  geom_bar(stat = "identity", fill = "blue") +
  coord_flip() +
  labs(title = "Distribution of Accident Types on State Highways", x = "Accident Type", y = "Count")


# Causes of accidents (MAN_COLLNAME)
# Group and count the causes of accidents
causes_counts <- state_highway_accidents %>%
  group_by(MAN_COLLNAME) %>%
  summarize(count = n())

# Horizontal bar chart
ggplot(causes_counts, aes(x = reorder(MAN_COLLNAME, -count), y = count)) +
  geom_bar(stat = "identity", fill = "dodgerblue") +
  coord_flip() +  # Horizontal bars
  labs(title = "Causes of Accidents on State Highways", x = "Count") +
  theme_minimal()


## Accidents by day of the week and hour of the day
day_of_week_counts <- state_highway_accidents %>%
  group_by(DAY_WEEKNAME) %>%
  summarize(Count = n())

# Bar plot for day of the week
ggplot(day_of_week_counts, aes(x = reorder(DAY_WEEKNAME, -Count), y = Count)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Accidents by Day of the Week",
       x = "Day of the Week",
       y = "Number of Accidents") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Filter the data for accidents on Fridays
friday_data <- filter(state_highway_accidents, DAY_WEEKNAME == "Friday")

# Graph for the number of accidents by hour on Fridays
hour_of_day_counts_friday <- friday_data %>%
  group_by(HOURNAME) %>%
  summarize(Count = n())

# Bar plot for the number of accidents by hour on Fridays
ggplot(hour_of_day_counts_friday, aes(x = reorder(HOURNAME, -Count), y = Count)) +
  geom_bar(stat = "identity", fill = "lightcoral") +
  labs(title = "Accidents by Hour on Fridays (State Highway)",
       x = "Hour of the Day",
       y = "Number of Accidents") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
        
