# Download and install packages
install.packages(c("sf", "ggplot2", "tigris", "dplyr", "viridis", "readr", "lubridate"))
library(sf)
library(ggplot2)
library(tigris)
library(dplyr)
library(viridis)
library(readr)
library(lubridate)

# Load data FEMA
fema_data <- read.csv("Data/DisasterDeclarationsSummaries.csv")

# Convert dates
fema_data$incidentBeginDate <- as.Date(fema_data$incidentBeginDate)

# Filter data
fema_data <- fema_data %>%
  filter(year(incidentBeginDate) >= 1960 & year(incidentBeginDate) <= 2010)

# Filter incidents
categories <- c("Fire", "Flood", "Snowstorm", "Tornado", "Hurricane")
fema_filtered <- fema_data %>%
  filter(incidentType %in% categories)

# Add decade column
fema_filtered <- fema_filtered %>%
  mutate(decade = floor(year(incidentBeginDate) / 10) * 10)

# Count incidents
incident_decade_counts <- fema_filtered %>%
  group_by(incidentType, fipsStateCode, fipsCountyCode, decade) %>%
  summarise(total_incidents = n(), .groups = "drop")

# Number of counties and decades
num_counties <- 3069
num_decades <- 5  # 1960-2010

# Calculate
incident_summary <- incident_decade_counts %>%
  group_by(incidentType, decade) %>%
  summarise(
    mean_incidents = sum(total_incidents) / num_counties,  # Moyenne par comté
    std_dev_incidents = sd(total_incidents, na.rm = TRUE),  # Écart-type des incidents
    .groups = "drop"
  ) %>%
  group_by(incidentType) %>%
  summarise(
    mean_incidents_per_county_decade = sum(mean_incidents) / num_decades,  # Moyenne par décennie
    std_dev_per_county_decade = sqrt(sum(std_dev_incidents^2, na.rm = TRUE) / num_decades),  # Écart-type par décennie
    .groups = "drop"
  )

# Display table
print(incident_summary)
