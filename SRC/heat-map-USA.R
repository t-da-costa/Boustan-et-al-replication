# Download libraries
install.packages(c("sf", "ggplot2", "tigris", "dplyr", "viridis", "readr"))
library(sf)
library(ggplot2)
library(tigris)
library(dplyr)
library(viridis)
library(readr)

# load data
fema_data <- read.csv("Data/DisasterDeclarationsSummaries.csv")

# Convert dates
fema_data$incidentBeginDate <- as.Date(fema_data$incidentBeginDate)

# Filter data
fema_data <- fema_data %>%
  filter(year(incidentBeginDate) >= 1960 & year(incidentBeginDate) <= 2010)

# Filter incident by type
categories <- c("Fire", "Flood", "Snowstorm", "Tornado", "Hurricane")
fema_filtered <- fema_data %>%
  filter(incidentType %in% categories)

# Count by counties(fipsCountyCode)
incident_counts <- fema_filtered %>%
  group_by(fipsStateCode, fipsCountyCode) %>%
  summarise(total_incidents = n(), .groups = "drop")

# Create FIPS column
incident_counts <- incident_counts %>%
  mutate(FIPS = sprintf("%02d%03d", fipsStateCode, fipsCountyCode))

# Télécharger les géométries des comtés US (hors Alaska & Hawaï)
counties <- counties(cb = TRUE) %>%
  filter(!(STATEFP %in% c("02", "15")))  # Exclure l'Alaska et Hawaï

# Joindre les données sur les FIPS
counties_data <- counties %>%
  left_join(incident_counts, by = c("GEOID" = "FIPS"))

# Remplace NA values by 0
counties_data$total_incidents[is.na(counties_data$total_incidents)] <- 0

# Create the map
ggplot(data = counties_data) +
  geom_sf(aes(fill = total_incidents)) +  # Colorier selon le nombre d'incidents
  scale_fill_viridis_c(option = "magma", trans = "log", na.value = "white") +  # Palette & échelle log
  theme_minimal() +
  labs(title = "Number of incidents (1960-2010) by counties in USA",
       fill = "Total incidents") +
  coord_sf(xlim = c(-125, -66), ylim = c(24, 50))  # continental USA centered
