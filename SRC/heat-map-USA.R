# Download libraries
packages <- c("sf", "ggplot2", "tigris", "dplyr", "viridis", "readr", "lubridate", "magrittr")
installed_packages <- rownames(installed.packages())

for (pkg in packages) {
  if (!pkg %in% installed_packages) {
    install.packages(pkg, repos = "https://cloud.r-project.org/")
  }
}

library(sf)
library(ggplot2)
library(tigris)
library(dplyr)      # for %>% and data manipulation
library(magrittr)   # if needed for the pipe operator
library(lubridate)  # for the year() function
library(scales)
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
  filter(!(STATEFP %in% c("02", "15"))) # Exclure l'Alaska et Hawaï

# Joindre les données sur les FIPS
counties_data <- counties %>%
  left_join(incident_counts, by = c("GEOID" = "FIPS"))

# Remplace NA values by 0
counties_data$total_incidents[is.na(counties_data$total_incidents)] <- 0

# Create the map
ggplot(data = counties_data) +
  geom_sf(aes(fill = total_incidents)) + # Colorier selon le nombre d'incidents
  scale_fill_viridis_c(
  option = "magma", 
  direction = -1, 
  trans = "log", 
  na.value = "white",
  breaks = pretty_breaks(n = 5)  # Generates about 5 "nice" breaks
) +
  theme_minimal() +
  labs( # title = "Number of incidents (1960-2010) by counties in USA",
    fill = "Total incidents"
  ) +
  coord_sf(xlim = c(-125, -66), ylim = c(24, 50)) # continental USA centered

# # Create a categorical variable with 4 bins:
# counties_data <- counties_data %>%
#   mutate(incidents_cat = cut(total_incidents,
#                              breaks = c(0, 4, 8, 15, Inf),
#                              include.lowest = TRUE,
#                              right = FALSE,
#                              labels = c("0-4", "4-8", "8-15", "15+")))

# # Create the map with a discrete color scale:
# ggplot(data = counties_data) +
#   geom_sf(aes(fill = incidents_cat)) +
#   scale_fill_viridis_d(option = "magma", direction = -1, na.value = "white") +
#   theme_minimal() +
#   labs(fill = "Total incidents") +
#   coord_sf(xlim = c(-125, -66), ylim = c(24, 50))

  # Save the plot
  ggsave("images/USA_heatmap.png", width = 10, height = 7)