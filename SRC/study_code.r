# Install packages
install.packages(c("tidyverse", "lubridate", "dplyr"))

# Load packages
library(tidyverse)
library(lubridate)

########
# Data #
########

# Load data from FEMA
fema_data <- read.csv("Data/DisasterDeclarationsSummaries.csv")

# Convert data
fema_data$declarationDate <- as.Date(fema_data$declarationDate)
fema_data$incidentBeginDate <- as.Date(fema_data$incidentBeginDate)
fema_data$incidentEndDate <- as.Date(fema_data$incidentEndDate)

# Extract the decade
fema_data$decade <- floor(year(fema_data$incidentBeginDate) / 10) * 10

# Count disasters
disaster_counts <- fema_data %>%
  group_by(fipsCountyCode, decade) %>%
  summarise(
    total_disasters = n(),
    severe_disaster = any(paProgramDeclared == 1)  # Indicateur de désastre sévère
  )

# Load data from netmigration
migration_folder <- "Data/netmigration"
migration_files <- list.files(path = migration_folder, pattern = "*.csv", full.names = TRUE)

# Function to load data and extract decade from filename
load_migration_data <- function(file_path) {
  # Extract decade from filename (assumes format like 'migration_1990.csv')
  decade <- as.numeric(str_extract(basename(file_path), "\\d{4}"))
  
  # Read CSV file
  df <- read.csv(file_path) %>%
    mutate(decade = decade)  # Add decade column
  
  return(df)
}


######################
# Data visualisation #
######################

# Filtrer les données entre 1960 et 2010
fema_data <- fema_data %>%
  filter(year(incidentBeginDate) >= 1960 & year(incidentBeginDate) <= 2010)

# Filtrer les désastres des catégories souhaitées
categories <- c("Fire", "Flood", "Snowstorm", "Tornado", "Hurricane")
fema_data_filtered <- fema_data %>%
  filter(incidentType %in% categories)

# Regrouper par incidentType et compter les disasterNumber uniques
disaster_counts <- fema_data_filtered %>%
  group_by(incidentType) %>%
  summarise(
    unique_disasters = n_distinct(disasterNumber)
  ) %>%
  ungroup()  # Pour retirer le groupement

# Afficher le résultat
print(disaster_counts)