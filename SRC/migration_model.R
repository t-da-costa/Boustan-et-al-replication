# Download and install packages
install.packages(c("sf", "ggplot2", "tigris", "dplyr", "viridis", "readr", "lubridate", "readr", "stringr", "plm", "dplyr", "car", "lmtest", "sandwich", "tseries","AER"))
library(sf)
library(ggplot2)
library(tigris)
library(dplyr)
library(viridis)
library(readr)
library(lubridate)
library(readr)
library(stringr)
library(plm)
library(dplyr)
library(lmtest)                    # Pour les tests de spécification du modèle
library(car)                        # Pour calculer les VIF
library(sandwich)                  # Pour les erreurs standard robustes
library(tseries)                   # Pour les tests de stationnarité
library(AER)                        # Pour d'autres tests de spécification

##############
# PreProcess #
##############

### FEMA

# Charger les données de FEMA
fema_data <- read.csv("Data/DisasterDeclarationsSummaries.csv")

# Convertir les dates
fema_data$incidentBeginDate <- as.Date(fema_data$incidentBeginDate)

# Créer la colonne FIPS en format 5 caractères avec l'état et le comté
fema_data <- fema_data %>%
  mutate(fips = sprintf("%02d%03d", fipsStateCode, fipsCountyCode))

# Créer une colonne 'decade' en arrondissant l'année de début de l'incident à la décennie la plus proche
fema_data <- fema_data %>%
  mutate(decade = floor(year(incidentBeginDate) / 10) * 10)

# Filtrer les incidents qui sont pertinents pour l'analyse (par exemple, les incidents majeurs)
categories <- c("Fire", "Flood", "Snowstorm", "Tornado", "Hurricane")
fema_filtered <- fema_data %>%
  filter(incidentType %in% categories)

# Créer une colonne 'disasters_count' qui compte le nombre d'incidents par fips et decade
fema_filtered <- fema_filtered %>%
  filter(incidentType %in% categories) %>%
  group_by(fips, decade, incidentType) %>%
  summarise(disasters_count = n(), .groups = "drop")

# Créer les colonnes pour chaque type d'incident
fema_decade_summary <- fema_filtered %>%
  mutate(
    is_fire = ifelse(incidentType == "Fire", disasters_count, 0),
    is_flood = ifelse(incidentType == "Flood", disasters_count, 0),
    is_tornado = ifelse(incidentType == "Tornado", disasters_count, 0),
    is_hurricane = ifelse(incidentType == "Hurricane", disasters_count, 0),
    is_snowstorm = ifelse(incidentType == "Snowstorm", disasters_count, 0)
  ) %>%
  # Regrouper pour sommer les incidents par fips et decade
  group_by(fips, decade) %>%
  summarise(
    disasters_count = sum(disasters_count),
    is_fire = sum(is_fire),
    is_flood = sum(is_flood),
    is_tornado = sum(is_tornado),
    is_hurricane = sum(is_hurricane),
    is_snowstorm = sum(is_snowstorm),
    .groups = "drop"
  )


### Netmigration

folder_path <- "Data/netmigration"
migration_files <- list.files(path = folder_path, pattern = "netmigration_\\d{4}\\.csv", full.names = TRUE)

migration_data <- lapply(migration_files, function(file) {
  # Extraction de l'année à partir du nom du fichier
  year <- as.numeric(str_extract(file, "\\d{4}"))
  
  # Conversion en décennie
  decade <- floor(year / 10) * 10
  
  # Lecture du fichier
  df <- read_csv(file)
  
  # Ajout de la colonne décennie
  df <- df %>%
    mutate(decade = decade)
  
  return(df)
}) %>% bind_rows()  # Fusionner tous les fichiers en une seule table

migration_data <- migration_data %>%
  select(stname, name, fips, decade, starts_with("r5ttt"), starts_with("m5ttt"), starts_with("e5ttt"))

migration_data_aggregated <- migration_data %>%
  group_by(fips, decade) %>%
  summarise(
    total_migration = sum(r5ttt0, r5ttt5, r5ttt10, r5ttt15, r5ttt20, r5ttt25, r5ttt30, r5ttt35, r5ttt40, r5ttt45, r5ttt50, r5ttt55, r5ttt60, r5ttt65, r5ttt70, r5ttt75, 
                          m5ttt0, m5ttt5, m5ttt10, m5ttt15, m5ttt20, m5ttt25, m5ttt30, m5ttt35, m5ttt40, m5ttt45, m5ttt50, m5ttt55, m5ttt60, m5ttt65, m5ttt70, m5ttt75, 
                          e5ttt0, e5ttt5, e5ttt10, e5ttt15, e5ttt20, e5ttt25, e5ttt30, e5ttt35, e5ttt40, e5ttt45, e5ttt50, e5ttt55, e5ttt60, e5ttt65, e5ttt70, e5ttt75, 
                          na.rm = TRUE),  # Total des migrations nettes sur toutes les périodes
    .groups = "drop"
  )

# Fusionner les données de FEMA, de migrations et de revenus pour créer une table complète
panel_data <- migration_data_aggregated %>%
  left_join(fema_decade_summary, by = c("fips", "decade"))

# Préparer les données en tant que données de panel
panel_data_plm <- pdata.frame(panel_data, index = c("fips", "decade"))

# Modèle de régression pour les migrations nettes
migration_model <- plm(total_migration ~ is_fire + is_flood + is_tornado + 
                         is_hurricane + is_snowstorm, 
                       data = panel_data_plm, 
                       effect = "twoways", 
                       model = "within")

# Résumé des résultats
summary(migration_model)

#########
# Tests #
#########

model_fe <- plm(total_migration ~ is_fire + is_flood + is_tornado + is_hurricane + is_snowstorm,
                data = panel_data_plm, 
                model = "within")

model_re <- plm(total_migration ~ is_fire + is_flood + is_tornado + is_hurricane + is_snowstorm,
                data = panel_data_plm, 
                model = "random")

# Test de Hausman
# Compare les modèles à effets fixes et aléatoires
phtest(model_fe, model_re)

# Test de Breusch-Pagan pour les effets aléatoires (test d'hétéroscédasticité)
# Vérifie si les effets aléatoires sont significatifs
plmtest(model_fe, type = "bp")

# Test de Wooldridge pour l'autocorrélation des résidus dans les panels
# Test pour la présence d'autocorrélation dans les données en panel
pwartest(model_fe)

# Test de Hétéroscédasticité avec Breusch-Pagan
# Vérifie l'hétéroscédasticité des erreurs dans le modèle
bptest(model_fe)

# Test de normalité des résidus (Kolmogorov-Smirnov)
# Vérifie si les résidus suivent une distribution normale
ks.test(residuals(model_fe), "pnorm")

# Calcul des VIF (Variance Inflation Factor)
# Vérifie les problèmes de multicolinéarité dans le modèle
vif(model_fe)

# Test RESET pour la spécification du modèle
# Test pour voir si le modèle est bien spécifié (ajout de termes quadratiques)
resettest(model_fe)

# Erreurs standard robustes de Newey-West
# Utilise les erreurs standard robustes de Newey-West pour ajuster les erreurs standard
coeftest(model_fe, vcov = vcovNW(model_fe))

