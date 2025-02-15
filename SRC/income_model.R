# Télécharger et installer les packages
install.packages(c("sf", "ggplot2", "tigris", "dplyr", "viridis", "readr", "lubridate", "stringr", "plm"))
library(sf)
library(ggplot2)
library(tigris)
library(dplyr)
library(viridis)
library(readr)
library(lubridate)
library(stringr)
library(plm)

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

### NHGIS

# Lire le fichier
file_path <- "Data/nhgis0002_ts_nominal_county.csv"
county_data <- read_csv(file_path)

county_data <- county_data %>%
  mutate(
    fips = paste(STATEFP, COUNTYFP, sep = ""),  # Crée la colonne fips sans séparateur
    decade = floor(YEAR / 10) * 10  # Créer une colonne 'decade' à partir de l'année
  )

# Sélectionner les colonnes nécessaires et créer la colonne 'decade'
county_data <- county_data %>%
  select(YEAR, STATE, COUNTY, B79AA, CL6AA, fips) %>%
  mutate(
    decade = floor(YEAR / 10) * 10  # Créer une colonne 'decade' à partir de l'année
  )

# Regrouper les données par 'fips' et 'decade' et calculer la médiane
county_summary <- county_data %>%
  group_by(fips, decade) %>%
  summarise(
    median_income = median(B79AA, na.rm = TRUE),  # Médiane du revenu médian
    median_poverty = median(CL6AA, na.rm = TRUE),  # Médiane des personnes sous le seuil de pauvreté
    .groups = "drop"  # Enlève le groupement après la sommation
  )


panel_data <- county_summary %>%
  left_join(fema_decade_summary, by = c("fips", "decade")) %>%
  left_join(migration_data, by = c("fips", "decade"))

# Vérifier les données agrégées
head(panel_data)

# Préparer les données en tant que données de panel
panel_data_plm <- pdata.frame(panel_data, index = c("fips", "decade"))

# Modèle de régression pour le revenu médian
income_model <- plm(median_income ~ is_fire + is_flood + is_tornado + is_hurricane + is_snowstorm + disasters_count, 
                    data = panel_data_plm, 
                    model = "within", 
                    effect = "twoways")

# Résumé des résultats
summary(income_model)

#########
# Tests #
#########

model_fe <- plm(median_income ~ is_fire + is_flood + is_tornado + is_hurricane + is_snowstorm + disasters_count, 
                    data = panel_data_plm, 
                    model = "within")

model_re <- plm(median_income ~ is_fire + is_flood + is_tornado + is_hurricane + is_snowstorm + disasters_count, 
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
