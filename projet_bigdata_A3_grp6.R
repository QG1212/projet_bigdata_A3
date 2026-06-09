data <- read.csv("C:/Users/hecto/Music/IRVE (1).csv")

library(tidyverse)

for (col in colnames(data)) {
  print(unique(data[[col]]))
}

mots=c("inconnu", "inconnue", "accessibilité inconnue","unknown", "n/a", "na", "none", "null", "-", "?", "")
for (col in colnames(data)) {
  x = str_trim(str_to_lower(as.character(df[[col]])))
  df[[col]][x %in% mots_inconnus] <- NA
}

bool_cols <- c("prise_type_ef", "prise_type_2", "gratuit", "reservation")

for (col in bool_cols) {
  x <- str_trim(str_to_lower(as.character(df[[col]])))
  df[[col]] <- case_when(
    x == "true"  ~ "true",
    x == "false" ~ "false",
    .default = NA
  )
}

library(dplyr)

data <- read.csv("C:/Users/hecto/Music/IRVE (1).csv", sep = ",", stringsAsFactors = FALSE)

# 2. Filtrage intelligent par "Blocs Géographiques" (Métropole + Corse)
data_metropole_propre <- data %>%
  mutate(
    consolidated_latitude = as.numeric(consolidated_latitude),
    consolidated_longitude = as.numeric(consolidated_longitude)
  ) %>%
  filter(
    # ZONE 1 : Cœur et Ouest de la France
    (consolidated_latitude >= 42.3 & consolidated_latitude <= 51.1 & 
       consolidated_longitude >= -5.2 & consolidated_longitude <= 6.0) |
      
      # ZONE 2 : Est de la France (Alsace / Alpes)
      (consolidated_latitude >= 44.5 & consolidated_latitude <= 49.5 & 
         consolidated_longitude > 6.0 & consolidated_longitude <= 7.5) |
      
      # ZONE 3 : Corse
      (consolidated_latitude >= 41.3 & consolidated_latitude <= 43.1 & 
         consolidated_longitude >= 8.5 & consolidated_longitude <= 9.6)
  )

View(data_metropole_propre)
