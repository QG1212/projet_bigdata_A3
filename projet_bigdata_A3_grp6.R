data <- read.csv("C:/Users/hecto/Music/IRVE (1).csv")

install.packages("stringr") 
install.packages("dplyr")
install.packages("tidyverse") 
library(tidyverse)
library(stringr)
library(dplyr)


for (col in colnames(data)) {
  print(unique(data[[col]]))
}


# on debat pour Restriction de gabarit non précisée
mots=c("inconnu", "inconnue","Restriction de gabarit non précisée", "accessibilité inconnue","Accessibilité inconnue","Inconnue","NEANT","Néant","non concerné", "Non communiqué","non précisé","non renseigné","Non renseigné","unknown", "n/a", "na", "none", "null", "-", "?", "","Non communiqué","Non concerné ","aucune observations","aucune observation")
#gere aucune
print(mots)

for (col in colnames(data)) {
  #evite les chiffre
  if (is.character(data[[col]])) {
    
    # texte brut avec [[col]] et plus d'espace avec str_t
    x = str_trim(data[[col]])
    # On remplace par NA 
    data[[col]][x %in% mots] <- NA_character_
    
  }
}
#trouver sur le site du gouv https://doc.transport.data.gouv.fr/type-donnees/infrastructures-de-recharge-de-vehicules-electriques-irve/beta-base-nationale-irve-statique
liste_bool=c("prise_type_ef","prise_type_2","prise_type_combo_css","prise_type_combo_ccs","prise_type_chademo","prise_type_autre","gratuit","paiement_acte","paiement_cb","paiement_autre","reservation","station_deux_roues","cable_t2_attache", "consolidated_is_lon_lat_correct")

for (col in liste_bool) {
  #evite les chiffre
  if (is.character(data[[col]])){  
    
    t_minus =tolower(data[[col]])
    data[[col]][t_minus == "true"] = TRUE
    
    # Oremplace 
    data[[col]][t_minus == "false"] = FALSE
  }
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
