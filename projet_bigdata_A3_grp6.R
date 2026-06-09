data <- read.csv("C:/Users/hecto/Music/IRVE (1).csv")

install.packages("stringr") 
install.packages("dplyr")
install.packages("tidyverse") 
library(tidyverse)
library(stringr)
library(dplyr)


#suppression

data <- data[data[["id_pdc_itinerance"]] != "Non concerné", ]
#trier par la plus recente
data <- data[order(data[["date_maj"]], na.last = TRUE), ]
#arder uniquement la première de chaque id_pdc_itinerance, revoie false si ne la jamais renconter , true si la rencontrer= suppr
data <- data[!duplicated(data[["id_pdc_itinerance"]]), ]





# ------------------------------------------------------------------------------
# 1. LISTE DES VALEURS À CONVERTIR EN NA
# ------------------------------------------------------------------------------
mots_vides <- c(("sans", "xx" ,"inconnu", "inconnue","Inconnu","0000","Restriction de gabarit non précisée","restriction gabarit inconnue", "Non concerné","no information","Restriction de gabarit non pr\u008ecis\u008ee","Restriction de gabarit non prÃ©cisÃ©e","restriction gabarit inconnues","accessibilité inconnue","Accessibilité inconnue","Inconnue","NEANT","Néant","non concerné", "Non communiqué","non précisé","non renseigné","Non renseigné","unknown", "n/a", "na", "none", "null", "-", "?", "","/","Non communiqué","Non concerné ","aucune observations","aucune observation")
)

# ------------------------------------------------------------------------------
# 2. NETTOYAGE GLOBAL ET HARMONISATION EN UN SEUL PIPELINE
# ------------------------------------------------------------------------------
data <- data %>%
  # Étape A : Nettoyage global de TOUTES les colonnes texte
  mutate(across(where(is.character), ~ {
    texte_nettoye <- str_trim(.x)
    if_else(str_to_lower(texte_nettoye) %in% mots_vides, NA_character_, texte_nettoye)
  })) %>%
  
  # Étape B : Harmonisation spécifique de la colonne 'restriction_gabarit'
  mutate(
    restriction_gabarit = case_when(
      # Cas 1 : Si la valeur est déjà un NA (grâce au nettoyage de l'étape A)
      is.na(restriction_gabarit) ~ NA_character_,
      
      # Cas 2 : S'il n'y a pas de restriction -> "FALSE"
      str_detect(str_to_lower(restriction_gabarit), "aucune|pas de restriction|aucun|ras|^non$|sans restriction") ~ "FALSE",      
      
      # Cas 3 : Extraction et formatage de la dimension numérique (ex: "1,5" -> "1.5m")
      str_detect(restriction_gabarit, "[0-9]") ~ {
        format_standard <- str_replace_all(str_to_lower(restriction_gabarit), "([0-9]+)[,m]([0-9]+)", "\\1.\\2")
        valeur_num      <- str_extract(format_standard, "[0-9]+(\\.[0-9]+)?")
        paste0(valeur_num, "m")
      },
      
      # Cas 4 : Sécurité (on garde la valeur d'origine si aucun cas ne correspond)
      .default = restriction_gabarit
    )
  )

# ------------------------------------------------------------------------------
# 3. VÉRIFICATION DU RÉSULTAT
# ------------------------------------------------------------------------------
# Une seule ligne simple pour voir les valeurs uniques de chaque colonne
lapply(data, unique)

# Aperçu visuel dans RStudio


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
View(data)


trouver sur le site du gouv https://doc.transport.data.gouv.fr/type-donnees/infrastructures-de-recharge-de-vehicules-electriques-irve/beta-base-nationale-irve-statique
liste_bool=c("prise_type_ef","prise_type_2","prise_type_combo_css","prise_type_combo_ccs","prise_type_chademo","prise_type_autre","gratuit","paiement_acte","paiement_cb","paiement_autre","reservation","station_deux_roues","cable_t2_attache", "consolidated_is_lon_lat_correct")
for (col in liste_bool) {
  #evite les chiffre
  if (is.character(data[[col]])){  
    
    
    t_minus =tolower(data[[col]])
    data[[col]][t_minus == "true"] = "1"
    
    # Oremplace 
    data[[col]][t_minus == "false"] = "0" }
  
  
}

#retirer les 0 qui ne sont pas des false
for (col in colnames(data)) {
  if (!col %in% liste_bool) {
    data[[col]][data[[col]] == 0] = NA
  }
}

data$condition_acces <- ifelse(tolower(data$condition_acces) == "accès réservé", "accès réservé", "accès libre")
data[["implantation_station"]][data[["implantation_station"]] == "Parking priv\u008e \u0088 usage public"] = "Parking privé à usage public"

data[["implantation_station"]][data[["implantation_station"]] == "Parking priv\u008e r\u008eserv\u008e \u0088 la client\u008fle"] = "Parking privé réservé à la clientèle"





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
