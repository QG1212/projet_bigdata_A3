data <- read.csv("C:/Quentin/Ecole/ISEN/A3/IRVE.csv")

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
# Fonctionnalité 1
# ------------------------------------------------------------------------------

# Mots considérés comme non pertinents
mots_vides <- c(
  "inconnu", "inconnue", "sans", "xx", "/", "restriction de gabarit non précisée", 
  "accessibilité inconnue", "neant", "néant", "non concerné", "non communiqué", 
  "non précisé", "non renseigné", "unknown", "n/a", "na", "none", "null", 
  "-", "?", "", "aucune observations", "aucune observation"
)

# Règle de nettoyage de donnée
nettoyer_texte <- function(colonne_texte) {
  # Enlève les epaces en trop
  texte_propre <- str_trim(colonne_texte)
  
  # Vérifie si le mot fait parti de la liste ci-dessus
  if_else(
    is.element(str_to_lower(texte_propre), mots_vides), 
    NA_character_, 
    texte_propre
  )
}

# Nouveau data
data <- mutate(
  # arg 1 : notre jeu de donnée
  data,
  
  # arg 2 : applique la fct de nettoyage au colonne texte
  across(where(is.character), nettoyer_texte),
  
  # arg 3 : Harmonisation des colonnes
  restriction_gabarit = case_when(
    
    # Confirme que la valeur est Na
    is.na(restriction_gabarit) ~ NA_character_,
    
    # Cherche les mots clé et remplace par false
    str_detect(str_to_lower(restriction_gabarit), "aucune|pas de restriction|aucun|ras|^non$|sans restriction") ~ "FALSE",     
    
    # Cherche des chiffres
    str_detect(restriction_gabarit, "[0-9]") ~ {
      
      # Remplace les virgules et ajoute un 'm' derière le chiffre
      format_standard <- str_replace_all(str_to_lower(restriction_gabarit), "([0-9]+)[,m]([0-9]+)", "\\1.\\2")
      
      # Extrait partie numérique
      valeur_num<- str_extract(format_standard, "[0-9]+(\\.[0-9]+)?")
      
      # concaténation du chiffre et du m
      paste0(valeur_num, "m")
    },
    
    # Pas de changement
    .default = restriction_gabarit
  )
)

# Liste les valeurs uniques
lapply(data, unique)

# Visualisation
View(data)


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


#trouver sur le site du gouv https://doc.transport.data.gouv.fr/type-donnees/infrastructures-de-recharge-de-vehicules-electriques-irve/beta-base-nationale-irve-statique
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


# ==============================================================================
# FONCTIONNALITÉ 2 : VISUALISATION DES DONNÉES ET EXPORT PNG
# ==============================================================================

# Chargement des bibliothèques nécessaires pour le script
library(dplyr)      
library(ggplot2)    
library(lubridate)  
library(tidyr)      

# Importation du jeu de données
data <- read.csv("C:/Quentin/Ecole/ISEN/A3/IRVE.csv")


# GRAPHIQUE 1 : Évolution du nombre de stations mises en service


# Nouvelle colonne pour isoler des variables
data_evolution <- mutate(
  data, 
  # Convertion texte -> format date
  date_service = ymd(date_mise_en_service), 
  # Force toutes les dates au premier jour de leur mois (pour grouper par mois)
  annee_mois = floor_date(date_service, "month")
)

# On affiche selon cette nouvelle colonne
data_evolution <- group_by(data_evolution, annee_mois)

# On compte le nombre de lignes dans 'stations' pour chaque mois
data_evolution <- summarise(data_evolution, nombre_stations = n())

# On supprime les lignes avec mois incoonu
data_evolution <- drop_na(data_evolution, annee_mois)


# On crée plusieurs couche siur le graph avec le +

# Initialisation du graphe avec x et y
graph_evo <- ggplot(data_evolution, aes(x = annee_mois, y = nombre_stations)) +
  
  # Ligne bleu
  geom_line(color = "#0072B2", linewidth = 1) +      
  
  # Point orange
  geom_point(color = "#D55E00", size = 2) +          
  
  # Element textuel
  labs(
    title = "Évolution des mises en service de stations",
    x = "Date (Mois et Année)",
    y = "Nombre de nouvelles stations"
  ) +
  
  # Style simple
  theme_minimal()


# Exportation sur l'ordi
ggsave("1_evolution_stations.png", plot = graph_evo, width = 8, height = 5, bg = "white")


# GRAPHIQUE 2 : Parts de marché des opérateurs (Diagramme à barres)

# tab d'origine, on récupère les opérateurs
data_operateurs <- group_by(data, nom_operateur)

# Compte le nombre d'opérateur par station
data_operateurs <- summarise(data_operateurs, nombre = n())

# Tri par odre decroissant => avoir le plus grand 
data_operateurs <- arrange(data_operateurs, desc(nombre))

# Garde les 10 premiers
data_operateurs <- slice_head(data_operateurs, n = 10)


# ÉTAPE 2 : Création du graphique à barres

# Initialisation des axes avec x et y 
# reorder => prends le opérateurs mais les classes par la colonne nombre 
graph_parts <- ggplot(data_operateurs, aes(x = reorder(nom_operateur, -nombre), y = nombre)) +
  
  # Berre verticale verte
  geom_col(fill = "#009E73") + 
  
  # texte +titre
  labs(
    title = "Top 10 des opérateurs (Parts de marché en nb de stations)",
    x = "Opérateur",
    y = "Nombre de stations gérées"
  ) +
  
  # Style simple
  theme_minimal() +
  
  # Inclinaison à 45°pour faciliter la lecture
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Exportation du deuxième graphe
ggsave("2_parts_marche_operateurs.png", plot = graph_parts, width = 8, height = 5, bg = "white")

#-----------------------------------------------------------------------

data_sf <- data %>%
  mutate(
    consolidated_latitude  = as.numeric(consolidated_latitude),
    consolidated_longitude = as.numeric(consolidated_longitude)
  ) %>%
  #on prend que les lat et long qui existent
  filter(!is.na(consolidated_latitude), !is.na(consolidated_longitude)) %>%
  #on convertit le tableau en "Carte" (objet géographique)
  st_as_sf(coords = c("consolidated_longitude", "consolidated_latitude"), crs = 4326, remove = FALSE)

#telecharge le contour de la France + de la Corse
france_frontiere <- ne_countries(scale = "medium", country = "France", returnclass = "sf") %>%
  st_transform(crs = 4326) %>%
  st_crop(xmin = -10, ymin = 40, xmax = 15, ymax = 52)

#permet de trier les donnée et de garder celle qui sont dans le contour
data_metropole_propre <- st_intersection(data_sf, france_frontiere) %>%
  st_drop_geometry()