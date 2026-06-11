data <- read.csv("C:/Quentin/Ecole/ISEN/A3/IRVE.csv")

# install.packages("leaflet")
# install.packages("leaflet.extras")
# install.packages("sf")
# install.packages("rnaturalearth")
# install.packages("rnaturalearthdata")
library(leaflet)
library(leaflet.extras)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(data.table)

install.packages("stringr") 
install.packages("dplyr")
install.packages("tidyverse")
install.packages("corrplot") 
library(tidyverse)
library(stringr)
library(dplyr)
library(corrplot)


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
  "sans", "xx" ,"inconnu", "inconnue","Inconnu","Accessibilitˇ inconnu","Accessibilit\u008e inconnue","0000", "Accessibilit inconnue","Accessibilit\u0087 inconnue","AccessibilitĂ\u00a9 inconnue","Restriction de gabarit non précisée","restriction gabarit inconnue", "Non concerné","no information","Restriction de gabarit non pr\u008ecis\u008ee","Restriction de gabarit non prÃ©cisÃ©e","restriction gabarit inconnues","accessibilité inconnue","Accessibilité inconnue","Inconnue","NEANT","Néant","non concerné", "Non communiqué","non précisé","non renseigné","Non renseigné","unknown", "n/a", "na", "none", "null", "-", "?", "","/","Non communiqué","Non concerné ","aucune observations","aucune observation"
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
#https://www.bppulse.com/fr-fr/centre-de-contenu/tout-savoir-sur-la-charge-rapide
#https://www.automobile-propre.com/dossiers/borne-de-recharge-electrique-le-guide-complet-2024/
data[["puissance_nominale"]][data[["puissance_nominale"]] < 1]= NA
data[["puissance_nominale"]][data[["puissance_nominale"]] >400 ] = NA




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


# Création du graphique à barres

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

# Chargement de la librairie requise (à exécuter si ce n'est pas déjà fait plus haut)
library(ggplot2)


# GRAPHIQUE 3 : Répartition des puissances nominales

# On vérifie que la colonne est au format numérique
data$puissance_nominale <- as.numeric(data$puissance_nominale)

# Création de l'histogramme
graph_puissance <- ggplot(data, aes(x = puissance_nominale)) +
  # Barre violette
  geom_histogram(binwidth = 25, fill = "#CC79A7", color = "white") +
  
  # Titres + légende
  labs(
    title = "Répartition des puissances nominales",
    x = "Puissance Nominale (kW)",
    y = "Nombre de points de charge"
  ) +
  
  # thème simple
  theme_minimal()

# Exportation
ggsave("3_histogramme_puissance.png", plot = graph_puissance, width = 8, height = 5, bg = "white")



# GRAPHIQUE 4 : Répartition des types de prises (Diagramme à barres)

# On sélectionne uniquement les colonnes "prise_type_..." dans le tab d'origine
selectC <- select(data, starts_with("prise_type_"))

# On transforme plusieurs colonnes en deux
formatC <- pivot_longer(selectC, cols = everything(), names_to = "type_prise", values_to = "est_present")

# On garder les lignes où la prise est présente 
filtre <- filter(formatC, tolower(as.character(est_present)) == "true")

# On regroupe les données par "type_prise"
groupeD <- group_by(filtre, type_prise)

# On compte le nombre de lignes pour chaque groupe et on stocke ce total dans une nouvelle colonne "quantite"
comptage <- summarise(groupeD, quantite = n())

# On nettoie la colonne "type_prise" 
data_prises <- mutate(comptage, type_prise = gsub("prise_type_", "", type_prise))


# On initialise le graphique avec nos données finales, en triant l'axe X du plus grand au plus petit (-quantite)
graph_prises <- ggplot(data_prises, aes(x = reorder(type_prise, -quantite), y = quantite)) +
  # diagramme orange
  geom_col(fill = "#E69F00") +
  # titre x y
  labs(title = "Répartition par type de prise", x = "Type de prise", y = "Quantité totale") +
  # thème simple
  theme_minimal()

# Exportation des données
ggsave("4_repartition_prises.png", plot = graph_prises, width = 8, height = 5, bg = "white")

print("Les 4 graphiques ont été générés et sauvegardés en .png dans votre dossier de travail !")

#---------------------------------------------------------------------------
#tarification 

library(stringr)
library(data.table)

#chargement de tarification et [[1]] permet de convertir directement le data.frame en un vecteur simple
tarification <- fread("C:/Users/hecto/Music/IRVE (1).csv", select = "tarification", data.table = FALSE)[[1]]


#extration des données, si gratuit -> 0 etc 
extraire_prix_kwh <- function(texte) {
  if (is.na(texte) || texte == "") return(NA_real_)
  
  # Rejets précoces
  if (str_detect(texte, "inconnu|^nc$|non communiqu|^payant$|min(?:ute)?$|^http|fix")) {
    return(NA_real_)
  }
  
  # Cas de la gratuité explicite (0 EUR/kWh)
  if (str_detect(texte, "\\b0[.,]?0*\\s*(eur.*)?/?kwh")) return(0.0)
  
  # Extraction des nombres
  nombres <- str_extract_all(texte, "[0-9]+[.,]?[0-9]*")[[1]]
  if (length(nombres) == 0) return(NA_real_)
  
  nums <- as.numeric(str_replace(nombres, ",", "."))
  
  # Conversion centimes -> euros
  if (str_detect(texte, "c(?:t|ts?|ents?)\\s*/?\\s*kwh")) {
    nums <- nums / 100
  }
  
  # Filtrage des valeurs aberrantes (0.05€ à 3.00€ / kWh)
  valides <- nums[nums >= 0.05 & nums <= 3.00]
  if (length(valides) == 0) return(NA_real_)
  
  return(round(mean(valides), 4))
}


# 3. APPLICATION DU NETTOYAGE ET DE LA NORMALISATION
data$tarification <- tarification |> 
  str_to_lower() |> 
  str_replace_all("€", "eur") |> 
  str_replace_all("kw h", "kwh") |> 
  str_replace_all("kw\\b", "kwh") |> 
  str_trim() |> 
  sapply(extraire_prix_kwh, USE.NAMES = FALSE) |> 
  str_c(" €/kWh")



#-------------------------------------------------------------------------------------------
#Bivariée

df_cor <- data[!is.na(data[["consolidated_latitude"]])  &
                 !is.na(data[["consolidated_longitude"]]) &
                 !is.na(data[["nbre_pdc"]])               &
                 !is.na(data[["puissance_nominale"]])      &
                 !is.na(data[["implantation_station"]])    &
                 !is.na(data[["raccordement"]])            &
                 !is.na(data[["date_mise_en_service"]])            &
                 !is.na(data[["tarification"]])            , ]
df_cor[["annee"]]        <- as.numeric(format(as.Date(df_cor[["date_mise_en_service"]]), "%Y"))
df_cor[["charge_rapide"]] <- as.numeric(df_cor[["puissance_nominale"]] > 22)

cor.test(df_cor[["consolidated_latitude"]],  df_cor[["nbre_pdc"]])
cor.test(df_cor[["consolidated_longitude"]], df_cor[["nbre_pdc"]])
cor.test(df_cor[["consolidated_latitude"]],  df_cor[["puissance_nominale"]])
cor.test(df_cor[["consolidated_longitude"]], df_cor[["puissance_nominale"]])
cor.test(df_cor[["consolidated_longitude"]], df_cor[["puissance_nominale"]])


# PUISSANCE VS PRIX tarification 
cor.test(df_cor[["tarification"]], df_cor[["puissance_nominale"]], method = "spearman")

# Visualisation
ggplot(df_cor, aes(x = as.factor(charge_rapide), y = tarification)) +
  geom_boxplot(fill = c("lightblue", "pink")) +
  labs(title = "Puissance vs Tarification",
       x = "Charge rapide (0=Non, 1=Oui)", y = "Tarification (€/kWh)")

# LOCA {(xy)} VS EQUIPEMENT nbre_pdc

cor.test(df_cor[["consolidated_latitude"]],  df_cor[["nbre_pdc"]])
cor.test(df_cor[["consolidated_longitude"]], df_cor[["nbre_pdc"]])
cor.test(df_cor[["consolidated_latitude"]],  df_cor[["puissance_nominale"]])
cor.test(df_cor[["consolidated_longitude"]], df_cor[["puissance_nominale"]])

ggplot(df_cor, aes(x = consolidated_longitude, y = consolidated_latitude, color = nbre_pdc)) +
  geom_point(alpha = 0.5) +
  scale_color_gradient(low = "blue", high = "red") +
  labs(title = "Localisation vs Nombre de points de charge",
       x = "Longitude", y = "Latitude", color = "Nombre de PDC")



#IMPLANTATION vs NOMBRE DE PDC


# Moyenne par groupe
for (type in unique(df_cor[["implantation_station"]])) {
  lignes <- df_cor[df_cor[["implantation_station"]] == type, ]
  cat(type, "-> moyenne PDC:", round(mean(lignes[["nbre_pdc"]]), 1), "\n")
}

# Discrétisation nbre_pdc pour chi2 + mosaicplot
df_cor[["taille_station"]] <- cut(df_cor[["nbre_pdc"]],
                                  breaks = c(0, 1, 4, Inf),
                                  labels = c("Petite (1 PDC)", "Moyenne (2-4 PDC)", "Grande (5+ PDC)"))

df_cor[["implantation_simple"]] <- df_cor[["implantation_station"]]
df_cor[["implantation_simple"]][df_cor[["implantation_station"]] == "Parking privé réservé à la clientèle"] <- "Autre"
df_cor[["implantation_simple"]][df_cor[["implantation_station"]] == "Station dédiée à la recharge rapide"] <- "Autre"

tab2 <- table(df_cor[["implantation_simple"]], df_cor[["taille_station"]])
print(tab2)
chisq.test(tab2)

# V de Cramer : intensité de l'association, le chi2 dit juste y a  un lien ? (oui/non via p-value) V de Cramér= ce lien est fort ou faible ? 
cramer_v <- function(tbl) {
  chi2 <- chisq.test(tbl)$statistic
  n    <- sum(tbl)
  k    <- min(nrow(tbl), ncol(tbl))
  sqrt(chi2 / (n * (k - 1)))
}
cat("V de Cramér :", round(cramer_v(tab2), 3), "\n")


ggplot(df_cor, aes(x = implantation_station, y = nbre_pdc)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Implantation vs Nb points de charge",
       x = "Implantation", y = "Nb de PDC") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# 4. IMPLANTATION vs PUISSANCE

# Moyenne par groupe
for (type in unique(df_cor[["implantation_station"]])) {
  lignes <- df_cor[df_cor[["implantation_station"]] == type, ]
  cat(type, "-> puissance moyenne:", round(mean(lignes[["puissance_nominale"]]), 1), "kW\n")
}

# Kruskal-Wallis (> 2 groupes, non paramétrique)
kruskal.test(puissance_nominale ~ implantation_station, data = df_cor)

ggplot(df_cor, aes(x = implantation_station, y = puissance_nominale)) +
  stat_summary(fun = mean, geom = "point", size = 4, color = "blue") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2, color = "red") +
  labs(title = "Implantation vs Puissance (moyenne ± IC)",
       x = "Implantation", y = "Puissance (kW)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# RACCORDEMENT vs PUISSANCE


# Wilcoxon (2 groupes, non paramétrique )
wilcox.test(puissance_nominale ~ raccordement, data = df_cor)


ggplot(df_cor, aes(x = raccordement, y = puissance_nominale)) +
  geom_bar(stat = "summary", fun = "mean", fill = c("lightblue", "pink")) +
  labs(title = "Raccordement vs Puissance moyenne",
       x = "Raccordement", y = "Puissance moyenne (kW)")


# ── implantation vs tarification
kruskal.test(tarification ~ implantation_station, data = df_cor)

ggplot(df_cor[!is.na(df_cor[["implantation_station"]]) & !is.na(df_cor[["tarification"]]), ],
       aes(x = implantation_station, y = tarification)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Implantation vs Tarification",
       x = "Implantation", y = "Tarification (€/kWh)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# ── raccordement vs tarification
wilcox.test(tarification ~ raccordement, data = df_cor)

ggplot(df_cor, aes(x = raccordement, y = tarification)) +
  geom_boxplot(fill = c("lightblue", "pink")) +
  labs(title = "Raccordement vs Tarification",
       x = "Raccordement", y = "Tarification (€/kWh)")


# 6. VARIABLES QUALITATIVES : chi2 + mosaicplot


# ── implantation vs raccordement
tab3 <- table(df_cor[["implantation_station"]], df_cor[["raccordement"]])
print(tab3)
chisq.test(tab3)
cat("V de Cramér :", round(cramer_v(tab3), 3), "\n")
mosaicplot(tab3, main = "Implantation vs Raccordement",
           color = c("lightblue", "pink"), las = 2)

mosaicplot(tab2, main = "Implantation vs Taille de station",
           color = c("blue", "violet", "pink"), las = 2)

ggplot(df_cor, aes(x = annee)) +
  geom_bar(fill = "lightblue") +
  labs(title = "Nombre de stations mises en service par année",
       x = "Année", y = "Nombre de stations") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


df_matrice <- df_cor[, c("puissance_nominale", "nbre_pdc", "tarification",
                         "consolidated_latitude", "consolidated_longitude",
                         "charge_rapide", "annee")]

matrice_cor <- cor(df_matrice, method = "spearman")

corrplot(matrice_cor, method = "color", type = "full",
         addCoef.col = "black", tl.col = "black", tl.srt = 45,
         tl.cex = 0.7,
         title = "Matrice de corrélation", mar = c(0, 0, 2, 0))


#----------------------------------------------------------------------------------------------------------------------------------------------------





