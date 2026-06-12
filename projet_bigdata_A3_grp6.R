#data <- read.csv("C:/Quentin/Ecole/ISEN/A3/IRVE.csv")
#data <- read.csv("C:/Users/hecto/Music/IRVE (1).csv", sep = ",", stringsAsFactors = FALSE)
data =read.csv("C:/Users/anael/Documents/Isen/IRVE.csv")

# install.packages("leaflet")
# install.packages("leaflet.extras")
# install.packages("sf")
# install.packages("rnaturalearth")
# install.packages("rnaturalearthdata")
install.packages("stringr") 
install.packages("dplyr")
install.packages("tidyverse")
install.packages("corrplot") 
library(tidyverse)
library(stringr)
library(dplyr)
library(corrplot)
library(data.table)
library(ggplot2)
library(tidyr)
library(lubridate)
library(nnet)
library(leaflet)
library(leaflet.extras)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(data.table)





# ------------------------------------------------------------------------------
# Fonctionnalité 1 Trie
# ------------------------------------------------------------------------------

#code: Anaelle - corrigé par ia , surtout comprendre les erreurs
#suppression de ligne
#https://doc.transport.data.gouv.fr/type-donnees/infrastructures-de-recharge-de-vehicules-electriques-irve/beta-base-nationale-irve-statique
data <- data[data[["id_pdc_itinerance"]] != "Non concerné", ]
#trier par la plus recente
data <- data[order(data[["date_maj"]], na.last = TRUE), ]
#arder uniquement la première de chaque id_pdc_itinerance, revoie false si ne la jamais renconter , true si la rencontrer= suppr
data <- data[!duplicated(data[["id_pdc_itinerance"]]), ]
data[["coordonneesXY"]]= NULL


# mots considérés non pertinents
mots<- c(
  "sans", "xx" ,"inconnu","aucune","pas de restriction","aucun","ras","^non$","sans restriction", "inconnue","Inconnu","Accessibilitˇ inconnu","Accessibilit\u008e inconnue","0000", "Accessibilit inconnue","Accessibilit\u0087 inconnue","AccessibilitĂ\u00a9 inconnue","Restriction de gabarit non précisée","restriction gabarit inconnue", "Non concerné","no information","Restriction de gabarit non pr\u008ecis\u008ee","Restriction de gabarit non prÃ©cisÃ©e","restriction gabarit inconnues","accessibilité inconnue","Accessibilité inconnue","Inconnue","NEANT","Néant","non concerné", "Non communiqué","non précisé","non renseigné","Non renseigné","unknown", "n/a", "na", "none", "null", "-", "?", "","/","Non communiqué","Non concerné ","aucune observations","aucune observation"
)

for (col in colnames(data)) {
  if (is.character(data[[col]])) {
    x <- str_trim(data[[col]])
    data[[col]][x %in% mots] = NA
  }
}


#code: Quentin - corrigé par ia 
for (i in 1:nrow(data)) {
  val =data[["restriction_gabarit"]][i]
  #si la val est NA on passe à la suivante
  if (!is.na(val)) {
    #on formate en "X.Xm"
    if (str_detect(val, "[0-9]")) {
      #remplace les virgules et ajoute un 'm' derrière le chiffre
      val_format <- str_replace_all(tolower(val), "([0-9]+)[,m]([0-9]+)", "\\1.\\2")
      valeur_num <- str_extract(val_format, "[0-9]+(\\.[0-9]+)?")
      #concaténation du chiffre et du m
      data[["restriction_gabarit"]][i] <- paste0(valeur_num, "m")
    }
  }
}

# Liste les valeurs uniques
#lapply(data, unique)


#code: Anaelle - corrigé par ia 
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


#code: Hector - aidé par ia, plot sans ia
#tarification 

extraire_prix_kwh <- function(texte) {
  if (is.na(texte) || texte == "") return(NA_real_)
  if (str_detect(texte, "inconnu|^nc$|non communiqu|^payant$|min(?:ute)?$|^http|fix")) return(NA_real_)
  if (str_detect(texte, "\\b0[.,]?0*\\s*(eur.*)?/?kwh")) return(0.0)
  
  nombres <- str_extract_all(texte, "[0-9]+[.,]?[0-9]*")[[1]]
  if (length(nombres) == 0) return(NA_real_)
  
  nums <- as.numeric(str_replace(nombres, ",", "."))
  if (str_detect(texte, "c(?:t|ts?|ents?)\\s*/?\\s*kwh")) nums <- nums / 100
  
  valides <- nums[nums >= 0.05 & nums <= 3.00]
  if (length(valides) == 0) return(NA_real_)
  
  return(round(mean(valides), 4))
}

# Nettoyage + normalisation
data[["tarification"]] <- data[["tarification"]] |>
  str_to_lower() |>
  str_replace_all("€", "eur") |>
  str_replace_all("kw h", "kwh") |>
  str_replace_all("kw\\b", "kwh") |>
  str_trim() |>
  sapply(extraire_prix_kwh, USE.NAMES = FALSE) 
#str_c(" €/kWh")
data[["gratuit"]]= NULL


#carte france
#on prend que les lat et long qui existent
data<- data[!is.na(data[["consolidated_latitude"]]) & !is.na(data[["consolidated_longitude"]]), ]

#on convertit le tableau en "Carte" (objet géographique)
data_sf <- st_as_sf(data,
                    coords = c("consolidated_longitude", "consolidated_latitude"),
                    crs = 4326,
                    remove = FALSE)

#telecharge le contour de la France + de la Corse
france_frontiere <- ne_countries(scale = "medium", country = "France", returnclass = "sf")
france_frontiere <- st_transform(france_frontiere, crs = 4326)
france_frontiere <- st_crop(france_frontiere, xmin = -10, ymin = 40, xmax = 15, ymax = 52)

#permet de trier les donnée et de garder celle qui sont dans le contour
data <- st_filter(data_sf, france_frontiere)
data <- st_drop_geometry(data)
#------------------------------------------------------------------------------

#affichage nbre_pdc

data_bar <- data |>
  filter(!is.na(nbre_pdc)) |>
  count(nbre_pdc) |>
  arrange(desc(n)) |>
  slice_head(n = 10)   #on garde seulement les 10 valeurs les plus fréquentes

stats_nbre_pdc <- data_bar |> #calcul moyenne et variance
  summarise(
    moyenne = mean(n),
    variance = var(n)
  )

print(stats_nbre_pdc)

#barplot horizontal
ggplot(data_bar, aes(x = reorder(factor(nbre_pdc), n), y = n)) +
  geom_col(fill = "lightgreen") +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Top 10 des valeurs de nombre de points de charge",
    x = "Nombre de points de charge (PDC)",
    y = "Nombre d'occurrences"
  ) +
  theme(legend.position = "none")

#--------------------------------------------------------------------------------
#affichage puissance nominale


#regroupement en classes pour voir toutes les données
data_pie <- data |>
  filter(!is.na(puissance_nominale)) |>
  mutate(
    classe = cut(
      puissance_nominale,
      breaks = c(0, 50, 150, 350, 1000, 5000, Inf),
      labels = c("0–50", "50–150", "150–350", "350–1000", "1000–5000", ">5000")
    )
  ) |>
  count(classe) |>
  mutate(
    pourcentage = n / sum(n) * 100,
    label = paste0(classe, " (", round(pourcentage, 1), "%)")
  )

stats_puissance <- data_pie |>
  summarise(
    moyenne = mean(n),
    variance = var(n)
  )

print(stats_puissance)

#camembert
ggplot(data_pie, aes(x = "", y = pourcentage, fill = classe)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  theme_void() +
  labs(
    title = "Répartition des puissances nominales (par classes)",
    fill = "Classe (kW)"
  ) +
  geom_text(
    aes(label = label),
    position = position_stack(vjust = 0.5),
    size = 4
  )

#------------------------------------------------------------------------------
#affichage implantation station



#nettoyage de la colonne implantation_station 
data <- data |>
  mutate(
    implantation_station = str_trim(implantation_station),
    implantation_station = case_when(
      # Valeurs vides ou inutilisables
      is.na(implantation_station) | implantation_station %in% c("", "/", "false", "x", "X") ~ NA_character_,
      
      # Corrections d'encodage
      str_detect(implantation_station, "priv") & str_detect(implantation_station, "public") ~ 
        "Parking privé à usage public",
      
      str_detect(implantation_station, "priv") & str_detect(implantation_station, "client") ~ 
        "Parking privé réservé à la clientèle",
      
      # Sinon on garde la valeur telle quelle
      TRUE ~ implantation_station
    )
  )

#préparation des données
data_bar <- data |>
  filter(!is.na(implantation_station)) |>   # exclure les NA
  count(implantation_station) |>            # compter chaque catégorie
  arrange(n)                                # tri par fréquence

#calcul de la moyenne et de la variance des fréquences 
stats_implantation <- data_bar |>
  summarise(
    moyenne = mean(n),
    variance = var(n)
  )


print(stats_implantation)

#barplot horizontal 
ggplot(data_bar, aes(x = reorder(implantation_station, n), y = n)) +
  geom_col(fill = "orange") +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Répartition des types d'implantation de station",
    x = "Type d'implantation",
    y = "Nombre d'occurrences"
  ) +
  theme(legend.position = "none")

#affichage tarification


#extraction de la colonne tarification brute
tarification <- data$tarification


# On crée la colonne numérique en copiant les données propres existantes
data$tarification_num <- data$tarification

# On ajoute l'unité (texte) à la colonne d'origine pour l'affichage
data$tarification <- str_c(data$tarification, " €/kWh")

# Calcul des statistiques (Ton code reste identique ici)
stats <- data %>%
  summarise(
    moyenne = mean(tarification_num, na.rm = TRUE),
    variance = var(tarification_num, na.rm = TRUE)
  )

print(stats)

#boxplot
# 1. On force la colonne à être reconnue comme des valeurs numériques
data$tarification_num <- as.numeric(data$tarification_num)

# 2. On lance l'histogramme
ggplot(
  data |> filter(!is.na(tarification_num)),
  aes(x = tarification_num)
) +
  geom_histogram(
    bins = 30,
    fill = "steelblue",
    color = "black",
    alpha = 0.7
  ) +
  theme_minimal() +
  labs(
    title = "Histogramme des tarifs (€/kWh)",
    x = "Tarification (€/kWh)",
    y = "Fréquence"
  )



# ---------------------------------------------------------------------------------
# fonction 2
# --------------------------------------------------------------------------------------------

#code: Quentin - corriger par ia 
# GRAPHIQUE 1 : Évolution du nombre de stations mises en service


# GRAPHIQUE 1 : Évolution du nombre de stations mises en service

# Nouvelle colonne pour isoler des variables
data_evolution <- mutate(
  data, 
  # Convertion texte -> format date
  date_service = ymd(date_mise_en_service), 
  # Force toutes les dates au premier jour de leur mois (pour grouper par mois)
  annee_mois = floor_date(date_service, "month")
)

data_evolution <- filter(data_evolution, year(date_service) >= 2010 & date_service <= Sys.Date())


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

# --- CORRECTION DU FILTRE ---
# 1. On force en texte, on passe en minuscules, et on enlève les espaces invisibles avec trimws()
formatC <- mutate(formatC, valeur_propre = trimws(tolower(as.character(est_present))))

# 2. On garde "true" (ou vrai, ou 1, pour anticiper tout problème de formature de R)
filtre <- filter(formatC, valeur_propre %in% c("true", "vrai", "1", "oui"))

# 3. On supprime les éventuelles valeurs manquantes (NA)
filtre <- drop_na(filtre, type_prise)
# ----------------------------

# On regroupe les données par "type_prise"
groupeD <- group_by(filtre, type_prise)

# On compte le nombre de lignes pour chaque groupe et on stocke ce total
comptage <- summarise(groupeD, quantite = n())

# On nettoie la colonne "type_prise" (on enlève le préfixe)
data_prises <- mutate(comptage, type_prise = gsub("prise_type_", "", type_prise))


# On initialise le graphique avec nos données finales
graph_prises <- ggplot(data_prises, aes(x = reorder(type_prise, -quantite), y = quantite)) +
  # diagramme orange
  geom_col(fill = "#E69F00") +
  # titre x y
  labs(title = "Répartition par type de prise", x = "Type de prise", y = "Quantité totale") +
  # thème simple
  theme_minimal() +
  # BONUS : Inclinaison à 45° pour éviter que les noms des prises ne se chevauchent !
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Exportation des données
ggsave("4_repartition_prises.png", plot = graph_prises, width = 8, height = 5, bg = "white")

print("Le graphique 4 a été généré avec succès !")





# ---------------------------------------------------------------------------------
# fonction 3
# --------------------------------------------------------------------------------------------

#code: Hector - aidé par ia
#heatmap et clustering

sf_use_s2(FALSE)

# 2. Conversion en GPS
data[["consolidated_latitude"]]  <- as.numeric(data[["consolidated_latitude"]])
data[["consolidated_longitude"]] <- as.numeric(data[["consolidated_longitude"]])
data <- data[!is.na(data[["consolidated_latitude"]]) & !is.na(data[["consolidated_longitude"]]), ]

data<- st_as_sf(data,coords = c("consolidated_longitude", "consolidated_latitude"),crs = 4326,remove = FALSE)

# 3. Télécharge le contour, le REND VALIDE (anti-bug), puis le découpe sur la métropole
france_frontiere = ne_countries(scale = "medium", country = "France", returnclass = "sf")
france_frontiere = st_transform(france_frontiere, crs = 4326)
france_frontiere = st_make_valid(france_frontiere)
france_frontiere = st_crop(france_frontiere, st_bbox(c(xmin = -10, ymin = 40, xmax = 15, ymax = 52), crs = 4326))

# 4. Tri des données (conserve uniquement ce qui intersecte la France)
data= st_filter(data_sf, france_frontiere)
data_metropole_propre = st_drop_geometry(data_sf)

# 5. Création de la carte Heatmap
carte_heatmap = leaflet(data_metropole_propre)
carte_heatmap = addTiles(carte_heatmap)
carte_heatmap = setView(carte_heatmap, lng = 2.2137, lat = 46.2276, zoom = 6) 

# Trace le contour de la France
carte_heatmap <- addPolygons(carte_heatmap,data = france_frontiere,fill = FALSE,color = "#2c3e50",weight = 2,opacity = 1)
  
# Couche de chaleur
carte_heatmap <- addHeatmap(carte_heatmap,lng = ~consolidated_longitude,lat = ~consolidated_latitude,blur = 18,max = 0.08,radius = 12)
    
  
# Marqueurs regroupés (clusters)
carte_heatmap <- addMarkers(carte_heatmap,lng = ~consolidated_longitude,lat = ~consolidated_latitude, clusterOptions = markerClusterOptions())
    

# 6. Affichage final
carte_heatmap



# ---------------------------------------------------------------------------------
# fonction 4 Bivariée
# --------------------------------------------------------------------------------------------

#code: Anaelle - fait a la main
#2 variables numériques = cor.test(spearman, ne suit pas une loi normal)
#1 quali (2 groupes) vs 1 numérique =wilcox.test
#1 quali (3+ groupes) vs 1 numérique =kruskal.test
#2 variables qualitatives =chisq.test + cramer_v


#data sans vide
data_cor <- data[!is.na(data[["consolidated_latitude"]])  &
                   !is.na(data[["consolidated_longitude"]]) &
                   !is.na(data[["nbre_pdc"]])&
                   !is.na(data[["puissance_nominale"]])&
                   !is.na(data[["implantation_station"]]) &
                   !is.na(data[["raccordement"]])  &
                   !is.na(data[["date_mise_en_service"]]) &
                   !is.na(data[["tarification"]]) , ]
data_cor[["annee"]]        <- as.numeric(format(as.Date(data_cor[["date_mise_en_service"]]), "%Y"))
data_cor[["charge_rapide"]] <- as.numeric(data_cor[["puissance_nominale"]] > 22)

cor.test(data_cor[["consolidated_latitude"]],  data_cor[["nbre_pdc"]])
cor.test(data_cor[["consolidated_longitude"]], data_cor[["nbre_pdc"]])
cor.test(data_cor[["consolidated_latitude"]],  data_cor[["puissance_nominale"]])
cor.test(data_cor[["consolidated_longitude"]], data_cor[["puissance_nominale"]])
cor.test(data_cor[["consolidated_longitude"]], data_cor[["puissance_nominale"]])

#variable quanti
#code: Anaelle - expliquer par mathis et cyriac
# PUISSANCE VS PRIX tarification 
cor.test(data_cor[["tarification"]], data_cor[["puissance_nominale"]], method = "spearman")
#visu
ggplot(data_cor, aes(x = as.factor(charge_rapide), y = tarification)) +
  geom_boxplot(fill = c("lightblue", "pink")) +
  labs(title = "Puissance vs Tarification",
       x = "Charge rapide (0=Non,1=Oui)", y = "Tarification")

# LOCA {(xy)} VS EQUIPEMENT nbre_pdc

cor.test(data_cor[["consolidated_latitude"]],  data_cor[["nbre_pdc"]])
cor.test(data_cor[["consolidated_longitude"]], data_cor[["nbre_pdc"]])
cor.test(data_cor[["consolidated_latitude"]],  data_cor[["puissance_nominale"]])
cor.test(data_cor[["consolidated_longitude"]], data_cor[["puissance_nominale"]])

ggplot(data_cor, aes(x = consolidated_longitude, y = consolidated_latitude, color = nbre_pdc)) +
  geom_point(alpha = 0.5) +
  scale_color_gradient(low = "blue", high = "red") +
  labs(title = "Localisation vs Nombre de points de charge",
       x = "Longitude", y = "Latitude", color = "Nombre de PDC")



#IMPLANTATION vs nb pc

# Discrétisation nbre_pdc pour chi2 + mosaicplot
data_cor[["taille_station"]] <- cut(data_cor[["nbre_pdc"]],breaks = c(0, 1, 4, Inf),labels = c("Petite (1 PDC)", "Moyenne (2-4 PDC)", "Grande (5+ PDC)"))

data_cor[["implantation_simple"]] <- data_cor[["implantation_station"]]
data_cor[["implantation_simple"]][data_cor[["implantation_station"]] == "Parking privé réservé à la clientèle"] <- "Autre"
data_cor[["implantation_simple"]][data_cor[["implantation_station"]] == "Station dédiée à la recharge rapide"] <- "Autre"

tab2 <- table(data_cor[["implantation_simple"]], data_cor[["taille_station"]])
print(tab2)
chisq.test(tab2)

# V de Cramer : intensité de l'association, le chi2 dit juste y a  un lien  (p-value < 0.05  =oui il y a un lien p-value > 0.05  -> pas de lien significatif), V de Cramér= ce lien est fort ou faible ? 
cat("V de Cramér :", round(sqrt(chisq.test(tab2)$statistic / (sum(tab2) * (min(nrow(tab2), ncol(tab2)) - 1))), 3), "\n")


ggplot(data_cor, aes(x = implantation_station, y = nbre_pdc)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Implantation vs Nb points de charge",
       x = "Implantation", y = "Nb de PDC") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# IMPLANTATION VS PUISSANCE
# Kruskal-Wallis ( non paramétrique)
kruskal.test(puissance_nominale ~ implantation_station, data = data_cor)

ggplot(data_cor, aes(x = implantation_station, y = puissance_nominale)) +stat_summary(fun = mean, geom = "point", size = 4, color = "blue") +stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2, color = "red") + labs(title = "Implantation vs Puissance (moyenne ± IC)", x = "Implantation", y = "Puissance (kW)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# RACCORDEMENT VS PUISSANCE

# Wilcoxon (, non paramétrique )
wilcox.test(puissance_nominale ~ raccordement, data = data_cor)

ggplot(data_cor, aes(x = raccordement, y = puissance_nominale)) +
  geom_bar(stat = "summary", fun = "mean", fill = c("lightblue", "pink")) +
  labs(title = "Raccordement vs Puissance moyenne",
       x = "Raccordement", y = "Puissance moyenne ")

#implantation vs tarif
kruskal.test(tarification ~ implantation_station, data = data_cor)

ggplot(data_cor[!is.na(data_cor[["implantation_station"]]) & !is.na(data_cor[["tarification"]]), ],
       aes(x = implantation_station, y = tarification)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Implantation vs Tarification",
       x = "Implantation", y = "Tarification (€/kWh)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#raccordement vs tarif
wilcox.test(tarification ~ raccordement, data = data_cor)

ggplot(data_cor, aes(x = raccordement, y = tarification)) +
  geom_boxplot(fill = c("lightblue", "pink")) +
  labs(title = "Raccordement vs Tarification",
       x = "Raccordement", y = "Tarification")

#annee vs nb station
ggplot(data_cor, aes(x = annee)) +
  geom_bar(fill = "lightblue") +
  labs(title = "Nombre de stations mises en service par année",
       x = "Annee", y = "Nombre de stations") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#variable quali : chi2 + mosaicplot
#implantation vs raccordement
tab3 <- table(data_cor[["implantation_station"]], data_cor[["raccordement"]])
print(tab3)
chisq.test(tab3)
cat("V de Cramér :", round(sqrt(chisq.test(tab3)$statistic / (sum(tab3) * (min(nrow(tab3), ncol(tab3)) - 1))), 3), "\n")
mosaicplot(tab3, main = "Implantation vs Raccordement",color = c("lightblue", "pink"), las = 2)

mosaicplot(tab2, main = "Implantation vs Taille de station", color = c("blue", "violet", "pink"), las = 2)


#code: Anaelle - corrigé par ia , erreur recurente 
#matrice
data_matrice <- data_cor[, c("puissance_nominale", "nbre_pdc", "tarification","consolidated_latitude", "consolidated_longitude","charge_rapide", "annee")]
matrice_cor <- cor(data_matrice, method = "spearman")
corrplot(matrice_cor, method = "color", type = "full",addCoef.col = "black", tl.col = "black", tl.srt = 45,tl.cex = 0.7, title = "Matrice de corrélation", mar = c(0, 0, 2, 0))


#---------------------------------------------------------------------
# FONCTIONNALITÉ 5 : RÉGRESSIONS ET PRÉDICTIONS
#--------------------------------------------------------------------
#code: quentin +gemini 
# Chargement des librairies nécessaires
# install.packages("nnet")
# install.packages("caret")
library(nnet)
library(caret)

# -------------------------------------------------------------------
# 1. CRÉATION DE LA VARIABLE "charge_rapide"
# -------------------------------------------------------------------
# y'a des risque ATTENTION BIAIS : Créer cette variable à partir de la puissance nominale 
# puis l'utiliser pour prédire cette même puissance nominale (Modèle 1) 
# va artificiellement gonfler la performance de la régression.
data$charge_rapide <- ifelse(data$puissance_nominale >= 50, 1, 0)

# -------------------------------------------------------------------
# 2. MODÈLE 1 : RÉGRESSION LINÉAIRE MULTIPLE (Puissance Nominale)
# -------------------------------------------------------------------
modele_lineaire <- lm(puissance_nominale ~ charge_rapide + nbre_pdc, data = data)

# Affichage des coefficients et de l'efficacité du modèle linéaire
print("--- Résumé de la régression linéaire ---")
summary(modele_lineaire)

# -------------------------------------------------------------------
# 3. SÉCURISATION ET PRÉPARATION DE LA VARIABLE "tarification"
# -------------------------------------------------------------------

# --- Étape A : Nettoyage et conversion numérique ---
# Gestion du format européen (virgule décimale) et suppression des unités (€/kWh)
tarification_num <- data$tarification |>
  gsub(pattern = ",", replacement = ".", x = _) |>
  gsub(pattern = "[^0-9.]", replacement = "", x = _) |>
  as.numeric()

# --- Étape B : Discrétisation en 3 tiers (bas / modéré / élevé) ---
# On utilise les terciles pour un découpage équilibré
n_valides <- sum(!is.na(tarification_num))

if (n_valides > 0) {
  
  seuils <- quantile(tarification_num, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE)
  seuils_uniques <- unique(seuils)
  n_groupes <- length(seuils_uniques) - 1
  
  if (n_groupes < 2) {
    stop("Impossible de créer au moins 2 groupes de tarification : vérifier la variance des données.")
  }
  
  labels_groupes <- c("bas", "modéré", "élevé")[seq_len(n_groupes)]
  
  data$tarification_groupe <- cut(
    tarification_num,
    breaks       = seuils_uniques,
    labels       = labels_groupes,
    include.lowest = TRUE
  )
  
} else {
  # Fallback : la colonne ne contient pas de prix numériques
  warning("Aucune valeur numérique extraite de 'tarification'. Conversion directe en facteur.")
  data$tarification_groupe <- as.factor(data$tarification)
}

cat("\n=== DISTRIBUTION DES GROUPES DE TARIFICATION ===\n")
print(table(data$tarification_groupe, useNA = "ifany"))
# -------------------------------------------------------------------
# 4. MODÈLE 2 : RÉGRESSION LOGISTIQUE MULTINOMIALE (Tarification)
# -------------------------------------------------------------------

data_logistique <- subset(data, !is.na(tarification_groupe))
data_logistique$tarification_groupe <- droplevels(data_logistique$tarification_groupe)

# Entraînement du modèle
modele_logistique <- multinom(
  tarification_groupe ~ puissance_nominale + nbre_pdc,
  data  = data_logistique,
  trace = FALSE   # supprime les messages d'itération de nnet
)

cat("\n=== RÉSUMÉ : RÉGRESSION LOGISTIQUE MULTINOMIALE ===\n")
print(summary(modele_logistique))
# -------------------------------------------------------------------
# 5. ÉVALUATION DU MODÈLE : MATRICE DE CONFUSION
# -------------------------------------------------------------------

# Générer les prédictions (la catégorie la plus probable)
predictions_logistique <- predict(modele_logistique, newdata = data_logistique)

# S'assurer que les deux éléments sont des facteurs avec les mêmes niveaux
predictions_logistique <- factor(predictions_logistique, levels = levels(data_logistique$tarification_groupe))

# Matrice de confusion détaillée avec 'caret'
print("--- Matrice de confusion détaillée (caret) ---")
matrice_detaillee <- confusionMatrix(data = predictions_logistique, reference = data_logistique$tarification_groupe)
print(matrice_detaillee)

# -------------------------------------------------------------------
# 5. BIS : VISUALISATION DU MODÈLE LOGISTIQUE
# -------------------------------------------------------------------


# 1. Créer un jeu de données fictif pour la prédiction
nouvelles_donnees <- data.frame(
  puissance_nominale = seq(min(data_logistique$puissance_nominale, na.rm = TRUE), 
                           max(data_logistique$puissance_nominale, na.rm = TRUE), 
                           length.out = 100),
  nbre_pdc = median(data_logistique$nbre_pdc, na.rm = TRUE)
)

# 2. Calculer les probabilités prédites par le modèle
probabilites <- predict(modele_logistique, newdata = nouvelles_donnees, type = "probs")

# 3. Fusionner et restructurer les données pour le graphique
donnees_graphique <- cbind(nouvelles_donnees, probabilites)
donnees_long <- pivot_longer(donnees_graphique, 
                             cols = c("bas", "modéré", "élevé"), 
                             names_to = "Tarification", 
                             values_to = "Probabilite")

# 4. Créer et afficher le graphique
graphique_logistique <- ggplot(donnees_long, aes(x = puissance_nominale, y = Probabilite, color = Tarification)) +
  geom_line(linewidth = 1) +
  scale_color_manual(values = c("bas" = "#00BFC4", "modéré" = "#F8766D", "élevé" = "#C77CFF")) +
  labs(title = "Évolution de la tarification selon la puissance nominale",
       subtitle = "Prédictions du modèle logistique (Nombre de PDC fixé à la médiane)",
       x = "Puissance Nominale",
       y = "Probabilité prédite",
       color = "Groupe de tarification") +
  theme_minimal()

print(graphique_logistique)

# -------------------------------------------------------------------
# 6. EXPORT DES DONNÉES
# -------------------------------------------------------------------

# Exporter le dataframe finalisé en format CSV
write.csv(data, file = "donnees_nettoyees.csv", row.names = FALSE)
write.csv2(data, file = "donnees_nettoyees_excel.csv", row.names = FALSE, fileEncoding = "latin1")