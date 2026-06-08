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

# ── 2. NORMALISER les booléens en minuscules (false/true)
bool_cols <- c("prise_type_ef", "prise_type_2", "gratuit", "reservation")

for (col in bool_cols) {
  x <- str_trim(str_to_lower(as.character(df[[col]])))
  df[[col]] <- case_when(
    x == "true"  ~ "true",
    x == "false" ~ "false",
    .default = NA
  )
}
