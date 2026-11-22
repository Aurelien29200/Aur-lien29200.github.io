# =============================================================================
# 01_collecte_comtrade.R
# Étape 2 du cycle : collecte des données (UN Comtrade, données « miroirs »)
#
# Objectif : récupérer, pour 2015-2025, le commerce de chaque pays déclarant
# avec l'Afghanistan (code partenaire M49 = 4), tous produits confondus
# (cmdCode = TOTAL), dans les deux sens.
#
# Pourquoi des données miroirs ? L'Afghanistan ne transmet plus de déclarations
# régulières à UN Comtrade depuis 2021. On reconstitue donc son commerce à partir
# de ce que déclarent ses partenaires :
#   - exportations du pays déclarant vers l'Afghanistan (flowCode X)
#       = importations afghanes en provenance de ce pays ;
#   - importations du pays déclarant depuis l'Afghanistan (flowCode M)
#       = exportations afghanes vers ce pays.
#
# Point d'accès utilisé : l'API publique « preview » de UN Comtrade
# (sans clé, limitée à 500 lignes par requête et à un débit modeste).
# Une requête par année et par flux suffit (environ 80 à 140 déclarants).
#
# Dépendances : R >= 4.1, jsonlite, dplyr, readr.
# Remarque : ce script a servi à la collecte du 21 novembre 2026 ; les valeurs
# de UN Comtrade sont révisées dans le temps, une nouvelle exécution peut donc
# donner des chiffres légèrement différents (surtout pour 2024-2025).
# =============================================================================

library(jsonlite)
library(dplyr)
library(readr)

url_base <- "https://comtradeapi.un.org/public/v1/preview/C/A/HS"

# Une requête : une année, un flux ; on réessaie si l'API renvoie 429 (débit).
interroger_comtrade <- function(annee, flux, essais = 8) {
  requete <- paste0(
    url_base,
    "?period=", annee,
    "&partnerCode=4",          # Afghanistan comme partenaire
    "&flowCode=", flux,        # X = exportations, M = importations du déclarant
    "&cmdCode=TOTAL",          # tous produits
    "&customsCode=C00&motCode=0&partner2Code=0"  # totaux (tous régimes, tous modes)
  )
  for (i in seq_len(essais)) {
    rep <- tryCatch(fromJSON(requete), error = function(e) NULL)
    if (!is.null(rep) && !is.null(rep$data) && length(rep$data) > 0) {
      return(tibble(
        annee           = rep$data$refYear,
        code_declarant  = rep$data$reporterCode,
        flux_declarant  = rep$data$flowCode,
        valeur_usd      = rep$data$primaryValue,
        nomenclature_hs = rep$data$classificationCode
      ))
    }
    Sys.sleep(2 * i)  # attente croissante avant de réessayer
  }
  warning("Aucune donnée pour ", annee, " / ", flux)
  NULL
}

grille <- expand.grid(annee = 2015:2025, flux = c("X", "M"),
                      stringsAsFactors = FALSE)

brut <- bind_rows(Map(interroger_comtrade, grille$annee, grille$flux))

# Fichier complet (tous les déclarants)
write_csv(brut, "data/comtrade_miroir_afghanistan_2015_2025_complet.csv")

# Fichier allégé utilisé par l'analyse : on garde les 21 principaux partenaires
# (les 20 premiers en valeur cumulée + les voisins régionaux) et on regroupe le
# reste sous « OTH ».
cles <- brut |>
  group_by(code_declarant) |>
  summarise(total = sum(valeur_usd), .groups = "drop") |>
  slice_max(total, n = 20) |>
  pull(code_declarant)
cles <- union(cles, c(364, 586, 156, 860, 762, 398, 417, 699, 784, 792, 643))

brut |>
  mutate(code_declarant = if_else(code_declarant %in% cles,
                                  as.character(code_declarant), "OTH")) |>
  group_by(annee, code_declarant, flux_declarant) |>
  summarise(valeur_usd = round(sum(valeur_usd)), .groups = "drop") |>
  write_csv("data/comtrade_miroir_afghanistan_2015_2025.csv")
