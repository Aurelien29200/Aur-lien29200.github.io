# =============================================================================
# 02_analyse_visualisation.R
# Étape 3 du cycle : analyse et visualisation (R, tidyverse, ggplot2)
#
# Entrée  : data/comtrade_miroir_afghanistan_2015_2025.csv
# Sorties : data/commerce_afghan_par_groupe.csv, data/indicateurs_annuels.csv
#           images/fig1_commerce_par_groupe.png
#           images/fig2_parts_partenaires.png
#           images/fig3_concentration_hhi.png
#           images/fig4_panel_constant.png
#
# À exécuter depuis le dossier projet_session (le .Rmd le fait automatiquement).
# Dépendances : R >= 4.1, dplyr, tidyr, readr, ggplot2, scales.
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(ggplot2)
  library(scales)
})

# -----------------------------------------------------------------------------
# 1. Lecture et recodage
# -----------------------------------------------------------------------------
brut <- read_csv("data/comtrade_miroir_afghanistan_2015_2025.csv",
                 col_types = cols(code_declarant = col_character()))

# Table de correspondance codes M49 -> noms et groupes analytiques
pays <- tribble(
  ~code_declarant, ~pays,                  ~groupe,
  "586",           "Pakistan",             "Pakistan",
  "364",           "Iran",                 "Iran",
  "860",           "Ouzbékistan",          "Asie centrale",
  "398",           "Kazakhstan",           "Asie centrale",
  "762",           "Tadjikistan",          "Asie centrale",
  "417",           "Kirghizstan",          "Asie centrale",
  "156",           "Chine",                "Chine",
  "699",           "Inde",                 "Inde",
  "784",           "Émirats arabes unis",  "Émirats arabes unis",
  "842",           "États-Unis",           "Occident et OCDE",
  "276",           "Allemagne",            "Occident et OCDE",
  "251",           "France",               "Occident et OCDE",
  "528",           "Pays-Bas",             "Occident et OCDE",
  "56",            "Belgique",             "Occident et OCDE",
  "392",           "Japon",                "Occident et OCDE",
  "410",           "Corée du Sud",         "Occident et OCDE",
  "792",           "Turquie",              "Autres",
  "458",           "Malaisie",             "Autres",
  "643",           "Russie",               "Autres",
  "404",           "Kenya",                "Autres",
  "31",            "Azerbaïdjan",          "Autres",
  "OTH",           "Autres déclarants",    "Autres"
)

ordre_groupes <- c("Pakistan", "Iran", "Asie centrale", "Chine", "Inde",
                   "Émirats arabes unis", "Occident et OCDE", "Autres")

# Inversion miroir : X du déclarant = importation afghane ; M = exportation afghane
commerce <- brut |>
  left_join(pays, by = "code_declarant") |>
  mutate(
    flux_afghan = if_else(flux_declarant == "X", "Importations afghanes",
                                                 "Exportations afghanes"),
    groupe = factor(groupe, levels = ordre_groupes)
  )

stopifnot(!any(is.na(commerce$groupe)))  # tous les codes sont bien recodés

# -----------------------------------------------------------------------------
# 2. Contrôle de couverture : qui déclare quoi, et quand ?
# -----------------------------------------------------------------------------
# L'Iran, les Émirats et le Tadjikistan n'ont pas (encore) déclaré 2024-2025,
# la Russie ne déclare plus depuis 2022, l'Ouzbékistan n'apparaît qu'en 2017
# et le Turkménistan ne déclare jamais. Ces trous conditionnent l'interprétation.
couverture <- commerce |>
  distinct(annee, pays) |>
  mutate(present = TRUE) |>
  pivot_wider(names_from = annee, values_from = present, values_fill = FALSE)

# Période de comparaison principale : 2017-2023 (panel des voisins complet
# hormis le Turkménistan, et la Russie après 2021).
periode_principale <- 2017:2023

# -----------------------------------------------------------------------------
# 3. Agrégation par groupe et indicateurs
# -----------------------------------------------------------------------------
par_groupe <- commerce |>
  group_by(annee, groupe) |>
  summarise(commerce_total = sum(valeur_usd),
            importations   = sum(valeur_usd[flux_afghan == "Importations afghanes"]),
            exportations   = sum(valeur_usd[flux_afghan == "Exportations afghanes"]),
            .groups = "drop") |>
  group_by(annee) |>
  mutate(part = commerce_total / sum(commerce_total)) |>
  ungroup()

write_csv(par_groupe, "data/commerce_afghan_par_groupe.csv")

# Indice de Herfindahl-Hirschman (HHI) calculé par pays partenaire
# (somme des parts au carré, x 10 000) : plus il est bas, plus le commerce
# afghan est diversifié. Le regroupement « Autres déclarants » est exclu du
# calcul car il agrège des dizaines de petits partenaires.
indicateurs <- commerce |>
  filter(code_declarant != "OTH") |>
  group_by(annee, pays) |>
  summarise(v = sum(valeur_usd), .groups = "drop") |>
  group_by(annee) |>
  summarise(hhi = sum((v / sum(v))^2) * 10000, .groups = "drop") |>
  left_join(
    par_groupe |>
      select(annee, groupe, part) |>
      pivot_wider(names_from = groupe, values_from = part),
    by = "annee"
  )

write_csv(indicateurs, "data/indicateurs_annuels.csv")

# Comparaison avant / après la prise de Kaboul (moyennes de parts)
avant_apres <- par_groupe |>
  filter(annee %in% periode_principale, annee != 2021) |>
  mutate(periode = if_else(annee <= 2020, "2017-2020", "2022-2023")) |>
  group_by(groupe, periode) |>
  summarise(part_moyenne = mean(part),
            commerce_moyen_musd = mean(commerce_total) / 1e6,
            .groups = "drop")

# -----------------------------------------------------------------------------
# 4. Visualisations
# -----------------------------------------------------------------------------
# Palette catégorielle fixe (une couleur par groupe, toujours la même)
couleurs <- c(
  "Pakistan"            = "#2a78d6",
  "Iran"                = "#eb6834",
  "Asie centrale"       = "#1baf7a",
  "Chine"               = "#eda100",
  "Inde"                = "#e87ba4",
  "Émirats arabes unis" = "#6250d6",
  "Occident et OCDE"    = "#008300",
  "Autres"              = "#b5b3ad"
)

theme_projet <- theme_minimal(base_size = 11) +
  theme(
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(colour = "#e6e5e0", linewidth = 0.3),
    plot.title         = element_text(face = "bold"),
    plot.caption       = element_text(colour = "#52514e", hjust = 0, size = 8),
    legend.position    = "bottom",
    plot.background    = element_rect(fill = "white", colour = NA)
  )

source_txt <- "Source : UN Comtrade, données miroirs (déclarations des partenaires), extraction du 21 novembre 2026. Calculs propres."

marque_2021 <- list(
  geom_vline(xintercept = 2021.62, linetype = "dashed", colour = "#52514e",
             linewidth = 0.4),
  annotate("text", x = 2021.7, y = Inf, label = "Août 2021", hjust = 0,
           vjust = 1.5, size = 3, colour = "#52514e")
)

# Figure 1 : volume total par groupe, 2015-2025 (2024-2025 signalés incomplets)
fig1 <- ggplot(par_groupe, aes(annee, commerce_total / 1e9, fill = groupe)) +
  annotate("rect", xmin = 2023.5, xmax = 2025.5, ymin = -Inf, ymax = Inf,
           fill = "#f1f0ec") +
  annotate("text", x = 2024.5, y = Inf, vjust = 1.5, size = 2.8,
           colour = "#52514e",
           label = "Iran, EAU et\nTadjikistan absents") +
  geom_col(width = 0.75, colour = "white", linewidth = 0.3) +
  marque_2021 +
  scale_fill_manual(values = couleurs, name = NULL) +
  scale_x_continuous(breaks = 2015:2025) +
  scale_y_continuous(labels = label_number(suffix = " G$"),
                     expand = expansion(mult = c(0, 0.08))) +
  labs(title = "Commerce total de l'Afghanistan par groupe de partenaires",
       subtitle = "Importations + exportations, en milliards de dollars courants",
       x = NULL, y = NULL, caption = source_txt) +
  theme_projet

ggsave("images/fig1_commerce_par_groupe.png", fig1,
       width = 8, height = 5, dpi = 200)

# Figure 2 : parts des principaux partenaires, période principale 2015-2023
groupes_cles <- c("Pakistan", "Iran", "Asie centrale", "Chine",
                  "Émirats arabes unis")
fig2 <- par_groupe |>
  filter(annee <= 2023, groupe %in% groupes_cles) |>
  ggplot(aes(annee, part, colour = groupe)) +
  marque_2021 +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2.2) +
  scale_colour_manual(values = couleurs, name = NULL) +
  scale_x_continuous(breaks = 2015:2023) +
  scale_y_continuous(labels = label_percent(accuracy = 1)) +
  labs(title = "Part des principaux partenaires dans le commerce afghan",
       subtitle = "En % du commerce total observé (importations + exportations)",
       x = NULL, y = NULL, caption = source_txt) +
  theme_projet

ggsave("images/fig2_parts_partenaires.png", fig2,
       width = 8, height = 5, dpi = 200)

# Figure 3 : concentration (HHI), période principale
fig3 <- indicateurs |>
  filter(annee <= 2023) |>
  ggplot(aes(annee, hhi)) +
  marque_2021 +
  geom_line(linewidth = 0.9, colour = "#2a78d6") +
  geom_point(size = 2.2, colour = "#2a78d6") +
  scale_x_continuous(breaks = 2015:2023) +
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.1))) +
  labs(title = "Concentration géographique du commerce afghan",
       subtitle = "Indice de Herfindahl-Hirschman par pays partenaire (0 = dispersion maximale, 10 000 = un seul partenaire)",
       x = NULL, y = NULL, caption = source_txt) +
  theme_projet

ggsave("images/fig3_concentration_hhi.png", fig3,
       width = 8, height = 4.5, dpi = 200)

# Figure 4 : panel constant 2017-2025 (déclarants présents chaque année)
# Permet d'utiliser 2024-2025 sans biais de couverture, au prix de l'absence
# de l'Iran. On l'interprète donc comme un test partiel.
panel_constant <- commerce |>
  filter(annee >= 2017) |>
  group_by(pays) |>
  filter(n_distinct(annee) == length(2017:2025), pays != "Autres déclarants") |>
  ungroup()

fig4 <- panel_constant |>
  filter(groupe %in% c("Pakistan", "Asie centrale", "Chine", "Inde")) |>
  group_by(annee, groupe) |>
  summarise(v = sum(valeur_usd), .groups = "drop") |>
  ggplot(aes(annee, v / 1e9, colour = groupe)) +
  marque_2021 +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2.2) +
  geom_text(data = ~ filter(.x, annee == 2025),
            aes(label = groupe), hjust = -0.1, size = 3, show.legend = FALSE) +
  scale_colour_manual(values = couleurs, name = NULL) +
  scale_x_continuous(breaks = 2017:2025,
                     expand = expansion(add = c(0.3, 1.6))) +
  scale_y_continuous(labels = label_number(suffix = " G$"), limits = c(0, NA)) +
  labs(title = "Commerce afghan avec un panel constant de déclarants, 2017-2025",
       subtitle = "Asie centrale = Ouzbékistan, Kazakhstan, Kirghizstan (Tadjikistan exclu, non déclarant en 2024-2025)",
       x = NULL, y = NULL,
       caption = paste0(source_txt, "\nDonnées 2025 possiblement provisoires (déclarations tardives).")) +
  theme_projet

ggsave("images/fig4_panel_constant.png", fig4,
       width = 8, height = 5, dpi = 200)
