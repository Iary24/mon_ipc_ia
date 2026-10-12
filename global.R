# =============================================================================
# global.R — IPC Shiny App
# Chargé une seule fois au démarrage de l'application.
# Contient : packages, constantes, fonctions partagées, données initiales.
# =============================================================================

library(shiny)
library(bslib)
library(bsicons)
library(thematic)
library(ggiraph)
library(maps)
library(readxl)
library(tidyverse)
library(scales)

# -----------------------------------------------------------------------------
# Constantes
# -----------------------------------------------------------------------------

# Définition des 6 catégories de décomposition
categories_def <- list(
  "RIZ" = c("Non Riz", "Riz"),
  "ORIGINE" = c("Produits Locaux", "Produits Semi Importés", "Produits Importés"),
  "ENERGIE" = c("Non Energie", "Energie"),
  "PPN" = c("PPN", "Non PPN"),
  "SECTEUR DE PRODUCTION" = c(
    "Produits Vivriers Non Transformés", "Produits Vivriers Transformés",
    "Produits Manufacturés Industriels", "Produits Manufacturés Artisanaux",
    "Services Publics", "Services Privés"
  ),
  "FONCTION" = c(
    "Produits alimentaires et boissons non alcoolisés",
    "Boissons alcoolisées et tabacs",
    "Articles d'habillement et articles chaussants",
    "Logement, eau, éléctricité, gaz et autres combustibles",
    "Ameublement, équipement ménager et entretien courant",
    "Santé", "Transports", "Communications", "Loisirs et culture",
    "Enseignement, Education", "Hôtellerie, cafés, restauration",
    "Autres biens et services"
  )
)

# Noms courts pour graphiques et exports
noms_courts <- c(
  "Non Riz"                                                   = "Non-Riz",
  "Riz"                                                       = "Riz",
  "Produits Locaux"                                           = "Local",
  "Produits Semi Importés"                                    = "Semi-importé",
  "Produits Importés"                                         = "Importé",
  "Non Energie"                                               = "Non-Énergie",
  "Energie"                                                   = "Énergie",
  "PPN"                                                       = "PPN",
  "Non PPN"                                                   = "Non-PPN",
  "Produits Vivriers Non Transformés"                         = "Vivriers non transformés",
  "Produits Vivriers Transformés"                             = "Vivriers transformés",
  "Produits Manufacturés Industriels"                         = "Manufacturés industriels",
  "Produits Manufacturés Artisanaux"                          = "Manufacturés artisanaux",
  "Services Publics"                                          = "Services publics",
  "Services Privés"                                           = "Services privés",
  "Produits alimentaires et boissons non alcoolisés"          = "Alimentation & boissons",
  "Boissons alcoolisées et tabacs"                            = "Alcool & tabacs",
  "Articles d'habillement et articles chaussants"             = "Habillement & chaussures",
  "Logement, eau, éléctricité, gaz et autres combustibles"    = "Logement & énergie",
  "Ameublement, équipement ménager et entretien courant"      = "Ameublement",
  "Santé"                                                     = "Santé",
  "Transports"                                                = "Transports",
  "Communications"                                            = "Communications",
  "Loisirs et culture"                                        = "Loisirs & culture",
  "Enseignement, Education"                                   = "Enseignement",
  "Hôtellerie, cafés, restauration"                           = "Hôtellerie & restauration",
  "Autres biens et services"                                  = "Autres biens & services"
)

# -----------------------------------------------------------------------------
# Fonctions partagées
# -----------------------------------------------------------------------------

# Lecture et transformation du fichier Excel brut vers le format long tidy.
# Retourne une liste : $postes (libelle|ponderation|date|ipc)
#                      $provinces (libelle|date|ipc)
load_ipc_excel <- function(path) {
  villes <- c("Antananarivo", "Antsirabe", "Fianarantsoa",
              "Toamasina", "Mahajanga", "Toliara", "Antsiranana")

  ipc_raw <- read_excel(path, sheet = "IPC", skip = 3)
  colnames(ipc_raw)[1:2] <- c("libelle", "ponderation")

  # Pivot commun sur toutes les lignes utiles
  ipc_pivot <- ipc_raw |>
    filter(!is.na(libelle), !libelle %in% c("Source : INSTAT/DSCVM/SSPC")) |>
    pivot_longer(
      cols      = -c(libelle, ponderation),
      names_to  = "date_excel",
      values_to = "ipc"
    ) |>
    mutate(
      date = as.Date(as.numeric(date_excel), origin = "1899-12-30"),
      .keep = "unused"
    )

  list(
    postes    = ipc_pivot |>
      filter(!is.na(ponderation)) |>
      select(libelle, ponderation, date, ipc),
    provinces = ipc_pivot |>
      filter(libelle %in% villes) |>
      select(libelle, date, ipc)
  )
}

# Contributions point à point : mensuelle (lag 1), trimestrielle (lag 3),
# glissement annuel (lag 12). Retourne C_m, C_t, C_y en points de pourcentage.
#
# Formule (Balk, 1998) : C_i = w_i × (IPC_i(t) − IPC_i(t−k)) / IPC_ens(t−k)
compute_contrib_ptp <- function(ipc_long, composantes_vec) {
  ens_ref <- ipc_long |>
    filter(libelle == "Ensemble") |>
    arrange(date) |>
    transmute(
      date,
      ens_lag1  = lag(ipc,  1),
      ens_lag3  = lag(ipc,  3),
      ens_lag12 = lag(ipc, 12)
    )

  ipc_long |>
    filter(libelle %in% composantes_vec) |>
    arrange(libelle, date) |>
    group_by(libelle, ponderation) |>
    mutate(
      lag_ipc_1  = lag(ipc,  1),
      lag_ipc_3  = lag(ipc,  3),
      lag_ipc_12 = lag(ipc, 12)
    ) |>
    ungroup() |>
    left_join(ens_ref, by = "date") |>
    filter(!is.na(lag_ipc_12), !is.na(ens_lag12)) |>
    mutate(
      w   = ponderation / 10000,
      C_m = 100 * w * (ipc - lag_ipc_1)  / ens_lag1,
      C_t = 100 * w * (ipc - lag_ipc_3)  / ens_lag3,
      C_y = 100 * w * (ipc - lag_ipc_12) / ens_lag12
    ) |>
    select(libelle, ponderation, date, C_m, C_t, C_y)
}

# Contributions à la variation de la moyenne annuelle.
# Retourne C_a en points de pourcentage (années complètes uniquement).
compute_contrib_moy <- function(ipc_long, composantes_vec) {
  ens_moy <- ipc_long |>
    filter(libelle == "Ensemble") |>
    mutate(annee = year(date)) |>
    group_by(annee) |>
    filter(n() == 12) |>
    summarise(ipc_moy_ens = mean(ipc), .groups = "drop") |>
    mutate(ens_moy_lag = lag(ipc_moy_ens)) |>
    filter(!is.na(ens_moy_lag))

  ipc_long |>
    filter(libelle %in% composantes_vec) |>
    mutate(annee = year(date)) |>
    group_by(libelle, ponderation, annee) |>
    filter(n() == 12) |>
    summarise(ipc_moy = mean(ipc), .groups = "drop") |>
    group_by(libelle, ponderation) |>
    mutate(lag_ipc_moy = lag(ipc_moy)) |>
    ungroup() |>
    filter(!is.na(lag_ipc_moy)) |>
    left_join(ens_moy, by = "annee") |>
    mutate(
      w   = ponderation / 10000,
      C_a = 100 * w * (ipc_moy - lag_ipc_moy) / ens_moy_lag
    ) |>
    select(libelle, ponderation, annee, C_a)
}

# Graphique en barres horizontales des contributions,
# ordonné par valeur absolue de contribution.
plot_contrib <- function(df, col_contrib, titre, sous_titre) {
  df |>
    mutate(libelle_court = reorder(libelle_court, .data[[col_contrib]])) |>
    ggplot(aes(x = .data[[col_contrib]], y = libelle_court)) +
    geom_col() +
    geom_vline(xintercept = 0, linewidth = 0.4) +
    geom_text(
      aes(
        label = sprintf("%+.2f pp", .data[[col_contrib]]),
        hjust = if_else(.data[[col_contrib]] >= 0, -0.1, 1.1)
      ),
      size = 3
    ) +
    labs(
      title    = titre,
      subtitle = sous_titre,
      x        = "Contribution (points de pourcentage)",
      y        = NULL
    ) +
    scale_x_continuous(expand = expansion(mult = 0.25))
}

# -----------------------------------------------------------------------------
# Options ggiraph partagées
# -----------------------------------------------------------------------------
# Appliquées à tous les girafe() de l'application pour une cohérence visuelle.

girafe_opts <- function() {
  list(
    opts_tooltip(
      css = paste0(
        "background:#fff;",
        "border:1px solid #dee2e6;",
        "padding:8px 12px;",
        "border-radius:6px;",
        "font-size:13px;",
        "line-height:1.5;",
        "box-shadow:2px 3px 8px rgba(0,0,0,.12);"
      ),
      use_fill = TRUE
    ),
    opts_hover(css = "opacity:0.72;cursor:pointer;"),
    opts_sizing(rescale = TRUE, width = 1)
  )
}

# -----------------------------------------------------------------------------
# Données spatiales — statiques, chargées une seule fois
# -----------------------------------------------------------------------------

# Fond de carte Madagascar (package maps — aucune dépendance sf requise)
madagascar_map <- map_data("world", region = "Madagascar")

# Coordonnées des 7 zones d'enquête IPC (tibble simple lon/lat)
provinces_coord <- tibble(
  libelle = c("Antananarivo", "Antsirabe", "Fianarantsoa",
              "Toamasina",    "Mahajanga", "Toliara",     "Antsiranana"),
  lon     = c( 47.52,          47.03,       47.08,
               49.40,          46.32,       43.67,         49.30),
  lat     = c(-18.92,         -19.87,      -21.45,
              -18.15,         -15.72,      -23.36,        -12.35)
)

# -----------------------------------------------------------------------------
# Données initiales
# -----------------------------------------------------------------------------
# Le RDS est désormais une liste : list(postes = ..., provinces = ...)
# Compatibilité ascendante : si l'ancien RDS (tibble seul) est détecté,
# on charge uniquement les postes et ipc_geo_init reste NULL.

rds_path  <- "output/ipc_long.rds"
rds_data  <- if (file.exists(rds_path)) readRDS(rds_path) else NULL

if (is.list(rds_data) && !is.data.frame(rds_data)) {
  ipc_long_init <- rds_data$postes
  ipc_geo_init  <- rds_data$provinces
} else {
  ipc_long_init <- rds_data   # ancien format ou NULL
  ipc_geo_init  <- NULL
}
