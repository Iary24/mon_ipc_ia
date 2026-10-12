# =============================================================================
# IPC - Indice des Prix à la Consommation
# Source : IPC_RES_0116_0526.xlsx | Feuille : IPC
# Date   : 2026-07-08
# =============================================================================
#
# Étapes du script :
#   1. Lecture des données brutes depuis le fichier Excel
#   2. Mise en forme au format long (tidy) et conversion des dates
#   3. Sauvegarde du fichier transformé
#   4. Visualisation de l'évolution de l'IPC global
#   5. Calcul et graphique du taux d'inflation en moyenne annuelle
#   6. Calcul et graphique de l'inflation en glissement annuel de fin de période
#   7. Graphique comparatif : glissement annuel vs moyenne annuelle
#   8. Contributions absolues et relatives des composantes (mensuelle,
#      glissement fin de période, moyenne annuelle)
#   9. Export Excel récapitulatif des contributions (toutes les années)
#  10. Export Excel des contributions — 6 catégories
#  11. Décomposition STL de l'IPC Ensemble (tendance, saisonnalité, résidu)
# =============================================================================

library(readxl)
library(tidyverse)

# -----------------------------------------------------------------------------
# 1. Lecture des données
# -----------------------------------------------------------------------------
# Le fichier Excel contient 3 lignes d'en-tête à ignorer (skip = 3).
# Les lignes suivantes représentent les postes de dépenses, avec leur
# pondération et l'indice mensuel pour chaque colonne-date.

ipc_raw <- read_excel(
  "data/IPC_RES_0116_0526.xlsx",
  sheet = "IPC",
  skip = 3
)

# Renommer les deux premières colonnes (libellé du poste et pondération)
colnames(ipc_raw)[1:2] <- c("libelle", "ponderation")

# -----------------------------------------------------------------------------
# 2. Transformation au format long
# -----------------------------------------------------------------------------
# On pivote les colonnes-dates en lignes pour obtenir un format tidy :
# une ligne = un poste × un mois.
# Les lignes sans pondération sont des titres de catégories et sont retirées.
# Les colonnes-dates issues d'Excel sont des numéros de série à convertir
# en dates R (origine Windows Excel : 1899-12-30).

ipc_long <- ipc_raw |>
  filter(!is.na(ponderation)) |>
  pivot_longer(
    cols = -c(libelle, ponderation),
    names_to = "date_excel",
    values_to = "ipc"
  ) |>
  mutate(
    date = as.Date(as.numeric(date_excel), origin = "1899-12-30"),
    .keep = "unused"
  ) |>
  select(libelle, ponderation, date, ipc)

# -----------------------------------------------------------------------------
# 3. Sauvegarde
# -----------------------------------------------------------------------------
# On exporte le tableau long en .rds pour pouvoir le réutiliser
# dans d'autres scripts sans relire et retraiter le fichier Excel.

saveRDS(ipc_long, "output/ipc_long.rds")

# -----------------------------------------------------------------------------
# 4. Visualisation : évolution de l'IPC global
# -----------------------------------------------------------------------------
# Courbe de l'indice mensuel pour le poste "Ensemble" (IPC global),
# de janvier 2016 à mai 2026.

ipc_long |>
  filter(libelle == "Ensemble") |>
  ggplot(aes(x = date, y = ipc)) +
  geom_line() +
  labs(
    title = "Évolution de l'IPC global",
    subtitle = "Janvier 2016 – Mai 2026 (base 100 = juillet 2016)",
    x = NULL,
    y = "Indice des prix à la consommation"
  )

# -----------------------------------------------------------------------------
# 5. Taux d'inflation annuel (variation de la moyenne annuelle)
# -----------------------------------------------------------------------------
# Le taux d'inflation en moyenne annuelle mesure la variation de la moyenne
# des indices mensuels d'une année à l'autre. Seules les années avec 12 mois
# complets sont retenues pour garantir des moyennes comparables.

inflation_annuelle <- ipc_long |>
  filter(libelle == "Ensemble") |>
  mutate(annee = year(date)) |>
  group_by(annee) |>
  filter(n() == 12) |>
  summarise(ipc_moyen = mean(ipc), .groups = "drop") |>
  mutate(
    inflation = (ipc_moyen / lag(ipc_moyen) - 1) * 100
  ) |>
  filter(annee >= 2017)

ggplot(inflation_annuelle, aes(x = factor(annee), y = inflation)) +
  geom_col() +
  geom_text(
    aes(label = sprintf("%.1f%%", inflation)),
    vjust = -0.5, size = 3.5
  ) +
  labs(
    title = "Taux d'inflation annuel – IPC global",
    subtitle = "Variation de la moyenne annuelle de l'IPC (2017–2025)",
    x = NULL,
    y = "Taux d'inflation (%)"
  ) +
  scale_y_continuous(
    labels = scales::label_percent(scale = 1),
    expand = expansion(mult = c(0, 0.1))
  )

# -----------------------------------------------------------------------------
# 6. Inflation en glissement annuel de fin de période
# -----------------------------------------------------------------------------
# Le glissement annuel de fin de période compare l'IPC du dernier mois
# disponible d'une année à celui du même mois de l'année précédente.
# On retient décembre quand l'année est complète ; pour l'année en cours
# (2026), on retient le dernier mois disponible (ici mai).
#
# Étapes :
#   a. Extraire le dernier mois disponible par année (mois de référence)
#   b. Récupérer l'IPC de ce même mois pour l'année précédente (self-join)
#   c. Calculer la variation en %

# a. Série mensuelle complète + dernier mois disponible par année
ensemble_mensuel <- ipc_long |>
  filter(libelle == "Ensemble") |>
  mutate(annee = year(date), mois = month(date))

fin_periode <- ensemble_mensuel |>
  group_by(annee) |>
  slice_max(mois, n = 1) |>
  ungroup() |>
  select(annee, mois, ipc)

# b & c. Jointure avec l'IPC du même mois de l'année précédente, puis calcul.
# On joint fin_periode (année N, mois de référence) avec ensemble_mensuel
# en décalant l'année de +1, ce qui permet de retrouver le même mois pour N-1
# même si ce mois n'est pas le dernier de cette année-là (ex. mai 2025 pour 2026).
inflation_glissement <- fin_periode |>
  left_join(
    ensemble_mensuel |> mutate(annee = annee + 1) |> select(annee, mois, ipc_n1 = ipc),
    by = c("annee", "mois")
  ) |>
  filter(!is.na(ipc_n1)) |>
  mutate(
    glissement = (ipc / ipc_n1 - 1) * 100,
    # Étiquette : année seule si décembre, sinon on précise le mois
    label = if_else(
      mois == 12,
      as.character(annee),
      paste0(annee, "\n(", month(make_date(annee, mois), label = TRUE, abbr = TRUE), ")")
    )
  )

ggplot(inflation_glissement, aes(x = label, y = glissement)) +
  geom_col() +
  geom_text(
    aes(label = sprintf("%.1f%%", glissement)),
    vjust = -0.5, size = 3.5
  ) +
  labs(
    title = "Inflation en glissement annuel de fin de période – IPC global",
    subtitle = "Variation de l'IPC du dernier mois disponible vs même mois de l'année précédente",
    x = NULL,
    y = "Glissement annuel (%)"
  ) +
  scale_y_continuous(
    labels = scales::label_percent(scale = 1),
    expand = expansion(mult = c(0, 0.1))
  )

# -----------------------------------------------------------------------------
# 7. Graphique comparatif : glissement annuel vs moyenne annuelle
# -----------------------------------------------------------------------------
# On combine les deux mesures dans un tableau unique pour les afficher
# en barres groupées. 2026 n'a pas de moyenne annuelle (année incomplète) ;
# seul le glissement (mai vs mai 2025) y figure.

bind_rows(
  inflation_annuelle |>
    select(annee, inflation) |>
    mutate(mesure = "Moyenne annuelle"),
  inflation_glissement |>
    select(annee, mois, inflation = glissement) |>
    mutate(mesure = "Glissement annuel (fin de période)")
) |>
  ggplot(aes(x = factor(annee), y = inflation, fill = mesure)) +
  geom_col(position = "dodge") +
  geom_text(
    aes(label = sprintf("%.1f%%", inflation)),
    position = position_dodge(width = 0.9),
    vjust = -0.5, size = 3
  ) +
  labs(
    title = "Inflation – Comparaison des deux mesures",
    subtitle = "Glissement annuel de fin de période vs moyenne annuelle\n(2026 : glissement sur mai uniquement ; moyenne annuelle non disponible)",
    x = NULL,
    y = "Taux d'inflation (%)",
    fill = NULL
  ) +
  scale_y_continuous(
    labels = scales::label_percent(scale = 1),
    expand = expansion(mult = c(0, 0.15))
  ) +
  theme(legend.position = "bottom")

# -----------------------------------------------------------------------------
# 8. Catégories, noms courts et fonctions de calcul des contributions
# -----------------------------------------------------------------------------
# Les données sont classifiées selon 6 catégories indépendantes.
# Dans chacune, les pondérations des sous-catégories somment à 10 000.
#
# Formule exacte (Balk, 1998) :
#   C_i = w_i × (IPC_i(t) − IPC_i(t−k)) / IPC_ensemble(t−k)   [en pp]
#
# Cette formule est algébriquement exacte : sum(C_i) = π_global,
# car IPC_ensemble = Σ w_i × IPC_i (structure de Laspeyres enchaîné).
# La contribution relative CR_i est normalisée par sum(C_i) pour
# garantir sum(CR_i) = 100 par construction.

# 8a. Définition des 6 catégories -------------------------------------------

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

# Noms courts (renommage minimal) pour les graphiques et exports
noms_courts <- c(
  # RIZ
  "Non Riz"                                                   = "Non-Riz",
  "Riz"                                                       = "Riz",
  # ORIGINE
  "Produits Locaux"                                           = "Local",
  "Produits Semi Importés"                                    = "Semi-importé",
  "Produits Importés"                                         = "Importé",
  # ENERGIE
  "Non Energie"                                               = "Non-Énergie",
  "Energie"                                                   = "Énergie",
  # PPN
  "PPN"                                                       = "PPN",
  "Non PPN"                                                   = "Non-PPN",
  # SECTEUR DE PRODUCTION
  "Produits Vivriers Non Transformés"                         = "Vivriers non transformés",
  "Produits Vivriers Transformés"                             = "Vivriers transformés",
  "Produits Manufacturés Industriels"                         = "Manufacturés industriels",
  "Produits Manufacturés Artisanaux"                          = "Manufacturés artisanaux",
  "Services Publics"                                          = "Services publics",
  "Services Privés"                                           = "Services privés",
  # FONCTION
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

# 8b. Fonctions de calcul ----------------------------------------------------

# Contributions point à point : mensuelle (lag 1), trimestrielle (lag 3),
# glissement annuel (lag 12).
# Retourne C_m, C_t, C_y en points de pourcentage.
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
    # On supprime les 12 premiers mois (lags incomplets)
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

# 8c. Graphiques FONCTION (dernière période disponible) ----------------------
# Barres horizontales ordonnées par contribution absolue.

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
    labs(title = titre, subtitle = sous_titre,
         x = "Contribution (points de pourcentage)", y = NULL) +
    scale_x_continuous(expand = expansion(mult = 0.25))
}

# Calcul PTP pour FONCTION seul (pour les graphiques)
contrib_ptp_fonction <- compute_contrib_ptp(ipc_long, categories_def$FONCTION) |>
  mutate(libelle_court = noms_courts[libelle])

# Taux globaux de référence (pour les sous-titres des graphiques)
ens_pi <- ipc_long |>
  filter(libelle == "Ensemble") |>
  arrange(date) |>
  mutate(
    pi_m = 100 * (ipc - lag(ipc,  1)) / lag(ipc,  1),
    pi_t = 100 * (ipc - lag(ipc,  3)) / lag(ipc,  3),
    pi_y = 100 * (ipc - lag(ipc, 12)) / lag(ipc, 12)
  ) |>
  filter(!is.na(pi_y)) |>
  select(date, pi_m, pi_t, pi_y)

date_ref  <- max(contrib_ptp_fonction$date)
pi_ref    <- ens_pi |> filter(date == date_ref)

contrib_ref <- contrib_ptp_fonction |> filter(date == date_ref)

plot_contrib(
  contrib_ref, "C_m",
  paste("Contribution mensuelle – FONCTION |", format(date_ref, "%B %Y")),
  sprintf("IPC global : %+.2f pp | Σ contributions : %+.2f pp",
          pi_ref$pi_m, sum(contrib_ref$C_m))
)

plot_contrib(
  contrib_ref, "C_t",
  paste("Contribution trimestrielle – FONCTION |", format(date_ref, "%B %Y")),
  sprintf("IPC global : %+.2f pp | Σ contributions : %+.2f pp",
          pi_ref$pi_t, sum(contrib_ref$C_t))
)

plot_contrib(
  contrib_ref, "C_y",
  paste("Contribution en glissement annuel – FONCTION |", format(date_ref, "%B %Y")),
  sprintf("IPC global : %+.2f pp | Σ contributions : %+.2f pp",
          pi_ref$pi_y, sum(contrib_ref$C_y))
)

# Graphique moyenne annuelle (FONCTION, dernière année complète)
contrib_moy_fonction <- compute_contrib_moy(ipc_long, categories_def$FONCTION) |>
  mutate(libelle_court = noms_courts[libelle])

ens_pi_moy <- ipc_long |>
  filter(libelle == "Ensemble") |>
  mutate(annee = year(date)) |>
  group_by(annee) |>
  filter(n() == 12) |>
  summarise(ipc_moy = mean(ipc), .groups = "drop") |>
  mutate(pi_a = 100 * (ipc_moy - lag(ipc_moy)) / lag(ipc_moy)) |>
  filter(!is.na(pi_a))

annee_ref <- max(contrib_moy_fonction$annee)
contrib_moy_ref <- contrib_moy_fonction |> filter(annee == annee_ref)
pi_a_ref <- ens_pi_moy |> filter(annee == annee_ref) |> pull(pi_a)

plot_contrib(
  contrib_moy_ref, "C_a",
  paste("Contribution en moyenne annuelle – FONCTION |", annee_ref),
  sprintf("IPC global : %+.2f pp | Σ contributions : %+.2f pp",
          pi_a_ref, sum(contrib_moy_ref$C_a))
)

# -----------------------------------------------------------------------------
# 9. Calcul des contributions — 6 catégories (4 horizons)
# -----------------------------------------------------------------------------
# On applique compute_contrib_ptp() et compute_contrib_moy() à chacune
# des 6 catégories. Les contributions relatives (CR_*) sont normalisées
# par sum(C_*) au sein de chaque groupe (date × catégorie ou annee × catégorie)
# pour garantir sum(CR_*) = 100.

contrib_ptp_6cat <- imap_dfr(categories_def, \(composantes, cat) {
  compute_contrib_ptp(ipc_long, composantes) |>
    mutate(categorie = cat, libelle_court = noms_courts[libelle])
}) |>
  group_by(categorie, date) |>
  mutate(
    CR_m = 100 * C_m / sum(C_m, na.rm = TRUE),
    CR_t = 100 * C_t / sum(C_t, na.rm = TRUE),
    CR_y = 100 * C_y / sum(C_y, na.rm = TRUE)
  ) |>
  ungroup() |>
  select(categorie, date, libelle_court, ponderation, C_m, CR_m, C_t, CR_t, C_y, CR_y) |>
  arrange(categorie, date, desc(C_y))

contrib_moy_6cat <- imap_dfr(categories_def, \(composantes, cat) {
  compute_contrib_moy(ipc_long, composantes) |>
    mutate(categorie = cat, libelle_court = noms_courts[libelle])
}) |>
  group_by(categorie, annee) |>
  mutate(CR_a = 100 * C_a / sum(C_a, na.rm = TRUE)) |>
  ungroup() |>
  select(categorie, annee, libelle_court, ponderation, C_a, CR_a) |>
  arrange(categorie, annee, desc(C_a))

# -----------------------------------------------------------------------------
# 10. Export Excel — 6 catégories
# -----------------------------------------------------------------------------
# Deux fichiers :
#   contributions_6cat_ptp.xlsx      : mensuelle, trimestrielle, glissement annuel
#   contributions_6cat_moy_annuelle.xlsx : variation de la moyenne annuelle
# Chaque fichier contient une feuille par catégorie.

library(openxlsx)

fmt_n <- createStyle(numFmt = "0.0000")

# 10a. PTP (mensuelle, trimestrielle, glissement annuel) ---------------------
wb_ptp <- createWorkbook()

for (cat in names(categories_def)) {
  df <- contrib_ptp_6cat |>
    filter(categorie == cat) |>
    select(date, libelle_court, ponderation, C_m, CR_m, C_t, CR_t, C_y, CR_y)

  addWorksheet(wb_ptp, cat)
  writeDataTable(wb_ptp, cat, df, tableStyle = "TableStyleMedium2", withFilter = FALSE)

  cols_num <- which(names(df) %in% c("ponderation", "C_m", "CR_m", "C_t", "CR_t", "C_y", "CR_y"))
  addStyle(wb_ptp, cat, fmt_n,
           rows = 2:(nrow(df) + 1), cols = cols_num, gridExpand = TRUE)
  setColWidths(wb_ptp, cat, cols = seq_along(names(df)),
               widths = c(13, 26, 11, 10, 8, 10, 8, 10, 8))
}

saveWorkbook(wb_ptp, "output/contributions_6cat_ptp.xlsx", overwrite = TRUE)

# 10b. Variation de la moyenne annuelle ---------------------------------------
wb_moy <- createWorkbook()

for (cat in names(categories_def)) {
  df <- contrib_moy_6cat |>
    filter(categorie == cat) |>
    select(annee, libelle_court, ponderation, C_a, CR_a)

  addWorksheet(wb_moy, cat)
  writeDataTable(wb_moy, cat, df, tableStyle = "TableStyleMedium2", withFilter = FALSE)

  cols_num <- which(names(df) %in% c("ponderation", "C_a", "CR_a"))
  addStyle(wb_moy, cat, fmt_n,
           rows = 2:(nrow(df) + 1), cols = cols_num, gridExpand = TRUE)
  setColWidths(wb_moy, cat, cols = seq_along(names(df)),
               widths = c(8, 26, 11, 10, 8))
}

saveWorkbook(wb_moy, "output/contributions_6cat_moy_annuelle.xlsx", overwrite = TRUE)

# -----------------------------------------------------------------------------
# 11. Décomposition STL de l'IPC Ensemble
# -----------------------------------------------------------------------------
# La méthode STL (Seasonal and Trend decomposition using Loess) décompose la
# série en trois composantes additives :
#   - Tendance   : évolution de fond lissée par régression locale (Loess)
#   - Saisonnalité : patron intra-annuel récurrent
#   - Résidu     : ce qui n'est expliqué ni par la tendance ni par la saison
#
# Paramètres clés :
#   s.window = "periodic" : saisonnalité supposée stable dans le temps
#   robust   = TRUE       : atténue l'influence des points aberrants sur la tendance

ipc_ts <- ensemble_mensuel |>
  arrange(date) |>
  pull(ipc) |>
  ts(start = c(2016, 1), frequency = 12)

decomp <- stl(ipc_ts, s.window = "periodic", robust = TRUE)

decomp_df <- as.data.frame(decomp$time.series) |>
  rename(Saisonnalité = seasonal, Tendance = trend, Résidu = remainder) |>
  mutate(
    date    = sort(ensemble_mensuel$date),
    Observé = as.numeric(ipc_ts)
  ) |>
  pivot_longer(c(Observé, Tendance, Saisonnalité, Résidu),
               names_to = "composante", values_to = "valeur") |>
  mutate(
    composante = factor(composante,
                        levels = c("Observé", "Tendance", "Saisonnalité", "Résidu"))
  )

ggplot(decomp_df, aes(x = date, y = valeur, color = composante)) +
  geom_line(linewidth = 0.6) +
  facet_wrap(~composante, ncol = 1, scales = "free_y") +
  labs(
    title    = "Décomposition STL de l'IPC (Ensemble)",
    subtitle = "Méthode : Seasonal and Trend decomposition using Loess",
    x = NULL, y = NULL,
    caption  = "Source : INSTAT/DSCVM/IPC"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    strip.text     = element_text(face = "bold"),
    legend.position = "none"
  )
