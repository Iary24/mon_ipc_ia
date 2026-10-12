# =============================================================================
# server.R — Logique serveur | IPC Madagascar
# =============================================================================
# Architecture :
#   ipc_long_rv  : reactiveVal central — mis à jour par mod_import
#   Dérivés réactifs calculés ici une seule fois, transmis aux modules
#   Chaque module reçoit uniquement les données dont il a besoin

server <- function(input, output, session) {

  thematic::thematic_shiny()

  # --------------------------------------------------------------------------
  # Données centrales
  # --------------------------------------------------------------------------
  # Initialisées depuis le RDS (ou NULL si absent).
  # Mises à jour par mod_import quand l'utilisateur charge un nouveau fichier.

  ipc_long_rv <- reactiveVal(ipc_long_init)
  ipc_geo_rv  <- reactiveVal(ipc_geo_init)

  # --------------------------------------------------------------------------
  # Dérivés pré-calculés — recalculés uniquement quand les données changent
  # --------------------------------------------------------------------------

  # Taux d'inflation en moyenne annuelle (années complètes)
  inflation_annuelle_r <- reactive({
    df <- ipc_long_rv()
    req(df)

    df |>
      filter(libelle == "Ensemble") |>
      mutate(annee = year(date)) |>
      group_by(annee) |>
      filter(n() == 12) |>
      summarise(ipc_moyen = mean(ipc), .groups = "drop") |>
      mutate(inflation = (ipc_moyen / lag(ipc_moyen) - 1) * 100) |>
      filter(annee >= 2017, !is.na(inflation))
  })

  # Glissement annuel de fin de période
  inflation_glissement_r <- reactive({
    df <- ipc_long_rv()
    req(df)

    ensemble_mensuel <- df |>
      filter(libelle == "Ensemble") |>
      mutate(annee = year(date), mois = month(date))

    fin_periode <- ensemble_mensuel |>
      group_by(annee) |>
      slice_max(mois, n = 1) |>
      ungroup() |>
      select(annee, mois, ipc)

    fin_periode |>
      left_join(
        ensemble_mensuel |>
          mutate(annee = annee + 1) |>
          select(annee, mois, ipc_n1 = ipc),
        by = c("annee", "mois")
      ) |>
      filter(!is.na(ipc_n1)) |>
      mutate(
        glissement = (ipc / ipc_n1 - 1) * 100,
        label = if_else(
          mois == 12,
          as.character(annee),
          paste0(annee, "\n(", month(make_date(annee, mois), label = TRUE, abbr = TRUE), ")")
        )
      )
  })

  # Contributions PTP pour les 6 catégories (calcul lourd, une seule fois)
  contrib_ptp_6cat_r <- reactive({
    df <- ipc_long_rv()
    req(df)

    imap_dfr(categories_def, \(composantes, cat) {
      compute_contrib_ptp(df, composantes) |>
        mutate(categorie = cat)
    }) |>
      group_by(categorie, date) |>
      mutate(
        CR_m = 100 * C_m / sum(C_m, na.rm = TRUE),
        CR_t = 100 * C_t / sum(C_t, na.rm = TRUE),
        CR_y = 100 * C_y / sum(C_y, na.rm = TRUE)
      ) |>
      ungroup() |>
      arrange(categorie, date, desc(C_y))
  })

  # Contributions à la moyenne annuelle pour les 6 catégories
  contrib_moy_6cat_r <- reactive({
    df <- ipc_long_rv()
    req(df)

    imap_dfr(categories_def, \(composantes, cat) {
      compute_contrib_moy(df, composantes) |>
        mutate(categorie = cat)
    }) |>
      group_by(categorie, annee) |>
      mutate(CR_a = 100 * C_a / sum(C_a, na.rm = TRUE)) |>
      ungroup() |>
      arrange(categorie, annee, desc(C_a))
  })

  # --------------------------------------------------------------------------
  # Modules
  # --------------------------------------------------------------------------

  mod_import_server("import", ipc_long_rv, ipc_geo_rv)
  mod_carte_server("carte", ipc_geo_rv)

  mod_ipc_global_server("ipc_global", ipc_long_rv)

  mod_inflation_server(
    "inflation",
    inflation_annuelle_r   = inflation_annuelle_r,
    inflation_glissement_r = inflation_glissement_r
  )

  mod_contributions_server(
    "contributions",
    contrib_ptp_6cat_r = contrib_ptp_6cat_r,
    contrib_moy_6cat_r = contrib_moy_6cat_r
  )

  mod_stl_server("stl", ipc_long_rv)
}
