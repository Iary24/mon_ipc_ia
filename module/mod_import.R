# =============================================================================
# module/mod_import.R — Module d'import et de mise à jour des données IPC
# =============================================================================
# UI  : mod_import_ui(id)
# Server : mod_import_server(id, ipc_long_rv)
#
# Ce module permet à l'utilisateur de :
#   1. Charger un nouveau fichier Excel IPC
#   2. Prévisualiser les données importées
#   3. Valider et mettre à jour le reactiveVal partagé + sauvegarder le RDS

# -----------------------------------------------------------------------------
# UI
# -----------------------------------------------------------------------------

mod_import_ui <- function(id) {
  ns <- NS(id)

  tagList(
    layout_column_wrap(
      width = 1 / 2,
      fill  = FALSE,

      card(
        card_header(bs_icon("file-earmark-arrow-up"), " Importer un fichier Excel"),

        fileInput(
          ns("fichier_excel"),
          label    = "Sélectionner le fichier IPC (.xlsx)",
          accept   = ".xlsx",
          multiple = FALSE,
          buttonLabel = "Parcourir...",
          placeholder = "Aucun fichier sélectionné"
        ),

        p(
          class = "text-muted small",
          "Fichier attendu : feuille \"IPC\", 3 lignes d'en-tête à ignorer.",
          br(),
          "La colonne 1 = libellé, colonne 2 = pondération, colonnes suivantes = dates (numéros de série Excel)."
        ),

        input_task_button(ns("valider"), "Mettre à jour les données",
                          icon = bs_icon("arrow-repeat")),
      ),

      card(
        card_header(bs_icon("info-circle"), " Statut des données"),
        uiOutput(ns("statut"))
      )
    ),

    card(
      card_header(bs_icon("table"), " Aperçu des données importées"),
      tableOutput(ns("apercu"))
    )
  )
}

# -----------------------------------------------------------------------------
# Server
# -----------------------------------------------------------------------------

mod_import_server <- function(id, ipc_long_rv, ipc_geo_rv) {
  moduleServer(id, function(input, output, session) {

    # Données lues depuis le fichier uploadé (non encore validées)
    donnees_importees <- reactive({
      req(input$fichier_excel)

      tryCatch(
        load_ipc_excel(input$fichier_excel$datapath),
        error = function(e) {
          showNotification(
            paste("Erreur lors de la lecture du fichier :", conditionMessage(e)),
            type     = "error",
            duration = 10
          )
          NULL
        }
      )
    })

    # Validation et mise à jour sur clic du bouton
    observeEvent(input$valider, {
      data_list <- donnees_importees()
      req(data_list)

      # Mise à jour des deux reactiveVals partagés
      ipc_long_rv(data_list$postes)
      ipc_geo_rv(data_list$provinces)

      # Sauvegarde du RDS (liste complète)
      tryCatch({
        saveRDS(data_list, "output/ipc_long.rds")
        showNotification(
          "Données mises à jour et sauvegardées avec succès.",
          type     = "message",
          duration = 5
        )
      }, error = function(e) {
        showNotification(
          paste("Données mises à jour en mémoire, mais la sauvegarde a échoué :", conditionMessage(e)),
          type     = "warning",
          duration = 8
        )
      })
    })

    # Statut des données actuellement chargées dans l'app
    output$statut <- renderUI({
      df  <- ipc_long_rv()
      geo <- ipc_geo_rv()

      if (is.null(df)) {
        div(
          class = "text-warning",
          bs_icon("exclamation-triangle"), " Aucune donnée chargée.",
          br(), "Veuillez importer un fichier Excel."
        )
      } else {
        date_min   <- format(min(df$date), "%B %Y")
        date_max   <- format(max(df$date), "%B %Y")
        n_postes   <- n_distinct(df$libelle)
        n_villes   <- if (!is.null(geo)) n_distinct(geo$libelle) else 0

        div(
          class = "text-success",
          bs_icon("check-circle"), strong(" Données disponibles"),
          tags$ul(
            class = "mt-2 mb-0",
            tags$li(sprintf("Période : %s → %s", date_min, date_max)),
            tags$li(sprintf("Postes : %d", n_postes)),
            tags$li(sprintf("Zones géographiques : %d", n_villes)),
            tags$li(sprintf("Observations : %s", format(nrow(df), big.mark = "\u00a0")))
          )
        )
      }
    })

    # Aperçu : 5 premières et 5 dernières lignes des postes importés
    output$apercu <- renderTable({
      data_list <- donnees_importees()
      req(data_list)

      df <- data_list$postes
      bind_rows(head(df, 5), tail(df, 5)) |>
        mutate(date = format(date, "%Y-%m-%d"))
    }, striped = TRUE, hover = TRUE, bordered = TRUE)
  })
}
