# =============================================================================
# module/mod_contributions.R — Contributions des composantes à l'inflation
# =============================================================================
# Inputs interactifs :
#   - Catégorie : une des 6 catégories (RIZ, ORIGINE, ENERGIE, PPN, SECTEUR, FONCTION)
#   - Horizon   : Mensuel | Trimestriel | Glissement annuel | Moyenne annuelle
#   - Type      : Absolu (pp) | Relatif (%)
#
# Les données pré-calculées (contrib_ptp_6cat_r, contrib_moy_6cat_r) sont
# filtrées de façon réactive selon les sélections de l'utilisateur.

# -----------------------------------------------------------------------------
# UI
# -----------------------------------------------------------------------------

mod_contributions_ui <- function(id) {
  ns <- NS(id)

  tagList(
    layout_sidebar(
      sidebar = sidebar(
        title = "Paramètres",
        width = 260,

        selectInput(
          ns("categorie"),
          "Catégorie",
          choices  = names(categories_def),
          selected = "FONCTION"
        ),

        radioButtons(
          ns("horizon"),
          "Horizon",
          choices = c(
            "Mensuel"            = "C_m",
            "Trimestriel"        = "C_t",
            "Glissement annuel"  = "C_y",
            "Moyenne annuelle"   = "C_a"
          ),
          selected = "C_y"
        ),

        radioButtons(
          ns("type_contrib"),
          "Type de contribution",
          choices  = c("Absolue (pp)" = "abs", "Relative (%)" = "rel"),
          selected = "abs"
        ),

        hr(),

        # Sélecteur de période — dynamique selon l'horizon choisi
        uiOutput(ns("ui_periode"))
      ),

      # Graphique principal
      card(
        full_screen = TRUE,
        card_header(textOutput(ns("titre_carte"))),
        girafeOutput(ns("plot_contrib"), width = "100%", height = "auto")
      )
    )
  )
}

# -----------------------------------------------------------------------------
# Server
# -----------------------------------------------------------------------------

mod_contributions_server <- function(id, contrib_ptp_6cat_r, contrib_moy_6cat_r) {
  moduleServer(id, function(input, output, session) {

    # Vrai si l'horizon choisi est "Moyenne annuelle"
    is_moy <- reactive(input$horizon == "C_a")

    # --- Sélecteur de période dynamique -------------------------------------
    output$ui_periode <- renderUI({
      ns <- session$ns

      if (is_moy()) {
        df <- contrib_moy_6cat_r() |> filter(categorie == input$categorie)
        annees <- sort(unique(df$annee), decreasing = TRUE)
        selectInput(ns("periode"), "Année", choices = annees, selected = annees[1])
      } else {
        df <- contrib_ptp_6cat_r() |> filter(categorie == input$categorie)
        dates <- sort(unique(df$date), decreasing = TRUE)
        selectInput(
          ns("periode"), "Période",
          choices  = setNames(as.character(dates), format(dates, "%B %Y")),
          selected = as.character(dates[1])
        )
      }
    })

    # --- Données filtrées pour le graphique ---------------------------------
    df_filtre <- reactive({
      req(input$periode)
      cat_sel <- input$categorie

      if (is_moy()) {
        annee_sel <- as.integer(input$periode)
        contrib_moy_6cat_r() |>
          filter(categorie == cat_sel, annee == annee_sel) |>
          mutate(libelle_court = noms_courts[libelle])
      } else {
        date_sel <- as.Date(input$periode)
        contrib_ptp_6cat_r() |>
          filter(categorie == cat_sel, date == date_sel) |>
          mutate(libelle_court = noms_courts[libelle])
      }
    })

    # Colonne à afficher selon horizon + type
    col_affichee <- reactive({
      if (is_moy()) {
        if (input$type_contrib == "rel") "CR_a" else "C_a"
      } else {
        horizon <- input$horizon  # C_m, C_t ou C_y
        if (input$type_contrib == "rel") {
          sub("^C_", "CR_", horizon)  # C_m -> CR_m, C_t -> CR_t, C_y -> CR_y
        } else {
          horizon
        }
      }
    })

    # --- Titre dynamique ----------------------------------------------------
    output$titre_carte <- renderText({
      req(input$periode)
      horizon_label <- switch(input$horizon,
        C_m = "Mensuelle",
        C_t = "Trimestrielle",
        C_y = "Glissement annuel",
        C_a = "Moyenne annuelle"
      )
      type_label <- if (input$type_contrib == "abs") "absolue (pp)" else "relative (%)"
      periode_label <- if (is_moy()) {
        input$periode
      } else {
        format(as.Date(input$periode), "%B %Y")
      }
      sprintf("Contribution %s %s — %s | %s",
              horizon_label, type_label, input$categorie, periode_label)
    })

    # --- Graphique ----------------------------------------------------------
    output$plot_contrib <- renderGirafe({
      df  <- df_filtre()
      col <- col_affichee()
      req(nrow(df) > 0, col %in% names(df))

      somme <- sum(df[[col]], na.rm = TRUE)
      unite <- if (input$type_contrib == "abs") "pp" else "%"
      sous_titre <- sprintf("Σ contributions : %+.2f %s", somme, unite)

      p <- df |>
        mutate(libelle_court = reorder(libelle_court, .data[[col]])) |>
        ggplot(aes(x = .data[[col]], y = libelle_court)) +
        geom_col_interactive(
          aes(
            tooltip = paste0(
              "<b>", libelle_court, "</b><br/>",
              sprintf("%+.4f %s", .data[[col]], unite)
            ),
            data_id = libelle_court
          )
        ) +
        geom_vline(xintercept = 0, linewidth = 0.4) +
        geom_text(
          aes(
            label = sprintf("%+.2f %s", .data[[col]], unite),
            hjust = if_else(.data[[col]] >= 0, -0.1, 1.1)
          ),
          size = 3.5
        ) +
        labs(
          subtitle = sous_titre,
          x        = if (input$type_contrib == "abs")
            "Contribution (points de pourcentage)" else "Contribution relative (%)",
          y       = NULL,
          caption = "Source : INSTAT/DSCVM/IPC"
        ) +
        scale_x_continuous(expand = expansion(mult = 0.28)) +
        theme_minimal(base_size = 12)

      girafe(ggobj = p, width_svg = 10, height_svg = 5.2, options = girafe_opts())
    })
  })
}
