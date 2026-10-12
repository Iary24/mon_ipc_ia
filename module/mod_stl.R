# =============================================================================
# module/mod_stl.R — Décomposition STL de l'IPC Ensemble
# =============================================================================
# La méthode STL (Seasonal and Trend decomposition using Loess) décompose la
# série en trois composantes additives : Tendance, Saisonnalité, Résidu.
#
# Paramètres utilisateur :
#   - s.window : "periodic" (saisonnalité stable) ou valeur numérique (variable)
#   - robust   : TRUE/FALSE (atténue l'influence des points aberrants)

# -----------------------------------------------------------------------------
# UI
# -----------------------------------------------------------------------------

mod_stl_ui <- function(id) {
  ns <- NS(id)

  tagList(
    layout_sidebar(
      sidebar = sidebar(
        title = "Paramètres STL",
        width = 240,

        radioButtons(
          ns("s_window"),
          "Fenêtre saisonnière",
          choices  = c("Périodique (stable)" = "periodic",
                       "Variable (11 mois)"  = "11"),
          selected = "periodic"
        ),

        input_switch(
          ns("robust"),
          "Estimation robuste",
          value = TRUE
        ),

        hr(),
        p(class = "text-muted small",
          "L'estimation robuste atténue l'influence des points aberrants sur la tendance.")
      ),

      card(
        full_screen = TRUE,
        card_header("Décomposition STL de l'IPC (Ensemble)"),
        girafeOutput(ns("plot_stl"), width = "100%", height = "auto")
      )
    )
  )
}

# -----------------------------------------------------------------------------
# Server
# -----------------------------------------------------------------------------

mod_stl_server <- function(id, ipc_long_rv) {
  moduleServer(id, function(input, output, session) {

    # Série temporelle ts() de l'IPC Ensemble
    ipc_ts <- reactive({
      df <- ipc_long_rv()
      req(df)

      serie <- df |>
        filter(libelle == "Ensemble") |>
        arrange(date) |>
        pull(ipc)

      # Année et mois de début
      date_debut <- df |>
        filter(libelle == "Ensemble") |>
        summarise(d = min(date)) |>
        pull(d)

      ts(serie,
         start     = c(year(date_debut), month(date_debut)),
         frequency = 12)
    })

    # Décomposition STL réactive
    decomp <- reactive({
      s_win <- input$s_window
      # s.window attend un entier ou "periodic"
      if (s_win != "periodic") s_win <- as.integer(s_win)

      stl(ipc_ts(), s.window = s_win, robust = input$robust)
    })

    # Tableau long pour ggplot
    decomp_df <- reactive({
      dc   <- decomp()
      ts_  <- ipc_ts()

      df_raw <- ipc_long_rv() |>
        filter(libelle == "Ensemble") |>
        arrange(date)

      as.data.frame(dc$time.series) |>
        rename(Saisonnalité = seasonal, Tendance = trend, Résidu = remainder) |>
        mutate(
          date    = df_raw$date,
          Observé = as.numeric(ts_)
        ) |>
        pivot_longer(
          c(Observé, Tendance, Saisonnalité, Résidu),
          names_to  = "composante",
          values_to = "valeur"
        ) |>
        mutate(
          composante = factor(composante,
                              levels = c("Observé", "Tendance", "Saisonnalité", "Résidu"))
        )
    })

    # Graphique
    output$plot_stl <- renderGirafe({
      df <- decomp_df()

      p <- ggplot(df, aes(x = date, y = valeur, color = composante)) +
        geom_line(linewidth = 0.6) +
        # Points invisibles pour tooltip par date × composante
        geom_point_interactive(
          aes(
            tooltip = paste0(
              "<b>", composante, "</b><br/>",
              format(date, "%B %Y"), "<br/>",
              sprintf("%.4f", valeur)
            ),
            data_id = paste(composante, date)
          ),
          size = 1.5, alpha = 0
        ) +
        facet_wrap(~composante, ncol = 1, scales = "free_y") +
        labs(
          title    = "Décomposition STL de l'IPC (Ensemble)",
          subtitle = "Méthode : Seasonal and Trend decomposition using Loess",
          x        = NULL,
          y        = NULL,
          caption  = "Source : INSTAT/DSCVM/IPC"
        ) +
        theme_minimal(base_size = 12) +
        theme(
          strip.text      = element_text(face = "bold"),
          legend.position = "none"
        )

      girafe(ggobj = p, width_svg = 10, height_svg = 7.0, options = girafe_opts())
    })
  })
}
