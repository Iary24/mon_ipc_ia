# =============================================================================
# module/mod_ipc_global.R — Vue d'ensemble de l'IPC global
# =============================================================================
# Affiche :
#   - 3 value boxes : dernier indice, variation mensuelle, variation annuelle
#   - Onglet 1 : Variation mensuelle IPC Ensemble (courbe + curseur de plage)
#   - Onglet 2 : Évolution multi-séries avec choix En niveau / Variation mensuelle

# -----------------------------------------------------------------------------
# UI
# -----------------------------------------------------------------------------

mod_ipc_global_ui <- function(id) {
  ns <- NS(id)

  tagList(
    # Value boxes — toujours ancrées sur le dernier mois disponible
    layout_column_wrap(
      width = 1 / 3,
      fill  = FALSE,
      value_box(
        title    = "Dernier indice (Ensemble)",
        value    = uiOutput(ns("vb_indice")),
        theme    = "primary",
        showcase = bs_icon("bar-chart-line")
      ),
      value_box(
        title    = "Variation mensuelle",
        value    = uiOutput(ns("vb_mensuel")),
        theme    = "info",
        showcase = bs_icon("calendar-month")
      ),
      value_box(
        title    = "Glissement annuel",
        value    = uiOutput(ns("vb_annuel")),
        theme    = "warning",
        showcase = bs_icon("graph-up-arrow")
      )
    ),

    navset_card_underline(
      full_screen = TRUE,

      # ---- Onglet 1 : Variation mensuelle IPC Ensemble ----------------------
      nav_panel(
        title = tagList(bs_icon("activity"), " Variation mensuelle"),
        layout_sidebar(
          sidebar = sidebar(
            width = 280,
            title = "Période",
            uiOutput(ns("ui_slider_var"))
          ),
          girafeOutput(ns("plot_var_mensuelle"), width = "100%", height = "auto")
        )
      ),

      # ---- Onglet 2 : Évolution multi-séries --------------------------------
      nav_panel(
        title = tagList(bs_icon("graph-up"), " Évolution de l'indice"),
        layout_sidebar(
          sidebar = sidebar(
            width = 280,
            title = "Options",

            uiOutput(ns("ui_slider_evol")),

            hr(),

            radioButtons(
              ns("mode_evol"),
              "Affichage",
              choices  = c("En niveau"           = "niveau",
                           "Variation mensuelle"  = "var_m"),
              selected = "niveau",
              inline   = TRUE
            ),

            uiOutput(ns("ui_series"))
          ),
          girafeOutput(ns("plot_evol_indice"), width = "100%", height = "auto")
        )
      )
    )
  )
}

# -----------------------------------------------------------------------------
# Helpers UI internes
# -----------------------------------------------------------------------------

.slider_dates <- function(ns_id, df, ns_fn) {
  date_min <- min(df$date)
  date_max <- max(df$date)
  sliderInput(
    ns_fn(ns_id),
    label      = NULL,
    min        = date_min,
    max        = date_max,
    value      = c(date_min, date_max),
    step       = 1,
    timeFormat = "%b %Y",
    width      = "100%"
  )
}

.choices_series <- function() {
  c(
    list("Global" = c("Ensemble" = "Ensemble")),
    lapply(categories_def, \(v) setNames(v, v))
  )
}

# -----------------------------------------------------------------------------
# Server
# -----------------------------------------------------------------------------

mod_ipc_global_server <- function(id, ipc_long_rv) {
  moduleServer(id, function(input, output, session) {

    # --- Série Ensemble avec variation mensuelle ----------------------------
    ensemble <- reactive({
      req(ipc_long_rv())
      ipc_long_rv() |>
        filter(libelle == "Ensemble") |>
        arrange(date) |>
        mutate(var_m = (ipc / lag(ipc) - 1) * 100)
    })

    # --- Value boxes -------------------------------------------------------
    valeurs_cles <- reactive({
      df     <- ensemble()
      last   <- tail(df, 1)
      prev12 <- df |> slice(n() - 12)
      list(
        indice = last$ipc,
        date   = last$date,
        var_m  = last$var_m,
        var_y  = 100 * (last$ipc / prev12$ipc - 1)
      )
    })

    output$vb_indice <- renderUI({
      v <- valeurs_cles()
      tagList(
        strong(sprintf("%.2f", v$indice)),
        div(class = "small", format(v$date, "%B %Y"))
      )
    })

    output$vb_mensuel <- renderUI({
      v  <- valeurs_cles()
      cl <- if (isTRUE(v$var_m >= 0)) "text-danger" else "text-success"
      div(class = cl, sprintf("%+.2f%%", v$var_m))
    })

    output$vb_annuel <- renderUI({
      v  <- valeurs_cles()
      cl <- if (isTRUE(v$var_y >= 0)) "text-danger" else "text-success"
      div(class = cl, sprintf("%+.2f%%", v$var_y))
    })

    # --- Curseurs de plage -------------------------------------------------
    output$ui_slider_var <- renderUI({
      df <- ensemble()
      req(nrow(df) > 1)
      .slider_dates("plage_var", df, session$ns)
    })

    output$ui_slider_evol <- renderUI({
      df <- ensemble()
      req(nrow(df) > 1)
      .slider_dates("plage_evol", df, session$ns)
    })

    # --- Sélecteur de séries -----------------------------------------------
    output$ui_series <- renderUI({
      req(ipc_long_rv())
      selectizeInput(
        session$ns("series_sel"),
        label    = "Séries à afficher",
        choices  = .choices_series(),
        selected = "Ensemble",
        multiple = TRUE,
        options  = list(
          placeholder = "Ajouter une série…",
          plugins     = list("remove_button")
        )
      )
    })

    # --- Onglet 1 : Variation mensuelle Ensemble ---------------------------
    output$plot_var_mensuelle <- renderGirafe({
      req(input$plage_var)
      df <- ensemble() |>
        filter(
          lubridate::floor_date(date, 'month') >= lubridate::floor_date(input$plage_var[1], 'month'),
          lubridate::floor_date(date, 'month') <= lubridate::floor_date(input$plage_var[2], 'month'),
          !is.na(var_m)
        )
      req(nrow(df) > 1)

      p <- ggplot(df, aes(x = date, y = var_m)) +
        geom_hline(yintercept = 0, linewidth = 0.4, colour = "grey60") +
        geom_line(linewidth = 0.7, colour = "#2C7BB6") +
        geom_point_interactive(
          aes(
            tooltip = paste0("<b>", format(date, "%B %Y"), "</b><br/>",
                             "Variation : ", sprintf("%+.2f%%", var_m)),
            data_id = as.character(date)
          ),
          size = 1, colour = "#2C7BB6"
        ) +
        geom_point_interactive(
          data = \(d) slice_max(d, date, n = 1),
          aes(
            tooltip = paste0("<b>", format(date, "%B %Y"), "</b><br/>",
                             "Variation : ", sprintf("%+.2f%%", var_m)),
            data_id = as.character(date)
          ),
          size = 3, colour = "#D7191C"
        ) +
        labs(
          subtitle = paste(format(min(df$date), "%B %Y"), "–",
                           format(max(df$date), "%B %Y")),
          x = NULL, y = "Variation mensuelle (%)",
          caption = "Source : INSTAT/DSCVM/IPC"
        ) +
        scale_x_date(date_breaks = "6 months", date_labels = "%b %Y") +
        scale_y_continuous(labels = scales::label_percent(scale = 1)) +
        theme_minimal(base_size = 12) +
        theme(axis.text.x = element_text(angle = 30, hjust = 1))

      girafe(ggobj = p, width_svg = 10, height_svg = 4.2, options = girafe_opts())
    })

    # --- Onglet 2 : Évolution multi-séries ---------------------------------
    output$plot_evol_indice <- renderGirafe({
      req(input$plage_evol, input$series_sel, input$mode_evol, ipc_long_rv())

      mode <- input$mode_evol

      df <- ipc_long_rv() |>
        filter(libelle %in% input$series_sel) |>
        arrange(libelle, date) |>
        group_by(libelle) |>
        mutate(
          var_m = (ipc / lag(ipc) - 1) * 100,
          val   = if (mode == "niveau") ipc else var_m
        ) |>
        ungroup() |>
        filter(
          lubridate::floor_date(date, "month") >= lubridate::floor_date(input$plage_evol[1], "month"),
          lubridate::floor_date(date, "month") <= lubridate::floor_date(input$plage_evol[2], "month"),
          !is.na(val)
        ) |>
        mutate(
          serie        = if_else(libelle %in% names(noms_courts),
                                 noms_courts[libelle], libelle),
          est_ensemble = libelle == "Ensemble"
        )

      req(nrow(df) > 0)

      # Labels selon le mode
      y_label  <- if (mode == "niveau")
        "Indice des prix (base 100 = juillet 2016)"
      else
        "Variation mensuelle (%)"

      tooltip_val <- if (mode == "niveau")
        sprintf("Indice : %.2f", df$val)
      else
        sprintf("Variation : %+.2f%%", df$val)

      df <- df |> mutate(tooltip_txt = paste0(
        "<b>", serie, "</b><br/>",
        format(date, "%B %Y"), "<br/>",
        tooltip_val
      ))

      p <- ggplot(df, aes(x = date, y = val, colour = serie)) +
        geom_line(aes(linewidth = est_ensemble)) +
        geom_point_interactive(
          aes(tooltip = tooltip_txt, data_id = paste(serie, date)),
          size = 1.5
        ) +
        scale_linewidth_manual(
          values = c("TRUE" = 1.1, "FALSE" = 0.6),
          guide  = "none"
        ) +
        labs(
          subtitle = paste(format(min(df$date), "%B %Y"), "–",
                           format(max(df$date), "%B %Y")),
          x = NULL, y = y_label, colour = NULL,
          caption = "Source : INSTAT/DSCVM/IPC"
        ) +
        scale_x_date(date_breaks = "6 months", date_labels = "%b %Y") +
        theme_minimal(base_size = 12) +
        theme(
          axis.text.x     = element_text(angle = 30, hjust = 1),
          legend.position = "bottom"
        )

      # Axe Y en % pour variation mensuelle + ligne zéro
      if (mode == "var_m") {
        p <- p +
          geom_hline(yintercept = 0, linewidth = 0.4, colour = "grey60") +
          scale_y_continuous(labels = scales::label_percent(scale = 1))
      }

      girafe(ggobj = p, width_svg = 10, height_svg = 4.2, options = girafe_opts())
    })
  })
}
