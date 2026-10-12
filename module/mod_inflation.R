# =============================================================================
# module/mod_inflation.R — Taux d'inflation (moyenne annuelle & glissement)
# =============================================================================
# Affiche trois graphiques via des onglets :
#   1. Taux d'inflation en moyenne annuelle (barres)
#   2. Glissement annuel de fin de période (barres)
#   3. Comparaison des deux mesures (barres groupées)

# -----------------------------------------------------------------------------
# UI
# -----------------------------------------------------------------------------

mod_inflation_ui <- function(id) {
  ns <- NS(id)

  tagList(
    navset_card_underline(
      title = "Taux d'inflation – IPC global",
      full_screen = TRUE,

      nav_panel(
        "Moyenne annuelle",
        girafeOutput(ns("plot_moy"), width = "100%", height = "auto")
      ),

      nav_panel(
        "Glissement annuel",
        girafeOutput(ns("plot_gliss"), width = "100%", height = "auto")
      ),

      nav_panel(
        "Comparaison",
        girafeOutput(ns("plot_comp"), width = "100%", height = "auto")
      )
    )
  )
}

# -----------------------------------------------------------------------------
# Server
# -----------------------------------------------------------------------------

mod_inflation_server <- function(id, inflation_annuelle_r, inflation_glissement_r) {
  moduleServer(id, function(input, output, session) {

    # --- Graphique 1 : Moyenne annuelle --------------------------------------
    output$plot_moy <- renderGirafe({
      df <- inflation_annuelle_r()
      req(nrow(df) > 0)

      p <- ggplot(df, aes(x = factor(annee), y = inflation)) +
        geom_col_interactive(
          aes(
            tooltip = paste0("<b>", annee, "</b><br/>Inflation : ", sprintf("%.2f%%", inflation)),
            data_id = annee
          )
        ) +
        geom_text(
          aes(label = sprintf("%.1f%%", inflation)),
          vjust = -0.5, size = 3.5
        ) +
        labs(
          title    = "Taux d'inflation annuel – IPC global",
          subtitle = "Variation de la moyenne annuelle de l'IPC",
          x        = NULL,
          y        = "Taux d'inflation (%)",
          caption  = "Source : INSTAT/DSCVM/IPC"
        ) +
        scale_y_continuous(
          labels = label_percent(scale = 1),
          expand = expansion(mult = c(0, 0.12))
        ) +
        theme_minimal(base_size = 12)

      girafe(ggobj = p, width_svg = 10, height_svg = 4.4, options = girafe_opts())
    })

    # --- Graphique 2 : Glissement annuel de fin de période ------------------
    output$plot_gliss <- renderGirafe({
      df <- inflation_glissement_r()
      req(nrow(df) > 0)

      p <- ggplot(df, aes(x = label, y = glissement)) +
        geom_col_interactive(
          aes(
            tooltip = paste0(
              "<b>", annee, "</b>",
              if_else(
                mois != 12,
                paste0(" (", month(make_date(annee, mois), label = TRUE, abbr = TRUE), ")"),
                ""
              ),
              "<br/>Glissement : ", sprintf("%.2f%%", glissement)
            ),
            data_id = as.character(annee)
          )
        ) +
        geom_text(
          aes(label = sprintf("%.1f%%", glissement)),
          vjust = -0.5, size = 3.5
        ) +
        labs(
          title    = "Inflation en glissement annuel de fin de période – IPC global",
          subtitle = "Variation de l'IPC du dernier mois disponible vs même mois de l'année précédente",
          x        = NULL,
          y        = "Glissement annuel (%)",
          caption  = "Source : INSTAT/DSCVM/IPC"
        ) +
        scale_y_continuous(
          labels = label_percent(scale = 1),
          expand = expansion(mult = c(0, 0.12))
        ) +
        theme_minimal(base_size = 12)

      girafe(ggobj = p, width_svg = 10, height_svg = 4.4, options = girafe_opts())
    })

    # --- Graphique 3 : Comparaison des deux mesures -------------------------
    output$plot_comp <- renderGirafe({
      df_moy   <- inflation_annuelle_r()
      df_gliss <- inflation_glissement_r()
      req(nrow(df_moy) > 0, nrow(df_gliss) > 0)

      df_comp <- bind_rows(
        df_moy |>
          select(annee, inflation) |>
          mutate(mesure = "Moyenne annuelle"),
        df_gliss |>
          select(annee, mois, inflation = glissement) |>
          mutate(mesure = "Glissement annuel (fin de période)")
      )

      p <- df_comp |>
        ggplot(aes(x = factor(annee), y = inflation, fill = mesure)) +
        geom_col_interactive(
          aes(
            tooltip = paste0(
              "<b>", annee, "</b><br/>",
              mesure, "<br/>",
              sprintf("%.2f%%", inflation)
            ),
            data_id = paste(annee, mesure)
          ),
          position = "dodge"
        ) +
        geom_text(
          aes(label = sprintf("%.1f%%", inflation)),
          position = position_dodge(width = 0.9),
          vjust = -0.5, size = 3
        ) +
        labs(
          title    = "Inflation – Comparaison des deux mesures",
          subtitle = paste(
            "Glissement annuel de fin de période vs moyenne annuelle",
            "\n(année en cours : glissement uniquement ; moyenne annuelle non disponible)"
          ),
          x       = NULL,
          y       = "Taux d'inflation (%)",
          fill    = NULL,
          caption = "Source : INSTAT/DSCVM/IPC"
        ) +
        scale_y_continuous(
          labels = label_percent(scale = 1),
          expand = expansion(mult = c(0, 0.15))
        ) +
        theme_minimal(base_size = 12) +
        theme(legend.position = "bottom")

      girafe(ggobj = p, width_svg = 10, height_svg = 4.4, options = girafe_opts())
    })
  })
}
