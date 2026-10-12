# =============================================================================
# module/mod_carte.R — Cartographie spatiale de l'IPC par zone d'enquête
# =============================================================================
# Deux cartes de Madagascar côte à côte selon le mode choisi :
#
#   Mode Mensuel  → Carte 1 : Glissement annuel  (ipc_t / ipc_{t−12} − 1)
#                   Carte 2 : Variation mensuelle (ipc_t / ipc_{t−1}  − 1)
#
#   Mode Annuel   → Carte 1 : Variation de la moyenne annuelle
#                   Carte 2 : Glissement de fin de période (décembre N / décembre N−1)
#
# Les zones d'enquête sont les 7 villes IPC (provinces_coord dans global.R).

# -----------------------------------------------------------------------------
# UI
# -----------------------------------------------------------------------------

mod_carte_ui <- function(id) {
  ns <- NS(id)

  tagList(
    layout_sidebar(
      sidebar = sidebar(
        width = 250,
        title = "Paramètres",

        radioButtons(
          ns("horizon"),
          "Type de période",
          choices  = c("Mensuel" = "mensuel", "Annuel" = "annuel"),
          selected = "mensuel"
        ),

        uiOutput(ns("ui_periode")),

        hr(),

        uiOutput(ns("ui_note"))
      ),

      layout_column_wrap(
        width = 1 / 2,

        card(
          full_screen = TRUE,
          card_header(uiOutput(ns("titre_carte1"))),
          girafeOutput(ns("carte1"), width = "100%", height = "auto")
        ),

        card(
          full_screen = TRUE,
          card_header(uiOutput(ns("titre_carte2"))),
          girafeOutput(ns("carte2"), width = "100%", height = "auto")
        )
      )
    )
  )
}

# -----------------------------------------------------------------------------
# Helpers internes
# -----------------------------------------------------------------------------

# Calcule les deux métriques selon le mode et la période sélectionnée.
# Retourne un tibble : libelle | taux_c1 | taux_c2
.calc_taux_geo <- function(ipc_geo, horizon, periode) {
  df <- ipc_geo |> arrange(libelle, date)

  if (horizon == "mensuel") {
    date_sel <- as.Date(periode)

    result <- df |>
      group_by(libelle) |>
      mutate(
        ipc_lag1  = lag(ipc,  1),   # mois précédent
        ipc_lag12 = lag(ipc, 12)    # même mois année précédente
      ) |>
      ungroup() |>
      filter(date == date_sel, !is.na(ipc_lag12), !is.na(ipc_lag1)) |>
      mutate(
        taux_c1 = (ipc / ipc_lag12 - 1) * 100,  # glissement annuel
        taux_c2 = (ipc / ipc_lag1  - 1) * 100   # variation mensuelle
      ) |>
      select(libelle, taux_c1, taux_c2)

  } else {
    annee_sel <- as.integer(periode)

    # Carte 1 : variation de la moyenne annuelle
    moyennes <- df |>
      mutate(annee = lubridate::year(date)) |>
      group_by(libelle, annee) |>
      filter(n() == 12) |>
      summarise(ipc_moy = mean(ipc), .groups = "drop")

    c1 <- moyennes |>
      group_by(libelle) |>
      mutate(ipc_moy_lag = lag(ipc_moy)) |>
      ungroup() |>
      filter(annee == annee_sel, !is.na(ipc_moy_lag)) |>
      mutate(taux_c1 = (ipc_moy / ipc_moy_lag - 1) * 100) |>
      select(libelle, taux_c1)

    # Carte 2 : glissement de fin de période (décembre N vs décembre N−1)
    c2 <- df |>
      filter(lubridate::month(date) == 12) |>
      mutate(annee = lubridate::year(date)) |>
      group_by(libelle) |>
      arrange(date) |>
      mutate(ipc_dec_lag = lag(ipc, 1)) |>
      ungroup() |>
      filter(annee == annee_sel, !is.na(ipc_dec_lag)) |>
      mutate(taux_c2 = (ipc / ipc_dec_lag - 1) * 100) |>
      select(libelle, taux_c2)

    result <- left_join(c1, c2, by = "libelle")
  }

  if (nrow(result) == 0) return(NULL)
  result
}

# Fond de carte : polygone Madagascar depuis map_data (package maps)
.base_map <- function() {
  list(
    geom_polygon(
      data        = madagascar_map,
      aes(x = long, y = lat, group = group),
      fill        = "#F0F0F0",
      colour      = "#AAAAAA",
      linewidth   = 0.3,
      inherit.aes = FALSE
    )
  )
}

# Construit une carte girafe : fond + bulles colorées par la métrique choisie.
# Les limites de la palette sont calées sur la plage réelle des données.
.carte_girafe <- function(df, col_valeur, titre_legende, periode_label) {

  val_min <- min(df[[col_valeur]], na.rm = TRUE)
  val_max <- max(df[[col_valeur]], na.rm = TRUE)
  marge   <- (val_max - val_min) * 0.08

  # Ancrage de 0 pour la palette divergente
  lim_low  <- min(val_min - marge, 0)
  lim_high <- max(val_max + marge, 0)

  ggplot() +
    .base_map() +
    geom_point_interactive(
      data  = df,
      aes(
        x       = lon,
        y       = lat,
        fill    = .data[[col_valeur]],
        size    = abs(.data[[col_valeur]]),
        tooltip = paste0(
          "<b>", libelle, "</b><br/>",
          titre_legende, " : ",
          sprintf("%+.2f%%", .data[[col_valeur]])
        ),
        data_id = libelle
      ),
      shape  = 21,
      colour = "white",
      stroke = 0.6
    ) +
    geom_text(
      data   = df,
      aes(x = lon, y = lat, label = libelle),
      vjust  = -1.3,
      size   = 2.8,
      colour = "grey25"
    ) +
    scale_size_continuous(range = c(5, 16), guide = "none") +
    scale_fill_gradient2(
      low      = "#2166AC",
      mid      = "#F7F7F7",
      high     = "#D73027",
      midpoint = 0,
      limits   = c(lim_low, lim_high),
      name     = titre_legende,
      labels   = scales::label_percent(scale = 1, suffix = "%")
    ) +
    coord_quickmap() +
    labs(subtitle = periode_label, caption = "Source : INSTAT/DSCVM/IPC") +
    theme_void(base_size = 11) +
    theme(
      legend.position  = "bottom",
      legend.key.width = unit(1.8, "cm"),
      plot.subtitle    = element_text(hjust = 0.5, colour = "grey40", size = 10),
      plot.caption     = element_text(hjust = 1,   colour = "grey60", size = 8)
    )
}

# -----------------------------------------------------------------------------
# Server
# -----------------------------------------------------------------------------

mod_carte_server <- function(id, ipc_geo_rv) {
  moduleServer(id, function(input, output, session) {

    # --- Libellés dynamiques selon le mode --------------------------------
    meta <- reactive({
      if (input$horizon == "mensuel") {
        list(
          titre1 = tagList(bs_icon("thermometer-half"),
                           " Glissement annuel par zone (%)"),
          titre2 = tagList(bs_icon("arrow-left-right"),
                           " Variation mensuelle par zone (%)"),
          legende1 = "Glissement annuel (%)",
          legende2 = "Variation mensuelle (%)",
          note = "Carte 1 : IPC du mois sélectionné vs même mois N−1.
                  Carte 2 : IPC du mois sélectionné vs mois précédent."
        )
      } else {
        list(
          titre1 = tagList(bs_icon("thermometer-half"),
                           " Variation de la moyenne annuelle (%)"),
          titre2 = tagList(bs_icon("flag"),
                           " Glissement de fin de période — décembre (%)"),
          legende1 = "Variation moy. annuelle (%)",
          legende2 = "Glissement déc. (%)",
          note = "Carte 1 : variation de la moyenne annuelle de l'IPC.
                  Carte 2 : IPC de décembre N vs décembre N−1."
        )
      }
    })

    output$titre_carte1 <- renderUI(meta()$titre1)
    output$titre_carte2 <- renderUI(meta()$titre2)

    output$ui_note <- renderUI({
      p(class = "text-muted small", meta()$note)
    })

    # --- Sélecteur de période --------------------------------------------
    output$ui_periode <- renderUI({
      df <- ipc_geo_rv()
      req(df)

      if (input$horizon == "mensuel") {
        dates_dispo <- df |>
          group_by(libelle) |>
          arrange(date) |>
          mutate(ipc_lag12 = lag(ipc, 12)) |>
          ungroup() |>
          filter(!is.na(ipc_lag12)) |>
          pull(date) |> unique() |> sort(decreasing = TRUE)

        selectInput(
          session$ns("periode"), "Mois",
          choices  = setNames(as.character(dates_dispo),
                              format(dates_dispo, "%B %Y")),
          selected = as.character(dates_dispo[1])
        )
      } else {
        annees <- df |>
          mutate(annee = lubridate::year(date)) |>
          group_by(libelle, annee) |>
          filter(n() == 12) |>
          ungroup() |>
          pull(annee) |> unique() |> sort(decreasing = TRUE)

        # Retirer la plus ancienne (besoin de N−1 pour les deux cartes)
        annees_dispo <- head(annees, -1)

        selectInput(
          session$ns("periode"), "Année",
          choices  = annees_dispo,
          selected = annees_dispo[1]
        )
      }
    })

    # --- Données calculées -----------------------------------------------
    donnees_carte <- reactive({
      df <- ipc_geo_rv()
      req(df, input$periode)
      .calc_taux_geo(df, input$horizon, input$periode)
    })

    df_carte <- reactive({
      dc <- donnees_carte()
      req(dc, nrow(dc) > 0)
      provinces_coord |>
        left_join(dc, by = "libelle") |>
        filter(!is.na(taux_c1))
    })

    periode_label <- reactive({
      req(input$periode)
      if (input$horizon == "mensuel")
        format(as.Date(input$periode), "%B %Y")
      else
        as.character(input$periode)
    })

    # --- Carte 1 ---------------------------------------------------------
    output$carte1 <- renderGirafe({
      df <- df_carte()
      req(nrow(df) > 0)
      p <- .carte_girafe(df, "taux_c1", meta()$legende1, periode_label())
      girafe(ggobj = p, width_svg = 5, height_svg = 5.5, options = girafe_opts())
    })

    # --- Carte 2 ---------------------------------------------------------
    output$carte2 <- renderGirafe({
      df <- df_carte()
      req(nrow(df) > 0, !all(is.na(df$taux_c2)))
      p <- .carte_girafe(df, "taux_c2", meta()$legende2, periode_label())
      girafe(ggobj = p, width_svg = 5, height_svg = 5.5, options = girafe_opts())
    })
  })
}
