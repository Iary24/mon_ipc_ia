# =============================================================================
# ui.R — Interface Shiny | IPC Madagascar
# =============================================================================

source("module/mod_import.R")
source("module/mod_ipc_global.R")
source("module/mod_inflation.R")
source("module/mod_contributions.R")
source("module/mod_stl.R")
source("module/mod_carte.R")

ui <- page_navbar(
  title = "IPC Madagascar",
  theme = bs_theme(
    version  = 5,
    bootswatch = "flatly",
    base_font  = font_google("Inter")
  ),
  lang = "fr",

  # ---- Onglet 1 : Vue d'ensemble ------------------------------------------
  nav_panel(
    title = tagList(bs_icon("house"), " Vue d'ensemble"),
    mod_ipc_global_ui("ipc_global")
  ),

  # ---- Onglet 2 : Taux d'inflation ----------------------------------------
  nav_panel(
    title = tagList(bs_icon("graph-up"), " Taux d'inflation"),
    mod_inflation_ui("inflation")
  ),

  # ---- Onglet 3 : Contributions -------------------------------------------
  nav_panel(
    title = tagList(bs_icon("pie-chart"), " Contributions"),
    mod_contributions_ui("contributions")
  ),

  # ---- Onglet 4 : Décomposition STL ---------------------------------------
  nav_panel(
    title = tagList(bs_icon("layers"), " Décomposition STL"),
    mod_stl_ui("stl")
  ),

  # ---- Onglet 5 : Cartographie spatiale -----------------------------------
  nav_panel(
    title = tagList(bs_icon("geo-alt"), " Cartographie"),
    mod_carte_ui("carte")
  ),

  # ---- Onglet 6 : Import des données --------------------------------------
  nav_panel(
    title = tagList(bs_icon("cloud-upload"), " Import données"),
    mod_import_ui("import")
  ),

  # Séparateur + mention source à droite
  nav_spacer(),
  nav_item(
    tags$span(
      class = "navbar-text text-muted small",
      "Source : INSTAT/DSCVM"
    )
  )
)
