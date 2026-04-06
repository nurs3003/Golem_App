#' The application User-Interface
#'
#' @param request Internal parameter for \code{{shiny}}. DO NOT remove.
#' @noRd
#' @importFrom shiny tagList div
#' @importFrom bslib page_navbar nav_panel bs_theme nav_spacer
app_ui <- function(request) {
  tagList(
    golem_add_external_resources(),
    shinyjs::useShinyjs(),
    bslib::page_navbar(
      id       = "main_tabs",
      title    = "Market Dynamics Explorer",
      theme    = bslib::bs_theme(bootswatch = "flatly", version = 5),
      fillable = TRUE,
      # Pinned selector bar — hidden on Overview tab via shinyjs
      header   = shiny::div(id = "selector_bar", mod_market_selector_ui("selector")),
      bslib::nav_panel("Overview",     fillable = FALSE, mod_market_overview_ui("overview")),
      bslib::nav_panel("Fundamentals", fillable = TRUE,  mod_storage_ui("storage")),
      bslib::nav_panel("Curves",       fillable = TRUE,  mod_forward_curve_ui("fc")),
      bslib::nav_panel("Volatility",   fillable = TRUE,  mod_volatility_ui("vol")),
      bslib::nav_panel("Correlations", fillable = TRUE,  mod_codynamics_ui("codyn")),
      bslib::nav_panel("Seasonality",  fillable = TRUE,  mod_seasonality_ui("seas")),
      bslib::nav_panel("Spreads",      fillable = TRUE,  mod_spreads_ui("spreads")),
      bslib::nav_panel("Hedging",      fillable = TRUE,  mod_hedge_ratios_ui("hedge"))
    )
  )
}

#' Add external Resources to the Application
#'
#' @noRd
#' @importFrom shiny tags
golem_add_external_resources <- function() {
  shiny::tags$head(
    shiny::tags$title("Market Dynamics Explorer"),
    # Allow selectize dropdown to overflow the pinned header bar
    shiny::tags$style(
      ".selectize-dropdown { z-index: 9999 !important; }"
    )
  )
}
