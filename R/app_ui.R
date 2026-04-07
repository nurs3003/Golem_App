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
    shiny::tags$style("
      /* ── Selectize fix ───────────────────────── */
      .selectize-dropdown { z-index: 9999 !important; }

      /* ── Navbar ──────────────────────────────── */
      .navbar {
        position: sticky !important;
        top: 0 !important;
        z-index: 1030 !important;
        box-shadow: 0 2px 6px rgba(0,0,0,0.10) !important;
      }
      .navbar-brand { font-weight: 700 !important; letter-spacing: 0.01em; }

      /* ── Cards ───────────────────────────────── */
      .card {
        box-shadow: 0 2px 8px rgba(0,0,0,0.07) !important;
        border: 1px solid rgba(0,0,0,0.08) !important;
      }
      .card-header {
        font-weight: 600 !important;
        font-size: 0.88rem !important;
        letter-spacing: 0.02em;
      }

      /* ── Value boxes ─────────────────────────── */
      .bslib-value-box {
        border-radius: 10px !important;
        transition: transform 0.15s ease, box-shadow 0.15s ease !important;
      }
      .bslib-value-box:hover {
        transform: translateY(-3px) !important;
        box-shadow: 0 8px 20px rgba(0,0,0,0.13) !important;
        cursor: default;
      }
      .bslib-value-box .value-box-title {
        font-size: 0.75rem !important;
        font-weight: 600 !important;
        text-transform: uppercase !important;
        letter-spacing: 0.06em !important;
        opacity: 0.85;
      }
      .bslib-value-box .value-box-value {
        font-size: 1.35rem !important;
        font-weight: 700 !important;
      }

      /* ── Selector bar labels ─────────────────── */
      #selector_bar .fw-semibold {
        font-size: 0.75rem !important;
        letter-spacing: 0.05em !important;
        text-transform: uppercase !important;
      }

      /* ── DT table ────────────────────────────── */
      table.dataTable td, table.dataTable th {
        font-size: 0.875rem !important;
        padding: 5px 10px !important;
      }

      /* ── Tab polish ──────────────────────────── */
      .nav-tabs .nav-link {
        font-size: 0.82rem !important;
        font-weight: 500 !important;
        letter-spacing: 0.02em;
      }
      .nav-tabs .nav-link.active {
        font-weight: 700 !important;
      }
    ")
  )
}
