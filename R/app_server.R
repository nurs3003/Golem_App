#' The application server-side
#'
#' @param input,output,session Internal parameters for \code{{shiny}}.
#' @noRd
#' @importFrom shiny reactiveValues observe req showNotification removeNotification
app_server <- function(input, output, session) {

  # ---------------------------------------------------------------------------
  # Small-r strategy: one shared reactive environment passed to all modules.
  # mod_market_selector WRITES; all analysis modules READ.
  # ---------------------------------------------------------------------------
  r <- reactiveValues(
    data             = NULL,   # parsed RTL::dflong  (date, market, contract, value)
    cmt_data         = NULL,   # FRED CMT yields     (date, maturity_label, maturity_years, value)
    selected_markets = c("CL", "BRN", "NG"),
    date_range       = c(as.Date("2007-01-01"), Sys.Date())
  )

  # Hide the selector bar on Overview (it has no effect there)
  shiny::observeEvent(input$main_tabs, {
    if (isTRUE(input$main_tabs == "Overview")) {
      shinyjs::hide("selector_bar")
    } else {
      shinyjs::show("selector_bar")
    }
  }, ignoreNULL = FALSE)

  # Load data once at session start
  observe({
    showNotification("Loading futures data...", id = "load_fut", duration = NULL, type = "message")
    r$data <- fct_load_futures_data()
    removeNotification("load_fut")

    showNotification("Fetching Treasury yields from FRED...", id = "load_cmt", duration = NULL, type = "message")
    r$cmt_data <- fct_load_cmt_data()
    removeNotification("load_cmt")

    showNotification("Data ready.", type = "message", duration = 3)
  })

  # Module servers
  mod_market_selector_server("selector", r)
  mod_market_overview_server("overview", r)
  mod_forward_curve_server("fc",         r)
  mod_volatility_server("vol",           r)
  mod_codynamics_server("codyn",         r)
  mod_seasonality_server("seas",         r)
  mod_spreads_server("spreads",          r)
  mod_hedge_ratios_server("hedge",       r)
}
