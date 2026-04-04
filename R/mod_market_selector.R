#' market_selector UI Function
#'
#' Horizontal control bar pinned below the navbar.  Uses a two-column layout:
#' market selectize on the left, date range on the right.
#'
#' @param id Shiny module id.
#' @noRd
#' @importFrom shiny NS tagList selectizeInput dateRangeInput div span
#' @importFrom bslib layout_columns
mod_market_selector_ui <- function(id) {
  ns <- NS(id)

  market_choices <- c(
    "CL – WTI Crude"    = "CL",
    "BRN – Brent"       = "BRN",
    "NG – Natural Gas"  = "NG",
    "HO – Heating Oil"  = "HO",
    "RB – RBOB Gas"     = "RB",
    "HTT – WTI Houston" = "HTT"
  )

  shiny::div(
    class = "container-fluid py-2 px-3 border-bottom bg-light",
    bslib::layout_columns(
      col_widths = c(7, 5),
      gap        = "1rem",
      shiny::div(
        shiny::span("Markets", class = "fw-semibold me-2 small text-muted"),
        shiny::selectizeInput(
          inputId  = ns("markets"),
          label    = NULL,
          choices  = market_choices,
          selected = c("CL", "BRN", "NG"),
          multiple = TRUE,
          width    = "100%",
          options  = list(plugins = list("remove_button"), maxItems = 8)
        )
      ),
      shiny::div(
        shiny::span("Date range", class = "fw-semibold me-2 small text-muted"),
        shiny::dateRangeInput(
          inputId   = ns("date_range"),
          label     = NULL,
          start     = as.Date("2007-01-01"),
          end       = Sys.Date(),
          min       = as.Date("2007-01-01"),
          max       = Sys.Date(),
          separator = "\u2192",
          width     = "100%"
        )
      )
    )
  )
}

#' market_selector Server Function
#'
#' @param id Shiny module id.
#' @param r Shared \code{reactiveValues} environment.
#' @noRd
#' @importFrom shiny moduleServer observe req
mod_market_selector_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {

    observe({
      req(input$markets)
      r$selected_markets <- input$markets
    })

    observe({
      req(input$date_range)
      r$date_range <- input$date_range
    })

  })
}
