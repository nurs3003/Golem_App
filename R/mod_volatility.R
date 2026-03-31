#' volatility UI Function
#'
#' Two views:
#' \enumerate{
#'   \item Rolling annualised volatility of the front-month contract over time.
#'   \item Volatility surface heatmap: time on x, contract number on y,
#'         colour = annualised vol.
#' }
#'
#' @param id Shiny module id.
#' @noRd
#' @importFrom shiny NS tagList selectInput
#' @importFrom bslib layout_columns card card_header navset_tab nav_panel
#' @importFrom plotly plotlyOutput
mod_volatility_ui <- function(id) {
  ns <- NS(id)
  bslib::layout_columns(
    col_widths = c(3, 9),
    fill       = TRUE,
    bslib::card(
      bslib::card_header("Settings"),
      shiny::selectInput(
        ns("window"),
        "Rolling window (trading days)",
        choices  = c("21 days" = 21, "42 days" = 42, "63 days" = 63),
        selected = 21
      ),
      shiny::selectInput(
        ns("surface_market"),
        "Vol surface market",
        choices  = c("CL","BRN","NG","HO","RB","HTT"),
        selected = "CL"
      )
    ),
    bslib::card(
      fill = TRUE,
      bslib::card_header("Volatility"),
      bslib::navset_tab(
        bslib::nav_panel(
          "Rolling Vol (front month)",
          plotly::plotlyOutput(ns("vol_ts"), height = "100%")
        ),
        bslib::nav_panel(
          "Vol Surface (heatmap)",
          plotly::plotlyOutput(ns("vol_heatmap"), height = "100%")
        )
      )
    )
  )
}

#' volatility Server Function
#'
#' @param id Shiny module id.
#' @param r Shared \code{reactiveValues} environment.
#' @noRd
#' @importFrom shiny moduleServer reactive req
#' @importFrom dplyr filter arrange mutate group_by ungroup select
#' @importFrom tidyr pivot_wider
#' @importFrom slider slide_dbl
#' @importFrom plotly plot_ly add_trace layout renderPlotly
mod_volatility_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {

    # Reactive: log returns for front-month contracts of selected markets
    front_returns <- reactive({
      req(!is.null(r$data), r$selected_markets, r$date_range)
      window_days <- as.integer(input$window)

      r$data |>
        dplyr::filter(
          market   %in% r$selected_markets,
          contract == 1L,
          date     >= r$date_range[1],
          date     <= r$date_range[2]
        ) |>
        dplyr::arrange(market, date) |>
        dplyr::group_by(market) |>
        dplyr::mutate(
          log_ret  = c(NA_real_, diff(log(value))),
          roll_vol = slider::slide_dbl(
            log_ret,
            ~ stats::sd(.x, na.rm = TRUE) * sqrt(252),
            .before   = window_days - 1L,
            .complete = TRUE
          )
        ) |>
        dplyr::ungroup()
    })

    # Rolling vol time-series
    output$vol_ts <- plotly::renderPlotly({
      df <- front_returns()
      req(nrow(df) > 0L)

      p <- plotly::plot_ly()
      for (mkt in unique(df$market)) {
        sub <- dplyr::filter(df, market == mkt, !is.na(roll_vol))
        p <- plotly::add_trace(
          p,
          data = sub, x = ~date, y = ~roll_vol,
          type = "scatter", mode = "lines",
          name = mkt,
          hovertemplate = "%{x|%Y-%m-%d}<br>Ann. Vol: %{y:.1%}<extra></extra>"
        )
      }
      plotly::layout(
        p,
        xaxis     = list(title = "Date"),
        yaxis     = list(title = "Annualised Volatility", tickformat = ".0%"),
        hovermode = "x unified",
        legend    = list(orientation = "h", y = -0.2)
      )
    })

    # Vol surface heatmap for selected market
    output$vol_heatmap <- plotly::renderPlotly({
      req(!is.null(r$data), r$date_range)
      mkt         <- input$surface_market
      window_days <- as.integer(input$window)

      surface_df <- r$data |>
        dplyr::filter(
          market == mkt,
          date   >= r$date_range[1],
          date   <= r$date_range[2]
        ) |>
        dplyr::arrange(contract, date) |>
        dplyr::group_by(contract) |>
        dplyr::mutate(
          log_ret  = c(NA_real_, diff(log(value))),
          roll_vol = slider::slide_dbl(
            log_ret,
            ~ stats::sd(.x, na.rm = TRUE) * sqrt(252),
            .before   = window_days - 1L,
            .complete = TRUE
          )
        ) |>
        dplyr::ungroup() |>
        dplyr::filter(!is.na(roll_vol))

      # Downsample dates for heatmap performance (weekly)
      surface_df <- surface_df |>
        dplyr::filter(lubridate::wday(date) == 2L)  # Mondays only

      wide <- tidyr::pivot_wider(
        surface_df,
        id_cols     = date,
        names_from  = contract,
        values_from = roll_vol
      )

      mat      <- as.matrix(wide[, -1])
      dates    <- wide$date
      contracts <- as.integer(colnames(wide)[-1])

      plotly::plot_ly(
        x         = dates,
        y         = contracts,
        z         = t(mat),
        type      = "heatmap",
        colorscale = "RdYlGn",
        reversescale = TRUE,
        hovertemplate = "Date: %{x|%Y-%m-%d}<br>Contract: %{y}<br>Vol: %{z:.1%}<extra></extra>"
      ) |>
        plotly::layout(
          xaxis = list(title = "Date"),
          yaxis = list(title = "Contract #"),
          title = paste0(mkt, " Volatility Surface")
        )
    })

  })
}
