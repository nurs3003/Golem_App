#' seasonality UI Function
#'
#' Two views:
#' \enumerate{
#'   \item Monthly average log-return bar chart — reveals seasonal biases.
#'   \item Box plots of monthly returns — shows dispersion and outliers.
#' }
#'
#' @param id Shiny module id.
#' @noRd
#' @importFrom shiny NS tagList
#' @importFrom bslib layout_columns card card_header navset_tab nav_panel
#' @importFrom plotly plotlyOutput
mod_seasonality_ui <- function(id) {
  ns <- NS(id)
  bslib::card(
    fill = TRUE,
    bslib::card_header(
      "Seasonal Patterns — monthly log returns, front-month contracts"
    ),
    bslib::navset_tab(
      bslib::nav_panel(
        "Average Monthly Return",
        plotly::plotlyOutput(ns("monthly_avg"), height = "100%")
      ),
      bslib::nav_panel(
        "Monthly Distribution (box)",
        plotly::plotlyOutput(ns("monthly_box"), height = "100%")
      )
    )
  )
}

#' seasonality Server Function
#'
#' @param id Shiny module id.
#' @param r Shared \code{reactiveValues} environment.
#' @noRd
#' @importFrom shiny moduleServer reactive req
#' @importFrom dplyr filter arrange mutate group_by ungroup summarise
#' @importFrom lubridate month
#' @importFrom plotly plot_ly layout renderPlotly add_trace
mod_seasonality_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {

    month_labels <- c("Jan","Feb","Mar","Apr","May","Jun",
                      "Jul","Aug","Sep","Oct","Nov","Dec")

    # Reactive: monthly log returns for front-month, selected markets
    monthly_returns <- reactive({
      req(!is.null(r$data), r$selected_markets, r$date_range)
      r$data |>
        dplyr::filter(
          market   %in% r$selected_markets,
          contract == 1L,
          date     >= r$date_range[1],
          date     <= r$date_range[2]
        ) |>
        dplyr::arrange(market, date) |>
        dplyr::group_by(market) |>
        dplyr::mutate(log_ret = c(NA_real_, diff(log(value)))) |>
        dplyr::ungroup() |>
        dplyr::filter(!is.na(log_ret)) |>
        dplyr::mutate(month = lubridate::month(date, label = FALSE))
    })

    # Average return by month
    output$monthly_avg <- plotly::renderPlotly({
      df <- monthly_returns()
      req(nrow(df) > 0L)

      avg_df <- df |>
        dplyr::group_by(market, month) |>
        dplyr::summarise(avg_ret = mean(log_ret, na.rm = TRUE), .groups = "drop") |>
        dplyr::mutate(month_lbl = month_labels[month])

      p <- plotly::plot_ly()
      for (mkt in unique(avg_df$market)) {
        sub <- dplyr::filter(avg_df, market == mkt) |>
          dplyr::arrange(month)
        p <- plotly::add_trace(
          p,
          data          = sub,
          x             = ~month_lbl,
          y             = ~avg_ret,
          type          = "bar",
          name          = mkt,
          hovertemplate = "%{x}: %{y:.2%}<extra></extra>"
        )
      }
      plotly::layout(
        p,
        barmode   = "group",
        xaxis     = list(title = "Month", categoryorder = "array",
                         categoryarray = month_labels),
        yaxis     = list(title = "Avg. Log Return", tickformat = ".1%"),
        hovermode = "x unified",
        legend    = list(orientation = "h", y = -0.2)
      )
    })

    # Box plot of returns by month
    output$monthly_box <- plotly::renderPlotly({
      df <- monthly_returns()
      req(nrow(df) > 0L)

      df <- df |>
        dplyr::mutate(month_lbl = month_labels[month])

      p <- plotly::plot_ly()
      for (mkt in unique(df$market)) {
        sub <- dplyr::filter(df, market == mkt)
        p <- plotly::add_trace(
          p,
          data = sub,
          x    = ~month_lbl,
          y    = ~log_ret,
          type = "box",
          name = mkt,
          hovertemplate = "%{x}<br>%{y:.2%}<extra></extra>"
        )
      }
      plotly::layout(
        p,
        xaxis  = list(title = "Month", categoryorder = "array",
                      categoryarray = month_labels),
        yaxis  = list(title = "Log Return", tickformat = ".1%"),
        legend = list(orientation = "h", y = -0.2)
      )
    })

  })
}
