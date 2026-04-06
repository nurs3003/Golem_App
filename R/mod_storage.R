#' storage UI Function
#'
#' Two views per market:
#' \enumerate{
#'   \item Storage vs 5-year seasonal average (dual-axis with surplus/deficit bars).
#'   \item Implied S/D — week-over-week stock change (all markets).
#' }
#'
#' @param id Shiny module id.
#' @noRd
#' @importFrom shiny NS tagList selectInput tags
#' @importFrom bslib layout_columns card card_header navset_tab nav_panel
#' @importFrom plotly plotlyOutput
mod_storage_ui <- function(id) {
  ns <- NS(id)
  bslib::layout_columns(
    col_widths = c(3, 9),
    fill       = TRUE,
    bslib::card(
      bslib::card_header("Settings"),
      shiny::selectInput(
        ns("market"),
        "Market",
        choices = c(
          "CL – WTI Crude / HTT" = "CL",
          "NG – Natural Gas"     = "NG",
          "HO – Heating Oil"     = "HO",
          "RB – Gasoline"        = "RB"
        ),
        selected = "CL"
      )
    ),
    bslib::card(
      fill = TRUE,
      bslib::card_header("Fundamentals"),
      bslib::navset_tab(
        bslib::nav_panel(
          "Storage vs 5-yr Average",
          plotly::plotlyOutput(ns("storage_chart"), height = "calc(100% - 2.8rem)"),
          shiny::tags$p(
            "Weekly EIA inventory vs the 5-year seasonal average (EIA methodology: prior 5 years, no look-ahead).
             Green bars = surplus (bearish, caps price upside); red bars = deficit (bullish, supports prices).
             A deficit heading into peak demand season is the strongest fundamental buy signal.",
            style = "font-size:0.82rem; color:#666; padding:0.2rem 0.6rem; margin:0;"
          )
        ),
        bslib::nav_panel(
          "Implied S/D",
          plotly::plotlyOutput(ns("implied_sd"), height = "calc(100% - 2.8rem)"),
          shiny::tags$p(
            "Week-over-week stock change = net supply/demand balance.
             Negative (green) = draw, demand exceeded supply — bullish.
             Positive (red) = build, supply exceeded demand — bearish.
             Persistent draws tighten the market; persistent builds loosen it.",
            style = "font-size:0.82rem; color:#666; padding:0.2rem 0.6rem; margin:0;"
          )
        )
      )
    )
  )
}

#' storage Server Function
#'
#' @param id Shiny module id.
#' @param r Shared \code{reactiveValues} environment.
#' @noRd
#' @importFrom shiny moduleServer req validate need
#' @importFrom dplyr filter mutate arrange lag
#' @importFrom plotly plot_ly add_trace layout renderPlotly
mod_storage_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {

    market_to_series <- c(
      CL = "CrudeCushing",
      NG = "NGLower48",
      HO = "Distillate",
      RB = "Gasoline"
    )

    market_units <- c(
      CL = "Thousand Barrels",
      NG = "Bcf",
      HO = "Thousand Barrels",
      RB = "Thousand Barrels"
    )

    # ── Storage vs 5-yr average ───────────────────────────────────────────────
    output$storage_chart <- plotly::renderPlotly({
      mkt       <- input$market
      series_nm <- market_to_series[mkt]
      unit_lbl  <- market_units[mkt]
      col       <- market_colors[mkt] %||% "#2980b9"

      shiny::req(!is.null(r$eia_storage))
      df <- dplyr::filter(r$eia_storage, series == series_nm, !is.na(five_yr_avg))
      shiny::req(nrow(df) > 0L)

      if (!is.null(r$date_range)) {
        df <- dplyr::filter(df, date >= r$date_range[1], date <= r$date_range[2])
      }

      plotly::plot_ly() |>
        plotly::add_trace(
          data = df, x = ~date, y = ~five_yr_avg,
          type = "scatter", mode = "lines", name = "5-yr avg",
          line = list(color = "grey", dash = "dash", width = 1.5),
          hovertemplate = paste0("%{x|%Y-%m-%d}<br>5-yr avg: %{y:,.0f} ", unit_lbl, "<extra></extra>")
        ) |>
        plotly::add_trace(
          data = df, x = ~date, y = ~value,
          type = "scatter", mode = "lines", name = "Actual",
          line = list(color = col, width = 2),
          hovertemplate = paste0("%{x|%Y-%m-%d}<br>Actual: %{y:,.0f} ", unit_lbl, "<extra></extra>")
        ) |>
        plotly::add_trace(
          data = df, x = ~date, y = ~surplus_deficit,
          type = "bar", name = "Surplus / Deficit vs 5-yr avg",
          yaxis = "y2",
          marker = list(color = dplyr::case_when(
            df$surplus_deficit >= 0 ~ "rgba(39,174,96,0.5)",
            TRUE                    ~ "rgba(192,57,43,0.5)"
          )),
          hovertemplate = paste0("%{x|%Y-%m-%d}<br>+/- 5yr: %{y:,.0f} ", unit_lbl, "<extra></extra>")
        ) |>
        plotly::layout(
          xaxis     = list(title = ""),
          yaxis     = list(title = unit_lbl),
          yaxis2    = list(title = paste0("Surplus / Deficit (", unit_lbl, ")"),
                           overlaying = "y", side = "right",
                           showgrid = FALSE, zeroline = TRUE),
          hovermode = "x unified",
          legend    = list(orientation = "h", y = -0.2)
        )
    })

    # ── Implied S/D (week-over-week stock change) ─────────────────────────────
    output$implied_sd <- plotly::renderPlotly({
      mkt       <- input$market
      series_nm <- market_to_series[mkt]
      unit_lbl  <- market_units[mkt]

      shiny::req(!is.null(r$eia_storage))
      df <- dplyr::filter(r$eia_storage, series == series_nm) |>
        dplyr::arrange(date) |>
        dplyr::mutate(wow_change = value - dplyr::lag(value)) |>
        dplyr::filter(!is.na(wow_change))

      if (!is.null(r$date_range)) {
        df <- dplyr::filter(df, date >= r$date_range[1], date <= r$date_range[2])
      }

      shiny::req(nrow(df) > 0L)

      plotly::plot_ly() |>
        plotly::add_trace(
          data = df, x = ~date, y = ~wow_change,
          type = "bar", name = "Week-over-week change",
          marker = list(color = dplyr::case_when(
            df$wow_change <= 0 ~ "rgba(39,174,96,0.6)",
            TRUE               ~ "rgba(192,57,43,0.6)"
          )),
          hovertemplate = paste0("%{x|%Y-%m-%d}<br>Change: %{y:,.0f} ", unit_lbl, "<extra></extra>")
        ) |>
        plotly::layout(
          xaxis     = list(title = ""),
          yaxis     = list(title = paste0("WoW Change (", unit_lbl, ")")),
          hovermode = "x unified",
          legend    = list(orientation = "h", y = -0.2)
        )
    })

  })
}
