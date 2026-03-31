#' spreads UI Function
#'
#' Three spread stories in energy markets:
#' \enumerate{
#'   \item \strong{WTI-Brent spread} — reflects US export capacity and regional
#'         crude supply/demand imbalances.
#'   \item \strong{Crack spreads} — refinery margin proxies: HO-CL (distillate)
#'         and RB-CL (gasoline).  HO is in USD/gal so multiply by 42 to convert
#'         to per-barrel equivalent before subtracting.
#'   \item \strong{Calendar spread (C1 - C2)} — the premium or discount of the
#'         front month vs the next.  Backwardation (positive) signals tight
#'         near-term supply; contango (negative) signals oversupply / high storage.
#' }
#'
#' @param id Shiny module id.
#' @noRd
#' @importFrom shiny NS tagList selectInput
#' @importFrom bslib layout_columns card card_header navset_tab nav_panel
#' @importFrom plotly plotlyOutput
mod_spreads_ui <- function(id) {
  ns <- NS(id)
  bslib::layout_columns(
    col_widths = c(3, 9),
    fill       = TRUE,
    bslib::card(
      bslib::card_header("Settings"),
      shiny::selectInput(
        ns("cal_market"),
        "Calendar spread market",
        choices  = c("CL","BRN","NG","HO","RB","HTT"),
        selected = "CL"
      ),
      shiny::selectInput(
        ns("cal_contract_back"),
        "Back contract (C1 minus ...)",
        choices  = as.character(2:12),
        selected = "2"
      )
    ),
    bslib::card(
      fill = TRUE,
      bslib::card_header("Spread Dynamics"),
      bslib::navset_tab(
        bslib::nav_panel(
          "WTI – Brent Spread",
          plotly::plotlyOutput(ns("wti_brent"), height = "100%")
        ),
        bslib::nav_panel(
          "Crack Spreads (refinery margin)",
          plotly::plotlyOutput(ns("crack"), height = "100%")
        ),
        bslib::nav_panel(
          "Calendar Spread (C1 – Cn)",
          plotly::plotlyOutput(ns("calendar"), height = "100%")
        )
      )
    )
  )
}

#' spreads Server Function
#'
#' @param id Shiny module id.
#' @param r Shared \code{reactiveValues} environment.
#' @noRd
#' @importFrom shiny moduleServer reactive req
#' @importFrom dplyr filter arrange mutate select inner_join
#' @importFrom plotly plot_ly add_trace layout renderPlotly
mod_spreads_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {

    # Helper: front-month price series for one market
    front <- function(mkt) {
      req(!is.null(r$data))
      r$data |>
        dplyr::filter(
          market   == mkt,
          contract == 1L,
          date     >= r$date_range[1],
          date     <= r$date_range[2]
        ) |>
        dplyr::select(date, value) |>
        dplyr::arrange(date)
    }

    # ----- WTI – Brent spread ------------------------------------------------
    # Both in USD/bbl.  Positive = WTI premium; negative = WTI discount.
    # Historically WTI traded at a slight premium; it went to deep discount
    # (~$25) during the 2011-2013 Cushing pipeline glut.

    output$wti_brent <- plotly::renderPlotly({
      cl  <- front("CL")  |> dplyr::rename(cl  = value)
      brn <- front("BRN") |> dplyr::rename(brn = value)

      spread <- dplyr::inner_join(cl, brn, by = "date") |>
        dplyr::mutate(spread = cl - brn)

      plotly::plot_ly(
        data          = spread,
        x             = ~date,
        y             = ~spread,
        type          = "scatter",
        mode          = "lines",
        line          = list(color = "#2c3e50"),
        fill          = "tozeroy",
        fillcolor      = "rgba(44,62,80,0.15)",
        hovertemplate = "%{x|%Y-%m-%d}<br>WTI-Brent: $%{y:.2f}/bbl<extra></extra>"
      ) |>
        plotly::add_trace(
          x = range(spread$date), y = c(0, 0),
          type = "scatter", mode = "lines",
          line = list(color = "grey", dash = "dot"),
          showlegend = FALSE
        ) |>
        plotly::layout(
          title  = "WTI – Brent Spread (USD/bbl)",
          xaxis  = list(title = ""),
          yaxis  = list(title = "Spread (USD/bbl)"),
          shapes = list(list(
            type = "line", x0 = min(spread$date), x1 = max(spread$date),
            y0 = 0, y1 = 0,
            line = list(color = "grey", dash = "dot")
          ))
        )
    })

    # ----- Crack spreads -----------------------------------------------------
    # Refinery margin = product price - crude cost.
    # HO and RB are priced in USD/gallon; multiply by 42 to convert to USD/bbl.
    # HO crack: (HO * 42) - CL    → distillate margin
    # RB crack: (RB * 42) - CL    → gasoline margin

    output$crack <- plotly::renderPlotly({
      cl <- front("CL") |> dplyr::rename(cl = value)

      p <- plotly::plot_ly()

      for (prod in c("HO", "RB")) {
        prod_data <- front(prod) |> dplyr::rename(prod = value)
        crack_df  <- dplyr::inner_join(cl, prod_data, by = "date") |>
          dplyr::mutate(crack = prod * 42 - cl)

        label <- if (prod == "HO") "HO crack (distillate)" else "RB crack (gasoline)"
        p <- plotly::add_trace(
          p,
          data          = crack_df,
          x             = ~date,
          y             = ~crack,
          type          = "scatter",
          mode          = "lines",
          name          = label,
          hovertemplate = paste0(label, "<br>%{x|%Y-%m-%d}<br>$%{y:.2f}/bbl<extra></extra>")
        )
      }

      plotly::layout(
        p,
        title     = "Crack Spreads — Refinery Margin Proxies (USD/bbl)",
        xaxis     = list(title = ""),
        yaxis     = list(title = "Crack Spread (USD/bbl)"),
        hovermode = "x unified",
        legend    = list(orientation = "h", y = -0.2)
      )
    })

    # ----- Calendar spread ---------------------------------------------------
    # C1 minus Cn for a user-selected market.
    # Positive = backwardation (spot premium) — tight supply.
    # Negative = contango (deferred premium) — oversupply / high storage costs.

    output$calendar <- plotly::renderPlotly({
      req(!is.null(r$data))
      mkt  <- input$cal_market
      cn   <- as.integer(input$cal_contract_back)

      c1_data <- r$data |>
        dplyr::filter(market == mkt, contract == 1L,
                      date >= r$date_range[1], date <= r$date_range[2]) |>
        dplyr::select(date, c1 = value)

      cn_data <- r$data |>
        dplyr::filter(market == mkt, contract == cn,
                      date >= r$date_range[1], date <= r$date_range[2]) |>
        dplyr::select(date, cn = value)

      cal <- dplyr::inner_join(c1_data, cn_data, by = "date") |>
        dplyr::mutate(spread = c1 - cn)

      plotly::plot_ly(
        data          = cal,
        x             = ~date,
        y             = ~spread,
        type          = "scatter",
        mode          = "lines",
        fill          = "tozeroy",
        fillcolor      = "rgba(231,76,60,0.15)",
        line          = list(color = "#e74c3c"),
        hovertemplate = paste0(mkt, " C1-C", cn,
                               "<br>%{x|%Y-%m-%d}<br>$%{y:.2f}<extra></extra>")
      ) |>
        plotly::add_trace(
          x = range(cal$date), y = c(0, 0),
          type = "scatter", mode = "lines",
          line = list(color = "grey", dash = "dot"),
          showlegend = FALSE
        ) |>
        plotly::layout(
          title  = paste0(mkt, " Calendar Spread: C1 – C", cn,
                          "  |  + = backwardation  |  – = contango"),
          xaxis  = list(title = ""),
          yaxis  = list(title = "Spread (USD)")
        )
    })

  })
}
