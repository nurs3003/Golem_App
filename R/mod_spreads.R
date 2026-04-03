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
#'   \item \strong{Cushing storage} — EIA weekly utilization overlaid with the
#'         WTI C1-C2 spread.  Explains the fundamental driver of WTI curve shape.
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
          "WTI \u2013 Brent Spread",
          plotly::plotlyOutput(ns("wti_brent"), height = "calc(100% - 2.8rem)"),
          shiny::tags$p(
            "Positive = WTI at premium (rare post-2015 export ban lift); negative = WTI discount.
             The 2011\u20132013 Cushing pipeline glut drove WTI to a $25 discount vs Brent.",
            style = "font-size:0.82rem; color:#666; padding:0.2rem 0.6rem; margin:0;"
          )
        ),
        bslib::nav_panel(
          "Crack Spreads (refinery margin)",
          plotly::plotlyOutput(ns("crack"), height = "calc(100% - 2.8rem)"),
          shiny::tags$p(
            "Crack spread = refined product price (\u00d742 to $/bbl) minus WTI cost.
             Rising cracks incentivise higher refinery run rates. HO cracks peak in winter; RB cracks peak pre-summer driving season.",
            style = "font-size:0.82rem; color:#666; padding:0.2rem 0.6rem; margin:0;"
          )
        ),
        bslib::nav_panel(
          "Calendar Spread (C1 \u2013 Cn)",
          plotly::plotlyOutput(ns("calendar"), height = "calc(100% - 2.8rem)"),
          shiny::tags$p(
            "Positive (backwardation) = tight near-term supply. Negative (contango) = oversupply or storage carry.
             The sign and magnitude determine the cost of rolling a futures hedge forward.",
            style = "font-size:0.82rem; color:#666; padding:0.2rem 0.6rem; margin:0;"
          )
        ),
        bslib::nav_panel(
          "Spread Z-Scores",
          plotly::plotlyOutput(ns("spread_zscore"), height = "calc(100% - 2.8rem)"),
          shiny::tags$p(
            "How statistically wide or tight is each spread relative to its 252-day history?
             Readings above +2\u03c3 or below \u22122\u03c3 are rare and often signal mean-reversion opportunities or structural regime shifts.",
            style = "font-size:0.82rem; color:#666; padding:0.2rem 0.6rem; margin:0;"
          )
        ),
        bslib::nav_panel(
          "Cushing Storage (WTI)",
          plotly::plotlyOutput(ns("cushing"), height = "calc(100% - 2.8rem)"),
          shiny::tags$p(
            "Cushing, Oklahoma is the delivery point for NYMEX WTI and the largest crude storage hub in North America (~80 Mbbl capacity).
             When utilization is high (\u226580%), sellers cannot find storage and the market is forced into contango \u2014 deferred prices
             rise above spot to compensate holders for storage costs.
             When utilization is low, nearby barrels are scarce and the market flips into backwardation.
             The C1\u2013C2 spread (red line) confirms the regime in real time: negative = contango, positive = backwardation.
             Data: EIA weekly, 2011\u2013present.",
            style = "font-size:0.82rem; color:#666; padding:0.2rem 0.6rem; margin:0;"
          )
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
#' @importFrom dplyr filter arrange mutate select inner_join bind_rows
#' @importFrom slider slide_dbl
#' @importFrom stats sd
#' @importFrom RTL cushing
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

      ho_data <- front("HO") |> dplyr::rename(ho = value)
      rb_data <- front("RB") |> dplyr::rename(rb = value)

      ho_crack_df <- dplyr::inner_join(cl, ho_data, by = "date") |>
        dplyr::mutate(crack = ho * 42 - cl)
      rb_crack_df <- dplyr::inner_join(cl, rb_data, by = "date") |>
        dplyr::mutate(crack = rb * 42 - cl)

      p <- plotly::add_trace(p, data = ho_crack_df, x = ~date, y = ~crack,
        type = "scatter", mode = "lines", name = "HO crack (distillate)",
        line = list(color = market_colors["HO"] %||% "#e67e22"),
        hovertemplate = "HO crack<br>%{x|%Y-%m-%d}<br>$%{y:.2f}/bbl<extra></extra>")

      p <- plotly::add_trace(p, data = rb_crack_df, x = ~date, y = ~crack,
        type = "scatter", mode = "lines", name = "RB crack (gasoline)",
        line = list(color = market_colors["RB"] %||% "#8e44ad"),
        hovertemplate = "RB crack<br>%{x|%Y-%m-%d}<br>$%{y:.2f}/bbl<extra></extra>")

      # 3-2-1 crack: 3 bbl crude → 2 bbl gasoline + 1 bbl distillate
      # = (2 × RB×42 + 1 × HO×42 - 3 × CL) / 3
      crack_321_df <- dplyr::inner_join(cl, ho_data, by = "date") |>
        dplyr::inner_join(rb_data, by = "date") |>
        dplyr::mutate(crack = (2 * rb * 42 + ho * 42 - 3 * cl) / 3)

      p <- plotly::add_trace(p, data = crack_321_df, x = ~date, y = ~crack,
        type = "scatter", mode = "lines", name = "3-2-1 crack (refinery margin)",
        line = list(color = "#2c3e50", width = 2),
        hovertemplate = "3-2-1 crack<br>%{x|%Y-%m-%d}<br>$%{y:.2f}/bbl<extra></extra>")

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

    # ----- Spread Z-Scores ---------------------------------------------------
    # 252-day rolling z-score of each spread: (spread - roll_mean) / roll_sd
    # ±2σ reference lines show historically extreme readings.

    output$spread_zscore <- plotly::renderPlotly({
      cl  <- front("CL")  |> dplyr::rename(cl  = value)
      brn <- front("BRN") |> dplyr::rename(brn = value)
      ho  <- front("HO")  |> dplyr::rename(ho  = value)
      rb  <- front("RB")  |> dplyr::rename(rb  = value)

      z_win <- 252L

      compute_z <- function(df, spread_col, label) {
        df |>
          dplyr::mutate(
            spread    = .data[[spread_col]],
            roll_mean = slider::slide_dbl(spread,
                          ~ mean(.x, na.rm = TRUE),
                          .before = z_win - 1L, .complete = TRUE),
            roll_sd   = slider::slide_dbl(spread,
                          ~ stats::sd(.x, na.rm = TRUE),
                          .before = z_win - 1L, .complete = TRUE),
            z_score   = (spread - roll_mean) / roll_sd,
            name      = label
          ) |>
          dplyr::filter(!is.na(z_score))
      }

      wti_brent_df <- dplyr::inner_join(cl, brn, by = "date") |>
        dplyr::mutate(wtibrent = cl - brn)
      ho_crack_df  <- dplyr::inner_join(cl, ho,  by = "date") |>
        dplyr::mutate(hocrack  = ho * 42 - cl)
      rb_crack_df  <- dplyr::inner_join(cl, rb,  by = "date") |>
        dplyr::mutate(rbcrack  = rb * 42 - cl)

      spreads_z <- dplyr::bind_rows(
        compute_z(wti_brent_df, "wtibrent", "WTI\u2013Brent"),
        compute_z(ho_crack_df,  "hocrack",  "HO Crack"),
        compute_z(rb_crack_df,  "rbcrack",  "RB Crack")
      )

      req(nrow(spreads_z) > 0L)
      x_range <- range(spreads_z$date)

      colors <- c("WTI\u2013Brent" = "#2c3e50",
                  "HO Crack"    = market_colors["HO"] %||% "#2980b9",
                  "RB Crack"    = market_colors["RB"] %||% "#e67e22")

      p <- plotly::plot_ly()
      for (nm in unique(spreads_z$name)) {
        sub <- dplyr::filter(spreads_z, name == nm)
        p <- plotly::add_trace(
          p,
          data          = sub,
          x             = ~date,
          y             = ~z_score,
          type          = "scatter",
          mode          = "lines",
          name          = nm,
          line          = list(color = colors[nm]),
          hovertemplate = paste0(nm, "<br>%{x|%Y-%m-%d}<br>Z: %{y:.2f}\u03c3<extra></extra>")
        )
      }

      p |>
        plotly::add_trace(x = x_range, y = c( 2,  2), type = "scatter", mode = "lines",
          line = list(color = alert_color, dash = "dash", width = 1),
          showlegend = FALSE, hoverinfo = "skip") |>
        plotly::add_trace(x = x_range, y = c(-2, -2), type = "scatter", mode = "lines",
          line = list(color = alert_color, dash = "dash", width = 1),
          showlegend = FALSE, hoverinfo = "skip") |>
        plotly::add_trace(x = x_range, y = c( 0,  0), type = "scatter", mode = "lines",
          line = list(color = "grey", dash = "dot", width = 1),
          showlegend = FALSE, hoverinfo = "skip") |>
        plotly::layout(
          title     = paste0("Spread Z-Scores \u2014 ", z_win, "-day rolling normalisation"),
          xaxis     = list(title = ""),
          yaxis     = list(title = "Z-Score (\u03c3)"),
          hovermode = "x unified",
          legend    = list(orientation = "h", y = -0.2)
        )
    })

    # ----- Cushing storage utilization vs WTI C1-C2 spread ------------------
    # RTL::cushing$storage is a static weekly EIA dataset (2011-present).
    # Utilization = stocks / capacity; c1c2 = front-month minus second-month price.
    # High utilization → contango (negative c1c2); low utilization → backwardation.

    output$cushing <- plotly::renderPlotly({
      storage <- RTL::cushing$storage |>
        dplyr::select(date, utilization, c1c2) |>
        dplyr::arrange(date)

      plotly::plot_ly() |>
        plotly::add_trace(
          data          = storage,
          x             = ~date,
          y             = ~utilization,
          type          = "scatter",
          mode          = "lines",
          name          = "Cushing utilization",
          fill          = "tozeroy",
          fillcolor      = "rgba(41,128,185,0.12)",
          line          = list(color = "#2980b9", width = 1.5),
          hovertemplate = "%{x|%Y-%m-%d}<br>Utilization: %{y:.1%}<extra></extra>"
        ) |>
        plotly::add_trace(
          data          = storage,
          x             = ~date,
          y             = ~c1c2,
          type          = "scatter",
          mode          = "lines",
          name          = "C1\u2013C2 spread ($/bbl)",
          yaxis         = "y2",
          line          = list(color = alert_color, width = 1.5),
          hovertemplate = "%{x|%Y-%m-%d}<br>C1\u2013C2: $%{y:.2f}/bbl<extra></extra>"
        ) |>
        plotly::add_trace(
          x = range(storage$date), y = c(0, 0),
          type = "scatter", mode = "lines",
          yaxis = "y2",
          line = list(color = "grey", dash = "dot", width = 1),
          showlegend = FALSE, hoverinfo = "skip"
        ) |>
        plotly::layout(
          title     = "Cushing Storage Utilization vs WTI Calendar Spread",
          xaxis     = list(title = "Date"),
          yaxis     = list(title = "Storage Utilization", tickformat = ".0%",
                           range = c(0.3, 1.05)),
          yaxis2    = list(title = "C1\u2013C2 Spread ($/bbl)", overlaying = "y",
                           side = "right", showgrid = FALSE, zeroline = FALSE),
          hovermode = "x unified",
          legend    = list(orientation = "h", y = -0.2)
        )
    })

  })
}
