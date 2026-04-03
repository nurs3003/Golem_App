#' codynamics UI Function
#'
#' Two views:
#' \enumerate{
#'   \item Correlation matrix heatmap — snapshot at a user-selected date.
#'   \item Rolling pairwise correlation time-series for a chosen market pair.
#' }
#'
#' @param id Shiny module id.
#' @noRd
#' @importFrom shiny NS tagList selectInput dateInput
#' @importFrom bslib layout_columns card card_header navset_tab nav_panel
#' @importFrom plotly plotlyOutput
mod_codynamics_ui <- function(id) {
  ns <- NS(id)
  bslib::layout_columns(
    col_widths = c(3, 9),
    fill       = TRUE,
    bslib::card(
      bslib::card_header("Settings"),
      shiny::selectInput(
        ns("window"),
        "Rolling window (trading days)",
        choices  = c("63 days (~1 qtr)" = 63, "126 days (~6 mo)" = 126, "252 days (~1 yr)" = 252),
        selected = 63
      ),
      shiny::dateInput(
        ns("snapshot_date"),
        "Matrix snapshot date",
        value = Sys.Date() - 30,
        min   = as.Date("2007-01-01"),
        max   = Sys.Date()
      ),
      shiny::selectInput(ns("pair_a"), "Pair — Market A", choices = NULL),
      shiny::selectInput(ns("pair_b"), "Pair — Market B", choices = NULL)
    ),
    bslib::card(
      fill = TRUE,
      bslib::card_header("Cross-Market Co-Dynamics"),
      bslib::navset_tab(
        bslib::nav_panel(
          "Correlation Matrix",
          plotly::plotlyOutput(ns("corr_matrix"), height = "calc(100% - 2.8rem)"),
          shiny::tags$p(
            "Dark blue (\u22481) = strong positive correlation; dark red (\u2248\u22121) = strong negative correlation.
             WTI and Brent typically exceed 0.9; Natural Gas is the most independent market in this set.",
            style = "font-size:0.82rem; color:#666; padding:0.2rem 0.6rem; margin:0;"
          )
        ),
        bslib::nav_panel(
          "Rolling Correlation (pair)",
          plotly::plotlyOutput(ns("rolling_corr"), height = "calc(100% - 2.8rem)"),
          shiny::tags$p(
            "When correlation drops sharply, markets are diverging — often a structural shift (e.g. the US export ban
             lift in 2015 lowered WTI\u2013Brent correlation). The grey line shows the WTI\u2013Brent spread for context.",
            style = "font-size:0.82rem; color:#666; padding:0.2rem 0.6rem; margin:0;"
          )
        ),
        bslib::nav_panel(
          "Factor Analysis",
          # Top row: scree (left) + loadings (right)
          bslib::layout_columns(
            col_widths = c(5, 7),
            shiny::div(
              plotly::plotlyOutput(ns("pca_scree"), height = "260px"),
              shiny::tags$p(
                "How many independent risk factors drive these markets?
                 F1 capturing >60% of variance means one dominant factor (broad energy complex / macro demand) moves all markets together.
                 More factors needed = more markets moving for their own reasons.",
                style = "font-size:0.82rem; color:#666; padding:0.2rem 0.6rem; margin:0;"
              )
            ),
            shiny::div(
              plotly::plotlyOutput(ns("pca_loadings"), height = "260px"),
              shiny::tags$p(
                "How much does each market contribute to each factor?
                 Same-sign bars on F1 = all move together with global demand.
                 F2 typically isolates Natural Gas (weather/power-burn driven) from the oil complex.",
                style = "font-size:0.82rem; color:#666; padding:0.2rem 0.6rem; margin:0;"
              )
            )
          ),
          # Bottom row: F1 score time series
          plotly::plotlyOutput(ns("pca_scores"), height = "200px"),
          shiny::tags$p(
            "Daily reading of the dominant factor (F1). Extreme spikes = broad market stress events
             (COVID demand collapse Mar 2020; Russia-Ukraine supply shock Feb 2022).
             Use this as a systemic risk barometer across your energy book.",
            style = "font-size:0.82rem; color:#666; padding:0.2rem 0.6rem; margin:0;"
          )
        )
      )
    )
  )
}

#' codynamics Server Function
#'
#' @param id Shiny module id.
#' @param r Shared \code{reactiveValues} environment.
#' @noRd
#' @importFrom shiny moduleServer reactive observe req updateSelectInput
#' @importFrom dplyr filter arrange mutate group_by ungroup select inner_join
#' @importFrom tidyr pivot_wider
#' @importFrom slider slide_dbl slide2_dbl
#' @importFrom plotly plot_ly layout renderPlotly add_trace add_bars
#' @importFrom stats prcomp cor complete.cases
mod_codynamics_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {

    # Update pair dropdowns whenever selected markets change
    observe({
      mkts <- r$selected_markets
      req(length(mkts) >= 1L)
      shiny::updateSelectInput(session, "pair_a", choices = mkts, selected = mkts[1])
      shiny::updateSelectInput(session, "pair_b", choices = mkts, selected = mkts[min(2L, length(mkts))])
    })

    # Reactive: wide returns matrix (one column per market, front-month)
    wide_returns <- reactive({
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
        tidyr::pivot_wider(
          id_cols     = date,
          names_from  = market,
          values_from = log_ret
        ) |>
        dplyr::arrange(date)
    })

    # Correlation matrix at snapshot date
    output$corr_matrix <- plotly::renderPlotly({
      df   <- wide_returns()
      req(nrow(df) > 5L)
      snap <- input$snapshot_date
      win  <- as.integer(input$window)

      # Use the most recent `win` rows up to snap date
      sub <- df[df$date <= snap, , drop = FALSE]
      sub <- tail(sub, win)
      req(nrow(sub) >= 10L)

      mat <- stats::cor(sub[, -1, drop = FALSE], use = "pairwise.complete.obs")
      nms <- colnames(mat)

      # Explicit colorscale: -1 = dark blue, 0 = white, +1 = dark red
      corr_colorscale <- list(
        list(0,    "#053061"),
        list(0.25, "#4393c3"),
        list(0.5,  "#f7f7f7"),
        list(0.75, "#d6604d"),
        list(1,    "#67001f")
      )

      plotly::plot_ly(
        x    = nms,
        y    = nms,
        z    = mat,
        type = "heatmap",
        colorscale    = corr_colorscale,
        zmid = 0,
        zmin = -1, zmax = 1,
        hovertemplate = "%{x} vs %{y}<br>Corr: %{z:.2f}<extra></extra>"
      ) |>
        plotly::layout(
          title  = paste0("Correlation Matrix — ", win, "-day window at ", format(snap)),
          xaxis  = list(title = ""),
          yaxis  = list(title = "", autorange = "reversed")
        )
    })

    # Rolling pairwise correlation for selected pair
    output$rolling_corr <- plotly::renderPlotly({
      df  <- wide_returns()
      req(nrow(df) > 5L)
      a   <- input$pair_a
      b   <- input$pair_b
      req(!is.null(a), !is.null(b), a != b)
      req(a %in% names(df), b %in% names(df))
      win <- as.integer(input$window)

      roll <- df |>
        dplyr::select(date, dplyr::all_of(c(a, b))) |>
        dplyr::filter(!is.na(.data[[a]]), !is.na(.data[[b]])) |>
        dplyr::rename(col_a = dplyr::all_of(a), col_b = dplyr::all_of(b)) |>
        dplyr::mutate(
          roll_corr = slider::slide2_dbl(
            col_a, col_b,
            function(.x, .y) {
              if (length(.x) < 10L) return(NA_real_)
              stats::cor(.x, .y, use = "complete.obs")
            },
            .before   = win - 1L,
            .complete = TRUE
          )
        )

      p <- plotly::plot_ly(
        data = roll, x = ~date, y = ~roll_corr,
        type = "scatter", mode = "lines",
        name = paste0(a, " vs ", b),
        line = list(color = "#2980b9"),
        hovertemplate = "%{x|%Y-%m-%d}<br>Corr: %{y:.2f}<extra></extra>"
      ) |>
        plotly::add_trace(
          x = range(roll$date), y = c(0, 0),
          type = "scatter", mode = "lines",
          line = list(color = "grey", dash = "dash"),
          showlegend = FALSE
        )

      # Overlay WTI-Brent spread on second axis if both markets are in the data
      if (!is.null(r$data)) {
        cl_s <- r$data |>
          dplyr::filter(market == "CL", contract == 1L,
                        date >= r$date_range[1], date <= r$date_range[2]) |>
          dplyr::select(date, cl = value)
        brn_s <- r$data |>
          dplyr::filter(market == "BRN", contract == 1L,
                        date >= r$date_range[1], date <= r$date_range[2]) |>
          dplyr::select(date, brn = value)
        if (nrow(cl_s) > 0L && nrow(brn_s) > 0L) {
          basis_df <- dplyr::inner_join(cl_s, brn_s, by = "date") |>
            dplyr::mutate(basis = cl - brn)
          p <- p |>
            plotly::add_trace(
              data  = basis_df, x = ~date, y = ~basis,
              type  = "scatter", mode = "lines",
              name  = "WTI\u2013Brent ($/bbl)",
              yaxis = "y2",
              line  = list(color = "rgba(231,76,60,0.45)", width = 1),
              hovertemplate = "%{x|%Y-%m-%d}<br>WTI\u2013Brent: $%{y:.2f}<extra></extra>"
            ) |>
            plotly::layout(
              yaxis2 = list(title = "WTI\u2013Brent ($/bbl)", overlaying = "y",
                            side = "right", showgrid = FALSE)
            )
        }
      }

      p |> plotly::layout(
        title  = paste0(a, " vs ", b, " \u2014 ", win, "-day rolling correlation"),
        xaxis  = list(title = "Date"),
        yaxis  = list(title = "Correlation", range = c(-1, 1)),
        legend = list(orientation = "h", y = -0.2)
      )
    })

    # PCA on the full wide returns matrix (all selected markets).
    # Returns a list with the prcomp object AND the corresponding dates so
    # pca_scores does not need to call wide_returns() separately (avoids
    # a reactive race where row counts can mismatch).
    pca_result <- reactive({
      df <- wide_returns()
      req(nrow(df) > 10L)
      mat     <- as.matrix(df[, -1, drop = FALSE])
      ok_cols <- colMeans(is.na(mat)) < 0.5
      mat     <- mat[, ok_cols, drop = FALSE]
      ok_rows <- stats::complete.cases(mat)
      mat     <- mat[ok_rows, , drop = FALSE]
      req(nrow(mat) > 10L, ncol(mat) >= 2L)
      list(
        pca   = stats::prcomp(mat, center = TRUE, scale. = TRUE),
        dates = df$date[ok_rows]
      )
    })

    output$pca_scree <- plotly::renderPlotly({
      res     <- pca_result()
      pca     <- res$pca
      var_exp <- pca$sdev^2 / sum(pca$sdev^2) * 100
      n_pcs   <- length(var_exp)
      cum_var <- cumsum(var_exp)

      plotly::plot_ly() |>
        plotly::add_bars(
          x = paste0("F", seq_len(n_pcs)),
          y = var_exp,
          name = "Explained",
          marker = list(color = "#2980b9"),
          hovertemplate = "%{x}: %{y:.1f}%<extra></extra>"
        ) |>
        plotly::add_trace(
          x    = paste0("F", seq_len(n_pcs)),
          y    = cum_var,
          type = "scatter", mode = "lines+markers",
          name = "Cumulative",
          yaxis = "y2",
          line  = list(color = "#e74c3c"),
          hovertemplate = "%{x}: %{y:.1f}%<extra></extra>"
        ) |>
        plotly::layout(
          title  = "Factor Importance — variance explained by each factor",
          xaxis  = list(title = ""),
          yaxis  = list(title = "Variance explained (%)"),
          yaxis2 = list(title = "Cumulative (%)", overlaying = "y", side = "right",
                        range = c(0, 100)),
          legend = list(orientation = "h", x = 0, y = -0.2),
          bargap = 0.3
        )
    })

    output$pca_loadings <- plotly::renderPlotly({
      pca <- pca_result()$pca
      rot <- pca$rotation[, 1:min(3L, ncol(pca$rotation)), drop = FALSE]
      markets <- rownames(rot)

      plotly::plot_ly() |>
        plotly::add_bars(
          x    = markets,
          y    = rot[, 1],
          name = "F1",
          marker = list(color = "#2980b9"),
          hovertemplate = "%{x}: %{y:.3f}<extra></extra>"
        ) |>
        plotly::add_bars(
          x    = markets,
          y    = rot[, 2],
          name = "F2",
          marker = list(color = "#e67e22"),
          hovertemplate = "%{x}: %{y:.3f}<extra></extra>"
        ) |>
        {
          if (ncol(rot) >= 3L) {
            function(p) plotly::add_bars(
              p,
              x    = markets,
              y    = rot[, 3],
              name = "F3",
              marker = list(color = "#27ae60"),
              hovertemplate = "%{x}: %{y:.3f}<extra></extra>"
            )
          } else {
            identity
          }
        }() |>
        plotly::layout(
          title   = "Market Exposures to Each Factor (first 3)",
          barmode = "group",
          xaxis   = list(title = ""),
          yaxis   = list(title = "Loading"),
          legend  = list(orientation = "h", x = 0, y = -0.25)
        )
    })

    output$pca_scores <- plotly::renderPlotly({
      # Use the cached result — dates and pca$x are guaranteed to match.
      res    <- pca_result()
      dates  <- res$dates
      scores <- res$pca$x[, 1]   # F1 score per observation

      plotly::plot_ly(
        x    = dates,
        y    = scores,
        type = "scatter",
        mode = "lines",
        line = list(color = "#8e44ad"),
        hovertemplate = "%{x|%Y-%m-%d}<br>F1 score: %{y:.3f}<extra></extra>"
      ) |>
        plotly::layout(
          title = "F1 Daily Reading — systemic risk barometer across energy markets",
          xaxis = list(title = "Date"),
          yaxis = list(title = "F1 score")
        )
    })

  })
}
