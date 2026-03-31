#' Run the Shiny Application
#'
#' @param ... Arguments passed to \code{\link[golem]{with_golem_options}}.
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
run_app <- function(...) {
  with_golem_options(
    app = shinyApp(
      ui     = app_ui,
      server = app_server
    ),
    golem_opts = list(...)
  )
}
