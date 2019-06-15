#' Plot rPackedBar in 'shiny'
#'
#' @description
#' Output and render functions for using 'rPackedBar' within 'shiny' applications and interactive Rmd documents.
#'
#' @param outputId output variable to read from
#' @param width,height Must be a valid CSS unit (like \code{"100\%"},
#'   \code{"400px"}, \code{"auto"}) or a number, which will be coerced to a
#'   string and have \code{"px"} appended.
#' @param clickedBarInputId The input id to assign the label of the clicked bar.
#' Defaults to \code{outputId_clicked} and can be referenced in the server as \code{input$outputId_clicked}.
#' @param inline use an inline (span()) or block container (div()) for the output
#' @param expr An expression that generates an rPackedBar.
#' @param env The environment in which to evaluate \code{expr}.
#' @param quoted Is \code{expr} a quoted expression (with \code{quote()})? This
#'   is useful if you want to save an expression in a variable.
#'
#' @name packed_bar_shiny
NULL

# nolint start
get_clicked_packed_bar = function(outputId, inputId) {
  script = sprintf(SCRIPT_TEMPLATE, outputId, outputId, inputId)
  shiny::tags$script(shiny::HTML(script))
}

#' @rdname packed_bar_shiny
#' @export
packedBarOutput = function(outputId, width = "100%", height = "400px", inline = FALSE,
                           clickedBarInputId = paste0(outputId, "_clicked")) {
  plotly_out = plotly::plotlyOutput(outputId)
  shiny::div(
    get_clicked_packed_bar(outputId, clickedBarInputId),
    plotly_out
  )
}

#' @rdname packed_bar_shiny
#' @export
renderPackedBar = function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) {
    expr = substitute(expr)
    quoted = TRUE
  }
  plotly::renderPlotly(expr = expr, env = env, quoted = quoted)
}
# nolint end
