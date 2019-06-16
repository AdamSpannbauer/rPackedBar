#' internal helper function to do actual plotting of packed barchart

#' @keywords internal
gen_plotly_packed_bar = function(summ_dt, value_column, color_bar_color, hover_label, canvas_df,
                                 plot_title, xaxis_label,
                                 gray_hover_point_dt, colored_hover_point_dt,
                                 gray_bar_list, colored_bar_list,
                                 gray_ann_list, colored_ann_list,
                                 tick_breaks, tick_text) {

  if (trimws(hover_label) != "") {
    hover_label = paste0(hover_label, ": ")
  }

  #plot blank canvas and hover info points
  p = plotly::plot_ly(canvas_df,
                      x = ~x,
                      y = ~y,
                      mode = "markers",
                      type = "scatter",
                      opacity = 0,
                      hoverinfo = "none",
                      showlegend = FALSE) %>%
    plotly::add_trace(data = gray_hover_point_dt,
                      x = ~x,
                      y = ~y,
                      size = ~size,
                      hoverinfo = "text",
                      text = ~paste0(
                        name, "<br>", hover_label,
                        format(size * sum(summ_dt[[value_column]]), big.mark = ",")),
                      opacity = 0,
                      marker = list(color = "#E8E8E8"),
                      showlegend = FALSE) %>%
    plotly::add_trace(data = colored_hover_point_dt,
                      x = ~x,
                      y = ~y,
                      size = ~size,
                      hoverinfo = "text",
                      text = ~paste0(
                        name, "<br>", hover_label,
                        format(size * sum(summ_dt[[value_column]]), big.mark = ",")),
                      opacity = 0,
                      marker = list(color = color_bar_color),
                      showlegend = FALSE)

  # add shapes to the layout
  p = plotly::layout(p, title = plot_title,
                     shapes = c(
                       colored_bar_list,
                       gray_bar_list
                     ),
                     annotations = c(
                       colored_ann_list,
                       gray_ann_list
                     ),
                     hovermode = "closest",
                     yaxis = list(title = "",
                                  zeroline = FALSE,
                                  showline = FALSE,
                                  showticklabels = FALSE,
                                  showgrid = FALSE),
                     xaxis = list(title = xaxis_label,
                                  zeroline = FALSE,
                                  showline = FALSE,
                                  showticklabels = TRUE,
                                  showgrid = TRUE,
                                  tickvals = tick_breaks,
                                  ticktext = tick_text)
  )
  p
}
