#' internal helper function to create colored portion of packed barchart

#' @keywords internal
gen_color_bars = function(summ_dt, number_rows, bar_color, label_column, min_label_width, label_color) {
  # set aside data for colored bars
  colored_bar_data = summ_dt[order(-summ_dt[["max_rel_val"]]), ][1:number_rows, ]

  # calc row height based on num rows
  bar_h = 1 / number_rows
  #calc y vals for bar heights
  row_y_list = lapply(1:number_rows, function(i) {
    list(y0 = 1 - bar_h * (i - 1), y1 = 1 - bar_h * i)
  })

  #initalize storage
  colored_bar_list = vector("list", nrow(colored_bar_data))
  colored_ann_list = vector("list", nrow(colored_bar_data))
  colored_hover_point_list = vector("list", nrow(colored_bar_data))

  #track which bar level we're in
  color_row_i = 1
  #loop through n bar levels
  for (i in 1:nrow(colored_bar_data)) {
    row = colored_bar_data[i, ]

    #set corners for colored rectangle shape objs
    out_bar = list(
      type = "rect",
      fillcolor = bar_color,
      line = list(color = bar_color, width = .1),
      x0 = 0,
      x1 = row[["max_rel_val"]],
      xref = "x",
      y0 = row_y_list[[color_row_i]]$y0,
      y1 = row_y_list[[color_row_i]]$y1,
      yref = "y"
    )

    #calc center x and y point for current bar
    x = as.numeric(row[["max_rel_val"]]) / 2
    y = row_y_list[[color_row_i]]$y0 - (row_y_list[[color_row_i]]$y0 - row_y_list[[color_row_i]]$y1) / 2
    out_ann = list(
      x = .001,
      y = y,
      xref = "x",
      yref = "y",
      #set label
      text = row[[label_column]],
      showarrow = FALSE,
      xanchor = "left",
      font = list(color = label_color)
    )

    #put point at center of shape for hover info
    hover_point = data.table::data.table(
      name = row[[label_column]],
      x = seq(0, x * 2, length.out = 25),
      y = y,
      size = row[["max_rel_val"]] - 0
    )

    #dont show annotation if smaller than threshold
    if (row[["max_rel_val"]] < min_label_width) out_ann = NULL

    colored_bar_list[[i]] = out_bar
    colored_ann_list[[i]] = out_ann
    colored_hover_point_list[[i]] = hover_point

    color_row_i = 1 + color_row_i
  }
  #combine hover info list into single df
  colored_hover_point_dt = data.table::rbindlist(colored_hover_point_list)

  out = list(raw_data = colored_bar_data,
             bar_list = colored_bar_list,
             ann_list = colored_ann_list,
             hover_point_dt = colored_hover_point_dt)

  out
}
