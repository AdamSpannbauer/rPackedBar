#' internal helper function to create gray portion of packed barchart

#' @importFrom grDevices "colorRampPalette"
#' @keywords internal
gen_gray_bars = function(summ_dt, number_rows, color_bar_data, label_column, min_label_width) {
  gray_bar_data = summ_dt[order(-summ_dt[['max_rel_val']]),][-c(1:number_rows),]

  # calc row height based on num rows
  bar_h = 1/number_rows
  #calc y vals for bar heights
  row_y_list = lapply(1:number_rows, function(i) {
    list(y0 = 1-bar_h*(i-1), y1 = 1-bar_h*i)
  })

  #initalize storage
  gray_bar_list = vector('list', nrow(gray_bar_data))
  gray_ann_list = vector('list', nrow(gray_bar_data))
  gray_hover_point_list = vector('list', nrow(gray_bar_data))

  #get max x level for each bar level
  row_sums = color_bar_data$max_rel_val
  #gen gray ramp function
  gray_gen = colorRampPalette(c("#E8E8E8","#cccccc"))
  #gen gray ramp
  grays = gray_gen(105)
  low_grays = grays[1:50]
  hi_grays  = grays[56:105]
  last_gray_vec = sample(c(low_grays, hi_grays), number_rows, replace=TRUE)
  for (i in 1:nrow(gray_bar_data)) {
    row = gray_bar_data[i,]

    #calc which row to put block in based on min x value after added this block
    x_val = as.numeric(row[['max_rel_val']])
    home_row = which.min(row_sums + x_val)
    x0 = row_sums[[home_row]]
    x1 = row_sums[[home_row]] + x_val
    row_sums[[home_row]] <- x1

    if (i == 1) {
      # random gray
      color_i = sample(low_grays, 1)
    } else {
      last_gray = last_gray_vec[home_row]
      color_i = ifelse(last_gray %in% low_grays,
                       sample(hi_grays, 1),
                       sample(low_grays, 1))
    }
    last_gray_vec[home_row] = color_i

    #set corners for gray rectangle shape objs (based on derived bar row num)
    out_bar = list(
      type = "rect",
      fillcolor = color_i,
      line = list(color = color_i, width=.1),
      x0 = x0,
      x1 = x1,
      xref = "x",
      y0 = row_y_list[[home_row]]$y0,
      y1 = row_y_list[[home_row]]$y1, yref = "y"
    )

    #calc center x and y point for current bar
    x = x0 + x_val/2
    y = row_y_list[[home_row]]$y0 - (row_y_list[[home_row]]$y0 - row_y_list[[home_row]]$y1)/2
    out_ann = list(
      x = x,
      y = y,
      xref = 'x',
      yref = 'y',
      #set label
      text = row[[label_column]],
      showarrow = FALSE
    )

    #put point at center of shape for hover info
    hover_point = data.table::data.table(
      name = row[[label_column]],
      x = seq(x0, x0+x_val, length.out = 10),
      y = y,
      size = x1-x0
    )

    #dont show annotation if smaller than threshold
    if (x_val < min_label_width) out_ann = NULL

    gray_bar_list[[i]] = out_bar
    gray_ann_list[[i]] = out_ann
    gray_hover_point_list[[i]] = hover_point
  }
  #combine hover info list into single df
  gray_hover_point_dt = data.table::rbindlist(gray_hover_point_list)

  #remove NULLs from annotations (shapes that are too narrow for label)
  gray_ann_list = gray_ann_list[-which(vapply(gray_ann_list, is.null, logical(1)))]

  out = list(raw_data = gray_bar_data,
             bar_list = gray_bar_list,
             ann_list = gray_ann_list,
             hover_point_dt = gray_hover_point_dt,
             row_sums = row_sums)

  out
}
