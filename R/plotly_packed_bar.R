#' Create a plotly packed bar chart

#' Packed bar charts are a variation of treemaps for visualizing skewed data.  The concept was introduced by XanGregg at JMP (\url{https://community.jmp.com/t5/JMP-Blog/Introducing-packed-bars-a-new-chart-form/ba-p/39972}).
#' @param input_data data.frame with data to plot, should have a column of labels for bars and column of numbers relating to bar length
#' @param label_column either the column number or quoted name in \code{input_data} to be used as labels (will be used by \code{[[]} to subset)
#' @param value_column either the column number or quoted name in \code{input_data} to be used as numbers for bar lengths (will be used by \code{[[]} to subset)
#' @param number_rows the number of rows to occur in barchart (i.e. the number of colored bars)
#' @param plot_title main title for plot
#' @param xaxis_label label to put on xaxis
#' @param hover_label text to appear by number in hover information (typically same as xaxis label)
#' @param min_label_width min relative length of bar to receive a static label (too small and text will overflow sides of bar)
#' @param color_bar_color color of main bars in chart (can be name or hex) remaining bars will be variations of gray
#' @return plotly object of the packed bar chart
#' @importFrom plotly "%>%"
#' @details The packed barchart currently only works for positive data.
#' @examples
#' \dontrun{
#' data(GNI2014, package = 'treemap')
#'
#' data.table::setDT(GNI2014)
#' my_input_data = GNI2014[,sum(population), by=country]
#'
#' plotly_packed_bar(my_input_data,
#'                   label_column    = 'country',
#'                   value_column    = 'V1',
#'                   number_rows     = 4,
#'                   plot_title      = 'Population 2014',
#'                   xaxis_label     = 'Population',
#'                   hover_label     = 'Population',
#'                   min_label_width = .025,
#'                   color_bar_color ='orange')
#' }

#' @export
plotly_packed_bar = function(input_data, label_column, value_column,
                             number_rows=3,
                             plot_title='',
                             xaxis_label='',
                             hover_label='',
                             min_label_width=.03,
                             color_bar_color='steelblue') {

  my_data_sum = data.table::copy(input_data)
  my_data_sum$max_rel_val = my_data_sum[[value_column]]/sum(my_data_sum[[value_column]])

  color_data = gen_color_bars(my_data_sum, number_rows, color_bar_color, label_column)
  gray_data  = gen_gray_bars(my_data_sum, number_rows, color_data$raw_data, label_column, min_label_width)

  #set canvas shape based on xvalues. make y points 0-1
  canvas_df = data.frame(x=0:max(gray_data$row_sums), y=0:1)

  x_labs = gen_xaxis_labels(gray_data$row_sums, sum(my_data_sum[[value_column]]))

  p = gen_plotly_packed_bar(my_data_sum, value_column,
                            color_bar_color, hover_label,
                            canvas_df,
                            plot_title, xaxis_label,
                            gray_data$hover_point_dt, color_data$hover_point_dt,
                            gray_data$bar_list, color_data$bar_list,
                            gray_data$ann_list, color_data$ann_list,
                            x_labs$tick_breaks, x_labs$tick_text)

  return(p)
}

