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
#' @examples
#' \dontrun{
#' data(GNI2014, package = 'treemap')
#'
#' data.table::setDT(GNI2014)
#' my_input_data = GNI2014[,sum(population), by=country]
#'
#' plotly_packed_bar(input_data,
#'                   label_column    = 'country',
#'                   value_column    = 'V1',
#'                   number_rows     = 4,
#'                   plot_title      = 'Population 2014',
#'                   xaxis_label     = 'Population',
#'                   hover_label     = 'Population',
#'                   min_label_width = .025,
#'                   color_bar_color ='orange')
#' }

#function to take data.frame and create plotly packed bar
plotly_packed_bar = function(input_data, label_column, value_column,
                             number_rows=3,
                             plot_title='',
                             xaxis_label='',
                             hover_label='',
                             min_label_width=.03,
                             color_bar_color='steelblue') {

  my_data_sum = data.table::copy(input_data)
  my_data_sum$max_rel_val = my_data_sum[[value_column]]/sum(my_data_sum[[value_column]])

  # calc row height based on num rows
  bar_h = 1/number_rows
  # set aside data for colored bars
  colored_bar_data = my_data_sum[order(-my_data_sum[['max_rel_val']])][1:number_rows]
  # set aside data for grey bars
  gray_bar_data = my_data_sum[order(-my_data_sum[['max_rel_val']])][-c(1:number_rows)]

  #calc y vals for bar heights
  row_y_list = lapply(1:number_rows, function(i) {
    list(y0 = 1-bar_h*(i-1), y1 = 1-bar_h*i)
  })

  #initalize storage
  colored_bar_list = vector('list', nrow(colored_bar_data))
  colored_ann_list = vector('list', nrow(colored_bar_data))
  colored_hover_point_list = vector('list', nrow(colored_bar_data))
  gray_bar_list = vector('list', nrow(gray_bar_data))
  gray_ann_list = vector('list', nrow(gray_bar_data))
  gray_hover_point_list = vector('list', nrow(gray_bar_data))

  #track which bar level we're in
  color_row_i = 1
  #loop through n bar levels
  for (i in 1:nrow(colored_bar_data)) {
    row = colored_bar_data[i,]

    #set corners for colored rectangle shape objs
    out_bar = list(
      type = "rect",
      fillcolor = color_bar_color,
      line = list(color = color_bar_color, width=.1),
      x0 = 0,
      x1 = row[['max_rel_val']],
      xref = "x",
      y0 = row_y_list[[color_row_i]]$y0,
      y1 = row_y_list[[color_row_i]]$y1,
      yref = "y"
    )

    #calc center x and y point for current bar
    x = as.numeric(row[['max_rel_val']])/2
    y = row_y_list[[color_row_i]]$y0 - (row_y_list[[color_row_i]]$y0 - row_y_list[[color_row_i]]$y1)/2
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
      x = x,
      y = y,
      size = row[['max_rel_val']]-0
    )

    colored_bar_list[[i]] = out_bar
    colored_ann_list[[i]] = out_ann
    colored_hover_point_list[[i]] = hover_point

    color_row_i = 1+color_row_i
  }
  #combine hover info list into single df
  colored_hover_point_dt = data.table::rbindlist(colored_hover_point_list)

  #get max x level for each bar level
  row_sums = colored_bar_data$max_rel_val
  #gen gray ramp function
  gray_gen = colorRampPalette(c("#E8E8E8","#909090"))
  #gen gray ramp
  grays = gray_gen(20)
  for (i in 1:nrow(gray_bar_data)) {
    row = gray_bar_data[i,]
    # random gray
    color_i = sample(grays, 1)

    #calc which row to put block in based on min x value after added this block
    x_val = as.numeric(row[['max_rel_val']])
    home_row = which.min(row_sums + x_val)
    x0 = row_sums[[home_row]]
    x1 = row_sums[[home_row]] + x_val
    row_sums[[home_row]] <- x1

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
      x = x,
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


  #set canvas shape based on xvalues. make y points 0-1
  canvas_df = data.frame(x=0:max(row_sums), y=0:1)

  #gen x axis breaks and labels (cheating by rounding rn)
  tick_breaks = seq(0, max(row_sums)*.9, length.out = 5)
  tick_text = tick_breaks*sum(my_data_sum[[value_column]])
  round_num = nchar(round(min(tick_text[tick_text>0]),0))-1
  tick_text = format(round(tick_text, -round_num), big.mark = ',')

  #plot blank canvas and hover info points
  p = plotly::plot_ly(canvas_df,
                      x = ~x,
                      y = ~y,
                      mode='markers',
                      type='scatter',
                      opacity=0,
                      hoverinfo='none',
                      showlegend=FALSE) %>%
    plotly::add_trace(data=gray_hover_point_dt,
                      x=~x,
                      y=~y,
                      size=~size,
                      hoverinfo='text',
                      text=~paste0(
                        name,'<br>',hover_label,':',
                        format(size*sum(my_data_sum[[value_column]]),big.mark=',')),
                      opacity=0,
                      marker=list(color='#E8E8E8'),
                      showlegend=FALSE) %>%
    plotly::add_trace(data=colored_hover_point_dt,
                      x=~x,
                      y=~y,
                      size=~size,
                      hoverinfo='text',
                      text=~paste0(
                        name,'<br>',hover_label,':',
                        format(size*sum(my_data_sum[[value_column]]),big.mark=',')),
                      opacity=0,
                      marker=list(color=color_bar_color),
                      showlegend=FALSE)

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
                     hovermode="closest",
                     yaxis=list(title='',
                                zeroline=FALSE,
                                showline=FALSE,
                                showticklabels=FALSE,
                                showgrid=FALSE),
                     xaxis=list(title=xaxis_label,
                                zeroline=FALSE,
                                showline=FALSE,
                                showticklabels=TRUE,
                                showgrid=TRUE,
                                tickvals=tick_breaks,
                                ticktext=tick_text)
                     )
  p
}
