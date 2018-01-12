context("gen color bars")

test_that("output object class and structure", {
  set.seed(42)
  plot_df = data.frame(lab=letters,
                       val=rexp(26),
                       stringsAsFactors = FALSE)

  plot_df$max_rel_val = plot_df$val/sum(plot_df$val)

  output = gen_color_bars(plot_df, number_rows=3, bar_color='blue', label_column='lab', min_label_width=.03, 'black')

  expect_identical(class(output), "list")

  expect_identical(class(output$raw_data), "data.frame")
  expect_identical(dim(output$raw_data), c(3L,3L))

  expect_identical(class(output$bar_list), "list")
  expect_identical(length(output$bar_list), 3L)
  expect_identical(lapply(output$bar_list, length), list(9L, 9L, 9L))
  expect_identical(names(output$bar_list[[1]]),
                   c("type", "fillcolor", "line",
                     "x0", "x1", "xref", "y0", "y1", "yref"))
  expect_identical(lapply(output$bar_list[[1]], class),
                   list(type = "character", fillcolor = "character", line = "list",
                        x0 = "numeric", x1 = "numeric", xref = "character", y0 = "numeric",
                        y1 = "numeric", yref = "character"))

  expect_identical(class(output$ann_list), "list")
  expect_identical(length(output$ann_list), 3L)
  expect_identical(lapply(output$ann_list, length), list(7L, 7L, 7L))
  expect_identical(names(output$ann_list[[1]]),
                   c("x", "y", "xref", "yref", "text", "showarrow", "font"))
  expect_identical(lapply(output$ann_list[[1]], class),
                   list(x = "numeric", y = "numeric", xref = "character",
                        yref = "character", text = "character", showarrow = "logical", font="list"))

  expect_identical(class(output$hover_point_dt), c("data.table", "data.frame"))
  expect_identical(dim(output$hover_point_dt), c(3L,4L))
  expect_identical(names(output$hover_point_dt), c("name", "x", "y", "size"))
  expect_identical(lapply(output$hover_point_dt, class), list(name = "character", x = "numeric",
                                                              y = "numeric", size = "numeric"))
})

test_that("object value", {
  set.seed(42)
  plot_df = data.frame(lab=letters,
                       val=rexp(26),
                       stringsAsFactors = FALSE)

  plot_df$max_rel_val = plot_df$val/sum(plot_df$val)

  output = gen_color_bars(plot_df, number_rows=3,
                          bar_color='blue',
                          label_column='lab', min_label_width=.03, 'black')

  output$hover_point_dt = as.data.frame(output$hover_point_dt)

  sink(tempfile())
  test = dput(output)
  sink()

  expected = structure(list(raw_data = structure(list(lab = c("w", "u", "l"),
                                                      val = c(4.99596849545054, 4.86280499555566, 2.40868442953805),
                                                      max_rel_val = c(0.187743417417093, 0.182739268458137, 0.0905159123186251)),
                                                 .Names = c("lab", "val", "max_rel_val"),
                                                 row.names = c(23L, 21L, 12L),
                                                 class = "data.frame"),
                            bar_list = list(structure(list(type = "rect",
                                                           fillcolor = "blue",
                                                           line = structure(list(color = "blue",
                                                                                 width = 0.1),
                                                                            .Names = c("color", "width")),
                                                           x0 = 0, x1 = 0.187743417417093, xref = "x",y0 = 1, y1 = 0.666666666666667, yref = "y"),
                                                      .Names = c("type", "fillcolor", "line", "x0", "x1", "xref", "y0", "y1", "yref")),
                                            structure(list(type = "rect",
                                                           fillcolor = "blue",
                                                           line = structure(list(color = "blue",
                                                                                 width = 0.1),
                                                                            .Names = c("color", "width")),
                                                           x0 = 0, x1 = 0.182739268458137, xref = "x", y0 = 0.666666666666667, y1 = 0.333333333333333, yref = "y"),
                                                      .Names = c("type", "fillcolor", "line", "x0", "x1", "xref", "y0", "y1", "yref")),
                                            structure(list(type = "rect",
                                                           fillcolor = "blue",
                                                           line = structure(list(color = "blue", width = 0.1),
                                                                            .Names = c("color", "width")),
                                                           x0 = 0, x1 = 0.0905159123186251, xref = "x", y0 = 0.333333333333333, y1 = 0, yref = "y"),
                                                      .Names = c("type", "fillcolor", "line", "x0", "x1", "xref", "y0", "y1", "yref"))),
                            ann_list = list(structure(list(x = 0.0938717087085463, y = 0.833333333333333,
                                                           xref = "x", yref = "y",
                                                           text = "w", showarrow = FALSE, font=list(color="black")),
                                                      .Names = c("x", "y", "xref", "yref", "text", "showarrow", "font")),
                                            structure(list(x = 0.0913696342290684, y = 0.5,
                                                           xref = "x", yref = "y",
                                                           text = "u", showarrow = FALSE, font=list(color="black")),
                                                      .Names = c("x", "y", "xref", "yref", "text", "showarrow", "font")),
                                            structure(list(x = 0.0452579561593126, y = 0.166666666666667,
                                                           xref = "x", yref = "y",
                                                           text = "l", showarrow = FALSE, font=list(color="black")),
                                                      .Names = c("x", "y", "xref", "yref", "text", "showarrow", "font"))),
                            hover_point_dt = structure(list(name = c("w", "u", "l"),
                                                            x = c(0.0938717087085463, 0.0913696342290684, 0.0452579561593126),
                                                            y = c(0.833333333333333, 0.5, 0.166666666666667),
                                                            size = c(0.187743417417093, 0.182739268458137, 0.0905159123186251)),
                                                       .Names = c("name", "x", "y", "size"),
                                                       row.names = c(NA, -3L),
                                                       class = c("data.frame"))))

  expect_equivalent(test, expected)
})
