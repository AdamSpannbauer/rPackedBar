context("gen gray bars")

test_that("output object class and structure", {
  set.seed(42)
  plot_df = data.frame(lab=letters,
                       val=rexp(26),
                       stringsAsFactors = FALSE)

  plot_df$max_rel_val = plot_df$val/sum(plot_df$val)

  color_output = gen_color_bars(plot_df, number_rows=3, bar_color='blue', label_column='lab', min_label_width=.03, 'black')

  output = gen_gray_bars(plot_df, number_rows=3,
                         color_output$raw_data,
                         "lab", min_label_width=0.03)

  expect_identical(class(output), "list")

  expect_identical(class(output$raw_data), "data.frame")
  expect_identical(dim(output$raw_data), c(23L,3L))

  expect_identical(class(output$bar_list), "list")
  expect_identical(length(output$bar_list), 23L)
  expect_identical(lapply(output$bar_list, length),
                   list(9L, 9L, 9L, 9L, 9L, 9L, 9L, 9L,
                        9L, 9L, 9L, 9L, 9L, 9L, 9L, 9L,
                        9L, 9L, 9L, 9L, 9L, 9L, 9L))
  expect_identical(names(output$bar_list[[1]]),
                   c("type", "fillcolor", "line",
                     "x0", "x1", "xref", "y0", "y1", "yref"))
  expect_identical(lapply(output$bar_list[[1]], class),
                   list(type = "character", fillcolor = "character", line = "list",
                        x0 = "numeric", x1 = "numeric", xref = "character", y0 = "numeric",
                        y1 = "numeric", yref = "character"))

  expect_identical(class(output$ann_list), "list")
  expect_identical(length(output$ann_list), 6L)
  expect_identical(lapply(output$ann_list, length),
                   list(6L, 6L, 6L, 6L, 6L, 6L))
  expect_identical(names(output$ann_list[[1]]),
                   c("x", "y", "xref", "yref", "text", "showarrow"))
  expect_identical(lapply(output$ann_list[[1]], class),
                   list(x = "numeric", y = "numeric", xref = "character",
                        yref = "character", text = "character", showarrow = "logical"))

  expect_identical(class(output$hover_point_dt), c("data.table", "data.frame"))
  expect_identical(dim(output$hover_point_dt), c(23L,4L))
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

  color_output = gen_color_bars(plot_df, number_rows=3, bar_color='blue', label_column='lab', min_label_width=.03, 'black')

  output = gen_gray_bars(plot_df, number_rows=3,
                         color_output$raw_data,
                         "lab", min_label_width=0.03)

  output$hover_point_dt = as.data.frame(output$hover_point_dt)

  sink(tempfile())
  test = dput(output)
  sink()


  expected = structure(list(raw_data = structure(list(lab = c("f", "k", "o",
                                                              "s", "y", "i", "z", "j", "v", "b", "r", "q", "e", "h", "t", "g",
                                                              "p", "c", "x", "a", "m", "n", "d"),
                                                      val = c(1.46362713902237,1.34471559669334, 1.25396104897828, 1.24558031182261, 1.211384142707,
                                                              1.19159781029892, 0.719092395454044, 0.714862477923644, 0.665832160506397,
                                                              0.660895252134651, 0.622110282536596, 0.475191235542297, 0.473176629282534,
                                                              0.410129568073899, 0.370339458808303, 0.313984580803663, 0.309815737514284,
                                                              0.283491037786007, 0.223557286895812, 0.198336811783888, 0.0961465616733308,
                                                              0.0571413609690035, 0.0381918982602656),
                                                      max_rel_val = c(0.055001620037174,0.050533045155743, 0.0471225815089681, 0.0468076419260796, 0.0455225846527637,
                                                                      0.0447790343946253, 0.027022761229204, 0.0268638052283259, 0.0250212957414584,
                                                                      0.0248357717434527, 0.0233782720127423, 0.0178572035769645, 0.0177814967216667,
                                                                      0.0154122522518121, 0.0139169803941682, 0.0117992213661941, 0.0116425604732088,
                                                                      0.0106533050177418, 0.00840105558482471, 0.00745329576794296,
                                                                      0.00361309005008578, 0.00214731425827715, 0.00143521271272179
                                                                      )), .Names = c("lab", "val", "max_rel_val"),
                                                 row.names = c(6L, 11L, 15L, 19L, 25L, 9L, 26L, 10L, 22L, 2L, 18L, 17L, 5L, 8L,
                                                               20L, 7L, 16L, 3L, 24L, 1L, 13L, 14L, 4L),
                                                 class = "data.frame"),
                            bar_list = list(structure(list(type = "rect", fillcolor = "#CDCDCD",
                                                           line = structure(list(color = "#CDCDCD", width = 0.1),
                                                                            .Names = c("color","width")),
                                                           x0 = 0.0905159123186251, x1 = 0.145517532355799,
                                                           xref = "x", y0 = 0.333333333333333, y1 = 0, yref = "y"),
                                                      .Names = c("type","fillcolor", "line", "x0", "x1", "xref", "y0", "y1", "yref")),
                                            structure(list(type = "rect", fillcolor = "#909090",
                                                           line = structure(list(color = "#909090", width = 0.1),
                                                                            .Names = c("color","width")),
                                                           x0 = 0.145517532355799, x1 = 0.196050577511542,
                                                           xref = "x", y0 = 0.333333333333333, y1 = 0, yref = "y"),
                                                      .Names = c("type","fillcolor", "line", "x0", "x1", "xref", "y0", "y1", "yref")),
                                            structure(list(type = "rect", fillcolor = "#CECECE",
                                                           line = structure(list(color = "#CECECE", width = 0.1),
                                                                            .Names = c("color","width")),
                                                           x0 = 0.182739268458137, x1 = 0.229861849967105,
                                                           xref = "x", y0 = 0.666666666666667, y1 = 0.333333333333333,yref = "y"),
                                                      .Names = c("type", "fillcolor", "line","x0", "x1", "xref", "y0", "y1", "yref")),
                                            structure(list(type = "rect", fillcolor = "#ABABAB",
                                                           line = structure(list(color = "#ABABAB", width = 0.1),
                                                                            .Names = c("color","width")),
                                                           x0 = 0.187743417417093, x1 = 0.234551059343172,
                                                           xref = "x", y0 = 1, y1 = 0.666666666666667, yref = "y"),
                                                      .Names = c("type","fillcolor", "line", "x0", "x1", "xref", "y0", "y1", "yref")),
                                            structure(list(type = "rect", fillcolor = "#D9D9D9",line = structure(list(color = "#D9D9D9", width = 0.1),
                                                                                                                 .Names = c("color","width")),
                                                           x0 = 0.196050577511542, x1 = 0.241573162164306,
                                                           xref = "x", y0 = 0.333333333333333, y1 = 0, yref = "y"),
                                                      .Names = c("type","fillcolor", "line", "x0", "x1", "xref", "y0", "y1", "yref")),
                                            structure(list(type = "rect", fillcolor = "#A9A9A9",
                                                           line = structure(list(color = "#A9A9A9", width = 0.1),
                                                                            .Names = c("color","width")),
                                                           x0 = 0.229861849967105, x1 = 0.27464088436173,
                                                           xref = "x", y0 = 0.666666666666667, y1 = 0.333333333333333,yref = "y"),
                                                      .Names = c("type", "fillcolor", "line","x0", "x1", "xref", "y0", "y1", "yref")),
                                            structure(list(type = "rect", fillcolor = "#C7C7C7",
                                                           line = structure(list(color = "#C7C7C7", width = 0.1),
                                                                            .Names = c("color","width")),
                                                           x0 = 0.234551059343172, x1 = 0.261573820572376,
                                                           xref = "x", y0 = 1, y1 = 0.666666666666667, yref = "y"),
                                                      .Names = c("type","fillcolor", "line", "x0", "x1", "xref", "y0", "y1", "yref")),
                                            structure(list(type = "rect", fillcolor = "#B8B8B8",
                                                           line = structure(list(color = "#B8B8B8", width = 0.1),
                                                                            .Names = c("color","width")),
                                                           x0 = 0.241573162164306, x1 = 0.268436967392632,
                                                           xref = "x", y0 = 0.333333333333333, y1 = 0, yref = "y"),
                                                      .Names = c("type","fillcolor", "line", "x0", "x1", "xref", "y0", "y1", "yref")),
                                            structure(list(type = "rect", fillcolor = "#9A9A9A",
                                                           line = structure(list(color = "#9A9A9A", width = 0.1),
                                                                            .Names = c("color","width")),
                                                           x0 = 0.261573820572376, x1 = 0.286595116313835,
                                                           xref = "x", y0 = 1, y1 = 0.666666666666667, yref = "y"),
                                                      .Names = c("type","fillcolor", "line", "x0", "x1", "xref", "y0", "y1", "yref")),
                                            structure(list(type = "rect", fillcolor = "#CCCCCC",
                                                           line = structure(list(color = "#CCCCCC", width = 0.1),
                                                                            .Names = c("color","width")),
                                                           x0 = 0.268436967392632, x1 = 0.293272739136084,
                                                           xref = "x", y0 = 0.333333333333333, y1 = 0, yref = "y"),
                                                      .Names = c("type","fillcolor", "line", "x0", "x1", "xref", "y0", "y1", "yref")),
                                            structure(list(type = "rect", fillcolor = "#E1E1E1",
                                                           line = structure(list(color = "#E1E1E1", width = 0.1),
                                                                            .Names = c("color","width")),
                                                           x0 = 0.27464088436173, x1 = 0.298019156374472,
                                                           xref = "x", y0 = 0.666666666666667, y1 = 0.333333333333333,yref = "y"),
                                                      .Names = c("type", "fillcolor", "line","x0", "x1", "xref", "y0", "y1", "yref")),
                                            structure(list(type = "rect", fillcolor = "#DDDDDD",
                                                           line = structure(list(color = "#DDDDDD", width = 0.1),
                                                                            .Names = c("color","width")),
                                                           x0 = 0.286595116313835, x1 = 0.304452319890799,
                                                           xref = "x", y0 = 1, y1 = 0.666666666666667, yref = "y"),
                                                      .Names = c("type","fillcolor", "line", "x0", "x1", "xref", "y0", "y1", "yref")),
                                            structure(list(type = "rect", fillcolor = "#A4A4A4",
                                                           line = structure(list(color = "#A4A4A4", width = 0.1),
                                                                            .Names = c("color","width")),
                                                           x0 = 0.293272739136084, x1 = 0.311054235857751,
                                                           xref = "x", y0 = 0.333333333333333, y1 = 0, yref = "y"),
                                                      .Names = c("type","fillcolor", "line", "x0", "x1", "xref", "y0", "y1", "yref")),
                                            structure(list(type = "rect", fillcolor = "#9D9D9D",
                                                           line = structure(list(color = "#9D9D9D", width = 0.1),
                                                                            .Names = c("color","width")), x0 = 0.298019156374472, x1 = 0.313431408626285,
                                                           xref = "x", y0 = 0.666666666666667, y1 = 0.333333333333333,yref = "y"),
                                                      .Names = c("type", "fillcolor", "line","x0", "x1", "xref", "y0", "y1", "yref")),
                                            structure(list(type = "rect", fillcolor = "#909090",
                                                           line = structure(list(color = "#909090", width = 0.1),
                                                                            .Names = c("color","width")),
                                                           x0 = 0.304452319890799, x1 = 0.318369300284967,
                                                           xref = "x", y0 = 1, y1 = 0.666666666666667, yref = "y"),
                                                      .Names = c("type","fillcolor", "line", "x0", "x1", "xref", "y0", "y1", "yref")),
                                            structure(list(type = "rect", fillcolor = "#C8C8C8",
                                                           line = structure(list(color = "#C8C8C8", width = 0.1),
                                                                            .Names = c("color","width")),
                                                           x0 = 0.311054235857751, x1 = 0.322853457223945,
                                                           xref = "x", y0 = 0.333333333333333, y1 = 0, yref = "y"),
                                                      .Names = c("type","fillcolor", "line", "x0", "x1", "xref", "y0", "y1", "yref")),
                                            structure(list(type = "rect", fillcolor = "#D0D0D0",
                                                           line = structure(list(color = "#D0D0D0", width = 0.1),
                                                                            .Names = c("color","width")),
                                                           x0 = 0.313431408626285, x1 = 0.325073969099493,
                                                           xref = "x", y0 = 0.666666666666667, y1 = 0.333333333333333,yref = "y"),
                                                      .Names = c("type", "fillcolor", "line","x0", "x1", "xref", "y0", "y1", "yref")),
                                            structure(list(type = "rect", fillcolor = "#C4C4C4",
                                                           line = structure(list(color = "#C4C4C4", width = 0.1),
                                                                            .Names = c("color","width")),
                                                           x0 = 0.318369300284967, x1 = 0.329022605302709,
                                                           xref = "x", y0 = 1, y1 = 0.666666666666667, yref = "y"),
                                                      .Names = c("type","fillcolor", "line", "x0", "x1", "xref", "y0", "y1", "yref")),
                                            structure(list(type = "rect", fillcolor = "#B1B1B1",
                                                           line = structure(list(color = "#B1B1B1", width = 0.1),
                                                                            .Names = c("color","width")),
                                                           x0 = 0.322853457223945, x1 = 0.33125451280877,
                                                           xref = "x", y0 = 0.333333333333333, y1 = 0, yref = "y"),
                                                      .Names = c("type","fillcolor", "line", "x0", "x1", "xref", "y0", "y1", "yref")),
                                            structure(list(type = "rect", fillcolor = "#AEAEAE",
                                                           line = structure(list(color = "#AEAEAE", width = 0.1),
                                                                            .Names = c("color","width")),
                                                           x0 = 0.325073969099493, x1 = 0.332527264867436,
                                                           xref = "x", y0 = 0.666666666666667, y1 = 0.333333333333333,yref = "y"),
                                                      .Names = c("type", "fillcolor", "line","x0", "x1", "xref", "y0", "y1", "yref")),
                                            structure(list(type = "rect", fillcolor = "#969696",
                                                           line = structure(list(color = "#969696", width = 0.1),
                                                                            .Names = c("color","width")),
                                                           x0 = 0.329022605302709, x1 = 0.332635695352795,
                                                           xref = "x", y0 = 1, y1 = 0.666666666666667, yref = "y"),
                                                      .Names = c("type","fillcolor", "line", "x0", "x1", "xref", "y0", "y1", "yref")),
                                            structure(list(type = "rect", fillcolor = "#CBCBCB",
                                                           line = structure(list(color = "#CBCBCB", width = 0.1),
                                                                            .Names = c("color","width")),
                                                           x0 = 0.33125451280877, x1 = 0.333401827067047,
                                                           xref = "x", y0 = 0.333333333333333, y1 = 0, yref = "y"),
                                                      .Names = c("type","fillcolor", "line", "x0", "x1", "xref", "y0", "y1", "yref")),
                                            structure(list(type = "rect", fillcolor = "#DDDDDD",
                                                           line = structure(list(color = "#DDDDDD", width = 0.1),
                                                                            .Names = c("color","width")),
                                                           x0 = 0.332527264867436, x1 = 0.333962477580158,
                                                           xref = "x", y0 = 0.666666666666667, y1 = 0.333333333333333,yref = "y"),
                                                      .Names = c("type", "fillcolor", "line","x0", "x1", "xref", "y0", "y1", "yref"))),
                            ann_list = list(structure(list(x = 0.118016722337212, y = 0.166666666666667,
                                                           xref = "x", yref = "y", text = "f", showarrow = FALSE),
                                                      .Names = c("x","y", "xref", "yref", "text", "showarrow")),
                                            structure(list(x = 0.170784054933671, y = 0.166666666666667,
                                                           xref = "x",yref = "y", text = "k", showarrow = FALSE),
                                                      .Names = c("x","y", "xref", "yref", "text", "showarrow")),
                                            structure(list(x = 0.206300559212621, y = 0.5,
                                                           xref = "x", yref = "y",text = "o", showarrow = FALSE),
                                                      .Names = c("x", "y","xref", "yref", "text", "showarrow")),
                                            structure(list(x = 0.211147238380132, y = 0.833333333333333,
                                                           xref = "x",yref = "y", text = "s", showarrow = FALSE),
                                                      .Names = c("x","y", "xref", "yref", "text", "showarrow")),
                                            structure(list(x = 0.218811869837924, y = 0.166666666666667,
                                                           xref = "x",yref = "y", text = "y", showarrow = FALSE),
                                                      .Names = c("x","y", "xref", "yref", "text", "showarrow")),
                                            structure(list(x = 0.252251367164417, y = 0.5,
                                                           xref = "x", yref = "y",text = "i", showarrow = FALSE),
                                                      .Names = c("x", "y","xref", "yref", "text", "showarrow"))),
                            hover_point_dt = structure(list(name = c("f", "k", "o", "s", "y", "i", "z", "j", "v","b", "r",
                                                                     "q", "e", "h", "t", "g", "p", "c", "x", "a","m", "n", "d"),
                                                            x = c(0.118016722337212, 0.170784054933671,0.206300559212621,
                                                                  0.211147238380132, 0.218811869837924,0.252251367164417,
                                                                  0.248062439957774, 0.255005064778469,0.274084468443105,
                                                                  0.280854853264358, 0.286330020368101,0.295523718102317,
                                                                  0.302163487496918, 0.305725282500379,0.311410810087883,
                                                                  0.316953846540848, 0.319252688862889,0.323695952793838,
                                                                  0.327053985016358, 0.328800616983465,0.330829150327752,
                                                                  0.332328169937909, 0.333244871223797),
                                                            y = c(0.166666666666667, 0.166666666666667, 0.5, 0.833333333333333,
                                                                  0.166666666666667, 0.5, 0.833333333333333, 0.166666666666667,
                                                                  0.833333333333333, 0.166666666666667, 0.5, 0.833333333333333,
                                                                  0.166666666666667, 0.5, 0.833333333333333, 0.166666666666667,
                                                                  0.5, 0.833333333333333, 0.166666666666667, 0.5, 0.833333333333333,
                                                                  0.166666666666667, 0.5),
                                                            size = c(0.055001620037174,0.050533045155743, 0.0471225815089681,
                                                                     0.0468076419260796,0.0455225846527637, 0.0447790343946253,
                                                                     0.027022761229204,0.0268638052283259, 0.0250212957414584,
                                                                     0.0248357717434527,0.0233782720127423, 0.0178572035769645,
                                                                     0.0177814967216666,0.0154122522518121, 0.0139169803941682,
                                                                     0.0117992213661942,0.0116425604732088, 0.0106533050177418,
                                                                     0.00840105558482473,0.00745329576794296, 0.00361309005008575,
                                                                     0.00214731425827713,0.00143521271272179)),
                                                       .Names = c("name", "x", "y", "size"),
                                                       row.names = c(NA, -23L), class = "data.frame"),
                            row_sums = c(0.332635695352795,0.333962477580158, 0.333401827067047)),
                       .Names = c("raw_data","bar_list", "ann_list", "hover_point_dt", "row_sums"))

  expect_equivalent(test, expected)
})

