context("plotly_packed_bar")

#TODO: add better, more exhaustive tests

test_that("runs", {
  plot_df = data.frame(lab=letters,
                       val=rexp(26),
                       stringsAsFactors = FALSE)
  expect_s3_class(
    plotly_packed_bar(plot_df, 'lab', 'val'),
    c("plotly", "htmlwidget")
  )
})
