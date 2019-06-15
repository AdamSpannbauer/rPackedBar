#' internal function to 'guess' correct number of colored bars in packed barchat using an elbow method

#' @keywords internal
guess_bar_count = function(x, min_bar = 3, max_bar = 25) {
  fit_x = seq_along(x)
  fit_y = sort(x, decreasing = TRUE)

  range_fit_x = c(head(fit_x, 1), tail(fit_x, 1))
  range_fit_y = c(head(fit_y, 1), tail(fit_y, 1))

  fit_df = data.frame(x = range_fit_x, y = range_fit_y)

  # Creating straight line between the max values
  fit = lm(y ~ x, data = fit_df)

  # Distance from point to line
  distances = vapply(seq_along(x), function(i) {
    new_obs = data.frame(x = i)
    pred = c(i, predict(fit, new_obs))
    obs = c(i, fit_y[i])

    dist(rbind(pred, obs))[1]
  }, numeric(1))

  # Max distance point
  elbow_ind = which.max(distances) - 1

  if (min_bar > elbow_ind) elbow_ind = min_bar
  if (max_bar < elbow_ind) elbow_ind = max_bar

  return(elbow_ind)
}



