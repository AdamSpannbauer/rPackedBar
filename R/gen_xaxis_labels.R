#' internal helper function to create xaxis labels

#' @keywords internal
gen_xaxis_labels = function(row_sums, total_x) {
  #gen x axis breaks and labels (cheating by rounding rn)
  max_x = max(row_sums)
  max_val = max_x * total_x
  tick_text = scales::cbreaks(c(0, max_val))
  tick_breaks = tick_text$breaks / total_x
  if (max(tick_text$breaks) > 10000) {
    tick_text = tick_text$labels
  } else {
    tick_text = as.character(tick_text$breaks)
  }

  out = list(tick_text = tick_text,
             tick_breaks = tick_breaks)

  out
}
