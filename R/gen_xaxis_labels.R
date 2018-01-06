gen_xaxis_labels = function(summ_dt, row_sums, value_column) {
  #gen x axis breaks and labels (cheating by rounding rn)
  max_x = max(row_sums)
  max_val = max_x*sum(summ_dt[[value_column]])
  tick_text = scales::cbreaks(c(0, max_val))
  tick_breaks = tick_text$breaks/sum(summ_dt[[value_column]])
  if(max(tick_text$breaks) > 10000) {
    tick_text = tick_text$labels
  } else {
    tick_text = as.character(tick_text$breaks)
  }

  out = list(tick_text = tick_text,
             tick_breaks = tick_breaks)

  out
}
