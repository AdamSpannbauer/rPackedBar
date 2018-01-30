#' internal function to 'guess' correct number of colored bars in packed barchat using an elbow method

#' @keywords internal
guess_bar_count = function(num_vec, min_bar = 3, max_bar = 25) {
  sorted_num_vec = sort(abs(num_vec), decreasing = TRUE)

  low_perc = 0.08
  hi_perc  = 0.9

  perc_of_total = sorted_num_vec/sum(sorted_num_vec)
  while (low_perc >= max(perc_of_total)) {
    low_perc <- low_perc/2
  }

  not_too_high = (perc_of_total < low_perc)
  not_too_low  = perc_of_total > low_perc/8
  candidate_inds = which(not_too_high & not_too_low)
  too_high = length(which(!not_too_high))
  if (length(candidate_inds) > 0) {
    if (length(candidate_inds) == 1) {
      elbow = candidate_inds
    } else {
      candidate_percs = perc_of_total[candidate_inds]
      perc_drops = abs(diff(candidate_percs))/(candidate_percs[1:(length(candidate_percs) - 1)])
      is_inf = is.infinite(perc_drops)
      elbow = which(perc_drops == max(perc_drops[!is_inf], na.rm = TRUE))[1] + too_high
    }
  } else {
    elbow = min_bar
  }
  if (elbow > max_bar) {
    elbow = max_bar
  }

  names(elbow) = NULL
  return(elbow)
}
