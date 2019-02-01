#Function to compute the miscalibration error in each bin used to construct the
#calibration simplex.

error = function(x,true_error) {
  UseMethod("error")
}

error.calibration_simplex = function(x,
                                     true_error = TRUE) {
  error = data.frame(a = rep(NA,x$n_bins),
                     b = rep(NA,x$n_bins))
  if(true_error) {
    error$a[x$freq > 0] = x$cond_rel_freq_a[x$freq > 0] - x$cond_p_a_ave[x$freq > 0]
    error$b[x$freq > 0] = x$cond_rel_freq_b[x$freq > 0] - x$cond_p_b_ave[x$freq > 0]
  }
  else {
    rounded_forecasts = make_forecasts(x)
    error$a[x$freq > 0] = x$cond_rel_freq_a[x$freq > 0] - rounded_forecasts$p_a[x$freq > 0]
    error$b[x$freq > 0] = x$cond_rel_freq_b[x$freq > 0] - rounded_forecasts$p_b[x$freq > 0]
  }
  return(error)
}
