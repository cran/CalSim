#Function to construct the forecast vectors for the forecasts
#corresponding to the centers of the hexagons in the calibration simplex.

make_forecasts = function(x) {
  UseMethod("make_forecasts")
}

make_forecasts.calibration_simplex = function(x) {
  n = x$n
  n_bins = x$n_bins
  forecasts = data.frame(p_a = rep(0,n_bins),p_b=rep(0,n_bins))
  k = n-2
  pa = 0
  pb = n-1
  for(i in 1:n_bins) {
    forecasts[i,] = c(pa,pb)
    if(pb>0) {
      pa=pa+1
      pb=pb+-1
    }
    else {
      pa=0
      pb=k
      k=k-1
    }
  }
  return(forecasts/(n-1))
}
