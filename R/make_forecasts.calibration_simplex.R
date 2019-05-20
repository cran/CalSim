#Function to construct the forecast vectors for the forecasts
#corresponding to the centers of the hexagons in the calibration simplex.

make_forecasts = function(x) {
  UseMethod("make_forecasts")
}

make_forecasts.calibration_simplex = function(x) {
  n = x$n
  n_bins = x$n_bins
  forecasts = data.frame(p1=rep(0,n_bins),p3 = rep(0,n_bins))
  k = n-2
  p3 = 0
  p1 = n-1
  for(i in 1:n_bins) {
    forecasts[i,] = c(p3,p1)
    if(p1>0) {
      p3=p3+1
      p1=p1+-1
    }
    else {
      p3=0
      p1=k
      k=k-1
    }
  }
  return(forecasts/(n-1))
}
