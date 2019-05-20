#' @return Object of class \code{calibration_simplex}.
#'
#' @rdname calibration_simplex
#' @export
#'
#' @note In contrast to the calibration simplex proposed by Daniel S. Wilks, 2013, the simplex has been
#' mirrored at the diagonal through the left bottom hexagon. The miscalibration error is by default calculated
#' precisely (in each bin as the difference of the relative frequencies of each class and the
#' average forecast probabilities) instead of approximately (using Wilks original formula).
#' Approximate errors can be used by setting \code{true_error = FALSE} when using \code{\link{plot.calibration_simplex}}.
#'
#' @references Daniel S. Wilks, 2013, The Calibration Simplex: A Generalization of the Reliability Diagram for Three-Category Probability Forecasts, \emph{Weather and Forecasting}, \strong{28}, 1210-1218
#' @seealso \code{\link{plot.calibration_simplex}}
#' @seealso \code{\link{ternary_forecast_example}}
#'
#' @importFrom stats aggregate

calibration_simplex.default = function(n = 10,
                                       p1 = NULL,
                                       p2 = NULL,
                                       p3 = NULL,
                                       obs = NULL,
                                       percentagewise = FALSE,
                                       p_a = NULL,
                                       p_n = NULL,
                                       p_b = NULL) {
  factor_percent = if(percentagewise) 100 else 1 #=div (prev)

  if(!is.null(p_a)){
    warning("The parameter p_a is deprecated and will be removed. Please use p3 instead!")
    if(is.null(p3)) p3 = p_a
  }
  if(!is.null(p_b)){
    warning("The parameter p_b is deprecated and will be removed. Please use p1 instead!")
    if(is.null(p1)) p1 = p_b
  }
  if(!is.null(p_n)){
    warning("The parameter p_n is deprecated and will be removed. Please use p2 instead!")
    if(is.null(p2)) p2 = p_n
  }



  if(is.null(obs)) stop("Observations are missing!")
  stopifnot(all(obs %in% c(1,2,3)))

  eps = 0.01

  if(is.null(p3)) {
    if(is.null(p1)||is.null(p2)) stop("Probability vectors are missing!")
    if(any(p2 < 0)||any(p1 < 0)) stop("Negative probabilities detected!")
    if(any(p2+p1>(1 + eps)*factor_percent)) stop("Specified probabilities do not sum to <=1!")
    p3 = factor_percent-p1-p2
  }

  else if(is.null(p1)) {
    if(is.null(p3)||is.null(p2)) stop("Probability vectors are missing!")
    if(any(p2 < 0)||any(p3 < 0)) stop("Negative probabilities detected!")
    if(any(p3+p2>(1 + eps)*factor_percent)) stop("Specified probabilities do not sum to <=1!")
    p1 = factor_percent-p3-p2
  }

  else if(is.null(p2)) {
    if(any(p3 < 0)||any(p1 < 0)) stop("Negative probabilities detected!")
    if(any(p3+p1>(1 + eps)*factor_percent)) stop("Specified probabilities do not sum to <=1!")
  }

  else {
    if(any(p3 < 0)||any(p2 < 0)||any(p1 < 0)) stop("Negative probabilities detected!")
    if(any((1-eps)*factor_percent>p3+p2+p1|p3+p2+p1>(1 + eps)*factor_percent)) stop("Probabilities do not sum to 1!")
  }

  stopifnot(length(obs) == length(p1),
            length(p3) == length(p1),
            (is.null(p2) || length(p2) == length(p1)))

  n_bins = n*(n+1)/2 #= n_points (prev)
  n_obs = length(obs)

  assign_bin = function(p1,p3) { #bins ordered by p3,-p1 (ascending)
    p3_bin = floor((n-1)*p3+0.5) #rounding up (on border)
    p1_bin = ceiling((n-1)*p1-0.5) #rounding down (on border)
    p2_bin = n-1-p3_bin-p1_bin
    bin = (n*(p2_bin + 1)) - (p2_bin^2+p2_bin)/2 - p1_bin #=n_bin (prev)
    return(bin)
  }

  p3 = p3/factor_percent
  p1 = p1/factor_percent

  bin = mapply(assign_bin,p1,p3)

  data = data.frame(p3,p1,obs,bin)

  out = list(n = n,
             n_bins = n_bins,
             n_obs = n_obs,
             freq = rep(0,n_bins),
             cond_rel_freq_1 = rep(NA,n_bins),
             cond_rel_freq_3 = rep(NA,n_bins),
             cond_p3_ave = rep(NA,n_bins),
             cond_p1_ave = rep(NA,n_bins))

  cond_rel_freq = prop.table(table(data[,3:4]),2)
  cond_p_ave = aggregate(data[,1:2],list(data[,4]),mean)

  bins = cond_p_ave[,1]

  out$freq[bins] = margin.table(table(data[,4]),1)
  out$cond_rel_freq_1[bins] = cond_rel_freq[1,]
  out$cond_rel_freq_3[bins] = cond_rel_freq[3,]
  out$cond_p3_ave[bins] = cond_p_ave[,2]
  out$cond_p1_ave[bins] = cond_p_ave[,3]

  class(out) = append(class(out),"calibration_simplex")
  return(out)
}
