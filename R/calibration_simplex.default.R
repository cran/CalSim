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
#'
#' @importFrom stats aggregate

calibration_simplex.default = function(n = 10,
                                       p_a = NULL,
                                       p_n = NULL,
                                       p_b = NULL,
                                       obs = NULL,
                                       percentagewise = FALSE) {
  factor_percent = if(percentagewise) 100 else 1 #=div (prev)

  if(is.null(obs)) stop("Observations are missing!")
  stopifnot(all(obs %in% c(1,2,3)))

  eps = 0.01

  if(is.null(p_a)) {
    if(is.null(p_b)||is.null(p_n)) stop("Probability vectors are missing!")
    if(any(p_n < 0)||any(p_b < 0)) stop("Negative probabilities detected!")
    if(any(p_n+p_b>(1 + eps)*factor_percent)) stop("Specified probabilities do not sum to <=1!")
    p_a = factor_percent-p_b-p_n
  }

  else if(is.null(p_b)) {
    if(is.null(p_a)||is.null(p_n)) stop("Probability vectors are missing!")
    if(any(p_n < 0)||any(p_a < 0)) stop("Negative probabilities detected!")
    if(any(p_a+p_n>(1 + eps)*factor_percent)) stop("Specified probabilities do not sum to <=1!")
    p_b = factor_percent-p_a-p_n
  }

  else if(is.null(p_n)) {
    if(any(p_a < 0)||any(p_b < 0)) stop("Negative probabilities detected!")
    if(any(p_a+p_b>(1 + eps)*factor_percent)) stop("Specified probabilities do not sum to <=1!")
  }

  else {
    if(any(p_a < 0)||any(p_n < 0)||any(p_b < 0)) stop("Negative probabilities detected!")
    if(any((1-eps)*factor_percent>p_a+p_n+p_b|p_a+p_n+p_b>(1 + eps)*factor_percent)) stop("Probabilities do not sum to 1!")
  }

  stopifnot(length(obs) == length(p_b) &&
            length(p_a) == length(p_b) &&
            (is.null(p_n) || length(p_n) == length(p_b)))

  n_bins = n*(n+1)/2 #= n_points (prev)
  n_obs = length(obs)

  assign_bin = function(p_a,p_b) { #bins ordered by p_a,-p_b (ascending)
    pa_bin = floor((n-1)*p_a+0.5) #rounding up (on border)
    pb_bin = ceiling((n-1)*p_b-0.5) #rounding down (on border)
    pn_bin = n-1-pa_bin-pb_bin
    bin = (n*(pn_bin + 1)) - (pn_bin^2+pn_bin)/2 - pb_bin #=n_bin (prev)
    return(bin)
  }

  p_a = p_a/factor_percent
  p_b = p_b/factor_percent

  bin = mapply(assign_bin,p_a,p_b)

  data = data.frame(p_a,p_b,obs,bin)

  out = list(n = n,
             n_bins = n_bins,
             n_obs = n_obs,
             freq = rep(0,n_bins),
             cond_rel_freq_a = rep(NA,n_bins),
             cond_rel_freq_b = rep(NA,n_bins),
             cond_p_a_ave = rep(NA,n_bins),
             cond_p_b_ave = rep(NA,n_bins))

  cond_rel_freq = prop.table(table(data[,3:4]),2)
  cond_p_ave = aggregate(data[,1:2],list(data[,4]),mean)

  bins = cond_p_ave[,1]

  out$freq[bins] = margin.table(table(data[,4]),1)
  out$cond_rel_freq_a[bins] = cond_rel_freq[3,]
  out$cond_rel_freq_b[bins] = cond_rel_freq[1,]
  out$cond_p_a_ave[bins] = cond_p_ave[,2]
  out$cond_p_b_ave[bins] = cond_p_ave[,3]

  class(out) = append(class(out),"calibration_simplex")
  return(out)
}
