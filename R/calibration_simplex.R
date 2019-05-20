#' @title Calibration Simplex
#' @aliases CalSim
#'
#' @description Generates an object of class \code{calibration_simplex} which can be used to assess the calibration
#' of ternary probability forecasts. The Calibration Simplex can be seen as generalization of the reliability diagram
#' for binary probability forecasts. For details on the interpretation of the calibration simplex cf. Wilks, 2013. Be
#' aware that some minor changes have been made compared to the calibration simplex as suggested by Wilks, 2013 (cf. note below).
#'
#' @param n A natural number.
#' @param p1 A vector containing the forecasted probabilities for the first (1) category, e.g. below-normal.
#' @param p2 A vector containing the forecasted probabilities for the second (2) category, e.g. near-normal.
#' @param p3 A vector containing the forecasted probabilities for the third (3) category, e.g. above-normal.
#' @param obs A vector containing the observed outcomes (Categories are encoded as 1 (e.g. below-normal), 2 (e.g. near-normal) and 3 (e.g. above-normal)).
#' @param percentagewise Logical, specifying whether probabilities are percentagewise (summing to 100) or not (summing to 1).
#' @param p_a (deprecated) Use p3 instead! A vector containing the forecasted probabilities for the above-normal (3) category.
#' @param p_n (deprecated) Use p2 instead! A vector containing the forecasted probabilities for the near-normal (2) category.
#' @param p_b (deprecated) Use p1 instead! A vector containing the forecasted probabilities for the below-normal (1) category.
#'
#' @rdname calibration_simplex
#' @export
#'
#' @details Only two of the three forecast probability vectors (\code{p1}, \code{p2} and \code{p3}) need to be specified.
#'
#' @examples
#' attach(ternary_forecast_example)   #see also documentation of sample data
#' # ?ternary_forecast_example
#'
#' # Calibrated forecast sample
#' calsim0 = calibration_simplex(p1 = p1, p3 = p3, obs = obs0)
#' plot(calsim0)
#'
#' #Overconfident forecast sample
#' calsim1 = calibration_simplex(p1 = p1, p3 = p3, obs = obs1)
#' plot(calsim1)
#'
#' #Underconfident forecast sample
#' calsim2 = calibration_simplex(p1 = p1, p3 = p3, obs = obs2)
#' plot(calsim2)
#'
#' #Unconditionally biased forecast sample
#' calsim3 = calibration_simplex(p1 = p1, p3 = p3, obs = obs3)
#' plot(calsim3)
#'
#' #Using a different number of bins
#' calsim = calibration_simplex(n=4, p1 = p1, p3 = p3, obs = obs3)
#' plot(calsim)
#'
#' calsim = calibration_simplex(n=13, p1 = p1, p3 = p3, obs = obs3)
#' plot(calsim)
#'
#' #Using some additional plotting parameters
#' plot(calsim,
#'      error_scale = 0.5,    #errors are less pronounced (smaller shifts)
#'      min_bin_freq = 100,   #dots are plotted only for bins,
#'                            #which contain at least 100 forecast-outcome pairs
#'      category_labels = c("below-normal","near-normal","above-normal"),
#'      main = "Sample calibration simplex")
#'
#' detach(ternary_forecast_example)


calibration_simplex = function(n,p1,p2,p3,obs,percentagewise,p_a,p_n,p_b){
  UseMethod("calibration_simplex")
}
