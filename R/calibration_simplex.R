#' @title Calibration Simplex
#' @aliases CalSim
#'
#' @description Generates an object of class \code{calibration_simplex} which can be used to assess the calibration
#' of ternary probability forecasts. The Calibration Simplex can be seen as generalization of the reliability diagram
#' for binary probability forecasts. For details on the interpretation of the calibration simplex cf. Wilks, 2013. Be
#' aware that some minor changes have been made compared to the calibration simplex as suggested by Wilks, 2013 (cf. note below).
#'
#' @param n A natural number.
#' @param p_a A vector containing the forecasted probabilities for the above-normal (3) category.
#' @param p_n A vector containing the forecasted probabilities for the near-normal (2) category.
#' @param p_b A vector containing the forecasted probabilities for the below-normal (1) category.
#' @param obs A vector containing the observed outcomes (Categories are encoded as 1 (below-normal), 2 (near-normal) and 3 (above-normal)).
#' @param percentagewise Logical, specifying whether probabilities are percentagewise (summing to 100) or not (summing to 1).
#'
#' @rdname calibration_simplex
#' @export
#'
#' @details Only two of the three forecast probability vectors (\code{p_a}, \code{p_b} and \code{p_n}) need to be specified.
#'
#' @examples
#' #install.packages("scoring") #if this package is not installed already
#' data("WeatherProbs", package = "scoring")
#'
#' #Calibration Simplex for Temprature Forecasts
#' cst = calibration_simplex(p_a = WeatherProbs$tabv,
#'                           p_b = WeatherProbs$tblw,
#'                           obs=WeatherProbs$tcat)
#' plot(cst)
#' plot(cst,error_scale=1)
#'
#' #Calibration Simplex for Precipitation Forecasts
#' csp = calibration_simplex(n=19,
#'                           p_a = WeatherProbs$pabv,
#'                           p_b = WeatherProbs$pblw,
#'                           obs=WeatherProbs$pcat)
#' plot(csp)
#' plot(csp,error_scale=1)
#'
#' #Both forecasts are very(!) underconfident. This seems like an unrealistic example!
#'
calibration_simplex = function(n,p_a,p_n,p_b,obs,percentagewise){
  UseMethod("calibration_simplex")
}
