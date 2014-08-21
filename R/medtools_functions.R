#' Mean Arterial Pressure (MAP) calculator
#' @param systolic A single or vector of systolic blood pressure(s) Defaults to 120.
#' @param diastolic A single or vector of diastolic blood pressure(s) Defaults to 80.
#' @keywords blood pressure
#' @export
#' @examples 
#' mapcalc(sysolic = c(120, 100, 90, 114, 89),
#'         diastolic = c(80, 52,67,98,45))
#' mapcalc()

mapcalc = function(systolic = 120, diastolic = 80){
  total = data.frame(systolic, diastolic, MAP = (2*diastolic + systolic)/3)
  total = total[order(total$MAP, decreasing = TRUE),]
  total
}

#' CV SOFA score calculator 
#' 
#' Calculate a Sequential Organ Failure Assessment score using volume of norepinephrine, drug concentration, and patient weight in kilograms
#' @param volume Volume given Defaults to 10 mL
#' @param conc Concentration of norepinephrine Defaults to 32 mcg/mL
#' @param kg Patient weight in kilograms Defaults to 50 kg
#' @keywords CV SOFA
#' @export
#' @examples 
#' cvsofa()
#' cvsofa(volume = 11.5, conc = 64, kg = 104)

cvsofa = function(volume = 10, conc = 32, kg = 50){
  volume/60*conc/kg
}


