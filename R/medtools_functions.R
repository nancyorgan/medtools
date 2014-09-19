#' Mean Arterial Pressure (MAP) calculator
#' @param systolic A single or vector of systolic blood pressure(s). Defaults to 120.
#' @param diastolic A single or vector of diastolic blood pressure(s). Defaults to 80.
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
#' @param volume Volume given. Defaults to 10 mL
#' @param conc Concentration of epinephrine or norepinephrine. Defaults to 32 mcg/mL
#' @param kg Patient weight in kilograms. Defaults to 50 kg
#' @keywords CV SOFA
#' @export
#' @examples 
#' cvsofa()
#' cvsofa(volume = 11.5, conc = 64, kg = 104)

cvsofa = function(volume = 10, conc = 32, kg = 50){
  volume/60*conc/kg
}

#'O2Sat/FiO2 ratio calculator
#'@param o2sat Oxygen Saturation. Defaults to 90%
#'@param fio2 Fraction of Inspired Oxygen. Defaults to 0.40
#'@param liter Liters of Oxygen. Defaults to 8
#'@export
#'@examples
#'SF.ratio()
#'[1] 225
#'
#'SF.ratio(o2sat = 70, liter = 9)
SF.ratio = function(o2sat = 90, fio2 = 0.40, liter = 8){
  ifelse(missing(liter), o2sat/fio2,
  ifelse(missing(fio2), o2sat/(0.21 + liter*.03), 
  ifelse(missing(o2sat, "Missing o2sat",
         o2sat/fio2))))
}


#BMI.calc = function(lbs = 150, inch = 68, kg, m){
#  #ifelse(missing(lbs)  & missing(inch) & missing(kg), "Missing weight in kg",
#  #ifelse(missing(lbs)  & missing(inch) & missing(m),  "Missing height in m",
#  #ifelse(missing(lbs)  & missing(kg)   & missing(m),  "Missing weight in lbs",
#  #ifelse(missing(inch) & missing(kg)   & missing(m), "Missing height in inch",
#  ifelse(missing(kg), (lbs/inch^2)*703,
#  ifelse(missing(lbs), kg/(m^2),
#  ifelse(missing(m), kg/((inch*39.37)^2)),
#  ifelse(missing(inch), lbs/((m*0.0254)^2)),
#  ifelse(missing(lbs) & missing(m), kg/((inch*39.37)^2),
#  ifelse(missing(kg) & missing(inch), lbs/((m*0.0254)^2)),
#  "Not enough information provided" )))
#}
#BMI.calc(kg = 55)

#BMI.calc = function(height, weight, units){
#  ifelse(missing(lbs) & missing(kg), "Please provide a weight",
#         ifelse(missing(inch) & missing(m), "Please provide a height",
#                if(missing(lbs)){
#                  
#                }))
#  


