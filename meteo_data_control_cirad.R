#' Quick check up of weather data
#'
#' This function scan weather file and highlights possible errors, Temp (°C), rain (mm), rad (MJ/m²/day), RH (%), wind (km/day)
#' Units convertion factors are available here : http://www.fao.org/3/x0490e/x0490e0i.htm
#' @param dt A dataset of weather data; The columns should be labelled as follow: "date","TMIN","TMAX","RAIN","RAD","WIND","RH"
#' @keywords weather data
#' @export
#' @examples
#' meteo_data_control_cirad(dt)

meteo_data_control_cirad <- function(dt){

  if("RH" %in% colnames(dt)) {
  RHchange <- rep(median(dt$RH)<1,nrow(dt))
  dt$RH <- ifelse(RHchange,dt$RH*100,dt$RH) }# control unit
  if("RAD" %in% colnames(dt)) {
  RAD_change <- rep(median(dt$RAD)>50,nrow(dt))
  dt$RAD <- ifelse(RAD_change,dt$RAD*0.0864,dt$RAD)} # if unit of radiation is W/m², it needs to be converted to MJ/m²
  if("WIND" %in% colnames(dt)) {
  WIND_change <- rep(median(dt$WIND)<5,nrow(dt))
  dt$WIND <- ifelse(WIND_change,dt$WIND,dt$WIND*0.01157)} # if unit of wind is km/d, it needs to be converted to m/s

  for(i in 1:nrow(dt)){
# errors on dates
    dt$date_error[i]<- dplyr::case_when(
          dt$date[i+1]-dt$date[i]>1 ~ "next day(s) missing",
          dt$date[i+1]-dt$date[i]!=1 ~ "check date",
          is.na(dt$date[i]) ~ "No value",
          TRUE ~"")
# errors on temperatures
    dt$temp_error[i]<- dplyr::case_when(
          is.na(dt$TMAX[i]) ~ "Tmax missing",
          dt$TMIN[i]>=dt$TMAX[i] ~ "Tmin >=Tmax",
          is.na(dt$TMIN[i]) ~ "Tmin missing",
          dt$TMIN[i]<5 ~"Low value of Tmin",
          dt$TMIN[i]>35 ~"High value of Tmin",
          dt$TMAX[i]<15 ~"Low value of Tmax",
          dt$TMAX[i]>45 ~"High value of Tmax",
          dt$TMIN[i+1]==dt$TMIN[i]~"Duplicate Tmin",
          dt$TMAX[i+1]==dt$TMAX[i]~"Duplicate Tmax",
          dt$TMIN[i]<dt$TMAX[i]~"")
# errors on rainfall
    dt$rainfall_error[i]<- dplyr::case_when(
      dt$RAIN[i]>60~"Heavy rain",
      dt$RAIN[i]+dt$RAIN[i+1]<0.5& dt$RAIN[i]+dt$RAIN[i+1]>0~ "Bug",
      dt$RAIN[i+1]==dt$RAIN[i]&dt$RAIN[i]!=0~"Duplicate rainfall",
      TRUE ~ "")
# errors on radiation
    dt$radiation_error[i]<- dplyr::case_when(
      dt$RAD[i]<0~ "Negative radiation",
      dt$RAD[i]>25~"High radiation",
      dt$RAD[i]<5~"Low radiation",
      dt$RAD[i+1]==dt$RAD[i]~"Duplicate radiation",
      TRUE ~ "")
# errors on relative humidity
    dt$RH_error[i]<- dplyr::case_when(
      dt$RH[i]<0|dt$RH[i]>100 ~"out of boundaries",
      dt$RH[i]>95~"High relative humidity",
      dt$RH[i]<5~"Low relative humidity",
      dt$RH[i+1]==dt$RH[i]~"Duplicate RH",
      TRUE ~ "")
# errors on wind speed
    dt$WIND_error[i]<- dplyr::case_when(
      dt$WIND[i]<0 ~"out of boundaries",
      dt$WIND[i]>28~"High wind",
      dt$WIND[i+1]==dt$WIND[i]~"Duplicate wind speed",
      TRUE ~ "")

  }
  dt_reviewed <- dt
  return(dt_reviewed)
}
