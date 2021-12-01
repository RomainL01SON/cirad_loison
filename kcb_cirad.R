#' test
#'
#' This function calculates Kcb FAO (http://www.fao.org/3/x0490e0c.htm).
#' @param param A vector of length 7 with days to t1, t2, t3 and t4,values of Kcb ini, at plateau and at the end of cycle
#' @param x Day(s) after planting
#' @keywords plant transpiration
#' @export
#' @examples
#' kcb_cirad(c("cotton",25,75,95,110,0.15,1.15,0.5),100)

kcb_cirad <- function(param,x){
y <- dplyr::case_when(
    x <= param[1] ~ param[5],
    x <= param[2] ~ (param[6]-param[5])/(param[2]-param[1])*(x-param[1])+param[5],
    x <= param[3] ~ param[6],
    x <= param[4] ~ (param[7]-param[6])/(param[4]-param[3])*(x-param[3])+param[6],
    x>param[4] ~param[7])
  return(y)
}
