#' Calculate recommended P dose for grassland in the Netherlands
#' 
#' This function calculates the recommended P dose (kg P2O5 / ha) given P-status of the soil.
#' @param B_LU_CAT (integer) The crop category. Options: arable, grassland, nature
#' @param B_SOILTYPE_USDA (character) The  type of soil. Options: clay, silty clay, loam, sand
#' @param A_P_AL (numeric) The P-content, measured via ammonium lactate extraction (mg P2O5 / 100g soil)
#' @param A_P_CC (numeric) The P-content, measured via calcium chloride extraction (mg P/ kg soil)
#' 
#' @import data.table
#' 
#' @references none yet
#'
#' @export
rfp_p_gld <- function(B_LU_CAT,B_SOILTYPE_USDA,A_P_AL,A_P_CC){
  
  # add visible bindings 
  id = NULL
  
  # number of soils for wich a recommendation is asked
  arg.length <- max(length(B_LU_CAT), length(B_SOILTYPE_USDA), length(A_P_AL),length(A_P_CC))
  
  # make local copy of function input into data.table
  dt <- data.table(id = 1:arg.length,
                   B_LU_CAT = B_LU_CAT, 
                   B_SOILTYPE_USDA = B_SOILTYPE_USDA,
                   A_P_AL = A_P_AL,
                   A_P_CC = A_P_CC,
                   value = NA_real_)
  
  # add a fake P dose
  dt[A_P_AL <= 16, value := 250]
  dt[A_P_AL > 16 & A_P_AL <= 45, value := 125]
  dt[A_P_AL > 45, value := 0]
  
  # add other categories than grassland to a zero P dose
  dt[B_LU_CAT != 'grassland', value := 0]
  
  # ensure right order
  setorder(dt,id)
  
  # extract output variable
  value <- dt[,value]
  
  # return value
  return(value)
  
}