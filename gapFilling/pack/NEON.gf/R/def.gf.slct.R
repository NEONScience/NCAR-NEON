##############################################################################################
#' @title Choose the best predictions among gap-filling methods

#' @author 
#' Cove Sturtevant

#' @description 
#' Selected the best gap-filling prediction for each gap based on uncertaintay estimates provided for each 

#' @param data A vector of data, with gaps
#' @param predGf Data frame of predictions for the timeseries in \code{data}, each column from a different method 
#' @param ucrtGf Numeric data frame of uncertainty estimates, the same size as \code{predGf} and each column named 
#' the same as in \code{predGf}, holding the uncertainty estimate for each prediction. Obviously, these should be
#' comparable among columns/methods. Recommend the half-width of the 95% prediction interval. 

#' 
#' @references None

#' @return A data frame with the variables:\cr
#' \code{data} The gap-filled dataset
#' \code{gap} Logical. TRUE for original gap indices (whether or not there was a value available for filling the gap)
#' \code{pred} The best prediction (with the lowest uncertainty) for every data value. Will match the returned value in \code{data} for original gap indices.
#' \code{ucrt} The uncertainty of the best prediction corresponding to the values in \code{pred}
#' \code{varSlct} The variable/method in \code{predGf} chosen to populate the corresponding value in \code{pred} 


#' @keywords Currently None.

#' @examples 
#' data <- c(1.1,2.0,NA,3.9,NA,6.2)
#' predGf <- data.frame(method1=c(1,2,3,4,NA,6),
#'                      method2=c(0.89,2,3.3,4.4,4.8,6.2))
#' ucrtGf <- data.frame(method1=c(0.05,0.1,0.04,0.06,NA,0.12),
#'                      method2=c(0.1,0.18,0.19,0.15,0.13,0.12))
#' def.gf.slct(data=data,predGf=predGf,ucrtGf=ucrtGf)


#' @export

# changelog and author contributions / copyrights
#   Cove Sturtevant (2021-01-26)
#     original creation 
##############################################################################################
def.gf.slct <- function(data,
                        predGf,
                        ucrtGf
){
  
  # Error checking
  if(!base::is.vector(data) && !("list" %in% base::class(data))){
    stop('data must be a vector')
  }
  if(!base::is.data.frame(predGf)){
    stop('predGf must be a data frame')
  }
  if(!base::is.data.frame(ucrtGf)){
    stop('ucrtGf must be a data frame')
  }
  nameVarPred <- base::names(predGf) # columns Names of predictions
  nameVarUcrt <- base::names(predGf) # column Names of uncertainties (should be same as those of predictions)
  numVarGf <- length(nameVarPred) 
  if(!base::all(nameVarPred %in% nameVarUcrt) && numVarGf != length(nameVarUcrt)){
    stop('Columns names for predGf must exactly match those of ucrtGf')
  }

  # Take inventory of the gaps
  gap <- base::is.na(data)
  
  # Find the prediction with the lowest uncertainty for each value
  setMin <- Rfast::rowMins(base::as.matrix(ucrtGf),value=FALSE)
  varSlct <- nameVarPred[setMin] 
  pred <- base::sapply(base::seq.int(length(setMin)),FUN=function(idx){predGf[idx,setMin[idx]]})
  ucrt <- base::sapply(base::seq.int(length(setMin)),FUN=function(idx){ucrtGf[idx,setMin[idx]]})
  
  # Fill gaps
  data[gap] <- pred[gap]
  
  # Account for all uncertainties NA (thus no proper selection of the lowest uncertainty)
  setNa <- base::is.na(ucrt)
  data[gap & setNa] <- NA
  pred[setNa] <- NA
  varSlct[setNa] <- NA
  
  # Compile the output
  rpt <- base::data.frame(data=data,
                          gap=gap,
                          pred=pred,
                          ucrt=ucrt,
                          varSlct=varSlct,
                          stringsAsFactors = FALSE)
  return(rpt)
  
}
