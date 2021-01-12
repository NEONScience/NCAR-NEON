##############################################################################################
#' @title Gap-fill with replicate sensor

#' @author 
#' Cove Sturtevant

#' @description 
#' Gap-fill meteorological data with data from a replicate sensor, using a time and/or range-windowed regression approach

#' @param dataGf Numeric data frame including all variables needed for gap-filling (target sensor data and replicate sensor data).
#' @param meas Logical vector same length as nrow(dataGf), where TRUE indicates originally-measured values of the gap-filled variable. 
#' Defaults to TRUE for non-NA indices of the gap-filled variable in dataGf.
#' @param NameVarGf Character string. The name of the variable to gap-fill
#' @param NameVarRep Character vector. One or more strings indicating the names of the variables that represent replicate sensor measurements 
#' of the gap-filled variable. 
#' @param Rbst Logical. Use robust regression? TRUE for robust. FALSE (default) for ordinary least squares.
#' @param Wndw Integer value. Window [+- number of points] around each missing value for computing the regression with replicate sensor data. 
#' If NULL, no windowing is applied. Defaults to NULL. 
#' @param Rng Numeric value. Range [+- value] around the replicate value associated with each missing value to constrain data used in the 
#' regression. If NULL, all available data (within Wndw) is used. Defaults to NULL. Wndw and Range are additive constraints.
#' @param NumSampMin Integer value. The minimum number of samples required to perform the regression with the replicate sensor. Defaults to 5.
#' @param RtioRngRepMax Numeric value [fraction]. The maximum fraction of the range of replicate sensor data (used in the regression) 
#' that the replicate sensor value at the target gap may exceed be in order to fill with the predicted value. For example, if
#' the replicate sensor data used in the regression ranged from 0-10, and RtioRngRepMax was 0.1, a predicted target sensor value 
#' would only be accepted if the replicate sensor value at the gap was no less than -1 and no greater than 11 (since 10% 
#' of 10 is 1). Can be negative (for a range narrower than the data range). If NULL, this condition is not applied. Defaults to NULL.
#' @param RsqMin Numeric value. The minimum coefficient of determination (r-squared) required for a regression to be 
#' considered for use to gap-fill missing target values. If NULL, this condition is not applied. Defaults to NULL.
#' @param LvlPred Numeric value between 0 and 1 (non-inclusive). Tolerance/confidence level of the prediction (from which the prediction 
#' uncertainty interval is calculated). Defaults to .95.
#' 
#' @references None

#' @return A data frame with the variables:\cr
#' \code{pred} The gap-filled dataset
#' \code{lwr} The lower prediction interval for each gap-filled value. NA for non-filled values
#' \code{upr} The upper prediction interval for each gap-filled value, NA for non-filled values
#' \code{sdMean} The standard deviation of the predicted mean y at x, NA for non-filled values
#' \code{sdObs} The standard deviation of the y residuals from the regression line, NA for non-filled values
#' \code{sd} The standard deviation of a single (predicted) y observation at x, NA for non-filled values. Scale this with the t-distribution to achieve the upper and lower prediction interval
#' \code{df} The degrees of freedom of the regression used to fill each gap (n-2), NA for non-filled values
#' \code{rsqAdj} The adjusted coefficient of determination (r-squared) of the regression used to fill each gap, NA for non-filled values
#' \code{varFill} The replicate sensor regression used to fill each gap. NA for non-filled values.

#' @keywords Currently None.

#' @examples Currently None

#' @export

# changelog and author contributions / copyrights
#   Cove Sturtevant (2018-08-07)
#     original creation 
#   Cove sturtevant (2018-10-16)
#     Added options for robust regression and constraining the regression data to a range
#        around the replicate value at each gap
#     Added skipping of the for-loop when both Wndw and Rng are NULL
#   Cove Sturtevant (2020-12-22)
#     Adjusted uncertainty outputs to be able to recreate the confidence interval around predicted y values 
#     Output the variable of the chosen replicate regression for each filled value
##############################################################################################
def.gf.rep <- function(dataGf,
                       meas = !base::is.na(dataGf[[NameVarGf]]),
                       NameVarGf,
                       NameVarRep,
                       Rbst=FALSE,
                       Wndw=NULL,
                       Rng=NULL,
                       NumSampMin=5,
                       RtioRngRepMax=NULL,
                       RsqMin=NULL,
                       LvlPred=0.95
){
  
  # Error checking
  if(!base::is.data.frame(dataGf)){
    stop('dataGf must be a data frame')
  }
  if(sum((c(NameVarGf,NameVarRep) %in% base::names(dataGf))) != (1+base::length(NameVarRep))){
    stop(base::paste0('Cannot find one or more of the variables ', base::paste0(NameVarGf,' ',base::paste0(NameVarRep,collapse=' ')), ' in dataGf. Check input data and parameters.'))
  }
  if(LvlPred <= 0 | LvlPred >= 1){
    stop('LvlPred must be between 0 and 1 (non-inclusive)')
  }
  
  # Preliminary info
  numData <- base::nrow(dataGf)
  
  # Pull out the gap-filled variable, and measured values of the gap-filled variable
  varGf <- dataGf[[NameVarGf]]
  varGfMeas <- varGf
  varGfMeas[!meas] <- NA

  # Initialize the output
  dmmy <- base::rep(NA,times=numData)
  rpt <- base::data.frame(pred=varGf,lwr=dmmy,upr=dmmy,sdMean=dmmy,sdObs=dmmy,sd=dmmy,df=dmmy,rsqAdj=dmmy,varFill=dmmy)
  nameColRpt <- names(rpt)
  
  # Window the data around each value to gap-fill (no point in evaluating the points which have no replicate sensor values)
  setGf <- base::which(base::is.na(varGf) & base::apply(X=!base::is.na(base::as.matrix(dataGf[,NameVarRep])),MARGIN=1,FUN=base::sum)>0)
  for(idxGf in setGf){
    
    # If both Wndw and Rng are NULL, we are using all the data for every regression, so let's do all gaps at once
    if(base::is.null(Wndw) && base::is.null(Rng)){
      idxGf <- setGf
    }
    
    # Create window around this value
    if(base::is.null(Wndw)){
      idxBgn <- 1
      idxEnd <- numData
    } else {
      idxBgn <- idxGf - Wndw
      idxEnd <- idxGf + Wndw
    }
    
    # If the window falls outside the data range, truncate it
    if(idxBgn < 1){
      idxBgn <- 1
    } 
    if(idxEnd > numData){
      idxEnd <- numData
    }
    
    
    # Develop a linear regression between the replicate sensor and the target sensor data
    lm <- base::lapply(NameVarRep,FUN=function(nameVar){
      # Initialize
      lm <- NULL 
      
      # Pull the data
      idepGf <- dataGf[[nameVar]][idxGf] # replicate sensor value at the gap
      dataRep <- data.frame(idep=dataGf[[nameVar]][idxBgn:idxEnd],dep=varGfMeas[idxBgn:idxEnd])
      dataRep <- dataRep[!base::is.na(dataRep$idep) & !base::is.na(dataRep$dep),] # Constraint to non-NA rows

      # Constrain regression data to a range around the replicate sensor value of the gap
      if(!base::is.null(Rng)){
        dataRep <- dataRep[(dataRep$idep <= idepGf + Rng) & (dataRep$idep >= idepGf - Rng),]
      }

      # Check for minimum sample size
      if(base::nrow(dataRep) < NumSampMin){
        return(lm)
      }
      
      # Construct linear model
      if(Rbst){
        # Robust regression
        lm <- base::try(robustbase::lmrob(dep ~ idep,data=dataRep),silent=TRUE)
      } else {
        # Ordinary least squares regression
        lm <- base::try(stats::lm(dep ~ idep,data=dataRep),silent=TRUE)
      }
      
      if(base::class(lm) != 'try-error'){

        # Predict the missing value with a prediction interval (default 5%). Nice thing about the prediction interval is that 
        # it accounts for the number of samples used in the regression OLS: Y+-t*se, since t depends on the df of the residuals
        predLm <- stats::predict(object=lm,newdata=data.frame(idep=idepGf),interval='prediction',level=LvlPred,se.fit=TRUE)
        lm$pred <- base::as.data.frame(predLm$fit) # prediction + desired prediction interval
        
      
        # Add the standard error (deviation) of the predicted mean and value, and the degrees of freedom
        # Moore, McCabe, Craig - Introduction to the practice of statistics, 6th Ed., pg. 586-589 - don't need! Already computed
        #Lower and upper bounds of the confidence interval of a single predicted value is constructed by prediction +- (t critical value)*sqrt(predLm$se.fit^2+predLm$residual.scale^2) - Devore & Peck pg. 587
        lm$pred$sdMean <- predLm$se.fit # Standard deviation of the predicted mean y value at x (which is what the gap is filled with)
        lm$pred$sdObs <- predLm$residual.scale # Standard deviation of the y residuals from the regression line
        lm$pred$sd <- sqrt(predLm$se.fit^2+predLm$residual.scale^2) # Standard deviation of a single y observation made at the x value 
        lm$pred$df <- lm$df.residual # Degrees of freedom
        
        # # Add in R2 of the regression
        # meanDep <- base::mean(dataRep$dep)
        # lm$pred$rsq <- 1-base::sum(lm$residuals^2)/base::sum((dataRep$dep-meanDep)^2)
        smmyLm <- base::summary(lm)
        lm$pred$rsqAdj <- smmyLm$adj.r.squared
        
        # Does the R-squared pass our threshold for acceptance?
        if(!base::is.null(RsqMin) && lm$pred$rsq < RsqMin){
          lm <- NULL
        }
        
        # Remove predictions for which the independent data fell outside the appropriate range
        maxIdep <- base::max(dataRep$idep)
        minIdep <- base::min(dataRep$idep)
        rngIdep <- maxIdep-minIdep
        if(!base::is.null(RtioRngRepMax)) {
          setRmv <- (idepGf < (minIdep-rngIdep*RtioRngRepMax)) | (idepGf > (maxIdep+rngIdep*RtioRngRepMax))
          setRmv[is.na(setRmv)] <- TRUE
          lm$pred[setRmv,] <- NA
          
          if(base::sum(setRmv) == base::length(idxGf)){
            lm <- NULL
          }
        } 
        
      } else {
        lm <- NULL
      }
      return(lm)
    })
    base::names(lm) <- NameVarRep
    
    # Make sure we have at least one valid linear model
    if(base::sum(!base::unlist(base::lapply(lm,base::is.null))) == 0){next}
    
    # Choose whichever confidence interval for the prediction is smallest for each gap, and fill it
    ucrtPred <- base::do.call(base::cbind,base::lapply(lm,FUN=function(lmRep){lmRep$pred$upr-lmRep$pred$lwr}))
    idxColMinUcrt <- Rfast::rowMins(ucrtPred,value=FALSE)
    for(idxLm in base::unique(idxColMinUcrt)){
      idxRow <- idxColMinUcrt==idxLm
      # Fill the gap 
      rpt[idxGf[idxRow],setdiff(nameColRpt,'varFill')] <- lm[[idxLm]]$pred[idxRow,]
      rpt[idxGf[idxRow],'varFill'] <- NameVarRep[idxLm]
      
    }

    # If we filled all data at once, quit the loop
    if(base::is.null(Wndw) && base::is.null(Rng)){
      break
    }
  }
 
  return(rpt) 
}
