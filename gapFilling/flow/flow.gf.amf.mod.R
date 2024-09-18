##############################################################################################
#' @title Workflow: Test script to investigate improved framework for gap-filling meteo data at AmeriFlux, 
#' with modified reddyproc package
#' 
#' @author 
#' Cove Sturtevant \email{csturtevant@battelleecology.org}

#' @description 
#' Workflow. Test script to investigate improved framework for gap-filling meteo data at AmeriFlux. REddyProc package was 
#' modified to pull out the gap-filling functions into the main R environment. Note that plotting with plotly requires the 
#' package webshot to be installed, along with having run the command webshot::install_phantomjs()

#' @references 
#' AmeriFlux FP Standard formatting: \url{http://ameriflux.lbl.gov/data/aboutdata/data-variables/} \cr
#' 
#' @return TWB

#' @keywords L2, gap-filling, AmeriFlux

#' @examples TBD

#' @seealso TBD

# changelog and author contributions / copyrights
#   Cove Sturtevant (2017-03-29)
#     original creation
##############################################################################################
rm(list=ls())

try(detach('package:REddyProc',unload=TRUE),silent=TRUE)
library(REddyProc.gapFill)
library(plotly)
source('C:/Users/csturtevant/R/NCAR-NEON-covesturtevant./gapFilling/pack/NEON.gf/R/def.gf.rep.R')
source('C:/Users/csturtevant/R/NCAR-NEON-covesturtevant/gapFilling/pack/NEON.gf/R/def.gf.slct.R')

DirData <- 'N:/Science/FIUDATA/IPT_data/dynamic/GF_AMF/collaborationTestData'
Plot <- FALSE # plot the interim output as the code loops through scenarios?
PlotSave <- FALSE # Save the interim plots?
DirPlot <- base::paste0(DirData,'/plots/US-xUN_USTAR_',Sys.Date())
RptSave <- FALSE # Save output stats and options?
DirRpt <- base::paste0(DirData,'/rpt')

# Specify variable & gap-filling framework details
Opt <- base::list(
  #NameFileData = "US-xBR_HH_201611301900_202005311900.csv",
  NameFileData = "US-xUN_HH_201701311800_202012311800.csv",
  tz = 'EST',
  NameSite = 'US-xUN', # Site
  NameVarGfBase = "USTAR", # Base-name of gap-filled variable
  NameVarGf = "USTAR", # variable to gap-fill
  UnitVarGf = 'm s-1', # Units of gap-filled variable
  TimeBgn = as.POSIXct('2015-01-01',tz='EST'), # Start date of data to use
  TimeEnd = as.POSIXct('2021-01-01',tz='EST'), # End date of data to use
  # No specify gap-filling framework(s) - enter multiple frameworks to test in additional named lists (make sure list names 
  # are unique). Non-null entries are assumed to indicate performing that step
  NameVarRep = c("WS_1_5_1"), # replicates for the gap-filled variable. Enter NULL for none.
  NameVarSiml = NULL, # Theoretically similar measurements at same location (like PAR similar to incoming shortwave). Just one variable for now. Enter NULL for none.
  NameVarSpac = NULL, # same variable as gap-filled, but confounded by spatial variation (horizontal or vertical). Enter NULL for none.
  NameVarMds = c("SW_IN_1_1_1","VPD","TA_1_1_1"), # Input vars to MDS, e.g. SW_IN, VPD, TA. Enter NULL for none.
  TolVarMds = c(50,5,3) # Tolerance interval for MDS variables specified above, in same units as variable
) # End options


# Load in test dataset
print('Reading in dataset...')
data <- utils::read.csv(file=base::paste0(DirData,'/',Opt$NameFileData),header=TRUE,stringsAsFactors=FALSE)
nameVar <- base::names(data) # all variable names

# Ensure all data are numeric
chkNumc <- base::unlist(base::lapply(data,FUN=function(var){base::class(var) %in% c('numeric','integer')}))
if(sum(chkNumc) != base::length(chkNumc)){
  stop('Not all variables were loaded as numeric. Please check variable chkNumc to find which are not.')
}

# Turn missing data to NA
data[data==-9999] <- NA

# Interpret times
data$TIMESTAMP_START <- base::as.POSIXct(base::as.character(data$TIMESTAMP_START),format='%Y%m%d%H%M',tz=Opt$tz)
data$TIMESTAMP_END <- base::as.POSIXct(base::as.character(data$TIMESTAMP_END),format='%Y%m%d%H%M',tz=Opt$tz)

# Restrict data to start at 00:30 and end at 00:00
idxBgn <- which(data.table::hour(data$TIMESTAMP_START) == 0 & data.table::minute(data$TIMESTAMP_START) == 30)[1]
idxEnd <- utils::tail(which(data.table::hour(data$TIMESTAMP_START) == 0 & data.table::minute(data$TIMESTAMP_START) == 0),n=1)
data <- data[idxBgn:idxEnd,]
rownames(data) <- c()

# Restrict data to desired time range
data <- data[data$TIMESTAMP_START >= Opt$TimeBgn & data$TIMESTAMP_END < Opt$TimeEnd,]

# Work on this module if/when there is a DYNAMIC selection (rather than specified above) of variables that are replicates, spatial variates, etc.
# For now it is not used
if(FALSE) {
  # Find matches to the gap-filling variable (matching base name, with optional location indices)
  # Note, matching variables are assumed to consist of the base-name only or have the 3-component _H_V_R
  # location indexing attached to the base-name. Modify for aggregated variables later.
  mtch <- regexpr(pattern=nameVarGfBase,text=nameVar)
  setMtch <- base::which(mtch == 1 & base::attr(mtch,"match.length") == base::nchar(nameVarGfBase) &
                           ((base::nchar(nameVar)-base::nchar(nameVarGfBase)) %in% c(0,6)))
  nameVarMtch <- nameVar[setMtch] # variable matches
  
  # Error out if there are no matches
  if(base::length(setMtch) == 0){
    base::stop(base::paste0('Cannot find ',nameVarGfBase,' in the data file. Please check.'))
  }
  
  # If there is more than one match, pull the _1_1_1 location index
  if(base::length(nameVarMtch) == 1){
    nameVarGf <- nameVarMtch
  } else {
    nameVarGf <- nameVarMtch[base::grepl(pattern="_1_1_1",x=nameVarMtch)]
  }
}

######## Create gap scenarios ##########
# Follows the testing framework of Moffat et al. 2007
# 10 % artificial gaps, Gaps distributed randomly, Permute each gap scenario 10 times
# Gap-length scenarios
#    Very short gaps (VS) - single half-hours
#    Short gaps (S) - 8 consecutive half-hours
#    Medium gaps (M) - 64 consecutive half-hours
#    Long gaps (L) - 12 consecutive days (576 half hours)
#    Very long gaps (VL) - 30 consecutive days (1440 half hours)
#    Mixed scenario (X) - combination of other gap types (25% VS, 25% S, 25% M, 25% L)

data00 <- data # Save the original data. We are going to modify 'data' for each new scenario

# Initialize 
numPerm <- 10; # permutations for each gap scenario
numData <- length(data[[Opt$NameVarGf]])
setGap <- which(is.na(data[[Opt$NameVarGf]])) # gap indices in original dataset
setMeas <- which(!is.na(data[[Opt$NameVarGf]])) # availabile (non-NA) measurement indices in original dataset
numGf <- floor(length(setMeas)*0.1) # nominal number of artificial gaps. This is adjusted slightly below to create sequences of specific lengths
dmmyNa <- rep(NA,length=numPerm) # Dummy variable for initializing stats output
dmmyMeth <- data.frame(rep=dmmyNa,siml=dmmyNa,spac=dmmyNa,mdsMeth01=dmmyNa,mdsMeth02=dmmyNa,mdsMeth03=dmmyNa) # A data frame of numPerm rows and columns for each method
numGfMeth = dmmyMeth # Record the number of gap-filled points accomplished with each successive method in the framework
stat = data.frame(numGf=dmmyNa,rsq=dmmyNa,rmseAbs=dmmyNa,rmseRltv=dmmyNa,mae=dmmyNa,bias=dmmyNa,sd=dmmyNa,methRite=dmmyNa) # overall gap-filling performance statistics
statMeth = list(numGf=dmmyMeth,rsq=dmmyMeth,rmseAbs=dmmyMeth,rmseRltv=dmmyMeth,mae=dmmyMeth,bias=dmmyMeth,sd=dmmyMeth,df=dmmyMeth,ucrtMed=dmmyMeth,ucrtInPerc=dmmyMeth) # performance statistics for each method (regardless of whether values were gap-filled with the method)

# Initialize output report
rpt <- list(VS = list(nameGf = 'Very short gaps',
                      numValuSeq = 1, # Number of missing values in a single sequence
                      numSeq = max(floor(numGf/1),1), # Number of gap sequences
                      setGfBgn = as.data.frame(matrix(NA,nrow=max(floor(numGf/1),1),ncol=numPerm)), # initialize set of indices to gap-fill
                      numGfMeth=numGfMeth,
                      statFmwk = stat, # Stats for the entire framework (end result)
                      statMeth=statMeth), # Stats for each individual method, as if it were used to fill all gaps
            S = list(nameGf = 'Short gaps',
                     numValuSeq = 8, # Number of missing values in a single sequence
                     numSeq = max(floor(numGf/8),1), # Number of gap sequences
                     setGfBgn = as.data.frame(matrix(NA,nrow=max(floor(numGf/8),1),ncol=numPerm)), # initialize set of indices to gap-fill
                     numGfMeth=numGfMeth,
                     statFmwk = stat,
                     statMeth=statMeth),
            M = list(nameGf = 'Medium gaps',
                     numValuSeq = 64, # Number of missing values in a single sequence
                     numSeq = max(floor(numGf/64),1), # Number of gap sequences
                     setGfBgn = as.data.frame(matrix(NA,nrow=max(floor(numGf/64),1),ncol=numPerm)), # initialize set of indices to gap-fill
                     numGfMeth=numGfMeth,
                     statFmwk = stat,
                     statMeth=statMeth),
            L = list(nameGf = 'Long gaps',
                     numValuSeq = 12*48, # Number of missing values in a single sequence
                     numSeq = max(floor(numGf/(12*48)),1), # Number of gap sequences
                     setGfBgn = as.data.frame(matrix(NA,nrow=max(floor(numGf/(12*48)),1),ncol=numPerm)), # initialize set of indices to gap-fill
                     numGfMeth=numGfMeth,
                     statFmwk = stat,
                     statMeth=statMeth),
            VL = list(nameGf = 'Very long gaps',
                     numValuSeq = 30*48, # Number of missing values in a single sequence
                     numSeq = max(floor(numGf/(30*48)),1), # Number of gap sequences
                     setGfBgn = as.data.frame(matrix(NA,nrow=max(floor(numGf/(30*48)),1),ncol=numPerm)), # initialize set of indices to gap-fill
                     numGfMeth=numGfMeth,
                     statFmwk = stat,
                     statMeth=statMeth),
            X = list(nameGf = 'Mixed-length gaps',
                     numValuSeq = c(1,8,64,12*48), # Number of missing values in a single sequence
                     numSeq = c(max(floor(numGf*.25/1),1),max(floor(numGf*.25/8),1),max(floor(numGf*.25/64),1),max(1,floor(numGf*.25/(12*48)))), # Number of gap sequences
                     setGfBgn = list(as.data.frame(matrix(NA,nrow=max(floor(numGf*.25/1),1),ncol=numPerm)),
                                     as.data.frame(matrix(NA,nrow=max(floor(numGf*.25/8),1),ncol=numPerm)),
                                     as.data.frame(matrix(NA,nrow=max(floor(numGf*.25/64),1),ncol=numPerm)),
                                     as.data.frame(matrix(NA,nrow=max(1,floor(numGf*.25/(12*48))),ncol=numPerm))),
                     numGfMeth=numGfMeth,
                     statFmwk = stat,
                     statMeth=statMeth)
            )

# Find starting indices of continuous data sequences 
setReal <- which(!is.na(data[[Opt$NameVarGf]]))
seqReal <- data.frame(setBgn = c(setReal[1],setReal[which(diff(setReal) > 1)+1]), # Starting indices of continuous non-NA data sequences
                      setEnd = c(setReal[which(diff(setReal) > 1)],tail(setReal,1))) # Ending indices of continuous non-NA data sequences
seqReal$num <- seqReal$setEnd-seqReal$setBgn


# Get the starting sample indices for each gap scenario, except the mixed one. We'll do that after
print('Creating gap scenarios...')
nameTypeGf <- names(rpt)
for(idxTypeGf in nameTypeGf[nameTypeGf != 'X']){
  
  # If we started the artifical gap sequence at each index, how many non-NA values would we get?
  #numRealSeq <- unlist(lapply(X=1:(numData-rpt[[idxTypeGf]]$numValuSeq+1),FUN=function(idx){sum(!is.na(data[[Opt$NameVarGf]][idx:(idx+rpt[[idxTypeGf]]$numValuSeq)-1]))}))
  
  # Form the set of available starting indices to sample (those with a gap-free original sequence)
  #setRealBgnUse <- which(numRealSeq == rpt[[idxTypeGf]]$numValuSeq)
  
  # Which continous non-NA data sequences are long enough to support this gap scenario
  setSeqUse <- which(seqReal$num >= rpt[[idxTypeGf]]$numValuSeq)
  
  # Form the set of available starting indices to sample from
  setRealBgnUse <- unlist(lapply(X=setSeqUse,FUN=function(idxUse){seqReal$setBgn[idxUse]:(seqReal$setEnd[idxUse]-rpt[[idxTypeGf]]$numValuSeq+1)}))

  # If the total length of long-enough sequences to sample from is less than 2 times what we need, use the longest ones available 
  numUseMin <- 2*rpt[[idxTypeGf]]$numValuSeq*rpt[[idxTypeGf]]$numSeq # Minimum number of unique gap indices to use in the analysis
  if(sum(seqReal$num[setSeqUse]) < numUseMin){
    seqRealSort <- seqReal[base::order(seqReal$num,decreasing=TRUE),] # Sort the non-NA data sequences by descending length
    
    # Error check that we have enough data to proceed
    if(tail(cumsum(seqRealSort$num),1) < numUseMin){stop('Not enough non-NA data of the gap-filled variable to test all gap scenarios')}
    
    setSeqUse <- 1:which(cumsum(seqRealSort$num) >= numUseMin)[1] # indices of best available sequences to use
    
    setRealBgnUse <- unlist(lapply(X=setSeqUse,FUN=function(idxUse){seqRealSort$setBgn[idxUse]:max(seqRealSort$setBgn[idxUse],(seqRealSort$setEnd[idxUse]-rpt[[idxTypeGf]]$numValuSeq+1))}))
    setRealBgnUse <- sort(unique(setRealBgnUse))
  }
  
  # Loop around permutations for each gap scenario
  for(idxPerm in 1:numPerm){
    
    # Sample the beginning index of each gap sequence
    rpt[[idxTypeGf]]$setGfBgn[,idxPerm] <- sample(x=setRealBgnUse,size=rpt[[idxTypeGf]]$numSeq,replace = FALSE)

  } # End loop around permutations
} # End loop around gap scenarios

# Get the starting sample indices for the mixed gap scenario
idxTypeGf <- 'X'
# Loop around permutations 
for(idxPerm in 1:numPerm){
  
  for(idxTypeSamp in 1:length(rpt[[idxTypeGf]]$numSeq)){
    # Sample the starting indices of the existing gap scenarios - this may result in some overlap
    rpt[[idxTypeGf]]$setGfBgn[[idxTypeSamp]][,idxPerm] <- sample(x=rpt[[idxTypeSamp]]$setGfBgn[,idxPerm],size=rpt[[idxTypeGf]]$numSeq[idxTypeSamp],replace = FALSE)
    
  }
    
  
} # End loop around permutations


########## Perform Gap-filling ###########

# Plot the data involved 
nameVarFmwk <- base::unique(c(Opt$NameVarGf,Opt$NameVarRep,Opt$NameVarSiml,
                              Opt$NameVarSpac,Opt$NameVarMds))


plotVarFmwk <- list()
titl <- base::paste0('Data involved')
for(idxVar in base::seq.int(1,length(nameVarFmwk))){
  plotVarFmwk[[idxVar]] <- plotly::plot_ly(data=data00, x=~TIMESTAMP_START, y=data00[[nameVarFmwk[idxVar]]], type='scatter', 
                                           mode='lines', line = base::list(color = 'rgba(20,20,20,1)', width = 1),
                                           name=nameVarFmwk[idxVar]) %>%
    plotly::layout(margin = list(b = 50, t = 50, r=50),
                   title = titl,
                   xaxis = list(title = base::paste0(c(rep("\n&nbsp;", 3),
                                                       rep("&nbsp;", 20),
                                                       paste0("Date-Time [",base::attr(data00$TIMESTAMP_START,'tzone'),']'),
                                                       rep("&nbsp;", 20)),
                                                     collapse = ""),
                                nticks=6,
                                range = data00$TIMESTAMP_START[c(1,base::length(data00$TIMESTAMP_START))],
                                zeroline=FALSE
                   ),
                   yaxis = list(title = nameVarFmwk[idxVar],
                                zeroline=FALSE))
}

# Combine the plots into a single graphic
plotVarFmwkAll <- plotly::subplot(plotVarFmwk,nrows=length(plotVarFmwk),titleY=TRUE,shareX=TRUE)

# Save plot to file, or print to screen
if(PlotSave){
  plotly::export(plotVarFmwkAll,base::paste0(DirPlot,'/AllVars_',Opt$NameSite,'.pdf'))
} else {
  print(plotVarFmwkAll)
}

# Loop around gap scenarios
for(idxTypeGf in nameTypeGf){
  
  #if(idxTypeGf != 'VL'){next} # DELETE ME
  
  print(paste0('Running ',rpt[[idxTypeGf]]$nameGf,' scenario...'))
  
  # Loop around permutations for each gap scenario
  for(idxPerm in 1:numPerm){
    
    print(paste0('Running ',rpt[[idxTypeGf]]$nameGf,' scenario, permutation ',idxPerm,'...'))

    # Create the full sets of gap indices
    if(idxTypeGf == 'X'){
      # Mixed length scenario has a list of dataframes rather than a single data frame
      setGf <- lapply(1:length(rpt[[idxTypeGf]]$setGfBgn),FUN=function(idx){
        setGfBgn <- rpt[[idxTypeGf]]$setGfBgn[[idx]][,idxPerm]
        setGf <- matrix(rep(0:(rpt[[idxTypeGf]]$numValuSeq[idx]-1),times=length(setGfBgn)),nrow=length(setGfBgn),byrow=TRUE)+
          matrix(rep(rpt[[idxTypeGf]]$setGfBgn[[idx]][,idxPerm],times=rpt[[idxTypeGf]]$numValuSeq[idx]),ncol=rpt[[idxTypeGf]]$numValuSeq[idx],byrow=FALSE)
        setGf <- as.vector(matrix(setGf,ncol=nrow(setGf),byrow=TRUE)) # 1-D vector of all indices gap-filled
      })
      setGf <- unique(unlist(setGf))
    } else {
      setGfBgn <- rpt[[idxTypeGf]]$setGfBgn[,idxPerm]
      setGf <- matrix(rep(0:(rpt[[idxTypeGf]]$numValuSeq-1),times=length(setGfBgn)),nrow=length(setGfBgn),byrow=TRUE)+
        matrix(rep(rpt[[idxTypeGf]]$setGfBgn[,idxPerm],times=rpt[[idxTypeGf]]$numValuSeq),ncol=rpt[[idxTypeGf]]$numValuSeq,byrow=FALSE)
      setGf <- as.vector(matrix(setGf,ncol=nrow(setGf),byrow=TRUE)) # 1-D vector of all indices gap-filled
    }
    
    # Make sure setGf does not go beyond the data range
    setGf <- setGf[setGf <= numData]
    
    # Start our gap-filling journey! 
    dataGf <- data
    dataGf[setGf,Opt$NameVarGf] <- NA # Create the artificial gaps
    varGf <- dataGf[[Opt$NameVarGf]] # The gap-filled variable - will be updated at the end
    meas <- !base::is.na(varGf) # Note the originally measured values of the gap-filled variable
    
    setGfRep <- numeric(0) # Intialize indices of data gap-filled by the components of the framework
    setGfSiml <- numeric(0) # Intialize indices of data gap-filled by the components of the framework
    setGfSpac <- numeric(0) # Intialize indices of data gap-filled by the components of the framework
    setGfMds <- numeric(0) # Intialize indices of data gap-filled by the components of the framework
    
    ## Replicate Sensor ##
    if(!is.null(Opt$NameVarRep)){
      print('Gap-filling with replicate sensor regression...')
      
      NameVarGf <- Opt$NameVarGf
      NameVarRep <- Opt$NameVarRep
      
      # General regression - no time or range windowing
      dataRepRegAll <- def.gf.rep(dataGf=dataGf[,c(NameVarGf,NameVarRep)],meas=meas,NameVarGf=NameVarGf,NameVarRep=NameVarRep,
                                          Rbst=FALSE,Wndw=NULL,Rng=NULL,NumSampMin=5,RtioRngRepMax=0.1,RsqMin=NULL,LvlPred=0.95)
      
      # If the regression is poor using all the data
      # Warning - THE CODE IS VERY SLOW WITH THESE CONSTRAINTS. WORK ON SPEEDING UP THE CODE
      dataRepRegRng25 <- NULL
      dataRepRegRng25Wndw365 <- NULL
      if(FALSE){
        # Range windowing - form regression using data within 25% of the overall data range, centered on the replicate sensor value at the gap
        Rng <- (base::max(dataGf[,NameVarRep],na.rm=TRUE)-base::min(dataGf[,NameVarRep],na.rm=TRUE))*0.25/2 # Divide by 2 because want half the range window
        dataRepRegRng25 <- eddy4R.gf::def.gf.rep(dataGf=dataGf[,c(NameVarGf,NameVarRep)],meas=meas,NameVarGf=NameVarGf,NameVarRep=NameVarRep,
                                                 Rbst=TRUE,Wndw=NULL,Rng=Rng,NumSampMin=5,RtioRngRepMax=0.1,RsqMin=NULL,LvlPred=0.95)
        
        # Time windowing + range windowing, same 25 range window, use data within 1 year
        # Eventually need to incorporate metadata history. Constraint time windows to same sensor and calibration period
        Wndw <- 365*48/2 # Divide by 2 because want half the range window
        dataRepRegRng25Wndw365 <- eddy4R.gf::def.gf.rep(dataGf=dataGf[,c(NameVarGf,NameVarRep)],meas=meas,NameVarGf=NameVarGf,NameVarRep=NameVarRep,
                                                        Rbst=TRUE,Wndw=Wndw,Rng=Rng,NumSampMin=5,RtioRngRepMax=0.1,RsqMin=NULL,LvlPred=0.95)
      }
      
      # NEED TO ADD IN SELECTION OF BEST RESULT AMONG THE REPLICATE SENSOR REGRESSIONS (Already the best replicate sensor is selected)
      dataRepReg <- dataRepRegAll
      
      # Accounting
      setGfRep <- setGf[which(!is.na(dataRepReg$pred[setGf]+data[setGf,Opt$NameVarGf]))] # artificial gap-indices filled with replicate sensor regression
      setGapRep <- setGap[which(!is.na(dataRepReg$pred[setGap]))] # original gap-indices filled with replicate sensor regression
      print(paste0(length(setGfRep),' artificial gaps able to be filled with replicate sensor regression (these may overlap with original gaps)'))
      print(paste0(length(setGapRep),' original gaps able to be filled with replicate sensor regression (these may overlap with artifical gaps)'))

      # Calculate performance stats of this method
      idxMeth <- 'rep'
      varGfMeth <- varGf # Intialize
      varGfMeth[setGfRep] <- dataRepReg$pred[setGfRep] # Fill all possible artificial gaps
      dataStat <- data.frame(varGfMeas=data[setGfRep,Opt$NameVarGf],varGfPred=varGfMeth[setGfRep])
      dataStat$err <- dataStat$varGfPred-dataStat$varGfMeas # Error
      dataStat$ucrtIn <- dataStat$varGfMeas >= dataRepReg$lwr[setGfRep] & dataStat$varGfMeas <= dataRepReg$upr[setGfRep] # Boolean - did the actual observation lie within the prediction interval?
      rpt[[idxTypeGf]]$statMeth$numGf[[idxMeth]][idxPerm] <- length(setGfRep)
      rpt[[idxTypeGf]]$statMeth$rsq[[idxMeth]][idxPerm] <- stats::cor(dataStat$varGfPred,dataStat$varGfMeas)^2
      rpt[[idxTypeGf]]$statMeth$rmseAbs[[idxMeth]][idxPerm] <- sqrt((1/rpt[[idxTypeGf]]$statMeth$numGf[[idxMeth]][idxPerm])*sum((dataStat$varGfPred-dataStat$varGfMeas)^2))
      rpt[[idxTypeGf]]$statMeth$rmseRltv[[idxMeth]][idxPerm] <- sqrt(sum((dataStat$err)^2)/sum(dataStat$varGfMeas^2))
      rpt[[idxTypeGf]]$statMeth$mae[[idxMeth]][idxPerm] <- (1/rpt[[idxTypeGf]]$statMeth$numGf[[idxMeth]][idxPerm])*sum(abs(dataStat$err))
      rpt[[idxTypeGf]]$statMeth$bias[[idxMeth]][idxPerm] <- (1/rpt[[idxTypeGf]]$statMeth$numGf[[idxMeth]][idxPerm])*sum(dataStat$err) # Mean error
      rpt[[idxTypeGf]]$statMeth$sd[[idxMeth]][idxPerm] <- stats::sd(dataStat$err) # standard deviation of the error
      rpt[[idxTypeGf]]$statMeth$df[[idxMeth]][idxPerm] <- stats::median(dataRepReg$df[setGfRep]) # median degrees of freedom used for uncertainty calcs
      rpt[[idxTypeGf]]$statMeth$ucrtMed[[idxMeth]][idxPerm] <-  stats::median((dataRepReg$upr[setGfRep]-dataRepReg$lwr[setGfRep])/2) # median uncertainty in the prediction 
      rpt[[idxTypeGf]]$statMeth$ucrtInPerc[[idxMeth]][idxPerm] <- sum(dataStat$ucrtIn)/nrow(dataStat)*100 # % of the measured obs that fell within the prediction interval (effectiveness of the uncertainty estimate)
      
      # Develop a linear regression between the all replicate sensor and the target sensor data
      # Note - this is not used to fill gaps, only for plotting
      # Plotting
      if(Plot){
        print('Plotting...')
        
        plotReg <- vector(length(Opt$NameVarRep),mode='list')
        names(plotReg) <- Opt$NameVarRep
        
        for(idxVarRep in Opt$NameVarRep){
          
          dataRep <- data.frame(idep=dataGf[[idxVarRep]],dep=dataGf[[Opt$NameVarGf]])
          #coef <- robustbase::lmrob(dep ~ idep,data=dataRep)
          coef <- lm(dep ~ idep,data=dataRep)
        
          
          # Create the regression line
          minReg <- min(min(dataRep$idep,na.rm=TRUE),min(dataRep$dep,na.rm=TRUE))
          maxReg <- max(max(dataRep$idep,na.rm=TRUE),max(dataRep$dep,na.rm=TRUE))
          lineReg <- data.frame(idep=c(minReg,maxReg))
          lineReg$dep <- coef$coefficients[1]+lineReg$idep*coef$coefficients[2]
          line0101 <- data.frame(idep=c(minReg-0.05*(maxReg-minReg),maxReg+0.05*(maxReg-minReg)),
                                 dep=c(minReg-0.05*(maxReg-minReg),maxReg+0.05*(maxReg-minReg)))
          
          # plot 
          titl <- base::paste0('Replicate Regressions: ',Opt$NameSite, ', ',rpt[[idxTypeGf]]$nameGf,', Run ',idxPerm)
          
          plotReg[[idxVarRep]] <- plotly::plot_ly(data=dataRep,x=~idep) %>%
            plotly::add_markers(y=~dep,marker = base::list(color = 'rgba(30,30,30,1)', size = 4),name='data') %>%
            plotly::add_lines(data=lineReg,x=~idep,y=~dep,line = base::list(color = 'rgba(200,30,30,1)', width = 2),
                              name='Regression') %>%
            # plotly::add_lines(data=line0101,x=~idep,y=~dep,line = base::list(color = 'rgba(30,30,30,1)', width = 1),
            #                   name='1:1') %>%
            plotly::layout(margin = list(b = 50, t = 50, r=50),
                           title = titl,
                           xaxis = list(title = base::paste0(c(rep("\n&nbsp;", 3),
                                                               rep("&nbsp;", 20),
                                                               paste0(idxVarRep),
                                                               rep("&nbsp;", 20)),
                                                             collapse = ""),
                                        nticks=6,
                                        range=c(minReg-0.05*(maxReg-minReg),maxReg+0.05*(maxReg-minReg)),
                                        zeroline=FALSE
                           ),
                           yaxis = list(title = Opt$NameVarGf,
                                        range=c(minReg-0.05*(maxReg-minReg),maxReg+0.05*(maxReg-minReg)),
                                        zeroline=FALSE))
  
          # Save plot to file, or print to screen
          if(PlotSave){
            plotly::export(plotReg[[idxVarRep]],base::paste0(DirPlot,'/RepReg_',Opt$NameSite,'_',idxTypeGf,'_',idxPerm,'_',Sys.Date(),'_',idxVarRep,'_',Opt$NameVarGf,'.pdf'))
          } else {
            print(plotReg[[idxVarRep]])
          }
          
        }
        
        # option to plot all replicate regressions in same figure
        plotRegAll <- plotly::subplot(plotReg,nrows=1,titleX=TRUE,shareY=TRUE)
        #print(plotRegAll) 

        
        # Plot the counts of the regression chosen to gap fill
        numGfRep <- lapply(Opt$NameVarRep,FUN=function(idxVarRep){sum(dataRepRegAll$varFill==idxVarRep,na.rm=TRUE)})
        names(numGfRep) <- Opt$NameVarRep
        numGfRep <- unlist(numGfRep)
        
        titl <- base::paste0('Repl. Sensor Regression: ',Opt$NameSite, ', ',rpt[[idxTypeGf]]$nameGf,', Run ',idxPerm)
        
        plotGfRep <- plotly::plot_ly(
          x = Opt$NameVarRep,
          y = numGfRep,
          type = "bar"
        ) %>%
          plotly::layout(margin = list(b = 50, t = 50, r=50),
                         title = titl,
                         xaxis = list(title = base::paste0(c(rep("\n&nbsp;", 3),
                                                             rep("&nbsp;", 20),
                                                             paste0('Replicate regression used for gap-filling ',Opt$NameVarGf),
                                                             rep("&nbsp;", 20)),
                                                           collapse = "")
                         ),
                         yaxis = list(title = 'Count')
        )
        # Save plot to file, or print to screen
        if(PlotSave){
          plotly::export(plotGfRep,base::paste0(DirPlot,'/RepVarCount_',Opt$NameSite,'_',idxTypeGf,'_',idxPerm,'_',Sys.Date(),'.pdf'))
        } else {
          print(plotGfRep)
        }
        
        
        # Plot histogram of errors (artificial gaps)
        titl <- base::paste0('Repl. Sensor Regression: ',Opt$NameSite, ', ',rpt[[idxTypeGf]]$nameGf,', Run ',idxPerm)
        plotErrReg <- plotly::plot_ly(x = dataStat$err, type = "histogram") %>%
          plotly::layout(margin = list(b = 50, t = 50, r=50),
                         title = titl,
                         xaxis = list(title = base::paste0(c(rep("\n&nbsp;", 3),
                                                             rep("&nbsp;", 20),
                                                             paste0('Error (Pred-Meas)'),
                                                             rep("&nbsp;", 20)),
                                                           collapse = "")
                         ),
                         yaxis = list(title = 'Count')
          )
        
        # Save plot to file, or print to screen
        if(PlotSave){
          plotly::export(plotErrReg,base::paste0(DirPlot,'/RepErr_',Opt$NameSite,'_',idxTypeGf,'_',idxPerm,'_',Sys.Date(),'.pdf'))
        } else {
          print(plotErrReg)
        }
          
      } # End plotting
        

    } # End Replicate Sensor gap-filling
    

    ## MDS gap-filling ##
    if(!is.null(Opt$NameVarMds)){
      
      # Prepare data frame for MDS gap-filling
      dataMdsIn <- dataGf[c('TIMESTAMP_START',Opt$NameVarGf,Opt$NameVarMds)] 
      names(dataMdsIn)[1] <- 'sDateTime'
      
      # Are there any SW_IN variables? Change them to Rg so that the MDS code will treat it appropriately
      names(dataMdsIn) <- gsub(pattern='SW_IN',replacement='Rg',x=names(dataMdsIn))
      NameVarMds<-Opt$NameVarMds
      NameVarMds <- gsub(pattern='SW_IN',replacement='Rg',x=NameVarMds)

      # Run the MDS according to how many vars are used to create the lookup table
      if (length(NameVarMds) == 1){
        dataMdsOut <- sMDSGapFill(sDATA=dataMdsIn,Var.s=Opt$NameVarGf, V1.s=NameVarMds[1],T1.n=Opt$TolVarMds[1],FillAll.b = FALSE)
      } else if (length(NameVarMds) == 2){
        dataMdsOut <- sMDSGapFill(sDATA=dataMdsIn,Var.s=Opt$NameVarGf, V1.s=NameVarMds[1],T1.n=Opt$TolVarMds[1],V2.s=NameVarMds[2],
                                  T2.n=Opt$TolVarMds[2],FillAll.b = FALSE)
      } else if (length(NameVarMds) == 3){
        dataMdsOut <- sMDSGapFill(sDATA=dataMdsIn,Var.s=Opt$NameVarGf, V1.s=NameVarMds[1],T1.n=Opt$TolVarMds[1],V2.s=NameVarMds[2],
                                  T2.n=Opt$TolVarMds[2],V3.s=NameVarMds[3],T3.n=Opt$TolVarMds[3],FillAll.b = FALSE)
      } else {
        stop('Code is not yet set up to run more than 3 MDS predictor variables')
      }
      
      names(dataMdsOut)[1] <- 'DateTime' # Add timestamp
      
      # Compute the 95% confidence interval of the predictions from the MDS method
      df <- dataMdsOut[,grepl(pattern='_fnum',x=names(dataMdsOut))]-1 # Degrees of freedom
      #df <- dataMdsOut[,grepl(pattern='_fnum',x=names(dataMdsOut))]-6; df[df < 1] <- 1 # Testing set reduction in df
      tcrit <- stats::qt(p=.05/2, df=df, lower.tail=FALSE) # T distribution critical value
      diffPredMds <- tcrit*(dataMdsOut[,grepl(pattern='_fsd',x=names(dataMdsOut))]*sqrt(1+1/dataMdsOut[,grepl(pattern='_fnum',x=names(dataMdsOut))])) # half-width of the prediction interval
      dataMdsOut[[paste0(Opt$NameVarGf,'_lwr')]] <- dataMdsOut[[paste0(Opt$NameVarGf,'_f')]]-diffPredMds
      dataMdsOut[[paste0(Opt$NameVarGf,'_upr')]] <- dataMdsOut[[paste0(Opt$NameVarGf,'_f')]]+diffPredMds
      
      
      # Change any Rg variables back to SW_IN
      names(dataMdsOut) <- gsub(pattern='SW_IN',replacement='Rg',x=names(dataMdsOut))
      
      # What gaps were filled with MDS?
      setGfMds <- setGf[which(!is.na(dataMdsOut[setGf,base::paste0(Opt$NameVarGf,'_f')]))] # artificial gap-indices filled with MDS
      setGapMds <- setGap[which(!is.na(dataMdsOut[setGap,base::paste0(Opt$NameVarGf,'_f')]))] # original gap-indices filled with MDS
      print(paste0(length(setGfMds),' artificial gaps able to be filled with MDS (these may overlap with original gaps)'))
      print(paste0(length(setGapMds),' original gaps able to be filled with MDS (these may overlap with artifical gaps)'))
      
      # Calculate performance stats of this method
      idxMeth <- 'mds'  # ADJUST ME FOR METHOD ('rep','siml','spac','mds')
      varGfMeth <- varGf # Intialize
      varGfMeth[setGfMds] <- dataMdsOut[setGfMds,base::paste0(Opt$NameVarGf,'_f')] # Fill all possible artificial gaps - ADJUST ME FOR METHOD
      dataStat <- data.frame(varGfMeas=data[setGfMds,Opt$NameVarGf],varGfPred=varGfMeth[setGfMds])
      dataStat$err <- dataStat$varGfPred-dataStat$varGfMeas # Error
      dataStat$meth <- dataMdsOut[setGfMds,base::paste0(Opt$NameVarGf,'_fmeth')]
      dataStat$wndw <- dataMdsOut[setGfMds,base::paste0(Opt$NameVarGf,'_fwin')]
      dataStat$df <- df[setGfMds] # degrees of freedom used to form the prediction interval
      dataStat$ucrtPred <- (dataMdsOut[setGfMds,base::paste0(Opt$NameVarGf,'_upr')] - dataMdsOut[setGfMds,base::paste0(Opt$NameVarGf,'_lwr')])/2  # Uncertainty in the prediction (half width of confidence interval)
      dataStat$ucrtIn <- dataStat$varGfMeas >= dataMdsOut[setGfMds,base::paste0(Opt$NameVarGf,'_lwr')] & dataStat$varGfMeas <= dataMdsOut[setGfMds,base::paste0(Opt$NameVarGf,'_upr')] # Boolean - did the actual observation lie within the prediction interval?
      dataStat <- dataStat[!is.na(dataStat$varGfPred) & !is.na(dataStat$varGfMeas),] # Keep only pairwise complete observations
      
      for(idxMethMds in 1:3){
        idxMeth <- paste0('mdsMeth0',idxMethMds)
        dataStatIdx <- dataStat[dataStat$meth==idxMethMds,]
        rpt[[idxTypeGf]]$statMeth$numGf[[idxMeth]][idxPerm] <- nrow(dataStatIdx) # aritifical gaps able to be filled - ADJUST ME FOR METHOD
        rpt[[idxTypeGf]]$statMeth$rsq[[idxMeth]][idxPerm] <- stats::cor(dataStatIdx$varGfPred,dataStatIdx$varGfMeas)^2
        rpt[[idxTypeGf]]$statMeth$rmseAbs[[idxMeth]][idxPerm] <- sqrt((1/rpt[[idxTypeGf]]$statMeth$numGf[[idxMeth]][idxPerm])*sum((dataStatIdx$err)^2))
        rpt[[idxTypeGf]]$statMeth$rmseRltv[[idxMeth]][idxPerm] <- sqrt(sum((dataStatIdx$err)^2)/sum(dataStatIdx$varGfMeas^2))
        rpt[[idxTypeGf]]$statMeth$mae[[idxMeth]][idxPerm] <- (1/rpt[[idxTypeGf]]$statMeth$numGf[[idxMeth]][idxPerm])*sum(abs(dataStatIdx$err))
        rpt[[idxTypeGf]]$statMeth$bias[[idxMeth]][idxPerm] <- (1/rpt[[idxTypeGf]]$statMeth$numGf[[idxMeth]][idxPerm])*sum(dataStatIdx$err) # Mean error
        rpt[[idxTypeGf]]$statMeth$sd[[idxMeth]][idxPerm] <- stats::sd(dataStatIdx$err) # standard deviation of the error
        rpt[[idxTypeGf]]$statMeth$df[[idxMeth]][idxPerm] <- stats::median(dataStatIdx$df) # median degrees of freedom used for uncertainty calcs
        rpt[[idxTypeGf]]$statMeth$ucrtMed[[idxMeth]][idxPerm] <-  stats::median(dataStatIdx$ucrtPred) # median uncertainty in the prediction 
        rpt[[idxTypeGf]]$statMeth$ucrtInPerc[[idxMeth]][idxPerm] <- sum(dataStatIdx$ucrtIn)/nrow(dataStatIdx)*100 # % of the measured obs that fell within the prediction interval (effectiveness of the uncertainty estimate)
      }

      # Re-fill the artificial gaps in dataMdsIn with the measured values
      dataMdsIn[setGf,Opt$NameVarGf] <- data[setGf,Opt$NameVarGf]
      
    
      # MDS plotting
      if(Plot){
        print('Plotting...')
        
        # Produce some plots 
        titl <- base::paste0('MDS: ',Opt$NameSite, ', ',rpt[[idxTypeGf]]$nameGf,', Run ',idxPerm)
        
        # Plot the gaps indices
        dataSetGf <- data.frame(sDateTime=dataMdsIn$sDateTime,setGap=rep(0,times=numData),setGf=rep(0,times=numData))
        dataSetGf[setGap,"setGap"] <- 1 # Original gaps
        dataSetGf[setGf,"setGf"] <- 1 # Artificial gaps
        
        plotSetGf <- plotly::plot_ly(data=dataSetGf,x=~sDateTime,y=~setGap,type='scatter',mode='lines',
                                     line = base::list(color = 'rgba(30,30,200,1)', width = 1),
                                     name='orig gaps') %>%
          plotly::add_trace(data=dataSetGf,x=~sDateTime,y=~setGf,type='scatter',mode='lines',
                            line = base::list(color = 'rgba(100,100,30,1)', width = 1),
                            name='artif gaps') %>%
          plotly::layout(margin = list(b = 50, t = 50, r=50),
                         title = titl,
                         xaxis = list(title = base::paste0(c(rep("\n&nbsp;", 3),
                                                             rep("&nbsp;", 20),
                                                             paste0("Date-Time [",base::attr(dataMdsOut$DateTime,'tzone'),']'),
                                                             rep("&nbsp;", 20)),
                                                           collapse = ""),
                                      nticks=6,
                                      range = dataMdsOut$DateTime[c(1,base::length(dataMdsOut$DateTime))],
                                      zeroline=FALSE
                         ),
                         yaxis = list(title = '',
                                      tickmode='array',
                                      tickvals=c(0,1),
                                      ticktext=c('','gap'),
                                      range=c(-0.1,1.1),
                                      zeroline=FALSE))
        
        # plot the gap-filled dataset
        plotData <- plotly::plot_ly(data=dataMdsOut, x=~DateTime, y=dataMdsOut[[base::paste0(Opt$NameVarGf,'_f')]], type='scatter', 
                                    mode='lines', line = base::list(color = 'rgb(205, 12, 24)', width = 1),
                                    name='filled') %>%
          plotly::add_trace(data=dataMdsIn,x=~sDateTime,y=dataMdsIn[[Opt$NameVarGf]],
                            type='scatter',mode='lines',#'lines+markers',
                            line = base::list(color = 'rgba(30,30,30,1)', width = 1),
                            #marker = base::list(color = 'rgba(30,30,30,1)', size = 4),
                            name='raw') %>%
          plotly::layout(margin = list(b = 50, t = 50, r=50),
                         title = titl,
                         xaxis = list(title = base::paste0(c(rep("\n&nbsp;", 3),
                                                             rep("&nbsp;", 20),
                                                             paste0("Date-Time [",base::attr(dataMdsOut$DateTime,'tzone'),']'),
                                                             rep("&nbsp;", 20)),
                                                           collapse = ""),
                                      nticks=6,
                                      range = dataMdsOut$DateTime[c(1,base::length(dataMdsOut$DateTime))],
                                      zeroline=FALSE
                         ),
                         yaxis = list(title = Opt$NameVarGf,
                                      zeroline=FALSE),
                         legend = list(x = 100, y = 0.5))
        
        
        # Plot quality flags
        plotQf <- plotly::plot_ly(data=dataMdsOut, x=~DateTime, y=dataMdsOut[[base::paste0(Opt$NameVarGf,'_fqc')]], type='scatter', 
                                  mode='lines', line = base::list(color = 'rgb(30, 100, 100)', width = 1),
                                  name='QF') %>%
          plotly::layout(margin = list(b = 50, t = 50, r=50),
                         title = titl,
                         xaxis = list(title = base::paste0(c(rep("\n&nbsp;", 3),
                                                             rep("&nbsp;", 20),
                                                             paste0("Date-Time [",base::attr(dataMdsOut$DateTime,'tzone'),']'),
                                                             rep("&nbsp;", 20)),
                                                           collapse = ""),
                                      nticks=6,
                                      range = dataMdsOut$DateTime[c(1,base::length(dataMdsOut$DateTime))],
                                      zeroline=FALSE
                         ),
                         yaxis = list(title = 'QF',
                                      zeroline=FALSE))
        
        # Plot Gap-filling method
        plotMeth <- plotly::plot_ly(data=dataMdsOut, x=~DateTime, y=dataMdsOut[[base::paste0(Opt$NameVarGf,'_fmeth')]], type='scatter', 
                                    mode='lines', line = base::list(color = 'rgb(30, 100, 30)', width = 1),
                                    name='Method') %>%
          plotly::layout(margin = list(b = 50, t = 50, r=50),
                         title = titl,
                         xaxis = list(title = base::paste0(c(rep("\n&nbsp;", 3),
                                                             rep("&nbsp;", 20),
                                                             paste0("Date-Time [",base::attr(dataMdsOut$DateTime,'tzone'),']'),
                                                             rep("&nbsp;", 20)),
                                                           collapse = ""),
                                      nticks=6,
                                      range = dataMdsOut$DateTime[c(1,base::length(dataMdsOut$DateTime))],
                                      zeroline=FALSE
                         ),
                         yaxis = list(title = 'Method',
                                      zeroline=FALSE))
        
        # Combine the plots into a single graphic
        plotMdsAll <- plotly::subplot(plotSetGf,plotData,plotQf,plotMeth,nrows=4,titleY=TRUE,shareX=TRUE,heights=c(0.1,0.3,0.3,0.3))
        
        # Save plot to file, or print to screen
        if(PlotSave){
          plotly::export(plotMdsAll,base::paste0(DirPlot,'/MDS_',Opt$NameSite,'_',idxTypeGf,'_',idxPerm,'_',Sys.Date(),'.pdf'))
        } else {
          print(plotMdsAll)
        }
        
        
        
        # Also plot the 3 predictor variables. Highlight the points where there are gaps in the filled dataset.
        plotVar01 <- plotly::plot_ly(data=dataMdsIn, x=~sDateTime, y=dataMdsIn[[Opt$NameVarMds[1]]], type='scatter', 
                                     mode='lines', line = base::list(color = 'rgba(60,30,30,1)', width = 1),
                                     name='Var 1') %>%
          plotly::layout(margin = list(b = 50, t = 50, r=50),
                         title = titl,
                         xaxis = list(title = base::paste0(c(rep("\n&nbsp;", 3),
                                                             rep("&nbsp;", 20),
                                                             paste0("Date-Time [",base::attr(dataMdsOut$DateTime,'tzone'),']'),
                                                             rep("&nbsp;", 20)),
                                                           collapse = ""),
                                      nticks=6,
                                      range = dataMdsOut$DateTime[c(1,base::length(dataMdsOut$DateTime))],
                                      zeroline=FALSE
                         ),
                         yaxis = list(title = Opt$NameVarMds[1],
                                      zeroline=FALSE))
        
        plotVar02 <- plotly::plot_ly(data=dataMdsIn, x=~sDateTime, y=dataMdsIn[[Opt$NameVarMds[2]]], type='scatter', 
                                     mode='lines', line = base::list(color = 'rgb(30, 60, 30)', width = 1),
                                     name='Var 2') %>%
          plotly::layout(margin = list(b = 50, t = 50, r=50),
                         title = titl,
                         xaxis = list(title = base::paste0(c(rep("\n&nbsp;", 3),
                                                             rep("&nbsp;", 20),
                                                             paste0("Date-Time [",base::attr(dataMdsOut$DateTime,'tzone'),']'),
                                                             rep("&nbsp;", 20)),
                                                           collapse = ""),
                                      nticks=6,
                                      range = dataMdsOut$DateTime[c(1,base::length(dataMdsOut$DateTime))],
                                      zeroline=FALSE
                         ),
                         yaxis = list(title = Opt$NameVarMds[2],
                                      zeroline=FALSE))
        
        plotVar03 <- plotly::plot_ly(data=dataMdsIn, x=~sDateTime, y=dataMdsIn[[Opt$NameVarMds[3]]], type='scatter', 
                                     mode='lines', line = base::list(color = 'rgb(30, 30, 60)', width = 1),
                                     name='Var 3') %>%
          plotly::layout(margin = list(b = 50, t = 50, r=50),
                         title = titl,
                         xaxis = list(title = base::paste0(c(rep("\n&nbsp;", 3),
                                                             rep("&nbsp;", 20),
                                                             paste0("Date-Time [",base::attr(dataMdsOut$DateTime,'tzone'),']'),
                                                             rep("&nbsp;", 20)),
                                                           collapse = ""),
                                      nticks=6,
                                      range = dataMdsOut$DateTime[c(1,base::length(dataMdsOut$DateTime))],
                                      zeroline=FALSE
                         ),
                         yaxis = list(title = Opt$NameVarMds[3],
                                      zeroline=FALSE))
        
        # Combine the plots into a single graphic
        plotVarAll <- plotly::subplot(plotSetGf,plotVar01,plotVar02,plotVar03,nrows=4,heights=c(0.1,0.3,0.3,0.3),titleY=TRUE,shareX=TRUE)
        
        # Save plot to file, or print to screen
        if(PlotSave){
          plotly::export(plotVarAll,base::paste0(DirPlot,'/MDSpvar_',Opt$NameSite,'_',idxTypeGf,'_',idxPerm,'_',Sys.Date(),'.pdf'))
        } else {
          print(plotVarAll)
        }
        
        # Plot histogram of errors (artificial gaps)
        titl <- base::paste0('Repl. Sensor Regression: ',Opt$NameSite, ', ',rpt[[idxTypeGf]]$nameGf,', Run ',idxPerm)
        plotErrMds <- list()
        for(idxPlot in 1:3){
          plotErrMds[[idxPlot]] <- plotly::plot_ly(x = dataStat$err[dataStat$meth==idxPlot], type = "histogram") %>%
            plotly::layout(margin = list(b = 50, t = 50, r=50),
                           title = paste0('Method ',idxPlot),
                           xaxis = list(title = base::paste0(c(rep("\n&nbsp;", 3),
                                                               rep("&nbsp;", 20),
                                                               paste0('Error (Meth ', idxPlot,')'),
                                                               rep("&nbsp;", 20)),
                                                             collapse = "")
                           ),
                           yaxis = list(title = 'Count',
                                        zeroline=FALSE)
            )
        }
        
        # Combine the plots into a single graphic
        plotErrMdsAll <- plotly::subplot(plotErrMds[[1]],plotErrMds[[2]],plotErrMds[[3]],nrows=1,titleY=TRUE,shareX=TRUE)
        
        # Save plot to file, or print to screen
        if(PlotSave){
          plotly::export(plotErrMdsAll,base::paste0(DirPlot,'/MdsErr_',Opt$NameSite,'_',idxTypeGf,'_',idxPerm,'_',Sys.Date(),'.pdf'))
        } else {
          print(plotErrMdsAll)
        }
        
        # Write to screen the number of observations in which predictor variables coincide with a gap in the gap-filled dataset, 
        # and how many observations have all predictor & gap-filled variable availability
        print(base::paste0('N data: ',nrow(dataMdsIn)))
        print(base::paste0('N of all-predictor availability coinciding with target gap: ',
                           sum(apply(X=cbind(!is.na(dataMdsIn[,!(names(dataMdsIn) %in% c('sDateTime',Opt$NameVarGf))]),
                                             is.na(dataMdsIn[,Opt$NameVarGf])),
                                     MARGIN=1,FUN=sum)==length(Opt$NameVarMds)+1)))
        print(base::paste0('N of all predictor & target variable availability: ',
                           sum(apply(X=!is.na(dataMdsIn[,!(names(dataMdsIn) %in% 'sDateTime')]),MARGIN=1,FUN=sum)==length(Opt$NameVarMds)+1)))
        
      } # End MDS plotting
    } # End MDS gap-filling

    # From the different methods, choose the one with the best performance for each gap based on it's prediction interval
    if(!is.null(Opt$NameVarRep) && !is.null(Opt$NameVarMds)){
      
      predGf <- base::data.frame(mds=dataMdsOut[[base::paste0(Opt$NameVarGf,'_f')]],
                                 rep=dataRepRegAll$pred,
                                 stringsAsFactors = FALSE)
      ucrtGf <- base::data.frame(mds=dataMdsOut[[base::paste0(Opt$NameVarGf,'_upr')]]-dataMdsOut[[base::paste0(Opt$NameVarGf,'_lwr')]],
                                 rep=dataRepRegAll$upr-dataRepRegAll$lwr,
                                 stringsAsFactors = FALSE)
      dataSlct <- def.gf.slct(data=varGf,predGf=predGf,ucrtGf=ucrtGf)
      varGf <- dataSlct$data # Gap-filled dataset
      
      # Count gaps filled with replicate sensor regression
      setGfRep <- intersect(setGf,which(dataSlct$varSlct == 'rep')) # indices of artifical gaps filled with replicate sensor regression
      setGapRep <- intersect(setGap,which(dataSlct$varSlct == 'rep')) # indices of original gaps filled with replicate sensor regression
      rpt[[idxTypeGf]]$numGfMeth[idxPerm,'rep'] <- length(setGfRep) #  - save it.
      print(paste0(length(setGfRep),' artificial gaps actually filled with replicate sensor regression (these may overlap with original gaps)'))
      print(paste0(length(setGapRep),' original gaps actually filled with replicate sensor regression (these may overlap with original gaps)'))
      
      # Count gaps filled with MDS
      setGfMds <- intersect(setGf,which(dataSlct$varSlct == 'mds')) # indices of artifical gaps filled with MDS/MDC
      setGapMds <- intersect(setGap,which(dataSlct$varSlct == 'mds')) # indices of original gaps filled with replicate sensor regression
      methMds <- dataMdsOut[setGfMds,paste0(Opt$NameVarGf,'_fmeth')] # Which mds method was used
      rpt[[idxTypeGf]]$numGfMeth[idxPerm,'mdsMeth01'] <- sum(methMds == 1) #  - save it.
      rpt[[idxTypeGf]]$numGfMeth[idxPerm,'mdsMeth02'] <- sum(methMds == 2) #  - save it.
      rpt[[idxTypeGf]]$numGfMeth[idxPerm,'mdsMeth03'] <- sum(methMds == 3) #  - save it.
      print(paste0(length(setGfMds),' artificial gaps actually filled with MDS/MDC (these may overlap with original gaps)'))
      print(paste0(length(setGapMds),' original gaps actually filled with MDS/MDC (these may overlap with original gaps)'))
      
      # How often did we choose the method closest to the actual value
      err <- abs(dataSlct$pred[dataSlct$gap]-data[[Opt$NameVarGf]][dataSlct$gap])
      errMds <- abs(dataMdsOut[dataSlct$gap,base::paste0(Opt$NameVarGf,'_f')]-data[[Opt$NameVarGf]][dataSlct$gap])
      errRep <- abs(dataRepRegAll$pred[dataSlct$gap]-data[[Opt$NameVarGf]][dataSlct$gap])
      setEval <- !is.na(errMds+errRep)
      setMin <- Rfast::rowMins(as.matrix(data.frame(errMds[setEval],errRep[setEval])),value=FALSE)
      methBest <- c('mds','rep')[setMin] 
      methSlct <- dataSlct$varSlct[dataSlct$gap][setEval]
      compMeth <- data.frame(best=methBest,slct=methSlct,errMds=errMds[setEval],errRep=errRep[setEval],stringsAsFactors=FALSE)
      compMeth$rite <- compMeth$slct==compMeth$best
    } 
    
    # Calculate performance stats of the entire gap-filling framework
    dataStat <- data.frame(varGfMeas=data[setGf,Opt$NameVarGf],varGfPred=varGf[setGf])
    dataStat <- dataStat[!is.na(dataStat$varGfPred) & !is.na(dataStat$varGfMeas),] # Keep only pairwise complete observations
    rpt[[idxTypeGf]]$statFmwk$numGf[idxPerm] <- nrow(dataStat)
    rpt[[idxTypeGf]]$statFmwk$rsq[idxPerm] <- stats::cor(dataStat$varGfPred,dataStat$varGfMeas)^2
    rpt[[idxTypeGf]]$statFmwk$rmseAbs[idxPerm] <- sqrt((1/rpt[[idxTypeGf]]$statFmwk$numGf[idxPerm])*sum((dataStat$varGfPred-dataStat$varGfMeas)^2))
    rpt[[idxTypeGf]]$statFmwk$rmseRltv[idxPerm] <- sqrt(sum((dataStat$varGfPred-dataStat$varGfMeas)^2)/sum(dataStat$varGfMeas^2))
    rpt[[idxTypeGf]]$statFmwk$mae[idxPerm] <- (1/rpt[[idxTypeGf]]$statFmwk$numGf[idxPerm])*sum(abs(dataStat$varGfPred-dataStat$varGfMeas))
    rpt[[idxTypeGf]]$statFmwk$bias[idxPerm] <- (1/rpt[[idxTypeGf]]$statFmwk$numGf[idxPerm])*sum(dataStat$varGfPred-dataStat$varGfMeas)
    rpt[[idxTypeGf]]$statFmwk$methRite[idxPerm] <- sum(compMeth$rite)/nrow(compMeth)*100 # Percentage of time the best prediction was chosen 
    
    # Plot performance of the overall gap-filling framework
    if(Plot){
      print('Plotting overall gap-filling performance...')
      
      # Regress measured vs. predicted
      coef <- stats::lm(varGfPred ~ varGfMeas,data=dataStat)
      
      # Create the regression line
      minReg <- min(min(dataStat$varGfMeas,na.rm=TRUE),min(dataStat$varGfPred,na.rm=TRUE))
      maxReg <- max(max(dataStat$varGfMeas,na.rm=TRUE),max(dataStat$varGfPred,na.rm=TRUE))
      lineReg <- data.frame(idep=c(minReg,maxReg))
      lineReg$dep <- coef$coefficients[1]+lineReg$idep*coef$coefficients[2]
      line0101 <- data.frame(idep=c(minReg-0.05*(maxReg-minReg),maxReg+0.05*(maxReg-minReg)),
                             dep=c(minReg-0.05*(maxReg-minReg),maxReg+0.05*(maxReg-minReg)))
      
      
      # plot 
      titl <- base::paste0('Overall performance: ',Opt$NameSite, ', ',rpt[[idxTypeGf]]$nameGf,', Run ',idxPerm)
      
      plotPfmc <- plotly::plot_ly() %>%
        plotly::add_markers(data=dataStat[setGf %in% setGfRep,],x=~varGfMeas,y=~varGfPred,marker = base::list(color = 'rgba(30,200,30,1)', size = 4),name=paste0('Replicate Sensor (',length(setGfRep), ')')) %>%
        plotly::add_markers(data=dataStat[setGf %in% setGfSiml,],x=~varGfMeas,y=~varGfPred,marker = base::list(color = 'rgba(200,200,30,1)', size = 4),name=paste0('Similar Sensor (',length(setGfSiml), ')')) %>%
        plotly::add_markers(data=dataStat[setGf %in% setGfSpac,],x=~varGfMeas,y=~varGfPred,marker = base::list(color = 'rgba(30,200,200,1)', size = 4),name=paste0('Spatial Sensor (',length(setGfSpac), ')')) %>%
        plotly::add_markers(data=dataStat[setGf %in% setGfMds,],x=~varGfMeas,y=~varGfPred,marker = base::list(color = 'rgba(30,30,200,1)', size = 4),name=paste0('MDS (',length(setGfMds), ')')) %>%
        # plotly::add_text(x=c(minReg+0.75*(maxReg-minReg)),y=c(minReg+0.1*(maxReg-minReg)),
        #                  text=paste0('y=',round(coef$coefficients[2],2),'x+',round(coef$coefficients[1],2)),
        #                  textposition = 'middle center',textfont = list(color = '#ff0000', size = 12),
        #                  showlegend=FALSE) %>%
        plotly::add_lines(data=lineReg,x=~idep,y=~dep,line = base::list(color = 'rgba(200,30,30,1)', width = 2),
                          name=paste0('Overall fit (y=',round(coef$coefficients[2],2),'x+',round(coef$coefficients[1],2),')')) %>%
        plotly::add_lines(data=line0101,x=~idep,y=~dep,line = base::list(color = 'rgba(30,30,30,1)', width = 1),
                          name='1:1') %>%
        plotly::layout(margin = list(b = 50, t = 50, r=50),
                       title = titl,
                       xaxis = list(title = base::paste0(c(rep("\n&nbsp;", 3),
                                                           rep("&nbsp;", 20),
                                                           paste0('Measured ',Opt$NameVarGf),
                                                           rep("&nbsp;", 20)),
                                                         collapse = ""),
                                    nticks=6,
                                    range=c(minReg-0.05*(maxReg-minReg),maxReg+0.05*(maxReg-minReg)),
                                    zeroline=FALSE
                       ),
                       yaxis = list(title = paste0('Predicted ',Opt$NameVarGf),
                                    range=c(minReg-0.05*(maxReg-minReg),maxReg+0.05*(maxReg-minReg)),
                                    zeroline=FALSE))
      
      # Save plot to file, or print to screen
      if(PlotSave){
        plotly::export(plotPfmc,base::paste0(DirPlot,'/Pfmc_',Opt$NameSite,'_',idxTypeGf,'_',idxPerm,'_',Sys.Date(),'.pdf'))
      } else {
        print(plotPfmc)
      }
    } # End overall performance plotting
    
  } # End loop around permutations
  
} # End loop around gap-filling scenarios


# Plot the R-squared performance 
stat <- lapply(names(rpt),FUN=function(idxTypeGf){rpt[[idxTypeGf]]$statMeth$rsq})
names(stat)<-names(rpt)
statAll <- reshape2::melt(stat)
statAll <- statAll[,c('variable','value','L1')]
names(statAll) <- c('method','Rsq','typeGf')

titl=base::paste0('R-squared Performance')

plotBoxRsq <- plotly::plot_ly(statAll,x=~typeGf,y=~Rsq,color=~method,type='box') %>%
  plotly::layout(boxmode='group',
                 yaxis=list(title='R-squared'),
                 xaxis=list(title='Gap type',
                            categoryorder='array',
                            categoryarray=c('VS','S','M','L','VL','X')),
                 title=titl
  )
if(PlotSave){
  #plotly::export(plotBoxRsq,base::paste0(DirPlot,'/','PfmcAll_numGf_',Opt$NameSite,'_',Opt$NameVarGf,'_',Sys.Date(),'.pdf'))
} else {
  print(plotBoxRsq)
}

# Plot the bias performance of the (repMdsDflt framework only)
stat <- lapply(names(rpt),FUN=function(idxTypeGf){rpt[[idxTypeGf]]$statMeth$bias})
names(stat)<-names(rpt)
statAll <- reshape2::melt(stat)
statAll <- statAll[,c('variable','value','L1')]
names(statAll) <- c('method','bias','typeGf')

titl=base::paste0('Bias Performance')

plotBias <- plotly::plot_ly(statAll,x=~typeGf,y=~bias,color=~method,type='box') %>%
  plotly::layout(boxmode='group',
                 yaxis=list(title='Bias'),
                 xaxis=list(title='Gap type',
                            categoryorder='array',
                            categoryarray=c('VS','S','M','L','VL','X')),
                 title=titl
  )
if(PlotSave){
  #plotly::export(plotBoxRsq,base::paste0(DirPlot,'/','PfmcAll_numGf_',Opt$NameSite,'_',Opt$NameVarGf,'_',Sys.Date(),'.pdf'))
} else {
  print(plotBias)
}

# Plot the ability of the uncertainty estimates to adequately reflect the uncertainty 
stat <- lapply(names(rpt),FUN=function(idxTypeGf){rpt[[idxTypeGf]]$statMeth$ucrtInPerc})
names(stat)<-names(rpt)
statAll <- reshape2::melt(stat)
statAll <- statAll[,c('variable','value','L1')]
names(statAll) <- c('method','percWithinUcrt95','typeGf')
statAll$method <- plyr::revalue(statAll$method, c("rep"="Regression (WS_1_5_1)","mdsMeth01"="MDS (SW_IN,VPD,TA)","mdsMeth02"="MDS (SW_IN only)","mdsMeth03"="MDV"))

titl=base::paste0('Performance of uncertainty estimates')


plotBoxUcrtIn <- plotly::plot_ly(statAll,x=~typeGf,y=~percWithinUcrt95,color=~method,type='box') %>%
  plotly::layout(boxmode='group',
                 yaxis=list(title='% observations within 95% prediction interval'),
                 xaxis=list(title='Gap type',
                            categoryorder='array',
                            categoryarray=c('VS','S','M','L','VL','X')),
                 title=titl
  )
if(PlotSave){
  #plotly::export(plotBoxRsq,base::paste0(DirPlot,'/','PfmcAll_numGf_',Opt$NameSite,'_',Opt$NameVarGf,'_',Sys.Date(),'.pdf'))
} else {
  print(plotBoxUcrtIn)
}


# Plot the median uncertainty in the prediction 
stat <- lapply(names(rpt),FUN=function(idxTypeGf){rpt[[idxTypeGf]]$statMeth$ucrtMed})
names(stat)<-names(rpt)
statAll <- reshape2::melt(stat)
statAll <- statAll[,c('variable','value','L1')]
names(statAll) <- c('method','ucrt','typeGf')

titl=base::paste0('Median uncertainty of the prediction (half width of confidence interval)')


plotBoxUcrt <- plotly::plot_ly(statAll,x=~typeGf,y=~ucrt,color=~method,type='box') %>%
  plotly::layout(boxmode='group',
                 yaxis=list(title='Prediction Uncertainty'),
                 xaxis=list(title='Gap type',
                            categoryorder='array',
                            categoryarray=c('VS','S','M','L','VL','X')),
                 title=titl
  )
if(PlotSave){
  #plotly::export(plotBoxRsq,base::paste0(DirPlot,'/','PfmcAll_numGf_',Opt$NameSite,'_',Opt$NameVarGf,'_',Sys.Date(),'.pdf'))
} else {
  print(plotBoxUcrt)
}



# Plot the median degrees of freedom used in the uncertainty calcs
stat <- lapply(names(rpt),FUN=function(idxTypeGf){rpt[[idxTypeGf]]$statMeth$df})
names(stat)<-names(rpt)
statAll <- reshape2::melt(stat)
statAll <- statAll[,c('variable','value','L1')]
names(statAll) <- c('method','df','typeGf')

titl=base::paste0('Degrees of freedom for uncertainty estimates')


plotBoxDf <- plotly::plot_ly(statAll,x=~typeGf,y=~df,color=~method,type='box') %>%
  plotly::layout(boxmode='group',
                 yaxis=list(title='Median degrees of freedom used in prediction interval'),
                 xaxis=list(title='Gap type',
                            categoryorder='array',
                            categoryarray=c('VS','S','M','L','VL','X')),
                 title=titl
  )
if(PlotSave){
  #plotly::export(plotBoxRsq,base::paste0(DirPlot,'/','PfmcAll_numGf_',Opt$NameSite,'_',Opt$NameVarGf,'_',Sys.Date(),'.pdf'))
} else {
  print(plotBoxDf)
}




# Plot the proportion of artifical gaps filled with each method
stat <- lapply(names(rpt),FUN=function(idxTypeGf){rpt[[idxTypeGf]]$numGf})
names(stat)<-names(rpt)
statAll <- reshape2::melt(stat)
statAll <- statAll[,c('variable','value','L1')]
names(statAll) <- c('method','numGf','typeGf')

titl=base::paste0('Number of gap-filled points')

plotBoxNumMeth <- plotly::plot_ly(statAll,x=~typeGf,y=~numGf,color=~method,type='box') %>%
  plotly::layout(boxmode='group',
                 yaxis=list(title='Number'),
                 xaxis=list(title='Gap type',
                            categoryorder='array',
                            categoryarray=c('VS','S','M','L','VL','X')),
                 title=titl
  )
if(PlotSave){
  #plotly::export(plotBoxRsq,base::paste0(DirPlot,'/','PfmcAll_numGf_',Opt$NameSite,'_',Opt$NameVarGf,'_',Sys.Date(),'.pdf'))
} else {
  print(plotBoxNumMeth)
}




# Plot the proportion of time the best method was chosen
stat <- lapply(names(rpt),FUN=function(idxTypeGf){rpt[[idxTypeGf]]$statFmwk$methRite})
names(stat)<-names(rpt)
statAll <- reshape2::melt(stat)
statAll <- statAll[,c('value','L1')]
names(statAll) <- c('methRite','typeGf')

titl=base::paste0('Percentage of the time the best prediction was chosen')

plotBoxMethRite <- plotly::plot_ly(statAll,x=~typeGf,y=~methRite,type='box') %>%
  plotly::layout(boxmode='group',
                 yaxis=list(title='% right'),
                 xaxis=list(title='Gap type',
                            categoryorder='array',
                            categoryarray=c('VS','S','M','L','VL','X')),
                 title=titl
  )
if(PlotSave){
  #plotly::export(plotBoxRsq,base::paste0(DirPlot,'/','PfmcAll_numGf_',Opt$NameSite,'_',Opt$NameVarGf,'_',Sys.Date(),'.pdf'))
} else {
  print(plotBoxMethRite)
}


# Save the output stats and input options
if(RptSave){
  
  save(Opt,rpt,file=base::paste0(DirRpt,'/rpt_',Opt$NameSite,'_',Opt$NameVarGf,'_',Sys.Date(),'.RData'))
  
}