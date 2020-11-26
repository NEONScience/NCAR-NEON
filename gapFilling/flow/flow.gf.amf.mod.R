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

DirData <- 'N:/Science/FIUDATA/IPT_data/dynamic/GF_AMF/collaborationTestData'
Plot <- TRUE # plot the output?
PlotSave <- TRUE # Save the plots?
DirPlot <- base::paste0(DirData,'/plots/US-SER_TA_',Sys.Date())
RptSave <- TRUE # Save output stats and options?
DirRpt <- base::paste0(DirData,'/rpt')

# Specify variable & gap-filling framework details
Opt <- base::list(
  NameFileData = "US-SER_HH_201512311900_201805311900.csv",
  tz = 'EST',
  NameSite = 'US-SER', # Site
  NameVarGfBase = "TA", # Base-name of gap-filled variable
  NameVarGf = "TA_1_1_1", # variable to gap-fill
  UnitVarGf = 'deg C', # Units of gap-filled variable
  TimeBgn = as.POSIXct('2017-05-01',tz='EST'), # Start date of data to use
  TimeEnd = as.POSIXct('2018-03-01',tz='EST'), # End date of data to use
  # No specify gap-filling framework(s) - enter multiple frameworks to test in additional named lists (make sure list names 
  # are unique). Non-null entries are assumed to indicate performing that step
  Fmwk=base::list(
    repMdsDflt = base::list(
      NameFmwk = 'Replicate Sensor + MDS Default',
      NameVarRep = c("TA_1_1_2"), # replicates for the gap-filled variable. Enter NULL for none.
      NameVarSiml = NULL, # Theoretically similar measurements at same location (like PAR similar to incoming shortwave). Just one variable for now. Enter NULL for none.
      NameVarSpac = NULL, # same variable as gap-filled, but confounded by spatial variation (horizontal or vertical). Enter NULL for none.
      NameVarMds = c("SW_IN_1_1_1","VPD","TA_1_1_1"), # Input vars to MDS, e.g. SW_IN, VPD, TA. Enter NULL for none.
      TolVarMds = c(50,5,2.5) # Tolerance interval for MDS variables specified above, in same units as variable
    )#,
    # mdsDflt=base::list(
    #   NameFmwk = 'MDS default',
    #   NameVarRep = NULL, # replicates for the gap-filled variable. Enter NULL for none.
    #   NameVarSiml = NULL, # Theoretically similar measurements at same location (like PAR similar to incoming shortwave). Just one variable for now. Enter NULL for none.
    #   NameVarSpac = NULL, # same variable as gap-filled, but confounded by spatial variation (horizontal or vertical). Enter NULL for none.
    #   NameVarMds = c("SW_IN_1_1_1","VPD","TA_1_1_1"), # Input vars to MDS, e.g. SW_IN, VPD, TA. Enter NULL for none.
    #   TolVarMds = c(50,5,2.5) # Tolerance interval for MDS variables specified above, in same units as variable
    # )
  ) # End frameworks
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

# Work on this module if/when there is a dynamic selection of variables that are replicates, spatial variates, etc.
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
numGfMeth <- lapply(1:length(Opt$Fmwk),FUN=function(idx){numGfMeth}) # Replicate the gap-filled numbers df, one for each gap-filling framwork we will test
names(numGfMeth) <- names(Opt$Fmwk) # name the frameworks
stat = data.frame(numGf=dmmyNa,rsq=dmmyNa,rmseAbs=dmmyNa,rmseRltv=dmmyNa,mae=dmmyNa,bias=dmmyNa,sd=dmmyNa) # overall gap-filling performance statistics
stat <- lapply(1:length(Opt$Fmwk),FUN=function(idx){stat}) # Replicate the statistics df, one for each gap-filling framwork we will test
names(stat) <- names(Opt$Fmwk) # name the frameworks
statMeth = list(numGf=dmmyMeth,rsq=dmmyMeth,rmseAbs=dmmyMeth,rmseRltv=dmmyMeth,mae=dmmyMeth,bias=dmmyMeth,sd=dmmyMeth) # performance statistics for each method (regardless of whether values were gap-filled with the method)
statMeth <- lapply(1:length(Opt$Fmwk),FUN=function(idx){statMeth}) # Replicate the statistics, one for each gap-filling framework we will test
names(statMeth) <- names(Opt$Fmwk) # name the frameworks

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
# Loop around gap-filling frameworks
for(idxFmwk in names(Opt$Fmwk)) {
  
  # Plot the data involved in this method
  nameVarFmwk <- base::unique(c(Opt$NameVarGf,Opt$Fmwk[[idxFmwk]]$NameVarRep,Opt$Fmwk[[idxFmwk]]$NameVarSiml,
                                Opt$Fmwk[[idxFmwk]]$NameVarSpac,Opt$Fmwk[[idxFmwk]]$NameVarMds))
  

  plotVarFmwk <- list()
  titl <- base::paste0('Data in the "', Opt$Fmwk[[idxFmwk]]$NameFmwk, '" framework')
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
    plotly::export(plotVarFmwkAll,base::paste0(DirPlot,'/',idxFmwk,'_AllVars_',Opt$NameSite,'.pdf'))
  } else {
    print(plotVarFmwkAll)
  }
  
  # Loop around gap scenarios
  for(idxTypeGf in nameTypeGf){
    
    #if(idxTypeGf != 'VL'){next} # DELETE ME
    
    print(paste0('Running ',rpt[[idxTypeGf]]$nameGf,' scenario...'))
    
    # Loop around permutations for each gap scenario
    for(idxPerm in 1:numPerm){
      
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
      if(!is.null(Opt$Fmwk[[idxFmwk]]$NameVarRep)){
        print('Gap-filling with replicate sensor regression...')
        
        NameVarGf <- Opt$NameVarGf
        NameVarRep <- Opt$Fmwk[[idxFmwk]]$NameVarRep
        
        # General regression - no time or range windowing
        dataRepRegAll <- eddy4R.gf::def.gf.rep(dataGf=dataGf[,c(NameVarGf,NameVarRep)],meas=meas,NameVarGf=NameVarGf,NameVarRep=NameVarRep,
                                            Rbst=TRUE,Wndw=NULL,Rng=NULL,NumSampMin=5,RtioRngRepMax=0.1,RsqMin=NULL,LvlPred=0.95)
        
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
        setGfRep <- setGf[which(!is.na(dataRepReg$pred[setGf]))] # artificial gap-indices filled with replicate sensor regression
        setGapRep <- setGap[which(!is.na(dataRepReg$pred[setGap]))] # original gap-indices filled with replicate sensor regression
        print(paste0(length(setGfRep),' artificial gaps able to be filled with replicate sensor regression (these may overlap with original gaps)'))
        print(paste0(length(setGapRep),' original gaps able to be filled with replicate sensor regression (these may overlap with artifical gaps)'))

        # Calculate performance stats of this method
        idxMeth <- 'rep'
        varGfMeth <- varGf # Intialize
        varGfMeth[setGfRep] <- dataRepReg$pred[setGfRep] # Fill all possible artificial gaps
        dataStat <- data.frame(varGfMeas=data[setGf,Opt$NameVarGf],varGfPred=varGfMeth[setGf])
        dataStat <- dataStat[!is.na(dataStat$varGfPred) & !is.na(dataStat$varGfMeas),] # Keep only pairwise complete observations
        dataStat$err <- dataStat$varGfPred-dataStat$varGfMeas # Error
        rpt[[idxTypeGf]]$statMeth[[idxFmwk]]$numGf[[idxMeth]][idxPerm] <- length(setGfRep)
        rpt[[idxTypeGf]]$statMeth[[idxFmwk]]$rsq[[idxMeth]][idxPerm] <- stats::cor(dataStat$varGfPred,dataStat$varGfMeas)^2
        rpt[[idxTypeGf]]$statMeth[[idxFmwk]]$rmseAbs[[idxMeth]][idxPerm] <- sqrt((1/rpt[[idxTypeGf]]$statMeth[[idxFmwk]]$numGf[[idxMeth]][idxPerm])*sum((dataStat$varGfPred-dataStat$varGfMeas)^2))
        rpt[[idxTypeGf]]$statMeth[[idxFmwk]]$rmseRltv[[idxMeth]][idxPerm] <- sqrt(sum((dataStat$err)^2)/sum(dataStat$varGfMeas^2))
        rpt[[idxTypeGf]]$statMeth[[idxFmwk]]$mae[[idxMeth]][idxPerm] <- (1/rpt[[idxTypeGf]]$statMeth[[idxFmwk]]$numGf[[idxMeth]][idxPerm])*sum(abs(dataStat$err))
        rpt[[idxTypeGf]]$statMeth[[idxFmwk]]$bias[[idxMeth]][idxPerm] <- (1/rpt[[idxTypeGf]]$statMeth[[idxFmwk]]$numGf[[idxMeth]][idxPerm])*sum(dataStat$err) # Mean error
        rpt[[idxTypeGf]]$statMeth[[idxFmwk]]$sd[[idxMeth]][idxPerm] <- stats::sd(dataStat$err) # standard deviation of the error
        
        # Develop a linear regression between the all replicate sensor and the target sensor data
        # Note - this is not used to fill gaps, only for plotting
        dataRep <- data.frame(idep=data[[Opt$Fmwk[[idxFmwk]]$NameVarRep[1]]],dep=data[[Opt$NameVarGf]])
        coef <- robustbase::lmrob(dep ~ idep,data=dataRep)
        
        # Plotting
        if(Plot){
          print('Plotting...')
          
          # Create the regression line
          minReg <- min(min(dataRep$idep,na.rm=TRUE),min(dataRep$dep,na.rm=TRUE))
          maxReg <- max(max(dataRep$idep,na.rm=TRUE),max(dataRep$dep,na.rm=TRUE))
          lineReg <- data.frame(idep=c(minReg,maxReg))
          lineReg$dep <- coef$coefficients[1]+lineReg$idep*coef$coefficients[2]
          line0101 <- data.frame(idep=c(minReg-0.05*(maxReg-minReg),maxReg+0.05*(maxReg-minReg)),
                                 dep=c(minReg-0.05*(maxReg-minReg),maxReg+0.05*(maxReg-minReg)))
          
          # plot 
          titl <- base::paste0(Opt$Fmwk[[idxFmwk]]$NameFmwk,', Overall Repl. Sensor Regression: ',Opt$NameSite, ', ',rpt[[idxTypeGf]]$nameGf,', Run ',idxPerm)
          
          plotReg <- plotly::plot_ly(data=dataRep,x=~idep) %>%
            plotly::add_markers(y=~dep,marker = base::list(color = 'rgba(30,30,30,1)', size = 4),name='data') %>%
            plotly::add_lines(data=lineReg,x=~idep,y=~dep,line = base::list(color = 'rgba(200,30,30,1)', width = 2),
                              name='Regression') %>%
            plotly::add_lines(data=line0101,x=~idep,y=~dep,line = base::list(color = 'rgba(30,30,30,1)', width = 1),
                              name='1:1') %>%
            plotly::layout(margin = list(b = 50, t = 50, r=50),
                           title = titl,
                           xaxis = list(title = base::paste0(c(rep("\n&nbsp;", 3),
                                                               rep("&nbsp;", 20),
                                                               paste0(Opt$Fmwk[[idxFmwk]]$NameVarRep[1]),
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
            plotly::export(plotReg,base::paste0(DirPlot,'/',idxFmwk,'_RepReg_',Opt$NameSite,'_',idxTypeGf,'_',idxPerm,'_',Sys.Date(),'.pdf'))
          } else {
            print(plotReg)
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
            plotly::export(plotErrReg,base::paste0(DirPlot,'/',idxFmwk,'_RepErr_',Opt$NameSite,'_',idxTypeGf,'_',idxPerm,'_',Sys.Date(),'.pdf'))
          } else {
            print(plotErrReg)
          }
          
        } # End plotting
      } # End Replicate Sensor gap-filling
      

      ## MDS gap-filling ##
      if(!is.null(Opt$Fmwk[[idxFmwk]]$NameVarMds)){
        
        # Prepare data frame for MDS gap-filling
        dataMdsIn <- dataGf[c('TIMESTAMP_START',Opt$NameVarGf,Opt$Fmwk[[idxFmwk]]$NameVarMds)] 
        names(dataMdsIn)[1] <- 'sDateTime'
        
        # Are there any SW_IN variables? Change them to Rg so that the MDS code will treat it appropriately
        names(dataMdsIn) <- gsub(pattern='SW_IN',replacement='Rg',x=names(dataMdsIn))
        NameVarMds<-Opt$Fmwk[[idxFmwk]]$NameVarMds
        NameVarMds <- gsub(pattern='SW_IN',replacement='Rg',x=NameVarMds)

        # Run the MDS according to how many vars are used to create the lookup table
        if (length(NameVarMds) == 1){
          dataMdsOut <- sMDSGapFill(sDATA=dataMdsIn,Var.s=Opt$NameVarGf, V1.s=NameVarMds[1],T1.n=Opt$Fmwk[[idxFmwk]]$TolVarMds[1],FillAll.b = FALSE)
        } else if (length(NameVarMds) == 2){
          dataMdsOut <- sMDSGapFill(sDATA=dataMdsIn,Var.s=Opt$NameVarGf, V1.s=NameVarMds[1],T1.n=Opt$Fmwk[[idxFmwk]]$TolVarMds[1],V2.s=NameVarMds[2],
                                    T2.n=Opt$Fmwk[[idxFmwk]]$TolVarMds[2],FillAll.b = FALSE)
        } else if (length(NameVarMds) == 3){
          dataMdsOut <- sMDSGapFill(sDATA=dataMdsIn,Var.s=Opt$NameVarGf, V1.s=NameVarMds[1],T1.n=Opt$Fmwk[[idxFmwk]]$TolVarMds[1],V2.s=NameVarMds[2],
                                    T2.n=Opt$Fmwk[[idxFmwk]]$TolVarMds[2],V3.s=NameVarMds[3],T3.n=Opt$Fmwk[[idxFmwk]]$TolVarMds[3],FillAll.b = FALSE)
        } else {
          stop('Code is not yet set up to run more than 3 MDS predictor variables')
        }
        
        names(dataMdsOut)[1] <- 'DateTime' # Add timestamp
        
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
        dataStat <- data.frame(varGfMeas=data[setGf,Opt$NameVarGf],varGfPred=varGfMeth[setGf])
        dataStat$err <- dataStat$varGfPred-dataStat$varGfMeas # Error
        dataStat$meth <- dataMdsOut[setGf,base::paste0(Opt$NameVarGf,'_fmeth')]
        dataStat$wndw <- dataMdsOut[setGf,base::paste0(Opt$NameVarGf,'_fwin')]
        dataStat <- dataStat[!is.na(dataStat$varGfPred) & !is.na(dataStat$varGfMeas),] # Keep only pairwise complete observations
        
        for(idxMethMds in 1:3){
          idxMeth <- paste0('mdsMeth0',idxMethMds)
          dataStatIdx <- dataStat[dataStat$meth==idxMethMds,]
          rpt[[idxTypeGf]]$statMeth[[idxFmwk]]$numGf[[idxMeth]][idxPerm] <- nrow(dataStatIdx) # aritifical gaps able to be filled - ADJUST ME FOR METHOD
          rpt[[idxTypeGf]]$statMeth[[idxFmwk]]$rsq[[idxMeth]][idxPerm] <- stats::cor(dataStatIdx$varGfPred,dataStatIdx$varGfMeas)^2
          rpt[[idxTypeGf]]$statMeth[[idxFmwk]]$rmseAbs[[idxMeth]][idxPerm] <- sqrt((1/rpt[[idxTypeGf]]$statMeth[[idxFmwk]]$numGf[[idxMeth]][idxPerm])*sum((dataStatIdx$err)^2))
          rpt[[idxTypeGf]]$statMeth[[idxFmwk]]$rmseRltv[[idxMeth]][idxPerm] <- sqrt(sum((dataStatIdx$err)^2)/sum(dataStatIdx$varGfMeas^2))
          rpt[[idxTypeGf]]$statMeth[[idxFmwk]]$mae[[idxMeth]][idxPerm] <- (1/rpt[[idxTypeGf]]$statMeth[[idxFmwk]]$numGf[[idxMeth]][idxPerm])*sum(abs(dataStatIdx$err))
          rpt[[idxTypeGf]]$statMeth[[idxFmwk]]$bias[[idxMeth]][idxPerm] <- (1/rpt[[idxTypeGf]]$statMeth[[idxFmwk]]$numGf[[idxMeth]][idxPerm])*sum(dataStatIdx$err) # Mean error
          rpt[[idxTypeGf]]$statMeth[[idxFmwk]]$sd[[idxMeth]][idxPerm] <- stats::sd(dataStatIdx$err) # standard deviation of the error
        }

        # Re-fill the artificial gaps in dataMdsIn with the measured values
        dataMdsIn[setGf,Opt$NameVarGf] <- data[setGf,Opt$NameVarGf]
        
      
        # MDS plotting
        if(Plot){
          print('Plotting...')
          
          # Produce some plots 
          titl <- base::paste0(Opt$Fmwk[[idxFmwk]]$NameFmwk,', MDS: ',Opt$NameSite, ', ',rpt[[idxTypeGf]]$nameGf,', Run ',idxPerm)
          
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
            plotly::export(plotMdsAll,base::paste0(DirPlot,'/',idxFmwk,'_MDS_',Opt$NameSite,'_',idxTypeGf,'_',idxPerm,'_',Sys.Date(),'.pdf'))
          } else {
            print(plotMdsAll)
          }
          
          
          
          # Also plot the 3 predictor variables. Highlight the points where there are gaps in the filled dataset.
          plotVar01 <- plotly::plot_ly(data=dataMdsIn, x=~sDateTime, y=dataMdsIn[[Opt$Fmwk[[idxFmwk]]$NameVarMds[1]]], type='scatter', 
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
                           yaxis = list(title = Opt$Fmwk[[idxFmwk]]$NameVarMds[1],
                                        zeroline=FALSE))
          
          plotVar02 <- plotly::plot_ly(data=dataMdsIn, x=~sDateTime, y=dataMdsIn[[Opt$Fmwk[[idxFmwk]]$NameVarMds[2]]], type='scatter', 
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
                           yaxis = list(title = Opt$Fmwk[[idxFmwk]]$NameVarMds[2],
                                        zeroline=FALSE))
          
          plotVar03 <- plotly::plot_ly(data=dataMdsIn, x=~sDateTime, y=dataMdsIn[[Opt$Fmwk[[idxFmwk]]$NameVarMds[3]]], type='scatter', 
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
                           yaxis = list(title = Opt$Fmwk[[idxFmwk]]$NameVarMds[3],
                                        zeroline=FALSE))
          
          # Combine the plots into a single graphic
          plotVarAll <- plotly::subplot(plotSetGf,plotVar01,plotVar02,plotVar03,nrows=4,heights=c(0.1,0.3,0.3,0.3),titleY=TRUE,shareX=TRUE)
          
          # Save plot to file, or print to screen
          if(PlotSave){
            plotly::export(plotVarAll,base::paste0(DirPlot,'/',idxFmwk,'_MDSpvar_',Opt$NameSite,'_',idxTypeGf,'_',idxPerm,'_',Sys.Date(),'.pdf'))
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
            plotly::export(plotErrMdsAll,base::paste0(DirPlot,'/',idxFmwk,'_MdsErr_',Opt$NameSite,'_',idxTypeGf,'_',idxPerm,'_',Sys.Date(),'.pdf'))
          } else {
            print(plotErrMdsAll)
          }
          
          # Write to screen the number of observations in which predictor variables coincide with a gap in the gap-filled dataset, 
          # and how many observations have all predictor & gap-filled variable availability
          print(base::paste0('N data: ',nrow(dataMdsIn)))
          print(base::paste0('N of all-predictor availability coinciding with target gap: ',
                             sum(apply(X=cbind(!is.na(dataMdsIn[,!(names(dataMdsIn) %in% c('sDateTime',Opt$NameVarGf))]),
                                               is.na(dataMdsIn[,Opt$NameVarGf])),
                                       MARGIN=1,FUN=sum)==length(Opt$Fmwk[[idxFmwk]]$NameVarMds)+1)))
          print(base::paste0('N of all predictor & target variable availability: ',
                             sum(apply(X=!is.na(dataMdsIn[,!(names(dataMdsIn) %in% 'sDateTime')]),MARGIN=1,FUN=sum)==length(Opt$Fmwk[[idxFmwk]]$NameVarMds)+1)))
          
        } # End MDS plotting
      } # End MDS gap-filling
      
      # From the different methods, choose the one with the best performance for each gap.
      # WOULD BE GREAT TO GENERALIZE THIS
      # Chose replicate sensor over MDS
      if(!is.null(Opt$Fmwk[[idxFmwk]]$NameVarRep) && !is.null(Opt$Fmwk[[idxFmwk]]$NameVarMds)){
        # First choose either replicate sensor, MDS (MDS method 1/2), or MDC = 1 day window (MDS method 3; linear interp), comparing the 
        # se of the prediction with the sd of the data points used for gap-filling. Then use MDC (any window) for the remaining gaps.
        setEval <- which(!is.na(dataRepReg$rsqAdj) & (((dataMdsOut[[base::paste0(Opt$NameVarGf,'_fmeth')]] == 1 | 
                                                          dataMdsOut[[base::paste0(Opt$NameVarGf,'_fmeth')]] == 2) & 
                                                         dataMdsOut[[base::paste0(Opt$NameVarGf,'_fnum')]] >= 10) | 
                                                        (dataMdsOut[[base::paste0(Opt$NameVarGf,'_fmeth')]] == 3 & 
                                                           dataMdsOut[[base::paste0(Opt$NameVarGf,'_fwin')]] == 1)))
        sd <- as.matrix(data.frame(Rep=dataRepReg$se[setEval],Mds=dataMdsOut[setEval,base::paste0(Opt$NameVarGf,'_fsd')])) # Pull the standard deviations for each method
        meth <- apply(X=sd,MARGIN=1,FUN=which.min) # Which method has the lowest standard deviation. 1 = Replicate sensor, 2=MDS/MDC
        
        # Fill gaps with replicate sensor regression
        varGf[setEval[meth==1]] <- dataRepReg$pred[setEval[meth==1]] # Fill the gaps
        setGfRep <- intersect(setGf,setEval[meth==1]) # indices of artifical gaps filled with replicate sensor regression
        setGapRep <- intersect(setGap,setEval[meth==1]) # indices of original gaps filled with replicate sensor regression
        rpt[[idxTypeGf]]$numGfMeth[[idxFmwk]][idxPerm,'rep'] <- length(setGfRep) #  - save it.
        print(paste0(length(setGfRep),' artificial gaps actually filled with replicate sensor regression (these may overlap with original gaps)'))
        print(paste0(length(setGapRep),' original gaps actually filled with replicate sensor regression (these may overlap with original gaps)'))
        
        # Fill remaining gaps with MDS
        setGfNext <- setdiff(setGf,setGfRep)
        setGapNext <- setdiff(setGap,setGapRep)
        varGf[union(setGfNext,setGapNext)]  <- dataMdsOut[union(setGfNext,setGapNext),base::paste0(Opt$NameVarGf,'_f')] # Fill the gaps
        setGfMds <- intersect(setGfNext,which(!is.na(varGf))) # indices of artifical gaps filled with MDS/MDC
        setGapMds <- intersect(setGapNext,which(!is.na(varGf))) # indices of original gaps filled with MDS/MDC
        rpt[[idxTypeGf]]$numGfMeth[[idxFmwk]][idxPerm,'mds'] <- length(setGfMds) #  - save it.
        print(paste0(length(setGfMds),' artificial gaps actually filled with MDS/MDC (these may overlap with original gaps)'))
        print(paste0(length(setGapMds),' original gaps actually filled with MDS/MDC (these may overlap with original gaps)'))
        
      } else if(!is.null(Opt$Fmwk[[idxFmwk]]$NameVarMds)){
        # Fill gaps with MDS
        varGf[union(setGf,setGap)]  <- dataMdsOut[union(setGf,setGap),base::paste0(Opt$NameVarGf,'_f')] # Fill the gaps
        setGfMds <- intersect(setGf,which(!is.na(varGf))) # indices of artifical gaps filled with replicate sensor regression
        setGapMds <- intersect(setGap,which(!is.na(varGf))) # indices of original gaps filled with MDS/MDC
        rpt[[idxTypeGf]]$numGfMeth[[idxFmwk]][idxPerm,'mds'] <- length(setGfMds) #  - save it.
        print(paste0(length(setGfMds),' artificial gaps actually filled with MDS/MDC (these may overlap with original gaps)'))
        print(paste0(length(setGapMds),' original gaps actually filled with MDS/MDC (these may overlap with original gaps)'))
      }
      
      
      
      # Calculate performance stats of the entire gap-filling framework
      dataStat <- data.frame(varGfMeas=data[setGf,Opt$NameVarGf],varGfPred=varGf[setGf])
      dataStat <- dataStat[!is.na(dataStat$varGfPred) & !is.na(dataStat$varGfMeas),] # Keep only pairwise complete observations
      rpt[[idxTypeGf]]$statFmwk[[idxFmwk]]$numGf[idxPerm] <- nrow(dataStat)
      rpt[[idxTypeGf]]$statFmwk[[idxFmwk]]$rsq[idxPerm] <- stats::cor(dataStat$varGfPred,dataStat$varGfMeas)^2
      rpt[[idxTypeGf]]$statFmwk[[idxFmwk]]$rmseAbs[idxPerm] <- sqrt((1/rpt[[idxTypeGf]]$statFmwk[[idxFmwk]]$numGf[idxPerm])*sum((dataStat$varGfPred-dataStat$varGfMeas)^2))
      rpt[[idxTypeGf]]$statFmwk[[idxFmwk]]$rmseRltv[idxPerm] <- sqrt(sum((dataStat$varGfPred-dataStat$varGfMeas)^2)/sum(dataStat$varGfMeas^2))
      rpt[[idxTypeGf]]$statFmwk[[idxFmwk]]$mae[idxPerm] <- (1/rpt[[idxTypeGf]]$statFmwk[[idxFmwk]]$numGf[idxPerm])*sum(abs(dataStat$varGfPred-dataStat$varGfMeas))
      rpt[[idxTypeGf]]$statFmwk[[idxFmwk]]$bias[idxPerm] <- (1/rpt[[idxTypeGf]]$statFmwk[[idxFmwk]]$numGf[idxPerm])*sum(dataStat$varGfPred-dataStat$varGfMeas)
      
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
        titl <- base::paste0(Opt$Fmwk[[idxFmwk]]$NameFmwk,' overall performance: ',Opt$NameSite, ', ',rpt[[idxTypeGf]]$nameGf,', Run ',idxPerm)
        
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
          plotly::export(plotPfmc,base::paste0(DirPlot,'/',idxFmwk,'_Pfmc_',Opt$NameSite,'_',idxTypeGf,'_',idxPerm,'_',Sys.Date(),'.pdf'))
        } else {
          print(plotPfmc)
        }
      } # End overall performance plotting
      
    } # End loop around permutations
  } # End loop around gap-filling scenarios
} # End loop around frameworks

stop('Done')
####### Final Plotting #########
# Compare the performance of the different frameworks
if(Plot & length(opt$Fmwk) > 1){ 
  # Plot box plots with the metrics
  statAll <- lapply(names(rpt),FUN=function(idxTypeGf){rpt[[idxTypeGf]]$statFmwk})
  names(statAll)<-names(rpt)
  statAll <- reshape2::melt(statAll)
  names(statAll) <- c('stat','valu','fmwk','typeGf')
  
  titl=base::paste0(Opt$NameSite, ', ',Opt$NameVarGf,' gap-filling performance')
                     
  # Number of points
  statPlot <- statAll[statAll$stat=='numGf',c('valu','fmwk','typeGf')]
  plotBoxRsq <- plotly::plot_ly(statPlot,x=~typeGf,y=~valu,color=~fmwk,type='box') %>%
    plotly::layout(boxmode='group',
                   yaxis=list(title='# Artificial Gaps'),
                   xaxis=list(title='Gap type',
                              categoryorder='array',
                              categoryarray=c('VS','S','M','L','VL','X')),
                   title=titl
    )
  if(PlotSave){
    plotly::export(plotBoxRsq,base::paste0(DirPlot,'/','PfmcAll_numGf_',Opt$NameSite,'_',Opt$NameVarGf,'_',Sys.Date(),'.pdf'))
  } else {
    print(plotBoxRsq)
  }
  
  # R-squared
  statPlot <- statAll[statAll$stat=='rsq',c('valu','fmwk','typeGf')]
  plotBoxRsq <- plotly::plot_ly(statPlot,x=~typeGf,y=~valu,color=~fmwk,type='box') %>%
    plotly::layout(boxmode='group',
                   yaxis=list(title='R-squared'),
                   xaxis=list(title='Gap type',
                              categoryorder='array',
                              categoryarray=c('VS','S','M','L','VL','X')),
                    title=titl
                   )
  if(PlotSave){
    plotly::export(plotBoxRsq,base::paste0(DirPlot,'/','PfmcAll_rsq_',Opt$NameSite,'_',Opt$NameVarGf,'_',Sys.Date(),'.pdf'))
  } else {
    print(plotBoxRsq)
  }
  
  # RMSE (absolute)
  statPlot <- statAll[statAll$stat=='rmseAbs',c('valu','fmwk','typeGf')]
  plotBoxRmseAbs <- plotly::plot_ly(statPlot,x=~typeGf,y=~valu,color=~fmwk,type='box') %>%
    plotly::layout(boxmode='group',
                   yaxis=list(title=base::paste0('RMSE [',Opt$UnitVarGf,']')),
                   xaxis=list(title='Gap type',
                              categoryorder='array',
                              categoryarray=c('VS','S','M','L','VL','X')),
                   title=titl
    )
  if(PlotSave){
    plotly::export(plotBoxRmseAbs,base::paste0(DirPlot,'/','PfmcAll_rmseAbs_',Opt$NameSite,'_',Opt$NameVarGf,'_',Sys.Date(),'.pdf'))
  } else {
    print(plotBoxRmseAbs)
  }  
 
  # RMSE (relative)
  statPlot <- statAll[statAll$stat=='rmseRltv',c('valu','fmwk','typeGf')]
  plotBoxRmseRltv <- plotly::plot_ly(statPlot,x=~typeGf,y=~valu,color=~fmwk,type='box') %>%
    plotly::layout(boxmode='group',
                   yaxis=list(title=base::paste0('relative RMSE')),
                   xaxis=list(title='Gap type',
                              categoryorder='array',
                              categoryarray=c('VS','S','M','L','VL','X')),
                   title=titl
    )
  if(PlotSave){
    plotly::export(plotBoxRmseRltv,base::paste0(DirPlot,'/','PfmcAll_rmseRltv_',Opt$NameSite,'_',Opt$NameVarGf,'_',Sys.Date(),'.pdf'))
  } else {
    print(plotBoxRmseRltv)
  }   
  
  # MAE
  statPlot <- statAll[statAll$stat=='mae',c('valu','fmwk','typeGf')]
  plotBoxMae <- plotly::plot_ly(statPlot,x=~typeGf,y=~valu,color=~fmwk,type='box') %>%
    plotly::layout(boxmode='group',
                   yaxis=list(title=base::paste0('MAE [',Opt$UnitVarGf,']')),
                   xaxis=list(title='Gap type',
                              categoryorder='array',
                              categoryarray=c('VS','S','M','L','VL','X')),
                   title=titl
    )
  if(PlotSave){
    plotly::export(plotBoxMae,base::paste0(DirPlot,'/','PfmcAll_mae_',Opt$NameSite,'_',Opt$NameVarGf,'_',Sys.Date(),'.pdf'))
  } else {
    print(plotBoxMae)
  }  
  
  # Bias
  statPlot <- statAll[statAll$stat=='bias',c('valu','fmwk','typeGf')]
  plotBoxBias <- plotly::plot_ly(statPlot,x=~typeGf,y=~valu,color=~fmwk,type='box') %>%
    plotly::layout(boxmode='group',
                   yaxis=list(title=base::paste0('Bias [',Opt$UnitVarGf,']')),
                   xaxis=list(title='Gap type',
                              categoryorder='array',
                              categoryarray=c('VS','S','M','L','VL','X')),
                   title=titl
    )
  if(PlotSave){
    plotly::export(plotBoxBias,base::paste0(DirPlot,'/','PfmcAll_bias_',Opt$NameSite,'_',Opt$NameVarGf,'_',Sys.Date(),'.pdf'))
  } else {
    print(plotBoxBias)
  }  
  
  # Proportion of samples filled with each method - THIS NEEDS WORK
  rtioNumGf <- list()
  for (idxFmwk in names(Opt$Fmwk)){
    rtioNumGf[[idxFmwk]] <- lapply(rpt,FUN=function(rpt){
      if(rpt$nameGf != "Mixed-length gaps"){
        rtioNumGf <- rpt$numGfMeth[[idxFmwk]]/rpt$statFmwk[[idxFmwk]]$numGf
      } else {
        rtioNumGf <- rpt$numGfMeth[[idxFmwk]]/rpt$statFmwk[[idxFmwk]]$numGf
      }
    })
  }
  
  # Plot the gap-filled data, the replicate sensor data, and the MDS data - WORK ON THIS > NEST WITHIN EACH FRAMEWORK
  titl <- paste0('All variables used in ', Opt$Fmwk[[idxFmwk]]$NameFmwk,' gap-filling')
  plotDataGf <- plotly::plot_ly(data=data, x=~TIMESTAMP_START, y=data[[Opt$NameVarGf]], type='scatter', 
                              mode='lines', line = base::list(color = 'rgba(30,30,30,1)', width = 1),
                              name=Opt$NameVarGf) %>%
    plotly::layout(margin = list(b = 50, t = 50, r=50),
                   title = titl,
                   xaxis = list(title = base::paste0(c(rep("\n&nbsp;", 3),
                                                       rep("&nbsp;", 20),
                                                       paste0("Date-Time [",base::attr(data$TIMESTAMP_START,'tzone'),']'),
                                                       rep("&nbsp;", 20)),
                                                     collapse = ""),
                                nticks=6,
                                range = data$TIMESTAMP_START[c(1,base::length(data$TIMESTAMP_START))],
                                zeroline=FALSE
                   ),
                   yaxis = list(title = Opt$NameVarGf,
                                zeroline=FALSE),
                   showlegend=FALSE)
  
  plotDataRep <- plotly::plot_ly(data=data, x=~TIMESTAMP_START, y=data[[Opt$Fmwk[[2]]$NameVarRep[1]]], type='scatter', 
                                mode='lines', line = base::list(color = 'rgba(30,30,30,1)', width = 1),
                                name=Opt$Fmwk[[2]]$NameVarRep[1]) %>%
    plotly::layout(margin = list(b = 50, t = 50, r=50),
                   title = titl,
                   xaxis = list(title = base::paste0(c(rep("\n&nbsp;", 3),
                                                       rep("&nbsp;", 20),
                                                       paste0("Date-Time [",base::attr(data$TIMESTAMP_START,'tzone'),']'),
                                                       rep("&nbsp;", 20)),
                                                     collapse = ""),
                                nticks=6,
                                range = data$TIMESTAMP_START[c(1,base::length(data$TIMESTAMP_START))],
                                zeroline=FALSE
                   ),
                   yaxis = list(title = Opt$Fmwk[[2]]$NameVarRep[1],
                                zeroline=FALSE),
                   showlegend=FALSE)
  
  plotDataMds01 <- plotly::plot_ly(data=data, x=~TIMESTAMP_START, y=data[[Opt$Fmwk[[1]]$NameVarMds[1]]], type='scatter', 
                                 mode='lines', line = base::list(color = 'rgba(30,30,30,1)', width = 1),
                                 name=Opt$Fmwk[[1]]$NameVarMds[1]) %>%
    plotly::layout(margin = list(b = 50, t = 50, r=50),
                   title = titl,
                   xaxis = list(title = base::paste0(c(rep("\n&nbsp;", 3),
                                                       rep("&nbsp;", 20),
                                                       paste0("Date-Time [",base::attr(data$TIMESTAMP_START,'tzone'),']'),
                                                       rep("&nbsp;", 20)),
                                                     collapse = ""),
                                nticks=6,
                                range = data$TIMESTAMP_START[c(1,base::length(data$TIMESTAMP_START))],
                                zeroline=FALSE
                   ),
                   yaxis = list(title = Opt$Fmwk[[1]]$NameVarMds[1],
                                zeroline=FALSE),
                   showlegend=FALSE)
  
  plotDataMds02 <- plotly::plot_ly(data=data, x=~TIMESTAMP_START, y=data[[Opt$Fmwk[[1]]$NameVarMds[2]]], type='scatter', 
                                   mode='lines', line = base::list(color = 'rgba(30,30,30,1)', width = 1),
                                   name=Opt$Fmwk[[1]]$NameVarMds[2]) %>%
    plotly::layout(margin = list(b = 50, t = 50, r=50),
                   title = titl,
                   xaxis = list(title = base::paste0(c(rep("\n&nbsp;", 3),
                                                       rep("&nbsp;", 20),
                                                       paste0("Date-Time [",base::attr(data$TIMESTAMP_START,'tzone'),']'),
                                                       rep("&nbsp;", 20)),
                                                     collapse = ""),
                                nticks=6,
                                range = data$TIMESTAMP_START[c(1,base::length(data$TIMESTAMP_START))],
                                zeroline=FALSE
                   ),
                   yaxis = list(title = Opt$Fmwk[[1]]$NameVarMds[2],
                                zeroline=FALSE),
                   showlegend=FALSE)
  
  plotDataMds03 <- plotly::plot_ly(data=data, x=~TIMESTAMP_START, y=data[[Opt$Fmwk[[1]]$NameVarMds[3]]], type='scatter', 
                                   mode='lines', line = base::list(color = 'rgba(30,30,30,1)', width = 1),
                                   name=Opt$Fmwk[[1]]$NameVarMds[3]) %>%
    plotly::layout(margin = list(b = 50, t = 50, r=50),
                   title = titl,
                   xaxis = list(title = base::paste0(c(rep("\n&nbsp;", 3),
                                                       rep("&nbsp;", 20),
                                                       paste0("Date-Time [",base::attr(data$TIMESTAMP_START,'tzone'),']'),
                                                       rep("&nbsp;", 20)),
                                                     collapse = ""),
                                nticks=6,
                                range = data$TIMESTAMP_START[c(1,base::length(data$TIMESTAMP_START))],
                                zeroline=FALSE
                   ),
                   yaxis = list(title = Opt$Fmwk[[1]]$NameVarMds[3],
                                zeroline=FALSE),
                   showlegend=FALSE)
  
  # Combine the plots into a single graphic
  plotVarAll <- plotly::subplot(plotDataGf,plotDataRep,plotDataMds01,plotDataMds02,plotDataMds03,nrows=5,heights=c(0.2,0.2,0.2,0.2,0.2),titleY=TRUE,shareX=TRUE)
  
  # Save plot to file, or print to screen
  if(PlotSave){
    plotly::export(plotVarAll,base::paste0(DirPlot,'/',idxFmwk,'_varAll_',Opt$NameSite,'_',Sys.Date(),'.pdf'))
  } else {
    print(plotVarAll)
  }
  
  
}

# Plot the error statistics for each of the methods within each framework, as if each were used to fill each gap
#for (idxFmwk in names(Opt$Fmwk)){
  idxFmwk <- names(Opt$Fmwk)[1] # Let's just do the first framework for now
  
  # Make a box plot for each size, showing the bias and sd (mean and standard deviation of the error distribution)

    # Bias
    statAll <- lapply(names(rpt),FUN=function(idxTypeGf){rpt[[idxTypeGf]]$statMeth[[idxFmwk]]$bias})
    names(statAll)<-names(rpt)
    statAll <- reshape2::melt(statAll)
    statAll <- statAll[c('variable','value','L1')]
    names(statAll) <- c('meth','valu','typeGf')
    
    titl=base::paste0(Opt$NameSite, ', ',Opt$NameVarGf,': Bias comparison among gap-filling methods')
    
    plotBoxBias <- plotly::plot_ly(statAll,x=~typeGf,y=~valu,color=~meth,type='box') %>%
      plotly::layout(boxmode='group',
                     yaxis=list(title=base::paste0('Bias [',Opt$UnitVarGf,']')),
                     xaxis=list(title='Gap type',
                                categoryorder='array',
                                categoryarray=c('VS','S','M','L','VL','X')),
                     title=titl
      )
    
    if(PlotSave){
      plotly::export(plotBoxBias,base::paste0(DirPlot,'/','PfmcMeth_bias_',Opt$NameSite,'_',Opt$NameVarGf,'_',Sys.Date(),'.pdf'))
    } else {
      print(plotBoxBias)
    }  

  
#}




# Save the output stats and input options
if(RptSave){
  
  save(Opt,rpt,file=base::paste0(DirRpt,'/rpt_',Opt$NameSite,'_',Opt$NameVarGf,'_',Sys.Date(),'.RData'))
  
}