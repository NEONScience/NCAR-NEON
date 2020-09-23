##############################################################################################
#' @title Workflow to NCAR CLM data set

#' @author
#' David Durden \email{eddy4R.info@gmail.com}

#' @description 
#' Workflow for collating NEON data from API, gap-filling, and packaging in NCAR CLM netcdf format.

# changelog and author contributions / copyrights
# David Durden (2019-07-05)
#   original creation
# David Durden (2020-05-31)
# Updating to use neonUtilities for all data retrieval from API
##############################################################################################

#############################################################
#Dependencies
#############################################################


#Call the R HDF5 Library
packReq <- c("rhdf5", "REddyProc", "ncdf4", "devtools")

#Install and load all required packages
lapply(packReq, function(x) {
  print(x)
  if(require(x, character.only = TRUE) == FALSE) {
    install.packages(x)
    library(x, character.only = TRUE)
  }})

#Install packages from github repos
devtools::install_github(c("NEONScience/eddy4R/pack/eddy4R.base", "NEONScience/NEON-utilities/neonUtilities"))

#Setup Environment
options(stringsAsFactors=F)


#############################################################
#Workflow parameters
#############################################################
#Which NEON site are we grabbing data from (4-letter ID)
Site <- "NIWO"
#Which type of data package (expanded or basic)
Pack <- "basic"
#Time averaging period
TimeAgr <- 30
#Beginning date for data grabbing
dateBgn <- "2018-01-01"

#End date for date grabbing
dateEnd <- "2018-12-31"

#The version data for the FP standard conversion processing
ver <- paste0("v",format(Sys.time(), "%Y%m%dT%H%m"))
#Base directory for output
DirOutBase <-paste0("~/Users/wwieder/Desktop/Working_files/NEON/NCAR_NEON/NEONforcing/",ver)
#Download directory for HDF5 files from the API
DirDnld=tempdir()

# check environment variables for eddy4R workflow, if it exists grab ENV variables 
if("METHPARAFLOW" %in% base::names(base::Sys.getenv())) {
  Site <- Sys.getenv("SITE")
  dateBgn <- Sys.getenv("DATEBGN") 
  dateEnd <- Sys.getenv("DATEEND")
  DirOutBase <- Sys.getenv("DIROUT")
}




#############################################################
#static workflow parameters
#############################################################

#H5 extraction directory
DirExtr <- paste0(DirDnld,"/extr")
#Append the site to the base output directory
DirOut <- paste0(DirOutBase, "/", Site)

#Check if directory exists and create if not
if(!dir.exists(DirOut)) dir.create(DirOut, recursive = TRUE)

#DP number
idDpFlux <- 'DP4.00200.001'

#Set dates for pulling data from API
dateBgn <- as.Date(dateBgn) - lubridate::days(1) #neonUtitilities a month behind
dateEnd <- as.Date(dateEnd)

##############################################################################
#Flux data download
##############################################################################

#Download zip files
neonUtilities::zipsByProduct(dpID=idDpFlux, package=Pack, 
                             site=Site, 
                             startdate= as.character(dateBgn),
                             enddate= as.character(dateEnd),
                             savepath= DirDnld, 
                             check.size=F)

#Grab one zip file for the site to extract metadata and unzip
zipFile <- base::list.files(paste0(DirDnld,"/filesToStack00200"), pattern = ".zip", full.names = TRUE)[1]
utils::unzip(zipFile, exdir = DirExtr)

#Get HDF5 filename
fileNameHdf5 <- base::list.files(path = DirExtr, pattern = "*.h5", full.names = TRUE)

##############################################################################
#Metadata determination
##############################################################################

#Read site level metadata from HDF5
metaSite <- rhdf5::h5readAttributes(file = fileNameHdf5, name = Site)
#Grab latitude and longitude from site metadata
latSite <- metaSite$LatTow #Latitude of tower
lonSite <- metaSite$LonTow #Longitude of tower
distTowSite <- metaSite$DistZaxsTow #Tower height
#Tower top level in NEON DP number convention
IdHor <- "000"
IdVer <-paste0("0",metaSite$LvlMeasTow,"0")
LvlTowr <- paste0(IdHor,IdVer)

# time difference between local time and UTC
if(!base::is.null(metaSite$ZoneTime)) {
  
  # start date and time of dataset in UTC
  timeTmp01 <- base::as.POSIXlt(x = base::paste0(dateBgn, "T00:00:00Z"), format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC")
  timeTmp02 <- timeTmp01
  
  # assign local timezone attribute, if available in database
  if(metaSite$ZoneTime %in% base::OlsonNames()) {
    
    attributes(timeTmp02)$tzone <- metaSite$ZoneTime
    
  } else {
    
    base::warning(base::paste("Time zone attribute", metaSite$ZoneTime,
                              "not available in R base::OlsonNames() database. Continue with local time equals UTC time."))
    
  }}
  
  # time difference between local time and UTC
  metaSite$TimeDiffUtcLst <- base::as.numeric(base::difftime(timeTmp01, timeTmp02, units = "hours"))
  
  # clean up
  rm(timeTmp01, timeTmp02)

##############################################################################
#Flux data read in
##############################################################################
  
#Initialize data List
dataList <- list()

#Read data from downloaded zip files
dataList$dp04 <- neonUtilities::stackEddy(filepath=paste0(DirDnld,"/filesToStack00200/"), level = "dp04", avg = 30)
dataList$dp01 <- neonUtilities::stackEddy(filepath=paste0(DirDnld,"/filesToStack00200/"), level = "dp01", avg = 30)  

#Create flux data.frame
dataDfFlux <-   data.frame(
    "TIMESTAMP_START" = as.POSIXlt(dataList$dp04[[Site]]$timeBgn, format="%Y-%m-%dT%H:%M:%OSZ", tz = "GMT"), #Timestamp represents end of period in ReddyProc
    "TIMESTAMP_END" = as.POSIXlt(dataList$dp04[[Site]]$timeEnd, format="%Y-%m-%dT%H:%M:%OSZ", tz = "GMT"),
    "NEE"= dataList$dp04[[Site]]$data.fluxCo2.nsae.flux,#Net ecosystem exchange (turb + stor)
    "LE" = dataList$dp04[[Site]]$data.fluxH2o.turb.flux, #Latent heat flux (turb)
    "Ustar" = dataList$dp04[[Site]]$data.fluxMome.turb.veloFric, #Friction velocity
    "H" = dataList$dp04[[Site]]$data.fluxTemp.turb.flux,#Sensible heat flux (turb)
    "qfTurbFlow" = dataList$dp01[[Site]][which(dataList$dp01[[Site]]$verticalPosition == IdVer), "qfqm.h2oTurb.frt00Samp.qfFinl"],
    "qfTurbH2oFinl" = dataList$dp01[[Site]][which(dataList$dp01[[Site]]$verticalPosition == IdVer), "qfqm.h2oTurb.rtioMoleDryH2o.qfFinl"],
    "qfTurbCo2Finl" = dataList$dp01[[Site]][which(dataList$dp01[[Site]]$verticalPosition == IdVer), "qfqm.co2Turb.rtioMoleDryCo2.qfFinl"],
    "WS_MDS" = dataList$dp01[[Site]][which(dataList$dp01[[Site]]$verticalPosition == IdVer), "data.soni.veloXaxsYaxsErth.mean"],
    #"Pa_MDS" = dataList[[x]][[Site]]$dp01$data$h2oTurb[[paste0(LvlTowr,"_30m")]]$presAtm$mean,
    "Tair" = dataList$dp01[[Site]][which(dataList$dp01[[Site]]$verticalPosition == IdVer), "data.soni.tempAir.mean"]
    , stringsAsFactors = FALSE)

###################################################################################
#Time regularization if needed
# Regularize timeseries to 30 minutes in case missing data after CI processing
timeRglr <- eddy4R.base::def.rglr(timeMeas = as.POSIXlt(dataDfFlux$TIMESTAMP_START), dataMeas = dataDfFlux, BgnRglr = as.POSIXlt(dataDfFlux$TIMESTAMP_START[1]), EndRglr = as.POSIXlt(dataDfFlux$TIMESTAMP_END[length(dataDfFlux$TIMESTAMP_END)]), TzRglr = "UTC", FreqRglr = 1/(60*30))
# 
# #Reassign data to data.frame
dataDfFlux <- timeRglr$dataRglr
# 
# 
# #Format timestamps
dataDfFlux$TIMESTAMP_START <- NULL
dataDfFlux$TIMESTAMP_END <- NULL
dataDfFlux$TIMESTAMP <- timeRglr$timeRglr+ lubridate::minutes(30)
 # dataDfFlux$TIMESTAMP_START <- strftime(timeRglr$timeRglr , format = "%Y%m%d%H%M")
 # dataDfFlux$TIMESTAMP_END <- strftime(timeRglr$timeRglr+ lubridate::minutes(30), format = "%Y%m%d%H%M")
###################################################################################

dataDfFlux$NEE[(which(dataDfFlux$qfTurbFlow == 1))] <- NaN
dataDfFlux$LE[(which(dataDfFlux$qfTurbFlow == 1))] <- NaN
#dataDfFlux$Pa_MDS[(which(dataDfFlux$qfTurbFlow == 1))] <- NaN


#Remove flagging variables from output
dataDfFlux$qfTurbCo2Finl <- NULL
dataDfFlux$qfTurbH2oFinl <- NULL
dataDfFlux$qfTurbFlow <- NULL



##############################################################################
#Met data
##############################################################################

#Year and month of interest

#varMetaData <- c("lat" = "LatTow", "lon" = "LonTow", "ZBOT" = "DistZaxsTow" )

#varReqData <- c("TBOT" = "tempAir", "FLDS" =, "FSDS" =, "PRECTmms" = , "PSRF", "RH")
#Convert Level Tower to Met format
#LvlTowrMet <- gsub(pattern = "_", replacement = ".", x = LvlTowr)


#List of DP numbers by eddy4R DP names
listDpNum <- c( "PRECTmms_MDS" = "DP1.00006.001", "rH" = "DP1.00098.001", "FLDS_MDS" = "DP1.00023.001", "Rg" = "DP1.00023.001", "Pa_MDS" = "DP1.00004.001")

#names for individual variables of interest
varDp <- c("PRECTmms_MDS" = "SECPRE_30min", "rH" = "RH_30min", "FLDS_MDS" = "SLRNR_30min", "Rg" = "SLRNR_30min", "Pa_MDS" = "BP_30min") #Currently using the relative humidity from the soil array, tower top was not reporting data at HARV during this time

subVar <- c("PRECTmms_MDS" = "secPrecipBulk", "rH" = "RHMean", "FLDS_MDS" = "inLWMean", "Rg" = "inSWMean", "Pa_MDS" = "staPresMean")

#Grab data for data products using neonUtilities
#neonUtilities::getPackage(site_code = site, package = pack, year_month =  )

##Grab data for data products using Noble package
dataMet <- lapply(listDpNum, function(x){
  try(expr = neonUtilities::loadByProduct(site = Site, dpID = x, startdate = as.character(dateBgn), enddate = as.character(dateEnd), package = Pack, avg = TimeAgr, check.size = FALSE), silent = TRUE)
  })

#Check if primary precipitation exists at the site, if not change to secondary precip
varDp["PRECTmms_MDS"] <- ifelse(test = any(grepl(pattern = varDp["PRECTmms_MDS"], x = names(dataMet[["PRECTmms_MDS"]]))), "SECPRE_30min", "PRIPRE_30min")

#Failsafe if using primary precip
subVar["PRECTmms_MDS"] <- ifelse(test = varDp["PRECTmms_MDS"] == "SECPRE_30min", "secPrecipBulk", "priPrecipBulk")



#Grab the actual data tables
dataMetSub <- lapply(seq_along(varDp), function(x) {
  tmp <- dataMet[[names(varDp[x])]][[grep(pattern = varDp[[x]], x = names(dataMet[[names(varDp[x])]]))]]
  return(tmp)
})

#Name the output lists
names(dataMetSub) <- names(varDp)

#Remove unwanted measurement levels
dataMetSub$Rg <- dataMetSub$Rg[dataMetSub$Rg$verticalPosition == IdVer,]
dataMetSub$FLDS_MDS <- dataMetSub$FLDS_MDS[dataMetSub$FLDS_MDS$verticalPosition == IdVer,]
dataMetSub$rH <- dataMetSub$rH[dataMetSub$rH$horizontalPosition == "003",]

#time regularization of met data
dataMetSubRglr <- lapply(names(dataMetSub), function(x){
timeRglrMet <- eddy4R.base::def.rglr(timeMeas = as.POSIXlt(dataMetSub[[x]]$startDateTime), dataMeas = dataMetSub[[x]], BgnRglr = dataDfFlux$TIMESTAMP[1] - lubridate::minutes(30), EndRglr = dataDfFlux$TIMESTAMP[length(dataDfFlux$TIMESTAMP)] - lubridate::minutes(30), TzRglr = "UTC", FreqRglr = 1/(60*30))

return(timeRglrMet$dataRglr)
})#End lapply for time regularization of met data

#Add names to list of Dataframes of regularized data
names(dataMetSubRglr) <- names(varDp)

#Grab just the Met data of interest for the forcing data
dataDfMet <- lapply(seq_along(subVar), function(x){
  #print(x)
  #Grab the variables of interest
  tmp <- dataMetSubRglr[[names(subVar[x])]][,grep(pattern = paste0("^",subVar[x]), x = names(dataMetSubRglr[[names(varDp[x])]]))] # use ^ to indicate the pattern starts with the name given
  return(tmp)
})

#Give the ReddyProc names
names(dataDfMet) <- names(varDp)

#Calculate precip rate from bulk
dataDfMet$PRECTmms_MDS <- dataDfMet$PRECTmms_MDS/1800 #1800 sec/0.5 hours

#Calculate net radiation
dataDfMet$radNet <-dataMetSubRglr$Rg[["inSWMean"]] - dataMetSubRglr$Rg[["outSWMean"]] + dataMetSubRglr$Rg[["inLWMean"]] - dataMetSubRglr$Rg[["outLWMean"]]


##############################################################################
#Combine flux and met data
##############################################################################

#Bind data frames together
dataDf <- cbind(dataDfFlux, dataDfMet)

#Change NA to -9999
dataDf[is.na(dataDf)] <- -9999

#Convert time to ReddyProc format
dataDf$Year <- lubridate::year(dataDf$TIMESTAMP) 
dataDf$DoY <- lubridate::yday(dataDf$TIMESTAMP) 
dataDf$Hour <- lubridate::hour(dataDf$TIMESTAMP) + lubridate::minute(dataDf$TIMESTAMP)/60

#Remove timestamp
dataDf$TIMESTAMP <- NULL

#Vector of units for each variable
unitDf <- c("Year" = "--", "DoY" = "--", "Hour" = "--", "NEE" = "umolm-2s-1", "LE" = "Wm-2", "H" = "Wm-2", "Ustar" = "ms-1", "WS_MDS" = "ms-1", "Pa_MDS" = "kPa", "Tair" = "degC", "PRECTmms_MDS" = "mms-1", "rH" = "%", "FLDS_MDS" = "Wm-2", "Rg" = "Wm-2", "radNet" = "Wm-2")

#Set the output data column order based off of the units vector
dataDf <- data.table::setcolorder(dataDf, names(unitDf))

#Create filename
fileOut <- paste0(DirOut,"/",Site,'_',lubridate::date(dataDfFlux$TIMESTAMP[1]),'_',utils::tail(dataDfFlux$TIMESTAMP,n=1),'.txt')


h1 <- paste(names(unitDf), collapse = "\t")
h2 <- paste(unitDf, collapse = "\t")

#Output data in ReddyProc format
conFile <- file(fileOut, "w")
#write the variable names header
writeLines(text = c(h1,h2), sep = "\n", con = conFile)
#write the variable units header
#writeLines(text = unitDf, sep = "\t", con = conFile)
#Write output in tab delimited format
write.table(x = dataDf, file = conFile, sep = "\t", row.names = FALSE, col.names = FALSE)

#Close file connection
close(conFile)

##############################################################################
#ReddyProc workflow
##############################################################################


#  Dir.s <- paste(system.file(package='REddyProc'), 'examples', sep='/')
EddyData.F <- fLoadTXTIntoDataframe(fileOut)

#Threshold bounds to prevent rH > 100%
EddyData.F$rH[EddyData.F$rH > 100] <- 100
#Threshold bounds to prevent Rg < 0
EddyData.F$Rg[EddyData.F$Rg < 0] <- 0
#Threshold bounds to prevent NEE > 100
EddyData.F$NEE[EddyData.F$NEE > 100] <- NA
#Threshold bounds to prevent NEE < -100
EddyData.F$NEE[EddyData.F$NEE < -100] <- NA

#+++ If not provided, calculate VPD from Tair and rH
EddyData.F <- cbind(EddyData.F,VPD=fCalcVPDfromRHandTair(EddyData.F$rH, EddyData.F$Tair))

#+++ Add time stamp in POSIX time format
EddyDataWithPosix.F <- fConvertTimeToPosix(EddyData.F, 'YDH', Year='Year', Day='DoY', Hour='Hour')


#+++ Initalize R5 reference class sEddyProc for processing of eddy data
#+++ with all variables needed for processing later
EddyProc.C <- sEddyProc$new(Site, EddyDataWithPosix.F, c('NEE','Rg','Tair','VPD','rH','LE','H','Ustar','Pa_MDS', 'FLDS_MDS','WS_MDS', 'PRECTmms_MDS', 'radNet'))

#Set location information
EddyProc.C$sSetLocationInfo(LatDeg=latSite, LongDeg=lonSite, TimeZoneHour = metaSite$TimeDiffUtcLst)

#+++ Fill gaps in variables with MDS gap filling algorithm (without prior ustar filtering)
EddyProc.C$sMDSGapFill('NEE', FillAll=TRUE) #Fill all values to estimate flux uncertainties
EddyProc.C$sMDSGapFill('LE', FillAll=TRUE)
EddyProc.C$sMDSGapFill('H', FillAll=TRUE)
EddyProc.C$sMDSGapFill('Ustar', FillAll=TRUE)
EddyProc.C$sMDSGapFill('Tair', FillAll=FALSE)  
EddyProc.C$sMDSGapFill('VPD',    FillAll=FALSE) 
EddyProc.C$sMDSGapFill('rH',     FillAll=FALSE) 
EddyProc.C$sMDSGapFill('WS_MDS', FillAll=FALSE) 
EddyProc.C$sMDSGapFill('PRECTmms_MDS', FillAll=FALSE) 
EddyProc.C$sMDSGapFill('Pa_MDS', FillAll=FALSE) 
EddyProc.C$sMDSGapFill('FLDS_MDS', FillAll=FALSE) 
EddyProc.C$sMDSGapFill('Rg', FillAll=FALSE) 
EddyProc.C$sMDSGapFill('radNet', FillAll=FALSE) 
EddyProc.C$sMRFluxPartition()
#+++ Export gap filled and partitioned data to standard data frame
FilledEddyData.F <- EddyProc.C$sExportResults()

#Grab just the filled data products
dataClm <- FilledEddyData.F[,grep(pattern = "_f$", x = names(FilledEddyData.F))]

#Grab the POSIX timestamp
dataClm$DateTime <- EddyDataWithPosix.F$DateTime - lubridate::minutes(30) # putting back to time at the beginning of the measurement period

names(dataClm) <- c("NEE", "LE", "H", "Ustar", "TBOT", "VPD", "RH", "WIND", "PRECTmms", "PSRF",  "FLDS", "FSDS", "radNet", "GPP", "DateTime")

#Convert degC to K for temperature
dataClm$TBOT <- dataClm$TBOT + 273.15
attributes(obj = dataClm$TBOT)$units <- "K"
#Convert kPa to Pa for pressure
dataClm$PSRF <- dataClm$PSRF * 1000.0
attributes(obj = dataClm$PSRF)$units <- "Pa"

#Create tower height measurement field
dataClm$ZBOT <- rep(distTowSite,nrow(dataClm))

#Year month combination for data filtering
dataClm$yearMon <- strftime(dataClm$DateTime, "%Y-%m")

##############################################################################
#Write output to CLM
##############################################################################

#Define the timesteps for data output
# year       <- unique(lubridate::year(dataClm$DateTime + lubridate::days(1))) 
# mon        <- unique(lubridate::month(dataClm$DateTime + lubridate::days(1)))
# regu_days  <- c(31,28,31,30,31,30,31,31,30,31,30,31)
# leap_days  <- c(31,29,31,30,31,30,31,31,30,31,30,31)
# regu_steps <- regu_days * 48
# leap_steps <- leap_days * 48
# nyear <- length(year)

#Define missing value fill
mv <- -9999.  
# startStep <- 1

#Loop around years of data
# for (y in ) {
#   #  y <- 1
#   if(year[y]==2008 || year[y]==2012) {
#     nsteps <- leap_steps
#   } else {
#     nsteps <- regu_steps
#   }

#Set of year/month combinations for netCDF output
setYearMon <- unique(strftime(dataClm$DateTime, "%Y-%m"))

  for (m in setYearMon) {
    #m <- setYearMon[1] #for testing
    Data.mon <- dataClm[dataClm$yearMon == m,]
    timeStep <- seq(0,nrow(Data.mon)-1,1)
    time     <- timeStep/48
    #endStep  <- startStep + nsteps[m]-1
    
    print(paste(m,"Data date =",Data.mon$DateTime[1]))
    names(Data.mon)
  
#NetCDF output filename
fileOutNcdf <- paste(DirOut,"/",m,".nc", sep = "")
  #sub(pattern = ".txt", replacement = ".nc", fileOut)



# define the netcdf coordinate variables (name, units, type)
lat  <- ncdf4::ncdim_def("lat","degrees_north", as.double(latSite), create_dimvar=TRUE)
lon <- ncdf4::ncdim_def("lon","degrees_east", as.double(lonSite), create_dimvar=TRUE)

#Variables to output to netCDF
time <- ncdf4::ncdim_def("time", paste("days since",Data.mon$DateTime[1]),
                       vals=as.double(time),unlim=FALSE, create_dimvar=TRUE )
LATIXY  <- ncdf4::ncvar_def("LATIXY", "degrees N", list(lat), mv,
                        longname="latitude", prec="double")
LONGXY  <- ncdf4::ncvar_def("LONGXY", "degrees E", list(lon), mv,
                        longname="longitude", prec="double")
FLDS  <- ncdf4::ncvar_def("FLDS", "W/m^2", list(lon,lat,time), mv,
                      longname="incident longwave (FLDS)", prec="double")
FSDS  <- ncdf4::ncvar_def("FSDS", "W/m^2", list(lon,lat,time), mv,
                      longname="incident shortwave (FSDS)", prec="double")
PRECTmms <- ncdf4::ncvar_def("PRECTmms", "mm/s", list(lon,lat,time), mv,
                         longname="precipitation (PRECTmms)", prec="double")
PSRF  <- ncdf4::ncvar_def("PSRF", "Pa", list(lon,lat,time), mv,
                      longname="pressure at the lowest atmospheric level (PSRF)", prec="double")
RH    <- ncdf4::ncvar_def("RH", "%", list(lon,lat,time), mv,
                      longname="relative humidity at lowest atm level (RH)", prec="double")
TBOT  <- ncdf4::ncvar_def("TBOT", "K", list(lon,lat,time), mv,
                      longname="temperature at lowest atm level (TBOT)", prec="double")
WIND  <- ncdf4::ncvar_def("WIND", "m/s", list(lon,lat,time), mv,
                      longname="wind at lowest atm level (WIND)", prec="double")
ZBOT  <- ncdf4::ncvar_def("ZBOT", "m", list(lon,lat,time), mv,
                      longname="observational height", prec="double")
NEE <- ncdf4::ncvar_def("NEE", "umolm-2s-1", list(lon,lat,time), mv,
                          longname="net ecosystem exchange", prec="double")
FSH  <- ncdf4::ncvar_def("FSH", "Wm-2", list(lon,lat,time), mv,
                          longname="sensible heat flux", prec="double")
EFLX_LH_TOT  <- ncdf4::ncvar_def("EFLX_LH_TOT", "Wm-2", list(lon,lat,time), mv,
                                 longname="latent heat flux", prec="double")
GPP <- ncdf4::ncvar_def("GPP", "umolm-2s-1", list(lon,lat,time), mv,
                        longname="gross primary productivity", prec="double")
Rnet  <- ncdf4::ncvar_def("Rnet", "W/m^2", list(lon,lat,time), mv,
                          longname="net radiation", prec="double")

#Create the output file
ncnew <- ncdf4::nc_create(fileOutNcdf, list(LATIXY,LONGXY,FLDS,FSDS,PRECTmms,RH,PSRF,TBOT,WIND,ZBOT,NEE,FSH,EFLX_LH_TOT,GPP,Rnet))


# Write some values to this variable on disk.
 ncdf4::ncvar_put(ncnew, LATIXY, latSite)
 ncdf4::ncvar_put(ncnew, LONGXY, lonSite)
 ncdf4::ncvar_put(ncnew, FLDS, Data.mon$FLDS)
 ncdf4::ncvar_put(ncnew, FSDS, Data.mon$FSDS)
 ncdf4::ncvar_put(ncnew, RH,   Data.mon$RH)
 ncdf4::ncvar_put(ncnew, PRECTmms, Data.mon$PRECTmms)
 ncdf4::ncvar_put(ncnew, PSRF, Data.mon$PSRF)
 ncdf4::ncvar_put(ncnew, TBOT, Data.mon$TBOT)
 ncdf4::ncvar_put(ncnew, WIND, Data.mon$WIND)
 ncdf4::ncvar_put(ncnew, ZBOT, Data.mon$ZBOT)
 ncdf4::ncvar_put(ncnew, NEE, Data.mon$NEE)
 ncdf4::ncvar_put(ncnew, FSH, Data.mon$H)
 ncdf4::ncvar_put(ncnew, EFLX_LH_TOT, Data.mon$LE)
 ncdf4::ncvar_put(ncnew, GPP, Data.mon$GPP)
 ncdf4::ncvar_put(ncnew, Rnet, Data.mon$radNet)
#add attributes
#ncdf4::ncatt_put(ncnew, time,"calendar", "gregorian" ,prec=NA,verbose=FALSE,definemode=FALSE )
ncdf4::ncatt_put(ncnew, FLDS,"mode","time-dependent" ,prec=NA,verbose=FALSE,definemode=FALSE )
ncdf4::ncatt_put(ncnew, FSDS,"mode","time-dependent" ,prec=NA,verbose=FALSE,definemode=FALSE )
ncdf4::ncatt_put(ncnew, RH  ,"mode","time-dependent" ,prec=NA,verbose=FALSE,definemode=FALSE )
ncdf4::ncatt_put(ncnew, PRECTmms,"mode","time-dependent" ,prec=NA,verbose=FALSE,definemode=FALSE )
ncdf4::ncatt_put(ncnew, PSRF,"mode","time-dependent" ,prec=NA,verbose=FALSE,definemode=FALSE )
ncdf4::ncatt_put(ncnew, TBOT,"mode","time-dependent" ,prec=NA,verbose=FALSE,definemode=FALSE )
ncdf4::ncatt_put(ncnew, WIND,"mode","time-dependent" ,prec=NA,verbose=FALSE,definemode=FALSE )
ncdf4::ncatt_put(ncnew, ZBOT,"mode","time-dependent" ,prec=NA,verbose=FALSE,definemode=FALSE )
ncdf4::ncatt_put(ncnew, NEE,"mode","time-dependent" ,prec=NA,verbose=FALSE,definemode=FALSE )
ncdf4::ncatt_put(ncnew, FSH,"mode","time-dependent" ,prec=NA,verbose=FALSE,definemode=FALSE )
ncdf4::ncatt_put(ncnew, EFLX_LH_TOT,"mode","time-dependent" ,prec=NA,verbose=FALSE,definemode=FALSE )
ncdf4::ncatt_put(ncnew, GPP,"mode","time-dependent" ,prec=NA,verbose=FALSE,definemode=FALSE )
ncdf4::ncatt_put(ncnew, Rnet,"mode","time-dependent" ,prec=NA,verbose=FALSE,definemode=FALSE )
ncdf4::ncatt_put(ncnew, 0, "created_on",date()       ,prec=NA,verbose=FALSE,definemode=FALSE )
ncdf4::ncatt_put(ncnew, 0, "created_by","David Durden",prec=NA,verbose=FALSE,definemode=FALSE )
ncdf4::ncatt_put(ncnew, 0, "created_from",fileOut        ,prec=NA,verbose=FALSE,definemode=FALSE )
ncdf4::ncatt_put(ncnew, 0, "created_with", "flow.api.clm.R",prec=NA,verbose=FALSE,definemode=FALSE )

#Close Netcdf file connection
ncdf4::nc_close(ncnew)
#Add step
#startStep <- endStep + 1
#Remove not needed variables
remove(endStep, time, timeStep, fileOutNcdf, ncnew, Data.mon,
       FLDS,FSDS,RH,PRECTmms,PSRF,TBOT,WIND,ZBOT)
  } #End of monthloop

#} #End of year loop
