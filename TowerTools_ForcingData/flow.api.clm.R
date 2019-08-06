#Call the R HDF5 Library
packReq <- c("rhdf5", "eddy4R.base", "neonUtilities", "REddyProc", "ncdf4")

#Install and load all required packages
lapply(packReq, function(x) {
  print(x)
  if(require(x, character.only = TRUE) == FALSE) {
    install.packages(x)
    library(x, character.only = TRUE)
  }})

#############################################################
#Workflow parameters
#############################################################
#Which NEON site are we grabbing data from (4-letter ID)
Site <- "HARV"
#Which type of data package (expanded or basic)
Pack <- "basic"
#Time averaging period
TimeAgr <- 30
#Beginning date for data grabbing
dateBgn <- "2018-03-01"

#End date for date grabbing
dateEnd <- "2018-07-01"


#The version data for the FP standard conversion processing
ver = paste0("v",format(Sys.time(), "%Y%m%dT%H%m"))
#Download directory for HDF5 files from the API
DirDnld=tempdir()
#paste0("~/eddy/data/Ameriflux/",ver,"/",site)
#Output directory
if("DIROUT" %in% base::names(base::Sys.getenv())) {
  DirOutBase <- Sys.getenv("DIROUT")
}else{  
  #DirOut <- "N:/Science/FIUDATA/IPT_data/dynamic/AmeriFlux/" #Default folder Ameriflux data output
  DirOutBase <-paste0("~/eddy/data/CLM/",ver)
  
}

#############################################################


#Append the site to the base output directory
DirOut <- paste0(DirOutBase, "/", Site)
#Check if directory exists and create if not
if(!dir.exists(DirOut)) dir.create(DirOut, recursive = TRUE)


#Define site info
siteInfo <- Noble::tis_site_config
siteInfo <- siteInfo[siteInfo$SiteID == Site,]

#Grab latitude and longitude from site metadata
latSite <- siteInfo$Latitude
lonSite <- siteInfo$Longitude

#DP number
idDpFlux <- 'DP4.00200.001'

#Create the method for date sequence based off of the package chosen
MethDateSeq <- ifelse(test = Pack == "basic", yes = "month", no = "day")

#If monthly files from basic chosen round to the beginning and end of the month for dateBgn and dateEnd
if(MethDateSeq == "month"){
  dateBgn <- lubridate::floor_date(as.Date(dateBgn), unit = "month")
  dateEnd <- lubridate::ceiling_date(as.Date(dateEnd), unit = "month") - lubridate::days(1)
}
#Create the date sequence
setDate <- seq(from = as.Date(dateBgn), to = as.Date(dateEnd), by = MethDateSeq)

##############################################################################
#Flux data
##############################################################################

#Initialize data List
dataList <- list()

#Read data from the API
dataList <- lapply(setDate, function(x) {
  year <- lubridate::year(x)
  mnth <- lubridate::month(x)
  tryCatch(som::def.neon.api.get.data.hdf5(site=Site,idDpMain=idDpFlux,year=year,mnth=mnth,DirDnld=DirDnld,Rm = TRUE), error=function(e) NULL)
})

#Add names to list for year/month combinations
names(dataList) <- paste0(lubridate::year(setDate),sprintf("%02d",lubridate::month(setDate)))

#Remove NULL elements from list
dataList <- dataList[vapply(dataList, Negate(is.null), NA)]

##############################################################################
#Determine HOR & VER indices
##############################################################################

#Tower top level
LvlTowr <- grep(pattern = "_30m", names(dataList[[1]][[Site]]$dp01$data$co2Turb), value = TRUE)
LvlTowr <- gsub(x = LvlTowr, pattern = "_30m", replacement = "")

#Subset to the Ameriflux variables to deliver
dataListFlux <- lapply(names(dataList), function(x) {
  data.frame(
   # "TIMESTAMP_START" = as.POSIXlt(dataList[[x]][[Site]]$dp04$data$fluxCo2$turb$timeBgn, format="%Y-%m-%dT%H:%M:%OSZ", tz = "GMT"), #Timestamp represents end of period in ReddyProc
    "TIMESTAMP" = as.POSIXlt(dataList[[x]][[Site]]$dp04$data$fluxCo2$turb$timeEnd, format="%Y-%m-%dT%H:%M:%OSZ", tz = "GMT"),
    "NEE"= dataList[[x]][[Site]]$dp04$data$fluxCo2$nsae$flux,
    "LE" = dataList[[x]][[Site]]$dp04$data$fluxH2o$turb$flux,
    "Ustar" = dataList[[x]][[Site]]$dp04$data$fluxMome$turb$veloFric,
    "H" = dataList[[x]][[Site]]$dp04$data$fluxTemp$turb$flux, 
    "qfTurbFlow" = dataList[[x]][[Site]]$dp01$qfqm$h2oTurb[[paste0(LvlTowr,"_30m")]]$frt00Samp$qfFinl,
    "qfTurbH2oFinl" = dataList[[x]][[Site]]$dp01$qfqm$h2oTurb[[paste0(LvlTowr,"_30m")]]$rtioMoleDryH2o$qfFinl,
    "qfTurbCo2Finl" = dataList[[x]][[Site]]$dp01$qfqm$co2Turb[[paste0(LvlTowr,"_30m")]]$rtioMoleDryCo2$qfFinl,
    "WS_MDS" = dataList[[x]][[Site]]$dp01$data$soni[[paste0(LvlTowr,"_30m")]]$veloXaxsYaxsErth$mean,
    "Pa_MDS" = dataList[[x]][[Site]]$dp01$data$h2oTurb[[paste0(LvlTowr,"_30m")]]$presAtm$mean,
    "Tair" = dataList[[x]][[Site]]$dp01$data$soni[[paste0(LvlTowr,"_30m")]]$tempAir$mean
    , stringsAsFactors = FALSE)
})

#Names for the data output
names(dataListFlux) <- names(dataList)

#Combine the monthly data into a single dataframe
dataDfFlux <- do.call(rbind.data.frame,dataListFlux)

#Round up the timestamp for end of periods
dataDfFlux$TIMESTAMP <- lubridate::round_date(dataDfFlux$TIMESTAMP, unit = "minute")
###################################################################################
#Time regularization if needed
# Regularize timeseries to 30 minutes in case missing data after CI processing
# timeRglr <- eddy4R.base::def.rglr(timeMeas = as.POSIXlt(dataDfFlux$TIMESTAMP_START), dataMeas = dataDfFlux, BgnRglr = as.POSIXlt(dataDfFlux$TIMESTAMP_START[1]), EndRglr = as.POSIXlt(dataDfFlux$TIMESTAMP_END[length(dataDfFlux$TIMESTAMP_END)]), TzRglr = "UTC", FreqRglr = 1/(60*30))
# 
# #Reassign data to data.frame
# dataDfFlux <- timeRglr$dataRglr
# 
# 
# #Format timestamps
# dataDfFlux$TIMESTAMP_START <- strftime(timeRglr$timeRglr , format = "%Y%m%d%H%M")
# dataDfFlux$TIMESTAMP_END <- strftime(timeRglr$timeRglr+ lubridate::minutes(30), format = "%Y%m%d%H%M")
###################################################################################

#Remove flagging variables from outpu
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
LvlTowrMet <- gsub(pattern = "_", replacement = ".", x = LvlTowr)


#List of DP numbers by eddy4R DP names
listDpNum <- c( "PRECTmms_MDS" = "DP1.00006.001", "rH" = "DP1.00098.001", "FLDS_MDS" = "DP1.00023.001", "Rg" = "DP1.00023.001")

#names for individual variables of interest
varDp <- c("PRECTmms_MDS" = "secPrecipBulk", "rH" = paste("RHMean","003.000", sep = "."), "FLDS_MDS" = paste("inLWMean",LvlTowrMet, sep = "."), "Rg" = paste("inSWMean",LvlTowrMet, sep = ".")) #Currently using the relative humidity from the soil array, tower top was not reporting data at HARV during this time

#Grab data for data products using neonUtilities
#neonUtilities::getPackage(site_code = site, package = pack, year_month =  )

##Grab data for data products using Noble package
dataMet <- lapply(listDpNum, function(x){
  try(expr = Noble::pull.date(site = Site, dpID = x, bgn.date = dateBgn - lubridate::minutes(1), end.date = dateEnd + lubridate::days(1), package = Pack, time.agr = TimeAgr), silent = TRUE)
  })

#Grab just the Met data of interest for the forcing data
dataDfMet <- as.data.frame(lapply(seq_along(varDp), function(x){
  #Grab the variables of interest
  dataMet[[names(varDp[x])]][,grep(pattern = paste0("^",varDp[x]), x = names(dataMet[[names(varDp[x])]]))] # use ^ to indicate the pattern starts with the name given
}))

#Give the ReddyProc names
names(dataDfMet) <- names(varDp)

#Calculate precip rate from bulk
dataDfMet$PRECTmms_MDS <- dataDfMet$PRECTmms_MDS/1800 #1800 sec/0.5 hours

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
unitDf <- c("Year" = "--", "DoY" = "--", "Hour" = "--", "NEE" = "umolm-2s-1", "LE" = "Wm-2", "H" = "Wm-2", "Ustar" = "ms-1", "WS_MDS" = "ms-1", "Pa_MDS" = "Pa", "Tair" = "degC", "PRECTmms_MDS" = "mms-1", "rH" = "%", "FLDS_MDS" = "Wm-2", "Rg" = "Wm-2")

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
EddyDataWithPosix.F <- fConvertTimeToPosix(EddyData.F, 'YDH', Year.s='Year', Day.s='DoY', Hour.s='Hour')


#+++ Initalize R5 reference class sEddyProc for processing of eddy data
#+++ with all variables needed for processing later
EddyProc.C <- sEddyProc$new(Site, EddyDataWithPosix.F, c('NEE','Rg','Tair','VPD','rH','LE','H','Ustar','Pa_MDS', 'FLDS_MDS','WS_MDS', 'PRECTmms_MDS'))

#+++ Fill gaps in variables with MDS gap filling algorithm (without prior ustar filtering)
EddyProc.C$sMDSGapFill('NEE', FillAll.b=TRUE) #Fill all values to estimate flux uncertainties
EddyProc.C$sMDSGapFill('LE', FillAll.b=TRUE)
EddyProc.C$sMDSGapFill('H', FillAll.b=TRUE)
EddyProc.C$sMDSGapFill('Ustar', FillAll.b=TRUE)
EddyProc.C$sMDSGapFill('Tair', FillAll.b=FALSE)  
EddyProc.C$sMDSGapFill('VPD',    FillAll.b=FALSE) 
EddyProc.C$sMDSGapFill('rH',     FillAll.b=FALSE) 
EddyProc.C$sMDSGapFill('WS_MDS', FillAll.b=FALSE) 
EddyProc.C$sMDSGapFill('PRECTmms_MDS', FillAll.b=FALSE) 
EddyProc.C$sMDSGapFill('Pa_MDS', FillAll.b=FALSE) 
EddyProc.C$sMDSGapFill('FLDS_MDS', FillAll.b=FALSE) 
EddyProc.C$sMDSGapFill('Rg', FillAll.b=FALSE) 
#+++ Export gap filled and partitioned data to standard data frame
FilledEddyData.F <- EddyProc.C$sExportResults()

#Grab just the filled data products
dataClm <- FilledEddyData.F[,grep(pattern = "_f$", x = names(FilledEddyData.F))]

#Grab the POSIX timestamp
dataClm$time <- EddyDataWithPosix.F$DateTime

names(dataClm) <- c("NEE", "LE", "H", "Ustar", "TBOT", "VPD", "RH", "WIND", "PRECTmms", "PSRF",  "FLDS", "FSDS", "time")



##############################################################################
#Write output to CLM
##############################################################################
#NetCDF output filename
fileOutNcdf <- sub(pattern = ".txt", replacement = ".nc", fileOut)

#Define missing value fill
mv <- -9999.  

# define the netcdf coordinate variables (name, units, type)
lat  <- ncdf4::ncdim_def("lat","degrees_north", as.double(latSite), create_dimvar=TRUE)
lon <- ncdf4::ncdim_def("lon","degrees_east", as.double(lonSite), create_dimvar=TRUE)

#Variables to output to netCDF
time <- ncdf4::ncdim_def("time", units=paste("days since",dataClm$time[1]),
                     vals=as.double(dataClm$time),unlim=FALSE, create_dimvar=TRUE )
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

#Create the output file
ncnew <- ncdf4::nc_create(fileOutNcdf, list(LATIXY,LONGXY,FLDS,FSDS,PRECTmms,RH,PSRF,TBOT,WIND,ZBOT))


# Write some values to this variable on disk.
 ncdf4::ncvar_put(ncnew, LATIXY, latSite)
 ncdf4::ncvar_put(ncnew, LONGXY, lonSite)
 ncdf4::ncvar_put(ncnew, FLDS, dataClm$FLDS)
 ncdf4::ncvar_put(ncnew, FSDS, dataClm$FSDS)
 ncdf4::ncvar_put(ncnew, RH,   dataClm$RH)
 ncdf4::ncvar_put(ncnew, PRECTmms, dataClm$PRECTmms)
 ncdf4::ncvar_put(ncnew, PSRF, dataClm$PSRF)
 ncdf4::ncvar_put(ncnew, TBOT, dataClm$TBOT)
 ncdf4::ncvar_put(ncnew, WIND, dataClm$WIND)
 ncdf4::ncvar_put(ncnew, ZBOT, dataClm$ZBOT)
#add attributes
ncdf4::ncatt_put(ncnew, time,"calendar", "gregorian" ,prec=NA,verbose=FALSE,definemode=FALSE )
ncdf4::ncatt_put(ncnew, FLDS,"mode","time-dependent" ,prec=NA,verbose=FALSE,definemode=FALSE )
ncdf4::ncatt_put(ncnew, FSDS,"mode","time-dependent" ,prec=NA,verbose=FALSE,definemode=FALSE )
ncdf4::ncatt_put(ncnew, RH  ,"mode","time-dependent" ,prec=NA,verbose=FALSE,definemode=FALSE )
ncdf4::ncatt_put(ncnew, PRECTmms,"mode","time-dependent" ,prec=NA,verbose=FALSE,definemode=FALSE )
ncdf4::ncatt_put(ncnew, PSRF,"mode","time-dependent" ,prec=NA,verbose=FALSE,definemode=FALSE )
ncdf4::ncatt_put(ncnew, TBOT,"mode","time-dependent" ,prec=NA,verbose=FALSE,definemode=FALSE )
ncdf4::ncatt_put(ncnew, WIND,"mode","time-dependent" ,prec=NA,verbose=FALSE,definemode=FALSE )
ncdf4::ncatt_put(ncnew, ZBOT,"mode","time-dependent" ,prec=NA,verbose=FALSE,definemode=FALSE )
ncdf4::ncatt_put(ncnew, 0, "created_on",date()       ,prec=NA,verbose=FALSE,definemode=FALSE )
ncdf4::ncatt_put(ncnew, 0, "created_by","David Durden",prec=NA,verbose=FALSE,definemode=FALSE )
ncdf4::ncatt_put(ncnew, 0, "created_from",fin        ,prec=NA,verbose=FALSE,definemode=FALSE )
ncdf4::ncatt_put(ncnew, 0, "created_with",file       ,prec=NA,verbose=FALSE,definemode=FALSE )

#Close Netcdf file connection
ncdf4::nc_close(ncnew)
