#Call the R HDF5 Library
packReq <- c("rhdf5", "eddy4R.base", "neonUtilities", "REddyProc")

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
dateBgn <- "2018-06-01"

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




#Define site info
siteInfo <- Noble::tis_site_config
siteInfo <- siteInfo[siteInfo$SiteID == Site,]

#Grab latitude and longitude from site metadata
lat <- siteInfo$Latitude
lon <- siteInfo$Longitude

#List of DP numbers by eddy4R DP names
listDpNum <- c("TBOT" = "DP1.00003.001", "PRECTmms" = "DP1.00006.001", "WIND" = "DP1.00001.001", "PSRF" = "DP1.00004.001", "RH" = "DP1.00098.001", "FLDS" = "DP1.00023.001", "FSDS" = "DP1.00023.001")

varDp <- c("TBOT" = "tempTripleMean", "PRECTmms" = "secPrecipBulk", "WIND" = "windSpeedMean.000.050", "PSRF" = "staPresMean", "RH" = "RHMean", "FLDS" = "inLWMean", "FSDS" = "inSWMean")




#Create the method for date sequence based off of the package chosen
MethDateSeq <- ifelse(test = Pack == "basic", yes = "month", no = "day")

#If monthly files from basic chosen round to the beginning and end of the month for dateBgn and dateEnd
if(MethDateSeq == "month"){
  dateBgn <- lubridate::floor_date(as.Date(dateBgn), unit = "month")
  dateEnd <- lubridate::ceiling_date(as.Date(dateEnd), unit = "month") - lubridate::days(1)
}
#Create the date sequence
setDate <- seq(from = as.Date(dateBgn), to = as.Date(dateEnd), by = MethDateSeq)




#Year and month of interest

#varMetaData <- c("lat" = "LatTow", "lon" = "LonTow", "ZBOT" = "DistZaxsTow" )

#varReqData <- c("TBOT" = "tempAir", "FLDS" =, "FSDS" =, "PRECTmms" = , "PSRF", "RH")

#Grab data for data products using neonUtilities
#neonUtilities::getPackage(site_code = site, package = pack, year_month =  )

##Grab data for data products using Noble package
data <- lapply(listDpNum, function(x){
  try(expr = Noble::pull.date(site = Site, dpID = x, bgn.date = dateBgn, end.date = dateEnd, package = Pack, time.agr = TimeAgr), silent = TRUE)
  })

#ReddyProc workflow
EddyProc.C <- sEddyProc$new('NR-TVan', EddyDataWithPosix.F, c('NEE','Rg','Tair','VPD','ws_MDS','Rn_MDS','Rh','LE','H','Pa_MDS'))
#+++ Generate plots of all data in directory \plots (of current R working dir)
EddyProc.C$sPlotHHFluxes('NEE')
EddyProc.C$sPlotFingerprint('Rg')
EddyProc.C$sPlotDiurnalCycle('Tair')
EddyProc.C$sPlotDiurnalCycle('NEE')
#+++ Plot individual months/years to screen (of current R graphics device)
years <- seq(2008,2014,1)
nyears <- length(years)
for (i in 1:nyears) {  
  EddyProc.C$sPlotHHFluxesY('NEE', Year.i=years[i])
  EddyProc.C$sPlotFingerprintY('NEE', Year.i=years[i])
}
EddyProc.C$sPlotDiurnalCycleM('NEE', Month.i=6)

#+++ Fill gaps in variables with MDS gap filling algorithm (without prior ustar filtering)
EddyProc.C$sMDSGapFill('NEE', FillAll.b=TRUE) #Fill all values to estimate flux uncertainties
