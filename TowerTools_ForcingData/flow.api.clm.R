#Call the R HDF5 Library
packReq <- c("rhdf5", "eddy4R.base", "neonUtilities")

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

#Define site info
siteInfo <- Noble::tis_site_config
siteInfo <- siteInfo[siteInfo$SiteID == Site,]

#Grab latitude and longitude from site metadata
lat <- siteInfo$Latitude
lon <- siteInfo$Longitude

#List of DP numbers by eddy4R DP names
listDpNum <- c("TBOT" = "DP1.00003.001", "PRECTmms" = "DP1.00006.001", "WIND" = "DP1.00001.001", "PSRF" = "DP1.00004.001", "RH" = "DP1.00098.001", "FLDS" = "DP1.00023.001", "FSDS" = "DP1.00023.001")

varDp <- c("TBOT" = "tempTripleMean", "PRECTmms" = "secPrecipBulk", "WIND" = "windSpeedMean.000.050", "PSRF" = "staPresMean", "RH" = "RHMean", "FLDS" = "inLWMean", "FSDS" = "inSWMean")
#Year and month of interest

#varMetaData <- c("lat" = "LatTow", "lon" = "LonTow", "ZBOT" = "DistZaxsTow" )

#varReqData <- c("TBOT" = "tempAir", "FLDS" =, "FSDS" =, "PRECTmms" = , "PSRF", "RH")

#Grab data for data products using neonUtilities
#neonUtilities::getPackage(site_code = site, package = pack, year_month =  )

##Grab data for data products using Noble package
data <- lapply(listDpNum, function(x){
  try(expr = Noble::pull.date(site = Site, dpID = x, bgn.date = dateBgn, end.date = dateEnd, package = Pack, time.agr = TimeAgr), silent = TRUE)
  })


