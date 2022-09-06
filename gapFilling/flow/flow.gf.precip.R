##############################################################################################
#' @title Workflow to gap-fill precip data

#' @author
#' David Durden \email{eddy4R.info@gmail.com}

#' @description 
#' Workflow for collating NEON data from API, gap-filling, and packaging in NCAR CLM netcdf format.

# changelog and author contributions / copyrights
# David Durden (2019-07-05)
#   original creation

##############################################################################################

#############################################################
#Dependencies
#############################################################


#Call the R HDF5 Library
packReq <- c("rhdf5", "REddyProc", "devtools",'reshape2', 'ggplot2','tidyverse','gridExtra','knitr','naniar',  "metScanR", "jsonlite", "rnoaa")#"Rfast",#"ncdf4", 

#Install and load all required packages
lapply(packReq, function(x) {
  print(x)
  if(require(x, character.only = TRUE) == FALSE) {
    install.packages(x)
    library(x, character.only = TRUE)
  }})

#Install packages from github repos
devtools::install_github("https://github.com/ddurden/metget")


#Which NEON site are we grabbing data from (4-letter ID)
setSiteTis <- c("BARR","CLBJ","MLBS","DSNY","NIWO","ORNL","OSBS",
             "SCBI","LENO","TALL","CPER","BART","HARV","BLAN",
             "SERC","JERC","GUAN","LAJA","STEI","TREE","UNDE",
             "KONA","KONZ","UKFS","GRSM","DELA","DCFS","NOGP",
             "WOOD","RMNP","OAES","YELL","MOAB","STER","JORN",
             "SRER","ONAQ","ABBY","WREF","SJER","SOAP","TEAK",
             "TOOL","BONA","DEJU","HEAL","PUUM")

#Site for analysis
#Site <- "NIWO"

#Parameters
dateBgn <- "2018-01-01"
dateEnd <- "2022-04-01"
numDay <- difftime(as.POSIXct(dateEnd), as.POSIXct(dateBgn, tz="UTC"), units="days")

TimeAgr <- "30"
Pack <- "basic"
MethDnld <- FALSE #Logical to download data or load from file

#List of DP numbers by eddy4R DP names
listDpNum <- c( "precip" = "DP1.00006.001")

infoProd <- neonUtilities::getProductInfo("DP1.00006.001")

#Logical to download data or load from file
if(MethDnld == TRUE){
#download data
dataPrecip <- neonUtilities::loadByProduct(dpID = listDpNum[["precip"]], startdate = dateBgn, enddate = dateEnd, package = Pack, timeIndex = TimeAgr, check.size = FALSE)

#Save precip data for additional testing
saveRDS(dataPrecip, file = "/home/ddurden/eddy/tmp/CLM/precip/NEON_precip_all_sites_2018_2022.rds")
} else{
  dataPrecip <- readRDS("/home/ddurden/eddy/tmp/CLM/precip/NEON_precip_all_sites_2018_2022.rds")
  }
#Subset data to the site of interest
dataSub <- dataPrecip$PRIPRE_30min %>% select(siteID,startDateTime, priPrecipBulk, priPrecipFinalQF) %>% filter(siteID == "NIWO")

#time regularization of met data
  dataSubTmp <- eddy4R.base::def.rglr(timeMeas = as.POSIXlt(dataSub$startDateTime), 
                                       dataMeas = dataSub, 
                                       BgnRglr = as.POSIXlt(dataSub$startDateTime[[1]]), 
                                       EndRglr = as.POSIXlt(dataSub$startDateTime[length(dataSub$startDateTime)]), 
                                       TzRglr = "UTC", FreqRglr = 1/(60*30))$dataRglr #End lapply for time regularization of met data

#Add names to list of Dataframes of regularized data
names(dataSubTmp) <- names(dataSub)


  #Plot primary precip 
  p <- ggplot2::ggplot(data = dataPrecip$PRIPRE_30min, aes(x = startDateTime, y= priPrecipBulk))+ 
    geom_point(aes(colour = factor(priPrecipFinalQF))) + facet_wrap(~siteID)
  
  #Plot secondary precip
    p <- ggplot2::ggplot(data = dataPrecip$SECPRE_30min, aes(x = startDateTime, y= secPrecipBulk))+ 
    geom_point(aes(colour = factor(secPrecipSciRvwQF))) + facet_wrap(~siteID)

#     
#############################################################
    #Precip data available across sites
#############################################################    
# 
# listSites <- list("priPrecip" = intersect(unique(dataPrecip$PRIPRE_30min$siteID), setSiteTis), "secPrecip" = intersect(unique(dataPrecip$SECPRE_30min$siteID), setSiteTis), "tfPrecip" = intersect(unique(dataPrecip$THRPRE_30min$siteID), setSiteTis))
# 
#  listSites$`pri + sec` <- intersect(listSites$priPrecip,listSites$secPrecip)
#  listSites$`pri + tf` <- intersect(listSites$priPrecip,listSites$tfPrecip)
#  listSites$`sec + tf` <- intersect(listSites$secPrecip,listSites$tfPrecip)
#  listSites$`pri + sec + tf` = intersect(listSites$`pri + sec`, listSites$tfPrecip)    
 
 #Precip data available at NEON TIS sites  
 #varPrecip <- sort(sapply(listSites, length), decreasing = TRUE)
 #Barplot of available data
 #x <- barplot(varPrecip, col = terrain.colors(7), ylim = c(0,50), main = "Precip data stream available at TIS sites (47 TIS sites)")
 #Add text to barplot
# text(x,varPrecip+3,labels=as.character(varPrecip))
    

#############################################################
    #Data from other sites
#############################################################    
    
#Site metadata
metaSite <- metScanR::getStation(paste0("NEON:",Site))

#Find nearby sites (100 km radius) with precip data
siteNear <- metScanR::siteFinder(siteID = paste0("NEON:",Site), radius = 10, vars = "precipitation")


metScanR::mapResults(siteNear)

#test <- metget::getData(site_meta = siteNear, start_date = dateBgn, end_date = dateEnd, temp_agg = "hourly")

# temp_agg <- "subhourly"
# sid <- "USW00003047"
# start_date <- as.POSIXct("2014-04-01 00:00:01", format="%Y-%m-%d %H:%M:%S", tz="UTC")
# end_date <- as.POSIXct("2015-02-01 00:00:00", format = "%Y-%m-%d %H:%M:%S", tz="UTC")
# 
# test <-metget::getUSCRNData(temp_agg = temp_agg, sid = sid, , start_date = start_date, end_date = end_date)

#test <- nrow(dataMet$PRECTmms_MDS$THRPRE_30min$TFPrecipBulk %>% filter(horizontalPosition == "005"))

#plot(as.POSIXct(dataMet$PRECTmms_MDS$PRIPRE_30min$startDateTime), dataMet$PRECTmms_MDS$PRIPRE_30min$priPrecipBulk, col = ifelse(dataMet$PRECTmms_MDS$PRIPRE_30min$priPrecipFinalQF == 1, "red", "black"))



# soilWater <- neonUtilities::loadByProduct(site = Site, dpID = "DP1.00094.001", 
#                              startdate = as.character(dateBgn), 
#                              enddate = as.character(dateEnd), 
#                              package = Pack, avg = TimeAgr, check.size = FALSE)

# test <- dataPrecip$THRPRE_30min %>% filter(siteID == "HARV") %>% select(horizontalPosition, startDateTime, TFPrecipBulk) %>% spread(horizontalPosition, TFPrecipBulk)
# 
# temp <- dataPrecip$PRIPRE_30min %>% filter(siteID == "HARV") %>% select(priPrecipBulk)
# 
# test$precip <- temp$priPrecipBulk

##########################################################################################
#Plot gaps using naniar package
##########################################################################################
#grab dataSub for testing
test <- dataSub

#Flare graph of missing data
vis_miss(test)

#Number of variables with missing data
varMiss <- n_var_miss(test)

#Upset interactions plot of missing data
gg_miss_upset(test, nsets = varMiss)

#Plot of the amount of missing data per variable (as %)
gg_miss_var(test, show_pct = TRUE)

#Add a Month column
test$month <- strftime(test$startDateTime, format = "%Y%m")

#Heat map of missing data
gg_miss_fct(test, fct = month)




#test2 <- def.gf.rep(dataGf = test, NameVarGf = "precip", NameVarRep = c("001", "002","003","004","005"))

#plot(test$startDateTime, test2$pred, ylab = "Precip", xlab = "Time", col = "burlywood1")
#points(test$startDateTime, test$precip, col = "cyan")

##########################################################################################
#Mesonet API testing
##########################################################################################
# #Time for gathering data for gap-filling
# timeBgn <- "201801010000"
# timeEnd <- "202104300000"
# 
# #Radius for searching for sites
# rad <- 10
# 
# #Location information for the site for building mesonet query
# locInfo <- paste0("radius=",metaSite$NIWO$location$latitude_dec, ",", metaSite$NIWO$location$longitude_dec, ",", rad)
# 
# timeInfo <- paste0("&start=",timeBgn,"&end=",timeEnd,"&pmode=intervals&interval=1&")
# 
# 
# apiMesoBase <- "https://api.synopticdata.com/v2/stations/precip?"
# 
# apiMesoToke <- "token=53fbb4722bad4ff391f351e1e425f8c3"
# 
# 
# 
# #Create download URL
# urlMeso <- paste0(apiMesoBase,locInfo,timeInfo,apiMesoToke)
# 
# 
# jsonMeso <- jsonlite::fromJSON(urlMeso)
# 
# dataMeso <- jsonMeso$STATION$OBSERVATIONS$precipitation[[1]]
# 
# #Calculate precip rate from bulk
# dataMeso$ratePrecip <- dataMeso$total/3600 #1800 sec/0.5 hours
# 
# 
# dataGf$PRECTmms_MDS$PRECTmms_MDS_002
# plot(lubridate::fast_strptime(dataMeso$first_report, "%Y-%m-%dT%H:%M:%SZ"), dataMeso$total)
# 
# plot(as.POSIXct(jsonMeso$STATION$OBSERVATIONS$precipitation[[1]]$first_report),jsonMeso$STATION$OBSERVATIONS$precipitation[[1]]$total)
# 
# head(jsonMeso$STATION$OBSERVATIONS$precipitation[[5]]$report_type)
# 


##########################################################################################
#RNOAA testing
##########################################################################################

#Define site
#Site <- "NIWO"

#Site metadata
metaSite <- metScanR::getStation(paste0("NEON:",Site))

#Find nearby sites (100 km radius) with precip data
siteNear <- metScanR::siteFinder(siteID = paste0("NEON:",Site), radius = 10, vars = "precipitation")

station_data <- rnoaa::ghcnd_stations()

siteLocaNoaa <- data.frame(id = Site, latitude = metaSite[[Site]]$location$latitude_dec, longitude = metaSite[[Site]]$location$longitude_dec)

# Get all stations within 50 kilometers
dataNoaaSub <- rnoaa::meteo_nearby_stations(lat_lon_df = siteLocaNoaa, station_data = station_data,
                      radius = 50, var = c("PRCP"),
                      year_min = lubridate::year(dateBgn), year_max = lubridate::year(dateEnd))
# Get the closest 10 monitors
dataNoaaSub10 <- rnoaa::meteo_nearby_stations(lat_lon_df = siteLocaNoaa, station_data = station_data,
                      limit = 10, var = c("PRCP"),
                      year_min = lubridate::year(dateBgn), year_max = lubridate::year(dateEnd))

#Pull data from the RNOAA for dateBgn to dateEnd
testData <- rnoaa::meteo_pull_monitors(dataNoaaSub10[[Site]]$id[2],date_min = dateBgn, date_max = dateEnd)

#Read all nearby site data
listData <- lapply(seq_along(dataNoaaSub10[[Site]]$id), function(x){
  rnoaa::meteo_pull_monitors(dataNoaaSub10[[Site]]$id[x],date_min = dateBgn, date_max = dateEnd)
})
#Names of sites
names(listData) <- dataNoaaSub10[[Site]]$id

#Plot the daily precip from the closest GHCND site
ggplot2::qplot(as.POSIXct(testData$date), testData$prcp/10, main = "RNOAA closest site", xlab = "Date", ylab = "Daily precipitation")

#plot NIWO data
ggplot2::ggplot(dataSub, ggplot2::aes(startDateTime,priPrecipBulk)) + ggplot2::geom_point(ggplot2::aes(colour = factor(priPrecipFinalQF))) + ggplot2::ggtitle("NEON NIWO") + ggplot2::xlab("Time") + ggplot2::ylab("precipitation")


#Grab date for summing precip totals
dataSub$Date <- lubridate::date(dataSub$startDateTime)

#Aggregate daily precip from NEON data
dfPrcpDaily <- stats::aggregate(priPrecipBulk~Date, dataSub, sum)

#Remove missing days from one dataset to the other
tmp <- testData[testData$date %in% dfPrcpDaily$Date,]
tmp2 <- dfPrcpDaily[dfPrcpDaily$Date %in% tmp$date,]


