##############################################################################################
#' @title Workflow to create NCAR CLM data set

#' @author
#' David Durden \email{eddy4R.info@gmail.com}

#' @description 
#' Workflow for collating NEON data from API, gap-filling, and packaging in NCAR CLM netcdf format. The data are then uploaded to the S3 bucket.

# changelog and author contributions / copyrights
# David Durden (2019-07-05)
#   original creation
# David Durden (2020-05-31)
# Updating to use neonUtilities for all data retrieval from API
# David Durden (2020-04-24)
# Updating gap-filling strategy
##############################################################################################


#############################################################
##!Dependencies
#############################################################

#devtools::install("rlang@1.0.6")
#Call the R HDF5 Library
packReq <- c("rhdf5", "REddyProc", "ncdf4","reshape2","ggplot2","gridExtra","knitr","naniar", "Rfast", "neonUtilities", "googleCloudStorageR","dplyr", "tidyr","mlegp")

#Install and load all required packages
lapply(packReq, function(x) {
  print(x)
  if(require(x, character.only = TRUE) == FALSE) {
    install.packages(x, dependencies=TRUE, repos='http://cran.rstudio.com/')
    library(x, character.only = TRUE)
  }})


#update neonUtilities
# remove.packages("neonUtilities")
#  install.packages("tidyr", repos ='http://cran.rstudio.com/')
#  detach(rlang)
#  install.packages("rlang", repos ='http://cran.rstudio.com/')
# library(rlang)
# 
# #Install packages from github repos
# devtools::install_github("NEONScience/eddy4R/pack/eddy4R.base")
# devtools::install_github("NEONScience/eddy4R/pack/eddy4R.qaqc")
library(eddy4R.base)
library(eddy4R.qaqc)

#Setup Environment
options(stringsAsFactors=F)

###############################################################################
##!Set options to local vs s3 and define S3 ENV variables
###############################################################################
MethOut <- c("local", "gcs")[2] # CHANGE ME FOR DESIRED CONFIGURATION

if(MethOut == "gcs"){

# S3 Bucket
buck = "neon-ncar"

# Setting up environment
Sys.setenv(
  #Set ENV variables
"GCSPATHUPLDATM" = "NEON/atm/cdeps/v3",
"GCSPATHUPLDEVAL" = "NEON/eval_files/v3",
  "GCS_AUTH_FILE" = "/home/ddurden/eddy/tmp/neon_ncar_writer.json"
)

#Grab needed ENV variable
GcsPathUpldAtm <- base::Sys.getenv("GCSPATHUPLDATM")
GcsPathUpldEval <- base::Sys.getenv("GCSPATHUPLDEVAL")

gcsCred <- Sys.getenv("GCS_AUTH_FILE")

# gcs credentials authorize
tryCatch({googleCloudStorageR::gcs_auth(json_file=gcsCred)},
         error=function(cond) {
           stop(paste0("Failed to authorize GCS credentials by ", key_file))
         })

}#end method GCS


##############################################################################
##!Workflow parameters
##############################################################################
#WhOSBSich NEON site are we grabbing data from (4-letter ID)
Site <- "YELL"
#Which type of data package (expanded or basic)
Pack <- "basic"
#Time averaging period
TimeAgr <- 30
#Beginning date for data grabbing
dateBgn <- "2018-01-01"

#End date for date grabbing
dateEnd <- "2024-06-30"

# Run using less memory (but more time);
# if lowmem == TRUE, how many months of data should stackEddy handle at a time?
lowmem <- FALSE
maxmonths <- 2 
user <- 'David Durden'
methPlot <- TRUE #Should data quality plots be created?

#The version data for the FP standard conversion processing
ver <- paste0("v3/",format(Sys.time(), "%Y%m%d"))
#Base directory for output
DirOutBase <-paste0("~/eddy/data/CLM/",ver)
#Download directory for HDF5 files from the API
DirDnld= c("/home/ddurden/eddy/tmp/CLM",tempdir())[1]


# check environment variables for eddy4R workflow, if it exists grab ENV variables 
if("METHPARAFLOW" %in% base::names(base::Sys.getenv())) {
  Site <- Sys.getenv("SITE")
  dateBgn <- Sys.getenv("DATEBGN") 
  dateEnd <- Sys.getenv("DATEEND")
  DirOutBase <- Sys.getenv("DIROUT")
  lowmem  <- Sys.getenv("LOWMEM")
}


##############################################################################
##!static workflow parameters
##############################################################################

#H5 extraction directory
DirDnld <- paste0(DirDnld,"/", Site)

#H5 extraction directory
DirExtr <- paste0(DirDnld,"/extr/", Site)
#Create input directory for double zip files
DirInp <- paste0(DirExtr,"/inp")
#Append the site to the base output directory
if(DirOutBase == "tmp") DirOutBase <- tempdir()
DirOut <- paste0(DirOutBase, "/", Site)
DirOutAtm <- paste0(DirOutBase, "/", Site, "/atm")
DirOutEval <- paste0(DirOutBase, "/", Site, "/eval")

#Check if directory exists and create if not
if(!dir.exists(DirOutAtm)) dir.create(DirOutAtm, recursive = TRUE)
if(!dir.exists(DirOutEval)) dir.create(DirOutEval, recursive = TRUE)
if(!dir.exists(DirExtr)) dir.create(DirExtr, recursive = TRUE)

#DP number
idDpFlux <- 'DP4.00200.001'

#Set dates for pulling data from API
dateBgn <- as.Date(dateBgn) - lubridate::days(1) #neonUtitilities a month behind
dateEnd <- as.Date(dateEnd)

##############################################################################
##!Flux data download
##############################################################################

# #Testing bucket listing
# listObjGcs <- googleCloudStorageR::gcs_list_objects(bucket = buck, prefix = "NEON/qfVali/")$name
# 
# #Filename for flux QFQM
# fileObjValiGcs <- grep(Site, listObjGcs, value = TRUE)
# 
# #Check if data is available in GCS for the current year
# logiDataGcs <- any(grepl(Site, listObjGcs))

#Check if object exists
#logiDataS3 <- aws.s3::object_exists(object = paste0("qfqm_flux_shiny/v20210104/",Site,"/", Site,"_",lubridate::year(dateEnd),".rds"), bucket = s3Buck)

# if(logiDataGcs == TRUE){
#   #Read in data currently formatted and stored in S3
#   #listDfIn <- aws.s3::s3readRDS(object = paste0("qfqm_flux_shiny/v20210104/",Site,"/", Site,"_",lubridate::year(dateEnd),".rds"), bucket = s3Buck)
#   
#   #temporary directory and local file name
#   DirTmp <- tempdir() 
#   fileObjValiLocl <- paste0(DirTmp,"/", Site,"_qfVali.rds")
#   
#   #Read in data currently formatted and stored in S3
#   googleCloudStorageR::gcs_get_object(object = fileObjValiGcs, bucket = buck, saveToDisk = fileObjValiLocl, overwrite = TRUE)
#   
#   #Read in data from local temp directory after download from GCS  
#   dfVali <- readRDS(file = fileObjValiLocl)
#   
#   #Subset down to data being processed
#   dfValiSub <- dfVali[as.Date(dfVali$Date) >= as.Date(dateBgn) + 1 & as.Date(dfVali$Date) <= as.Date(dateEnd),] 
# }
# 

##############################################################################
##!Flux data download
##############################################################################

#Download zip files
neonUtilities::zipsByProduct(dpID=idDpFlux, package=Pack, 
                             site=Site, 
                             startdate= as.character(dateBgn),
                             enddate= as.character(dateEnd),
                             savepath= DirDnld,
                             include.provisional=TRUE,
                             check.size=F)

#Grab one zip file for the site to extract metadata and unzip
zipFile <- base::list.files(paste0(DirDnld,"/filesToStack00200"), pattern = ".zip", 
                            full.names = TRUE)[1]
utils::unzip(zipFile, exdir = DirExtr)


#ungzip file
gzFile <- list.files(DirExtr, pattern = ".gz", full.names = TRUE)
lapply(gzFile, R.utils::gunzip)

#Get HDF5 filename
fileNameHdf5 <- base::list.files(path = DirExtr, pattern = "*.h5", full.names = TRUE)

##############################################################################
##!Metadata determination
##############################################################################

#Read site level metadata from HDF5
metaSite <- rhdf5::h5readAttributes(file = fileNameHdf5, name = Site)
#Grab latitude and longitude from site metadata
latSite <- metaSite$LatTow #Latitude of tower
lonSite <- 360 + metaSite$LonTow #Longitude of tower (degrees east)
distTowSite <- metaSite$DistZaxsTow #Tower height

#Tower top level in NEON DP number convention
IdHor <- "000"
IdVer <-paste0("0",metaSite$LvlMeasTow,"0")
LvlTowr <- paste0(IdHor,IdVer)

# time difference between local time and UTC
if(!base::is.null(metaSite$ZoneTime)) {
  mapZoneTime <- c("AST" = -4L, "AKST" = -9L, "CST" = -6L, "EST" = -5L, "HST" = -10L, "MST" = -7L, "PST" = -8L, "PST/MST" = -7L)
  
  # time difference between local time and UTC
  metaSite$TimeDiffUtcLst <- ifelse(metaSite$ZoneTime %in% names(mapZoneTime), mapZoneTime[metaSite$ZoneTime], "NA")
}

##############################################################################
##!Flux data read in
##############################################################################
  
#Initialize data List
dataList <- list()

#Read data from downloaded zip files and save it in a dataframe
  if(lowmem == FALSE) {
    # memory-intensive option, but faster
    # Read in all data at one time
    dataList$dp04 <- neonUtilities::stackEddy(filepath=paste0(DirDnld,"/filesToStack00200/"), level = "dp04", avg = 30)
     
    dp01Var <- c("rtioMoleDryH2o", "rtioMoleDryCo2", "veloXaxsYaxsErth","presAtm","tempAir","frt00Samp")                                          
                                                                                       
    dataList$dp01 <- lapply(dp01Var, function(x) {
      neonUtilities::stackEddy(filepath=paste0(DirDnld,"/filesToStack00200/"), 
                                              level = "dp01", avg = 30, var = x)  
    })#End of lapply for dp01
    
    names(dataList$dp01) <- dp01Var
    
    # Create flux data.frame
    dataDfFlux <-   data.frame(
      "TIMESTAMP_START" = as.POSIXlt(dataList$dp04[[Site]]$timeBgn, format="%Y-%m-%dT%H:%M:%OSZ", tz = "GMT"), #Timestamp represents end of period in ReddyProc
      "TIMESTAMP_END" = as.POSIXlt(dataList$dp04[[Site]]$timeEnd, format="%Y-%m-%dT%H:%M:%OSZ", tz = "GMT"),
      "NEE"= dataList$dp04[[Site]]$data.fluxCo2.nsae.flux,#Net ecosystem exchange (turb + stor)
      "qfNEE"= dataList$dp04[[Site]]$qfqm.fluxCo2.nsae.qfFinl,#Net ecosystem exchange (turb + stor)
      "FC"= dataList$dp04[[Site]]$data.fluxCo2.turb.flux,#CO2 flux (turb)
      "qfFC"= dataList$dp04[[Site]]$qfqm.fluxCo2.turb.qfFinl,#CO2 flux (turb)
      "LE" = dataList$dp04[[Site]]$data.fluxH2o.turb.flux, #Latent heat flux (turb)
      "qfLE" = dataList$dp04[[Site]]$qfqm.fluxH2o.turb.qfFinl, #Latent heat flux (turb)
      "LE_NSAE" = dataList$dp04[[Site]]$data.fluxH2o.nsae.flux, #Latent heat flux (nsae)
      "qfLE_NSAE" = dataList$dp04[[Site]]$qfqm.fluxH2o.nsae.qfFinl, #Latent heat flux (nsae)
      "Ustar" = dataList$dp04[[Site]]$data.fluxMome.turb.veloFric, #Friction velocity
      "qfUstar" = dataList$dp04[[Site]]$qfqm.fluxMome.turb.qfFinl, #Friction velocity
      "H" = dataList$dp04[[Site]]$data.fluxTemp.turb.flux, #Sensible heat flux (turb)
      "qfH" = dataList$dp04[[Site]]$qfqm.fluxTemp.turb.qfFinl,#Sensible heat flux quality flag (turb)
      "H_NSAE" = dataList$dp04[[Site]]$data.fluxTemp.nsae.flux,#Sensible heat flux (nsae)
      "qfH_NSAE" = dataList$dp04[[Site]]$qfqm.fluxTemp.nsae.qfFinl,
      "qfTurbFlow" = dataList$dp01$frt00Samp[[Site]][which(dataList$dp01$frt00Samp[[Site]]$verticalPosition == IdVer), "qfqm.h2oTurb.frt00Samp.qfFinl"],
      "qfTurbH2oFinl" = dataList$dp01$rtioMoleDryH2o[[Site]][which(dataList$dp01$rtioMoleDryH2o[[Site]]$verticalPosition == IdVer), "qfqm.h2oTurb.rtioMoleDryH2o.qfFinl"],
      "qfTurbCo2Finl" = dataList$dp01$rtioMoleDryCo2[[Site]][which(dataList$dp01$rtioMoleDryCo2[[Site]]$verticalPosition == IdVer), "qfqm.co2Turb.rtioMoleDryCo2.qfFinl"],
      "WS_MDS" = dataList$dp01$veloXaxsYaxsErth[[Site]][which(dataList$dp01$veloXaxsYaxsErth[[Site]]$verticalPosition == IdVer), "data.soni.veloXaxsYaxsErth.mean"],
      "qfWS_MDS" = dataList$dp01$veloXaxsYaxsErth[[Site]][which(dataList$dp01$veloXaxsYaxsErth[[Site]]$verticalPosition == IdVer), "qfqm.soni.veloXaxsYaxsErth.qfFinl"],
      "presAtmTurb" = dataList$dp01$presAtm[[Site]][which(dataList$dp01$presAtm[[Site]]$verticalPosition == IdVer), "data.co2Turb.presAtm.mean"],
      "qfPresAtmTurb" = dataList$dp01$presAtm[[Site]][which(dataList$dp01$presAtm[[Site]]$verticalPosition == IdVer), "qfqm.co2Turb.presAtm.qfFinl"],
      "presAtmBaro" = dataList$dp01$presAtm[[Site]][which(dataList$dp01$presAtm[[Site]]$verticalPosition == IdVer), "data.presBaro.presAtm.mean"],
      "qfPresAtmBaro" = dataList$dp01$presAtm[[Site]][which(dataList$dp01$presAtm[[Site]]$verticalPosition == IdVer), "qfqm.presBaro.presAtm.qfFinl"],
      "tempAirSoni" = dataList$dp01$tempAir[[Site]][which(dataList$dp01$tempAir[[Site]]$verticalPosition == IdVer), "data.soni.tempAir.mean"], 
      "qfTempAirSoni" = dataList$dp01$tempAir[[Site]][which(dataList$dp01$tempAir[[Site]]$verticalPosition == IdVer), "qfqm.soni.tempAir.qfFinl"],
      "rtioMoleDryH2o" = dataList$dp01$rtioMoleDryH2o[[Site]][which(dataList$dp01$rtioMoleDryH2o[[Site]]$verticalPosition == IdVer), "data.h2oTurb.rtioMoleDryH2o.mean"]
      #"tempAirTop" = dataList$dp01$tempAirTop[[Site]][which(dataList$dp01$tempAirTop[[Site]]$verticalPosition == IdVer), "data.tempAirTop.temp.mean"],
      #"qfTempAirTop" = dataList$dp01$tempAirTop[[Site]][which(dataList$dp01$tempAirTop[[Site]]$verticalPosition == IdVer), "qfqm.tempAirTop.temp.qfFinl"]
     # "timeTempAirTop" = dataList$dp01$tempAirTop[[Site]][which(dataList$dp01$tempAirTop[[Site]]$verticalPosition == IdVer), "timeBgn"]
      , stringsAsFactors = FALSE)

    #Remove dataList to free up space
    rm(dataList)
    invisible(gc()) #Clean up after removing object
  
    } else 
      {
    # Lower memory option: place zips in separate folders, read in data for each subdirectory 
    # and create a dataframe for each. Then concatenate the dataframes.
    
    # make subdirectories with maxmonths months data in each
    writeLines(paste0("Moving zips into subdirectories"))
    ziplist <- list.files(paste0(DirDnld,"/filesToStack00200/"), full.names = TRUE)
    zipgroups <- split(ziplist,ceiling(seq_along(ziplist)/maxmonths))
    
    sapply(names(zipgroups), function(x) {dir.create(paste0(DirDnld,"/filesToStack00200/",x))})
    
    # move zip files into subdirectories
    lapply(seq_along(zipgroups), function(x) { 
      folder <- names(zipgroups)[[x]]
      print(folder)
      newname <- gsub(pattern = "/filesToStack00200/", 
                      replacement = paste0("/filesToStack00200/", folder), zipgroups[[x]])
      file.rename(from = zipgroups[[x]], newname)
      })
    
    # Run stackEddy for each subdirectory
    writeLines(paste0("Extracting data"))
    dataDfFlux_part <- vector(mode = "list", 
                      length = length(list.dirs(paste0(DirDnld,"/filesToStack00200/"), 
                                                               recursive = FALSE)))

    for (i in seq_along(list.dirs(paste0(DirDnld,"/filesToStack00200/"), 
                                  recursive = FALSE))) {
      writeLines(paste0("Getting data from subdirectory ", i, "..."))
      tmpList_04 <- neonUtilities::stackEddy(filepath=paste0(DirDnld,"/filesToStack00200/", i), level = "dp04", avg = 30)
      tmpList_01 <- neonUtilities::stackEddy(filepath=paste0(DirDnld,"/filesToStack00200/", i), level = "dp01", avg = 30)
      
      #Create flux data.frame for each subdirectory
      writeLines(paste0("Adding flux data from folder ", i, " to the flux data.frame"))
      dataDfFlux_part[[i]] <-   data.frame(
        "TIMESTAMP_START" = as.POSIXlt(tmpList_04[[Site]]$timeBgn, format="%Y-%m-%dT%H:%M:%OSZ", tz = "GMT"), #Timestamp represents end of period in ReddyProc
        "TIMESTAMP_END" = as.POSIXlt(tmpList_04[[Site]]$timeEnd, format="%Y-%m-%dT%H:%M:%OSZ", tz = "GMT"),
        "NEE"= tmpList_04[[Site]]$data.fluxCo2.nsae.flux,#Net ecosystem exchange (turb + stor)
        "LE" = tmpList_04[[Site]]$data.fluxH2o.turb.flux, #Latent heat flux (turb)
        "Ustar" = tmpList_04[[Site]]$data.fluxMome.turb.veloFric, #Friction velocity
        "H" = tmpList_04[[Site]]$data.fluxTemp.turb.flux,#Sensible heat flux (turb)
        "qfTurbFlow" = tmpList_01[[Site]][which(tmpList_01[[Site]]$verticalPosition == IdVer), "qfqm.h2oTurb.frt00Samp.qfFinl"],
        "qfTurbH2oFinl" = tmpList_01[[Site]][which(tmpList_01[[Site]]$verticalPosition == IdVer), "qfqm.h2oTurb.rtioMoleDryH2o.qfFinl"],
        "qfTurbCo2Finl" = tmpList_01[[Site]][which(tmpList_01[[Site]]$verticalPosition == IdVer), "qfqm.co2Turb.rtioMoleDryCo2.qfFinl"],
        "WS_MDS" = tmpList_01[[Site]][which(tmpList_01[[Site]]$verticalPosition == IdVer), "data.soni.veloXaxsYaxsErth.mean"],
        "qfWS_MDS" = tmpList_01[[Site]][which(tmpList_01[[Site]]$verticalPosition == IdVer), "qfqm.soni.veloXaxsYaxsErth.qfFinl"],
        "presAtmTurb" = tmpList_01[[Site]][which(tmpList_01[[Site]]$verticalPosition == IdVer), "data.co2Turb.presAtm.mean"],
        "qfPresAtmTurb" = tmpList_01[[Site]][which(tmpList_01[[Site]]$verticalPosition == IdVer), "qfqm.co2Turb.presAtm.qfFinl"],
        "presAtmBaro" = tmpList_01[[Site]][which(tmpList_01[[Site]]$verticalPosition == IdVer), "data.presBaro.presAtm.mean"],
        "qfPresAtmBaro" = tmpList_01[[Site]][which(tmpList_01[[Site]]$verticalPosition == IdVer), "qfqm.presBaro.presAtm.qfFinl"],
        "tempAirSoni" = tmpList_01[[Site]][which(tmpList_01[[Site]]$verticalPosition == IdVer), "data.soni.tempAir.mean"]
        , 
        "qfTempAirSoni" = tmpList_01[[Site]][which(tmpList_01[[Site]]$verticalPosition == IdVer), "qfqm.soni.tempAir.qfFinl"]
        #"tempAirTop" = tmpList_01[[Site]][which(tmpList_01[[Site]]$verticalPosition == IdVer), "data.tempAirTop.temp.mean"],
        #"qfTempAirTop" = tmpList_01[[Site]][which(tmpList_01[[Site]]$verticalPosition == IdVer), "qfqm.tempAirTop.temp.qfFinl"]
        
        , stringsAsFactors = FALSE)
    }#End loop around segments
      
    # clean up
    rm(tmpList_04, tmpList_01)
    
    # combine output
    writeLines(paste0("Combining all ", i, " dataframes into one."))
    dataDfFlux <- do.call(rbind, dataDfFlux_part)
    } #End Low memory logic




##############################################################################
##!Time regularization if needed
##############################################################################  
# Regularize timeseries to 30 minutes in case missing data after CI processing
timeRglr <- eddy4R.base::def.rglr(timeMeas = as.POSIXlt(dataDfFlux$TIMESTAMP_START, tz = "UTC"), 
                                  dataMeas = dataDfFlux, 
                                  BgnRglr = as.POSIXlt(dataDfFlux$TIMESTAMP_START[1], tz = "UTC"), 
                                  EndRglr = as.POSIXlt(dataDfFlux$TIMESTAMP_END[length(dataDfFlux$TIMESTAMP_END)], tz = "UTC"), 
                                  TzRglr = "UTC", FreqRglr = 1/(60*30))
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


dataDfFlux$NEE[(which(dataDfFlux$qfTurbCo2Finl == 1|dataDfFlux$qfWS_MDS == 1))] <- NaN
dataDfFlux$LE[(which(dataDfFlux$qfTurbH2oFinl == 1|dataDfFlux$qfWS_MDS == 1))] <- NaN
dataDfFlux$FC[(which(dataDfFlux$qfTurbCo2Finl == 1|dataDfFlux$qfWS_MDS == 1))] <- NaN
dataDfFlux$H[(which(dataDfFlux$qfTempAirSoni == 1|dataDfFlux$qfWS_MDS == 1))] <- NaN
dataDfFlux$Ustar[(which(dataDfFlux$qfUstar == 1|dataDfFlux$qfWS_MDS == 1))] <- NaN
dataDfFlux$LE_NSAE[(which(dataDfFlux$qfTurbH2oFinl == 1|dataDfFlux$qfWS_MDS == 1))] <- NaN
dataDfFlux$H_NSAE[(which(dataDfFlux$qfTempAirSoni == 1|dataDfFlux$qfWS_MDS == 1))] <- NaN
dataDfFlux$WS_MDS[(which(dataDfFlux$qfWS_MDS == 1))] <- NaN
dataDfFlux$tempAirSoni[(which(dataDfFlux$qfTempAirSoni == 1))] <- NaN
dataDfFlux$rtioMoleDryH2o[(which(dataDfFlux$qfTurbH2oFinl == 1))] <- NaN
#dataDfFlux$tempAirTop[(which(dataDfFlux$qfTempAirTop== 1))] <- NaN
#dataDfFlux$Pa_MDS[(which(dataDfFlux$qfTurbFlow == 1))] <- NaN

#Set Rng thresholds
#assign list
Rng <- list()

Rng$Min <- data.frame(
  "FC" = -100,            #[umol m-2 s-1]
  "NEE" = -100,            #[umol m-2 s-1]
  "LE" = -500,            #[W m-2]
  "LE_NSAE" = -500,            #[W m-2]
  "H" = -500,             #[W m-2]
  "H_NSAE" = -500,             #[W m-2]
  "Ustar" = 0,            #[m s-1]
  "WS_MDS" = 0,         #[m s-1]
  "tempAirSoni" = -55.0       #[C]
 # "tempAirTop" = -55.0       #[C]
)


#Set Max thresholds
Rng$Max <- data.frame(
  "FC" = 100,            #[umol m-2 s-1]
  "NEE" = 100,            #[umol m-2 s-1]
  "LE" = 1000,            #[W m-2]
  "LE_NSAE" = 1000,            #[W m-2]
  "H" = 1000,             #[W m-2]
  "H_NSAE" = 1000,             #[W m-2]
  "Ustar" = 5,            #[m s-1]
  "WS_MDS" = 50,         #[m s-1]
  "tempAirSoni" = 45.0       #[C]
#  "tempAirTop" = 45.0       #[C]
)


#Apply the range test to the output, and replace values with NaN
lapply(names(dataDfFlux), function(x) {
  dataDfFlux[which(dataDfFlux[,x]<Rng$Min[[x]] | dataDfFlux[,x]>Rng$Max[[x]]),x] <<- NaN})

#Remove bad validation data for CO2 fluxes
# for(idx in dfValiSub$Date){
#   #idx <- dfValiSub$Date[321]
#   #test[idx]
#   tmpIdx <- which(as.Date(dataDfFlux$TIMESTAMP) == idx)
#   if(dfValiSub[dfValiSub$Date == idx, "qfVali"] == 1){
#     print(idx)
#     print(tmpIdx)
#     dataDfFlux$NEE[tmpIdx] <- NaN
#     dataDfFlux$FC[tmpIdx] <- NaN
#     #dataDfFlux$NEE[test,] <- NaN
#   }
# }

#List of variables to perform despiking on
varDspk <- c("FC","NEE","LE","LE_NSAE","H","H_NSAE","Ustar","WS_MDS","tempAirSoni","rtioMoleDryH2o")

#Lapply to perform despiking for flux variables
base::lapply(varDspk, function(y){
  print(y)
  #y <- "asrpCo2"
  tmp <- eddy4R.qaqc::def.dspk.br86(
    # input data, univariate vector of integers or numerics
    dataInp = dataDfFlux[[y]],
    # filter width
    WndwFilt = 9, #Default
    # initial number/step size of histogram bins
    NumBin = 2, #Default
    # resolution threshold
    ThshReso = 10 #Default
  ) #Remove high frequency data that is flagged by sensor specific flags or plausibility tests flags
  
  dataDfFlux[[y]] <<- tmp$dataOut
  #return(tmp)
})  #End lapply for variables

#Remove flagging variables from output
dataDfFlux$qfTurbCo2Finl <- NULL
dataDfFlux$qfTurbH2oFinl <- NULL
dataDfFlux$qfTurbFlow <- NULL

#Remove extraction directory
# clean up temp directory
base::file.remove(base::list.files(DirExtr, full.names = TRUE, recursive = TRUE), force = TRUE)
# clean up temp directory
base::file.remove(base::list.files(DirDnld, full.names = TRUE, recursive = TRUE), force = TRUE)

##############################################################################
##!Met data
##############################################################################

#Year and month of interest

#varMetaData <- c("lat" = "LatTow", "lon" = "LonTow", "ZBOT" = "DistZaxsTow" )

#varReqData <- c("TBOT" = "tempAir", "FLDS" =, "FSDS" =, "PRECTmms" = , "PSRF", "RH")
#Convert Level Tower to Met format
#LvlTowrMet <- gsub(pattern = "_", replacement = ".", x = LvlTowr)


#List of DP numbers by eddy4R DP names
listDpNum <- c( "PRECTmms_MDS" = "DP1.00006.001", "rH" = "DP1.00098.001", "FLDS_MDS" = "DP1.00023.001", "Rg" = "DP1.00023.001", "Pa_MDS" = "DP1.00004.001", "TBOT" = "DP1.00003.001", "PAR" = "DP1.00024.001", "SW_DIR" = "DP1.00014.001", "WS_MDS" = "DP1.00001.001")

#names for individual variables of interest
varDp <- c("PRECTmms_MDS" = "SECPRE_30min", "rH" = "RH_30min", "FLDS_MDS" = "SLRNR_30min", "Rg" = "SLRNR_30min", "Pa_MDS" = "BP_30min", "TBOT" = "TAAT_30min", "PAR" = "PARPAR_30min", "SW_DIR" = "SRDDP_30min", "WS_MDS" = "twoDWSD_30min")  #was twoDWSD_30min #Currently using the relative humidity from the soil array, tower top was not reporting data at HARV during this time

#Sub data product variables
subVar <- c("PRECTmms_MDS" = "secPrecipBulk", "rH" = "RHMean", "FLDS_MDS" = "inLWMean", "Rg" = "inSWMean", "Pa_MDS" = "staPresMean", "TBOT" = "tempTripleMean", "PAR" = "PARMean", "SW_DIR" = "gloRadMean", "WS_MDS" = "windSpeedMean")


#Sub data product quality flags
subVarQf <- c("PRECTmms_MDS" = "secPrecipFinalQF", "rH" = "RHFinalQF", "FLDS_MDS" = "inLWFinalQF", "Rg" = "inSWFinalQF", "Pa_MDS" = "staPresFinalQF", "TBOT" = "finalQF", "PAR" = "PARFinalQF", "SW_DIR" = "gloRadFinalQF", "WS_MDS" = "windSpeedFinalQF")

#Grab data for data products using neonUtilities
#neonUtilities::getPackage(site_code = site, package = pack, year_month =  )

##Grab data for data products using Noble package
dataMet <- lapply(listDpNum, function(x){
  #x <- listDpNum[9]
  try(expr = neonUtilities::loadByProduct(site = Site, dpID = x, 
                                          startdate = as.character(dateBgn), 
                                          enddate = as.character(dateEnd), 
                                          package = Pack, timeIndex = TimeAgr, 
                                          include.provisional = TRUE, 
                                          check.size = FALSE), 
      silent = TRUE)
  })

#Check if primary precipitation exists at the site, if not change to secondary precip
varDp["PRECTmms_MDS"] <- ifelse(test = any(grepl(pattern = varDp["PRECTmms_MDS"], x = names(dataMet[["PRECTmms_MDS"]]))), "SECPRE_30min", "PRIPRE_30min")

#Failsafe if using primary precip
subVar["PRECTmms_MDS"] <- ifelse(test = varDp["PRECTmms_MDS"] == "SECPRE_30min", "secPrecipBulk", "priPrecipBulk")
subVarQf["PRECTmms_MDS"] <- ifelse(test = varDp["PRECTmms_MDS"] == "SECPRE_30min", "secPrecipFinalQF", "priPrecipFinalQF")



#Grab the actual data tables
dataMetSub <- lapply(seq_along(varDp), function(x) {
  print(x)
  tmp <- dataMet[[names(varDp[x])]][[grep(pattern = varDp[[x]], x = names(dataMet[[names(varDp[x])]]))]]
  return(tmp)
})

#Name the output lists
names(dataMetSub) <- names(varDp)

#Remove unwanted measurement levels
dataMetSub$Rg_002 <- dataMetSub$Rg[!dataMetSub$Rg$verticalPosition == IdVer,]
dataMetSub$Rg <- dataMetSub$Rg[dataMetSub$Rg$verticalPosition == IdVer,]
dataMetSub$FLDS_MDS_002 <- dataMetSub$FLDS_MDS[!dataMetSub$FLDS_MDS$verticalPosition == IdVer,]
dataMetSub$FLDS_MDS <- dataMetSub$FLDS_MDS[dataMetSub$FLDS_MDS$verticalPosition == IdVer,]
dataMetSub$rH_002 <- dataMetSub$rH[!dataMetSub$rH$horizontalPosition == "003",] #tower top rH
dataMetSub$rH <- dataMetSub$rH[dataMetSub$rH$horizontalPosition == "003"|dataMetSub$rH$horizontalPosition == "002",] #soil plot rH, added 002 for PUUM


dataMetSub$PAR <- dataMetSub$PAR[dataMetSub$PAR$verticalPosition == IdVer,]




dataMetSub$WS_MDS_002 <- dataMetSub$WS_MDS[dataMetSub$WS_MDS$verticalPosition == sprintf("%03d",as.integer(IdVer) - 20),]
dataMetSub$WS_MDS <- dataMetSub$WS_MDS[dataMetSub$WS_MDS$verticalPosition == sprintf("%03d",as.integer(IdVer) - 10),]

#logical statement looking for throughfall precip data
if(any(grepl(pattern = "THRPRE_30min", x = names(dataMet[["PRECTmms_MDS"]])))){
  for(idx in unique(dataMet$PRECTmms_MDS$THRPRE_30min$horizontalPosition)){
    tmpVar <- paste0("PRECTmms_MDS_",idx)
      dataMetSub[[tmpVar]] <- dataMet$PRECTmms_MDS$THRPRE_30min[dataMet$PRECTmms_MDS$THRPRE_30min$horizontalPosition == idx,]
  }#End for loop for Throughfall

}#End of logical statement looking for throughfall precip data

#time regularization of met data
dataMetSubRglr <- lapply(names(dataMetSub), function(x){
timeRglrMet <- eddy4R.base::def.rglr(timeMeas = as.POSIXlt(dataMetSub[[x]]$startDateTime), 
                                     dataMeas = dataMetSub[[x]], 
                                     BgnRglr = dataDfFlux$TIMESTAMP[1] - lubridate::minutes(30), 
                                     EndRglr = dataDfFlux$TIMESTAMP[length(dataDfFlux$TIMESTAMP)] - lubridate::minutes(30), 
                                     TzRglr = "UTC", FreqRglr = 1/(60*30))

return(timeRglrMet$dataRglr)
})#End lapply for time regularization of met data


#Add names to list of Dataframes of regularized data
names(dataMetSubRglr) <- names(dataMetSub)

#######################################################################################################################################################
#Calculate rH from IRGA rtioMoleDryH2o
presH2o <- eddy4R.base::def.pres.h2o.rtio.mole.h2o.dry.pres(rtioMoleDryH2o = eddy4R.base::def.unit.conv(dataDfFlux$rtioMoleDryH2o, unitFrom = c("mmolH2o mol-1Dry"), unitTo = c("molH2o mol-1Dry")), pres = eddy4R.base::def.unit.conv(dataMetSubRglr$Pa_MDS$staPresMean, unitFrom = "kPa", unitTo = "Pa")) #Calculate water vapor pressure from dry mole fraction and static pressure
presH2oSat <- eddy4R.base::def.pres.h2o.sat.temp.mag(temp = as.numeric(eddy4R.base::def.unit.conv(dataMetSubRglr$TBOT$tempTripleMean, unitFrom = "C", unitTo = "K"))) #Calculate saturated water vapor pressure from temperature using Magnus equation
attributes(presH2o)$unit = "Pa"
attributes(presH2oSat)$unit = "Pa"
dataMetSubRglr$rH_003 <-  data.frame("startDateTime" = dataMetSubRglr$Pa_MDS$startDateTime, "RHMean" = eddy4R.base::def.rh.pres.h2o.pres.sat.h2o(presH2o = presH2o, presH2oSat = presH2oSat), "RHFinalQF" = as.integer(dataMetSubRglr$Pa_MDS$staPresFinalQF == 1|dataMetSubRglr$TBOT$finalQF == 1|dataDfFlux$qfLE == 1)) #Calculation of RH from water vapor partial pressure and saturation pressure
########################################################################################################################################################  

#############TODO##############
#May need to remove wiht latest updates
######################################
# #Grab just the Met data of interest for the forcing data
# dataDfMet <- lapply(seq_along(subVar), function(x){
#   #print(x)
#   #Grab the variables of interest
#   tmp <- dataMetSubRglr[[names(subVar[x])]][,grep(pattern = paste0("^",subVar[x]), x = names(dataMetSubRglr[[names(varDp[x])]]))] # use ^ to indicate the pattern starts with the name given
#   return(tmp)
# })
# 
# #Give the ReddyProc names
# names(dataDfMet) <- names(varDp)
# 
# #Calculate precip rate from bulk
# dataDfMet$PRECTmms_MDS <- dataDfMet$PRECTmms_MDS/1800 #1800 sec/0.5 hours
# 
# #Calculate net radiation
# dataDfMet$radNet <-dataMetSubRglr$Rg[["inSWMean"]] - dataMetSubRglr$Rg[["outSWMean"]] + dataMetSubRglr$Rg[["inLWMean"]] - dataMetSubRglr$Rg[["outLWMean"]]
####################################


##############################################################################
##!Combine streams for gapfilling
##############################################################################
#Initialize data lists
dataGf <- list()
qfGf <- list()

#temporary list to deal with precip
tmpList <- list()
tmpList <- dataMetSubRglr[grep("PRECTmms_MDS", names(dataMetSubRglr), value = TRUE)]

dataGf$PRECTmms_MDS <- as.data.frame(sapply(tmpList, function(x){
  #x <- tmpList[[1]] #for testing
  #print(names(x))
  x[,grep("PrecipBulk", names(x))]
}))


#Grab temp data streams
dataGf$Tair <- data.frame("Tair"  = dataMetSubRglr$TBOT$tempTripleMean, "Tair_002"  = dataDfFlux$tempAirSoni, "Tair_003" = dataMetSubRglr$rH$tempRHMean)
#Grab temp qfqm streams
qfGf$Tair <- data.frame("Tair" = dataMetSubRglr$TBOT$finalQF,"Tair_002" = dataDfFlux$qfTempAirSoni, "Tair_003" = dataMetSubRglr$rH$tempRHFinalQF)

#Grab Pa data streams
dataGf$Pa_MDS <- data.frame("Pa_MDS" = dataMetSubRglr$Pa_MDS$staPresMean, "Pa_MDS_002" = dataDfFlux$presAtmTurb)
#Grab temp qfqm streams
qfGf$Pa_MDS <- data.frame("Pa_MDS" = dataMetSubRglr$Pa_MDS$staPresFinalQF, "Pa_MDS_002" = dataDfFlux$qfPresAtmTurb)


#Grab rH data streams
dataGf$rH <- data.frame("rH" = dataMetSubRglr$rH$RHMean, "rH_002" = dataMetSubRglr$rH_002$RHMean, "rH_003" = dataMetSubRglr$rH_003$RHMean)
#Grab rH qfqm streams
qfGf$rH <- data.frame("rH" = dataMetSubRglr$rH$RHFinalQF, "rH_002" = dataMetSubRglr$rH_002$RHFinalQF, "rH_003" = dataMetSubRglr$rH_003$RHFinalQF)

#Grab Rg data streams
dataGf$Rg <- data.frame("Rg" = dataMetSubRglr$Rg$inSWMean, "Rg_002" = dataMetSubRglr$SW_DIR$gloRadMean, "Rg_003" = dataMetSubRglr$PAR$PARMean)
#Grab Rg qfqm streams
qfGf$Rg <- data.frame("Rg" = dataMetSubRglr$Rg$inSWFinalQF, "Rg_002" = dataMetSubRglr$SW_DIR$gloRadFinalQF, "Rg_003" = dataMetSubRglr$PAR$PARFinalQF)

#Grab FLDS data streams
dataGf$FLDS_MDS <- data.frame("FLDS_MDS" = dataMetSubRglr$FLDS_MDS$inLWMean, "FLDS_MDS_002" = dataMetSubRglr$FLDS_MDS_002$inLWMean)
#Grab FLDS qfqm streams
qfGf$FLDS_MDS <- data.frame("FLDS_MDS" = dataMetSubRglr$FLDS_MDS$inLWFinalQF, "FLDS_MDS_002" = dataMetSubRglr$FLDS_MDS_002$inLWFinalQF)

#Grab radNet data streams
dataGf$radNet <- data.frame("radNet" = dataMetSubRglr$Rg[["inSWMean"]] - dataMetSubRglr$Rg[["outSWMean"]] + dataMetSubRglr$Rg[["inLWMean"]] - dataMetSubRglr$Rg[["outLWMean"]]
, "radNet_002" = dataMetSubRglr$SW_DIR$gloRadMean)

#Grab FLDS qfqm streams
qfGf$radNet <- data.frame("radNet" = dataMetSubRglr$Rg$inSWFinalQF, "radNet_002" = dataMetSubRglr$SW_DIR$gloRadFinalQF)

#Grab wind speed data streams
  dataGf$WS_MDS <- data.frame("WS_MDS"  = dataDfFlux$WS_MDS, "WS_MDS_002"  = dataMetSubRglr$WS_MDS$windSpeedMean, "WS_MDS_003" = dataMetSubRglr$WS_MDS_002$windSpeedMean)
#Grab temp qfqm streams
qfGf$WS_MDS <- data.frame("WS_MDS" = dataDfFlux$qfWS_MDS,"WS_MDS_002" = dataMetSubRglr$WS_MDS$windSpeedFinalQF, "WS_MDS_003" = dataMetSubRglr$WS_MDS_002$windSpeedFinalQF)

#Grab Rg data streams
dataGf$PAR <- data.frame("PAR" = dataMetSubRglr$PAR$PARMean, "PAR_002" =  dataMetSubRglr$Rg$inSWMean, "PAR_003" =  dataMetSubRglr$SW_DIR$gloRadMean)
#Grab Rg qfqm streams
qfGf$PAR <- data.frame("PAR" = dataMetSubRglr$PAR$PARFinalQF, "PAR_002" = dataMetSubRglr$Rg$inSWFinalQF, "PAR_003" = dataMetSubRglr$SW_DIR$gloRadFinalQF)

#Grab Rg data streams
dataGf$RadDir <- data.frame("RadDir" = dataMetSubRglr$SW_DIR$dirRadMean, "RadDir_002" =  dataMetSubRglr$SW_DIR$gloRadMean)
#Grab Rg qfqm streams
qfGf$RadDir <- data.frame("RadDir" = dataMetSubRglr$SW_DIR$dirRadFinalQF, "RadDir_002" = dataMetSubRglr$SW_DIR$gloRadFinalQF)

#Grab Rg data streams
dataGf$RadDif <- data.frame("RadDif" = dataMetSubRglr$SW_DIR$difRadMean, "RadDif_002" =  dataMetSubRglr$SW_DIR$gloRadMean)
#Grab Rg qfqm streams
qfGf$RadDif <- data.frame("RadDif" = dataMetSubRglr$SW_DIR$difRadFinalQF, "RadDif_002" = dataMetSubRglr$SW_DIR$gloRadFinalQF)


#Variables to apply quality flag removal to main variable
nameQfVar <- names(dataGf)[!names(dataGf) %in% "PRECTmms_MDS"]

#Remove bad quality flags in main data stream
lapply(nameQfVar, function(x){
  #x <- nameQfVar[1]#for testing
  lapply(names(dataGf[[x]]), function(y){
    #y <- "Tair_002" #for testing
    if(Site %in% c("DELA") & y == "Pa_MDS_002"){
      dataGf[[x]][which(dataGf[[x]][[y]] > 110|dataGf[[x]][[y]] < 80),y]  <<- NaN
    } else if (Site %in% c("RMNP") & y == "Pa_MDS_002"){
      dataGf[[x]][which(dataGf[[x]][[y]] > 90|dataGf[[x]][[y]] < 70),y]  <<- NaN
    } 
    else{
dataGf[[x]][which(qfGf[[x]][[y]] == 1),y] <<- NaN #Put NaN's directly in data, not doing for gap-filling streams due to some over flagging
}
  })#End of lapply around subVars
})#End of lapply around Vars


#Reported gap-filled outputs
rpt <- list()
#Run Replicate stream gap-filling function
rpt <- lapply(names(dataGf), function(x){
  #x <- "Tair" #For testing
 NEON.gf::def.gf.rep(dataGf = dataGf[[x]], NameVarGf = names(dataGf[[x]])[1], NameVarRep = names(dataGf[[x]])[-1])
}) #End lapply around gap-fill function

#Add names to list of data.frames
names(rpt) <- names(dataGf)


##############################################################################
##!Combine flux and met data
##############################################################################
#Grab gap-filled predicitions
dataDfMet <- as.data.frame(sapply(rpt, function(x){
  x[["pred"]]
})) 

#Grab gap-filled flags
qfGfMet <- as.data.frame(sapply(rpt, function(x){
  x[["varFill"]]
}))

#Names
names(qfGfMet) <- paste0(names(qfGfMet),"_fqc")

#Flux variables to report
varRptFlux <- c("NEE", "qfNEE", "FC", "qfFC", "LE", "qfLE", "LE_NSAE", "qfLE_NSAE",  "Ustar", "qfUstar", "H", "qfH", "H_NSAE", "qfH_NSAE","TIMESTAMP") 

#Bind data frames together
dataDf <- cbind(dataDfFlux[,varRptFlux], dataDfMet)

#Calculate precip rate from bulk
dataDf$PRECTmms_MDS <- dataDf$PRECTmms_MDS/1800 #1800 sec/0.5 hours

#Change NA to -9999
dataDf[is.na(dataDf)] <- -9999

#Convert time to ReddyProc format
dataDf$Year <- lubridate::year(dataDf$TIMESTAMP) 
dataDf$DoY <- lubridate::yday(dataDf$TIMESTAMP) 
dataDf$Hour <- lubridate::hour(dataDf$TIMESTAMP) + lubridate::minute(dataDf$TIMESTAMP)/60

#Remove timestamp
dataDf$TIMESTAMP <- NULL

#Vector of units for each variable
unitDf <- c("Year" = "--", "DoY" = "--", "Hour" = "--", "NEE" = "umolm-2s-1", "FC" = "umolm-2s-1", "LE" = "Wm-2", "LE_NSAE" = "Wm-2", "H" = "Wm-2", "H_NSAE" = "Wm-2", "Ustar" = "ms-1", "WS_MDS" = "ms-1", "Pa_MDS" = "kPa", "Tair" = "degC", "PRECTmms_MDS" = "mms-1", "rH" = "%", "FLDS_MDS" = "Wm-2", "Rg" = "Wm-2", "radNet" = "Wm-2", "RadDir" = "Wm-2", "RadDif" = "Wm-2", "PAR" = "umolm-2s-1", "qfNEE" = "NA", "qfFC" = "NA", "qfLE" = "NA", "qfLE_NSAE" = "NA", "qfH" = "NA", "qfH_NSAE" = "NA", "qfUstar" = "NA")

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
conFile
#Close file connection
close(conFile)

##############################################################################
##!ReddyProc workflow
##############################################################################


#  Dir.s <- paste(system.file(package='REddyProc'), 'examples', sep='/')
EddyData.F <- REddyProc::fLoadTXTIntoDataframe(fileOut)

#Threshold bounds to prevent rH > 100%
EddyData.F$rH[EddyData.F$rH > 100] <- 100
#Threshold bounds to prevent Rg < 0
EddyData.F$Rg[EddyData.F$Rg < 0] <- 0
#Threshold bounds to prevent NEE > 50
EddyData.F$NEE[EddyData.F$NEE > 50] <- NA
#Threshold bounds to prevent NEE < -50
EddyData.F$NEE[EddyData.F$NEE < -50] <- NA

EddyData.F$FC[EddyData.F$FC > 50] <- NA
#Threshold bounds to prevent NEE < -50
EddyData.F$FC[EddyData.F$FC < -50] <- NA

#+++ If not provided, calculate VPD from Tair and rH
EddyData.F <- cbind(EddyData.F,VPD=fCalcVPDfromRHandTair(EddyData.F$rH, EddyData.F$Tair))

#+++ Add time stamp in POSIX time format
EddyDataWithPosix.F <- fConvertTimeToPosix(EddyData.F, 'YDH', Year='Year', Day='DoY', Hour='Hour')


#+++ Initalize R5 reference class sEddyProc for processing of eddy data
#+++ with all variables needed for processing later
EddyProc.C <- sEddyProc$new(Site, EddyDataWithPosix.F, c('NEE','Rg','Tair','VPD','rH','FC','LE','LE_NSAE','H', 'H_NSAE','Ustar','Pa_MDS', 'FLDS_MDS','WS_MDS', 'PRECTmms_MDS', 'radNet', 'RadDir', 'RadDif', 'PAR', 'qfNEE', 'qfFC', 'qfLE', 'qfLE_NSAE', 'qfH', 'qfH_NSAE', 'qfUstar'))

#Set location information
EddyProc.C$sSetLocationInfo(LatDeg=latSite, LongDeg=metaSite$LonTow, TimeZoneHour = metaSite$TimeDiffUtcLst)

#+++ Fill gaps in variables with MDS gap filling algorithm (without prior ustar filtering)
EddyProc.C$sMDSGapFill('NEE', FillAll=TRUE) #Fill all values to estimate flux uncertainties
EddyProc.C$sMDSGapFill('FC', FillAll=TRUE)
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
EddyProc.C$sGLFluxPartition()
# SCBI won't calculate GPP, unsure why?
# KONZ won't calculate GPP, 
#      "Detected following columns in dataset to be non numeric: FP_VARnight!"

#+++ Export gap filled and partitioned data to standard data frame
FilledEddyData.F <- EddyProc.C$sExportResults()
EvalDataUnfilled <- EddyProc.C$sExportData()


#Grab just the filled data products & rename variables
dataClm <- FilledEddyData.F[,grep(pattern = "_f$|_DT$", x = names(FilledEddyData.F))]

#Gap-filling flags from ReddyProc
qfClm <- FilledEddyData.F[,grep(pattern = "_fqc$", x = names(FilledEddyData.F))]
#Changing gap-filling qf variable names
#names(qfClm) <- str_remove(names(qfClm), '_fqc')
if(!"GPP_f" %in% names(dataClm)) {
  dataClm$GPP_f <- rep(NaN, nrow(dataClm))
  qfClm$GPP_fqc <- rep(0, nrow(qfClm))
}

#Creating character vector of gap-filling methods used from ReddyProc
qfClm[qfClm == 1] <- "ReddyProc_methA"
qfClm[qfClm == 2] <- "ReddyProc_methB"
qfClm[qfClm == 3] <- "ReddyProc_methC"
qfClm[qfClm == 0] <- NA
#qfGfMet[is.na(qfGfMet)] <- 0

#Save qf names
nameQfClm <- names(qfClm)

#Consolidate gap-filling flags
qfClm <- lapply(names(qfClm), function(x){
  #x <- "NEE" #for testing
ifelse(is.na(qfClm[[x]])& x %in% names(qfGfMet), qfGfMet[[x]], qfClm[[x]]) 
})

#reassigning qf names
names(qfClm) <- nameQfClm

#Change names to match CLM data output
names(qfClm) <- stringr::str_remove(names(qfClm), '_MDS')
names(qfClm) <- stringr::str_replace(names(qfClm), 'Tair','TBOT')
names(qfClm) <- stringr::str_replace(names(qfClm),'WS','WIND')
names(qfClm) <- stringr::str_replace(names(qfClm),'rH','RH')
names(qfClm) <- stringr::str_replace(names(qfClm),'Pa','PSRF')
names(qfClm) <- stringr::str_replace(names(qfClm),'Rg','FSDS')

#numeric flags
qfGfClm <- as.data.frame(qfClm)
#nameGfMeth <- unique(unlist(qfGfClm))
#test <- factor(nameGfMeth)
#qfGfClm[!is.na(qfGfClm)] <- 1
qfGfClm[is.na(qfGfClm)] <- 0
qfGfClm[qfGfClm == "ReddyProc_methA"] <- 2
qfGfClm[qfGfClm == "ReddyProc_methB"] <- 3
qfGfClm[qfGfClm == "ReddyProc_methC"] <- 4

#Turning all redundant data stream gap-fills to 4
lapply(names(qfGfClm), function(x){
  #Testing
  #x <- "TBOT_fqc"
  qfGfClm[[x]][which(nchar(qfGfClm[[x]])>2)] <<- 1
})

#Convert to numeric
qfGfClm <- as.data.frame(sapply(qfGfClm,as.numeric))


#Grab the POSIX timestamp
dataClm$DateTime <- EddyDataWithPosix.F$DateTime - lubridate::minutes(30) # putting back to time at the beginning of the measurement period

#Changing data variable names
names(dataClm) <- stringr::str_remove(names(dataClm), '_f')
names(dataClm) <- stringr::str_remove(names(dataClm), '_MDS')
names(dataClm) <- stringr::str_replace(names(dataClm), 'Tair','TBOT')
names(dataClm) <- stringr::str_replace(names(dataClm),'WS','WIND')
names(dataClm) <- stringr::str_replace(names(dataClm),'rH','RH')
names(dataClm) <- stringr::str_replace(names(dataClm),'Pa','PSRF')
names(dataClm) <- stringr::str_replace(names(dataClm),'Rg','FSDS')
names(dataClm)

#Convert degC to K for temperature
dataClm$TBOT <- dataClm$TBOT + 273.15
attributes(obj = dataClm$TBOT)$units <- "K"

#Convert kPa to Pa for pressure
dataClm$PSRF <- dataClm$PSRF * 1000.0
attributes(obj = dataClm$PSRF)$units <- "Pa"

#Create tower height measurement field
dataClm$ZBOT <- rep(as.numeric(distTowSite),nrow(dataClm))
attributes(obj = dataClm$ZBOT)$units <- "m"

#Year month combination for data filtering
dataClm$yearMon <- strftime(dataClm$DateTime, "%Y-%m", tz='UTC')

dataClm <- cbind(dataClm,EvalDataUnfilled[,c("RadDif","RadDir","qfNEE", "qfFC", "qfLE", "LE_NSAE", "qfLE_NSAE", "qfUstar", "qfH", "H_NSAE", "qfH_NSAE")])

#Combine data with qap-filling quality flags
dataClm <- cbind(dataClm, qfGfClm)

##############################################################################
##!Write CLM output
##############################################################################

#Define missing value fill
mv <- -9999.  
# startStep <- 1

#Set of year/month combinations for netCDF output
setYearMon <- unique(strftime(dataClm$DateTime, "%Y-%m", tz='UTC'))

  for (m in setYearMon) {
    #m <- setYearMon[1] #for testing
    Data.mon <- dataClm[dataClm$yearMon == m,]
    timeStep <- seq(0,nrow(Data.mon)-1,1)
    time     <- timeStep/48
    #endStep  <- startStep + nsteps[m]-1
    # not sure why DateTime[1] doesn't include h:m:s
    tempTime <- Data.mon$DateTime[1]
    tempTime <- format(tempTime,'%Y-%m-%d %H:%M:%S')
    
    print(paste(m,"Data date =",tempTime))
    names(Data.mon)
  
#NetCDF output filenames
fileOutAtm <- paste(DirOutAtm,"/",Site,"_atm_",m,".nc", sep = "")
fileOutEval <- paste(DirOutEval,"/",Site,"_eval_",m,".nc", sep = "")
  #sub(pattern = ".txt", replacement = ".nc", fileOut)

DirOut

# define the netcdf coordinate variables (name, units, type)
lat  <- ncdf4::ncdim_def("lat","degrees_north", as.double(latSite), create_dimvar=TRUE)
lon <- ncdf4::ncdim_def("lon","degrees_east", as.double(lonSite), create_dimvar=TRUE)

#Character dimension for flags
#dimnchar <- ncdim_def("nchar", "", 1:17, create_dimvar=FALSE )

#Variables to output to netCDF
time <- ncdf4::ncdim_def("time", paste("days since",tempTime), calendar = "gregorian",
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
FC <- ncdf4::ncvar_def("FC", "umolm-2s-1", list(lon,lat,time), mv,
                        longname="turbulent CO2 flux", prec="double")
FSH  <- ncdf4::ncvar_def("FSH", "Wm-2", list(lon,lat,time), mv,
                          longname="sensible heat flux", prec="double")
EFLX_LH_TOT  <- ncdf4::ncvar_def("EFLX_LH_TOT", "Wm-2", list(lon,lat,time), mv,
                                 longname="latent heat flux", prec="double")
FSH_NSAE  <- ncdf4::ncvar_def("FSH_NSAE", "Wm-2", list(lon,lat,time), mv,
                         longname="sensible heat flux net surface atmosphere exchange (turbulent + storage)", prec="double")
EFLX_LH_TOT_NSAE  <- ncdf4::ncvar_def("EFLX_LH_TOT_NSAE", "Wm-2", list(lon,lat,time), mv, longname="latent heat flux net surface atmosphere exchange (turbulent + storage)", prec="double")
GPP <- ncdf4::ncvar_def("GPP", "umolm-2s-1", list(lon,lat,time), mv,
                        longname="gross primary productivity calculated using nighttime partitioning method following Reichstein et al. (2005)", prec="double")
GPP_DT <- ncdf4::ncvar_def("GPP_DT", "umolm-2s-1", list(lon,lat,time), mv,
                        longname="gross primary productivity calculated using daytime partitioning method following Lasslop et al. (2010)", prec="double")
Reco_DT <- ncdf4::ncvar_def("Reco_DT", "umolm-2s-1", list(lon,lat,time), mv,
                           longname="Ecosystem respiration calculated using daytime partitioning method following Lasslop et al. (2010)", prec="double")
Ustar <- ncdf4::ncvar_def("Ustar", "m/s", list(lon,lat,time), mv,
                        longname="friction velocity", prec="double")
VPD <- ncdf4::ncvar_def("VPD", "m/s", list(lon,lat,time), mv,
                          longname="vapor pressure deficit", prec="double")
Rnet  <- ncdf4::ncvar_def("Rnet", "W/m^2", list(lon,lat,time), mv,
                          longname="net radiation", prec="double")
RadDif <- ncdf4::ncvar_def("RadDif", "W/m^2", list(lon,lat,time), mv,
                          longname="diffuse radiation", prec="double")
RadDir  <- ncdf4::ncvar_def("RadDir", "W/m^2", list(lon,lat,time), mv,
                          longname="direct radiation", prec="double")
#quality flag variables
qfNEE <- ncdf4::ncvar_def("qfNEE", "NA", list(lon,lat,time), mv,
                        longname="net ecosystem exchange final quality flag, 0 = good data and 1 = flagged data", prec="integer")
qfFC <- ncdf4::ncvar_def("qfFC", "NA", list(lon,lat,time), mv,
                       longname="turbulent CO2 flux final quality flag, 0 = good data and 1 = flagged data", prec="integer")
qfFSH  <- ncdf4::ncvar_def("qfFSH", "NA", list(lon,lat,time), mv,
                         longname="sensible heat flux final quality flag, 0 = good data and 1 = flagged data", prec="integer")
qfEFLX_LH_TOT  <- ncdf4::ncvar_def("qfEFLX_LH_TOT", "NA", list(lon,lat,time), mv,
                                 longname="latent heat flux final quality flag, 0 = good data and 1 = flagged data", prec="integer")
qfFSH_NSAE  <- ncdf4::ncvar_def("qfFSH_NSAE", "NA", list(lon,lat,time), mv,
                              longname="sensible heat flux net surface atmosphere exchange (turbulent + storage) final quality flag, 0 = good data and 1 = flagged data", prec="integer")

qfEFLX_LH_TOT_NSAE  <- ncdf4::ncvar_def("qfEFLX_LH_TOT_NSAE", "NA", list(lon,lat,time), mv, longname="latent heat flux net surface atmosphere exchange (turbulent + storage) final quality flag, 0 = good data and 1 = flagged data", prec="integer")

#gap-filling quality flag variables
FLDS_fqc  <- ncdf4::ncvar_def("FLDS_fqc", "NA", list(lon,lat,time),
                          longname="incident longwave (FLDS) gap-filling flag", prec="integer")
FSDS_fqc  <- ncdf4::ncvar_def("FSDS_fqc", "NA", list(lon,lat,time),
                          longname="incident shortwave (FSDS) gap-filling flag", prec="integer")
PRECTmms_fqc <- ncdf4::ncvar_def("PRECTmms_fqc", "NA", list(lon,lat,time), 
                             longname="precipitation (PRECTmms) gap-filling flag", prec="integer")
PSRF_fqc  <- ncdf4::ncvar_def("PSRF_fqc", "NA", list(lon,lat,time),
                          longname="pressure at the lowest atmospheric level (PSRF) gap-filling flag", prec="integer")
RH_fqc    <- ncdf4::ncvar_def("RH_fqc", "NA", list(lon,lat,time), 
                          longname="relative humidity at lowest atm level (RH) gap-filling flag", prec="integer")
TBOT_fqc  <- ncdf4::ncvar_def("TBOT_fqc", "NA", list(lon,lat,time),
                          longname="temperature at lowest atm level (TBOT) gap-filling flag", prec="integer")
WIND_fqc  <- ncdf4::ncvar_def("WIND_fqc", "NA", list(lon,lat,time), 
                          longname="wind at lowest atm level (WIND) gap-filling flag", prec="integer")
NEE_fqc <- ncdf4::ncvar_def("NEE_fqc", "NA", list(lon,lat,time), 
                        longname="net ecosystem exchange gap-filling flag", prec="integer")
FSH_fqc  <- ncdf4::ncvar_def("FSH_fqc", "NA", list(lon,lat,time),
                         longname="sensible heat flux gap-filling flag", prec="integer")
EFLX_LH_TOT_fqc  <- ncdf4::ncvar_def("EFLX_LH_TOT_fqc", "NA", list(lon,lat,time),
                                 longname="latent heat flux gap-filling flag", prec="integer")
GPP_fqc <- ncdf4::ncvar_def("GPP_fqc", "NA", list(lon,lat,time),
                        longname="gross primary productivity gap-filling flag", prec="integer")
Ustar_fqc <- ncdf4::ncvar_def("Ustar_fqc", "NA", list(lon,lat,time),
                            longname="friction velocity gap-filling flag", prec="integer")
VPD_fqc <- ncdf4::ncvar_def("VPD_fqc", "NA", list(lon,lat,time),
                              longname="vapor pressure deficit gap-filling flag", prec="integer")
Rnet_fqc  <- ncdf4::ncvar_def("Rnet_fqc", "NA", list(lon,lat,time),
                          longname="net radiation gap-filling flag", prec="integer")

#Create the output file
ncAtm <- ncdf4::nc_create(fileOutAtm, list(LATIXY,LONGXY,FLDS,FSDS,PRECTmms,RH,PSRF,TBOT,WIND,ZBOT,FLDS_fqc,FSDS_fqc,PRECTmms_fqc,RH_fqc,PSRF_fqc,TBOT_fqc,WIND_fqc))

ncEval <- ncdf4::nc_create(fileOutEval, list(LATIXY,LONGXY,NEE,FC,FSH,EFLX_LH_TOT,FSH_NSAE,EFLX_LH_TOT_NSAE,GPP,GPP_DT,Reco_DT,Ustar,VPD,Rnet,RadDif,RadDir,ZBOT,NEE_fqc,FSH_fqc,EFLX_LH_TOT_fqc,GPP_fqc,Ustar_fqc,VPD_fqc,Rnet_fqc,qfNEE,qfFC,qfFSH,qfEFLX_LH_TOT,qfFSH_NSAE,qfEFLX_LH_TOT_NSAE))


# Write some values to this variable on disk.
 ncdf4::ncvar_put(ncAtm, LATIXY, latSite)
 ncdf4::ncvar_put(ncAtm, LONGXY, lonSite)
 ncdf4::ncvar_put(ncAtm, FLDS, Data.mon$FLDS)
 ncdf4::ncvar_put(ncAtm, FSDS, Data.mon$FSDS)
 ncdf4::ncvar_put(ncAtm, RH,   Data.mon$RH)
 ncdf4::ncvar_put(ncAtm, PRECTmms, Data.mon$PRECTmms)
 ncdf4::ncvar_put(ncAtm, PSRF, Data.mon$PSRF)
 ncdf4::ncvar_put(ncAtm, TBOT, Data.mon$TBOT)
 ncdf4::ncvar_put(ncAtm, WIND, Data.mon$WIND)
 ncdf4::ncvar_put(ncAtm, ZBOT, Data.mon$ZBOT)
 ncdf4::ncvar_put(ncAtm, FLDS_fqc, Data.mon$FLDS_fqc)
 ncdf4::ncvar_put(ncAtm, FSDS_fqc, Data.mon$FSDS_fqc)
 ncdf4::ncvar_put(ncAtm, RH_fqc,   Data.mon$RH_fqc)
 ncdf4::ncvar_put(ncAtm, PRECTmms_fqc, Data.mon$PRECTmms_fqc)
 ncdf4::ncvar_put(ncAtm, PSRF_fqc, Data.mon$PSRF_fqc)
 ncdf4::ncvar_put(ncAtm, TBOT_fqc, Data.mon$TBOT_fqc)
 ncdf4::ncvar_put(ncAtm, WIND_fqc, Data.mon$WIND_fqc)
 
 #add attributes
 #ncdf4::ncatt_put(ncAtm, time,"calendar", "gregorian" ,prec=NA,verbose=FALSE,definemode=FALSE )
 ncdf4::ncatt_put(ncAtm, FLDS,"mode","time-dependent" ,prec=NA,verbose=FALSE,definemode=FALSE )
 ncdf4::ncatt_put(ncAtm, FSDS,"mode","time-dependent" ,prec=NA,verbose=FALSE,definemode=FALSE )
 ncdf4::ncatt_put(ncAtm, RH  ,"mode","time-dependent" ,prec=NA,verbose=FALSE,definemode=FALSE )
 ncdf4::ncatt_put(ncAtm, PRECTmms,"mode","time-dependent" ,prec=NA,verbose=FALSE,definemode=FALSE )
 ncdf4::ncatt_put(ncAtm, PSRF,"mode","time-dependent" ,prec=NA,verbose=FALSE,definemode=FALSE )
 ncdf4::ncatt_put(ncAtm, TBOT,"mode","time-dependent" ,prec=NA,verbose=FALSE,definemode=FALSE )
 ncdf4::ncatt_put(ncAtm, WIND,"mode","time-dependent" ,prec=NA,verbose=FALSE,definemode=FALSE )
 ncdf4::ncatt_put(ncAtm, ZBOT,"mode","time-dependent" ,prec=NA,verbose=FALSE,definemode=FALSE )
 ncdf4::ncatt_put(ncAtm, FLDS_fqc,"mode","time-dependent" ,prec=NA,verbose=FALSE,definemode=FALSE )
 ncdf4::ncatt_put(ncAtm, FSDS_fqc,"mode","time-dependent" ,prec=NA,verbose=FALSE,definemode=FALSE )
 ncdf4::ncatt_put(ncAtm, RH_fqc,"mode","time-dependent" ,prec=NA,verbose=FALSE,definemode=FALSE )
 ncdf4::ncatt_put(ncAtm, PRECTmms_fqc,"mode","time-dependent" ,prec=NA,verbose=FALSE,definemode=FALSE )
 ncdf4::ncatt_put(ncAtm, PSRF_fqc,"mode","time-dependent" ,prec=NA,verbose=FALSE,definemode=FALSE )
 ncdf4::ncatt_put(ncAtm, TBOT_fqc,"mode","time-dependent" ,prec=NA,verbose=FALSE,definemode=FALSE )
 ncdf4::ncatt_put(ncAtm, WIND_fqc,"mode","time-dependent" ,prec=NA,verbose=FALSE,definemode=FALSE )
 ncdf4::ncatt_put(ncAtm, FLDS_fqc,"method_gap-fill","0=no gap-filling, 1=regression, 2=ReddyProc_methA, 3=ReddyProc_methB, 4=ReddyProc_methC" ,prec=NA,verbose=FALSE,definemode=FALSE)
 ncdf4::ncatt_put(ncAtm, FSDS_fqc,"method_gap-fill","0=no gap-filling, 1=regression, 2=ReddyProc_methA, 3=ReddyProc_methB, 4=ReddyProc_methC" ,prec=NA,verbose=FALSE,definemode=FALSE)
 ncdf4::ncatt_put(ncAtm, RH_fqc,"method_gap-fill","0=no gap-filling, 1=regression, 2=ReddyProc_methA, 3=ReddyProc_methB, 4=ReddyProc_methC" ,prec=NA,verbose=FALSE,definemode=FALSE)
 ncdf4::ncatt_put(ncAtm, PRECTmms_fqc,"method_gap-fill","0=no gap-filling, 1=regression, 2=ReddyProc_methA, 3=ReddyProc_methB, 4=ReddyProc_methC" ,prec=NA,verbose=FALSE,definemode=FALSE)
 ncdf4::ncatt_put(ncAtm, PSRF_fqc,"method_gap-fill","0=no gap-filling, 1=regression, 2=ReddyProc_methA, 3=ReddyProc_methB, 4=ReddyProc_methC" ,prec=NA,verbose=FALSE,definemode=FALSE)
 ncdf4::ncatt_put(ncAtm, TBOT_fqc,"method_gap-fill","0=no gap-filling, 1=regression, 2=ReddyProc_methA, 3=ReddyProc_methB, 4=ReddyProc_methC" ,prec=NA,verbose=FALSE,definemode=FALSE)
 ncdf4::ncatt_put(ncAtm, WIND_fqc,"method_gap-fill","0=no gap-filling, 1=regression, 2=ReddyProc_methA, 3=ReddyProc_methB, 4=ReddyProc_methC" ,prec=NA,verbose=FALSE,definemode=FALSE)
 
 ncdf4::ncatt_put(ncAtm, 0, "created_on",date()      ,prec=NA,verbose=FALSE,definemode=FALSE )
 ncdf4::ncatt_put(ncAtm, 0, "created_by",user,prec=NA,verbose=FALSE,definemode=FALSE )
 ncdf4::ncatt_put(ncAtm, 0, "created_from",fileOut   ,prec=NA,verbose=FALSE,definemode=FALSE )
 ncdf4::ncatt_put(ncAtm, 0, "NEON site",Site         ,prec=NA,verbose=FALSE,definemode=FALSE )
 ncdf4::ncatt_put(ncAtm, 0, "TimeDiffUtcLt",metaSite$TimeDiffUtcLst          ,prec=NA,verbose=FALSE,definemode=FALSE )
 ncdf4::ncatt_put(ncAtm, 0, "created_with", "flow.api.clm.R",prec=NA,verbose=FALSE,definemode=FALSE )
 ncdf4::ncatt_put(ncAtm, 0, "supported_by", "This data development was funded by the National Science Foundation (NSF) solicitation PD 20-7684",prec=NA,verbose=FALSE,definemode=FALSE )
 
 
 # Write some values to this variable on disk.
 ncdf4::ncvar_put(ncEval, LATIXY, latSite)
 ncdf4::ncvar_put(ncEval, LONGXY, lonSite) 
 ncdf4::ncvar_put(ncEval, NEE, Data.mon$NEE)
 ncdf4::ncvar_put(ncEval, FC, Data.mon$FC)
 ncdf4::ncvar_put(ncEval, FSH, Data.mon$H)
 ncdf4::ncvar_put(ncEval, FSH_NSAE, Data.mon$H_NSAE)
 ncdf4::ncvar_put(ncEval, EFLX_LH_TOT, Data.mon$LE)
 ncdf4::ncvar_put(ncEval, EFLX_LH_TOT_NSAE, Data.mon$LE_NSAE)
 ncdf4::ncvar_put(ncEval, GPP, Data.mon$GPP)
 ncdf4::ncvar_put(ncEval, GPP_DT, Data.mon$GPP_DT)
 ncdf4::ncvar_put(ncEval, Reco_DT, Data.mon$Reco_DT)
 ncdf4::ncvar_put(ncEval, Ustar, Data.mon$Ustar)
 ncdf4::ncvar_put(ncEval, VPD, Data.mon$VPD)
 ncdf4::ncvar_put(ncEval, Rnet, Data.mon$radNet)
 ncdf4::ncvar_put(ncEval, RadDir, Data.mon$RadDir)
 ncdf4::ncvar_put(ncEval, RadDif, Data.mon$RadDif)
 ncdf4::ncvar_put(ncEval, ZBOT, Data.mon$ZBOT)
 ncdf4::ncvar_put(ncEval, NEE_fqc, Data.mon$NEE_fqc)
 ncdf4::ncvar_put(ncEval, FSH_fqc, Data.mon$H_fqc)
 ncdf4::ncvar_put(ncEval, EFLX_LH_TOT_fqc, Data.mon$LE_fqc)
 ncdf4::ncvar_put(ncEval, GPP_fqc, Data.mon$GPP_fqc)
 ncdf4::ncvar_put(ncEval, Ustar_fqc, Data.mon$Ustar_fqc)
 ncdf4::ncvar_put(ncEval, VPD_fqc, Data.mon$VPD_fqc)
 ncdf4::ncvar_put(ncEval, Rnet_fqc, Data.mon$radNet_fqc)
 ncdf4::ncvar_put(ncEval, qfNEE, Data.mon$qfNEE)
 ncdf4::ncvar_put(ncEval, qfFC, Data.mon$qfFC)
 ncdf4::ncvar_put(ncEval, qfFSH, Data.mon$qfH)
 ncdf4::ncvar_put(ncEval, qfFSH_NSAE, Data.mon$qfH_NSAE)
 ncdf4::ncvar_put(ncEval, qfEFLX_LH_TOT, Data.mon$qfLE)
 ncdf4::ncvar_put(ncEval, qfEFLX_LH_TOT_NSAE, Data.mon$qfLE_NSAE)


ncdf4::ncatt_put(ncEval, NEE,"mode","time-dependent" ,prec=NA,verbose=FALSE,definemode=FALSE )
ncdf4::ncatt_put(ncEval, FC,"mode","time-dependent" ,prec=NA,verbose=FALSE,definemode=FALSE )
ncdf4::ncatt_put(ncEval, FSH,"mode","time-dependent" ,prec=NA,verbose=FALSE,definemode=FALSE )
ncdf4::ncatt_put(ncEval, FSH_NSAE,"mode","time-dependent" ,prec=NA,verbose=FALSE,definemode=FALSE )
ncdf4::ncatt_put(ncEval, EFLX_LH_TOT,"mode","time-dependent" ,prec=NA,verbose=FALSE,definemode=FALSE )
ncdf4::ncatt_put(ncEval, EFLX_LH_TOT_NSAE,"mode","time-dependent" ,prec=NA,verbose=FALSE,definemode=FALSE )
ncdf4::ncatt_put(ncEval, GPP,"mode","time-dependent" ,prec=NA,verbose=FALSE,definemode=FALSE )
ncdf4::ncatt_put(ncEval, GPP_DT,"mode","time-dependent" ,prec=NA,verbose=FALSE,definemode=FALSE )
ncdf4::ncatt_put(ncEval, Reco_DT,"mode","time-dependent" ,prec=NA,verbose=FALSE,definemode=FALSE )
ncdf4::ncatt_put(ncEval, Ustar,"mode","time-dependent" ,prec=NA,verbose=FALSE,definemode=FALSE )
ncdf4::ncatt_put(ncEval, VPD,"mode","time-dependent" ,prec=NA,verbose=FALSE,definemode=FALSE )
ncdf4::ncatt_put(ncEval, Rnet,"mode","time-dependent",prec=NA,verbose=FALSE,definemode=FALSE )
ncdf4::ncatt_put(ncEval, RadDif,"mode","time-dependent",prec=NA,verbose=FALSE,definemode=FALSE )
ncdf4::ncatt_put(ncEval, RadDir,"mode","time-dependent",prec=NA,verbose=FALSE,definemode=FALSE )
ncdf4::ncatt_put(ncEval, ZBOT,"mode","time-dependent" ,prec=NA,verbose=FALSE,definemode=FALSE )
ncdf4::ncatt_put(ncEval, NEE_fqc,"mode","time-dependent" ,prec=NA,verbose=FALSE,definemode=FALSE )
ncdf4::ncatt_put(ncEval, FSH_fqc,"mode","time-dependent" ,prec=NA,verbose=FALSE,definemode=FALSE )
ncdf4::ncatt_put(ncEval, EFLX_LH_TOT_fqc,"mode","time-dependent" ,prec=NA,verbose=FALSE,definemode=FALSE )
ncdf4::ncatt_put(ncEval, GPP_fqc,"mode","time-dependent" ,prec=NA,verbose=FALSE,definemode=FALSE )
ncdf4::ncatt_put(ncEval, Ustar_fqc,"mode","time-dependent" ,prec=NA,verbose=FALSE,definemode=FALSE )
ncdf4::ncatt_put(ncEval, VPD_fqc,"mode","time-dependent" ,prec=NA,verbose=FALSE,definemode=FALSE )
ncdf4::ncatt_put(ncEval, Rnet_fqc,"mode","time-dependent",prec=NA,verbose=FALSE,definemode=FALSE )
ncdf4::ncatt_put(ncEval, NEE_fqc,"method_gap-fill","0=no gap-filling, 1=regression, 2=ReddyProc_methA, 3=ReddyProc_methB, 4=ReddyProc_methC" ,prec=NA,verbose=FALSE,definemode=FALSE)
ncdf4::ncatt_put(ncEval, FSH_fqc,"method_gap-fill","0=no gap-filling, 1=regression, 2=ReddyProc_methA, 3=ReddyProc_methB, 4=ReddyProc_methC" ,prec=NA,verbose=FALSE,definemode=FALSE)
ncdf4::ncatt_put(ncEval, EFLX_LH_TOT_fqc,"method_gap-fill","0=no gap-filling, 1=regression, 2=ReddyProc_methA, 3=ReddyProc_methB, 4=ReddyProc_methC" ,prec=NA,verbose=FALSE,definemode=FALSE)
ncdf4::ncatt_put(ncEval, GPP_fqc,"method_gap-fill","0=no gap-filling, 1=regression, 2=ReddyProc_methA, 3=ReddyProc_methB, 4=ReddyProc_methC" ,prec=NA,verbose=FALSE,definemode=FALSE)
ncdf4::ncatt_put(ncEval, Ustar_fqc,"method_gap-fill","0=no gap-filling, 1=regression, 2=ReddyProc_methA, 3=ReddyProc_methB, 4=ReddyProc_methC" ,prec=NA,verbose=FALSE,definemode=FALSE)
ncdf4::ncatt_put(ncEval, VPD_fqc,"method_gap-fill","0=no gap-filling, 1=regression, 2=ReddyProc_methA, 3=ReddyProc_methB, 4=ReddyProc_methC" ,prec=NA,verbose=FALSE,definemode=FALSE)
ncdf4::ncatt_put(ncEval, Rnet_fqc,"method_gap-fill","0=no gap-filling, 1=regression, 2=ReddyProc_methA, 3=ReddyProc_methB, 4=ReddyProc_methC" ,prec=NA,verbose=FALSE,definemode=FALSE)

#Quality flag attributes
ncdf4::ncatt_put(ncEval, qfNEE,"mode","time-dependent" ,prec=NA,verbose=FALSE,definemode=FALSE )
ncdf4::ncatt_put(ncEval, qfFC,"mode","time-dependent" ,prec=NA,verbose=FALSE,definemode=FALSE )
ncdf4::ncatt_put(ncEval, qfFSH,"mode","time-dependent" ,prec=NA,verbose=FALSE,definemode=FALSE )
ncdf4::ncatt_put(ncEval, qfFSH_NSAE,"mode","time-dependent" ,prec=NA,verbose=FALSE,definemode=FALSE )
ncdf4::ncatt_put(ncEval, qfEFLX_LH_TOT,"mode","time-dependent" ,prec=NA,verbose=FALSE,definemode=FALSE )
ncdf4::ncatt_put(ncEval, qfEFLX_LH_TOT_NSAE,"mode","time-dependent" ,prec=NA,verbose=FALSE,definemode=FALSE )

#Global attributes
ncdf4::ncatt_put(ncEval, 0, "created_on",date()      ,prec=NA,verbose=FALSE,definemode=FALSE )
ncdf4::ncatt_put(ncEval, 0, "created_by",user,prec=NA,verbose=FALSE,definemode=FALSE )
ncdf4::ncatt_put(ncEval, 0, "created_from",fileOut   ,prec=NA,verbose=FALSE,definemode=FALSE )
ncdf4::ncatt_put(ncEval, 0, "NEON site",Site         ,prec=NA,verbose=FALSE,definemode=FALSE )
ncdf4::ncatt_put(ncEval, 0, "TimeDiffUtcLt",metaSite$TimeDiffUtcLst         ,prec=NA,verbose=FALSE,definemode=FALSE)
ncdf4::ncatt_put(ncEval, 0, "created_with", "flow.api.clm.R",prec=NA,verbose=FALSE,definemode=FALSE )
ncdf4::ncatt_put(ncEval, 0, "supported_by", "This data development was funded by the National Science Foundation (NSF) solicitation PD 20-7684",prec=NA,verbose=FALSE,definemode=FALSE )

#Close Netcdf file connection
ncdf4::nc_close(ncAtm)
ncdf4::nc_close(ncEval)

remove(time, timeStep, fileOutAtm, fileOutEval, ncAtm, ncEval, Data.mon,
       FLDS,FSDS,RH,PRECTmms,PSRF,TBOT,WIND,ZBOT, NEE,FC, FSH, FSH_NSAE, EFLX_LH_TOT, EFLX_LH_TOT_NSAE,GPP,GPP_DT,Reco_DT, Ustar,VPD, Rnet,RadDif,RadDir,
       FLDS_fqc,FSDS_fqc,RH_fqc,PRECTmms_fqc,PSRF_fqc,TBOT_fqc,WIND_fqc, NEE_fqc, FSH_fqc,EFLX_LH_TOT_fqc, GPP_fqc, Ustar_fqc,VPD_fqc, Rnet_fqc,qfNEE,qfFC,qfFSH,qfFSH_NSAE,qfEFLX_LH_TOT,qfEFLX_LH_TOT_NSAE)
  } #End of monthloop

#} #End of year loop
###############################################################################
##!Output to S3
###############################################################################
#Should data be written out to S3
if(MethOut == "gcs"){
  #Grab all output files names
  fileOutAtm <- base::list.files(path = DirOutAtm, pattern = ".nc")
  fileOutEval <- base::list.files(path = DirOutEval, pattern = ".nc")
  
  GcsPathUpldAtm <- paste0(GcsPathUpldAtm,"/",Site)
  GcsPathUpldEval <- paste0(GcsPathUpldEval,"/",Site)
  
  #Upload Atm files to S3
  lapply(fileOutAtm, function(x){
   # x <- fileOutAtm[1] #for testing
    print(x)
    #Function to upload to ECS
    googleCloudStorageR::gcs_upload(file = paste0(DirOutAtm,"/",x), name=paste0(GcsPathUpldAtm,"/",x), bucket=buck, predefinedAcl="bucketLevel")

  })
  
  #Upload Atm files to S3
  # lapply(fileOutAtm, function(x){
  #   print(x)
  #   #Function to upload to ECS
  #   accs::upload.to.ecs(
  #     s3Path = S3PathUpldAtm,
  #     localPath = DirOutAtm,
  #     s3filename = x,
  #     filename = x
  #   ) 
  # })#End lapply for writing data out to GCS
  
    #Upload Eval files to GCS
    lapply(fileOutEval, function(x){
      print(x)
      #Function to upload to ECS
      googleCloudStorageR::gcs_upload(file = paste0(DirOutEval,"/",x), name=paste0(GcsPathUpldEval,"/",x), bucket=buck, predefinedAcl="bucketLevel")
      
  })#End lapply for writing data out to GCS
  
} #End if statement to write to GCS

##############################################################################
##!Plot input data
# this could be done better, but will work for now
##############################################################################
if(methPlot == TRUE){

# example from https://felixfan.github.io/stacking-plots-same-x/
mm <- reshape2::melt(subset(dataClm, select=c(DateTime,TBOT, RH,WIND,PRECTmms, PSRF,FLDS,FSDS)), id.var="DateTime")
ggplot(mm, aes(x = DateTime, y = value)) + 
  geom_line(aes(color = variable)) + 
  facet_grid(variable ~ ., scales = "free_y") + 
  theme(legend.position = "none") +
  ggtitle(Site)

ggsave(paste0(DirOut,"/",Site,"_forcing.pdf"), width = 30, height = 30, units = "cm")

# visualize missing data
# example from https://www.kaggle.com/jenslaufer/missing-value-visualization-with-ggplot2-and-dplyr
# see also https://stackoverflow.com/questions/57962514/geom-raster-to-visualize-missing-values-with-additional-colorcode

#Heat map of missing data
missing.values <- naniar::gg_miss_fct(dataClm[,grep("_fqc", x = names(dataClm), invert = TRUE)], fct = yearMon)
#Save heatmap
ggsave(paste0(DirOut,"/",Site,"_missing.pdf"), width = 30, height = 30, units = "cm")

#Plot gap-filling regressions
p <- lapply(names(dataGf), function(x){
#x <- "WS_MDS" #for testing
#Melt data fram to long format
xLong <- melt(dataGf[[x]], id.vars = x)
#Plot regressions
ggplot(xLong, aes_string(x, "value")) +
  geom_point() +
  geom_smooth(method="lm") +
  facet_wrap(~ variable)
}) #End lapply for plotting regressions

#output plotted regressions to pdf
pdf(paste0(DirOut,"/",Site,"_gf_regressions.pdf"))
invisible(lapply(p, function(x){try(print(x))}))
dev.off()#close plotting device


#Merge data sets to see the distribution of gap-filling methods
tmp <- reshape2::melt(subset(dataClm, select=c(DateTime,TBOT, RH,WIND,PRECTmms, PSRF,FLDS,FSDS)), id.var="DateTime")
tmpQf <- as.data.frame(qfClm)
tmpQf$DateTime <- dataClm$DateTime
names(tmpQf) <- stringr::str_remove(names(tmpQf), '_fqc')
tmpQfLong <- reshape2::melt(subset(tmpQf, select=c(DateTime,TBOT, RH,WIND,PRECTmms, PSRF,FLDS,FSDS)), id.var="DateTime")

dfLong <- merge(tmp,tmpQfLong, by = c("DateTime", "variable")) %>% 
  dplyr::mutate(value.y = tidyr::replace_na(value.y, "no gap-filling"))

n = length(levels(as.factor(dfLong$value.y)))
col = setNames(hcl(seq(15,375,length=n+1)[1:n], 100, 65), levels(as.factor(dfLong$value.y)))

#output plotted filled variables
pdf(paste0(DirOut,"/",Site,"_gf_variables.pdf"))

for(idxName in unique(as.character(dfLong$variable))){
#print(idxName)
  tmp <- dfLong %>% 
    filter(variable == idxName) 

  #Save plots
  p1 <- tmp %>% ggplot(aes(x = DateTime, y = value.x)) + 
    geom_point(aes(color = value.y)) +
    scale_colour_manual(values=col, drop=drop) +
    ggtitle(paste0(Site," ",idxName))
  
  p2 <- tmp %>%
    ggplot(aes(x=factor(value.y))) +
    geom_bar(aes(y = (after_stat(count))/sum(after_stat(count)))) + 
    scale_y_continuous(labels=scales::percent) +
    ylab("relative frequencies") +
    xlab("Gap-filling method") +
    ggtitle(paste0(Site," ",idxName))
  
  #Plot output
  print(p1)
  print(p2)
} #End of loop around variables

dev.off()#close plotting device

  
#ggsave(p, paste0(DirOut,"/",Site,"_gf_regressions.pdf"))
#Number of variables with missing data
#varMiss <- n_var_miss(dataClm)

#Upset interactions plot of missing data
#gg_miss_upset(dataClm, nsets = varMiss)

#Plot of the amount of missing data per variable (as %)
#gg_miss_var(dataClm, show_pct = TRUE)

#Output diagnostic plots to S3
if(MethOut == "gcs"){
  #Grab all output files names
  fileOutDiag<- base::list.files(path = DirOut, pattern = ".pdf")
  
  #Upload Atm files to S3
  lapply(fileOutDiag, function(x){
    print(x)
    #Upload plots to GCS
    googleCloudStorageR::gcs_upload(file = paste0(DirOut,"/",x), name=paste0(GcsPathUpldAtm,"/",x), bucket=buck, predefinedAcl="bucketLevel")

  })#End lapply for writing data out to S3
  
} #End if statement to write diagnostic plots to S3

}#End of plotting logical
