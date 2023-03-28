##############################################################################################
#' @title Workflow for downloading dp0p data from S3

#' @author
#' David Durden \email{eddy4R.info@gmail.com}

#' @description
#' Workflow. Downloading NCAR-NEON atmospheric forcing netCDF files from S3.

#' @param Currently none

#' @return Currently none

#' @references

#' @keywords meteorology, netCDF, NEON

#' @examples Currently none

#' @seealso Currently none

# changelog and author contributions / copyrights
#   David (2021-01-04)
#     original creation
#   David (2021-05-12)
#     updating to grab all files and changing s3 bucket structure
##############################################################################################

#site to download data for
site <- "NIWO"

#type of data file to download atmospheric (datm) or surface (surf) forcing files
TypeFile <- c("atm/cdeps", "surf_files", "eval_files")[2]

#Adding version number to s3 path
versData <- c("v1","v2")[2]
versDataSurf <- "v1"

#Base directory for downloading data
DirDnldBase <- "~/eddy/data/CLM/dnld"

#The version data for the FP standard conversion processing
dnldDate <- paste0("v",format(Sys.time(), "%Y%m%d"))

#Create download folder, create if it doesn't exist
DirDnld <- paste0(DirDnldBase,"/",dnldDate,"/",site)
if(!dir.exists(DirDnld)) dir.create(DirDnld, recursive = TRUE)

#Create data download string
DateBgn <- as.Date("2018-01-01")
DateEnd <- as.Date("2018-12-31")
DateSeq <- seq.Date(from = DateBgn,to = DateEnd, by = "month")
PrdWndwDnld <- strftime(DateSeq, format = "%Y-%m")

#logical statement about type of files
if(TypeFile == "surf_files"){
  urlDnld <- paste0("https://storage.neonscience.org/neon-ncar/NEON/",TypeFile,"/",versDataSurf,"/",site,"_surfaceData.csv")
  
  #Download filename (full path)
  fileDnld <- paste0(DirDnld,"/",site,"_",versData,"_surfaceData.csv")
} else
  {
    
    #Removing extra nesting used by CLM for atm files
    TypeFileDnld <- unlist(strsplit(TypeFile, "/|_"))[1]
    
  #Create URL for data files
  urlDnld <- paste0("https://storage.neonscience.org/neon-ncar/NEON/",TypeFile,"/",versData,"/",site,"/",site,"_",ifelse(TypeFile == "atm/cdeps",TypeFileDnld,TypeFile),"_",PrdWndwDnld,".nc")
  
  
  #Download filename (full path)
  fileDnld <-  paste0(DirDnld,"/",site,"_",TypeFileDnld,"_",versData,"_",PrdWndwDnld,".nc") 
  
} #End of else for logical statement about type of files

#Download files
sapply(seq_along(urlDnld), function(x){
  download.file(url = urlDnld[x], destfile = fileDnld[x])
})
