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
##############################################################################################

#site to download data for
site <- "NIWO"

#type of data file to download atmospheric (datm) or surface (surf) forcing files
TypeFile <- c("datm_files", "surf_files")[1]


#The version data for the FP standard conversion processing
dnldDate <- paste0("v",format(Sys.time(), "%Y%m%d"))

#Create download folder, create if it doesn't exist
DirDnld <- paste0("~/eddy/data/CLM/dnld/",dnldDate,"/",site)
if(!dir.exists(DirDnld)) dir.create(DirDnld, recursive = TRUE)

#Create data download string
DateBgn <- as.Date("2018-01-01")
DateEnd <- as.Date("2018-12-31")
DateSeq <- seq.Date(from = DateBgn,to = DateEnd, by = "month")
PrdWndwDnld <- strftime(DateSeq, format = "%Y-%m")


#Create URL for data files
urlDnld <- paste0("https://s3.data.neonscience.org/neon-ncar/NEON/",TypeFile,"/",site,"/",PrdWndwDnld,".nc")

#Download filename (full path)
fileDnld <-  paste0(DirDnld,"/",site,"_",PrdWndwDnld,".nc")

#Download files
sapply(seq_along(urlDnld), function(x){
  download.file(url = urlDnld[x], destfile = fileDnld[x])
})
