##############################################################################################
#' @title Test script to learn how to implement the MDS gap filling in REddyProc
#' 
#' @author 
#' Cove Sturtevant \email{csturtevant@battelleecology.org}

#' @description 
#' Workflow. Test script to learn how to implement the MDS gap filling in REddyProc

#' @references 
#' REddyProc Readme: \url{https://github.com/bgctw/REddyProc} \cr
#' 
#' @return TBD

#' @keywords L2, gap-filling, marginal distribution sampling

#' @examples TBD

#' @seealso TBD

# changelog and author contributions / copyrights
#   Cove Sturtevant (2017-04-19)
#     original creation - pirating components from the REddyProc readme
##############################################################################################
rm(list=ls())

library(REddyProc)


#+++ Input data from csv (example needs to be downloaded)
examplePath <- getExamplePath('Example_DETha98.txt', isTryDownload = TRUE) # Pretty sure this downloads the example into a temp directory
EddyData.F <- fLoadTXTIntoDataframe(examplePath) # Loads the file into a data frame with the following column variables
# *** Year(-) DoY(-) Hour(-) NEE(umolm-2s-1) LE(Wm-2) H(Wm-2) Rg(Wm-2) Tair(degC) Tsoil(degC) rH(%) VPD(hPa) Ustar(ms-1)
# Number of '-9999' convertered to NA: 11657

#+++ Add time stamp in POSIX time format in to variable "DateTime" (POSIXct)
EddyDataWithPosix.F <- fConvertTimeToPosix(EddyData.F, 'YDH', Year.s = 'Year', Day.s = 'DoY', Hour.s = 'Hour') # The parameter inputs 

#+++ Initalize R5 reference class sEddyProc for processing of eddy data
#+++ with all variables needed for processing later
# Pulls in the DateTime variable form the data frame, and those specified in the last argument. Returns it's own environment with 'methods' included.
# This appears to be a requirement to use the MDS gap-filling code
EddyProc.C <- sEddyProc$new('MySite', EddyDataWithPosix.F, c('NEE','Rg','Tair','VPD', 'Ustar'))  

#++ Fill NEE gaps with MDS gap filling algorithm (without prior ustar filtering)
# This appears to be how you call the function REddyProd::sEddyProc_sMDSGapFill. See the documentation for input options
EddyProc.C$sMDSGapFill('NEE', FillAll.b = FALSE)

#++ Export gap filled and partitioned data to standard data frame
# This is how you get your MDS-filled data back into the global environment
FilledEddyData.F <- EddyProc.C$sExportResults()
