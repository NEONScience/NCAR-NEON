install.packages("naniar")

library(naniar)
library(dplyr)
library(tidyr)
library(ggplot2)

Site <- "SRER"

dateBgn <- "2018-01-01"
dateEnd <- "2018-03-01"
Pack <- "basic"
TimeAgr <- 30
varGf <- "TBOT"

listDpNum <- c("temp" = "DP1.00003.001", "tempRh" = "DP1.00098.001", "tempSoni" = "DP4.00200.001")

##Grab data for data products using Noble package
dataMet <- lapply(listDpNum, function(x){
  try(expr = neonUtilities::loadByProduct(site = Site, dpID = x, 
                                          startdate = as.character(dateBgn), 
                                          enddate = as.character(dateEnd), 
                                          package = Pack, avg = TimeAgr, check.size = FALSE), 
      silent = TRUE)
})
