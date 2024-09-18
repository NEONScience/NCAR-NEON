install.packages("naniar")

library(naniar)
library(dplyr)
library(tidyr)
library(ggplot2)

Site <- "OAES"

dateBgn <- "2018-01-01"
dateEnd <- "2019-01-01"
TimeAgr <- "30"
Pack <- "basic"

#List of DP numbers by eddy4R DP names
listDpNum <- c( "PRECTmms_MDS" = "DP1.00006.001", "rH" = "DP1.00098.001", "FLDS_MDS" = "DP1.00023.001", "Rg" = "DP1.00023.001", "Pa_MDS" = "DP1.00004.001", "TBOT" = "DP1.00003.001")

#names for individual variables of interest
varDp <- c("PRECTmms_MDS" = "SECPRE_30min", "rH" = "RH_30min", "FLDS_MDS" = "SLRNR_30min", "Rg" = "SLRNR_30min", "Pa_MDS" = "BP_30min", "TBOT" = "TAAT_30min") #Currently using the relative humidity from the soil array, tower top was not reporting data at HARV during this time

subVar <- c("PRECTmms_MDS" = "secPrecipBulk", "rH" = "RHMean", "FLDS_MDS" = "inLWMean", "Rg" = "inSWMean", "Pa_MDS" = "staPresMean", "TBOT" = "tempTripleMean")

subVarQf <- c("PRECTmms_MDS" = "secPrecipFinalQF", "rH" = "RHFinalQF", "FLDS_MDS" = "inLWFinalQF", "Rg" = "inSWFinalQF", "Pa_MDS" = "staPresFinalQF", "TBOT" = "finalQF")

#test <- Noble::gap.vis(site = "NIWO",bgn.month = "2018-01", end.month = "2020-01", dpID = "DP1.00003.001", save.dir = "~/eddy/plot/NCAR-NEON")

##Grab data for data products using Noble package
dataMet <- lapply(listDpNum, function(x){
  try(expr = neonUtilities::loadByProduct(site = Site, dpID = x, 
                                          startdate = as.character(dateBgn), 
                                          enddate = as.character(dateEnd), 
                                          package = Pack, avg = TimeAgr, check.size = FALSE), 
      silent = TRUE)
})

#Check if primary precipitation exists at the site, if not change to secondary precip
varDp["PRECTmms_MDS"] <- ifelse(test = any(grepl(pattern = varDp["PRECTmms_MDS"], x = names(dataMet[["PRECTmms_MDS"]]))), "SECPRE_30min", "PRIPRE_30min")

#Failsafe if using primary precip
subVar["PRECTmms_MDS"] <- ifelse(test = varDp["PRECTmms_MDS"] == "SECPRE_30min", "secPrecipBulk", "priPrecipBulk")
subVarQf["PRECTmms_MDS"] <- ifelse(test = varDp["PRECTmms_MDS"] == "SECPRE_30min", "secPrecipFinalQF", "priPrecipFinalQF")



#Grab the actual data tables
dataMetSub <- lapply(seq_along(varDp), function(x) {
  tmp <- dataMet[[names(varDp[x])]][[grep(pattern = varDp[[x]], x = names(dataMet[[names(varDp[x])]]))]]
  return(tmp)
})

#Name the output lists
names(dataMetSub) <- names(varDp)

#Remove unwanted measurement levels
dataMetSub$Rg <- dataMetSub$Rg[dataMetSub$Rg$horizontalPosition == "000",]
dataMetSub$FLDS_MDS <- dataMetSub$FLDS_MDS[dataMetSub$FLDS_MDS$horizontalPosition == "000",]
dataMetSub$rH <- dataMetSub$rH[dataMetSub$rH$horizontalPosition == "003",]

#time regularization of met data
# dataMetSubRglr <- lapply(names(dataMetSub), function(x){
#   timeRglrMet <- eddy4R.base::def.rglr(timeMeas = as.POSIXlt(dataMetSub[[x]]$startDateTime), 
#                                        dataMeas = dataMetSub[[x]], 
#                                        BgnRglr = as.POSIXlt(dateBgn), 
#                                        EndRglr = as.POSIXlt(dateEnd), 
#                                        TzRglr = "UTC", FreqRglr = 1/(60*30))
#   
#   return(timeRglrMet$dataRglr)
# })#End lapply for time regularization of met data
# 
# #Add names to list of Dataframes of regularized data
# names(dataMetSubRglr) <- names(varDp)

#Grab just the Met data of interest for the forcing data
dataDfMet <- lapply(seq_along(subVar), function(x){
  #print(x)
  #Grab the variables of interest
  tmp <- dataMetSub[[names(subVar[x])]][,grep(pattern = paste0("^",subVar[x]), x = names(dataMetSub[[names(varDp[x])]]))] # use ^ to indicate the pattern starts with the name given
  return(tmp)
})

#Give the ReddyProc names
names(dataDfMet) <- names(varDp)

#Calculate precip rate from bulk
#dataDfMet$PRECTmms_MDS <- dataDfMet$PRECTmms_MDS/1800 #1800 sec/0.5 hours

#Calculate net radiation
dataDfMet$radNet <-dataMetSub$Rg[["inSWMean"]] - dataMetSub$Rg[["outSWMean"]] + dataMetSub$Rg[["inLWMean"]] - dataMetSub$Rg[["outLWMean"]]

#Add time
dataDfMet$time <- dataMetSub$Pa_MDS$startDateTime

#########################################################################################
#QFQM
#########################################################################################
#Grab just the Met data of interest for the forcing data
qfqmDfMet <- lapply(seq_along(subVar), function(x){
  #print(x)
  #Grab the variables of interest
  tmp <- dataMetSub[[names(subVarQf[x])]][,grep(pattern = paste0("^",subVarQf[x]), x = names(dataMetSub[[names(varDp[x])]]))] # use ^ to indicate the pattern starts with the name given
  return(tmp)
})

#Give the ReddyProc names
names(qfqmDfMet) <- names(varDp)

qfqmDfMet <- as.data.frame(qfqmDfMet)

#Add time
qfqmDfMet$time <- dataMetSub$Pa_MDS$startDateTime

tmpRadQfqm <- select(dataMetSub$Rg,"inSWFinalQF", "outSWFinalQF", "outLWFinalQF","inLWFinalQF")


#Add radNet
qfqmDfMet$radNet <- ifelse(rowSums(tmpRadQfqm) > 0, 1, 0)

#Add precip flay
dataMetSub$PRECTmms_MDS$secPrecipSciRvwQF[which(is.na(dataMetSub$PRECTmms_MDS$secPrecipSciRvwQF))] <- 0 #Science review NA will be replaced with 0
qfqmDfMet$PRECTmms_MDS <- ifelse((dataMetSub$PRECTmms_MDS$secPrecipRangeQF + dataMetSub$PRECTmms_MDS$secPrecipSciRvwQF) > 0, 1, 0)

qfqmDfMetLong <- gather(qfqmDfMet, "variable", "qfFinal", -time)

#########################################################################################


#Make a dataframe
dataDfMet <- as.data.frame(dataDfMet)

#Create long dataframe
dataDfMetLong <- gather(dataDfMet, "variable", "value", -time)

dataDfLong <- dplyr::left_join(dataDfMetLong, qfqmDfMetLong, by = c("time" = "time", "variable"="variable"))

#########################################################################################
#Plot
#########################################################################################

ggplot2::ggplot(data = dataDfLong, aes(x = time, y = value, colour = qfFinal == 1)) + geom_point(shape = 1) + 
  scale_colour_manual(name = 'qfFinal = 1', values = setNames(c('red','black'), c(T, F))) +
  facet_grid(variable ~ ., scales = "free_y")

#Plot precip
ggplot2::qplot(dataDfMet$time, dataDfMet$PRECTmms_MDS)


#########################################################################################
#Summary statistics
#########################################################################################
#Data summary stats
dataSummary <- dataDfLong %>% select(-time, -qfFinal) %>% group_by(variable) %>% summarise_all(list(mean = mean, median = median, max = max, min = min, sd = sd), na.rm = TRUE)

#qfFinal summary
qfqmSummary <-  dataDfLong %>% select(-time, -value) %>% group_by(variable) %>% summarise_all(list(sumQfFinal = sum, percentQfFinal = mean))

#Join summary tables
summary <- dplyr::left_join(dataSummary, qfqmSummary, by = "variable")


##########################################################################################
#Plot gaps using naniar package
##########################################################################################
#Flare graph of missing data
vis_miss(dataDfMet)

#Number of variables with missing data
varMiss <- n_var_miss(dataDfMet)

#Upset interactions plot of missing data
gg_miss_upset(dataDfMet, nsets = varMiss)

#Plot of the amount of missing data per variable (as %)
gg_miss_var(dataDfMet, show_pct = TRUE)

#Add a Month column
dataDfMet$month <- strftime(dataDfMet$time, format = "%Y%m")

#Heat map of missing data
gg_miss_fct(dataDfMet, fct = month)
