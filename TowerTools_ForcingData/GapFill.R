# Uses ReddyProc to gap fill Eddy-Flux data
# https://www.bgc-jena.mpg.de/bgi/index.php/Services/REddyProcWeb
# install.packages("REddyProc", repos="http://R-Forge.R-project.org", type="source")

  library(REddyProc)
#  help('REddyProc-package')
#  help('sEddyProc.example')


  #+++ Load data with one header and one unit row from (tab-delimited) text file

#  Dir.s <- paste(system.file(package='REddyProc'), 'examples', sep='/')
#  EddyData.F <- fLoadTXTIntoDataframe('Tvan08-13_new_MST.txt')
  EddyData.F <- fLoadTXTIntoDataframe('Tvan08-14_new_MST.txt')
  
  # calcculate Rh and VPD from rho_v data
  EddyData.F$ea <- EddyData.F$rho_v_MDS*8.3143*(EddyData.F$Tair+273.15)/(18.02*1000)*10      	#hPa
  EddyData.F$es <- 0.61365*exp((17.502*EddyData.F$Tair)/(EddyData.F$Tair + 240.97)) *10          	#hPa
  EddyData.F$Rh <- (EddyData.F$ea / EddyData.F$es) * 100									#%
  EddyData.F$VPD<- EddyData.F$es -   EddyData.F$ea    										#hPa

  plot(EddyData.F$Rg, EddyData.F$Rn_MDS)

#  set VPD and Rg values <0 to 0
  EddyData.F$VPD[EddyData.F$VPD < 0 ] <- 0.
  EddyData.F$Rg[EddyData.F$Rg   < 0 ] <- 0.
  EddyData.F$Rg[EddyData.F$Rg   > 1200 ] <- 1200.
  min(EddyData.F$VPD, na.rm=T)
  max(EddyData.F$Year)
  #+++ If not provided, calculate VPD from Tair and rH
  #EddyData.F <- cbind(EddyData.F,VPD=fCalcVPDfromRHandTair(EddyData.F$rH, EddyData.F$Tair))
  
  #+++ Add time stamp in POSIX time format
  EddyDataWithPosix.F <- fConvertTimeToPosix(EddyData.F, 'YDH', Year.s='Year', Day.s='DoY', Hour.s='Hour')
  
  #+++ Initalize R5 reference class sEddyProc for processing of eddy data
  #+++ with all variables needed for processing later

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
#  EddyProc.C$sMDSGapFill('Rg', FillAll.b=FALSE) #Fill only the gaps for the meteo condition, e.g. 'Rg'

  #+++ Example plots of filled data to screen or to directory \plots
  for (i in 1:nyears) {  
    EddyProc.C$sPlotFingerprintY('NEE_f', Year.i=years[i])
    EddyProc.C$sPlotDailySumsY('NEE_f','NEE_fsd', Year.i=years[i]) #Plot of sums with uncertainties
  }  
  EddyProc.C$sPlotDailySums('NEE_f','NEE_fsd')
  
  #+++ Partition NEE into GPP and respiration
  EddyProc.C$sMDSGapFill('Tair', FillAll.b=FALSE)  	# Gap-filled Tair (and NEE) needed for partitioning 
  EddyProc.C$sMRFluxPartition(Lat_deg.n=40.05, Long_deg.n=105.6, TimeZone_h.n=7)  #Location of Niwot
# other gaps filled data 
  EddyProc.C$sMDSGapFill('VPD',    FillAll.b=FALSE) 
  EddyProc.C$sMDSGapFill('Rh',     FillAll.b=FALSE) 
  EddyProc.C$sMDSGapFill('ws_MDS', FillAll.b=FALSE) 
  EddyProc.C$sMDSGapFill('Rn_MDS', FillAll.b=FALSE) 
  EddyProc.C$sMDSGapFill('Pa_MDS', FillAll.b=FALSE) 
  
  #+++ Example plots of calculated GPP and respiration 
  EddyProc.C$sPlotFingerprintY('GPP_f', Year.i=2008-13)
  EddyProc.C$sPlotFingerprint('GPP_f')
  EddyProc.C$sPlotHHFluxesY('Reco', Year.i=2008-13)
  EddyProc.C$sPlotHHFluxes('Reco')
	
  #+++ Processing with ustar threshold provided  
  #+++ Provide ustar value(s) as a single value or a vector with an entry for each year
#  Ustar.V.n <- 0.43 #For a dataset with three years of data, this could also be a vector, e.g. Ustar.V.n <- c(0.41, 0.43, 0.42)
  #+++ Gap filling and partitioning after ustar filtering
#  EddyProc.C$sMDSGapFillAfterUstar(FluxVar.s='NEE', UstarThres.V.n=Ustar.V.n)
#  EddyProc.C$sMRFluxPartition(Lat_deg.n=51.0, Long_deg.n=13.6, TimeZone_h.n=1, Suffix.s='WithUstar')  # Note suffix
  
  #+++ ! Coming soon: The Ustar filtering algorithm after Papale et al. (2006) ! +++
  
  #+++ Export gap filled and partitioned data to standard data frame
  FilledEddyData.F <- EddyProc.C$sExportResults()
  #+++ Save results into (tab-delimited) text file in directory \out
  CombinedData.F <- cbind(EddyData.F, FilledEddyData.F)
  fWriteDataframeToFile(CombinedData.F, 'NR-Tvan-Results_2008-14_MST.txt', 'out')
  
