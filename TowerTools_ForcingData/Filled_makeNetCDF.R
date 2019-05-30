# Filled_makeNetCDF.R
# Will Wieder
# April 2015
# Reads in gap filled flux tower measurements (now GMT)
# Writes out monthly .nc files to force single points simulation

# Uses incoming solar from US-NR1 (2008-2012), NOT subnivian lab data
# no incoming solar from 2013, can we get this & rerun results?
# Uses daily preciptiation data from Saddle, distributed as CRNS observations (30 minute) 
# All other data from Tvan towers, from John Knowles 2015

  library(REddyProc)
  library(ncdf)

  dir = "~/Desktop/Working_files/Niwot/NR_fluxes/TVan/mpg_formatted/out_2008_2013"
  setwd(dir)
  file <- paste(dir,"Filled_makeNetCDF.R", sep = "")

  #+++ Load data with one header and one unit row from (tab-delimited) text file
  indir ="/Users/wwieder/Desktop/Working_files/Niwot/NR_fluxes/TVan/mpg_formatted/out/"
  fin  <- paste(indir,"TVan_CLM_forcing_2008_2013_GMT.txt", sep = "")
  Data.F <- fLoadTXTIntoDataframe(fin)
  names(Data.F)
  Data.F <- fConvertTimeToPosix(Data.F, 'YDH', Year.s='Year', Day.s='DoY', Hour.s='Hour')
  names(Data.F)
  #attach(Data.F)

  Data.F$DateTime[2]
  year       <- c(2008, 2009, 2010, 2011, 2012, 2013 ) 
  mon        <- c("01","02","03","04","05","06","07","08","09","10","11","12") 
  regu_days  <- c(31,28,31,30,31,30,31,31,30,31,30,31)
	leap_days  <- c(31,29,31,30,31,30,31,31,30,31,30,31)
  regu_steps <- regu_days * 48
  leap_steps <- leap_days * 48
  nyear <- length(year)
#  Data.ann <- Data.F[Data.F$Year==2008, ]  
 

# coordinates for Tvan
LAT <- 40.051989   
LON <- 254.4142 

#-------------------------------------------------------------
#---------------write out .nc file----------------------------
#-------------------------------------------------------------
# define the netcdf coordinate variables (name, units, type)
lat     <- dim.def.ncdf("lat","degrees_north", as.double(LAT), create_dimvar=TRUE)
lon     <- dim.def.ncdf("lon","degrees_east", as.double(LON), create_dimvar=TRUE)

# Make varables (name, units, dims, missing_value)
  mv <- -9999.             # missing value to use
  startStep <- 1

  for (y in 1:(nyear)) {
    #  Data.ann <- Data.F[Data.F$Year==year[y], ]
    if(year[y]==2008 || year[y]==2012) {
      nsteps <- leap_steps
    } else {
      nsteps <- regu_steps
    }

    for (m in 1:12) {
      timeStep <- seq(0,nsteps[m]-1,1)
      time     <- timeStep/48
      endStep  <- startStep + nsteps[m]-1
      Data.mon <- Data.F[startStep:endStep, ]
      print(paste(year[y],m,"Data date =",Data.mon$DateTime[1]))
      names(Data.mon)
      time <- dim.def.ncdf("time", units=paste("days since",Data.mon$DateTime[1],"00:00:00"),
                           vals=as.double(time),unlim=FALSE, create_dimvar=TRUE )
      LATIXY  <- var.def.ncdf("LATIXY", "degrees N", list(lat), mv,
                              longname="latitude", prec="double")
      LONGXY  <- var.def.ncdf("LONGXY", "degrees E", list(lon), mv,
                              longname="longitude", prec="double")
      FLDS  <- var.def.ncdf("FLDS", "W/m^2", list(lon,lat,time), mv,
                            longname="incident longwave (FLDS)", prec="double")
      FSDS  <- var.def.ncdf("FSDS", "W/m^2", list(lon,lat,time), mv,
                            longname="incident shortwave (FSDS)", prec="double")
      PRECTmms <- var.def.ncdf("PRECTmms", "mm/s", list(lon,lat,time), mv,
                               longname="precipitation (PRECTmms)", prec="double")
      PSRF  <- var.def.ncdf("PSRF", "Pa", list(lon,lat,time), mv,
                            longname="pressure at the lowest atmospheric level (PSRF)", prec="double")
      RH    <- var.def.ncdf("RH", "%", list(lon,lat,time), mv,
                            longname="relative humidity at lowest atm level (RH)", prec="double")
      TBOT  <- var.def.ncdf("TBOT", "K", list(lon,lat,time), mv,
                            longname="temperature at lowest atm level (TBOT)", prec="double")
      WIND  <- var.def.ncdf("WIND", "m/s", list(lon,lat,time), mv,
                            longname="wind at lowest atm level (WIND)", prec="double")
      ZBOT  <- var.def.ncdf("ZBOT", "m", list(lon,lat,time), mv,
                            longname="observational height", prec="double")
      # Create a netCDF file with output variables
      fname <- paste(year[y],"-",mon[m],".nc", sep = "")
      ncnew <- create.ncdf( fname, list(LATIXY,LONGXY,FLDS,FSDS,PRECTmms,RH,PSRF,TBOT,WIND,ZBOT) )
      # Write some values to this variable on disk.
      put.var.ncdf( ncnew, LATIXY, LAT)
      put.var.ncdf( ncnew, LONGXY, LON)
      put.var.ncdf( ncnew, FLDS, Data.mon$FLDS)
      put.var.ncdf( ncnew, FSDS, Data.mon$FSDS)
      put.var.ncdf( ncnew, RH,   Data.mon$RH)
      put.var.ncdf( ncnew, PRECTmms, Data.mon$PRECTmms)
      put.var.ncdf( ncnew, PSRF, Data.mon$PSRF)
      put.var.ncdf( ncnew, TBOT, Data.mon$TBOT)
      put.var.ncdf( ncnew, WIND, Data.mon$WIND)
      put.var.ncdf( ncnew, ZBOT, Data.mon$ZBOT)
      #add attributes
      att.put.ncdf( ncnew, time,"calendar", "gregorian" ,prec=NA,verbose=FALSE,definemode=FALSE )
      att.put.ncdf( ncnew, FLDS,"mode","time-dependent" ,prec=NA,verbose=FALSE,definemode=FALSE )
      att.put.ncdf( ncnew, FSDS,"mode","time-dependent" ,prec=NA,verbose=FALSE,definemode=FALSE )
      att.put.ncdf( ncnew, RH  ,"mode","time-dependent" ,prec=NA,verbose=FALSE,definemode=FALSE )
      att.put.ncdf( ncnew, PRECTmms,"mode","time-dependent" ,prec=NA,verbose=FALSE,definemode=FALSE )
      att.put.ncdf( ncnew, PSRF,"mode","time-dependent" ,prec=NA,verbose=FALSE,definemode=FALSE )
      att.put.ncdf( ncnew, TBOT,"mode","time-dependent" ,prec=NA,verbose=FALSE,definemode=FALSE )
      att.put.ncdf( ncnew, WIND,"mode","time-dependent" ,prec=NA,verbose=FALSE,definemode=FALSE )
      att.put.ncdf( ncnew, ZBOT,"mode","time-dependent" ,prec=NA,verbose=FALSE,definemode=FALSE )
      att.put.ncdf( ncnew, 0, "created_on",date()       ,prec=NA,verbose=FALSE,definemode=FALSE )
      att.put.ncdf( ncnew, 0, "created_by","Will Wieder",prec=NA,verbose=FALSE,definemode=FALSE )
      att.put.ncdf( ncnew, 0, "created_from",fin        ,prec=NA,verbose=FALSE,definemode=FALSE )
      att.put.ncdf( ncnew, 0, "created_with",file       ,prec=NA,verbose=FALSE,definemode=FALSE )
      close.ncdf(ncnew)
      startStep <- endStep + 1
      remove(endStep, time, timeStep, fname, ncnew, Data.mon,
             FLDS,FSDS,RH,PRECTmms,PSRF,TBOT,WIND,ZBOT)
    }                        # close monthly loop
    remove(nsteps)
  }                          # close annual loop
  print('-------Wrote out .nc files-----------')
  