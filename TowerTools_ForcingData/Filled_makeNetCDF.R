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
  library(ncdf4)

  dir = "~/Desktop/Working_files/Niwot/NR_fluxes/TVan/mpg_formatted/out_2008_2013"
  outdir = "~/Desktop/temp/"
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
lat     <- ncdim_def("lat","degrees_north", as.double(LAT), create_dimvar=TRUE)
lon     <- ncdim_def("lon","degrees_east", as.double(LON), create_dimvar=TRUE)

# Make varables (name, units, dims, missing_value)
  mv <- -9999.             # missing value to use
  startStep <- 1

  for (y in 1:(nyear)) {
    #  Data.ann <- Data.F[Data.F$Year==year[y], ]
    if(year[y]==2008 || year[y]==2012 || year[y]==2016 || year[y]==2020) {
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
      time <- ncdim_def("time", units=paste("days since",Data.mon$DateTime[1],"00:00:00"),
                           vals=as.double(time$vals),unlim=FALSE, create_dimvar=TRUE )
      LATIXY  <- ncvar_def("LATIXY", "degrees N", dim = list(lat), mv,
                              longname="latitude")
      LONGXY  <- ncvar_def("LONGXY", "degrees E", list(lon), mv,
                              longname="longitude")
      FLDS  <- ncvar_def("FLDS", "W/m^2", list(lon,lat,time), mv,
                            longname="incident longwave (FLDS)")
      FSDS  <- ncvar_def("FSDS", "W/m^2", list(lon,lat,time), mv,
                            longname="incident shortwave (FSDS)")
      PRECTmms <- ncvar_def("PRECTmms", "mm/s", list(lon,lat,time), mv,
                               longname="precipitation (PRECTmms)")
      PSRF  <- ncvar_def("PSRF", "Pa", list(lon,lat,time), mv,
                            longname="pressure at the lowest atmospheric level (PSRF)")
      RH    <- ncvar_def("RH", "%", list(lon,lat,time), mv,
                            longname="relative humidity at lowest atm level (RH)")
      TBOT  <- ncvar_def("TBOT", "K", list(lon,lat,time), mv,
                            longname="temperature at lowest atm level (TBOT)")
      WIND  <- ncvar_def("WIND", "m/s", list(lon,lat,time), mv,
                            longname="wind at lowest atm level (WIND)")
      ZBOT  <- ncvar_def("ZBOT", "m", list(lon,lat,time), mv,
                            longname="observational height")
      # Create a netCDF file with output variables
      fname <- paste(outdir, year[y],"-",mon[m],".nc", sep = "")
      ncnew <- nc_create( fname, list(LATIXY,LONGXY,FLDS,FSDS,PRECTmms,RH,PSRF,TBOT,WIND,ZBOT) )
      # Write some values to this variable on disk.
      ncvar_put( ncnew, LATIXY, LAT)
      ncvar_put( ncnew, LONGXY, LON)
      ncvar_put( ncnew, FLDS, Data.mon$FLDS)
      ncvar_put( ncnew, FSDS, Data.mon$FSDS)
      ncvar_put( ncnew, RH,   Data.mon$RH)
      ncvar_put( ncnew, PRECTmms, Data.mon$PRECTmms)
      ncvar_put( ncnew, PSRF, Data.mon$PSRF)
      ncvar_put( ncnew, TBOT, Data.mon$TBOT)
      ncvar_put( ncnew, WIND, Data.mon$WIND)
      ncvar_put( ncnew, ZBOT, Data.mon$ZBOT)
      #add attributes
      ncatt_put( ncnew, 'time', attname = "calendar", attval = "gregorian" ,prec=NA,verbose=FALSE,definemode=FALSE )
      ncatt_put( ncnew, 'FLDS',"mode","time-dependent" ,prec=NA,verbose=FALSE,definemode=FALSE )
      ncatt_put( ncnew, 'FSDS',"mode","time-dependent" ,prec=NA,verbose=FALSE,definemode=FALSE )
      ncatt_put( ncnew, 'RH'  ,"mode","time-dependent" ,prec=NA,verbose=FALSE,definemode=FALSE )
      ncatt_put( ncnew, 'PRECTmms',"mode","time-dependent" ,prec=NA,verbose=FALSE,definemode=FALSE )
      ncatt_put( ncnew, 'PSRF',"mode","time-dependent" ,prec=NA,verbose=FALSE,definemode=FALSE )
      ncatt_put( ncnew, 'TBOT',"mode","time-dependent" ,prec=NA,verbose=FALSE,definemode=FALSE )
      ncatt_put( ncnew, 'WIND',"mode","time-dependent" ,prec=NA,verbose=FALSE,definemode=FALSE )
      ncatt_put( ncnew, 'ZBOT',"mode","time-dependent" ,prec=NA,verbose=FALSE,definemode=FALSE )
      ncatt_put( ncnew, 0, "created_on",date()       ,prec=NA,verbose=FALSE,definemode=FALSE )
      ncatt_put( ncnew, 0, "created_by","Will Wieder",prec=NA,verbose=FALSE,definemode=FALSE )
      ncatt_put( ncnew, 0, "created_from",fin        ,prec=NA,verbose=FALSE,definemode=FALSE )
      ncatt_put( ncnew, 0, "created_with",file       ,prec=NA,verbose=FALSE,definemode=FALSE )
      nc_close(ncnew)
      startStep <- endStep + 1
      remove(endStep, time, timeStep, fname, ncnew, Data.mon,
             FLDS,FSDS,RH,PRECTmms,PSRF,TBOT,WIND,ZBOT)
    }                        # close monthly loop
    remove(nsteps)
  }                          # close annual loop
  print('-------Wrote out .nc files-----------')
  