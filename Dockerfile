# start with the ropensci image including debian:testing, r-base, rocker/rstudio, rocker/hadleyverse
# https://hub.docker.com/r/rocker/ropensci/
FROM rocker/rstudio:3.6.1

WORKDIR /home/NCAR-NEON
# copy clone of GitHub source repo "NEONScience/NEON-FIU-algorithm" to the Docker image
COPY . .

# Build R dependencies using two cpu's worth of resources
ENV MAKEFLAGS='-j3'

# install OS-side dependencies: EBImage -> fftwtools -> fftw3, REddyProc -> RNetCDF -> udunits

  	# update the list of available packages and their versions

    RUN apt-get update \
    && apt-get dist-upgrade -y \
    && RUNDEPS="libudunits2-dev \
            udunits-bin \
            hdf5-helpers \
            libhdf5-cpp-100 \
            libnetcdf11 \
            libhdf5-100 \
            libsz2 \
            #libmysql++3v5 \
            #libmariadbclient18 \
            #libpng-tools \
            #libproj-dev \
			      libssl-dev \
			      libnetcdf-dev \
			      # Library for git via ssh key
			      ssh \
			      vim \
            libxml2-dev" \
            #mysql-common" \
            #fftw3\
    && BUILDDEPS="libhdf5-dev \
                  #libjpeg-dev \
                 #libtiff5-dev \
                 #libpng-dev \
                 #libmysql++-dev \
                 #fftw3-dev \
                 " \
    && apt-get install -y $BUILDDEPS $RUNDEPS \

    # Installing R package dependencies that are only workflow related (including CI combiner)
    && install2.r --error \
    rhdf5 \
    REddyProc \
    ncdf4 \
    devtools \

    # provide read and write access for default R library location to Rstudio users
    # TODO: PERHAPS THIS SHOULD JUST CHOWN TO rstudio instead of setting 777 perms? And at the end of the file -sj
    && chmod -R 777 /usr/local/lib/R/site-library \
    # Clean up build dependencies
    && apt-get remove --purge -y $BUILDDEPS \
    && apt-get autoremove -y \
    && apt-get autoclean -y \
    && rm -rf /var/lib/apt/lists/* \
    # Clean up the rocker image leftovers
    && rm -rf /tmp/rstudio* \
    && rm -rf /tmp/Rtmp* \