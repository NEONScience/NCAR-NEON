# start with the ropensci image including debian:testing, r-base, rocker/rstudio, rocker/hadleyverse
# https://hub.docker.com/r/rocker/ropensci/

FROM quay.io/battelleecology/rstudio:4.0.5
#

#LABEL org.label-schema.license="AGPL-3.0" \
#      org.label-schema.vcs-url="https://github.com/NEONScience/NCAR-NEON" \
 #     org.label-schema.vendor="NEON" \
#      maintainer="David Durden <ddurden@battelleecology.org>"\
 #     vers=$IMAGE_NAME

#ARG BUILD_DATE
#ARG IMAGE_NAME
#ARG VCS_REF
#ARG VERSION

#ENV VCS_REF=$VCS_REF

#RUN echo "${BUILD_DATE}, ${IMAGE_NAME}, $VCS_REF, $VERSION"

#ENV BUILD_DATE ${BUILD_DATE:-2020-04-24}
#ENV IMAGE_NAME

#RUN echo "${BUILD_DATE}, ${IMAGE_NAME}, ${VCS_REF}, ${VERSION}"

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
            libhdf5-cpp-103 \
            libhdf5-103 \
            libsz2 \
            libmysql++3v5 \
            libmariadb3 \
            libpng-tools \
            libproj-dev \
			      libssl-dev \
			      libgdal-dev \
			      libgsl-dev \
			      # Library for git via ssh key
			      ssh \
			      vim \
			      libhdf5-dev \
			      libnetcdf-dev \
            libxml2-dev" \
            #mysql-common" \
            #fftw3\
            #libnetcdf11 \
    && BUILDDEPS="libjpeg-dev \
                 libtiff5-dev \
                 libpng-dev \
                 " \
                 #libmysql++-dev \
                 #fftw3-dev \
                 
    && apt-get install -y $BUILDDEPS $RUNDEPS \

    # Installing R package dependencies that are only workflow related (including CI combiner)
#    && install2.r --error --repos "https://packagemanager.rstudio.com/cran/__linux__/focal/2021-05-17"\ 
    #"https://cran.rstudio.com/"\
#    devtools \
#    BiocManager \
#    REddyProc \
#    ncdf4 \
#    reshape2 \
#    ggplot2 \
#    gridExtra \
    #tidyverse \
#    naniar \
    #aws.s3 \
#    neonUtilities \
#    googleCloudStorageR \
    
 #    && install2.r --error --repos "https://packagemanager.rstudio.com/cran/__linux__/focal/2023-09-22"\ 
    #"https://cran.rstudio.com/"\
#    neonUtilities \
#    Rfast \
 #   tidyverse \
    
     ## from bioconductor
    && R -e 'utils::install.packages("remotes")' \
    && R -e 'remotes::install_github("rstudio/renv@0.16.0")'\
    && R -e 'renv::consent(provided=TRUE); renv::restore()' \ 
    && R -e 'renv::install(pkg = "gapFilling/pack/NEON.gf")' \
    #Install packages from github repos
   # && R -e "devtools::install_github('NEONScience/eddy4R/pack/eddy4R.base')" \
    
    


    # provide read and write access for default R library location to Rstudio users
    && chmod -R 777 /usr/local/lib/R/site-library \
    # Clean up build dependencies
    && apt-get remove --purge -y $BUILDDEPS \
    && apt-get autoremove -y \
    && apt-get autoclean -y \
    && rm -rf /var/lib/apt/lists/* \
    # Clean up the rocker image leftovers
    && rm -rf /tmp/rstudio* \
    && rm -rf /tmp/Rtmp* \
