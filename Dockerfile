# start with the ropensci image including debian:testing, r-base, rocker/rstudio, rocker/hadleyverse
# https://hub.docker.com/r/rocker/ropensci/
FROM rocker/rstudio:3.6.1

LABEL org.label-schema.license="AGPL-3.0" \
      org.label-schema.vcs-url="https://github.com/NEONScience/NCAR-NEON" \
      org.label-schema.vendor="NEON" \
      maintainer="David Durden <ddurden@battelleecology.org>"\
      vers=$IMAGE_NAME

ARG BUILD_DATE
#ARG IMAGE_NAME
ARG VCS_REF
#ARG VERSION

ENV VCS_REF=$VCS_REF

RUN echo "${BUILD_DATE}, ${IMAGE_NAME}, $VCS_REF, $VERSION"

ENV BUILD_DATE ${BUILD_DATE:-2020-04-24}
#ENV IMAGE_NAME

RUN echo "${BUILD_DATE}, ${IMAGE_NAME}, ${VCS_REF}, ${VERSION}"

WORKDIR /home/NCAR-NEON
# copy clone of GitHub source repo "NEONScience/NEON-FIU-algorithm" to the Docker image
COPY . .

# Build R dependencies using two cpu's worth of resources
#ENV MAKEFLAGS='-j3'
