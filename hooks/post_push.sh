#!/bin/bash
set -e

# $IMAGE_NAME var is injected into the build so the tag is correct. 
echo "Build hook running"

echo "tagging $IMAGE_NAME as $DOCKER_REPO:$VERSION..."


docker build . --build-arg VCS_REF=`git rev-parse — short HEAD` \
  --build-arg BUILD_DATE=`date -u +”%Y-%m-%dT%H:%M:%SZ”` \
  -t $IMAGE_NAME 