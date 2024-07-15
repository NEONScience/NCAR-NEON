FROM debian:bullseye

ARG BALD_DIR="bald"
ARG CONTAINER_BALD_DIR="/usr/src/bald"
ARG CONTAINER_APP_DIR="/usr/src/app"
ENV PYTHONPATH="${PYTHONPATH}:${CONTAINER_APP_DIR}"

COPY ${BALD_DIR} ${CONTAINER_BALD_DIR}
COPY NCAR-NEON/exchange/combine.py ${CONTAINER_APP_DIR}/combine.py

RUN apt-get update && \
    apt-get upgrade -y && \
    apt-get install -y python && \
    apt-get install -y pip && \
    apt-get install -y python-dev && \
    apt-get install -y python-setuptools && \
    apt-get install -y python3-venv && \
    apt-get install -y rclone && \
    alias python=python3 && \
    alias pip=pip3

WORKDIR ${CONTAINER_BALD_DIR}
RUN python3 -m venv /opt/venv && \
    . /opt/venv/bin/activate && \
    pip install rdflib-jsonld && \
    pip install -r requirements.txt && \
    python setup.py install && \
    pip install google-cloud-storage

RUN groupadd -g 999 appuser && \
    useradd -r -u 999 -g appuser appuser
USER appuser

WORKDIR ${CONTAINER_APP_DIR}
CMD . /opt/venv/bin/activate && \ 
    exec python3 combine.py --rcloneRemote NEONNCAR --sourceBucket neon-ncar-artifacts --targetBucket neon-ncar --stagingPrefix staging --distPrefix NCAR-NEON --metadataVocab https://schema.org/ --metadataFormat json-ld
