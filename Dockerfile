FROM rocker/geospatial

RUN apt-get update -y && apt-get -y install git cmake make gcc vim python python-dev python-pip libsqlite3-dev libssl-dev libffi-dev graphviz db-util

RUN git clone https://bitbucket.org/depauldbgroup/provenance-to-use /ptu && cd /ptu && sh run.sh -r
RUN git clone https://github.com/yesworkflow-org/yw-prototypes /yw

RUN pip install reprozip reprounzip sciunit2
