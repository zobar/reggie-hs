FROM debian:latest
ENV PATH /root/.local/bin:$PATH
RUN apt-get update\
 && apt-get --assume-yes --no-install-recommends install ca-certificates curl g++ gcc libc6-dev libffi-dev libgmp-dev make xz-utils zlib1g-dev\
 && curl --location --output - https://get.haskellstack.org | sh\
 && mkdir /usr/src/app\
 && cd /usr/src/app\
 && stack setup\
 && rm --force --recursive /var/lib/apt/lists/*
WORKDIR /usr/src/app
