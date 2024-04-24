# syntax=docker/dockerfile:1
# Note: the context directory for this dockerfile is ..

# see https://github.com/arnaudblois/python-ubuntu-image
FROM ursamajorlab/jammy-python:3.12

# Setup timezone
ENV TZ Europe/Berlin
RUN ln -snf /usr/share/zoneinfo/$TZ /etc/localtime && echo $TZ > /etc/timezone

# Install required packages here
RUN apt-get clean && apt-get update && apt-get install -y \
  locales \
  tzdata \
  git \
  vim \
  g++ \
  gcc \
  libc6-dev \
  libffi-dev \
  libgmp-dev \
  make \
  xz-utils \
  zlib1g-dev \
  gnupg \
  netbase \
  curl \
  build-essential \
  cmake \
  g++-multilib \
  libgcc-9-dev \
  lib32gcc-9-dev \
  ccache

RUN apt-get install -y \
  wabt \
  graphviz \
  spim

RUN curl -sL https://deb.nodesource.com/setup_21.x | bash
RUN apt-get install -y nodejs

RUN useradd -m praktomat

# We use a fresh tmpfs with /home in each container
RUN chmod 1777 /home

# Setup locale
RUN locale-gen en_US.UTF-8
RUN update-locale LANG=en_US.UTF-8 LANGUAGE=en_US.UTF-8
ENV LANG en_US.UTF-8
ENV LANGUAGE en_US.UTF-8

RUN python -m ensurepip --upgrade --root /usr/local --verbose
RUN ln -s /usr/local/bin/pip3.12 /usr/local/bin/pip
RUN ln -s /usr/local/bin/pip3.12 /usr/local/bin/pip3

COPY docker/install-iwasm-linux.sh /install-iwasm-linux.sh
RUN bash /install-iwasm-linux.sh

RUN mkdir -p /cc-support/wasm-support
COPY ./requirements.txt /cc-support/requirements.txt
COPY ./package.json /cc-support/package.json
RUN pip install -r /cc-support/requirements.txt
RUN cd /cc-support && npm install && \
  ln -s /cc-support/node_modules/.bin/pyright /usr/local/bin/pyright

COPY ./wasm-support /cc-support/wasm-support
RUN cd /cc-support/wasm-support && rm -rf native-lib/build
RUN cd /cc-support/wasm-support && make native WAMR_ROOT_DIR=/cc-support/wasm-micro-runtime

RUN mkdir /cc
WORKDIR /cc
