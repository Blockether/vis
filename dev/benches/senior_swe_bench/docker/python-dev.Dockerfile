ARG BASE_IMAGE
FROM ${BASE_IMAGE}

USER root
ENV DEBIAN_FRONTEND=noninteractive

RUN apt-get update \
    && apt-get install -y --no-install-recommends python3-dev \
    && rm -rf /var/lib/apt/lists/*
