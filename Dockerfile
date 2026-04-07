FROM node:25-bookworm

# Install R
RUN apt-get update && \
    apt-get install -y r-base && \
    rm -rf /var/lib/apt/lists/*
