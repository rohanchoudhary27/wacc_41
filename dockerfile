FROM ubuntu:22.04

RUN apt-get update && apt-get install -y \
    gcc-aarch64-linux-gnu \
    qemu-user \
    openjdk-17-jdk \
    curl \
    make \
    git \
    && rm -rf /var/lib/apt/lists/*

# Install Coursier and Scala 3.7.4
RUN curl -fL "https://github.com/coursier/launchers/raw/master/cs-aarch64-pc-linux.gz" | gzip -d > /usr/local/bin/cs && \
    chmod +x /usr/local/bin/cs && \
    cs setup --yes --jvm 17 && \
    cs install scala:3.7.4 && \
    cs install scalac:3.7.4

# Add Coursier bin to PATH
ENV PATH="/root/.local/share/coursier/bin:${PATH}"

WORKDIR /app
