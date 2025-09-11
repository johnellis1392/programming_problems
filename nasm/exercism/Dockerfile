FROM ubuntu:latest
WORKDIR /usr/src/app
VOLUME /usr/src/app
RUN apt-get update && apt-get install -y \
  build-essential gcc make nasm \
  gcc-multilib g++-multilib
ENTRYPOINT [ "/usr/bin/bash" ]