FROM fpco/stack-build:lts-19 as build
RUN mkdir /opt/build
COPY . /opt/build
RUN cd /opt/build && stack -v  build  --resolver lts-18.22 # --system-ghc


FROM ubuntu:lts
RUN mkdir -p /opt/myapp
ARG BINARY_PATH
WORKDIR /opt/myapp
RUN apt-get update && apt-get install -y \
  ca-certificates \
  libgmp-dev
# NOTICE THIS LINE


COPY --from=build /opt/build/.stack-work/install/x86_64-linux/lts-19/9.0.2/bin .
COPY static /opt/myapp
COPY config /opt/myapp/config
CMD ["/opt/myapp/myapp"]
