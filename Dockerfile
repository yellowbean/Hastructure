FROM fpco/stack-build:lts-19 as build
RUN mkdir /opt/build
COPY . /opt/build
RUN cd /opt/build && stack build  --copy-bins  \ 
    --local-bin-path /opt/build  --resolver lts-18.22 # --system-ghc


FROM ubuntu:22.04
RUN mkdir -p /opt/myapp
ARG BINARY_PATH
WORKDIR /opt/myapp
RUN apt-get update && apt-get install -y \
  ca-certificates \
  libgmp-dev
# NOTICE THIS LINE


COPY --from=build /opt/build/Hastructure-exe .
COPY --from=build /opt/build/config.yml .
#COPY config.yml /opt/myapp
CMD ["/opt/myapp/Hastructure-exe"]
