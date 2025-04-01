FROM haskell:slim-bullseye as build
RUN mkdir /opt/build
COPY . /opt/build
RUN cd /opt/build && cabal install


FROM --platform=linux/amd64 ubuntu:25.04
RUN mkdir -p /opt/myapp
ARG BINARY_PATH
WORKDIR /opt/myapp
RUN apt-get update && apt-get install -y \
  ca-certificates \
  libgmp-dev
# NOTICE THIS LINE


COPY --from=build /opt/build/dist/Hastructure-exe .
COPY --from=build /opt/build/config.yml .
COPY --from=build /opt/build/swagger.json .
#COPY config.yml /opt/myapp
CMD ["/opt/myapp/Hastructure-exe"]
