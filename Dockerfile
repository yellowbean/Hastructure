FROM fpco/stack-build:lts as build

RUN mkdir /opt/build

COPY . /opt/build

RUN cd /opt/build && stack build --copy-bins

FROM debian:stable

RUN mkdir -p /opt/myapp

ARG BINARY_PATH

WORKDIR /opt/myapp

RUN apt-get update && apt-get install -y \
  ca-certificates \
  libgmp-dev

#COPY --from=build /opt/build/.stack-work/install/x86_64-linux/stable/8.0.2/bin .

ADD . /app

RUN stack build

EXPOSE 8081

CMD ["Hastructure-exe"]
