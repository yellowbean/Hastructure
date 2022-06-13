FROM haskell:8.10.7-buster

WORKDIR /app 

ADD . /app

RUN stack build

EXPOSE 8081

CMD ["stack exec Hastructure-exe"]
