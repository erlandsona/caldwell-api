FROM haskell:8.0

RUN export PATH=$(stack path --local-bin):$PATH

RUN apt-get update
RUN apt-get upgrade -y
RUN apt-get install libpq-dev g++ -y
RUN mkdir -p /app
WORKDIR /app
COPY . $WORKDIR
RUN stack build --dependencies-only
RUN stack install
