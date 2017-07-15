FROM haskell:8

RUN export PATH=$(stack path --local-bin):$PATH

RUN mkdir -p /app
WORKDIR /app
COPY stack.yaml .
COPY *.cabal ./
RUN stack build --dependencies-only

COPY . /app
RUN stack install
