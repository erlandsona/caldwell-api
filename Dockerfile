FROM haskell:8.0

RUN export PATH=$(stack path --local-bin):$PATH


########################
# System Deps

RUN apt-get update
RUN apt-get upgrade -y --assume-yes

# Install packages needed for libraries used by our app.
RUN apt-get install -y --assume-yes libpq-dev g++

# Remove apt caches to reduce the size of our container.
RUN rm -rf /var/lib/apt/lists/*


########################
# Project Deps
RUN mkdir -p /app
WORKDIR /app

# Install all dependencies in app's .cabal file.
COPY ./package.yaml $WORKDIR
COPY ./stack.yaml $WORKDIR
RUN stack build --dependencies-only

COPY . $WORKDIR
RUN stack install

RUN docs
RUN elm-code
CMD caldwell
