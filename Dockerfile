FROM haskell:8.0

########################
# System Deps

RUN apt-get update
RUN apt-get upgrade -y --assume-yes

# Install packages needed for libraries used by our app.
RUN apt-get install -y --assume-yes curl
RUN curl -sL https://deb.nodesource.com/setup_8.x \
  | su -c bash -

RUN apt-get install -y --assume-yes \
  libpq-dev \
  g++ \
  nodejs

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
# RUN rm -rf $WORKDIR/.stack-work/

RUN docs
# RUN rm -rf $WORKDIR/documentation/

RUN elm-code
# RUN rm -rf $WORKDIR/elm-code/

RUN npm i
RUN npm run build
# RUN rm -rf $WORKDIR/node_modules/
# RUN rm -rf $WORKDIR/elm-stuff/

CMD caldwell
