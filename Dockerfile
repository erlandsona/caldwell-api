FROM fpco/stack-build:lts-9.10

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
# RUN rm -rf /var/lib/apt/lists/*


########################
# Project Deps
RUN mkdir -p /caldwell
WORKDIR /caldwell

# Install all dependencies in app's .cabal file.
COPY . $WORKDIR
CMD ./scripts/build
