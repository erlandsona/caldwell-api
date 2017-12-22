FROM fpco/stack-build:lts-9.10

########################
# Extra System Deps

RUN apt-get -q update
# Don't care about installing latest versions of packages.
# RUN apt-get -qq upgrade
RUN apt-get -qq install entr silversearcher-ag
RUN rm -rf /var/lib/apt/lists/*
