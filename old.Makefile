# This is the rather dirty Makefile – but it's self-documenting! So if you just run `make` it tells you what it can do, kinda.
# The commands without comments don't get included :)


# install: #Setup# stack, entr, yarn, elm, webpack (OS X only)
# 	# @TODO make this immutable?
# 	# brew install haskell-stack yarn
# 	# yarn global add elm@0.18.0 elm-test webpack
# 	# stack setup
# 	# So turns out buildpack-stack runs make install... so lets
# 	# just put this here temporarily
# 	stack setup
# 	stack build --fast --copy-bins

live: #Development# Run both frontend/backend and rebuild/rerun when files change
	@./scripts/live

live-frontend:
	find . | grep '\./client/.*' | entr -r make build-client

live-backend:
	find . | grep '\./src/.*'; find . | grep '\./apps/.*'; find . | grep '\./db/.*'; | entr -r make build-server

build-client:
	yarn build-dev | grep -v --line-buffered "./~/"

build-server: #Tools# Build & export server types to Elm
	stack build --pedantic --fast
	stack exec elm-code
	stack exec docs
	elm-format client/Server.elm --yes
	stack exec caldwell
	# @TODO don't export if the file is unchanged, so we don't trigger Elm rebuilds needlessly

test: #Development# Run server/frontend tests
	stack test
	elm-test

# wstest: #Development# Descriptions
# 	# There's an issue with thor that prevents npm install -g thor
# 	# https://github.com/observing/thor/issues/45#issuecomment-275754673
# 	# Maybe it'll be resolved eventually
# 	/Users/mario/.nvm/versions/node/v7.3.0/lib/node_modules/thor/bin/thor --amount 10 --messages 1 --generator test/thor-gen.js ws://localhost:8081/ws
# 	# With a load tester instead
# 	artillery run test/artillery.yml
# 	# Use this to fix file descriptor issue on OS X
# 	# https://blog.dekstroza.io/ulimit-shenanigans-on-osx-el-capitan/

# format: #Development# Format elm source with elm-format standard
# 	elm-format client/Main.elm

# build: #Development# Build server binary and client app.js
# 	stack build --pedantic --fast
# 	# yarn run build

# perf:
# 	# stack build --pedantic --fast --ghc-options="-fno-warn-orphans -ddump-splices" --profile --threaded
# 	/usr/local/lib/node_modules/elm/Elm-Platform/0.18.0/.cabal-sandbox/bin/elm-make client/App.elm --output=public/app-elm-make.js +RTS -s -p


# be: build

# clean: ## Clean build and test artifacts
	# Not fun to accidentally run this on a mobile-only connection :(
	# rm -rf ./elm-stuff ./public/app.js ./tests/elm-stuff

# watch: #Development# Rebuild server when files change
# 	stack build --pedantic --file-watch-poll --fast --ghc-options -ddump-splices


# precommit: #Tools# Install git pre-commit hook to ensure build is green
# 	echo "set -e; make build; make test" > .git/hooks/pre-commit
# 	chmod a+x .git/hooks/pre-commit

# format-install: format-install-clean #Tools# Install custom 2-spaced elm-format to ~/.local/bin
# 	@./scripts/install-elm-format.sh

# heroku:
# 	heroku buildpacks:clear
# 	heroku buildpacks:add https://github.com/heroku/heroku-buildpack-multi

# sizes:
# 	elm make client/App.elm --output=public/app-elm-make.js
# 	# yarn run build
# 	@echo
# 	# Elm Stats
# 	@echo
# 	@echo "`du -sh public/app-elm-make.js`   elm make"
# 	@echo "`du -sh public/app.js`            webpack + uglify"
# 	@echo "`du -sh public/app.js.gz`         webpack gzipped"
# 	@echo
# 	# Binary stats
# 	@echo
# 	@du -sh `which adventurePresenter`
# 	@otool -L `which adventurePresenter` | grep "/usr" | cut -d ' ' -f1 | xargs du -sh

# node-updates:
# 	# Need to have npm install -g npm-check-updates installed for this to work
# 	ncu

help:
	@./scripts/makefile-help ./Makefile

.DEFAULT_GOAL := help
