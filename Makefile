building:
	echo "Stack Building"

install:
	stack build --copy-bins
	stack exec elmify
	npm run build

.DEFAULT_GOAL := building
