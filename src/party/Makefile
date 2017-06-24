TSC = ../../node_modules/.bin/tsc
BROWSERIFY = ../../node_modules/.bin/browserify
BROWSER_SYNC = ../../node_modules/.bin/browser-sync
SEMISTANDARD = ../../node_modules/.bin/semistandard
CONCURRENTLY = ../../node_modules/.bin/concurrently

JS_ENTRY_POINT := ./js/main.js
BROWSERIFY_TARGET := ../assets/javascript/party.js
REL_BROWSERIFY_TARGET := ../assets/javascript/party.js

all: build

build: app browserify

app:
	@echo "Building chances-party browser client..."
	@${TSC}

browserify:
	@echo "Entry point: ${JS_ENTRY_POINT}"
	@echo "Browserify target: ${BROWSERIFY_TARGET}"
	@${BROWSERIFY} ${JS_ENTRY_POINT} -o ${BROWSERIFY_TARGET}

lint:
	@npm run lint:party -s
	@${SEMISTANDARD} ./js/{./party,spotify/**/*}.js --fix

test:
	@echo "No tests yet."

watch:
	@echo "Entry point: ${JS_ENTRY_POINT}"
	@echo "Browserify target: ${BROWSERIFY_TARGET}"
	@${CONCURRENTLY} --kill-others \
		"cd ../..; make --quiet watch" \
		"cd ../..; make --quiet watch-css" \
		"make --quiet browser-sync" \
		"make --quiet watch-ts"

browser-sync:
	@${BROWSER_SYNC} start -s "../../site" -f "../../site" --open "ui" --startPath "/party"

watch-ts:
	# Watch target adapted from http://stackoverflow.com/a/23734495/1363247
	@while true; do \
		make --quiet app browserify; \
		inotifywait -qre close_write ./ts; \
	done

clean:
	rm -rf ./js
	rm -f ${REL_BROWSERIFY_TARGET}

.PHONY: build app browserify lint test watch browser-sync watch-ts clean
