TSC = ../../node_modules/.bin/tsc
BROWSERIFY = ../../node_modules/.bin/browserify
BROWSER_SYNC = ../../node_modules/.bin/browser-sync
TS_LINT = ../../node_modules/.bin/tslint
TS_NODE = ../../node_modules/.bin/ts-node
TAPE = ../../node_modules/.bin/tape
FAUCET = ../../node_modules/.bin/faucet
NYC = ../../node_modules/.bin/nyc
CONCURRENTLY = ../../node_modules/.bin/concurrently

JS_ENTRY_POINT := ./js/main.js
BROWSERIFY_TARGET := ../assets/javascript/party.js
REL_BROWSERIFY_TARGET := ../assets/javascript/party.js

TS_TEST_SOURCES := ./ts/test/*.ts
JS_TEST_SOURCES := ./js/test/*.js

all: build

build: app browserify
.PHONY: build

app:
	@echo "Building chances-party browser client..."
	@${TSC}
.PHONY: app

browserify:
	@echo "Entry point: ${JS_ENTRY_POINT}"
	@echo "Browserify target: ${BROWSERIFY_TARGET}"
	@${BROWSERIFY} ${JS_ENTRY_POINT} -o ${BROWSERIFY_TARGET}
.PHONY: browserify

lint:
	@${TS_LINT} -c ./tslint.json ./ts/**.ts ./ts/**.tsx
.PHONY: lint

test: lint
	@${TS_NODE} --fast ${TAPE} ${TS_TEST_SOURCES} | ${FAUCET}
.PHONY: test

cover:
	@rm -rf coverage
	# @${ISTANBUL} cover ${TAPE} ${JS_TEST_SOURCES}
	@${NYC} make test
.PHONY: cover

watch:
	@echo "Entry point: ${JS_ENTRY_POINT}"
	@echo "Browserify target: ${BROWSERIFY_TARGET}"
	@${CONCURRENTLY} --kill-others \
		"cd ../..; make --quiet watch" \
		"cd ../..; make --quiet watch-css" \
		"make --quiet browser-sync" \
		"${TSC} -w 1> /dev/null" \
		"make --quiet watch-ts"
.PHONY: watch

browser-sync:
	@${BROWSER_SYNC} start -s "../../site" -f "../../site" --open "ui" --startPath "/party"
.PHONY: browser-sync

watch-ts:
	# Watch target adapted from http://stackoverflow.com/a/23734495/1363247
	@while true; do \
		make --quiet; \
		inotifywait -qre close_write ./ts; \
	done
.PHONY: watch-ts

watch-tests:
	@while true; do \
		${TAPE} ${JS_TEST_SOURCES} | ${FAUCET}; \
		echo ""; \
		inotifywait -qre close_write ./js/test; \
	done
.PHONY: watch-tests

clean:
	rm -rf ./js
	rm -f ${REL_BROWSERIFY_TARGET}
.PHONY: clean
