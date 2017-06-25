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

build: css app browserify
.PHONY: build

app:
	@echo "Building chances-party browser client..."
	@${TSC}
.PHONY: app

css:
	@cp -r scss/* ../assets/scss/.
.PHONY: css

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
		"cd ../..; make --quiet watch &> /dev/null" \
		"cd ../..; make --quiet watch-css" \
		"make --quiet browser-sync" \
		"${TSC} -w 1> /dev/null" \
		"make --quiet watch-scss" \
		"make --quiet watch-js"
.PHONY: watch

browser-sync:
	@${BROWSER_SYNC} start -s "../../site" -f "../../site" --open "ui" --startPath "/party"
.PHONY: browser-sync

watch-scss:
	# Watch target adapted from http://stackoverflow.com/a/23734495/1363247
	@while true; do \
		cp -r scss/* ../assets/scss/.; \
		inotifywait -qre close_write ./scss; \
	done
.PHONY: watch-scss

watch-js:
	# Watch target adapted from http://stackoverflow.com/a/23734495/1363247
	@while true; do \
		make --quiet browserify; \
		inotifywait -qre close_write ./js; \
	done
.PHONY: watch-js

watch-tests:
	@while true; do \
		${TS_NODE} --fast ${TAPE} ${TS_TEST_SOURCES} | ${FAUCET} \
		echo ""; \
		inotifywait -qre close_write ./ts/test; \
	done
.PHONY: watch-tests

clean:
	rm -rf ./js
	rm -f ${REL_BROWSERIFY_TARGET}
.PHONY: clean
