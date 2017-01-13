ENTRY_POINT := src/Main.elm
OUT_DIR := ./dist
TARGET := ../js/party.elm.js
JS_ENTRY_POINT := src/party/js/party.js
BROWSERIFY_TARGET := src/assets/javascript/party.js
REL_BROWSERIFY_TARGET := ../../assets/javascript/party.js

build: packages app browserify

packages: elm-package.json
	@elm package install --yes

app:
	@echo "Building chances-party browser client..."
	elm make ${ENTRY_POINT} --output ${TARGET}

browserify:
	@echo "Entry point: ${JS_ENTRY_POINT}"
	@echo "Browserify target: ${BROWSERIFY_TARGET}"
	@ENTRY_POINT=${JS_ENTRY_POINT} TARGET=${BROWSERIFY_TARGET} npm run build:party

lint:
	@npm run lint:party

test:
	@npm run test:party

watch:
	# Watch target adapted from http://stackoverflow.com/a/23734495/1363247
	@while true; do \
        make --quiet app browserify; \
        inotifywait -qre close_write ./src ../js; \
    done

clean:
	rm -f ${TARGET}
	rm -f ${REL_BROWSERIFY_TARGET}

.PHONY: build app packages lint test watch clean
