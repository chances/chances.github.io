ENTRY_POINT := src/Main.elm
OUT_DIR := ./dist
TARGET := ./js/party.elm.js
JS_ENTRY_POINT := src/party/js/main.js
BROWSERIFY_TARGET := src/assets/javascript/party.js
REL_BROWSERIFY_TARGET := ../assets/javascript/party.js

build: app browserify

app:
	@echo "Building chances-party browser client..."
	@npm run tsc

browserify:
	@echo "Entry point: ${JS_ENTRY_POINT}"
	@echo "Browserify target: ${BROWSERIFY_TARGET}"
	@ENTRY_POINT=${JS_ENTRY_POINT} TARGET=${BROWSERIFY_TARGET} npm run build:party

lint:
	@npm run lint:party -s

test:
	@npm run test:party

watch:
	# Watch target adapted from http://stackoverflow.com/a/23734495/1363247
	@while true; do \
        make --quiet app browserify; \
        inotifywait -qre close_write ./ts; \
    done

clean:
	rm -f ${TARGET}
	rm -f ${REL_BROWSERIFY_TARGET}

.PHONY: build app packages lint test watch clean
