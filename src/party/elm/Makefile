ENTRY_POINT := src/Main.elm
OUT_DIR := ./dist
JS_ENTRY_POINT := ../../assets/javascript/party.main.js
TARGET := ../../assets/javascript/party.elm.js
CAT_TARGET := ../../assets/javascript/party.js

build: packages app

app:
	@echo "Building chances-party browser client..."
	elm make ${ENTRY_POINT} --output ${TARGET}
	@cat ${TARGET} > ${CAT_TARGET}
	@cat ${JS_ENTRY_POINT} >> ${CAT_TARGET}

packages: elm-package.json
	@elm package install --yes

watch:
	# Watch target adapted from http://stackoverflow.com/a/23734495/1363247
	@while true; do \
        make --quiet app; \
        inotifywait -qre close_write ./src; \
    done

clean:
	rm ${TARGET}
	rm ${CAT_TARGET}

.PHONY: build app packages watch clean
