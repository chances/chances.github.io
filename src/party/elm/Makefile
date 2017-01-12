ENTRY_POINT := src/Main.elm
OUT_DIR := ./dist
JS_ENTRY_POINT := ../../assets/javascript/party.main.js
TARGET := ../../assets/javascript/party.elm.js
CAT_TARGET := ../../assets/javascript/party.js

build:
	@echo "Building chances-party browser client..."
	elm package install --yes
	elm make ${ENTRY_POINT} --output ${TARGET}
	@cat ${TARGET} > ${CAT_TARGET}
	@cat ${JS_ENTRY_POINT} >> ${CAT_TARGET}

clean:
	rm ${TARGET}
	rm ${CAT_TARGET}

.PHONY: build clean
