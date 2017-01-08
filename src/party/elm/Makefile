ENTRY_POINT := src/Main.elm
OUT_DIR := ./dist
TARGET := ../../assets/javascript/party.elm.js

clean:
	rm ${TARGET}

build:
	@echo "Building chances-party browser client..."
	elm package install --yes
	elm make ${ENTRY_POINT} --output ${TARGET}

.PHONY: clean build
