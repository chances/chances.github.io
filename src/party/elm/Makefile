ENTRY_POINT := src/Main.elm
OUT_DIR := ./dist
TARGET := ../../assets/javascript/party.elm.js

build:
	@echo "Building chances-party browser client..."
	elm package install --yes
	elm make ${ENTRY_POINT} --output ${TARGET}

clean:
	rm ${TARGET}

.PHONY: build clean
