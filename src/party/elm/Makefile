clean:
	rm -rf ./elm-stuff
	rm -rf ./dist

ENTRY_POINT := src/Types/TokenResponse.elm
OUT_DIR := ./dist
TARGET := ./dist/party.js

build:
	@echo "Building chances-party browser client..."
	elm package install --yes
	@mkdir -p ${OUT_DIR}
	elm make ${ENTRY_POINT} --output ${TARGET}

.PHONY: clean build
