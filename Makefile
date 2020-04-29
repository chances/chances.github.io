CSS_C=npx node-sass
CSS_FLAGS=--output-style compressed
CSS_SRC=src/assets/scss
CSS_OUT=site/assets/stylesheets
CSS_SOURCES=$(shell find $(CSS_SRC) -type f -name "*.scss")
CSS_TARGETS=$(patsubst $(CSS_SRC)/%.scss,$(CSS_OUT)/%.min.css,$(wildcard $(CSS_SRC)/*.scss))

all: bootstrap-and-build
.DEFAULT_GOAL := build

bootstrap: node_modules
	@git submodule update --init --recursive
.PHONY: bootstrap

node_modules:
	@npm install

build: build-party
	@echo "Building chances.github.io to ./site ..."
	@find src/assets/javascript -name \*.js -exec cp {} site/assets/javascript \;
	@make --quiet css
.PHONY: build

bootstrap-and-build: bootstrap build
.PHONY: bootstrap-and-build

build-party:
	@cd src/party && \
	make --quiet build
.PHONY: build-party

clean:
	@rm ${CSS_TARGETS}
.PHONY: clean

STATIC = ./node_modules/.bin/static

serve: build
	@${STATIC} site -z -p 3000
.PHONY: serve

CONCURRENTLY = ./node_modules/.bin/concurrently

watch: build
	@${CONCURRENTLY} -n "css,server" -c "magenta,gray.dim" --kill-others \
		"make --quiet watch-css" \
		"${STATIC} site -z -p 3000"
.PHONY: watch

watch-css:
	@fswatch -or ./src/assets/scss | xargs -n1 -I {} \
	make --quiet css
.PHONY: watch-css

watch-party:
	@cd src/party && \
	make --quiet watch
.PHONY: watch-party

css: $(CSS_TARGETS)
.PHONY: css

$(CSS_OUT):
	mkdir -p $(CSS_OUT)

$(CSS_TARGETS): $(CSS_SOURCES)
	@echo "Compiling src/assets/stylesheets/$(notdir $(basename $(basename $@))).css"
	@$(CSS_C) $(CSS_FLAGS) src/assets/scss/$(notdir $(basename $(basename $@))).scss > $@
