CSS_C=sassc
CSS_FLAGS=-t compressed
CSS_SRC=src/assets/scss
CSS_OUT=site/assets/stylesheets
CSS_SOURCES=$(shell find $(CSS_SRC) -type f -name "*.scss")
CSS_TARGETS=$(patsubst $(CSS_SRC)/%.scss,$(CSS_OUT)/%.min.css,$(wildcard $(CSS_SRC)/*.scss))

all: build
.DEFAULT_GOAL := build

bootstrap:
	@test -d ./node_modules || npm install
.PHONY: bootstrap

build: bootstrap build-party
	@echo "Building chances.github.io to ./site ..."
	@cp src/assets/javascript/* site/assets/javascript
	@make --quiet css
.PHONY: build

build-party:
	@cd src/party && \
	make --quiet build
.PHONY: build-party

clean:
	@rm -rf
	@cd src && \
	stack exec site clean
	@stack clean
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
	@$(CSS_C) $(CSS_FLAGS) src/assets/scss/$(notdir $(basename $(basename $@))).scss $@
