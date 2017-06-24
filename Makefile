CSS_C=sassc
CSS_FLAGS=-t compressed
CSS_SRC=src/assets/scss
CSS_OUT=site/assets/stylesheets
CSS_SOURCES=$(shell find $(CSS_SRC) -type f -name "*.scss")
CSS_TARGETS=$(patsubst $(CSS_SRC)/%.scss,$(CSS_OUT)/%.min.css,$(wildcard $(CSS_SRC)/*.scss))

all: build css

build: build-hakyll build-party
	@echo "Building chances.github.io to ./site ..."
	@cd src && \
	stack exec site rebuild
.PHONY: build

build-hakyll:
	@echo "Building Hakyll site builder ..."
	@stack build
.PHONY: build-hakyll

build-party:
	@cd src/party && \
	make --quiet build
.PHONY: build-party

clean:
	@rm -rf dist
	@cd src && \
	stack exec site clean
	@stack clean
.PHONY: clean

serve: build
	@cd src && \
	stack exec site serve
.PHONY: serve

watch: build
	@cd src && \
	stack exec site watch
.PHONY: watch

watch-css:
	@while true; do \
		make --quiet css; \
        inotifywait -qre close_write ./src/assets/scss; \
    done
.PHONY: watch-css

watch-party:
	@npm run watch:party
.PHONY: watch-party

deploy: build
	@echo "Deploying chances.github.io via master branch"
	@echo "  At commit" `git rev-parse --verify HEAD`
	@rm -rf dist
	@git clone `git config remote.origin.url` dist
	@cd dist && \
	git checkout master
	@rm -rf dist/*
	@cp -r site/* src/CNAME .gitattributes README.md LICENSE dist/.
	@export SHA=`git rev-parse --verify HEAD` && \
	export SHA_SMALL=`echo $$SHA | cut -c1-7` && \
	export COMMIT="$$(git log -1 --pretty=medium)" && \
	export COMMIT_MSG_SUBJECT="$$(git log -1 --pretty=format:%-s)" && \
	export MESSAGE="$$COMMIT_MSG_SUBJECT ($$SHA_SMALL)\n\nDeploy $$SHA" && \
	cd dist && \
	git add . && \
	git commit -m "$$(printf "$$MESSAGE")" && \
	git push origin master
.PHONY: deploy

css: $(CSS_TARGETS)
.PHONY: css

$(CSS_OUT):
	mkdir -p $(CSS_OUT)

$(CSS_TARGETS): $(CSS_SOURCES)
	@echo "Compiling src/assets/stylesheets/$(notdir $(basename $(basename $@))).css"
	@$(CSS_C) $(CSS_FLAGS) src/assets/scss/$(notdir $(basename $(basename $@))).scss $@
