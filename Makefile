clean:
	@rm -rf dist
	@cd src && \
	stack exec site clean
	@stack clean

build-hakyll:
	@echo "Building Hakyll site builder ..."
	@stack build

build-party:
	@cd src/party && \
	make --quiet build

build: build-hakyll build-party
	@echo "Building chances.github.io to ./site ..."
	@cd src && \
	stack exec site rebuild

serve: build
	@cd src && \
	stack exec site serve

watch: build
	@cd src && \
	stack exec site watch

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

.PHONY: clean build build-hakyll deploy
