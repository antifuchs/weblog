DOMAIN = boinkor.net
TEST_DOMAIN = testblog.boinkor.net

## deps for running / deploying:
THEME = themes/purehugo
THEME_GIT = https://github.com/antifuchs/weblog-purehugo
THEME_BRANCH = 2019

FONTAWESOME_VERSION = v4.3.0
FONTAWESOME_GIT = https://github.com/FortAwesome/Font-Awesome
FONTAWESOME = static/

DEPLOY_DEPS := $(THEME) $(FONTAWESOME)

BASE_URL := "https://boinkor.net/"

# HUGO_BIN gets overridden on the build environment in netlify.toml; unless this is set, use the dockerized version.
$(eval $(shell grep HUGO_VERSION netlify.toml | head -n1))   # use the build.environment version
HUGO_BIN ?= docker run --rm -it -v `pwd`:/src -v `pwd`/output:/src/public -p 1313:1313 klakegg/hugo:$(HUGO_VERSION)

all: demo

dev: demo
demo: deploy_deps
	git clean -fdx public/
	${HUGO_BIN} serve -d dev --bind 0.0.0.0

## Uploading the blog:

build: $(THEME)
	git clean -fdx public/
	(cd $(THEME) ; git fetch origin && git checkout $(THEME_BRANCH) && git reset --hard origin/$(THEME_BRANCH))
	${HUGO_BIN} --gc -b $(BASE_URL) # don't --minify until https://github.com/gohugoio/hugo/issues/6472 is fixed

build_test: build

deploy_deps: $(DEPLOY_DEPS)

## Rules for downloading stuff:

# Not vendored, but also not a submodule, ugh:
$(THEME):
	git clone --single-branch -b $(THEME_BRANCH) $(THEME_GIT) $(THEME)

# Vendored; `make fontawesome` to update these.
fontawesome:
	mkdir -p vendor/ static/fonts/
	if ! [ -d vendor/fontawesome ] ; then git clone $(FONTAWESOME_GIT) vendor/fontawesome ; else (cd vendor/fontawesome ; git fetch ) ;fi
	(cd vendor/fontawesome && git reset --hard $(FONTAWESOME_VERSION) )
	cp vendor/fontawesome/css/font-awesome.min.css static/css/
	cp vendor/fontawesome/fonts/* static/fonts/

.PHONY: all demo deploy_deps build build_test fontawesome
