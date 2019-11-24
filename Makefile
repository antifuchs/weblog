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

all: demo

dev: demo
demo: deploy_deps
	hugo serve -d dev

## Uploading the blog:

build: $(THEME)
	git clean -fdx public/
	hugo --gc --minify -b $(BASE_URL)

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
