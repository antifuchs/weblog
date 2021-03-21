DOMAIN = boinkor.net
TEST_DOMAIN = testblog.boinkor.net

## deps for running / deploying:
THEME = themes/purehugo
THEME_GIT = https://github.com/antifuchs/weblog-purehugo
THEME_BRANCH = 2019

FONTAWESOME_VERSION = 5.12.0
FONTAWESOME_GIT = https://github.com/FortAwesome/Font-Awesome
FONTAWESOME = static/

DEPLOY_DEPS := $(THEME) $(FONTAWESOME)

BASE_URL := "https://boinkor.net/"

# HUGO_BIN gets overridden on the build environment in netlify.toml; unless this is set, use the dockerized version.
HUGO_BIN ?= nix run ".\#hugo" --

all: demo

dev: demo
demo: deploy_deps
	git clean -fdx public/
	${HUGO_BIN} serve -d dev --buildDrafts --buildFuture --bind 0.0.0.0

## Uploading the blog:

build: $(THEME)
	git clean -fdx public/
	@(cd $(THEME) ; git fetch origin && git checkout $(THEME_BRANCH) && git reset --hard origin/$(THEME_BRANCH))
	@drafts=$$(${HUGO_BIN} list drafts) ; [ -z $$drafts ] || (echo "\nCan't deploy - drafts exist:"; echo "$$drafts\n" ; exit 1)
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
	cp vendor/fontawesome/css/all.min.css static/css/font-awesome.min.css
	cp -Rp vendor/fontawesome/webfonts static/

.PHONY: all demo deploy_deps build build_test fontawesome
