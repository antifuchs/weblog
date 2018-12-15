DOMAIN = boinkor.net
TEST_DOMAIN = testblog.boinkor.net

## deps for running / deploying:
THEME = themes/purehugo
THEME_GIT = https://github.com/antifuchs/weblog-purehugo

FONTAWESOME_VERSION = v4.3.0
FONTAWESOME_GIT = https://github.com/FortAwesome/Font-Awesome
FONTAWESOME = statics/

DEPLOY_DEPS := $(THEME) $(FONTAWESOME)

all: demo

demo: deploy_deps
	hugo serve -d dev

## Uploading the blog:

build: $(THEME)
	git clean -fdx public/
	hugo -b https://${DOMAIN}/

build_test: $(THEME)
	git clean -fdx public/
	hugo -b /

deploy_deps: $(DEPLOY_DEPS)

## Rules for downloading stuff:

# Not vendored, but also not a submodule, ugh:
$(THEME):
	git clone $(THEME_GIT) $(THEME)

# Vendored; `make fontawesome` to update these.
fontawesome:
	mkdir -p vendor/ static/fonts/
	if ! [ -d vendor/fontawesome ] ; then git clone $(FONTAWESOME_GIT) vendor/fontawesome ; else (cd vendor/fontawesome ; git fetch ) ;fi
	(cd vendor/fontawesome && git reset --hard $(FONTAWESOME_VERSION) )
	cp vendor/fontawesome/css/font-awesome.min.css static/css/
	cp vendor/fontawesome/fonts/* static/fonts/

.PHONY: all demo deploy_deps build build_test fontawesome
