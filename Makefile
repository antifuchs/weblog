THEME = themes/purehugo
THEME_GIT = https://github.com/antifuchs/weblog-purehugo

DOMAIN = boinkor.net
TEST_DOMAIN = testblog.boinkor.net

LIVE_APP = boinkor-net-blog-live
TEST_APP = plated-analyzer-117711

all: demo

demo: deploy_deps
	hugo serve -d dev

## Uploading the blog:

build: $(THEME)
	git clean -fdx public/
	hugo -b https://${DOMAIN}/

build_test: $(THEME)
	git clean -fdx public/
	hugo -b https://${TEST_DOMAIN}/

deploy_deps: $(THEME)

# File rules

$(THEME):
	git clone $(THEME_GIT) $(THEME)

.PHONY: all demo deploy_deps build build_test
