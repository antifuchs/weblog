THEME = themes/purehugo
THEME_GIT = https://github.com/toru-mano/purehugo

DOMAIN = boinkor.net
TEST_DOMAIN = testblog.boinkor.net

LIVE_APP = boinkor-net-blog-live
TEST_APP = plated-analyzer-117711

all: $(TEST_DOMAIN)

demo: deploy_deps
	hugo serve

## (Re-)issuing certificates:

certificates: letsencrypt.sh $(HOME)/.letsencrypt certs
	./letsencrypt.sh/letsencrypt.sh -c -f letsencrypt-config.sh

## Uploading the blog:

$(DOMAIN): deploy
$(TEST_DOMAIN): deploy_test

deploy_test: deploy_deps
	scripts/build https://$(TEST_DOMAIN) $(TEST_APP)

deploy: deploy_deps
	scripts/build https://$(DOMAIN) $(LIVE_APP)

deploy_deps: $(THEME)

# File rules

$(THEME):
	git clone $(THEME_GIT) $(THEME)

letsencrypt.sh:
	git clone https://github.com/lukas2511/letsencrypt.sh

certs:
	mkdir -p certs

.PHONY: phony all deploy_test certificates demo deploy_deps $(DOMAIN) $(TEST_DOMAIN)
