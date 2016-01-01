THEME = themes/purehugo
THEME_GIT = https://github.com/toru-mano/purehugo

all: deploy_test

deploy_test: deploy_deps
	scripts/build https://testblog.boinkor.net plated-analyzer-117711

deploy: deploy_deps
	scripts/build https://boinkor.net boinkor-net-blog-live

deploy_deps: $(THEME)

demo:
	hugo serve

certificates: letsencrypt.sh $(HOME)/.letsencrypt certs
	./letsencrypt.sh/letsencrypt.sh -c -f letsencrypt-config.sh

# File rules

$(THEME):
	git clone $(THEME_GIT) $(THEME)

letsencrypt.sh:
	git clone https://github.com/lukas2511/letsencrypt.sh

certs:
	mkdir -p certs

.PHONY: phony all deploy_test certificates demo deploy_deps
