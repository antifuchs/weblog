CERTS = certs/testblog.boinkor.net.key.pem certs/testblog.boinkor.net.cert.pem certs/boinkor.net.key.pem certs/boinkor.net.cert.pem
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

certificates: $(CERTS)

# File rules

$(THEME):
	git clone $(THEME_GIT) $(THEME)

letsencrypt/letsencrypt-auto:
	git clone https://github.com/letsencrypt/letsencrypt
	./letsencrypt/letsencrypt-auto --debug

certs:
	mkdir -p certs

certs/testblog.boinkor.net.key.pem: certs
	sudo openssl rsa -inform pem -in /etc/letsencrypt/live/testblog.boinkor.net/privkey.pem -outform pem > $@

certs/testblog.boinkor.net.cert.pem: certs
	sudo cat /etc/letsencrypt/live/testblog.boinkor.net/fullchain.pem > $@

certs/boinkor.net.key.pem: certs
	sudo openssl rsa -inform pem -in /etc/letsencrypt/live/boinkor.net/privkey.pem -outform pem > $@

certs/boinkor.net.cert.pem: certs
	sudo cat /etc/letsencrypt/live/boinkor.net/fullchain.pem > $@

.PHONY: phony all deploy_test certificates demo deploy_deps
