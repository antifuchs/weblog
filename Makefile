CERTS = certs/testblog.boinkor.net.key.pem certs/testblog.boinkor.net.cert.pem
THEME = themes/purehugo
THEME_GIT = https://github.com/toru-mano/purehugo

deploy: $(THEME)
	hugo
	goapp deploy

$(THEME):
	git clone $(THEME_GIT) $(THEME)

certificates: $(CERTS)

letsencrypt/letsencrypt-auto:
	git clone https://github.com/letsencrypt/letsencrypt
	./letsencrypt/letsencrypt-auto --debug

certs:
	mkdir -p certs

certs/testblog.boinkor.net.key.pem: certs
	sudo openssl rsa -inform pem -in /etc/letsencrypt/live/testblog.boinkor.net/privkey.pem -outform pem > $@

certs/testblog.boinkor.net.cert.pem: certs
	sudo cat /etc/letsencrypt/live/testblog.boinkor.net/fullchain.pem > $@

.PHONY: phony deploy certificates
