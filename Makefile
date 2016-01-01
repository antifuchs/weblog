CERTS = certs/testblog.boinkor.net.key.pem certs/testblog.boinkor.net.cert.pem

deploy:
	hugo
	goapp deploy

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
