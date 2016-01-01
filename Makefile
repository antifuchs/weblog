deploy:
	hugo
	goapp deploy

letsencrypt/letsencrypt-auto:
	git clone https://github.com/letsencrypt/letsencrypt
	./letsencrypt/letsencrypt-auto --debug


.PHONY: phony deploy
