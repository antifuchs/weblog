# My new blog

This is my new hugo blog. Mental note: Document how stuff works
because being unable to even operate this machinery when you want to
write a post is not going to make you want to write more posts.

## How to deploy

This blog is hosted on Google App Engine. There's a test and a live app, both live behind SSL. To deploy the test app, run:

    make deploy_test

And visit https://testblog.boinkor.net

`make deploy` and the https://boinkor.net for the live app.

## How to SSL

I use letsencrypt. This requires renewing certs every 3 months. But
it's fairly painless, except you have to text-edit a bunch of stuff.

Step 0: Have `~/.letsencrypt`

1. Make sure you are on the main branch & nothing is uncommitted.
2. Run `make certificates`
3. If it doesn't give you instructions and you know you must do a thing, run:
   `./scripts/letsencrypt-hook deploy_certificate boinkor.net ./certs/boinkor.net/privkey.pem ./certs/boinkor.net/cert.pem ./certs/boinkor.net/fullchain.pem`

## TODO

* Redirects
