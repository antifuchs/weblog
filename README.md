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

1. Make sure you are on the main branch & nothing is uncommitted.
1. Run `./letsencrypt-auto --debug -a manual certonly -d boinkor.net -d testblog.boinkor.net`
2. Copy each of the challenge/response pairs and paste them into [letsencrypt.go](blob/letsencrypt.go)
3. Run `make deploy deploy_test`
4. Wait until you can `curl` the challenge URL, then hit return in the letsencrypt challenge / response prompt and hope that they found the response.
5. Run `make certificates`
6. pbcopy each certificate/private key and paste it into a new cert on
  * https://console.cloud.google.com/appengine/settings/certificates?project=plated-analyzer-117711&moduleId=default&versionId=1
  * https://console.cloud.google.com/appengine/settings/certificates?project=plated-analyzer-117711&moduleId=default&versionId=1 and


## TODO

* Redirects
* Fully automatic letsencrypt cert renewal flow
