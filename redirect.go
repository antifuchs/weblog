package letsencrypt

import "net/http"

var redirects = map[string]string{
	"fdfun":     "https://github.com/antifuchs/bangbangcon-talk-2016",
	"!!con2016": "https://github.com/antifuchs/bangbangcon-talk-2016",
}

func init() {

	for name, target := range redirects {
		http.HandleFunc("/go/"+name, redirect(target))
	}
}

func redirect(target string) func(w http.ResponseWriter, r *http.Request) {
	return func(w http.ResponseWriter, r *http.Request) {
		http.Redirect(w, r, target, 302)
	}
}
