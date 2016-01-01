//go:generate hugo

package letsencrypt

import (
	"fmt"
	"net/http"
)

var challenges = map[string]string{
	// live blog:
	"vGN3naaZkeVUFR6OFLm2oCuljQeMpWBvetWo4vg6GlY": "vGN3naaZkeVUFR6OFLm2oCuljQeMpWBvetWo4vg6GlY.-dY7lM01g0wLcbiFBYNNacLm5cnT9vrS09MvG2cHhOc",
	// testblog:
	"F4BmE6MsjOQQFrEdchLwqu73PRdow03pYZ9hj3uFktU": "F4BmE6MsjOQQFrEdchLwqu73PRdow03pYZ9hj3uFktU.-dY7lM01g0wLcbiFBYNNacLm5cnT9vrS09MvG2cHhOc",
}

func init() {
	for challenge, response := range challenges {
		http.HandleFunc("/.well-known/acme-challenge/"+challenge, respond(response))
	}
}

func respond(response string) func(w http.ResponseWriter, r *http.Request) {
	return func(w http.ResponseWriter, r *http.Request) {
		fmt.Fprint(w, response)
	}
}
