/*
Package allocation provides the logic to allocate pupils to events.
*/
package allocation

import (
	_ "embed"
	"encoding/json"
	"fmt"
	"io"
	"net/http"

	"github.com/santhosh-tekuri/jsonschema/v5"
)

// Schema

//go:embed schema.json
var schemaFile string

const SchemaURL = "https://raw.githubusercontent.com/normanjaeckel/Lepus/main/server/schema.json"

var schema = jsonschema.MustCompileString(SchemaURL, schemaFile)

// Handler

type Logger interface {
	Printf(format string, v ...any)
}

type handler struct {
	logger Logger
}

func Handle(logger Logger) http.Handler {
	return handler{logger}
}

func (h handler) ServeHTTP(w http.ResponseWriter, req *http.Request) {
	// Read body
	body, err := io.ReadAll(req.Body)
	if err != nil {
		http.Error(w, fmt.Sprintf("Error: reading request body: %v", err), http.StatusBadRequest)
		return
	}

	var payload interface{}
	if err := json.Unmarshal(body, &payload); err != nil {
		http.Error(w, fmt.Sprintf("Error: decoding request: %v", err), http.StatusBadRequest)
		return
	}

	// Validate body
	if err = schema.Validate(payload); err != nil {
		http.Error(w, fmt.Sprintf("Error: invalid request: %v", err), http.StatusBadRequest)
		return
	}

	h.logger.Printf("Request comes in: %v", payload) // TODO: Change or remove this line later.

}
