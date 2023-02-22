/*
Package allocation provides the logic to allocate pupils to events.
*/
package allocation

import (
	"encoding/json"
	"fmt"
	"io"
	"net/http"

	"github.com/go-playground/validator/v10"
)

type Logger interface {
	Printf(format string, v ...any)
}

// Handling

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

	p, err := Validate(body)
	if err != nil {
		http.Error(w, fmt.Sprintf("Error: validating request body: %v", err), http.StatusBadRequest)

	}

	h.logger.Printf("Request comes in: %v", p) // TODO: Change or remove this line later.

}

// Payload and Validation

type payload struct {
	Events map[eventID]event `validate:"required,dive"`
	Pupils map[pupilID]pupil `validate:"required,dive"`
	Config struct {
		HowManySpecialPupils int `validate:"min=1"`
		Timeout              int `validate:"min=1"`
		NumberOfCycles       int `validate:"min=0"`
	}
}

type eventID string

type dayID string

type pupilID string

type classID string

type event struct {
	Days    []dayID   `validate:"min=1"`
	Amount  int       `validate:"min=1"`
	Classes []classID `validate:"min=1"`
}

type pupil struct {
	Class           classID `validate:"required"`
	Special         bool
	FixedAllocation map[dayID]eventID `validate:"dive,required"`
	Choices         struct {
		Green []eventID `validate:"dive,required"`
		Red   []eventID `validate:"dive,required"`
	}
}

func Validate(body []byte) (payload, error) {
	var p payload
	if err := json.Unmarshal(body, &p); err != nil {
		return p, fmt.Errorf("decoding request payload: %w", err)

	}

	if err := validator.New().Struct(p); err != nil {
		return p, fmt.Errorf("validating request payload: %w", err)
	}

	return p, nil
}
