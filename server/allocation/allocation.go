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

type logger interface {
	Printf(format string, v ...any)
}

// // Data type

// type Data struct {
// 	Events map[eventID]struct {
// 		Days    []dayID   `validate:"min=1"`
// 		Amount  int       `validate:"min=1"`
// 		Classes []classID `validate:"min=1"`
// 	} `validate:"required,dive"`

// 	Pupils map[pupilID]struct {
// 		Class           classID `validate:"required"`
// 		Special         bool
// 		FixedAllocation map[dayID]eventID `validate:"dive,required"`
// 		Choices         struct {
// 			Green []eventID `validate:"dive,required"`
// 			Red   []eventID `validate:"dive,required"`
// 		}
// 	} `validate:"required,dive"`

// 	Config struct {
// 		HowManySpecialPupils int `validate:"min=1"`
// 		Timeout              int `validate:"min=1"`
// 		NumberOfCycles       int `validate:"min=0"`
// 	} `validate:"required"`
// }

type eventID string
type dayID string

// type classID string
type pupilID string

// Handling

type handler struct {
	logger  logger
	decoder decoder
}

func Handle(logger logger) http.Handler {
	return handler{logger, decoder{}}
}

func (h handler) ServeHTTP(w http.ResponseWriter, r *http.Request) {
	fixedPupilInfos, err := h.decoder.decode(r.Body)
	if err != nil {
		http.Error(w, fmt.Sprintf("Error: invalid request body: %v", err), http.StatusBadRequest)
		return
	}

	res := doEverything(fixedPupilInfos)

	// Encode result
	w.Header().Set("Content-Type", "application/json")
	if err := json.NewEncoder(w).Encode(res); err != nil {
		http.Error(w, fmt.Sprintf("Error: marshalling response body: %v", err), http.StatusInternalServerError)
	}
}

type decoder struct {
	validate *validator.Validate
}

func (v *decoder) decode(r io.Reader) ([]fixedPupilInfo, error) {
	if v.validate == nil {
		v.validate = validator.New()
	}

	// Decode request body

	var body struct {
		Events map[string]struct {
			Days    []string `validate:"min=1"`
			Amount  int      `validate:"min=1"`
			Classes []string `validate:"min=1"`
		} `validate:"required,dive"`

		Pupils map[string]struct {
			Class           string `validate:"required"`
			Special         bool
			FixedAllocation map[string]string `validate:"dive,required"`
			Choices         struct {
				Green []string `validate:"dive,required"`
				Red   []string `validate:"dive,required"`
			}
		} `validate:"required,dive"`

		Config struct {
			HowManySpecialPupils int `validate:"min=1"`
			Timeout              int `validate:"min=1"`
			NumberOfCycles       int `validate:"min=0"`
		} `validate:"required"`
	}

	if err := json.NewDecoder(r).Decode(&body); err != nil {
		return nil, fmt.Errorf("decoding: %w", err)
	}

	// Validate request body

	if err := v.validate.Struct(body); err != nil {
		return nil, fmt.Errorf("validating: %w", err)

	}

	// Transform request body

	var fpiList []fixedPupilInfo
	for pID, pData := range body.Pupils {
		for dID, eID := range pData.FixedAllocation {
			fpi := fixedPupilInfo{
				pupil: pupilID(pID),
				event: eventID(eID),
				day:   dayID(dID),
			}
			fpiList = append(fpiList, fpi)
		}

	}

	return fpiList, nil

}

// Allocation

type result map[dayID]dayResult

type dayResult struct {
	Events           map[eventID][]pupilID
	UnassignedPupils []pupilID
}

type fixedPupilInfo struct {
	pupil pupilID
	event eventID
	day   dayID
}

func doEverything(fixedPupils []fixedPupilInfo) map[dayID]dayResult {

	res := make(result)

	for _, fpi := range fixedPupils {
		day := fpi.day
		if _, ok := res[day]; !ok {
			res[day] = dayResult{Events: make(map[eventID][]pupilID)}
		}
		res[day].Events[fpi.event] = append(res[day].Events[fpi.event], fpi.pupil)

	}

	return res
}
