package allocation

import (
	"encoding/json"
	"fmt"
	"io"

	"github.com/go-playground/validator/v10"
)

// Decoded data

type decodedData struct {
	days    map[dayID]bool
	events  map[eventID]event
	fpiList []fixedPupilInfo
	pupils  map[pupilID]pupil
}

type event struct {
	days map[dayID]bool
}

type pupil struct{}

type fixedPupilInfo struct {
	pupil pupilID
	event eventID
	day   dayID
}

// Decoder

type decoder struct {
	validate *validator.Validate
}

func (v *decoder) decode(r io.Reader) (decodedData, error) {
	if v.validate == nil {
		v.validate = validator.New()
	}

	// Decode request body

	var body struct {
		Events map[eventID]struct {
			Days    []dayID   `validate:"min=1"`
			Amount  int       `validate:"min=1"`
			Classes []classID `validate:"min=1"`
		} `validate:"required,dive"`

		Pupils map[pupilID]struct {
			Class           classID `validate:"required"`
			Special         bool
			FixedAllocation map[dayID]eventID `validate:"dive,required"`
			Choices         struct {
				Green []eventID `validate:"dive,required"`
				Red   []eventID `validate:"dive,required"`
			}
		} `validate:"required,dive"`

		Config struct {
			HowManySpecialPupils int `validate:"min=1"`
			Timeout              int `validate:"min=1"`
			NumberOfCycles       int `validate:"min=0"`
		} `validate:"required"`
	}

	if err := json.NewDecoder(r).Decode(&body); err != nil {
		return decodedData{}, fmt.Errorf("decoding: %w", err)
	}

	// Validate request body

	if err := v.validate.Struct(body); err != nil {
		return decodedData{}, fmt.Errorf("validating: %w", err)

	}

	// Transform request body to decoded data

	data := decodedData{}

	// Get all days and events
	data.days = make(map[dayID]bool)
	data.events = make(map[eventID]event)
	for eID, e := range body.Events {
		data.events[eID] = event{days: make(map[dayID]bool)}
		for _, dID := range e.Days {
			data.events[eID].days[dID] = true
			data.days[dID] = true
		}
	}

	// Get all pupils
	data.pupils = make(map[pupilID]pupil)
	for pID := range body.Pupils {
		data.pupils[pID] = pupil{}
	}

	// Get fixed pupils
	for pID, pData := range body.Pupils {
		for dID, eID := range pData.FixedAllocation {
			if _, ok := data.days[dID]; !ok {
				return decodedData{}, fmt.Errorf("day ID %q does not exist in events map", dID)
			}
			if _, ok := data.events[eID]; !ok {
				return decodedData{}, fmt.Errorf("event ID %q does not exist in events map", eID)
			}
			if _, ok := data.events[eID].days[dID]; !ok {
				return decodedData{}, fmt.Errorf("event with ID %q is not sheduled at day %q", eID, dID)
			}

			fpi := fixedPupilInfo{
				pupil: pID,
				event: eID,
				day:   dID,
			}
			data.fpiList = append(data.fpiList, fpi)
		}

	}
	return data, nil
}
