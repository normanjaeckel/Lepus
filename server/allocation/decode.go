package allocation

import (
	"encoding/json"
	"fmt"
	"io"

	"github.com/go-playground/validator/v10"
)

// Decoded data

type decodedData struct {
	days [][]eventID
	//events  map[eventID]event
	fpiList []fixedPupilInfo
	pupils  []pupilID
}

// type event struct {
// 	days   map[dayID]bool
// 	amount int
// }

type fixedPupilInfo struct {
	pupil pupilID
	event eventID
	day   int
}

// Decoder

type decoder struct {
	validate *validator.Validate
}

func newDecoder() decoder {
	return decoder{validate: validator.New()}
}

func (d decoder) decode(r io.Reader) (decodedData, error) {

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

	if err := d.validate.Struct(body); err != nil {
		return decodedData{}, fmt.Errorf("validating: %w", err)

	}

	// Transform request body to decoded data

	// Get all days
	daySet := make(map[dayID]struct{})
	for _, e := range body.Events {
		for _, dID := range e.Days {
			daySet[dID] = struct{}{}
		}
	}
	dayList := make([]dayID, 0, len(daySet))
	for dID := range daySet {
		dayList = append(dayList, dID)
	}
	dayMap := make(map[dayID]int)
	for idx, dID := range dayList {
		dayMap[dID] = idx
	}

	days := make([][]eventID, len(dayMap))
	for eID, e := range body.Events {
		for _, dID := range e.Days {
			dayIdx := dayMap[dID]
			days[dayIdx] = append(days[dayIdx], eID)
		}

	}

	// Get all pupils
	pupils := make([]pupilID, len(body.Pupils))
	for pID := range body.Pupils {
		pupils = append(pupils, pID)
	}

	// Get fixed pupils
	var fpiList []fixedPupilInfo
	for pID, pData := range body.Pupils {
		for dID, eID := range pData.FixedAllocation {

			// TODO: Reimplement checks ...
			// if _, ok := days[dID]; !ok {
			// 	return decodedData{}, fmt.Errorf("day ID %q does not exist in events map", dID)
			// }
			// if _, ok := data.events[eID]; !ok {
			// 	return decodedData{}, fmt.Errorf("event ID %q does not exist in events map", eID)
			// }
			// if _, ok := data.events[eID].days[dID]; !ok {
			// 	return decodedData{}, fmt.Errorf("event with ID %q is not sheduled at day %q", eID, dID)
			// }

			fpiList = append(
				fpiList,
				fixedPupilInfo{
					pupil: pID,
					event: eID,
					day:   dayMap[dID],
				},
			)
		}

	}

	return decodedData{days, fpiList, pupils}, nil

}
