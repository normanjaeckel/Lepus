package allocation

import (
	"encoding/json"
	"fmt"
	"io"
	"sort"

	"github.com/go-playground/validator/v10"
)

// Decoded data

type decodedData struct {
	days    [][]event
	fpiList []fixedPupilInfo
	pupils  []pupil
}

type event struct {
	id      eventID
	amount  int
	classes []classID
}

type pupil struct {
	id    pupilID
	class classID
}

type fixedPupilInfo struct {
	p    pupil
	eID  eventID
	dIdx int
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

	// Get all days from body's events map and transform it to a list of list of
	// event IDs.
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
	sort.Slice(dayList, func(i, j int) bool { return dayList[i] < dayList[j] })
	dayMap := make(map[dayID]int)
	for idx, dID := range dayList {
		dayMap[dID] = idx
	}
	days := make([][]event, len(dayMap))
	for eID, e := range body.Events {
		for _, dID := range e.Days {
			dayIdx := dayMap[dID]
			days[dayIdx] = append(days[dayIdx], event{id: eID, amount: e.Amount, classes: e.Classes})
		}

	}

	// Get all pupils and fixed pupils
	pupils := make([]pupil, len(body.Pupils))
	var fpiList []fixedPupilInfo
	for pID, pData := range body.Pupils {
		p := pupil{id: pID, class: pData.Class}
		pupils = append(pupils, p)

		for dID, eID := range pData.FixedAllocation {
			dIdx, ok := dayMap[dID]
			if !ok {
				return decodedData{}, fmt.Errorf("day ID %q does not exist in events map", dID)
			}
			found := false
			for _, e := range days[dIdx] {
				if eID == e.id {
					found = true
					break
				}
			}
			if !found {
				return decodedData{}, fmt.Errorf("event with ID %q is not sheduled at day %q or does not exist", eID, dID)
			}
			fpiList = append(
				fpiList,
				fixedPupilInfo{
					p:    p,
					eID:  eID,
					dIdx: dayMap[dID],
				},
			)
		}
	}

	return decodedData{days, fpiList, pupils}, nil
}
