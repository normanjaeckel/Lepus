/*
Package allocation provides the logic to allocate pupils to events.
*/
package allocation

import (
	"encoding/json"
	"fmt"
	"net/http"
)

// Data types

type eventID string
type dayID string
type classID string
type pupilID string

// Handling

type logger interface {
	Printf(format string, v ...any)
}

type handler struct {
	logger  logger
	decoder decoder
}

func Handle(logger logger) http.Handler {
	return handler{logger, decoder{}}
}

func (h handler) ServeHTTP(w http.ResponseWriter, r *http.Request) {
	data, err := h.decoder.decode(r.Body)
	if err != nil {
		http.Error(w, fmt.Sprintf("Error: invalid request body: %v", err), http.StatusBadRequest)
		return
	}

	res := doEverything(data.days, data.pupils, data.events, data.fpiList)

	// Encode result
	w.Header().Set("Content-Type", "application/json")
	if err := json.NewEncoder(w).Encode(res); err != nil {
		http.Error(w, fmt.Sprintf("Error: marshalling response body: %v", err), http.StatusInternalServerError)
	}
}

// Allocation

type result map[dayID]dayResult

type dayResult struct {
	Events           map[eventID][]pupilID
	UnassignedPupils map[pupilID]bool
}

func newResult(days map[dayID]bool, pupils map[pupilID]pupil) result {
	res := make(result)
	for dID := range days {
		dayRes := dayResult{
			Events:           make(map[eventID][]pupilID),
			UnassignedPupils: make(map[pupilID]bool),
		}
		for pID := range pupils {
			dayRes.UnassignedPupils[pID] = true
		}
		res[dID] = dayRes
	}

	return res
}

func doEverything(days map[dayID]bool, pupils map[pupilID]pupil, events map[eventID]event, fixedPupils []fixedPupilInfo) map[dayID]dayResult {
	res := newResult(days, pupils)

	for _, fpi := range fixedPupils {
		res[fpi.day].Events[fpi.event] = append(res[fpi.day].Events[fpi.event], fpi.pupil)
		delete(res[fpi.day].UnassignedPupils, fpi.pupil)
	}

	for dID := range days {

		orderedPupils := make([]pupilID, 0, len(res[dID].UnassignedPupils))
		for pID := range res[dID].UnassignedPupils {
			orderedPupils = append(orderedPupils, pID)
		}

		orderedEvents := make([]eventID, 0, len(events))
		for eID, e := range events {
			if found := e.days[dID]; found {
				orderedEvents = append(orderedEvents, eID)
			}

		}

		i := 0
		for _, pID := range orderedPupils {
			eID := orderedEvents[i%len(orderedEvents)]
			if events[eID].amount > len(res[dID].Events[eID]) {
				res[dID].Events[eID] = append(res[dID].Events[eID], pID)
				delete(res[dID].UnassignedPupils, pID)
			}
			i += 1
		}
	}

	return res
}
