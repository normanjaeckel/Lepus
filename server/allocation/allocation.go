/*
Package allocation provides the logic to allocate pupils to events.
*/
package allocation

import (
	"encoding/json"
	"fmt"
	"math/rand"
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
	return handler{logger, newDecoder()}
}

func (h handler) ServeHTTP(w http.ResponseWriter, r *http.Request) {
	data, err := h.decoder.decode(r.Body)
	if err != nil {
		http.Error(w, fmt.Sprintf("Error: invalid request body: %v", err), http.StatusBadRequest)
		return
	}

	res := doEverything(rand.Shuffle, data.days, data.pupils, data.fpiList)

	// Encode result
	w.Header().Set("Content-Type", "application/json")
	if err := json.NewEncoder(w).Encode(res); err != nil {
		http.Error(w, fmt.Sprintf("Error: marshalling response body: %v", err), http.StatusInternalServerError)
	}
}

// Shuffling

type shuffleSource func(int, func(i, j int))

func shuffle[T any](fn shuffleSource, s []T) {
	fn(len(s), func(i, j int) { s[i], s[j] = s[j], s[i] })
}

// Allocation

func doEverything(shuffleSrc shuffleSource, days [][]event, allPupils []pupil, fixedPupils []fixedPupilInfo) map[pupilID][]eventID {

	resultPupils := make(map[pupilID][]eventID, len(allPupils))

	for _, p := range allPupils {
		resultPupils[p.id] = make([]eventID, len(days))
	}

	for _, fpi := range fixedPupils {
		resultPupils[fpi.pID][fpi.dIdx] = fpi.eID
	}

	shuffle(shuffleSrc, allPupils)
	for _, p := range allPupils {
		for dIdx, events := range days {
			if resultPupils[p.id][dIdx] != "" {
				continue
			}
			shuffle(shuffleSrc, events)
			for _, e := range events {
				if canPupilVisitThisEvent(p, e, dIdx, resultPupils) {
					resultPupils[p.id][dIdx] = e.id
					break
				}
			}
		}
	}

	return resultPupils
}

func canPupilVisitThisEvent(p pupil, e event, dIdx int, currentPupils map[pupilID][]eventID) bool {
	// Event does not accept pupils from this class.
	if !inList(p.class, e.classes) {
		return false
	}

	// Pupil must not be in this event in another day
	if inList(e.id, currentPupils[p.id]) {
		return false
	}

	occupied := 0
	for _, eList := range currentPupils {
		if eList[dIdx] == e.id {
			occupied++
		}
	}
	return e.amount > occupied
}

func inList[T comparable](element T, list []T) bool {
	for _, e := range list {
		if element == e {
			return true
		}
	}
	return false
}
