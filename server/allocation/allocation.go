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
	return handler{logger, newDecoder()}
}

func (h handler) ServeHTTP(w http.ResponseWriter, r *http.Request) {
	data, err := h.decoder.decode(r.Body)
	if err != nil {
		http.Error(w, fmt.Sprintf("Error: invalid request body: %v", err), http.StatusBadRequest)
		return
	}

	res := doEverything(data.days, data.pupils, data.fpiList)

	// Encode result
	w.Header().Set("Content-Type", "application/json")
	if err := json.NewEncoder(w).Encode(res); err != nil {
		http.Error(w, fmt.Sprintf("Error: marshalling response body: %v", err), http.StatusInternalServerError)
	}
}

// Allocation

func doEverything(days [][]eventID, allPupilIDs []pupilID, fixedPupils []fixedPupilInfo) map[pupilID][]eventID {

	pupils := make(map[pupilID][]eventID, len(allPupilIDs))

	for _, pID := range allPupilIDs {
		pupils[pID] = make([]eventID, len(days))
	}

	for _, fpi := range fixedPupils {
		pupils[fpi.pupil][fpi.day] = fpi.event
	}

	for _, pID := range allPupilIDs {
		for dIdx, events := range days {
			if pupils[pID][dIdx] != "" {
				continue
			}
			event := events[0]
			// TODO: Event nehmen, bei dem Regel stimmen, und dann:
			pupils[pID][dIdx] = event
		}
	}

	// for dID := range days {
	// 	orderedPupils := make([]pupilID, 0, len(res[dID].UnassignedPupils))
	// 	for pID := range res[dID].UnassignedPupils {
	// 		orderedPupils = append(orderedPupils, pID)
	// 	}
	// 	sort.Slice(orderedPupils, func(i, j int) bool { return orderedPupils[i] < orderedPupils[j] })

	// 	orderedEvents := make([]eventID, 0, len(events))
	// 	for eID, e := range events {
	// 		if found := e.days[dID]; found {
	// 			orderedEvents = append(orderedEvents, eID)
	// 		}
	// 	}
	// 	sort.Slice(orderedEvents, func(i, j int) bool { return orderedEvents[i] < orderedEvents[j] })

	// 	i := -1
	// 	for _, pID := range orderedPupils {
	// 		for k := 0; k < len(orderedEvents); k++ {
	// 			i++
	// 			eID := orderedEvents[i%len(orderedEvents)]
	// 			if events[eID].amount > len(res[dID].Events[eID]) {
	// 				res[dID].Events[eID] = append(res[dID].Events[eID], pID)
	// 				delete(res[dID].UnassignedPupils, pID)
	// 				break
	// 			}
	// 		}
	// 	}
	// }

	return pupils
}

// func canPupilVisitThisEvent(p pupil, dID dayID, e event) bool {
// 	return true
// }
