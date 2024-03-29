/*
Package allocation provides the logic to allocate pupils to events.
*/
package allocation

import (
	"fmt"
	"math/rand"
	"net/http"
	"sort"
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

	w.Header().Set("Content-Type", "application/json")
	if err := encode(res, w); err != nil {
		http.Error(w, fmt.Sprintf("Error: encoding response body: %v", err), http.StatusInternalServerError)
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
		resultPupils[fpi.p.id][fpi.dIdx] = fpi.eID
	}

	shuffle(shuffleSrc, allPupils)
	for _, p := range allPupils {
		for dIdx, events := range days {
			// Skip if pupils is already allocated.
			if resultPupils[p.id][dIdx] != "" {
				continue
			}

			// Sort events according to numbers of allocated pupils.
			stats := make(map[eventID]int)
			for _, eList := range resultPupils {
				stats[eList[dIdx]]++
			}
			sort.SliceStable(events, func(i, j int) bool {
				return stats[events[i].id] < stats[events[j].id]
			})

			// Loop over all events and try to find a free seat.
			for _, e := range events {
				if canPupilVisitThisEvent(p, e, dIdx, resultPupils, allPupils) {
					resultPupils[p.id][dIdx] = e.id
					break
				}
			}
		}
	}

	// TODO: Step 2: Früherer Algorithmus um restliche Schüler noch reinzudrücken.

	// TODO: Step 3: Ggf. Tauschen um Anzahl der Grünwünsche zu verbessern.

	return resultPupils
}

func canPupilVisitThisEvent(p pupil, e event, dIdx int, currentPupils map[pupilID][]eventID, allPupils []pupil) bool {
	// Event does not accept pupils from this class.
	if !inList(p.class, e.classes) {
		return false
	}

	// Pupil must not be in this event in another day
	if inList(e.id, currentPupils[p.id]) {
		return false
	}

	// Exclude red states.
	if inList(e.id, p.redState) {
		return false
	}

	// Get statistics
	stats := make(map[classID]int)
	occupied := 0
	for pID, eList := range currentPupils {
		if eList[dIdx] == e.id {
			for _, p := range allPupils {
				if p.id == pID {
					stats[p.class]++
					occupied++
					break
				}
			}
		}
	}

	// All seats in this event are occupied
	if occupied >= e.amount {
		return false
	}

	// Check for class
	if !freeForClass(stats, occupied, e, p.class) {
		return false
	}

	return true
}

// pupilSurplus says how much the number of pupils from one class may exeed the number of pupils from other classes.
const pupilSurplus int = 2

// freeForClass returns true if a pupil from the
// given class id can be allocated with this event according to the currently
// present pupils (respecting their classes).
func freeForClass(stats map[classID]int, occupied int, e event, cID classID) bool {
	missingClasses := howManyClassesMustBePresent(e) - len(stats)
	if missingClasses > 0 {
		// There are less classes present than required.
		if !inList(cID, keys(stats)) {
			// Our class is not present so it is permitted.
			return true
		}
		// Our class is present: Keep at least one seat free for every required
		// class and look of there are still enough free seats.
		free := e.amount - occupied - missingClasses
		if free < 1 {
			return false
		}
		// Only permit pupil if there arn't too much pupils for its own class.
		return stats[cID] < 1+pupilSurplus
	}

	// There are enough classes present: Look for the smallest number and permit
	// pupil if there arn't too much pupils for its own class.
	smallestNum := 0
	for _, n := range stats {
		if smallestNum == 0 || n < smallestNum {
			smallestNum = n
		}
	}
	return stats[cID] < smallestNum+pupilSurplus
}

func howManyClassesMustBePresent(e event) int {
	// All classes must be present, we say at the moment.
	return len(e.classes)
}

// Helpers

// inList returns true of the list contains the element.
func inList[T comparable](element T, list []T) bool {
	for _, e := range list {
		if element == e {
			return true
		}
	}
	return false
}

// keys returns a slice of all keys of the given map.
func keys[K comparable, V any](m map[K]V) []K {
	k := make([]K, len(m))
	for i := range m {
		k = append(k, i)
	}
	return k
}
