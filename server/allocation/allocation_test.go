package allocation

import (
	"reflect"
	"strings"
	"testing"
)

func TestValidate(t *testing.T) {
	var dec decoder

	t.Run("test correct body", func(t *testing.T) {
		body := `
			{
				"events": {
					"event_1": {
						"days": ["day_1", "day_3"],
						"amount": 15,
						"classes": ["1a", "1b", "2a", "2b"]
					}
				},
				"pupils": {
					"pupil_1": {
						"class": "2a",
						"specialPupil": false,
						"fixedAllocation": {
							"day_1": "event_1"
						},
						"choices": {
							"green": ["event_76", "event_84"],
							"red": ["event_13"]
						}
					}
				},
				"config":
					{
						"howManySpecialPupils": 2,
						"Timeout": 20,
						"numberOfCycles": 1000
					}
			}
		`

		if _, err := dec.decode(strings.NewReader(body)); err != nil {
			t.Fatalf("expected correct validation and decoding, got error %q", err.Error())
		}

		// if p.Config.HowManySpecialPupils != 2 ||
		// 	p.Config.Timeout != 20 ||
		// 	p.Config.NumberOfCycles != 1000 {
		// 	t.Fatalf("wrong content for p.Config, expected some values (see test code), got %v", p.Config)
		// }
		// for _, pupil := range p.Pupils {
		// 	if pupil.Class != "2a" {
		// 		t.Fatalf("wrong content for p.Pupils, expected some values (see test code), got %v", p.Pupils)
		// 	}
		// }

	})

	t.Run("test body with invalid day IDs (not existing day)", func(t *testing.T) {
		body := `
			{
				"events": {
					"event_1": {
						"days": ["day_1"],
						"amount": 15,
						"classes": ["1a"]
					}
				},
				"pupils": {
					"pupil_1": {
						"class": "1a",
						"specialPupil": false,
						"fixedAllocation": {
							"day_42": "event_1"
						}
					}
				},
				"config":
					{
						"howManySpecialPupils": 2,
						"Timeout": 20,
						"numberOfCycles": 1000
					}
			}
		`

		_, err := dec.decode(strings.NewReader(body))
		if err == nil {
			t.Fatal("got nil, expected decoding error")
		}
		expected := `day ID "day_42" does not exist in events map`
		if err.Error() != expected {
			t.Fatalf("wrong error: got %q, expected %q", err.Error(), expected)
		}

	})

	t.Run("test body with invalid event IDs (event does not exist)", func(t *testing.T) {
		body := `
			{
				"events": {
					"event_1": {
						"days": ["day_1"],
						"amount": 15,
						"classes": ["1a"]
					}
				},
				"pupils": {
					"pupil_1": {
						"class": "1a",
						"specialPupil": false,
						"fixedAllocation": {
							"day_1": "event_2"
						}
					}
				},
				"config":
					{
						"howManySpecialPupils": 2,
						"Timeout": 20,
						"numberOfCycles": 1000
					}
			}
		`

		_, err := dec.decode(strings.NewReader(body))
		if err == nil {
			t.Fatal("got nil, expected decoding error")
		}
		expected := `event ID "event_2" does not exist in events map`
		if err.Error() != expected {
			t.Fatalf("wrong error: got %q, expected %q", err.Error(), expected)
		}
	})

	t.Run("test body with invalid event IDs (event that is not sheduled at the given day)", func(t *testing.T) {
		body := `
			{
				"events": {
					"event_1": {
						"days": ["day_1"],
						"amount": 15,
						"classes": ["1a"]
					},
					"event_2": {
						"days": ["day_2"],
						"amount": 15,
						"classes": ["1a"]
					}
				},
				"pupils": {
					"pupil_1": {
						"class": "1a",
						"specialPupil": false,
						"fixedAllocation": {
							"day_1": "event_2"
						}
					}
				},
				"config":
					{
						"howManySpecialPupils": 2,
						"Timeout": 20,
						"numberOfCycles": 1000
					}
			}
		`

		_, err := dec.decode(strings.NewReader(body))
		if err == nil {
			t.Fatal("got nil, expected decoding error")
		}
		expected := `event with ID "event_2" is not sheduled at day "day_1"`
		if err.Error() != expected {
			t.Fatalf("wrong error: got %q, expected %q", err.Error(), expected)
		}
	})

}

func TestDoEverything(t *testing.T) {
	t.Run("test small set of data 1", func(t *testing.T) {
		days := make(map[dayID]bool)
		days["d1"] = true

		pupils := make(map[pupilID]pupil)
		pupils["p1"] = pupil{}
		pupils["p2"] = pupil{}
		pupils["p3"] = pupil{}

		events := make(map[eventID]event)
		events["e1"] = event{days: days, amount: 2}

		fpiList := []fixedPupilInfo{
			{pupil: "p1", event: "e1", day: "d1"},
			{pupil: "p2", event: "e1", day: "d1"},
		}

		got := doEverything(days, pupils, events, fpiList)

		expected := map[dayID]dayResult{
			"d1": {
				Events:           map[eventID][]pupilID{"e1": {"p1", "p2"}},
				UnassignedPupils: map[pupilID]bool{"p3": true},
			},
		}
		if !reflect.DeepEqual(got, expected) {
			t.Fatalf("wrong result: got %v, expected %v", got, expected)
		}
	})

	t.Run("test small set of data 2", func(t *testing.T) {
		days := make(map[dayID]bool)
		days["d1"] = true

		pupils := make(map[pupilID]pupil)
		pupils["p1"] = pupil{}
		pupils["p2"] = pupil{}
		pupils["p3"] = pupil{}

		events := make(map[eventID]event)
		events["e1"] = event{days: days, amount: 3}

		fpiList := []fixedPupilInfo{
			{pupil: "p1", event: "e1", day: "d1"},
			{pupil: "p2", event: "e1", day: "d1"},
		}

		got := doEverything(days, pupils, events, fpiList)

		expected := map[dayID]dayResult{
			"d1": {
				Events:           map[eventID][]pupilID{"e1": {"p1", "p2", "p3"}},
				UnassignedPupils: map[pupilID]bool{},
			},
		}
		if !reflect.DeepEqual(got, expected) {
			t.Fatalf("wrong result: got %v, expected %v", got, expected)
		}
	})

	t.Run("test small set of data 3", func(t *testing.T) {
		days := make(map[dayID]bool)
		days["d1"] = true

		pupils := make(map[pupilID]pupil)
		pupils["p1"] = pupil{}
		pupils["p2"] = pupil{}
		pupils["p3"] = pupil{}

		events := make(map[eventID]event)
		events["e1"] = event{days: days, amount: 2}

		fpiList := []fixedPupilInfo{
			{pupil: "p3", event: "e1", day: "d1"},
		}

		got := doEverything(days, pupils, events, fpiList)

		expected := map[dayID]dayResult{
			"d1": {
				Events:           map[eventID][]pupilID{"e1": {"p3", "p1"}},
				UnassignedPupils: map[pupilID]bool{"p2": true},
			},
		}
		if !reflect.DeepEqual(got, expected) {
			t.Fatalf("wrong result: got %v, expected %v", got, expected)
		}
	})

}
