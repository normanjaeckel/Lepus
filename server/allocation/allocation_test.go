package allocation

import (
	"reflect"
	"strings"
	"testing"
)

func TestValidate(t *testing.T) {
	dec := newDecoder()

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
		expected := `event with ID "event_2" is not sheduled at day "day_1" or does not exist`
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
		expected := `event with ID "event_2" is not sheduled at day "day_1" or does not exist`
		if err.Error() != expected {
			t.Fatalf("wrong error: got %q, expected %q", err.Error(), expected)
		}
	})

}

func getTestData() ([][]event, []pupilID) {
	days := [][]event{
		{
			{
				id:     "e1",
				amount: 2,
			},
		},
	}

	pupils := []pupilID{
		"p1",
		"p2",
		"p3",
	}

	return days, pupils
}

func TestDoEverything(t *testing.T) {
	t.Run("test small set of data 1", func(t *testing.T) {
		days, pupils := getTestData()
		fpiList := []fixedPupilInfo{
			{pID: "p1", eID: "e1", dIdx: 0},
			{pID: "p2", eID: "e1", dIdx: 0},
		}

		got := doEverything(days, pupils, fpiList)

		expected := map[pupilID][]eventID{
			"p1": {"e1"},
			"p2": {"e1"},
			"p3": {""},
		}
		if !reflect.DeepEqual(got, expected) {
			t.Fatalf("wrong result: got %v, expected %v", got, expected)
		}
	})

	t.Run("test small set of data 2", func(t *testing.T) {
		days, pupils := getTestData()
		e := days[0][0]
		e.amount = 3
		days[0][0] = e

		fpiList := []fixedPupilInfo{
			{pID: "p1", eID: "e1", dIdx: 0},
			{pID: "p2", eID: "e1", dIdx: 0},
		}

		got := doEverything(days, pupils, fpiList)

		expected := map[pupilID][]eventID{
			"p1": {"e1"},
			"p2": {"e1"},
			"p3": {"e1"},
		}
		if !reflect.DeepEqual(got, expected) {
			t.Fatalf("wrong result: got %v, expected %v", got, expected)
		}
	})

	t.Run("test small set of data 3", func(t *testing.T) {
		days, pupils := getTestData()
		fpiList := []fixedPupilInfo{
			{pID: "p3", eID: "e1", dIdx: 0},
		}

		got := doEverything(days, pupils, fpiList)

		expected := map[pupilID][]eventID{
			"p1": {"e1"},
			"p2": {""},
			"p3": {"e1"},
		}
		if !reflect.DeepEqual(got, expected) {
			t.Fatalf("wrong result: got %v, expected %v", got, expected)
		}
	})

	t.Run("test small set of data 4", func(t *testing.T) {
		days, pupils := getTestData()
		pupils = append(pupils, "p4")
		e := days[0][0]
		e.amount = 1
		days[0][0] = e
		days[0] = append(days[0], event{id: "e2", amount: 2})
		fpiList := []fixedPupilInfo{
			{pID: "p3", eID: "e1", dIdx: 0},
		}

		got := doEverything(days, pupils, fpiList)

		expected := map[pupilID][]eventID{
			"p1": {"e2"},
			"p2": {"e2"},
			"p3": {"e1"},
			"p4": {""},
		}

		if !reflect.DeepEqual(got, expected) {
			t.Fatalf("wrong result: got %v, expected %v", got, expected)
		}
	})

	t.Run("test small set of data 5", func(t *testing.T) {
		days, pupils := getTestData()
		pupils = append(pupils, "p4")
		pupils = append(pupils, "p5")
		days[0] = append(days[0], event{id: "e2", amount: 2})

		fpiList := []fixedPupilInfo{}

		got := doEverything(days, pupils, fpiList)

		expected := map[pupilID][]eventID{
			"p1": {"e1"},
			"p2": {"e2"},
			"p3": {"e1"},
			"p4": {"e2"},
			"p5": {""},
		}
		if !reflect.DeepEqual(got, expected) {
			t.Fatalf("wrong result: got %v, expected %v", got, expected)
		}
	})

}
