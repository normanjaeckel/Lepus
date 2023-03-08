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

func getTestData() ([][]event, []pupil) {
	days := [][]event{
		{
			{
				id:      "e1",
				amount:  2,
				classes: []classID{"2a"},
			},
		},
	}

	pupils := []pupil{
		{id: "p1", class: "2a"},
		{id: "p2", class: "2a"},
		{id: "p3", class: "2a"},
	}

	return days, pupils
}

func noShuffleSrc(n int, swap func(int, int)) {}

func reverseShuffleSrc(length int, swap func(int, int)) {
	for i, j := 0, length-1; i < j; i, j = i+1, j-1 {
		swap(i, j)
	}
}

func TestDoEverything(t *testing.T) {

	t.Run("test small set of data 1", func(t *testing.T) {
		days, pupils := getTestData()
		fpiList := []fixedPupilInfo{
			{p: pupils[0], eID: "e1", dIdx: 0},
			{p: pupils[1], eID: "e1", dIdx: 0},
		}

		got := doEverything(noShuffleSrc, days, pupils, fpiList)

		expected := map[pupil][]eventID{
			{"p1", "2a"}: {"e1"},
			{"p2", "2a"}: {"e1"},
			{"p3", "2a"}: {""},
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
			{p: pupils[0], eID: "e1", dIdx: 0},
			{p: pupils[1], eID: "e1", dIdx: 0},
		}

		got := doEverything(noShuffleSrc, days, pupils, fpiList)

		expected := map[pupil][]eventID{
			{"p1", "2a"}: {"e1"},
			{"p2", "2a"}: {"e1"},
			{"p3", "2a"}: {"e1"},
		}
		if !reflect.DeepEqual(got, expected) {
			t.Fatalf("wrong result: got %v, expected %v", got, expected)
		}
	})

	t.Run("test small set of data 3", func(t *testing.T) {
		days, pupils := getTestData()
		fpiList := []fixedPupilInfo{
			{p: pupils[2], eID: "e1", dIdx: 0},
		}

		got := doEverything(noShuffleSrc, days, pupils, fpiList)

		expected := map[pupil][]eventID{
			{"p1", "2a"}: {"e1"},
			{"p2", "2a"}: {""},
			{"p3", "2a"}: {"e1"},
		}
		if !reflect.DeepEqual(got, expected) {
			t.Fatalf("wrong result: got %v, expected %v", got, expected)
		}
	})

	t.Run("test small set of data 4", func(t *testing.T) {
		days, pupils := getTestData()
		pupils = append(pupils, pupil{id: "p4", class: "2a"})
		e := days[0][0]
		e.amount = 1
		days[0][0] = e
		days[0] = append(days[0], event{id: "e2", amount: 2, classes: []classID{"2a"}})
		fpiList := []fixedPupilInfo{
			{p: pupils[2], eID: "e1", dIdx: 0},
		}

		got := doEverything(noShuffleSrc, days, pupils, fpiList)

		expected := map[pupil][]eventID{
			{"p1", "2a"}: {"e2"},
			{"p2", "2a"}: {"e2"},
			{"p3", "2a"}: {"e1"},
			{"p4", "2a"}: {""},
		}

		if !reflect.DeepEqual(got, expected) {
			t.Fatalf("wrong result: got %v, expected %v", got, expected)
		}
	})

	t.Run("test small set of data 5", func(t *testing.T) {
		days, pupils := getTestData()
		pupils = append(pupils, pupil{id: "p4", class: "2a"})
		pupils = append(pupils, pupil{id: "p5", class: "2a"})
		days[0] = append(days[0], event{id: "e2", amount: 2, classes: []classID{"2a"}})

		fpiList := []fixedPupilInfo{}

		got := doEverything(reverseShuffleSrc, days, pupils, fpiList)

		expected := map[pupil][]eventID{
			{"p1", "2a"}: {""},
			{"p2", "2a"}: {"e1"},
			{"p3", "2a"}: {"e2"},
			{"p4", "2a"}: {"e1"},
			{"p5", "2a"}: {"e2"},
		}

		if !reflect.DeepEqual(got, expected) {
			t.Fatalf("wrong result: got %v, expected %v", got, expected)
		}
	})

	t.Run("test small set of data with extra class", func(t *testing.T) {
		days, pupils := getTestData()
		fpiList := []fixedPupilInfo{}
		p := pupils[1]
		p.class = "1a"
		pupils[1] = p

		got := doEverything(reverseShuffleSrc, days, pupils, fpiList)

		expected := map[pupil][]eventID{
			{"p1", "2a"}: {"e1"},
			{"p2", "1a"}: {""},
			{"p3", "2a"}: {"e1"},
		}
		if !reflect.DeepEqual(got, expected) {
			t.Fatalf("wrong result: got %v, expected %v", got, expected)
		}
	})

	t.Run("test small set of data with two days 1", func(t *testing.T) {
		days, pupils := getTestData()
		fpiList := []fixedPupilInfo{}
		days = append(days, []event{
			{
				id:      "e1",
				amount:  2,
				classes: []classID{"2a"},
			},
			{
				id:      "e2",
				amount:  2,
				classes: []classID{"2a"},
			},
		})

		got := doEverything(noShuffleSrc, days, pupils, fpiList)

		expected := map[pupil][]eventID{
			{"p1", "2a"}: {"e1", "e2"},
			{"p2", "2a"}: {"e1", "e2"},
			{"p3", "2a"}: {"", "e1"},
		}
		if !reflect.DeepEqual(got, expected) {
			t.Fatalf("wrong result: got %v, expected %v", got, expected)
		}
	})

	t.Run("test small set of data with two days 2", func(t *testing.T) {
		days, pupils := getTestData()
		fpiList := []fixedPupilInfo{}
		days = append(days, []event{
			{
				id:      "e1",
				amount:  2,
				classes: []classID{"2a"},
			},
			{
				id:      "e2",
				amount:  2,
				classes: []classID{"2a"},
			},
		})
		pupils = append(pupils, pupil{id: "p4", class: "2a"})
		pupils = append(pupils, pupil{id: "p5", class: "2a"})

		got := doEverything(noShuffleSrc, days, pupils, fpiList)

		expected := map[pupil][]eventID{
			{"p1", "2a"}: {"e1", "e2"},
			{"p2", "2a"}: {"e1", "e2"},
			{"p3", "2a"}: {"", "e1"},
			{"p4", "2a"}: {"", "e1"},
			{"p5", "2a"}: {"", ""},
		}
		if !reflect.DeepEqual(got, expected) {
			t.Fatalf("wrong result: got %v, expected %v", got, expected)
		}
	})

	t.Run("test small set of data with two classes and maximum number of pupils of one class", func(t *testing.T) {
		_, pupils := getTestData()
		fpiList := []fixedPupilInfo{}
		days := [][]event{
			{
				{
					id:      "e1",
					amount:  5,
					classes: []classID{"2a", "2b"},
				},
			},
		}
		pupils = append(pupils, pupil{id: "p4", class: "2b"})
		pupils = append(pupils, pupil{id: "p5", class: "2a"})
		pupils = append(pupils, pupil{id: "p6", class: "2a"})
		pupils = append(pupils, pupil{id: "p7", class: "2b"})
		pupils = append(pupils, pupil{id: "p8", class: "2b"})
		pupils = append(pupils, pupil{id: "p9", class: "2b"})
		pupils = append(pupils, pupil{id: "p10", class: "2b"})

		got := doEverything(noShuffleSrc, days, pupils, fpiList)

		expected := map[pupil][]eventID{
			{"p1", "2a"}:  {"e1"},
			{"p2", "2a"}:  {"e1"},
			{"p3", "2a"}:  {"e1"},
			{"p4", "2b"}:  {"e1"},
			{"p5", "2a"}:  {""},
			{"p6", "2a"}:  {""},
			{"p7", "2b"}:  {"e1"},
			{"p8", "2b"}:  {""},
			{"p9", "2b"}:  {""},
			{"p10", "2b"}: {""},
		}
		if !reflect.DeepEqual(got, expected) {
			t.Fatalf("wrong result: got %v, expected %v", got, expected)
		}
	})

}
