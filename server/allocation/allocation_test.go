package allocation

import (
	"reflect"
	"strings"
	"testing"
)

func TestValidate(t *testing.T) {
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
							"day_1": "event_42"
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

		var v decoder

		if _, err := v.decode(strings.NewReader(body)); err != nil {
			t.Fatalf("expected correct validation, got error %q", err.Error())
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

}

func TestDoEverything(t *testing.T) {
	fpiList := []fixedPupilInfo{
		{pupil: "p1", event: "e1", day: "d1"},
		{pupil: "p2", event: "e1", day: "d1"},
	}

	got := doEverything(fpiList)

	expected := map[dayID]dayResult{
		"d1": {
			Events: map[eventID][]pupilID{"e1": {"p1", "p2"}},
		},
	}
	if !reflect.DeepEqual(got, expected) {
		t.Fatalf("wrong result: got %v, expected %v", got, expected)
	}

}
