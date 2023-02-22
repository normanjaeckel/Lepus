package allocation_test

import (
	"testing"

	"github.com/normanjaeckel/Lepus/server/allocation"
)

func TestValidate(t *testing.T) {
	t.Run("test correct body", func(t *testing.T) {
		body := []byte(`
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
		`)

		p, err := allocation.Validate(body)

		if err != nil {
			t.Fatalf("expected correct validation, got error %q", err.Error())
		}

		if p.Config.HowManySpecialPupils != 2 ||
			p.Config.Timeout != 20 ||
			p.Config.NumberOfCycles != 1000 {
			t.Fatalf("wrong content for p.Config, expected some values (see test code), got %v", p.Config)
		}
		for _, pupil := range p.Pupils {
			if pupil.Class != "2a" {
				t.Fatalf("wrong content for p.Pupils, expected some values (see test code), got %v", p.Pupils)
			}
		}

	})

}
