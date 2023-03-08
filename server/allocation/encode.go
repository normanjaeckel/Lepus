package allocation

import (
	"encoding/json"
	"fmt"
	"io"
)

func encode(res map[pupil][]eventID, w io.Writer) error {
	output := make(map[pupilID][]eventID, len(res))
	for p, eList := range res {
		output[p.id] = eList
	}
	if err := json.NewEncoder(w).Encode(output); err != nil {
		return fmt.Errorf("marshalling JSON: %w", err)
	}
	return nil
}
