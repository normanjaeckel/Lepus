package allocation

import (
	"encoding/json"
	"fmt"
	"io"
)

func encode(result map[pupilID][]eventID, w io.Writer) error {
	if err := json.NewEncoder(w).Encode(result); err != nil {
		return fmt.Errorf("marshalling JSON: %w", err)
	}
	return nil
}
