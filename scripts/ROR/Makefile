DATA := ../../data/raw/v1.73-2025-10-28-ror-data.json.gz

OUT := ../../data/interim/ror.json

$(OUT): $(DATA)
	gunzip -c $< | jq -c '.[] | select(.types[] == "Education") | {name: .name, country: .country.country_code, grid: .external_ids.GRID.preferred}' > $@
