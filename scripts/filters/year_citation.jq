#!/usr/bin/env jq -f
[
    select(.citations | length > 0)
    | {
        id: ._id,
        year_freq : (
            [
                .citations[] 
                | .catalog_record?
                | select(.year != null)
                | .year
            ]
            | sort
            | group_by(.)
            | map({year: .[0], count: length})
        )
    } 
    | . as $entry
    | $entry.year_freq[]
    | [$entry.id, .year, .count] 
] | (.[] | @csv)

