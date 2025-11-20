select(.citations | length > 0)
| {
    id: ._id,
    dois: ([
        .citations[]
        | select(.catalog_record != null )
        | .catalog_record
        | select(.dois | length > 0)
        | .dois[]
    ] | unique)
}
| select(.dois | length > 0)