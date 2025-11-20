select(.citations | length > 0)
| [
    .citations[]
    | select(.catalog_record != null )
    | .catalog_record
    | select(.dois | length > 0)
    | {
        id: ._id, 
        dois: .dois
    }
]