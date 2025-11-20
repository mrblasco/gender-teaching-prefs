select(.citations|length > 0)
| {
    id: ._id,
    num_cite: (.citations | length),
    citations: [
        .citations
        | unique_by(.catalog_key.title_key)
        | .[]
        | {
            title: .catalog_key? .title_key?,
            year: .catalog_record? .year?,
            type: .catalog_record? .publication_type?,
            venue: .catalog_record? .article? .venue?,
            id: .catalog_record? ._id?
        }
    ]
}
