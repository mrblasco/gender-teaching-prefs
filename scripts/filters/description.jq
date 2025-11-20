select(.extracted_sections?.description|length > 0)
| {
    id: ._id,
    desc: (
        .extracted_sections?
        .description
        | (if length > 0 then max_by(.mean_proba) else null end)
        | (if . == null then null else {
            p: ((.mean_proba * 100 | floor) / 100),
            text: (.text | gsub("[[:space:]]+";" "))
        } end)
    ),
}