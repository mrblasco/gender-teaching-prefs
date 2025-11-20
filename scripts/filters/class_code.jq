select(.extracted_sections != null) 
| {
    id: ._id,
    code: (
        .extracted_sections 
        | .code 
        | max_by(.mean_proba)? 
        | .text
    )
} | 
select(.code != null)
| [.id, .code]
| @csv
