select(.extracted_sections != null) 
| {
    id: ._id,
    info: [
        (.extracted_sections | .title | max_by(.mean_prova)? | .text? | gsub("\\n+"; " ")),
        (.extracted_sections | .code | max_by(.mean_prova)? | .text)
    ]
} 
| [.id, .info[]]
| @csv
