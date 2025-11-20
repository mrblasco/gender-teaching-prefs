{ 
    id: ._id,
    info: 
        (.extracted_sections? // {})
        | {
            title: (
                .title 
                | (if length > 0 then max_by(.mean_proba) else null end) 
                | .text? // ""
                | gsub("[[:space:]]+";" ") 
                | ascii_upcase
            ),
            code: (
                .code
                | (if length > 0 then max_by(.mean_proba) else null end) 
                | .text? // ""
                | gsub("[[:space:]]+";" ") 
                | ascii_upcase
            ),
        }
} 
| [.id, .info.title, .info.code] | @csv 

