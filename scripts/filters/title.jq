{
    id: ._id,
    title: 
        .extracted_sections?
        | .title?
        | select(. | length > 0)
        | max_by(.mean_prova) 
        | {
            txt: (.text | gsub("[[:space:]]+"; " ")),
            p: ((.mean_proba * 100 | floor) / 100)
        },
    code: 
        .extracted_sections?
        | .code?
        | select(. | length > 0)
        | max_by(.mean_prova) 
        | {
            txt: (.text | gsub("[[:space:]]+"; " ")),
            p: ((.mean_proba * 100 | floor) / 100)
        }

} | 
[.id, .code.txt, .code.p, .title.txt, .title.p] | @csv
