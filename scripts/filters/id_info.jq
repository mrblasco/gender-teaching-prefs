#!/usr/bin/env jq -f
[
    ._id, 
    ((.syllabus_probability * 100 | round) / 100),
    .date?.year // "", 
    .field?.name // "", 
    .language, 
    .institution._id
] | @csv