library(dplyr)
library(ggplot2)


# ------ functions ------

convert_gender_label <- function(original_labels) {
  dplyr::case_match(
    original_labels,
    "f"  ~ "Female only (F)",
    "ff" ~ "Female-Female (FF)",
    "mf" ~ "Mixed Gender (MF/FM)",
    "fm" ~ "Mixed Gender (MF/FM)",
    "mm" ~ "Male-Male (MM)",
    "m"  ~ "Male only (M)",
    .default = "Unknown"
  )
}



isced_mapping <- list(
  "Education" = c("Education"),

  "Arts and Humanities" = c("Anthropology", "Chinese", "Classics",
                            "Dance", "English Literature",
                            "Film and Photography", "Fine Arts",
                            "French", "German", "Hebrew",
                            "History", "Japanese", "Liberal Arts",
                            "Linguistics", "Music", "Philosophy",
                            "Religion", "Spanish", "Theatre Arts",
                            "Theology", "Women's Studies"),

  "Social Sciences, Journalism, and Information" = c("Criminal Justice", 
                                                     "Criminology", "Economics",
                                                     "Geography", "Journalism",
                                                     "Library Science",
                                                     "Media / Communications",
                                                     "Political Science",
                                                     "Psychology",
                                                     "Public Administration",
                                                     "Public Safety",
                                                     "Social Work",
                                                     "Sociology"),

  "Business, Administration, and Law" = c("Accounting", "Law",
                                          "Business", "Marketing"),

  "Natural Sciences, Mathematics, and Statistics" = c("Astronomy",
                                                      "Atmospheric Sciences", 
                                                      "Biology", "Chemistry",
                                                      "Earth Sciences",
                                                      "Mathematics",
                                                      "Natural Resource Management",
                                                      "Physics"),

  "Information and Communication Technologies (ICTs)" = c("Basic Computer Skills",
                                                          "Computer Science"),

  "Engineering, Manufacturing, and Construction" = c("Architecture", 
                                                     "Construction",
                                                     "Engineering",
                                                     "Engineering Technician",
                                                     "Mechanic / Repair Tech",
                                                     "Transportation"),
  
  "Agriculture, Forestry, Fisheries, and Veterinary" = c("Agriculture", 
                                                         "Veterinary Medicine"),
  
  "Health and Welfare" = c("Dentistry", "Health Technician",
                           "Medicine", "Nursing", "Nutrition"),

  "Services" = c("Basic Skills", "Career Skills", "Cosmetology",
                 "Culinary Arts", "Fitness and Leisure",
                 "Military Science", "Sign Language")
)

recode_field <- function(x) {
  sapply(x, function(f) {
    category <- names(isced_mapping)[sapply(isced_mapping, function(fields) f %in% fields)]
    ifelse(length(category) > 0, category, "Uncategorized")
  })
}
