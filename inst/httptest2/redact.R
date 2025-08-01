set_redactor(~ gsub_response(.,getOption("aqs_key"), "<redacted>") %>%
               gsub_response(.,getOption("aqs_username"), "<redacted>") %>%
               gsub_response(., "https\\://aqs.epa.gov/data/api/", "datamart/")
             )

