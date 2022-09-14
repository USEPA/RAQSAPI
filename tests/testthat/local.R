library(keyring)
library(magrittr)

Sys.setenv("RAQSAPIUSERNAME" = "aqsdatamart_packages@epa.gov")

key_get(service = "AQSDatamartAPI",
        username = "aqsdatamart_packages@epa.gov") %>%
  Sys.setenv("RAQSAPIKEY" = .)
