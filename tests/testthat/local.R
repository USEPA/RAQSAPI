library(keyring)
library(magrittr)

RAQSAPItestsetup_local <- function()
  {
    AQSusername <- "mccrowey.clinton@epa.gov"
    AQSkey <- key_get(service = "AQSDatamartAPI",
                       username = "mccrowey.clinton@epa.gov")

    return(list(AQSusername = AQSusername , AQSkey = AQSkey))
  }
