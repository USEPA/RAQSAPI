

if(!file.exists("local.R"))
{
  source("./tests/testthat/local.R")
  AQScredentials <- RAQSAPItestsetup_local()
  datamartAPI_user <- AQScredentials$AQSusername
  datamartAPI_key <- AQScredentials$AQSkey
  AQScredentials <- list(datamartAPI_user = datamartAPI_user,
                         datamartAPI_key = datamartAPI_key)

  return(AQScredentials)
  # RAQSAPI::aqs_credentials(username = datamartAPI_user,
  #                          key = datamartAPI_key
  #                          )

}
