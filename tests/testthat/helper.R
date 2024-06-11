RAQSAPItestsetup_helper <- function()
{
  path <- NA
  if(file.exists("local.R")) {path <-  "./"}
  else if(file.exists("tests/testthat/local.R")) {path <- "tests/testthat/"}

  if(!is.na(path))
  {
    source(paste0(path, "local.R"))
    AQScredentials <- RAQSAPItestsetup_local()
    datamartAPI_user <- AQScredentials$AQSusername
    datamartAPI_key <- AQScredentials$AQSkey
    AQScredentials <- list(datamartAPI_user = datamartAPI_user,
                           datamartAPI_key = datamartAPI_key)
    return(AQScredentials)
  }
}
