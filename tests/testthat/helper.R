RAQSAPItestsetup_local <- function()
{
  if(file.exists("local.R"))
  {
    message("test")
    cat("current working directory")
    cat(getwd())
    cat("list.files() \n
        ------------------------------------")
    cat(list.files(), sep = "\n")
    AQScredentials <- RAQSAPItestsetup_local()
    datamartAPI_user <- AQScredentials$AQSusername
    datamartAPI_key <- AQScredentials$AQSkey
    AQScredentials <- list(datamartAPI_user = datamartAPI_user,
                           datamartAPI_key = datamartAPI_key)

    return(AQScredentials)
  }
}
