RAQSAPI::aqs_credentials(
  username = keyring::key_get("aqs_user"),
  key = keyring::key_get("aqs")
)

vcr::vcr_configure(
  filter_query_parameters = list(
    "key" = "NOTSECRET",
    "email" = "janedoe@example.com"
  ),
  ,
  filter_sensitive_data = list(
    "janedoe@example.com" = keyring::key_get("aqs_user"),
    "NOTSECRET" = keyring::key_get("aqs")
  )
)
