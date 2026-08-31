if (requireNamespace("spelling", quietly = TRUE)) {
  if (file.exists("DESCRIPTION")) {
    spelling::spell_check_test(vignettes = TRUE, error = FALSE, lang = "en-US", skip_on_cran = TRUE)
  }
}
