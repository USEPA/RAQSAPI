if (requireNamespace("spelling", quietly = TRUE))
  {
    spelling::spell_check_test(pkg = "../",
                               vignettes = TRUE,
                               error = FALSE,
                               lang = "en-US",
                               skip_on_cran = FALSE)
  }
