test_that("maxima.apropos returns a maxima object listing matching symbols", {
  if(!maxima.env$maxima$isInstalled())
    skip("Maxima not installed")

  res <- maxima.apropos("int")

  expect_s3_class(res, "maxima")
  expect_type(res, "list")

  # parse only the no-label, linear branch; it holds the [a, b, c, ...] list
  lin <- paste(res$wol$linear, collapse = "")

  # strip the surrounding brackets and split the comma-separated symbol list
  syms <- trimws(unlist(strsplit(gsub("[][]", "", lin), ",")))
  syms <- syms[nzchar(syms)]

  # a real apropos("integer") hit-list has many symbols, not a stub
  expect_gt(length(syms), 50L)

  # the canonical entry must be present, regardless of Lisp backend or ordering
  expect_true("integer" %in% syms)
})
