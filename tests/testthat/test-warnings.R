test_that("warnings are forwarded", {
  if(maxima.version() < "5.45")
    skip(paste("No warnings to be tested under Maxima", maxima.version(), "\n"))

  expect_warning(maxima.get("load(\"abs_integrate\")$"))
})
