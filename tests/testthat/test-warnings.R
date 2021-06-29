test_that("warnings are forwarded", {
  if(!maxima.env$maxima$isInstalled()) 
    skip("Maxima not installed")
  if(maxima.version() < "5.43")
    skip(paste("No warnings to be tested under Maxima", maxima.version(), "\n"))

  expect_warning(maxima.get("load(\"abs_integrate\")$"))
})
