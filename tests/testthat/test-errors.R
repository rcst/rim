test_that("errors are forwarded", {
  if(!maxima.env$maxima$isInstalled()) 
    skip("Maxima not installed")

  expect_error(maxima.get("1/0;"))
})

