test_that("warnings are forwarded", {
  if(!maxima.env$maxima$isInstalled()) 
    skip("Maxima not installed")
  if(maxima.version() < "5.43" | maxima.version() >= "5.45.1")
    skip(paste("No warnings to be tested under Maxima", maxima.version(), "\n"))

  maxima.get("float2bf: false$")
  maxima.get("x: float(%pi)$")
  expect_warning(maxima.get("bfloat(x);"))
  expect_warning(maxima.eval("sum(1/x^2, x, 1, 10000);"))
})
