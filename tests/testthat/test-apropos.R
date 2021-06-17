test_that("apropos returns formatted strings", {
  if(!maxima.env$maxima$isInstalled()) 
    skip("Maxima not installed") 
  expect_match(maxima.apropos("int"), "^[[:space:]|[:print:]]*$")
})
