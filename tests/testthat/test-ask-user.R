test_that("ask prompts are returned", {
  if(!maxima.env$maxima$isInstalled()) 
    skip("Maxima not installed")
  expect_match(maxima.get("integrate(x^n,x)"), "Is n equal to -1?")
  expect_match(maxima.get("integrate(x^n,x)"), "Is n equal to -1?")
  expect_match(maxima.get("y;"), "^[[:space:]|[:print:]]*$")
})
