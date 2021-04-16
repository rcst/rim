test_that("ask prompts are returned", {
  maxima.start() 
  expect_match(maxima.get("integrate(x^n,x)"), "Is n equal to -1?")
  expect_match(maxima.get("integrate(x^n,x)"), "Is n equal to -1?")
  expect_match(maxima.get("y;"), "^\\$\\$[[:space:]|[:print:]]*\\$\\$$")
})
