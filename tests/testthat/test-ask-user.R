test_that("ask prompts are returned", {
  maxima.start() 
  expect_match(maxima.tell("integrate(x^n,x)"), "Is n equal to -1?")
  expect_match(maxima.tell("integrate(x^n,x)"), "Is n equal to -1?")
  expect_match(maxima.tell("y;"), "^\\$\\$[[:space:]|[:print:]]*\\$\\$$")
})
