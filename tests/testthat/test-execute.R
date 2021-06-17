test_that("execution returns formatted strings", {
  if(!maxima.env$maxima$isInstalled()) 
    skip("Maxima not installed")
  expect_match(maxima.get("2+2"), "^4$")
  expect_match(maxima.get("2+2;"), "^4$")
  expect_match(maxima.get("jacobian( [alpha / (alpha + beta), 1 / sqrt(alpha + beta)], [alpha, beta] )"), "^[[:space:]|[:print:]]*$")
  expect_match(maxima.get("4+4$"), "")

  # returns a error message between output prompt and new input prompt
  # argument needs to be quoted
  # expect_match(maxima.get("demo(append)"), "^\\$\\$[[:space:]|[:print:]]*\\$\\$$")

  # expect_match(maxima.get("example(append)"), "^\\$\\$[[:space:]|[:print:]]*\\$\\$$")
  # expect_match(maxima.get("describe(append)"), "^\\$\\$[[:space:]|[:print:]]*\\$\\$$")

  # test commands that depend on each other
  expect_match(maxima.get("V: 4/3 * %pi * r^3"), "^[[:space:]|[:print:]]*$")
  expect_match(maxima.get("r: 10;"), "^[[:space:]|[:print:]]*$")
  expect_match(maxima.get("V;"), "^[[:space:]|[:print:]]*$")

  # test re-evaluation of defined variable
  expect_match(maxima.get("''V;"), "^[[:space:]|[:print:]]*$")
})
